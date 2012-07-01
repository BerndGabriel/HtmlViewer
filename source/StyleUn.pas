{
Version   11.3
Copyright (c) 1995-2008 by L. David Baldwin,
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

unit StyleUn;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc,
{$else}
  Windows,
{$endif}
  Classes, Graphics, SysUtils, Math, Forms, Contnrs, Variants,
  HtmlGlobals, HtmlBuffer;

const
  IntNull = -12345678;
  Auto = -12348765;
  AutoParagraph = -12348766;
  ParagraphSpace = 14; {default spacing between paragraphs, etc.}
  ImageSpace = 3; {extra space for left, right images}
  ListIndent = 40;
  
  varInt = [varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord, varInt64];
  varFloat = [varSingle, varDouble, varCurrency];
  varNum = varInt + varFloat;

  EastEurope8859_2 = 31; {for 8859-2}
  CrLf = #$D#$A;

//BG, 16.09.2010: CSS2.2: same sizes like html font size:
type
  TFontSizeIncrement = -6..6;
const
  FontConvBase: array[1..7] of Double = (8.0, 10.0, 12.0, 14.0, 18.0, 24.0, 36.0);
  PreFontConvBase: array[1..7] of Double = (7.0, 8.0, 10.0, 12.0, 15.0, 20.0, 30.0);
var
  FontConv: array[1..7] of Double;
  PreFontConv: array[1..7] of Double;

type
  AlignmentType = (ANone, ATop, AMiddle, ABaseline, ABottom, ALeft, ARight, AJustify, ASub, ASuper);
  BoxSizingType = (ContentBox, BorderBox);

const
  CBoxSizing: array[BoxSizingType] of ThtString = (
    'content-box',
    'border-box');

type
  BorderStyleType = (bssNone, bssSolid, bssInset, bssOutset, bssGroove, bssRidge,
    bssDashed, bssDotted, bssDouble);

const
  CBorderStyle: array[BorderStyleType] of ThtString = (
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
  ListBulletType = (lbBlank, lbCircle, lbDecimal, lbDisc, lbLowerAlpha, lbLowerRoman,
    lbNone, lbSquare, lbUpperAlpha, lbUpperRoman);
  ClearAttrType = (clrNone, clLeft, clRight, clAll);
  PositionType = (posStatic, posRelative, posAbsolute, posFixed);
  VisibilityType = (viInherit, viHidden, viVisible);
  TextTransformType = (txNone, txUpper, txLower, txCaps);
  TBackgroundPosition = (bpTop, bpCenter, bpBottom, bpLeft, bpRight, bpPercent, bpDim);
  PositionRec = record
    PosType: TBackgroundPosition;
    Value: Integer;
    RepeatD: Boolean;
    Fixed: Boolean;
  end;
  PPositionRec = ^PositionRec;
  PtPositionRec = record
    X, Y: PositionRec;
  end;

{$IFDEF Ver90}
  TFontCharSet = Integer; {dummy for Delphi 2}
{$ENDIF}

  ThtFontInfo = class
  public
    iName: ThtString;
    iSize: Double;
    iStyle: TFontStyles;
    iColor: TColor;
    ibgColor: TColor;
    iCharSet: TFontCharSet;
    iCharExtra: Variant;
    procedure Assign(Source: ThtFontInfo);
  end;

  FIIndex = (LFont, VFont, HLFont, HVFont);
  TFontInfoArray = class
  public
    Ar: array[LFont..HVFont] of ThtFontInfo;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TFontInfoArray);
  end;

  TMyFont = class(TFont)
  public
    bgColor: TColor;
    tmHeight: Integer;
    tmDescent: Integer;
    tmExternalLeading: Integer;
    tmAveCharWidth: Integer;
    tmMaxCharWidth: Integer;
    tmCharset: Integer;
    CharExtra: Integer;
    EmSize: Integer;
    ExSize: Integer;
    procedure Assign(Source: TPersistent); override;
    procedure AssignToCanvas(Canvas: TCanvas);
    destructor Destroy; override;
    constructor Create; {$ifdef LCL} override; {$endif}
  end;

  PropIndices = (
    FontFamily, FontSize, FontStyle, FontWeight, TextAlign, TextDecoration,
    LetterSpacing, 
    //BG, 12.03.2011 removed: BorderStyle,
    Color, 
    // these properties are in MarginArrays
    BackgroundColor, 
    //BG, 12.03.2011 removed: BorderColor,
    MarginTop, MarginRight, MarginBottom, MarginLeft,
    piMinHeight, piMinWidth, piMaxHeight, piMaxWidth,
    BoxSizing,
    PaddingTop, PaddingRight, PaddingBottom, PaddingLeft,
    // BG, 31.01.2012: don't change the order of the border properties:
    BorderTopWidth, BorderRightWidth, BorderBottomWidth, BorderLeftWidth,
    BorderTopColor, BorderRightColor, BorderBottomColor, BorderLeftColor,
    BorderTopStyle, BorderRightStyle, BorderBottomStyle, BorderLeftStyle,
    //
    piWidth, piHeight, TopPos, BottomPos, RightPos, LeftPos, 

    Visibility, LineHeight, BackgroundImage, BackgroundPosition,
    BackgroundRepeat, BackgroundAttachment, VerticalAlign, Position, ZIndex,
    ListStyleType, ListStyleImage, Float, Clear, TextIndent,
    PageBreakBefore, PageBreakAfter, PageBreakInside, TextTransform,
    WordWrap, FontVariant, BorderCollapse, OverFlow, piDisplay, piEmptyCells);

  TVMarginArray = array[BackgroundColor..LeftPos] of Variant;
  TMarginArray = array[BackgroundColor..LeftPos] of Integer;

type
  TPropDisplay = (
    pdUnassigned,
    pdBlock,
    pdInline,
    pdInlineBlock,
    pdListItem,
    pdRunIn,
    pdNone);
const
  CPropDisplay: array [TPropDisplay] of ThtString = (
    '',
    'block',
    'inline',
    'inline-block',
    'list-item',
    'run-in',
    'none');

  CListStyleType: array[Low(ListBulletType)..High(ListBulletType)] of ThtString =
  ('blank', 'circle', 'decimal', 'disc', 'lower-alpha', 'lower-roman',
    'none', 'square', 'upper-alpha', 'upper-roman');

type
  TStyleList = class;
  TPropStack = class;

  TProperties = class(TObject)
  private
    PropStack: TPropStack; // owner
    TheFont: TMyFont;
    InLink: Boolean;
    DefFontname: ThtString;
    FUseQuirksMode : Boolean;
    procedure AddPropertyByIndex(Index: PropIndices; PropValue: ThtString);
    procedure AssignCharSet(CS: TFontCharset);
    procedure AssignCodePage(const CP: Integer);
    procedure CalcLinkFontInfo(Styles: TStyleList; I: Integer);
    procedure GetSingleFontInfo(var Font: ThtFontInfo);
  public
    PropTag, PropClass, PropID, PropPseudo, PropTitle: ThtString;
    PropStyle: TProperties;
    FontBG: TColor;
    FCharSet: TFontCharSet;
    FCodePage: Integer;
    FEmSize, FExSize: Integer; {# pixels for Em and Ex dimensions}
    Props: array[Low(PropIndices)..High(PropIndices)] of Variant;
    Originals: array[Low(PropIndices)..High(PropIndices)] of Boolean;
    FIArray: TFontInfoArray;
    ID: Integer;

    constructor Create; overload; // for use in style list only
    constructor Create(const AUseQuirksMode : Boolean); overload; // for use in style list only
    constructor Create(APropStack: TPropStack; const AUseQuirksMode : Boolean); overload; // for use in property stack

    destructor Destroy; override;
    function BorderStyleNotBlank: Boolean;
    function Collapse: Boolean;
    function GetBackgroundColor: TColor;
    function GetBackgroundImage(var Image: ThtString): Boolean;
    function GetBorderStyle(Index: PropIndices; var BorderStyle: BorderStyleType): Boolean;
    function GetClear(var Clr: ClearAttrType): Boolean;
    function GetDisplay: TPropDisplay; //BG, 15.09.2009
    function GetFloat(var Align: AlignmentType): Boolean;
    function GetFont: TMyFont;
    function GetFontVariant: ThtString;
    function GetLineHeight(NewHeight: Integer): Integer;
    function GetListStyleImage: ThtString;
    function GetListStyleType: ListBulletType;
    function GetOriginalForegroundColor: TColor;
    function GetPosition: PositionType;
    function GetTextIndent(out PC: Boolean): Integer;
    function GetTextTransform: TextTransformType;
    function GetVertAlign(var Align: AlignmentType): Boolean;
    function GetVisibility: VisibilityType;
    function GetZIndex: Integer;
    function HasBorderStyle: Boolean;
    function HasBorderWidth: Boolean;
    function IsOverflowHidden: Boolean;
    function ShowEmptyCells: Boolean;
    procedure AddPropertyByName(const PropName, PropValue: ThtString);
    procedure Assign(const Item: Variant; Index: PropIndices);
    procedure AssignCharSetAndCodePage(CS: TFontCharset; CP: Integer);
    procedure Combine(Styles: TStyleList; const Tag, AClass, AnID, Pseudo, ATitle: ThtString; AProp: TProperties; ParentIndexInPropStack: Integer);
    procedure Copy(Source: TProperties);
    procedure CopyDefault(Source: TProperties);
    procedure GetBackgroundPos(EmSize, ExSize: Integer; out P: PtPositionRec);
    procedure GetFontInfo(AFI: TFontInfoArray);
    procedure GetPageBreaks(out Before, After, Intact: Boolean);
    function GetBoxSizing(var VBoxSizing : BoxSizingType) : Boolean;
    procedure GetVMarginArray(var MArray: TVMarginArray; DefaultToColor: Boolean = true);
    procedure Inherit(Tag: ThtString; Source: TProperties);
    procedure SetFontBG;
    procedure Update(Source: TProperties; Styles: TStyleList; I: Integer);
    //BG, 20.09.2009:
    property Display: TPropDisplay read GetDisplay;
    property CharSet: TFontCharset read FCharSet write AssignCharSet;
    property CodePage: Integer read FCodePage write AssignCodePage;
    property EmSize: Integer read FEmSize;
    property ExSize: Integer read FExSize;
    property UseQuirksMode : Boolean read FUseQuirksMode;
  end;

  TStyleList = class(ThtStringList)
  private
    SeqNo: Integer;
  protected
    //this must be protected so that the property can be changed in
    //a descendant while being read only.
    FUseQuirksMode : Boolean;
    procedure setLinksActive(Value: Boolean); virtual; abstract;
    property LinksActive: Boolean write setLinksActive;
  public
    DefProp: TProperties;
    constructor Create; overload;
    constructor Create(const AUseQuirksMode : Boolean); overload;
    destructor Destroy; override;
    function AddDuplicate(const Tag: ThtString; Prop: TProperties): TProperties;
    function AddObject(const S: ThtString; AObject: TObject): Integer; override;
    function GetSeqNo: ThtString;
    procedure Clear; override;
    procedure AddModifyProp(const Selector, Prop, Value: ThtString);
    procedure FixupTableColor(BodyProp: TProperties);
    procedure Initialize(const FontName, PreFontName: ThtString;
      PointSize: Integer; AColor, AHotspot, AVisitedColor, AActiveColor: TColor;
      LinkUnderline: Boolean; ACharSet: TFontCharSet; MarginHeight, MarginWidth: Integer);
    procedure ModifyLinkColor(Pseudo: ThtString; AColor: TColor);
    property UseQuirksMode : Boolean read FUseQuirksMode write FUseQuirksMode;
  end;

  TPropStack = class(TObjectList)
  private
    function GetProp(Index: Integer): TProperties; {$ifdef UseInline} inline; {$endif}
  public
    function Last: TProperties; {$ifdef UseInline} inline; {$endif}
    property Items[Index: Integer]: TProperties read GetProp; default;
  end;

const
  PropWords: array[Low(PropIndices)..High(PropIndices)] of ThtString =
  ('font-family', 'font-size', 'font-style', 'font-weight', 'text-align',
    'text-decoration', 'letter-spacing',
    //'border-style',
    'color', 'background-color',
    //'border-color',
    'margin-top', 'margin-right', 'margin-bottom', 'margin-left',
    'min-height', 'min-width',    'max-height',    'max-width',
    'box-sizing',
    'padding-top', 'padding-right', 'padding-bottom', 'padding-left',
    'border-top-width', 'border-right-width', 'border-bottom-width', 'border-left-width',
    'border-top-color', 'border-right-color', 'border-bottom-color', 'border-left-color',
    'border-top-style', 'border-right-style', 'border-bottom-style', 'border-left-style',
    'width', 'height', 'top', 'bottom', 'right', 'left', 'visibility',
    'line-height', 'background-image', 'background-position',
    'background-repeat', 'background-attachment', 'vertical-align', 'position', 'z-index',
    'list-style-type', 'list-style-image', 'float', 'clear', 'text-indent',
    'page-break-before', 'page-break-after', 'page-break-inside', 'text-transform',
    'word-wrap', 'font-variant', 'border-collapse', 'overflow', 'display', 'empty-cells');

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

// BG, 25.04.2012: Added:
function IsAuto(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}

//BG, 05.10.2010: added:
function VarIsIntNull(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}
function VarIsAuto(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}

function VMargToMarg(const Value: Variant; Relative: Boolean; Base, EmSize, ExSize, Default: Integer): Integer;

procedure ConvMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize: Integer;
  BorderWidth: Integer; out AutoCount: Integer; var M: TMarginArray);

procedure ConvVertMargins(const VM: TVMarginArray;
  BaseHeight, EmSize, ExSize: Integer;
  var M: TMarginArray; out TopAuto, BottomAuto: Boolean);

procedure ConvMargArrayForCellPadding(const VM: TVMarginArray; EmSize,
  ExSize: Integer; var M: TMarginArray);

procedure ConvInlineMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize,
  ExSize: Integer; {BStyle: BorderStyleType;} out M: TMarginArray);

function OpacityFromStr(S : ThtString) : Byte;

function SortedColors: ThtStringList;
function ColorFromString(S: ThtString; NeedPound: Boolean; out Color: TColor): Boolean;
function ColorAndOpacityFromString(S: ThtString; NeedPound: Boolean; out Color: TColor; out VOpacity : Byte): Boolean;

function ReadURL(Item: Variant): ThtString;

function RemoveQuotes(const S: ThtString): ThtString;
function ReadFontName(S: ThtString): ThtString;

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

{ Media type handling from trunk}
function MediaTypesToStr(const MediaTypes: TMediaTypes): ThtString;
function TranslateMediaTypes(const MediaTypes: TMediaTypes): TMediaTypes;
function TryStrToMediaType(const Str: ThtString; out MediaType: TMediaType): Boolean;
function TryStrToMediaTypes(const Str: ThtString; out MediaTypes: TMediaTypes): Boolean;

{$ifdef JPM_DEBUGGING}
procedure LogProperties(AProp : TProperties; const APropName : String);
{$endif}

  procedure ApplyBoxWidthSettings(var AMarg : TMarginArray; var VMinWidth, VMaxWidth : Integer; const AUseQuirksMode : Boolean);

procedure ApplyBorderBoxModel(var AMarg : TMarginArray);
procedure ApplyBoxSettings(var AMarg : TMarginArray; const AUseQuirksMode : Boolean);

implementation
uses
 {$ifdef JPM_DEBUGGING}
 CodeSiteLogging,
 {$endif}
 HSLUtils;

type
  TMyFontCache = class
  private
    FFontsByName: ThtStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Font: TMyFont);
    function Find(FontInfo: ThtFontInfo): TMyFont;
  end;

var
  DefPointSize: Double;
  CharsetPerCharset: array [TFontCharset] of record Inited: Boolean; Charset: TFontCharset; end;
  AllMyFonts: TMyFontCache;

{$ifdef JPM_DEBUGGING}
const
    CVis : array [0..2] of string = ('viInherit','viHidden','viVisible');

function LogPropColor(const AInt : Integer): String; overload;
begin
  Result := Graphics.ColorToString( AInt);
end;

function LogPropColor(const AVar : Variant): String; overload;
begin
  if Variants.VarIsOrdinal(AVar) then
    Result := Graphics.ColorToString( AVar)
  else
    Result := VarToStr( AVar );
end;

function LogPropDisplay(const AInt : Integer): String; overload;
begin
  Result := CPropDisplay[ TPropDisplay(AInt)];
end;

function LogPropDisplay(const AVar : Variant): String; overload;
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CPropDisplay[ TPropDisplay(AVar)]  + ' int = '+ IntToStr(AVar);;
  end else begin
    Result := VarToStr( AVar );
  end;
end;

function LogPropBoxSizing(const AInt : Integer): String; overload;
begin
  case AInt of
    0 : Result := CBoxSizing[ContentBox];
    1 : Result := CBoxSizing[BorderBox];
  else
    Result := 'Error value is '+IntToStr(AInt);
  end;
//  Result := CBoxSizing[BoxSizingType (AInt)];
end;

function LogPropBoxSizing(const AVar : Variant): String; overload;
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CBoxSizing[BoxSizingType (AVar)]  + ' int = '+ IntToStr(AVar);;
  end else begin
    Result := VarToStr( AVar );
  end;
end;

function LogPropBorderStyle(const AVar : Variant): String;
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CBorderStyle[ BorderStyleType ( AVar )]  + ' int = '+ IntToStr(AVar);;
  end else begin
    Result := VarToStr( AVar);
  end;

end;

function LogPropListStyle(const AVar : Variant): String;
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CListStyleType[ ListBulletType ( AVar )]  + ' int = '+ IntToStr(AVar);;
  end else begin
    Result := VarToStr( AVar);
  end;
end;

function LogVisibility(const AVar : Variant): String;
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CVis[Integer( AVar )] + ' int = '+ IntToStr(AVar);
  end else begin
    Result := VarToStr( AVar);
  end;
end;

procedure LogTVMarginArray(const AMarg : TVMarginArray; const AMargName : String);
var i : PropIndices;
begin
  for i := Low(AMarg) to High(AMarg) do
        case I of
          Color, BackgroundColor,
          BorderTopColor..BorderLeftColor :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropColor(AMarg[I])]);
          piDisplay :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropDisplay(AMarg[I])]);
          BoxSizing :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropBoxSizing(AMarg[I])]);
          BorderTopStyle..BorderLeftStyle :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropBorderStyle(AMarg[I])]);
          ListStyleType :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropListStyle(AMarg[I])]);
          Visibility :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogVisibility(AMarg[I])]);
        else
          CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i],VarToStr( AMarg[I])]);

        end;
end;

procedure LogTMarginArray(AMarg : TMarginArray; const AMargName : String);
var i : PropIndices;
begin
  for i := Low(AMarg) to High(AMarg) do
    if VarIsIntNull(AMarg[I]) then begin
      CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], 'IntNul']);
    end else begin
      if VarIsAuto(AMarg[I]) then begin
        CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], 'Auto']);

      end else begin
        case I of
          Color, BackgroundColor,
          BorderTopColor..BorderLeftColor :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropColor(AMarg[I])]);
          piDisplay :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropDisplay(AMarg[I])]);
          BoxSizing :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropBoxSizing(AMarg[I])]);
          BorderTopStyle..BorderLeftStyle :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropBorderStyle(AMarg[I])]);
          ListStyleType :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogPropListStyle(AMarg[I])]);
          Visibility :
            CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i], LogVisibility(AMarg[I])]);
        else
          CodeSiteLogging.CodeSite.SendFmtMsg('%s[%s] = %s',[AMargName,PropWords[i],VarToStr( AMarg[I])]);

        end;
    end;
  end;
end;

procedure LogProperties(AProp : TProperties; const APropName : String);

var i : PropIndices;
begin
  if not Assigned(AProp) then begin
    CodeSiteLogging.CodeSite.SendFmtMsg('%s = nil',[APropName]);
    exit;
  end;
  CodeSiteLogging.CodeSite.SendFmtMsg('%s.PropTag = %s',[APropName,AProp.PropTag ]);
  CodeSiteLogging.CodeSite.SendFmtMsg('%s.PropClass = %s',[APropName,AProp.PropClass ]);
  CodeSiteLogging.CodeSite.SendFmtMsg('%s.PropID = %s',[APropName,AProp.PropID ]);
  CodeSiteLogging.CodeSite.SendFmtMsg('%s.PropTitle = %s',[APropName,AProp.PropTitle ]);
  CodeSiteLogging.CodeSite.SendFmtMsg('%s.PropPseudo = %s',[APropName,AProp.PropPseudo ]);
//  for I := Low(TMainTextOpacities) to High(TMainTextOpacities) do begin
//    CodeSiteLogging.CodeSite.SendFmtMsg('%s.MainTextOpacities[%s] = %d',[APropName,PropWords[i],AProp.MainTextOpacities[I] ]);
//  end;
//  for I := Low(TBorderOpacities) to High(TBorderOpacities) do begin
//    CodeSiteLogging.CodeSite.SendFmtMsg('%s.BorderOpacities[%s] = %d',[APropName,PropWords[i],AProp.BorderOpacities[I]]);
//  end;
  for I := Low(AProp.Props) to High(AProp.Props) do begin
    if VarIsIntNull(AProp.Props[I]) then begin
      CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = null',[APropName,PropWords[i]]);
    end else begin
      if VarIsAuto(AProp.Props[I]) then begin
        CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = auto',[APropName,PropWords[i]]);
      end else begin
        case I of
          Color, BackgroundColor,
          BorderTopColor..BorderLeftColor :
            begin
              CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i],LogPropColor( AProp.Props[I])]);
            end;
          piDisplay :
            begin
              CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i], LogPropDisplay(AProp.Props[I])]);
            end;
          BoxSizing :
            begin
              CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i],LogPropBoxSizing( AProp.Props[I])]);
            end;
          BorderTopStyle..BorderLeftStyle :
            begin
              CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i], LogPropBorderStyle( AProp.Props[I])]);
            end;
          ListStyleType :
            begin
              CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i], LogPropListStyle( AProp.Props[I])]);
            end;
          Visibility :
            begin
                CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i],LogVisibility( AProp.Props[I])]);
            end;
        else
          CodeSiteLogging.CodeSite.SendFmtMsg('%s.TPropertyArray[%s] = %s',[APropName,PropWords[i],VarToStr( AProp.Props[I])]);
        end;
      end;
    end;
  end;
end;
{$endif}

function FontSizeConv(const Str: ThtString; OldSize: Double; const AUseQuirksMode : Boolean): Double; forward;
function LengthConv(const Str: ThtString; Relative: Boolean; Base, EmSize, ExSize, Default: Integer): Integer; forward;

function FindPropIndex(const PropWord: ThtString; out PropIndex: PropIndices): Boolean;
var
  I: PropIndices;
begin
  Result := True;
  for I := Low(PropIndices) to High(PropIndices) do
    if PropWord = PropWords[I] then
    begin
      PropIndex := I;
      Exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 17.02.2011 --
function SkipWhiteSpace(const S: ThtString; I, L: Integer): Integer;
begin
  while I <= L do
  begin
    case S[I] of
      ' ',
      #10,
      #12,
      #13:;
    else
      break;
    end;
    Inc(I);
  end;
  Result := I;
end;

//-- BG ---------------------------------------------------------- 17.02.2011 --
function FindChar(const S: ThtString; C: ThtChar; I, L: Integer): Integer;
begin
  while (I <= L) and (S[I] <> C) do
    Inc(I);
  Result := I;
end;

{----------------ReadURL}

function ReadURL(Item: Variant): ThtString;
{
  If Item is a string try to find and parse:

    url( ["<url>"|'<url>'|<url>] )

  and if successful return the <url>.

  ReadURL tolerates
  - any substring before url
  - any substring after the (optionally quoted) <url>
  - nested '(' ')' pairs even in the unquoted <url>
}
var
  S: ThtString;
  I, J, L, N: Integer;
  Q: ThtChar;
begin
  Result := '';
  if VarIsStr(Item) then
  begin
    S := Item;
    I := Pos('url(', S);
    if I > 0 then
    begin
      L := Length(S);

      // optional white spaces
      I := SkipWhiteSpace(S, I + 4, L);

      // optional quote char
      Q := #0;
      if I < L then
        case S[I] of
          '''', '"':
          begin
            Q := S[I];
            Inc(I);
          end;
        end;

      // read url
      if Q <> #0 then
        // up to quote char
        J := FindChar(S, Q, I, L)
      else
      begin
        // unquoted: up to whitespace or ')'
        // beyond CSS: tolerate nested '(' ')' pairs as part of the name.
        N := 0;
        J := I;
        while J <= L do
        begin
          case S[J] of
            ' ',
            #10,
            #12,
            #13:
              if N = 0 then
                break;

            '(':
              Inc(N);

            ')':
            begin
              if N = 0 then
                break;
              Dec(N);
            end;
          end;
          Inc(J);
        end;
      end;
      Result := Copy(S, I, J - I);

      // ignore the rest: optional whitespaces and ')'
    end;
  end;
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

{----------------TMyFont.Assign}

procedure TMyFont.Assign(Source: TPersistent);
begin
  if Source is TMyFont then
  begin
    bgColor := TMyFont(Source).bgColor;
    tmHeight := TMyFont(Source).tmHeight;
    tmDescent := TMyFont(Source).tmDescent;
    tmExternalLeading := TMyFont(Source).tmExternalLeading;
    tmAveCharWidth := TMyFont(Source).tmAveCharWidth;
    tmMaxCharWidth := TMyFont(Source).tmMaxCharWidth;
    tmCharset := TMyFont(Source).tmCharset;
    CharExtra := TMyFont(Source).CharExtra;
    EmSize := TMyFont(Source).EmSize;
    ExSize := TMyFont(Source).ExSize;
  end;
  inherited Assign(Source);
end;

procedure TMyFont.AssignToCanvas(Canvas: TCanvas);
begin
  Canvas.Font := Self;
  SetTextCharacterExtra(Canvas.Handle, CharExtra);
end;

destructor TMyFont.destroy;
begin
  inherited;
end;

constructor TMyFont.Create;
begin
  inherited;
  Charset := DEFAULT_CHARSET;
end;

var
  Sequence: Integer;

{----------------TProperties.Create}

constructor TProperties.Create;
var
  I: PropIndices;
begin
  inherited Create;
  ID := Sequence;
  Inc(Sequence);
  FontBG := clNone;
  for I := MarginTop to LeftPos do
    Props[I] := IntNull;
  Props[ZIndex] := 0;
  FUseQuirksMode := False;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --

constructor TProperties.Create(APropStack: TPropStack; const AUseQuirksMode : Boolean);
begin
  Create;
  self.PropStack := APropStack;
  FUseQuirksMode := AUseQuirksMode;
end;

constructor TProperties.Create(const AUseQuirksMode : Boolean);
begin
  Create;
  FUseQuirksMode := AUseQuirksMode;
end;

destructor TProperties.Destroy;
begin
  TheFont.Free;
  FIArray.Free;
  inherited;
end;

{----------------TProperties.Copy}

procedure TProperties.Copy(Source: TProperties);
var
  I: PropIndices;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'Copy');
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Source,'Source');
  {$ENDIF}
  for I := Low(I) to High(I) do
    Props[I] := Source.Props[I];
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'Copy');
  {$ENDIF}
end;

{----------------TProperties.CopyDefault}

procedure TProperties.CopyDefault(Source: TProperties);
var
  I: PropIndices;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'CopyDefault');
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Source,'Source');
  {$ENDIF}
  for I := Low(I) to High(I) do
    Props[I] := Source.Props[I];
  AssignCharSet(Source.CharSet);
  DefFontname := Source.DefFontname;
  PropTag := 'default';
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'CopyDefault');
  {$ENDIF}
end;

procedure TProperties.Inherit(Tag: ThtString; Source: TProperties);
{copy the properties that are inheritable}
var
  I: PropIndices;
  Span, HBF: Boolean;
  isTable: Boolean;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.Inherit');
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.SendFmtMsg('tag = [%s]',[Tag]);
  StyleUn.LogProperties(Source,'Source');
  {$ENDIF}
  Span := Source.PropTag = 'span';
  HBF := (Source.PropTag = 'thead') or (Source.PropTag = 'tbody') or (Source.PropTag = 'tfoot');
  isTable := Tag = 'table';
  for I := Low(I) to High(I) do
    if Span {and (I <> BorderStyle)} then {Borderstyle already acted on}
      Props[I] := Source.Props[I]
    else if HBF then
    begin
      Props[I] := Source.Props[I]; {tr gets them all}
      Originals[I] := Source.Originals[I];
    end
    else if (I = WordWrap) and isTable then {table doesn't inherit word wrap}
      Props[WordWrap] := 'normal'
    else
      case I of
        MarginTop..BorderLeftStyle,
        piWidth, piHeight,
        TopPos..LeftPos:
          Props[I] := IntNull;
        BackgroundColor, BackgroundImage, BackgroundPosition,
        BackgroundRepeat, BackgroundAttachment,
        //BorderColor, BorderStyle,
        BorderCollapse,
        PageBreakBefore, PageBreakAfter, PageBreakInside,
        Clear, Float, Position, OverFlow, piDisplay:
          ; {do nothing}
      else
        Props[I] := Source.Props[I];
      end;
  DefFontname := Source.DefFontname;
  FontBG := Source.FontBG;
  CharSet := Source.CharSet;
  CodePage := Source.CodePage;
  PropTitle := Source.PropTitle;
  InLink := Source.InLink;
  if InLink then
  begin
    if not Assigned(FIArray) then
      FIArray := TFontInfoArray.Create;
    FIArray.Assign(Source.FIArray);
  end;

  FEmSize := Source.FEmSize; {actually this is calculated later }
  FExSize := Source.FExSize; {apparently correlates with what browsers are doing}
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.Inherit');
  {$ENDIF}
end;

{----------------TProperties.Update}

procedure TProperties.Update(Source: TProperties; Styles: TStyleList; I: Integer);
{Change the inherited properties for this item to those of Source}
var
  Index: PropIndices;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.Update');
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Source,'Source');
  {$ENDIF}
  for Index := Low(Index) to High(Index) do
    if not Originals[Index] then
      Props[Index] := Source.Props[Index];
  FreeAndNil(TheFont); {may no longer be good}
  if Assigned(FIArray) then
    if Source.Inlink then
      FIArray.Assign(Source.FIArray)
    else if PropPseudo = 'link' then {an <a href> tag}
      CalcLinkFontInfo(Styles, I)
    else
    begin {an <a href> tag has been removed}
      FreeAndNil(FIArray);
      Inlink := False;
    end;
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.Update');
  {$ENDIF}
end;

{----------------TProperties.Assign}

procedure TProperties.Assign(const Item: Variant; Index: PropIndices);
{Assignment should be made in order of importance as the first one in
 predominates}
var
  I: FIIndex;
begin
  if not Originals[Index] then
  begin
    Props[Index] := Item;
    Originals[Index] := True;
    if InLink then
      case Index of
        Color:
          for I := LFont to HVFont do
            FIArray.Ar[I].iColor := Item;
        FontSize:
          for I := LFont to HVFont do
            FIArray.Ar[I].iSize := Item;
        FontFamily:
          for I := LFont to HVFont do
            FIArray.Ar[I].iName := Item;
      end;
  end;
end;

function TProperties.GetBackgroundImage(var Image: ThtString): Boolean;
begin
  if (VarIsStr(Props[BackgroundImage])) then
    if (Props[BackgroundImage] = 'none') then
    begin
      Image := '';
      Result := True;
    end
    else
    begin
      Image := ReadUrl(Props[BackgroundImage]);
      Result := Image <> '';
    end
  else
    Result := False;
end;

procedure TProperties.AssignCharSet(CS: TFontCharset);
begin
  AssignCharSetAndCodePage(CS, CharSetToCodePage(CS));
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
function TranslateCharset(CS: TFontCharset): TFontCharset;
// extracted from TProperties.AssignCharSetAndCodePage()
var
  Save: THandle;
  tm: TTextmetric;
  DC: HDC;
  Font: TFont;
begin
  if not CharsetPerCharset[CS].Inited then
  begin
    {the following makes sure the CharSet is available}
    CharsetPerCharset[CS].Inited := True;
    CharsetPerCharset[CS].Charset := CS;
    Font := TFont.Create;
    try
      Font.Name := '';
      Font.CharSet := CS;
      DC := GetDC(0);
      try
        Save := SelectObject(DC, Font.Handle);
        try
          GetTextMetrics(DC, tm);
          CharsetPerCharset[CS].Charset := tm.tmCharSet;
        finally
          SelectObject(DC, Save);
        end;
      finally
        ReleaseDC(0, DC);
      end;
    finally
      Font.Free;
    end;
  end;
  Result := CharsetPerCharset[CS].Charset;
end;

//-- BG ---------------------------------------------------------- 25.12.2010 --
procedure TProperties.AssignCharSetAndCodePage(CS: TFontCharset; CP: Integer);
var
  IX: FIIndex;
begin
  if (FCharSet <> CS) or (FCodePage <> CP) then
  begin
    case CS of
      EastEurope8859_2:
        FCharSet := TranslateCharset(EASTEUROPE_CHARSET);

      DEFAULT_CHARSET:
        FCharSet := CS;
    else
      FCharSet := TranslateCharset(CS);
    end;

    if Assigned(FIArray) then
      for IX := LFont to HVFont do
        FIArray.Ar[IX].iCharset := CharSet;

    FCodePage := CP;
  end;
end;

procedure TProperties.AssignCodePage(const CP: Integer);
begin
  case CP of
    CP_UTF8,
    CP_UTF16LE,
    CP_UTF16BE:
      AssignCharSetAndCodePage(DEFAULT_CHARSET, CP);
  else
    AssignCharSetAndCodePage(CodePageToCharSet(CP), CP);
  end;
end;

{----------------TProperties.GetBackgroundPos}

procedure TProperties.GetBackgroundPos(EmSize, ExSize: Integer; out P: PtPositionRec);
var
  S: array[1..2] of ThtString;
  Tmp: ThtString;
  I, N, XY: Integer;
  PXY: PPositionRec;
begin
//BG, 29.08.2009: thanks to SourceForge user 'bolex': 'not' was missing.
  if (not VarIsStr(Props[BackgroundPosition])) then
  begin
    P.X.PosType := bpDim;
    P.X.Value := 0;
    P.Y := P.X;
  end
  else
  begin
    Tmp := Trim(Props[BackgroundPosition]);
    N := Pos(' ', Tmp);
    if N > 0 then
    begin
      S[1] := System.Copy(Tmp, 1, N - 1);
      S[2] := Trim(system.Copy(Tmp, N + 1, 255));
      N := 2;
    end
    else
    begin
      S[1] := Tmp;
      N := 1;
    end;
    I := 1;
    XY := 1; {X}
    while I <= N do
    begin
      if XY = 1 then
        PXY := @P.X
      else
        PXY := @P.Y;
      PXY.PosType := bpDim;
      if S[I] = 'center' then
        PXY.PosType := bpCenter
      else if Pos('%', S[I]) > 0 then
        PXY.PosType := bpPercent
      else if S[I] = 'left' then
      begin
        if XY = 2 then {entered in reverse direction}
          P.Y := P.X;
        P.X.PosType := bpLeft;
      end
      else if S[I] = 'right' then
      begin
        if XY = 2 then
          P.Y := P.X;
        P.X.PosType := bpRight;
      end
      else if S[I] = 'top' then
      begin
        P.Y.PosType := bpTop;
        if XY = 1 then
          Dec(XY); {read next one into X}
      end
      else if S[I] = 'bottom' then
      begin
        P.Y.PosType := bpBottom;
        if XY = 1 then
          Dec(XY);
      end;
      if PXY.PosType in [bpDim, bpPercent] then
      begin
        PXY.Value := LengthConv(S[I], False, 100, EmSize, ExSize, 0);
      end;
      Inc(I);
      Inc(XY);
    end;
    if N = 1 then
      if XY = 2 then
        P.Y.PosType := bpCenter
      else
        P.X.PosType := bpCenter; {single entry but it was a Y}
  end;
  P.X.RepeatD := True;
  P.Y.RepeatD := True;
  if (VarIsStr(Props[BackgroundRepeat])) then
  begin
    Tmp := Trim(Props[BackgroundRepeat]);
    if Tmp = 'no-repeat' then
    begin
      P.X.RepeatD := False;
      P.Y.RepeatD := False;
    end
    else if Tmp = 'repeat-x' then
      P.Y.RepeatD := False
    else if Tmp = 'repeat-y' then
      P.X.RepeatD := False;
  end;
  P.X.Fixed := False;
  if (VarIsStr(Props[BackgroundAttachment])) and
    (Trim(Props[BackgroundAttachment]) = 'fixed') then
    P.X.Fixed := True;
  P.Y.Fixed := P.X.Fixed;
end;

function TProperties.GetVertAlign(var Align: AlignmentType): Boolean;
{note:  'top' should have a catagory of its own}
var
  S: ThtString;
begin
  if (VarIsStr(Props[VerticalAlign])) then
  begin
    Result := True;
    S := Props[VerticalAlign];
    if (S = 'top') or (S = 'text-top') then
      Align := ATop
    else if S = 'middle' then
      Align := AMiddle
    else if S = 'baseline' then
      Align := ABaseline
    else if (S = 'bottom') then
      Align := ABottom
    else if (S = 'sub') then
      Align := ASub
    else if (S = 'super') then
      Align := ASuper
    else
      Result := False;
  end
  else
    Result := False;
end;

function TProperties.IsOverflowHidden: Boolean;
begin
  Result := (VarIsStr(Props[OverFlow])) and (Props[OverFlow] = 'hidden');
end;

function TProperties.GetFloat(var Align: AlignmentType): Boolean;
var
  S: ThtString;
begin
  if (VarIsStr(Props[Float])) then
  begin
    Result := True;
    S := Props[Float];
    if S = 'left' then
      Align := ALeft
    else if S = 'right' then
      Align := ARight
    else if S = 'none' then
      Align := ANone
    else
      Result := False;
  end
  else
    Result := False;
end;

function TProperties.GetClear(var Clr: ClearAttrType): Boolean;
var
  S: ThtString;
begin
  if (VarIsStr(Props[Clear])) then
  begin
    Result := True;
    S := Props[Clear];
    if (S = 'left') then
      Clr := clLeft
    else if S = 'right' then
      Clr := clRight
    else if S = 'both' then
      Clr := clAll
    else if S = 'none' then
      Clr := clrNone
    else
      Result := False;
    Props[Clear] := Unassigned; {allow only one read}
  end
  else
    Result := False;
end;

//-- BG ---------------------------------------------------------- 15.09.2009 --
function TProperties.GetDisplay: TPropDisplay;
var
  S: ThtString;
begin
  if VarIsStr(Props[piDisplay]) then
  begin
    S := Props[piDisplay];
    Result := high(TPropDisplay);
    while Result > pdUnassigned do
    begin
      if S = CPropDisplay[Result] then
        exit;
      dec(Result);
    end;
  end
  else
    Result := pdUnassigned;
end;

function TProperties.GetListStyleType: ListBulletType;
var
  I: ListBulletType;

begin
  if VarIsStr(Props[ListStyleType]) then
    for I := Low(ListBulletType) to High(ListBulletType) do
      if CListStyleType[I] = Props[ListStyleType] then
      begin
        Result := I;
        Exit;
      end;
  Result := lbBlank;
end;

function TProperties.GetListStyleImage: ThtString;
begin
  Result := ReadURL(Props[ListStyleImage])
end;

function TProperties.GetPosition: PositionType;
begin
  Result := posStatic;
  if VarIsStr(Props[Position]) then
  begin
    if Props[Position] = 'absolute' then
      Result := posAbsolute
    else if Props[Position] = 'fixed' then
      Result := posFixed
    else if Props[Position] = 'relative' then
      Result := posRelative;
  end;
end;

function TProperties.GetVisibility: VisibilityType;
begin
  Result := viVisible;
  if VarType(Props[Visibility]) in varInt then
    if Props[Visibility] = viHidden then
      Result := viHidden;
end;

function TProperties.GetZIndex: Integer;
begin
  Result := 0;
  if VarType(Props[ZIndex]) in VarInt then
    Result := Props[ZIndex]
  else if VarIsStr(Props[ZIndex]) then
    Result := StrToIntDef(Props[ZIndex], 0);
end;

//-- BG ---------------------------------------------------------- 15.10.2010 --
function TProperties.HasBorderWidth: Boolean;
begin
  Result := not (VarIsIntNull(Props[BorderTopWidth]) or VarIsEmpty(Props[BorderTopWidth]))
         or not (VarIsIntNull(Props[BorderRightWidth]) or VarIsEmpty(Props[BorderRightWidth]))
         or not (VarIsIntNull(Props[BorderBottomWidth]) or VarIsEmpty(Props[BorderBottomWidth]))
         or not (VarIsIntNull(Props[BorderLeftWidth]) or VarIsEmpty(Props[BorderLeftWidth]));
end;

function TProperties.Collapse: Boolean;
begin
  Result := (VarIsStr(Props[BorderCollapse])) and (Props[BorderCollapse] = 'collapse');
end;

function TProperties.GetLineHeight(NewHeight: Integer): Integer;
var
  V: Double;
  Code: Integer;
begin
  if VarIsStr(Props[LineHeight]) then
  begin
    Val(Props[LineHeight], V, Code);
    if Code = 0 then {a numerical entry with no 'em', '%', etc.  Use the new font height}
      Result := Round(V * NewHeight)
    else
    {note: 'normal' yields -1 in the next statement}
      Result := LengthConv(Props[LineHeight], True, EmSize, EmSize, ExSize, -1);
  end
  else
    Result := -1;
end;

function TProperties.GetTextIndent(out PC: Boolean): Integer;
var
  I: Integer;
begin
  PC := False;
  if VarIsStr(Props[TextIndent]) then
  begin
    I := Pos('%', Props[TextIndent]);
    if I > 0 then
    begin
      PC := True; {return value in percent}
      Result := LengthConv(Props[TextIndent], True, 100, 0, 0, 0);
    end
    else
      Result := LengthConv(Props[TextIndent], False, 0, EmSize, EmSize, 0);
  end
  else
    Result := 0;
end;

function TProperties.GetTextTransform: TextTransformType;
begin
  try
    if VarType(Props[TextTransform]) in VarInt then
      Result := Props[TextTransform]
    else
      Result := txNone;
  except
    Result := txNone;
  end;
end;

function TProperties.GetFontVariant: ThtString;
begin
  try
    if VarIsStr(Props[FontVariant]) then
      Result := Props[FontVariant]
    else
      Result := 'normal';
  except
    Result := 'normal';
  end;
end;

procedure TProperties.GetPageBreaks(out Before, After, Intact: Boolean);
begin
  Before := (VarIsStr(Props[PageBreakBefore])) and (Props[PageBreakBefore] = 'always');
  After := (VarIsStr(Props[PageBreakAfter])) and (Props[PageBreakAfter] = 'always');
  Intact := (VarIsStr(Props[PageBreakInside])) and (Props[PageBreakInside] = 'avoid');
end;

function TProperties.GetBackgroundColor: TColor;
begin
  if (VarType(Props[BackgroundColor]) in varInt) and Originals[BackgroundColor] then
      {Originals to prevent fonts from getting inherited background color}
    Result := Props[BackgroundColor]
  else
    Result := clNone;
end;

function TProperties.GetOriginalForegroundColor: TColor;
begin {return a color only if it hasn't been inherited}
  if (VarType(Props[Color]) in varInt) and Originals[Color] then
    Result := Props[Color]
  else
    Result := clNone;
end;


//-- JPM --------------------------------------------------------- 03.02-2012 --

function TryStrToBoxSizing(const Str: ThtString; out ABoxSizing: BoxSizingType): Boolean;
var
  I: BoxSizingType;
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

//-- BG ---------------------------------------------------------- 16.03.2011 --
function TryStrToBorderStyle(const Str: ThtString; out BorderStyle: BorderStyleType): Boolean;
var
  I: BorderStyleType;
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

function BorderStyleFromString(const S: ThtString): BorderStyleType;
begin
  if not TryStrToBorderStyle(S, Result) then
    Result := bssNone;
end;

//-- BG ---------------------------------------------------------- 12.03.2011 --
function TProperties.GetBorderStyle(Index: PropIndices; var BorderStyle: BorderStyleType): Boolean;
// Returns True, if there is a valid border style property. 
begin
  Result := False;
  if VarIsStr(Props[Index]) then
    Result := TryStrToBorderStyle(Props[Index], BorderStyle)
  else if VarType(Props[Index]) in varInt then
    if (Props[Index] >= low(BorderStyleType)) and (Props[Index] <= high(BorderStyleType)) then
    begin
      BorderStyle := BorderStyleType(Props[Index]);
      Result := True;
    end;
end;

function TProperties.GetBoxSizing(var VBoxSizing: BoxSizingType): Boolean;
begin
  Result := TryStrToBoxSizing(Props[BoxSizing], VBoxSizing);
end;

function TProperties.BorderStyleNotBlank: Boolean;
{was a border of some type (including bssNone) requested?}
var
  Dummy: BorderStyleType;
begin
  Dummy := bssNone;
  Result :=
    GetBorderStyle(BorderTopStyle, Dummy) or
    GetBorderStyle(BorderRightStyle, Dummy) or
    GetBorderStyle(BorderBottomStyle, Dummy) or
    GetBorderStyle(BorderLeftStyle, Dummy);
end;

//-- BG ---------------------------------------------------------- 12.03.2011 --
function TProperties.HasBorderStyle: Boolean;
// Returns True, if at least one border has a style.
var
  Dummy: BorderStyleType;
begin
  Dummy := bssNone;

  GetBorderStyle(BorderTopStyle, Dummy);
  Result := Dummy <> bssNone;
  if Result then
    exit;

  GetBorderStyle(BorderRightStyle, Dummy);
  Result := Dummy <> bssNone;
  if Result then
    exit;

  GetBorderStyle(BorderBottomStyle, Dummy);
  Result := Dummy <> bssNone;
  if Result then
    exit;

  GetBorderStyle(BorderLeftStyle, Dummy);
  Result := Dummy <> bssNone;
end;

procedure TProperties.SetFontBG;
{called for font tags like <b>, <small>, etc.  Sets the font background color.}
begin
  if (VarType(Props[BackgroundColor]) in varInt) and Originals[BackgroundColor] then
    FontBG := Props[BackgroundColor];
end;

//-- BG ---------------------------------------------------------- 23.11.2009 --
function TProperties.ShowEmptyCells: Boolean;
begin
  Result := (VarIsStr(Props[piEmptyCells])) and (Props[piEmptyCells] = 'show');
end;

procedure ConvVertMargins(const VM: TVMarginArray;
  BaseHeight, EmSize, ExSize: Integer;
  var M: TMarginArray; out TopAuto, BottomAuto: Boolean);

  function Convert(V: Variant; out IsAutoParagraph: Boolean): Integer;
  begin
    IsAutoParagraph := False;
    if VarIsStr(V) then
      Result := LengthConv(V, False, BaseHeight, EmSize, ExSize, 0) {Auto will be 0}
    else if VarType(V) in varInt then
    begin
      if V = IntNull then
        Result := 0
      else if V = AutoParagraph then
      begin
        Result := ParagraphSpace;
        IsAutoParagraph := True;
      end
      else
        Result := V;
    end
    else
      Result := 0;
  end;

begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod('ConvVertMargins');
  CodeSiteLogging.CodeSite.Send('BaseHeight  = [%d]',[BaseHeight]);
  CodeSiteLogging.CodeSite.Send('EmSize      = [%d]',[EmSize]);
  CodeSiteLogging.CodeSite.Send('ExSize      = [%d]',[ExSize]);
  StyleUn.LogTVMarginArray(VM,'VM');
  {$ENDIF}
  M[MarginTop] := Convert(VM[MarginTop], TopAuto);
  M[MarginBottom] := Convert(VM[MarginBottom], BottomAuto);
    {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.Send('Results');
  CodeSiteLogging.CodeSite.ExitMethod('ConvVertMargins');
  StyleUn.LogTMarginArray(M,'M');
  {$ENDIF}
end;

//-- BG ---------------------------------------------------------- 25.04.2012 --
function IsAuto(const Value: Variant): Boolean;
begin
  Result := Value = Auto;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function VarIsIntNull(const Value: Variant): Boolean;
begin
  Result := (VarType(Value) in varInt) and (Value = IntNull);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function VarIsAuto(const Value: Variant): Boolean;
begin
  Result := (VarType(Value) in varInt) and (Value = Auto);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function VMargToMarg(const Value: Variant; Relative: Boolean; Base, EmSize, ExSize, Default: Integer): Integer;
begin
  if VarIsStr(Value) then
    Result := LengthConv(Value, Relative, Base, EmSize, ExSize, Default)
  else if (VarType(Value) in varInt) and (Value <> IntNull) then
    Result := Value
  else
    Result := Default;
end;

  procedure ApplyBoxWidthSettings(var AMarg : TMarginArray; var VMinWidth, VMaxWidth : Integer; const AUseQuirksMode : Boolean);
  begin
    {Important!!!

    You have to do this with settings.  This is only for FindWidth methods}
    if not AUseQuirksMode then begin
      if AMarg[piMaxWidth] > 0 then begin
        VMaxWidth := AMarg[piMaxWidth];
        AMarg[piWidth] := Min(AMarg[piMaxWidth],AMarg[piWidth]);
      end;
      if AMarg[piMinWidth] > 0 then begin
        AMarg[piWidth] := Max(AMarg[piMinWidth],AMarg[piWidth]);
        VMinWidth := AMarg[piWidth];
      end;
    end;
  end;

procedure ApplyBoxSettings(var AMarg : TMarginArray; const AUseQuirksMode : Boolean);
begin
  if AUseQuirksMode then begin
    ApplyBorderBoxModel(AMarg);
  end else begin
    {JPM:  This test is here to prevent AMarg[piWidth] from being ruined
    if it is set to Auto.  If it is ruined, AutoCount might be incremented
    correctly causing a rendering bug. }
    if AMarg[piWidth] > -1 then begin
    //width
      if AMarg[piMaxWidth] > 0 then begin
        AMarg[piWidth] := Min(AMarg[piWidth],AMarg[piMaxWidth]);
      end;

      if AMarg[piMinWidth] > 0 then begin
        AMarg[piWidth] := Max(AMarg[piWidth],AMarg[piMinWidth]);
      end;
    end;
    //height
    if AMarg[piMaxHeight] > 0 then begin
      AMarg[piHeight] := Min(AMarg[piHeight],AMarg[piMaxHeight]);
    end;
    if AMarg[piMinHeight] > 0 then begin
      AMarg[piHeight] := Max(AMarg[piHeight],AMarg[piMinHeight]);
    end;
    case AMarg[BoxSizing] of
        0 : ;
        1 : ApplyBorderBoxModel(AMarg);
    else
    end;
  end;
end;

procedure ApplyBorderBoxModel(var AMarg : TMarginArray);
begin
  if AMarg[piWidth] > -1 then begin
    AMarg[piWidth] := AMarg[piWidth] -
      (AMarg[BorderLeftWidth] + AMarg[BorderRightWidth] +
       AMarg[PaddingLeft] + AMarg[PaddingRight]);
  end;
  if AMarg[piHeight] > -1 then begin
    AMarg[piHeight] := AMarg[piHeight] -
      (AMarg[BorderTopWidth] + AMarg[BorderBottomWidth] +
       AMarg[PaddingTop] + AMarg[PaddingBottom]);
  end;
end;

{----------------ConvMargArray}

procedure ConvMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize: Integer;
  BorderWidth: Integer; out AutoCount: Integer; var M: TMarginArray);
{This routine does not do MarginTop and MarginBottom as they are done by ConvVertMargins}
var
  I: PropIndices;
  Base: Integer;
  LBoxSizing : BoxSizingType;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod('ConvMargArray');
 CodeSiteLogging.CodeSite.AddSeparator;
 CodeSiteLogging.CodeSite.Send('Input');

 CodeSiteLogging.CodeSite.AddSeparator;
   LogTVMarginArray(VM,'VM');
   CodeSiteLogging.CodeSite.Send('BaseWidth   = [%d]',[BaseWidth]);
   CodeSiteLogging.CodeSite.Send('BaseHeight  = [%d]',[BaseHeight]);
   CodeSiteLogging.CodeSite.Send('BorderWidth  = [%d]',[BorderWidth]);

   CodeSiteLogging.CodeSite.Send('EmSize      = [%d]',[EmSize]);
   CodeSiteLogging.CodeSite.Send('ExSize      = [%d]',[ExSize]);
   CodeSiteLogging.CodeSite.Send('BorderWidth = [%d]',[BorderWidth]);
  {$ENDIF}
  AutoCount := 0; {count of 'auto's in width items}
  for I := Low(VM) to High(VM) do
  begin
    case I of
      piHeight, TopPos:
        Base := BaseHeight
    else
      Base := BaseWidth;
    end;
    case I of
      BackgroundColor, BorderTopColor..BorderLeftColor:
        begin
          if VarType(VM[I]) <= VarNull then
            M[I] := clNone
          else
            M[I] := VM[I];
        end;
      BorderTopWidth..BorderLeftWidth:
        begin
          if VM[PropIndices(Ord(BorderTopStyle) + (Ord(I) - Ord(BorderTopWidth)))] = bssNone then
            M[I] := 0
          else
          begin
            if VarIsStr(VM[I]) then
            begin
              if VM[I] = 'thin' then
                M[I] := 2
              else if VM[I] = 'medium' then
                M[I] := 4
              else if VM[I] = 'thick' then
                M[I] := 6
              else
                M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, BorderWidth); {Auto will be 4}
            end
            else if (VarType(VM[I]) in varInt) then
            begin
              if (VM[I] = IntNull) then
                M[I] := BorderWidth
              else
                M[I] := VM[I];
            end;
          end;
        end;
      piMinHeight, piMaxHeight,
      piHeight:
        begin
          if VarIsStr(VM[I]) then
          begin
            M[I] := LengthConv(VM[I], False, Base, EmSize, ExSize, 0); {Auto will be 0}
            if Pos('%', VM[I]) > 0 then {include border in % heights}
              M[I] := M[I] - M[BorderTopWidth] - M[BorderBottomWidth] - M[PaddingTop] - M[PaddingBottom];
          end
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := 0
            else
              M[I] := VM[I];
          end
          else
            M[I] := 0;
        end;
      PaddingTop..PaddingLeft:
        begin
          if VarIsStr(VM[I]) then
          begin
            M[I] := LengthConv(VM[I], False, Base, EmSize, ExSize, 0); {Auto will be 0}
          end
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := 0
            else
              M[I] := VM[I];
          end
          else
            M[I] := 0;
        end;
      TopPos, RightPos, BottomPos, LeftPos:
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, Base, EmSize, ExSize, Auto) {Auto will be Auto}
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := Auto
            else
              M[I] := VM[I];
          end
          else
            M[I] := Auto;
        end;
      BoxSizing :
        if TryStrToBoxSizing(VM[I],LBoxSizing) then begin
           M[I] := Ord(LBoxSizing);
        end else begin
          //assume content-box
           M[I] := 0;
        end;
      MarginLeft, MarginRight:
        begin
          if VarIsStr(VM[I]) then
          begin
            if VM[I] = 'auto' then
            begin
              M[I] := Auto;
              Inc(AutoCount);
            end
            else
              M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0);
          end
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := 0
            else
              M[I] := VM[I];
          end
          else
            M[I] := 0;
        end;
      piMinWidth,
      piMaxWidth :
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto)
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := 0
            else
              M[I] := VM[I];
          end
          else
            M[I] := 0;
        end;
      piWidth:
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto)
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := Auto
            else
              M[I] := VM[I];
          end
          else
            M[I] := Auto;
          if M[I] = Auto then
            Inc(AutoCount);
        end;
      MarginTop, MarginBottom:
//        if VarType(VM[I]) in varInt then
//          case VM[I] of
//            AutoParagraph: ; // do nothing
//
//            IntNull:
//              M[I] := 0;
//          else
//            M[I] := VM[I];
//          end
//        else if VarIsStr(VM[I]) then
//          M[I] := LengthConv(VM[I], False, BaseHeight, EmSize, ExSize, 0)
//        else
//          M[I] := 0
        ;
    else
      begin
        if VarIsStr(VM[I]) then
          M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0)
        else if VarType(VM[I]) in varInt then
        begin
          if VM[I] = IntNull then
            M[I] := 0
          else
            M[I] := VM[I];
        end
        else
          M[I] := 0;
      end;
    end;
  end;
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.Send('Results');
  LogTMarginArray(M,'M');
  CodeSiteLogging.CodeSite.Send('AutoCount  = [%d]',[AutoCount]);
  CodeSiteLogging.CodeSite.ExitMethod('ConvMargArray');
  {$ENDIF}
end;

procedure ConvMargArrayForCellPadding(const VM: TVMarginArray; EmSize,
  ExSize: Integer; var M: TMarginArray);
{Return negative for no entry or percent entry}
var
  I: PropIndices;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod('ConvMargArrayForCellPadding');
  CodeSiteLogging.CodeSite.AddSeparator;
  LogTVMarginArray(VM,'VM');
  CodeSiteLogging.CodeSite.Send('EmSize      = [%d]',[EmSize]);
  CodeSiteLogging.CodeSite.Send('ExSize      = [%d]',[ExSize]);
  {$ENDIF}
  for I := PaddingTop to PaddingLeft do
    if VarIsStr(VM[I]) then
      M[I] := LengthConv(VM[I], False, -100, EmSize, ExSize, 0) {Auto will be 0}
    else if VarType(VM[I]) in varInt then
    begin
      if VM[I] = IntNull then
        M[I] := -1
      else
        M[I] := VM[I];
    end
    else
      M[I] := -1;
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  LogTMarginArray(M,'M');
  CodeSiteLogging.CodeSite.ExitMethod('ConvMargArrayForCellPadding');
  {$ENDIF}
end;

{----------------ConvInlineMargArray}

procedure ConvInlineMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize,
  ExSize: Integer; {BStyle: BorderStyleType;} out M: TMarginArray);
{currently for images, form controls.  BaseWidth/Height and BStyle currently not supported}
var
  I: PropIndices;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod('ConvInlineMargArray');
  CodeSiteLogging.CodeSite.AddSeparator;
  LogTVMarginArray(VM,'VM');
  CodeSiteLogging.CodeSite.Send('BaseWidth   = [%d]',[BaseWidth]);
  CodeSiteLogging.CodeSite.Send('BaseHeight  = [%d]',[BaseHeight]);

  CodeSiteLogging.CodeSite.Send('EmSize      = [%d]',[EmSize]);
  CodeSiteLogging.CodeSite.Send('ExSize      = [%d]',[ExSize]);
  {$ENDIF}
  for I := Low(VM) to High(VM) do
    case I of
      piHeight, piWidth:
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto) {Auto will be Auto}
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := IntNull
            else
              M[I] := VM[I];
          end
          else
            M[I] := IntNull;
        end;
      piMinHeight, piMinWidth, piMaxHeight, piMaxWidth,

      MarginLeft, MarginRight, MarginTop, MarginBottom:
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0) {auto is 0}
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := IntNull
            else
              M[I] := VM[I];
          end
          else
            M[I] := IntNull;
        end;
      BorderTopWidth..BorderLeftWidth:
        begin
          if VM[PropIndices(Ord(BorderTopStyle) + (Ord(I) - Ord(BorderTopWidth)))] = bssNone then
            M[I] := 0
          else
          begin
            if VarIsStr(VM[I]) then
            begin
              if VM[I] = 'thin' then
                M[I] := 2
              else if VM[I] = 'medium' then
                M[I] := 4
              else if VM[I] = 'thick' then
                M[I] := 6
              else
                M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 4); {Auto will be 4}
            end
            else if (VarType(VM[I]) in varInt) then
            begin
              if (VM[I] = IntNull) then
                M[I] := 4
              else
                M[I] := VM[I];
            end;
          end;
        end;
    else
      ; {remaining items unsupported/unused}
    end;
  {$IFDEF JPM_DEBUGGING}
   CodeSiteLogging.CodeSite.AddSeparator;
   LogTMarginArray(M,'M');
   CodeSiteLogging.CodeSite.ExitMethod('ConvInlineMargArray');
  {$ENDIF}
end;

{----------------TProperties.Combine}

procedure TProperties.Combine(Styles: TStyleList;
  const Tag, AClass, AnID, PSeudo, ATitle: ThtString; AProp: TProperties; ParentIndexInPropStack: Integer);
{When called, this TProperties contains the inherited properties.  Here we
 add the ones relevant to this item. AProp are TProperties gleaned from the
 Style= attribute. AClass may be a multiple class like class="ab.cd"}

  procedure CombineX(Styles: TStyleList;
    const Tag, AClass, AnID, PSeudo, ATitle: ThtString; AProp: TProperties);
  {When called, this TProperties contains the inherited properties.  Here we
   add the ones relevant to this item. AProp are TProperties gleaned from the
   Style= attribute.}
  var
    OldSize: Double;
    NoHoverVisited: Boolean;

    procedure Merge(Source: TProperties);
    var
      Index: PropIndices;
      I: FIIndex;
      Wt: Integer;
      S1: ThtString;
    begin
{$ifdef JPM_DEBUGGING}
      CodeSiteLogging.CodeSite.EnterMethod('TProperties.Combine CombineX Merge');
      CodeSiteLogging.CodeSite.Send('Parameters');
      CodeSiteLogging.CodeSite.AddSeparator;
      LogProperties(Source,'Source');
{$endif}
      for Index := Low(Index) to High(PropIndices) do
        if (VarType(Source.Props[Index]) <> varEmpty) and (Vartype(Source.Props[Index]) <> varNull) then
          case Index of
            MarginTop..LeftPos:
              if VarIsStr(Source.Props[Index]) then // if VarType(Source.Props[Index]) = VarString then
              begin
                Props[Index] := Source.Props[Index];
                Originals[Index] := True;
              end
              else if Source.Props[Index] <> IntNull then
              begin
                Props[Index] := Source.Props[Index];
                Originals[Index] := True;
              end;
            FontFamily, FontSize, FontStyle, FontWeight, Color, BackgroundColor,
              TextDecoration, LetterSpacing:
              begin
                Originals[Index] := True;
                Props[Index] := Source.Props[Index];
                if InLink then
                  for I := LFont to HVFont do
                    with FIArray.Ar[I] do
                      case Index of
                        FontFamily:
                          begin
                            S1 := ReadFontName(Props[FontFamily]);
                            if S1 <> '' then
                              iName := S1;
                          end;

                        FontSize:
                          iSize := FontSizeConv(Props[FontSize], iSize, FUseQuirksMode);

                        Color:
                          iColor := Props[Color];

                        BackgroundColor:
                          ibgColor := Props[BackgroundColor];

                        FontStyle:
                          if (Props[FontStyle] = 'italic') or (Props[FontStyle] = 'oblique') then
                            iStyle := iStyle + [fsItalic]
                          else if Props[FontStyle] = 'normal' then
                            iStyle := iStyle - [fsItalic];

                        FontWeight:
                          if Pos('bold', Props[FontWeight]) > 0 then
                            iStyle := iStyle + [fsBold]
                          else if Pos('normal', Props[FontWeight]) > 0 then
                            iStyle := iStyle - [fsBold]
                          else
                          begin
                            Wt := StrToIntDef(Props[FontWeight], 0);
                            if Wt >= 600 then
                              iStyle := iStyle + [fsBold];
                          end;

                        TextDecoration:
                          if Props[TextDecoration] = 'underline' then
                            iStyle := iStyle + [fsUnderline]
                          else if Props[TextDecoration] = 'line-through' then
                            iStyle := iStyle + [fsStrikeOut]
                          else if Props[TextDecoration] = 'none' then
                            iStyle := iStyle - [fsStrikeOut, fsUnderline];
                            
                        LetterSpacing:
                          iCharExtra := Props[LetterSpacing];
                      end;
              end
          else
            begin
              Props[Index] := Source.Props[Index];
              Originals[Index] := True; {it's defined for this item, not inherited}
            end;
          end;
{$ifdef JPM_DEBUGGING}
      CodeSiteLogging.CodeSite.AddSeparator;
      CodeSiteLogging.CodeSite.Send('Results');
      CodeSiteLogging.CodeSite.AddSeparator;
      LogProperties(Self,'Self');
      CodeSiteLogging.CodeSite.ExitMethod('TProperties.Combine CombineX Merge');
{$endif}
    end;

    function CheckForContextual(I: Integer): Boolean;
    {process contextual selectors}
    var
      J, K, N: Integer;
      A: array[1..10] of record
        Tg, Cl, ID, PS: ThtString;
        gt: Boolean;
      end;
      MustMatchParent: Boolean;

      procedure Split(S: ThtString);
      var
        I, J: Integer;
      begin
        N := 1; {N is number of selectors in contextual ThtString}
        I := Pos(' ', S);
        while (I > 0) and (N < 10) do
        begin
          A[N].Tg := System.Copy(S, 1, I - 1);
          Delete(S, 1, I);
          S := Trim(S);
          Inc(N);
          I := Pos(' ', S);
        end;

        A[N].Tg := S;
        if N >= 2 then
          while Length(A[2].Tg) > 0 do
          begin
            case A[2].Tg[1] of
              '0'..'9':
                Delete(A[2].Tg, 1, 1); {remove the sort digit}
            else
              break;
            end;
          end;

        for I := 1 to N do
        begin
          J := Pos('>', A[I].Tg);
          if I > 1 then
            A[I - 1].gt := J > 0;
          if J > 0 then
            Delete(A[I].Tg, J, 1);
          J := Pos(':', A[I].Tg);
          if J > 0 then
          begin
            A[I].PS := System.Copy(A[I].Tg, J + 1, Length(A[I].Tg));
            A[I].Tg := System.Copy(A[I].Tg, 1, J - 1);
          end
          else
            A[I].PS := '';
          J := Pos('#', A[I].Tg);
          if J > 0 then
          begin
            A[I].ID := System.Copy(A[I].Tg, J + 1, Length(A[I].Tg));
            A[I].Tg := System.Copy(A[I].Tg, 1, J - 1);
          end
          else
            A[I].ID := '';
          J := Pos('.', A[I].Tg);
          if J > 0 then
          begin
            A[I].Cl := System.Copy(A[I].Tg, J + 1, Length(A[I].Tg));
            A[I].Tg := System.Copy(A[I].Tg, 1, J - 1);
          end
          else
            A[I].Cl := '';
        end;
      end;

      function PartOf(const S1, S2: ThtString): Boolean;
      {see if all classes in S1 are present in S2.  Classes are separated by '.'}
      var
        SL1, SL2: ThtStringList;
        J, X: Integer;

        function FormStringList(S: ThtString): ThtStringList;
        {construct a ThtStringList from classes in ThtString S}
        var
          I: Integer;
        begin
          Result := ThtStringList.Create;
          Result.Sorted := True;
          I := Pos('.', S);
          while I >= 1 do
          begin
            Result.Add(System.Copy(S, 1, I - 1));
            Delete(S, 1, I);
            I := Pos('.', S);
          end;
          Result.Add(S);
        end;

      begin {PartOf}
        SL1 := FormStringList(S1);
        try
          SL2 := FormStringList(S2);
          try
            Result := True; {assume all will be found}
            for J := 0 to SL1.Count - 1 do
              if not SL2.Find(SL1[J], X) then
              begin
                Result := False; {one is missing, return False}
                Break;
              end;
          finally
            SL2.Free;
          end;
        finally
          SL1.Free;
        end;
      end;

    begin
      Result := False;
      Split(Styles[I]); //split contextual selectors into parts in array A
      if (A[1].Tg <> Tag) and (A[1].Cl <> AClass) and (A[1].PS <> PSeudo) then
        Exit
      else
        Result := True;
      if (N > 1) //it's a contextual selector.  N is count of selectors
        and ((A[1].Tg = Tag) or (A[1].Tg = ''))
        and ((A[1].Cl = AClass) or (A[1].Cl = ''))
        and ((A[1].ID = AnID) or (A[1].ID = ''))
        and ((A[1].PS = PSeudo) or (A[1].PS = '') and (PSeudo = 'link')) then
      begin //look thru the stack to see if this contextual selector is appropriate
        K := 2; //K is selector index in the sequence
        J := ParentIndexInPropStack; //PropStack.Count - 2; // start on stack item below this one
        MustMatchParent := A[1].gt;
        while (K <= N) and (J >= 1) do
        begin
          with PropStack[J] do
            if ((A[K].Tg = PropTag) or (A[K].Tg = ''))
              and ((A[K].Cl = PropClass) or (A[K].Cl = '') or PartOf(A[K].Cl, PropClass))
              and ((A[K].ID = PropID) or (A[K].ID = ''))
              and ((A[K].PS = PropPseudo) or (A[K].PS = '')) then
            begin
              if K = N then //all parts of contextual selector match
                Merge(Styles.Objects[I] as TProperties);
              MustMatchParent := A[K].gt;
              Inc(K);
            end
            else if MustMatchParent then
              Break; {Didn't match}
          Dec(J);
        end;
      end
    end;

    procedure MergeItems(const Item: ThtString);
    {look up items in the Style list.  If found, merge them in this TProperties.
     Items may be duplicated in which case the last has priority.  Items may be
     simple tags like 'p', 'blockquote', 'em', etc or they may be more complex
     like  p.class, em#id, a.class:link, etc}
    var
      X: Integer;
    begin
      if Styles.Find(Item, X) then
      begin
        Merge(Styles.Objects[X] as TProperties);
        Inc(X);
        while (X < Styles.Count) and (Styles[X] = Item) do
        begin //duplicates, last one has highest priority
          Merge(Styles.Objects[X] as TProperties);
          Inc(X);
        end;
      end;
    end;

    //BG, 09.09.2010: extracted from below
    procedure MergeContextuals(Style: ThtString);
    var
      IX: Integer;
    begin
      Styles.Find(Style, IX); //place to start
      try
        while (IX < Styles.Count) and (Pos(Style, Styles[IX]) = 1) and CheckForContextual(IX) do
          Inc(IX);
      except
        raise;
      end;
    end;

  begin
    if FUseQuirksMode then begin
       if (Tag = 'td') or (Tag = 'th') then begin
          OldSize := DefPointSize;
       end else begin
          if (VarType(Props[FontSize]) in VarNum) and (Props[FontSize] > 0.0) then {should be true}
            OldSize := Props[FontSize]
          else
            OldSize := DefPointSize;
       end;
    end else begin
      if (VarType(Props[FontSize]) in VarNum) and (Props[FontSize] > 0.0) then {should be true}
        OldSize := Props[FontSize]
      else
        OldSize := DefPointSize;
    end;
  {Some hover and visited items adequately taken care of when link processed}
    NoHoverVisited := (Pseudo = '') or ((Pseudo <> 'hover') and (Pseudo <> 'visited'));

  // in the following, lowest priority on top, highest towards bottom.

    if (Tag = 'a') and (Pseudo <> '') then
      MergeItems('::' + Pseudo); {default Pseudo definition}

    if NoHoverVisited then
      MergeItems(Tag);

    if Pseudo <> '' then
      MergeItems(':' + Pseudo);

    if (AClass <> '') and NoHoverVisited then
    begin
      MergeItems('.' + AClass);
      MergeItems(Tag + '.' + AClass);
    end;

    if Pseudo <> '' then
    begin
      MergeItems(Tag + ':' + Pseudo);
      if AClass <> '' then
      begin
        MergeItems('.' + AClass + ':' + Pseudo);
        MergeItems(Tag + '.' + AClass + ':' + Pseudo);
      end;
    end;

    if AnID <> '' then
    begin
      MergeItems('#' + AnID);
      MergeItems(Tag + '#' + AnID);
      if AClass <> '' then
        MergeItems('.' + AClass + '#' + AnID);
      if Pseudo <> '' then
      begin
        MergeItems('#' + AnID + ':' + Pseudo);
        MergeItems(Tag + '#' + AnID + ':' + Pseudo);
      end;
      if AClass <> '' then
      begin
        MergeItems(Tag + '.' + AClass + '#' + AnID);
        if Pseudo <> '' then
        begin
          MergeItems('.' + AClass + '#' + AnID + ':' + Pseudo);
          MergeItems(Tag + '.' + AClass + '#' + AnID + ':' + Pseudo);
        end;
      end;
    end;

  {process the entries in Styles to see if they are contextual selectors}
    if NoHoverVisited then
      MergeContextuals(Tag + ' ');

    if Pseudo <> '' then
      MergeContextuals(':' + Pseudo + ' ');

    if (AClass <> '') and NoHoverVisited then
    begin
      MergeContextuals('.' + AClass + ' ');
      MergeContextuals(Tag + '.' + AClass + ' ');
    end;

    if Pseudo <> '' then
    begin
      MergeContextuals(Tag + ':' + Pseudo + ' ');
      if AClass <> '' then
      begin
        MergeContextuals('.' + AClass + ':' + Pseudo + ' ');
        MergeContextuals(Tag + '.' + AClass + ':' + Pseudo + ' ');
      end;
    end;

    if AnID <> '' then
    begin
      MergeContextuals('#' + AnID + ' ');
      MergeContextuals(Tag + '#' + AnID + ' ');
      if AClass <> '' then
        MergeContextuals('.' + AClass + '#' + AnID + ' ');
      if Pseudo <> '' then
      begin
        MergeContextuals('#' + AnID + ':' + Pseudo + ' ');
        MergeContextuals(Tag + '#' + AnID + ':' + Pseudo + ' ');
      end;
      if AClass <> '' then
      begin
        MergeContextuals(Tag + '.' + AClass + '#' + AnID + ' ');
        if Pseudo <> '' then
        begin
          MergeContextuals('.' + AClass + '#' + AnID + ':' + Pseudo + ' ');
          MergeContextuals(Tag + '.' + AClass + '#' + AnID + ':' + Pseudo + ' ');
        end;
      end;
    end;

    if AProp <> nil then //the Style= attribute
      Merge(AProp);

    if not (VarType(Props[FontSize]) in varNum) then {if still a ThtString, hasn't been converted}
      Props[FontSize] := FontSizeConv(Props[FontSize], OldSize, FUseQuirksMode);
  end;

var
  BClass, S: ThtString;
  I: Integer;
begin
  BClass := Trim(AClass);
  I := Pos('.', BClass);
  if I <= 0 then
    CombineX(Styles, Tag, BClass, AnID, PSeudo, '', AProp) {0 or 1 Class}
  else
  begin {more than one class}
    repeat
      S := System.Copy(BClass, 1, I - 1);
      CombineX(Styles, Tag, S, AnID, PSeudo, '', nil);
      Delete(BClass, 1, I);
      BClass := Trim(BClass);
      I := Pos('.', BClass);
    until I <= 0;
    CombineX(Styles, Tag, BClass, AnID, PSeudo, '', AProp);
    CombineX(Styles, Tag, AClass, AnID, PSeudo, '', AProp);
  end;
  PropTag := Tag;
  PropClass := AClass;
  PropID := AnID;
  PropPseudo := Pseudo;
  PropStyle := AProp;
  if ATitle <> '' then
    PropTitle := ATitle;
  if PSeudo = 'link' then
  begin
    if not Assigned(FIArray) then
      FIArray := TFontInfoArray.Create;
    CalcLinkFontInfo(Styles, PropStack.Count - 1);
    InLink := True;
  end;
end;

function TProperties.GetFont: TMyFont;
var
  Font: ThtFontInfo;
  Save: THandle;
  SaveCharSet: TFontCharSet;
  tm: TTextmetric;
  DC: HDC;
  V: Variant;
  SameFont: TMyFont;
begin {call only if all things valid}
  if TheFont = nil then
  begin
    Font := ThtFontInfo.Create;
    try
      GetSingleFontInfo(Font);
      SameFont := AllMyFonts.Find(Font);
      if SameFont = nil then
      begin
        SameFont := TMyFont.Create;
        SameFont.Name := Font.iName;
        SameFont.Height := -Round(Font.iSize * Screen.PixelsPerInch / 72);
        SameFont.Style := Font.iStyle;
        SameFont.Charset := Font.iCharSet;
        AllMyFonts.Add(SameFont);

        // If this is a Symbol charset, then keep it that way.
        // To check the font's real charset, use Default_Charset
        SaveCharSet := SameFont.CharSet;
        SameFont.CharSet := Default_Charset;
        DC := GetDC(0);
        try
          Save := SelectObject(DC, SameFont.Handle);
          try
            GetTextMetrics(DC, tm);
          finally
            SelectObject(DC, Save);
          end;
          if tm.tmCharset = Symbol_Charset then
            SameFont.Charset := Symbol_CharSet
          else
            SameFont.Charset := SaveCharSet;
          {now get the info on the finalized font}
          if SameFont.Charset <> Default_Charset then {else already have the textmetrics}
          begin
            Save := SelectObject(DC, SameFont.Handle);
            try
              GetTextMetrics(DC, tm);
            finally
              SelectObject(DC, Save);
            end;
          end;
        finally
          ReleaseDC(0, DC);
        end;
        {calculate EmSize with current font rather than inherited}
        SameFont.EmSize := tm.tmHeight - tm.tmInternalLeading;
        SameFont.ExSize := EmSize div 2; {apparently correlates with what browsers are doing}
        SameFont.tmHeight := tm.tmHeight;
        SameFont.tmDescent := tm.tmDescent;
        SameFont.tmExternalLeading := tm.tmExternalLeading;
        SameFont.tmMaxCharWidth := tm.tmMaxCharWidth;
        SameFont.tmAveCharWidth := tm.tmAveCharWidth;
        SameFont.tmCharset := tm.tmCharset;
      end;
      TheFont := TMyFont.Create;
      TheFont.Assign(SameFont);
      TheFont.bgColor := Font.ibgColor;
      TheFont.Color := Font.iColor;
      V := Font.iCharExtra;
    finally
      Font.Free;
    end;
    FEmSize := TheFont.EmSize;
    FExSize := TheFont.ExSize;
    if VarType(V) in VarInt then
      TheFont.CharExtra := V
    else if VarIsStr(V) then
      if V = 'normal' then
        TheFont.CharExtra := 0
      else
        TheFont.CharExtra := LengthConv(V, False, EmSize, EmSize, ExSize, 0)
    else
      TheFont.CharExtra := 0;
  end;
  Result := TMyFont.Create;
  Result.Assign(TheFont);
end;

{----------------RemoveQuotes}

function RemoveQuotes(const S: ThtString): ThtString;
{if ThtString is a quoted ThtString, remove the quotes (either ' or ")}
var
  L: Integer;
begin
  L := Length(S);
  if (L >= 2) and (S[L] = S[1]) and ((S[1] = '''') or (S[1] = '"')) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

{----------------ReadFontName}

function ReadFontName(S: ThtString): ThtString;
const
  AMax = 5;
var
  S1: ThtString;
  Done: Boolean;

  function NextFontName: ThtString;
  const
    Generic1: array[1..AMax] of ThtString = ('serif', 'monospace', 'sans-serif', 'cursive', 'Helvetica');
    Generic2: array[1..AMax] of ThtString = ('Times New Roman', 'Courier New', 'Arial', 'Lucida Handwriting', 'Arial');
  var
    I: Integer;
  begin
    I := Pos(',', S); {read up to the comma}
    if I > 0 then
    begin
      Result := Trim(System.Copy(S, 1, I - 1));
      Delete(S, 1, I);
    end
    else
    begin {last item}
      Result := Trim(S);
      S := '';
    end;

    for I := 1 to AMax do
      if CompareText(Result, Generic1[I]) = 0 then
      begin
        Result := Generic2[I];
        break;
      end;

    Result := RemoveQuotes(Result);
  end;

begin
  Done := False;
  S1 := NextFontName;
  while (S1 <> '') and not Done do
  begin
    Done := Screen.Fonts.IndexOf(S1) >= 0;
    if Done then
      Result := S1
    else
      S1 := NextFontName;
  end;
end;

{----------------TProperties.GetSingleFontInfo}

procedure TProperties.GetSingleFontInfo(var Font: ThtFontInfo);
var
  Wt: Integer;
  Style: TFontStyles;
begin {call only if all things valid}
  Font.ibgColor := FontBG;
  Font.iColor := Props[Color];
  Style := [];
  if Pos('bold', Props[FontWeight]) > 0 then
    Include(Style, fsBold)
  else
  begin
    Wt := StrToIntDef(Props[FontWeight], 0);
    if Wt >= 600 then
      Include(Style, fsBold);
  end;
  if (Props[FontStyle] = 'italic') or (Props[FontStyle] = 'oblique') then
    Include(Style, fsItalic);
  if Props[TextDecoration] = 'underline' then
    Include(Style, fsUnderline)
  else if Props[TextDecoration] = 'line-through' then
    Include(Style, fsStrikeOut);
  Font.iStyle := Style;
  Font.iSize := Props[FontSize];
  Font.iCharset := CharSet;
  Font.iCharExtra := Props[LetterSpacing];
  Font.iName := ReadFontName(Props[FontFamily]);
  if Font.iName = '' then
    Font.iName := DefFontname;
end;

procedure TProperties.CalcLinkFontInfo(Styles: TStyleList; I: Integer);
{I is index in PropStack for this item}

  procedure InsertNewProp(N: Integer; const Pseudo: ThtString);
  begin
    PropStack.Insert(N, TProperties.Create(PropStack,FUseQuirksMode));
    PropStack[N].Inherit('', PropStack[N - 1]);
    PropStack[N].Combine(Styles, PropTag, PropClass, PropID, Pseudo, PropTitle, PropStyle, N - 1);
  end;

begin
  PropStack[I].SetFontBG;
  GetSingleFontInfo(FIArray.Ar[LFont]);
  InsertNewProp(I + 1, 'visited');
  PropStack[I + 1].SetFontBG;
  PropStack[I + 1].GetSingleFontInfo(FIArray.Ar[VFont]);
  InsertNewProp(I + 2, 'hover');
  PropStack[I + 2].SetFontBG;
  PropStack[I + 2].GetSingleFontInfo(FIArray.Ar[HVFont]);
  PropStack.Delete(I + 2);
  PropStack.Delete(I + 1);
  InsertNewProp(I + 1, 'hover');
  PropStack[I + 1].SetFontBG;
  PropStack[I + 1].GetSingleFontInfo(FIArray.Ar[HLFont]);
  PropStack.Delete(I + 1);
end;

procedure TProperties.GetFontInfo(AFI: TFontInfoArray);
begin
  AFI.Assign(FIArray);
end;

procedure TProperties.GetVMarginArray(var MArray: TVMarginArray; DefaultToColor: Boolean);
var
  I: PropIndices;
  BS: BorderStyleType;
  NewColor : TColor;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.GetVMarginArray');
  LogProperties(Self,'Self');
  {$ENDIF}
  for I := Low(Marray) to High(MArray) do
    case I of
      BorderTopStyle..BorderLeftStyle:
      begin
        BS := MArray[I];
        GetBorderStyle(I, BS);
        MArray[I] := BS;
      end;
      {From: http://www.w3.org/TR/CSS21/box.html#x49

      If an element's border color is not specified with a
      border property, user agents must use the value of the
      element's 'color' property as the computed value for
      the border color.
      }
      BorderTopColor..BorderLeftColor:
      begin
        if ColorFromString(Props[I], False, NewColor) then
          MArray[I] := Props[I]
        else if DefaultToColor then
          MArray[I] := Props[StyleUn.Color]
        else
          MArray[I] := IntNull;
      end
    else
      MArray[I] := Props[I];
    end;
  {$IFDEF JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogTVMarginArray(MArray,'MArray');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.GetVMarginArray');
  {$ENDIF}
end;

procedure TProperties.AddPropertyByIndex(Index: PropIndices; PropValue: ThtString);
var
  NewColor: TColor;
begin
{$ifdef JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.AddPropertyByIndex');
  CodeSiteLogging.CodeSite.Send('Parameters');
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.SendFmtMsg('Index = %s',[PropWords[Index]]);
  LogProperties(Self,'Self');
{$endif}
  case Index of
//    BorderColor:
//      if ColorFromString(PropValue, False, NewColor) then
//      begin
//        Props[BorderColor] := NewColor;
//        Props[BorderLeftColor] := NewColor;
//        Props[BorderTopColor] := NewColor;
//        Props[BorderRightColor] := NewColor;
//        Props[BorderBottomColor] := NewColor;
//      end;
    BorderTopColor..BorderLeftColor:
      if ColorFromString(PropValue, False, NewColor) then
        Props[Index] := NewColor;
    Color, BackgroundColor:
      if ColorFromString(PropValue, False, NewColor) then
        Props[Index] := NewColor
      else if Index = Color then
        Props[Index] := clBlack
      else
        Props[Index] := clNone;
    MarginTop..BorderLeftWidth, piWidth..LeftPos:
      Props[Index] := PropValue;
    FontSize:
      Props[FontSize] := PropValue;
    Visibility:
      begin
        if PropValue = 'visible' then
          Props[Visibility] := viVisible
        else if PropValue = 'hidden' then
          Props[Visibility] := viHidden;
      end;
    TextTransform:
      begin
        if PropValue = 'uppercase' then
          Props[TextTransform] := txUpper
        else if PropValue = 'lowercase' then
          Props[TextTransform] := txLower
        else
          Props[TextTransform] := txNone;
      end;
    WordWrap:
      if PropValue = 'break-word' then
        Props[WordWrap] := PropValue
      else
        Props[WordWrap] := 'normal';
    FontVariant:
      if PropValue = 'small-caps' then
        Props[FontVariant] := PropValue
      else if PropValue = 'normal' then
        Props[FontVariant] := 'normal';
    BorderTopStyle..BorderLeftStyle:
      begin
//        if PropValue <> 'none' then
//          Props[BorderStyle] := PropValue;
        Props[Index] := PropValue;
      end;
//    BorderStyle:
//      begin
//        Props[BorderStyle] := PropValue;
//        Props[BorderTopStyle] := PropValue;
//        Props[BorderRightStyle] := PropValue;
//        Props[BorderBottomStyle] := PropValue;
//        Props[BorderLeftStyle] := PropValue;
//      end;
  else
    Props[Index] := PropValue;
  end;
{$ifdef JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.Send('Results');
  CodeSiteLogging.CodeSite.AddSeparator;
  LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.AddPropertyByIndex');
{$endif}
end;

procedure TProperties.AddPropertyByName(const PropName, PropValue: ThtString);
var
  Index: PropIndices;
begin
{$ifdef JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.AddPropertyByName');
{$endif}
  if FindPropIndex(PropName, Index) then
    AddPropertyByIndex(Index, PropValue);
{$ifdef JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.AddPropertyByName');
{$endif}
end;

{ TStyleList }

constructor TStyleList.Create;
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupAccept;
  SeqNo := 10;
end;

constructor TStyleList.Create(const AUseQuirksMode: Boolean);
begin
  Create;
  DefProp := nil;
  FUseQuirksMode := AUseQuirksMode;
end;

destructor TStyleList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStyleList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TProperties(Objects[I]).Free;
  SeqNo := 10;
  inherited;
end;

function TStyleList.GetSeqNo: ThtString;
begin {used to help sort contextual items by entry sequence}
  Result := IntToStr(SeqNo);
  Inc(SeqNo);
end;

procedure FixBordProps(AProp, BodyProp : TProperties);
var i : PropIndices;
begin
  for i := BorderTopColor to BorderLeftColor do
    AProp.Props[I] := BodyProp.Props[I];
end;

procedure TStyleList.FixupTableColor(BodyProp: TProperties);
{if Quirk is set, make sure that the table color is defined the same as the
 body color}
var
  Propty1: TProperties;
  I: Integer;
begin
  if Find('table', I) then
  begin
    Propty1 := TProperties(Objects[I]);
    Propty1.Props[FontSize] := BodyProp.Props[FontSize];
    Propty1.Props[FontStyle] := BodyProp.Props[FontStyle];
    Propty1.Props[FontWeight] := BodyProp.Props[FontWeight];
    Propty1.Props[FontVariant] := BodyProp.Props[FontVariant];
    Propty1.Props[Color] := BodyProp.Props[Color];
    FixBordProps(Propty1,BodyProp);
  end;
  if Find('td', I) then
  begin
    Propty1 := TProperties(Objects[I]);
    Propty1.Props[Color] := BodyProp.Props[Color];
    FixBordProps(Propty1,BodyProp);
  end;
  if Find('th', I) then
  begin
    Propty1 := TProperties(Objects[I]);
    Propty1.Props[Color] := BodyProp.Props[Color];
    FixBordProps(Propty1,BodyProp);
  end;
end;

procedure TStyleList.AddModifyProp(const Selector, Prop, Value: ThtString);
{strings are all lowercase here}
var
  I: Integer;
  PropIndex: PropIndices;
  Propty: TProperties;
  NewColor: TColor;
  NewProp: Boolean;
begin
{$ifdef JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TStyleList.AddModifyProp');
  CodeSiteLogging.CodeSite.Send('Parameters');

  CodeSiteLogging.CodeSite.SendFmtMsg('Selector = %s',[Selector]);
  CodeSiteLogging.CodeSite.SendFmtMsg('Prop = %s',[Prop]);
  CodeSiteLogging.CodeSite.SendFmtMsg('Value = %s',[Value]);
CodeSiteLogging.CodeSite.AddSeparator;
  {$endif}
  if FindPropIndex(Prop, PropIndex) then
  begin
    if not Find(Selector, I) then
    begin
      NewProp := True;
      Propty := TProperties.Create(); {newly created property}
    end
    else
    begin
      Propty := TProperties(Objects[I]); {modify existing property}
      NewProp := False;
    end;
    case PropIndex of
      Color:
        if ColorFromString(Value, False, NewColor) then
        begin
          if Selector = ':link' then
          begin {changed the defaults to be the same as link}
            ModifyLinkColor('hover', NewColor);
            ModifyLinkColor('visited', NewColor);
          end
          else if Selector = ':visited' then
            ModifyLinkColor('hover', NewColor);
          Propty.Props[PropIndex] := NewColor;
        end;
//      BorderColor:
//        if ColorFromString(Value, False, NewColor) then
//        begin
//          Propty.Props[BorderColor] := NewColor;
//          Propty.Props[BorderLeftColor] := NewColor;
//          Propty.Props[BorderTopColor] := NewColor;
//          Propty.Props[BorderRightColor] := NewColor;
//          Propty.Props[BorderBottomColor] := NewColor;
//        end;
      BorderTopColor..BorderLeftColor:
        if ColorFromString(Value, False, NewColor) then
          Propty.Props[PropIndex] := NewColor;
      BackgroundColor:
        if ColorFromString(Value, False, NewColor) then
          Propty.Props[PropIndex] := NewColor
        else
          Propty.Props[PropIndex] := clNone;
      Visibility:
        begin
          if Value = 'visible' then
            Propty.Props[Visibility] := viVisible
          else if Value = 'hidden' then
            Propty.Props[Visibility] := viHidden;
        end;
      TextTransform:
        begin
          if Value = 'uppercase' then
            Propty.Props[TextTransform] := txUpper
          else if Value = 'lowercase' then
            Propty.Props[TextTransform] := txLower
          else
            Propty.Props[TextTransform] := txNone;
        end;
      WordWrap:
        if Value = 'break-word' then
          Propty.Props[WordWrap] := Value
        else
          Propty.Props[WordWrap] := 'normal';
      FontVariant:
        if Value = 'small-caps' then
          Propty.Props[FontVariant] := Value
        else if Value = 'normal' then
          Propty.Props[FontVariant] := 'normal';
      BorderTopStyle..BorderLeftStyle:
        begin
//          if Value <> 'none' then
//            Propty.Props[BorderStyle] := Value;
          Propty.Props[PropIndex] := Value;
        end;
//      BorderStyle:
//        begin
//          Propty.Props[BorderStyle] := Value;
//          Propty.Props[BorderTopStyle] := Value;
//          Propty.Props[BorderRightStyle] := Value;
//          Propty.Props[BorderBottomStyle] := Value;
//          Propty.Props[BorderLeftStyle] := Value;
//        end;
      LineHeight:
        Propty.Props[PropIndex] := Value;
    else
      Propty.Props[PropIndex] := Value;
    end;
    if NewProp then
      AddObject(Selector, Propty); {it's a newly created property}
    if Pos(':hover', Selector) > 0 then
      LinksActive := True;
    if Selector = 'a' then
    begin
      AddModifyProp('::link', Prop, Value); {also applies to ::link}
    end;
    if UseQuirksMode then begin
      if (Selector = 'body') and (PropIndex = Color) then begin
        FixupTableColor(Propty);
      end;
    end;
  end;
{$ifdef JPM_DEBUGGING}
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TStyleList.AddModifyProp');
{$endif}
end;

function TStyleList.AddObject(const S: ThtString; AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
  TProperties(AObject).PropTag := S;
end;

function TStyleList.AddDuplicate(const Tag: ThtString; Prop: TProperties): TProperties;
begin
  Result := TProperties.Create(Prop.PropStack,FUseQuirksMode);
  Result.Copy(Prop);
  AddObject(Tag, Result);
end;

procedure TStyleList.ModifyLinkColor(Pseudo: ThtString; AColor: TColor);
var
  I: Integer;
begin
  if Find('::' + Pseudo, I) then {the defaults}
    with TProperties(Objects[I]) do
      Props[Color] := AColor;
end;

procedure TStyleList.Initialize(const FontName, PreFontName: ThtString;
  PointSize: Integer; AColor, AHotspot, AVisitedColor, AActiveColor: TColor;
  LinkUnderline: Boolean; ACharSet: TFontCharSet; MarginHeight, MarginWidth: Integer);
type
  ListTypes = (ul, ol, menu, dir, dl, dd, blockquote);
const
  ListStr: array[Low(ListTypes)..High(ListTypes)] of ThtString =
  ('ul', 'ol', 'menu', 'dir', 'dl', 'dd', 'blockquote');
var
  HIndex: Integer;
  Properties: TProperties;
  J: ListTypes;
  F: Double;

begin
  Clear;
  DefPointSize := PointSize;

  Properties := TProperties.Create(UseQuirksMode);
  Properties.DefFontname := FontName;
  Properties.Props[FontFamily] := FontName;
  Properties.Props[FontSize] := PointSize;
  Properties.Props[FontStyle] := 'none';
  Properties.Props[FontWeight] := 'normal';
  Properties.Props[TextAlign] := 'left';
  Properties.Props[TextDecoration] := 'none';
  Properties.Props[TextTransform] := txNone;
  Properties.Props[WordWrap] := 'normal';
  Properties.Props[FontVariant] := 'normal';
  Properties.Props[Color] := AColor;
  Properties.Props[MarginTop] := MarginHeight;
  Properties.Props[MarginBottom] := MarginHeight;
  Properties.Props[MarginLeft] := MarginWidth;
  Properties.Props[MarginRight] := MarginWidth;
  Properties.Props[Visibility] := viVisible;
  Properties.Props[LetterSpacing] := 0;
  Properties.Props[BoxSizing] := ContentBox;
  Properties.CharSet := ACharSet;
  AddObject('default', Properties);
  DefProp := Properties;

  if UseQuirksMode then begin
    Properties := TProperties.Create(UseQuirksMode);
    Properties.Props[FontSize] := PointSize * 1.0;
    Properties.Props[FontStyle] := 'none';
    Properties.Props[FontWeight] := 'normal';
    Properties.Props[Color] := AColor;
    AddObject('td', Properties);
    Properties := AddDuplicate('table', Properties);

    Properties := AddDuplicate('th', Properties);
    Properties.Props[FontWeight] := 'bold';
  end;

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[Color] := AHotSpot or PalRelative;
  if LinkUnderline then
    Properties.Props[TextDecoration] := 'underline'
  else
    Properties.Props[TextDecoration] := 'none';
  AddObject('::link', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[Color] := AVisitedColor or PalRelative;
  AddObject('::visited', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[Color] := AActiveColor or PalRelative;
  AddObject('::hover', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  AddObject('null', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontFamily] := PreFontName;
  Properties.Props[FontSize] := PointSize * 10.0 / 12.0;
  Properties.Props[FontStyle] := 'none';
  Properties.Props[FontWeight] := 'normal';
  Properties.Props[TextDecoration] := 'none';
  AddObject('pre', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[MarginTop] := AutoParagraph;
  Properties.Props[MarginBottom] := AutoParagraph;
  AddObject('p', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[MarginTop] := 0;
  AddObject('p 11pre', Properties);

  for J := Low(ListTypes) to High(ListTypes) do
  begin
    Properties := TProperties.Create(UseQuirksMode);
    case J of
      ol, ul, menu, dir:
      begin
        Properties.Props[ListStyleType] := 'blank';
        Properties.Props[MarginTop] := AutoParagraph;
        Properties.Props[MarginBottom] := AutoParagraph;
        Properties.Props[PaddingLeft] := IntNull;
      end;

      dl:
      begin
        Properties.Props[ListStyleType] := 'none';
        Properties.Props[MarginLeft] := 0;
        Properties.Props[MarginTop] := 0;
        Properties.Props[MarginBottom] := 0;
        Properties.Props[PaddingLeft] := 0;
      end;

      blockquote:
      begin
        Properties.Props[MarginTop] := AutoParagraph;
        Properties.Props[MarginBottom] := ParagraphSpace;
        Properties.Props[PaddingLeft] := ListIndent;
      end;

      dd:
      begin
        Properties.Props[MarginTop] := 0;
        Properties.Props[MarginBottom] := 0;
        Properties.Props[PaddingLeft] := ListIndent;
      end;
    end;
    AddObject(ListStr[J], Properties);
  end;

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontFamily] := PrefontName;
  Properties.Props[FontSize] := '0.83em'; {10.0 / 12.0;}
  AddObject('code', Properties);
  AddDuplicate('tt', Properties);
  AddDuplicate('kbd', Properties);
  AddDuplicate('samp', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontWeight] := 'bold';
  AddObject('b', Properties);
  AddDuplicate('strong', Properties);
  if UseQuirksMode = False then begin
    AddDuplicate('th', Properties);
    Properties := TProperties.Create;
    Properties.Props[TextAlign] := 'none';
    AddObject('table', Properties);
  end;

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontSize] := '0.83em';
  Properties.Props[VerticalAlign] := 'super';
  AddObject('sup', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontSize] := '0.83em';
  Properties.Props[VerticalAlign] := 'sub';
  AddObject('sub', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontSize] := '1.17em';
  AddObject('big', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontSize] := '0.83em';
  AddObject('small', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontStyle] := 'italic';
  AddObject('i', Properties);
  AddDuplicate('em', Properties);
  AddDuplicate('cite', Properties);
  AddDuplicate('var', Properties);

  AddDuplicate('address', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[TextDecoration] := 'underline';
  AddObject('u', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[TextDecoration] := 'line-through';
  AddObject('s', Properties);
  AddDuplicate('strike', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[TextAlign] := 'center';
  AddObject('center', Properties);
  AddDuplicate('caption', Properties);

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[FontFamily] := 'Arial Unicode MS, Arial';
  Properties.Props[FontSize] := '10pt';
  Properties.Props[FontStyle] := 'none';
  Properties.Props[FontWeight] := 'normal';
  Properties.Props[TextAlign] := 'left';
  Properties.Props[TextDecoration] := 'none';
  Properties.Props[Color] := AColor;
  AddObject('input', Properties);
  AddDuplicate('select', Properties);

  Properties := AddDuplicate('textarea', Properties);
  if IsWin32Platform then
    Properties.Props[FontFamily] := 'Arial Unicode MS, Arial'
  else
    Properties.Props[FontFamily] := PreFontName;

  Properties := TProperties.Create(UseQuirksMode);
  Properties.Props[MarginLeft] := 0;
  Properties.Props[MarginRight] := 0;
  Properties.Props[MarginTop] := 10;
  Properties.Props[MarginBottom] := 10;
  AddObject('hr', Properties);

  for HIndex := 1 to 6 do
  begin
    Properties := TProperties.Create(UseQuirksMode);
    F := PointSize / 12.0;
    case HIndex of
      1: Properties.Props[FontSize] := 24.0 * F;
      2: Properties.Props[FontSize] := 18.0 * F;
      3: Properties.Props[FontSize] := 14.0 * F;
      4: Properties.Props[FontSize] := 12.0 * F;
      5: Properties.Props[FontSize] := 10.0 * F;
      6: Properties.Props[FontSize] := 8.0 * F;
    end;
    Properties.Props[MarginTop] := 19;
    Properties.Props[MarginBottom] := Properties.Props[MarginTop];
    Properties.Props[FontWeight] := 'bold';
    AddObject('h' + IntToStr(HIndex), Properties);
  end;

  Properties := TProperties.Create;
  Properties.Props[FontStyle] := 'none';
  Properties.Props[BackgroundColor] := $00FFFF;
  Properties.Props[Color] := $000000;
  AddObject('mark', Properties);
end;


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

{ TPropStack }

function TPropStack.GetProp(Index: Integer): TProperties;
begin
  Result := Get(Index); //TProperties(inherited Items[Index]);
end;

function TPropStack.Last: TProperties;
begin
  Result := Get(Count - 1);
end;


const
  NumColors = 176;
  Colors: array[1..NumColors] of ThtString = ('transparent',
    'black', 'maroon', 'green', 'olive', 'navy', 'purple', 'teal', 'gray',
    'silver', 'red', 'lime', 'yellow', 'blue', 'fuchsia', 'aqua', 'white',
    'aliceblue', 'antiquewhite', 'aquamarine', 'azure', 'beige',
    'bisque', 'blanchedalmond', 'blueviolet', 'brown', 'burlywood',
    'cadetblue', 'chartreuse', 'chocolate', 'coral', 'cornflowerblue',
    'cornsilk', 'crimson', 'cyan', 'darkblue', 'darkcyan',
    'darkgoldenrod', 'darkgray', 'darkgreen', 'darkkhaki', 'darkmagenta',
    'darkolivegreen', 'darkorange', 'darkorchid', 'darkred', 'darksalmon',
    'darkseagreen', 'darkslateblue', 'darkslategray', 'darkturquoise', 'darkviolet',
    'deeppink', 'deepskyblue', 'dimgray', 'dodgerblue', 'firebrick',
    'floralwhite', 'forestgreen', 'gainsboro', 'ghostwhite', 'gold',
    'goldenrod', 'greenyellow', 'honeydew', 'hotpink', 'indianred',
    'indigo', 'ivory', 'khaki', 'lavender', 'lavenderblush',
    'lawngreen', 'lemonchiffon', 'lightblue', 'lightcoral', 'lightcyan',
    'lightgoldenrodyellow', 'lightgreen', 'lightgray', 'lightpink', 'lightsalmon',
    'lightseagreen', 'lightskyblue', 'lightslategray', 'lightsteelblue', 'lightyellow',
    'limegreen', 'linen', 'magenta', 'mediumaquamarine', 'mediumblue',
    'mediumorchid', 'mediumpurple', 'mediumseagreen', 'mediumslateblue', 'mediumspringgreen',
    'mediumturquoise', 'mediumvioletred', 'midnightblue', 'mintcream', 'mistyrose',
    'moccasin', 'navajowhite', 'oldlace', 'olivedrab', 'orange',
    'orangered', 'orchid', 'palegoldenrod', 'palegreen', 'paleturquoise',
    'palevioletred', 'papayawhip', 'peachpuff', 'peru', 'pink',
    'plum', 'powderblue', 'rosybrown', 'royalblue', 'saddlebrown',
    'salmon', 'sandybrown', 'seagreen', 'seashell', 'sienna',
    'skyblue', 'slateblue', 'slategray', 'snow', 'springgreen',
    'steelblue', 'tan', 'thistle', 'tomato', 'turquoise',
    'violet', 'wheat', 'whitesmoke', 'yellowgreen',

    'grey', 'darkgrey', 'darkslategrey', 'dimgrey', 'lightgrey', 'lightslategrey', 'slategrey',
    'background', 'activecaption', 'inactivecaption', 'menu', 'window',
    'windowframe', 'menutext', 'windowtext', 'captiontext', 'activeborder',
    'inactiveborder', 'appworkSpace', 'highlight', 'hightlighttext', 'buttonface',
    'buttonshadow', 'graytext', 'buttontext', 'inactivecaptiontext', 'buttonhighlight',
    'threeddarkshadow', 'threedlightshadow', 'infotext', 'infobackground', 'scrollbar',
    'threedface', 'threedhighlight', 'threedshadow');

  ColorValues: array[1..NumColors] of TColor = (clNone,
    clBLACK, clMAROON, clGREEN, clOLIVE, clNAVY, clPURPLE, clTEAL, clGRAY,
    clSILVER, clRED, clLIME, clYELLOW, clBLUE, clFUCHSIA, clAQUA, clWHITE,
    $FFF8F0, $D7EBFA, $D4FF7F, $FFFFF0, $DCF5F5,
    $C4E4FF, $CDEBFF, $E22B8A, $2A2AA5, $87B8DE,
    $A09E5F, $00FF7F, $1E69D2, $507FFF, $ED9564,
    $DCF8FF, $3614DC, $FFFF00, $8B0000, $8B8B00,
    $0B86B8, $A9A9A9, $006400, $6BB7BD, $8B008B,
    $2F6B55, $008CFF, $CC3299, $00008B, $7A96E9,
    $8FBC8F, $8B3D48, $4F4F2F, $D1CE00, $D30094,
    $9314FF, $FFBF00, $696969, $FF901E, $2222B2,
    $F0FAFF, $228B22, $DCDCDC, $FFF8F8, $00D7FF,
    $20A5DA, $2FFFAD, $F0FFF0, $B469FF, $5C5CCD,
    $82004B, $F0FFFF, $8CE6F0, $FAE6E6, $F5F0FF,
    $00FC7C, $CDFAFF, $E6D8AD, $8080F0, $FFFFE0,
    $D2FAFA, $90EE90, $D3D3D3, $C1B6FF, $7AA0FF,
    $AAB220, $FACE87, $998877, $DEC4B0, $E0FFFF,
    $32CD32, $E6F0FA, $FF00FF, $AACD66, $CD0000,
    $D355BA, $DB7093, $71B33C, $EE687B, $9AFA00,
    $CCD148, $8515C7, $701919, $FAFFF5, $E1E4FF,
    $B5E4FF, $ADDEFF, $E6F5FD, $238E6B, $00A5FF,
    $0045FF, $D670DA, $AAE8EE, $98FB98, $EEEEAF,
    $9370DB, $D5EFFF, $B9DAFF, $3F85CD, $CBC0FF,
    $DDA0DD, $E6E0B0, $8F8FBC, $E16941, $13458B,
    $7280FA, $60A4F4, $578B2E, $EEF5FF, $2D52A0,
    $EBCE87, $CD5A6A, $908070, $FAFAFF, $7FFF00,
    $B48246, $8CB4D2, $D8BFD8, $4763FF, $D0E040,
    $EE82EE, $B3DEF5, $F5F5F5, $32CD9A,
    clgray, $A9A9A9, $4F4F2F, $696969, $D3D3D3, $998877, $908070,
    clBackground, clActiveCaption, clInactiveCaption, clMenu, clWindow,
    clWindowFrame, clMenuText, clWindowText, clCaptionText, clActiveBorder,
    clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText, clBtnFace,
    clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight,
    cl3DDkShadow, clBtnHighlight, clInfoText, clInfoBk, clScrollBar,
    clBtnFace, cl3DLight, clBtnShadow);

var
  ColorStrings: ThtStringList;

function SortedColors: ThtStringList;
var
  I: Integer;
begin
// Put the Colors into a sorted StringList for faster access.
  if ColorStrings = nil then
  begin
    ColorStrings := ThtStringList.Create;
    for I := 1 to NumColors do
      ColorStrings.AddObject(Colors[I], @ColorValues[I]);
    ColorStrings.Sort;
  end;
  Result := ColorStrings;
end;

function OpacityFromStr(S : ThtString) : Byte;
var LErr : Integer;
  LR : Real;
begin
  Val(S,LR,LErr);
  if LErr <> 0 then begin
    Result := 255;
  end else begin
    Result := Trunc(255 * LR);
  end;
end;

function ColorFromString(S: ThtString; NeedPound: Boolean; out Color: TColor): Boolean;
var LDummy : Byte;
begin
  Result := ColorAndOpacityFromString(S,NeedPound,Color,LDummy);
end;

function ColorAndOpacityFromString(S: ThtString; NeedPound: Boolean; out Color: TColor; out VOpacity : Byte): Boolean;
{Translate StyleSheet color ThtString to Color.  If NeedPound is true, a '#' sign
 is required to preceed a hexidecimal value.}
const
  LastS: ThtString = '?&%@';
  LastColor: TColor = 0;

var
  I, Rd, Bl: Integer;
  S1: ThtString;

  function FindHSLColor(S: ThtString): Boolean;
  type
    Colors = (hue, saturation, luminance);
  var
    I, J: Integer;
  var
    A: array[hue..luminance] of ThtString;
    C: array[hue..luminance] of Integer;
    K: Colors;
  begin
    I := Pos('(', S);
    J := Pos(')', S);
    if (I > 0) and (J > 0) then
    begin
      S := copy(S, 1, J - 1);
      S := Trim(Copy(S, I + 1, 255));
      for K := hue to saturation do
      begin
        I := Pos(',', S);
        A[K] := Trim(copy(S, 1, I - 1));
        S := Trim(Copy(S, I + 1, 255));
      end;
      I := Pos(',', S);
      if I > 0 then begin
        A[luminance] := Trim(copy(S, 1, I - 1));
        S := Trim(Copy(S, I + 1, 255));
        VOpacity := OpacityFromStr(S);
      end else begin
        A[luminance] := S;
        VOpacity := 255;
      end;

      C[hue] := StrToIntDef(A[hue],0);
      while C[hue] >= 360 do begin
        C[hue] := C[hue] - 360;
      end;
      while C[hue] < 0 do begin
        C[hue] := C[hue] + 360;
      end;
      for K := saturation to luminance do begin
        I := Pos('%', A[K]);
        if I > 0 then begin
          Delete(A[K], I, 1);
        end;
        C[K] := StrToIntDef(A[K],0);
        if C[K] > 100 then begin
          C[K] := 100;
        end;
        if C[K] < 0 then begin
          C[K] := 0;
        end;
      end;
      Color := HSLUtils.HSLtoRGB(C[hue],C[saturation],C[luminance]);
      Result := True;
    end
    else
      Result := False;
  end;

  function FindRGBColor(S: ThtString): Boolean;
  type
    Colors = (red, green, blue);
  var
    A: array[red..blue] of ThtString;
    C: array[red..blue] of Integer;
    I, J: Integer;
    K: Colors;

  begin
    I := Pos('(', S);
    J := Pos(')', S);
    if (I > 0) and (J > 0) then
    begin
      S := copy(S, 1, J - 1);
      S := Trim(Copy(S, I + 1, 255));
      for K := Red to Green do
      begin
        I := Pos(',', S);
        A[K] := Trim(copy(S, 1, I - 1));
        S := Trim(Copy(S, I + 1, 255));
      end;
      I := Pos(',', S);
      if I > 0 then begin
        A[blue] := Trim(copy(S, 1, I - 1));
        S := Trim(Copy(S, I + 1, 255));
        VOpacity := OpacityFromStr(S);
      end else begin
        A[blue] := S;
        VOpacity := 255;
      end;

      for K := Red to Blue do
      begin
        I := Pos('%', A[K]);
        if I > 0 then
        begin
          Delete(A[K], I, 1);
          try
            C[K] := Round(StrToFloat(A[K]) * 2.55);
          except
            C[K] := 0;
          end;
        end
        else
          C[K] := StrToIntDef(A[K], 0);
        C[K] := Max(0, Min(255, C[K]));
      end;
      Color := (C[Blue] shl 16) or (C[Green] shl 8) or C[Red];
      Result := True;
    end
    else
      Result := False;
  end;

//BG, 26.08.2009: exceptions are very slow
var
  Int: Integer;
  Idx : Integer;
//BG, 26.08.2009
begin
  //Opacity is not supported with # hexidecimal notation or color names
  VOpacity := 255;
  if S = '' then
  begin
    Result := False;
    Exit;
  end;
  S := Lowercase(Trim(S));
  if S = LastS then
  begin {inquiries often come in pairs, this saves some recomputing}
    Color := LastColor;
    Result := True;
    Exit;
  end;
  I := Pos('hsl',S);
  if I > 0 then begin
    Result := FindHSLColor(Copy(S, I + 3, 255));
    if Result then
    begin
      LastS := S1;
      LastColor := Color;
    end;
    exit;
  end;
  I := Pos('rgb', S);
  if (I = 0) and (S[1] <> '#') then
  begin
    if SortedColors.Find(S, Idx) then
    begin
      Color := PColor(SortedColors.Objects[Idx])^;
      Result := True;
      LastS := S;
      LastColor := Color;
      Exit;
    end;
  end;
  S1 := S;
  if (I > 0) then
    Result := FindRGBColor(Copy(S, I + 3, 255))
  else
  begin
//    try
      I := Pos('#', S);
      if I > 0 then
        while I > 0 do {sometimes multiple ##}
        begin
          Delete(S, 1, I);
          I := Pos('#', S);
        end
      else if NeedPound then
      begin
        Result := False;
        Exit;
      end;
      if Length(S) <= 3 then
        for I := Length(S) downto 1 do
          Insert(S[I], S, I); {Double each character}
      Result := TryStrToInt('$' + S, Int);
      if Result then
      begin
      {ok, but bytes are backwards!}
        Rd := Int and $FF;
        Bl := Int and $FF0000;
        Color := (Int and $00FF00) + (Rd shl 16) + (Bl shr 16) or PalRelative;
      end;
//    except
//      Result := False;
//    end;
  end;
  if Result then
  begin
    LastS := S1;
    LastColor := Color;
  end;
end;

{ ThtFontInfo }

procedure ThtFontInfo.Assign(Source: ThtFontInfo);
begin
  iName := Source.iName;
  iSize := Source.iSize;
  iStyle := Source.iStyle;
  iColor := Source.iColor;
  ibgColor := Source.ibgColor;
  iCharSet := Source.iCharSet;
  iCharExtra := Source.iCharExtra;
end;

{ TFontInfoArray }

constructor TFontInfoArray.Create;
var
  I: FIIndex;
begin
  inherited Create;
  for I := LFont to HVFont do
    Ar[I] := ThtFontInfo.Create;
end;

destructor TFontInfoArray.Destroy;
var
  I: FIIndex;
begin
  for I := LFont to HVFont do
    Ar[I].Free;
  inherited;
end;

procedure TFontInfoArray.Assign(Source: TFontInfoArray);
var
  I: FIIndex;
begin
  for I := LFont to HVFont do
    Ar[I].Assign(Source.Ar[I]);
end;

//BG, 14.07.2010:
function decodeSize(const Str: ThtString; out V: extended; out U: ThtString): Boolean;
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

const
  f_cm = 1.0 / 2.54;
  f_px = 1.0 / 6.00;
  f_mm = 1.0 / 25.4;
  f_pt = 1.0 / 72.0;
  f_pc = 1.0 / 100.0;

function IncFontSize(OldSize: Double; Increment: TFontSizeIncrement): Double;
var
  OldIndex, NewIndex: Byte;
  D1, D2: Double;
begin
  // get nearest old font size index
  OldIndex := 4;
  D1 := OldSize - FontConv[OldIndex];
  repeat
    case Sign(D1) of
      -1:
      begin
        Dec(OldIndex);
        D2 := OldSize - FontConv[OldIndex];
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
        D2 := OldSize - FontConv[OldIndex];
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
    Result := OldSize
  else
    Result := OldSize * FontConv[NewIndex] / FontConv[OldIndex];
end;

function FontSizeConv(const Str: ThtString; OldSize: Double; const AUseQuirksMode : Boolean): Double;
{given a font-size ThtString, return the point size}
var
  V: extended;
  U: ThtString;
  i : Integer;
begin
  if decodeSize(Str, V, U) then
  begin
    if U = 'in' then
      Result := V * 72.0
    else if U = 'cm' then
      Result := V * 72.0 * f_cm
    else if U = 'mm' then
      Result := V * 72.0 * f_mm
    else if U = 'pt' then
      Result := V
    else if U = 'px' then
      Result := V * 72.0 / Screen.PixelsPerInch
    else if U = 'pc' then
      Result := V * 12.0
    else if U = 'em' then
      Result := V * OldSize
    else if U = 'ex' then
      Result := V * OldSize * 0.5 {1/2 of em}
    else if U = '%' then
      Result := V * OldSize * f_pc
    else if U = '' then
      Result := V * 72.0 / Screen.PixelsPerInch {pixels by default}
    else
      Result := DefPointSize; {error, return 12pt}
  end
  else
  begin
    U := Str;
    if AUseQuirksMode then begin
      i := 1;
    end else begin
      i := 0;
    end;
    if U = 'smaller' then
      Result := IncFontSize(OldSize, -1) // 0.75 * OldSize
    else if U = 'larger' then
      Result := IncFontSize(OldSize,  1) // 1.25 * OldSize
    else if U = 'xx-small' then
      Result := FontConv[1 + i]
    else if U = 'x-small' then
      Result := FontConv[2 + i]
    else if U = 'small' then
      Result := FontConv[3 + i]
    else if U = 'medium' then
      Result := FontConv[4 + i]
    else if U = 'large' then
      Result := FontConv[5 + i]
    else if U = 'x-large' then
      Result := FontConv[6 + i]
    else if U = 'xx-large' then
      Result := FontConv[7]
    else
      Result := DefPointSize;
  end;
end;

{----------------LengthConv}

function LengthConv(const Str: ThtString; Relative: Boolean; Base, EmSize, ExSize,
  Default: Integer): Integer;
{given a length ThtString, return the appropriate pixel value.  Base is the
 base value for percentage. EmSize, ExSize for units relative to the font.
 Relative makes a numerical entry relative to Base.
 Default returned if no match.}
var
  V: extended;
  U: ThtString;
begin
  if decodeSize(Str, V, U) then
  begin
    {U the units}
    if U = '' then
    begin
      if Relative then
        V := V * Base; {relative}
      //else
      //  V := V {same as pixels, at least for margins}
    end
    else if U = '%' then
      V := V * Base * f_pc
    else if U = 'in' then
      V := V * Screen.PixelsPerInch
    else if U = 'cm' then
      V := V * Screen.PixelsPerInch * f_cm
    else if U = 'mm' then
      V := V * Screen.PixelsPerInch * f_mm
    else if U = 'pt' then
      V := V * Screen.PixelsPerInch * f_pt
    else if U = 'px' then
    else if U = 'pc' then
      V := V * Screen.PixelsPerInch * f_px
    else if U = 'em' then
      V := V * EmSize
    else if U = 'ex' then
      V := V * ExSize
    else
      V := Default;
    Result := Trunc(V); // BG, 14.12.2011: issue 104: avoid too wide "50%". Replace Round() with Trunc().
  end
  else
    // anything else but a number, maybe 'auto'
    Result := Default;
end;

{ TMyFontCache }

type
  ThtStringListOpener = class(ThtStringList)
  end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
procedure TMyFontCache.Add(Font: TMyFont);
var
  I: Integer;
begin
  if not FFontsByName.Find(htLowerCase(Font.Name), I) then
    ThtStringListOpener(FFontsByName).InsertItem(I, Font.Name, TObjectList.Create(True));
  TObjectList(FFontsByName.Objects[I]).Add(Font);
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
constructor TMyFontCache.Create;
begin
  inherited;
  FFontsByName := ThtStringList.Create;
  FFontsByName.Sorted := True;
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
destructor TMyFontCache.Destroy;
var
  I: Integer;
begin
  for I := 0 to FFontsByName.Count - 1 do
    FFontsByName.Objects[I].Free;
  FFontsByName.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
function TMyFontCache.Find(FontInfo: ThtFontInfo): TMyFont;

  function SameFonts(F1: TMyFont; F2: ThtFontInfo): Boolean;
  begin
    if F2 <> nil then
      if F1.Height = -Round(F2.iSize * Screen.PixelsPerInch / 72) then
        if F1.Style = F2.iStyle then
          if F1.Charset = F2.iCharset then
          begin
            Result := True;
            exit;
          end;
    Result := False;
  end;

var
  I: Integer;
  Fonts: TObjectList;
begin
  if FFontsByName.Find(htLowerCase(FontInfo.iName), I) then
  begin
    Fonts := TObjectList(FFontsByName.Objects[I]);
    for I := 0 to Fonts.Count - 1 do
    begin
      Result := TMyFont(Fonts[I]);
      if SameFonts(Result, FontInfo) then
        exit;
    end;
  end;
  Result := nil;
end;

initialization
  AllMyFonts := TMyFontCache.Create;
finalization
  FreeAndNil(AllMyFonts);
  FreeAndNil(ColorStrings);
end.
