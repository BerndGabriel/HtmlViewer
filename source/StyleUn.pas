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

unit StyleUn;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc,
{$else}
  Windows,
{$endif}
  Classes, Graphics, SysUtils, Math, Forms, Contnrs, Variants,
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
  //
  HtmlBuffer,
  HtmlFonts,
  HtmlGlobals,
  HtmlSymb,
  HtmlUn2,
  StyleTypes;

{$ifdef UseOldStyleTypes}
type
  AlignmentType = ThtAlignmentStyle;
  BorderStyleType = ThtBorderStyle;
  ClearAttrType = ThtClearStyle;
  ListBulletType = ThtBulletStyle;
  PositionType = ThtBoxPositionStyle;
  TTextTransformStyle = ThtTextTransformStyle;
  TPropDisplay = ThtDisplayStyle;
  VisibilityType = ThtVisibilityStyle;
  TWhiteSpaceStyle = ThtWhiteSpaceStyle;
{$endif}

const
  CurColor_Val = 'currentColor';
  CurColorStr = 'currentcolor';
  varInt = [varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord, varInt64];
  varFloat = [varSingle, varDouble, varCurrency];
  varNum = varInt + varFloat;
  CrLf = #$D#$A;

//BG, 16.09.2010: CSS2.2: same sizes like html font size:

const
  FontConvBase: array[1..7] of Double = (8.0, 10.0, 12.0, 14.0, 18.0, 24.0, 36.0);
  PreFontConvBase: array[1..7] of Double = (7.0, 8.0, 10.0, 12.0, 15.0, 20.0, 30.0);
  
var
  FontConv: array[1..7] of Double;
  PreFontConv: array[1..7] of Double;

type
  // Notice: in ThtPropertyIndex order of rectangle properties required:
  //
  //  Top, Right, Bottom, Left.
  //
  // Some code relies on this order.
  //
  ThtPropertyIndex = (
    FontFamily, FontSize, FontStyle, FontWeight,
    TextAlign, TextDecoration, LetterSpacing, Color,

    // the below properties are in MarginArrays
    BackgroundColor, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundAttachment,
    BoxSizing,
    MarginTop, MarginRight, MarginBottom, MarginLeft,
    PaddingTop, PaddingRight, PaddingBottom, PaddingLeft,
    BorderTopWidth, BorderRightWidth, BorderBottomWidth, BorderLeftWidth,
    BorderTopColor, BorderRightColor, BorderBottomColor, BorderLeftColor,
    BorderTopStyle, BorderRightStyle, BorderBottomStyle, BorderLeftStyle,
    piMinHeight, piMinWidth, piMaxHeight, piMaxWidth,
    piWidth, piHeight, piTop, piRight, piBottom, piLeft,
    BorderSpacingHorz, BorderSpacingVert,  //These two are internal
    // the above properties are in MarginArrays

    Visibility, LineHeight, VerticalAlign, Position, ZIndex,
    ListStyleType, ListStyleImage, Float, Clear, TextIndent,
    PageBreakBefore, PageBreakAfter, PageBreakInside, TextTransform,
    WordWrap, FontVariant, BorderCollapse, OverFlow, piDisplay, piEmptyCells,
    piWhiteSpace,

    // the below properties are short hands
    MarginX, PaddingX, BorderWidthX, BorderX,
    BorderTX, BorderRX, BorderBX, BorderLX,
    FontX, BackgroundX, ListStyleX, BorderColorX,
    BorderStyleX,
    // the above properties are short hands

    //the following have multiple values sometimes
    BorderSpacing
    //the above have multiple values sometimes
  );

  TShortHand = MarginX..BorderSpacing;//BorderStyleX;

  ThtPropIndices = FontFamily..piWhiteSpace;
  ThtPropertyArray = array [ThtPropIndices] of Variant;
  ThtPropIndexSet = Set of ThtPropIndices;

  ThtMarginIndices = BackgroundColor..BorderSpacingVert; //piLeft;
  ThtVMarginArray = array [ThtMarginIndices] of Variant;
  ThtMarginArray = array [ThtMarginIndices] of Integer;

const
  PropWords: array [ThtPropertyIndex] of ThtString = (
    'font-family', 'font-size', 'font-style', 'font-weight',
    'text-align', 'text-decoration', 'letter-spacing', 'color',

    // these properties are in MarginArrays
    'background-color', 'background-image', 'background-position', 'background-repeat', 'background-attachment',
    'box-sizing',
    'margin-top', 'margin-right', 'margin-bottom', 'margin-left',
    'padding-top', 'padding-right', 'padding-bottom', 'padding-left',
    'border-top-width', 'border-right-width', 'border-bottom-width', 'border-left-width',
    'border-top-color', 'border-right-color', 'border-bottom-color', 'border-left-color',
    'border-top-style', 'border-right-style', 'border-bottom-style', 'border-left-style',
    'min-height', 'min-width', 'max-height', 'max-width',
    'width', 'height', 'top', 'right', 'bottom', 'left',
    'thv-border-spacing-horz', 'thv-border-spacing-vert', //These two are for internal use only

    'visibility', 'line-height', 'vertical-align', 'position', 'z-index',
    'list-style-type', 'list-style-image', 'float', 'clear', 'text-indent',
    'page-break-before', 'page-break-after', 'page-break-inside', 'text-transform',
    'word-wrap', 'font-variant', 'border-collapse', 'overflow', 'display', 'empty-cells',
    'white-space',

    // short hand names
    'margin', 'padding', 'border-width', 'border',
    'border-top', 'border-right', 'border-bottom', 'border-left',
    'font', 'background', 'list-style', 'border-color',
    'border-style',
    //multiple values
    'border-spacing'
  );

type
  TStyleList = class;
  TPropStack = class;

  TProperties = class
  private
    FDefFontSizeInPt: Double;
    FPixelsPerInch: Integer;
    PropStack: TPropStack; // owner
    TheFont: ThtFont;
    InLink: Boolean;
    DefFontname: ThtString;
    FUseQuirksMode : Boolean;
    procedure AssignCodePage(const CP: Integer);
    procedure CalcLinkFontInfo(Styles: TStyleList; I: Integer);
    procedure GetSingleFontInfo(var Font: ThtFontInfo);
    function GetFont: ThtFont;
    function GetEmSize: Integer;
    function GetExSize: Integer;
  public
    PropSym: TElemSymb;
    PropTag, PropClass, PropID, PropPseudo, PropTitle: ThtString;
    PropStyle: TProperties;
    FontBG: TColor;
    FCharSet: TFontCharSet;
    FCodePage: Integer;
    Props: ThtPropertyArray;
    Originals: array[ThtPropIndices] of Boolean;
    Important: array[ThtPropIndices] of Boolean;
    FIArray: TFontInfoArray;
    ID: Integer;

    constructor Create; overload; // for use in style list only
    constructor Create(const AUseQuirksMode : Boolean; APixelsPerInch: Integer); overload; // for use in style list only
    constructor Create(APropStack: TPropStack; const AUseQuirksMode : Boolean; APixelsPerInch: Integer); overload; // for use in property stack
    constructor CreateCopy(ASource: TProperties);
    destructor Destroy; override;
    function Clone: TProperties; virtual;

    function BorderStyleNotBlank: Boolean;
    function Collapse: Boolean;
    function GetBackgroundColor: TColor;
    function GetBackgroundImage(var Image: ThtString): Boolean;
    function GetBorderStyle(Index: ThtPropIndices; var BorderStyle: ThtBorderStyle): Boolean;
    function GetClear(var Clr: ThtClearStyle): Boolean;
    function GetDisplay: ThtDisplayStyle; //BG, 15.09.2009
    function GetFloat(var Align: ThtAlignmentStyle): Boolean;
    function GetFontVariant: ThtString;
    function GetLineHeight(NewHeight: Integer): Integer;
    function GetListStyleImage: ThtString;
    function GetListStyleType: ThtBulletStyle;
    function GetOriginalForegroundColor: TColor;
    function GetPosition: ThtBoxPositionStyle;
    function GetTextIndent(out PC: Boolean): Integer;
    function GetTextTransform: ThtTextTransformStyle;
    function GetVertAlign(var Align: ThtAlignmentStyle): Boolean;
    function GetVisibility: ThtVisibilityStyle;
    function GetZIndex: Integer;
    function HasBorderStyle: Boolean;
    function HasBorderWidth: Boolean;
    function IsOverflowHidden: Boolean;
    function ShowEmptyCells: Boolean;
    procedure AddPropertyByIndex(Index: ThtPropIndices; PropValue: ThtString; IsImportant: Boolean);
    procedure AddPropertyByName(const PropName, PropValue: ThtString; IsImportant: Boolean);
    procedure SetPropertyDefault(Index: ThtPropIndices; const Value: Variant);
    procedure SetPropertyDefaults(Indexes: ThtPropIndexSet; const Value: Variant);
    procedure Assign(const Item: Variant; Index: ThtPropIndices);
    procedure AssignCharSetAndCodePage(CS: TFontCharset; CP: Integer);
    procedure Combine(Styles: TStyleList; Sym: TElemSymb; const Tag, AClass, AnID, Pseudo, ATitle: ThtString; AProp: TProperties; Attributes: TAttributeList; ParentIndexInPropStack: Integer); overload;
    procedure Combine(Styles: TStyleList; Sym: TElemSymb; const Tag, Pseudo: ThtString; Properties: TProperties; Attributes: TAttributeList; ParentIndexInPropStack: Integer); overload;
    procedure Copy(Source: TProperties);
    procedure CopyDefault(Source: TProperties);
    procedure GetBackgroundPos(EmSize, ExSize: Integer; out P: PtPositionRec);
    procedure GetFontInfo(AFI: TFontInfoArray);
    procedure GetPageBreaks(out Before, After, Intact: Boolean);
    function GetBoxSizing(var VBoxSizing : ThtBoxSizing) : Boolean;
    function GetBorderSpacingHorz: Integer;
    function GetBorderSpacingVert: Integer;
    procedure GetVMarginArrayDefBorder(var MArray: ThtVMarginArray; const ADefColor : Variant);
    procedure GetVMarginArray(var MArray: ThtVMarginArray);
    function HasBorderSpacing: Boolean;
    procedure Inherit(Tag: ThtString; Source: TProperties);
    procedure SetFontBG;
    procedure Update(Source: TProperties; Styles: TStyleList; I: Integer);
    //BG, 20.09.2009:
    property Display: ThtDisplayStyle read GetDisplay;
    property CharSet: TFontCharset read FCharSet write FCharSet;
    property CodePage: Integer read FCodePage write AssignCodePage;
    property DefPointSize : Double read FDefFontSizeInPt write FDefFontSizeInPt;
    property EmSize: Integer read GetEmSize;
    property ExSize: Integer read GetExSize;
    property Font: ThtFont read GetFont;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    property UseQuirksMode : Boolean read FUseQuirksMode;
  end;

  TStyleList = class(ThtStringList)
  private
    SeqNo: Integer;
    FDefProp: TProperties;
  protected
    FDefFontSizeInPt : Double;
    FPixelsPerInch: Integer;
    //this must be protected so that the property can be changed in
    //a descendant while being read only.
    FUseQuirksMode : Boolean;
    procedure SetLinksActive(Value: Boolean); virtual; abstract;
    property LinksActive: Boolean write SetLinksActive;
  public
    constructor Create; overload;
    constructor Create(AUseQuirksMode : Boolean); overload;
    destructor Destroy; override;
    function AddDuplicate(const Tag: ThtString; Prop: TProperties): TProperties;
    function AddObject(const S: ThtString; AObject: TObject): Integer; override;
    function GetSeqNo: ThtString;
    procedure Clear; override;
    procedure AddModifyProp(const Selector, Prop, Value: ThtString; IsImportant: Boolean);
    procedure FixupTableColor(BodyProp: TProperties);
    procedure Initialize(const FontName, PreFontName: ThtString; PointSize: Integer;
      AColor, AHotspot, AVisitedColor, AActiveColor: TColor; LinkUnderline: Boolean;
      ACodePage: TBuffCodePage; ACharSet: TFontCharSet; MarginHeight, MarginWidth: Integer);
    procedure ModifyLinkColor(Pseudo: ThtString; AColor: TColor);
    property UseQuirksMode : Boolean read FUseQuirksMode write FUseQuirksMode;
    property DefProp: TProperties read FDefProp;
    property DefPointSize : Double read FDefFontSizeInPt write FDefFontSizeInPt;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
  end;

{$ifdef UseGenerics}
  TPropStack = class(TObjectList<TProperties>)
{$else}
  TPropStack = class(TObjectList)
  private
    function GetProp(Index: Integer): TProperties; {$ifdef UseInline} inline; {$endif}
{$endif}
  public
    function Last: TProperties; {$ifdef UseInline} inline; {$endif}
{$ifdef UseGenerics}
{$else}
    property Items[Index: Integer]: TProperties read GetProp; default;
{$endif}
  end;

type
  ThtPropertyIndexes = set of ThtPropertyIndex;

  ThtConvData = record
    BaseWidth, BaseHeight: Integer;
    EmSize, ExSize: Integer;
    BorderWidth: Integer;
    AutoCount: Integer;
    IsAutoParagraph: ThtPropertyIndexes;
  end;

const
  IntNull = -12345678;
  Auto = -12348765;
  AutoParagraph = -12348766;
  ParagraphSpace = 14; {default spacing between paragraphs, etc.}
  ImageSpace = 3; {extra space for left, right images}
  ListIndent = 40;

  EastEurope8859_2 = 31; {for 8859-2}


// BG, 25.04.2012: Added:
function IsAuto(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}

//BG, 05.10.2010: added:
function VarIsIntNull(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}
function VarIsAuto(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}

function VMargToMarg(const Value: Variant; Relative: Boolean; Base, EmSize, ExSize, Default, PixelsPerInch: Integer): Integer;

function ConvData(BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth: Integer; AutoCount: Integer = 0): ThtConvData;
procedure ConvMargProp(I: ThtPropIndices; const VM: ThtVMarginArray; var ConvData: ThtConvData; var M: ThtMarginArray; PixelsPerInch: Integer);

procedure ConvInlineMargArray(const VM: ThtVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth: Integer; {BStyle: ThtBorderStyle;} out M: ThtMarginArray; PixelsPerInch: Integer);
procedure ConvMargArray(const VM: ThtVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth: Integer; out AutoCount: Integer; var M: ThtMarginArray; PixelsPerInch: Integer);
procedure ConvMargArrayForCellPadding(const VM: ThtVMarginArray; EmSize, ExSize: Integer; var M: ThtMarginArray; PixelsPerInch: Integer);
procedure ConvVertMargins(const VM: ThtVMarginArray; var CD: ThtConvData; var M: ThtMarginArray; PixelsPerInch: Integer);


function OpacityFromStr(S : ThtString) : Byte;

function SortedColors: ThtStringList;
function TryStrToColor(S: ThtString; NeedPound: Boolean; out Color: TColor): Boolean;
function ColorAndOpacityFromString(S: ThtString; NeedPound: Boolean; out Color: TColor; out VOpacity : Byte): Boolean;

function ReadURL(Item: Variant): ThtString;

function LowerCaseUnquotedStr(const S : THtString) : THtString;
function RemoveQuotes(const S: ThtString): ThtString;
function ReadFontName(S: ThtString): ThtString;

{$ifdef JPM_DEBUGGING_STYLES}
const
    CVis : array [0..2] of string = ('viInherit','viHidden','viVisible');

procedure LogProperties(AProp : TProperties; const APropName : String);
procedure LogThtMarginArray(AMarg : ThtMarginArray; const AMargName : String);
{$endif}

procedure ApplyBoxWidthSettings(var AMarg : ThtMarginArray; var VMinWidth, VMaxWidth : Integer; const AUseQuirksMode : Boolean);
procedure ApplyBoxSettings(var AMarg : ThtMarginArray; const AUseQuirksMode : Boolean);

//here for inlining
function SkipWhiteSpace(const S: ThtString; I, L: Integer): Integer;
function FontSizeConv(const Str: ThtString; OldSize, DefFontSizeInPt: Double; PixelsPerInch: Integer; const AUseQuirksMode : Boolean): Double;
function LengthConv(const Str: ThtString; Relative: Boolean; Base, EmSize, ExSize, Default, PixelsPerInch: Integer): Integer;

procedure CalcAutoMinMaxConstraints(W, H, MinW, MaxW, MinH, MaxH: Integer; out ResW, ResH: Integer);

implementation
uses
{$ifdef Compiler24_Plus}
  System.UITypes,
{$endif}
{$if defined(JPM_DEBUGGING_STYLES) or defined(JPM_DEBUGGING_CONV)}
 CodeSiteLogging,
{$ifend}
 HSLUtils;

var
//  DefPointSize: Double;
  CharsetPerCharset: array [TFontCharset] of record Inited: Boolean; Charset: TFontCharset; end;

{$ifdef JPM_DEBUGGING_STYLES}

function LogPropColor(const AInt : Integer): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  Result := Graphics.ColorToString( AInt);
end;

function LogPropColor(const AVar : Variant): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  if Variants.VarIsOrdinal(AVar) then
    Result := Graphics.ColorToString( AVar)
  else
    Result := VarToStr( AVar );
end;

function LogPropDisplay(const AInt : Integer): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  Result := CDisplayStyle[ ThtDisplayStyle(AInt)];
end;

function LogPropDisplay(const AVar : Variant): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CDisplayStyle[ ThtDisplayStyle(AVar)]  + ' int = '+ IntToStr(AVar);;
  end else begin
    Result := VarToStr( AVar );
  end;
end;

function LogPropBoxSizing(const AInt : Integer): String; overload;
{$ifdef UseInline} inline; {$endif}
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
{$ifdef UseInline} inline; {$endif}
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CBoxSizing[ThtBoxSizing (AVar)]  + ' int = '+ IntToStr(AVar);
  end else begin
    Result := VarToStr( AVar );
  end;
end;

function LogPropBorderStyle(const AInt : Integer): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  case AInt of
    0 : Result := CBorderStyle[bssNone ];
    1 : Result := CBorderStyle[bssSolid ];
    2 : Result := CBorderStyle[ bssInset ];
    3 : Result := CBorderStyle[ bssOutset ];
    4 : Result := CBorderStyle[ bssGroove ];
    5 : Result := CBorderStyle[ bssRidge ];
    6 : Result := CBorderStyle[ bssDashed ];
    7 : Result := CBorderStyle[ bssDotted ];
    8 : Result := CBorderStyle[ bssDouble ];
  else
    Result := 'Error value is '+IntToStr(AInt);
  end;
end;

function LogPropBorderStyle(const AVar : Variant): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CBorderStyle[ ThtBorderStyle ( AVar )]  + ' int = '+ IntToStr(AVar);
  end else begin
    Result := VarToStr( AVar);
  end;

end;

function LogPropListStyle(const AInt : Integer): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  case AInt of
    0 : Result := CBulletStyle[ lbBlank ];
    1 : Result := CBulletStyle[ lbCircle ];
    2 : Result := CBulletStyle[ lbDecimal ];
    3 : Result := CBulletStyle[ lbDisc ];
    4 : Result := CBulletStyle[ lbLowerAlpha ];
    5 : Result := CBulletStyle[ lbLowerRoman ];
    6 : Result := CBulletStyle[ lbNone ];
    7 : Result := CBulletStyle[ lbSquare ];
    8 : Result := CBulletStyle[ lbUpperAlpha ];
    9 : Result := CBulletStyle[ lbUpperRoman ];
  else
    Result := 'Error value is '+IntToStr(AInt);
  end;
//  Result := CBoxSizing[BoxSizingType (AInt)];
end;

function LogPropListStyle(const AVar : Variant): String; overload;
{$ifdef UseInline} inline; {$endif}
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CBulletStyle[ ThtBulletStyle ( AVar )]  + ' int = '+ IntToStr(AVar);;
  end else begin
    Result := VarToStr( AVar);
  end;
end;

function LogVisibility(const AVar : Variant): String;
{$ifdef UseInline} inline; {$endif}
begin
  if Variants.VarIsOrdinal(AVar) then begin
    Result := CVis[Integer( AVar )] + ' int = '+ IntToStr(AVar);
  end else begin
    Result := VarToStr( AVar);
  end;
end;

procedure LogTVMarginArray(const AMarg : ThtVMarginArray; const AMargName : String);
{$ifdef UseInline} inline; {$endif}
var i : ThtPropIndices;
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

procedure LogThtMarginArray(AMarg : ThtMarginArray; const AMargName : String);
{$ifdef UseInline} inline; {$endif}
var i : ThtPropIndices;
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

var i : ThtPropIndices;
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



//-- BG ---------------------------------------------------------- 17.02.2011 --
function SkipWhiteSpace(const S: ThtString; I, L: Integer): Integer;
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
begin
  while (I <= L) and (S[I] <> C) do
    Inc(I);
  Result := I;
end;

{----------------ReadURL}

function ReadURL(Item: Variant): ThtString;
 {$ifdef UseInline} inline; {$endif}
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

//-- BG ---------------------------------------------------------- 15.03.2011 --
var
  PropertyStrings: ThtStringList;

function StyleProperties: ThtStringList;
 {$ifdef UseInline} inline; {$endif}
var
  I: ThtPropertyIndex;
begin
  // Put the Properties into a sorted StringList for faster access.
  if PropertyStrings = nil then
  begin
    PropertyStrings := ThtStringList.Create;
    for I := Low(I) to High(I) do
      PropertyStrings.AddObject(PropWords[I], Pointer(I));
    PropertyStrings.Sort;
  end;
  Result := PropertyStrings;
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function TryStrToPropIndex(const PropWord: ThtString; var PropIndex: ThtPropIndices): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
  P: ThtPropertyIndex;
begin
  I := StyleProperties.IndexOf(PropWord);
  Result := I >= 0;
  if Result then
  begin
    P := ThtPropertyIndex(StyleProperties.Objects[I]);
    Result := P in [Low(ThtPropIndices)..High(ThtPropIndices)];
    if Result then
      PropIndex := P;
  end;
end;

var
  Sequence: Integer;

{----------------TProperties.Create}

constructor TProperties.Create;
var
  I: ThtPropIndices;
begin
  inherited Create;
  ID := Sequence;
  Inc(Sequence);
  FontBG := clNone;
  for I := MarginTop to BorderSpacingVert do
    Props[I] := IntNull;
  Props[ZIndex] := 0;
  FUseQuirksMode := False;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
constructor TProperties.Create(APropStack: TPropStack; const AUseQuirksMode : Boolean; APixelsPerInch: Integer);
begin
  Create;
  Self.PropStack := APropStack;
  FUseQuirksMode := AUseQuirksMode;
  FPixelsPerInch := APixelsPerInch;
end;

constructor TProperties.Create(const AUseQuirksMode : Boolean; APixelsPerInch: Integer);
begin
  Create;
  FUseQuirksMode := AUseQuirksMode;
  FPixelsPerInch := APixelsPerInch;
end;

//-- BG ---------------------------------------------------------- 21.10.2016 --
function TProperties.Clone: TProperties;
begin
  Result := TProperties.CreateCopy(Self);

  Result.Props := Props;
  Result.Originals := Originals;
  Result.Important := Important;
end;

//-- BG ---------------------------------------------------------- 20.01.2013 --
constructor TProperties.CreateCopy(ASource: TProperties);
begin
  FDefFontSizeInPt := ASource.FDefFontSizeInPt;
  FPixelsPerInch   := ASource.FPixelsPerInch  ;
  PropStack        := ASource.PropStack       ;
  InLink           := ASource.InLink          ;
  DefFontname      := ASource.DefFontname     ;
  FUseQuirksMode   := ASource.FUseQuirksMode  ;
  PropTag          := ASource.PropTag         ;
  PropClass        := ASource.PropClass       ;
  PropID           := ASource.PropID          ;
  PropPseudo       := ASource.PropPseudo      ;
  PropTitle        := ASource.PropTitle       ;
  PropStyle        := ASource.PropStyle       ;
  FontBG           := ASource.FontBG          ;
  FCharSet         := ASource.FCharSet        ;
  FCodePage        := ASource.FCodePage       ;
  Props            := ASource.Props           ;
  Originals        := ASource.Originals       ;
  ID               := ASource.ID              ;
  if ASource.FIArray <> nil then
  begin
    FIArray := TFontInfoArray.Create;
    FIArray.Assign(ASource.FIArray);
  end;
  if ASource.TheFont <> nil then
  begin
    TheFont := ThtFont.Create;
    TheFont.Assign(ASource.TheFont);
  end;
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
  I: ThtPropIndices;
begin
  {$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'Copy');
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Source,'Source');
  {$ENDIF}
  FDefFontSizeInPt := Source.FDefFontSizeInPt;
  for I := Low(I) to High(I) do
    Props[I] := Source.Props[I];
  {$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'Copy');
  {$ENDIF}
end;

{----------------TProperties.CopyDefault}

procedure TProperties.CopyDefault(Source: TProperties);
var
  I: ThtPropIndices;
begin
  {$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'CopyDefault');
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Source,'Source');
  {$ENDIF}
  for I := Low(I) to High(I) do
    Props[I] := Source.Props[I];
  CodePage := Source.CodePage;
  DefFontname := Source.DefFontname;
  FDefFontSizeInPt := Source.FDefFontSizeInPt;
  PropTag := 'default';
  {$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'CopyDefault');
  {$ENDIF}
end;

procedure TProperties.Inherit(Tag: ThtString; Source: TProperties);
{copy the properties that are inheritable}
var
  I: ThtPropIndices;
  Span, HBF: Boolean;
  isTable: Boolean;
begin
  {$IFDEF JPM_DEBUGGING_STYLES}
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
    else
      case I of
        MarginTop..BorderLeftStyle,
        piMinHeight, piMinWidth,
        piMaxHeight, piMaxWidth,
        piWidth, piHeight,
        piTop..piLeft:
          Props[I] := IntNull;

        WordWrap:
          if isTable then
            Props[I] := 'normal'
          else
            Props[I] := Source.Props[I];

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
  DefPointSize := Source.DefPointSize;
  DefFontname := Source.DefFontname;
  FontBG := Source.FontBG;
  CodePage := Source.CodePage;
  PropTitle := Source.PropTitle;
  InLink := Source.InLink;
  if InLink then
  begin
    if not Assigned(FIArray) then
      FIArray := TFontInfoArray.Create;
    FIArray.Assign(Source.FIArray);
  end;

  {$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.Inherit');
  {$ENDIF}
end;

{----------------TProperties.Update}

procedure TProperties.Update(Source: TProperties; Styles: TStyleList; I: Integer);
{Change the inherited properties for this item to those of Source}
var
  Index: ThtPropIndices;
begin
  {$IFDEF JPM_DEBUGGING_STYLES}
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
  {$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.Update');
  {$ENDIF}
end;

{----------------TProperties.Assign}

procedure TProperties.Assign(const Item: Variant; Index: ThtPropIndices);
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

//procedure TProperties.AssignCharSet(CS: TFontCharset);
//begin
//  AssignCharSetAndCodePage(CS, CharSetToCodePage(CS));
//end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
function TranslateCharset(CS: TFontCharset): TFontCharset;
 {$ifdef UseInline} inline; {$endif}
// extracted from TProperties.AssignCharSetAndCodePage()
var
  Save: HFONT;
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
        PXY.Value := LengthConv(S[I], False, 100, EmSize, ExSize, 0, PixelsPerInch);
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

function TProperties.GetVertAlign(var Align: ThtAlignmentStyle): Boolean;
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

function TProperties.GetFloat(var Align: ThtAlignmentStyle): Boolean;
var
  S: ThtString;
begin
  if (VarIsStr(Props[Float])) then
  begin
    Result := True;
    S := Props[Float];
    if S = 'left' then
      Align := aLeft
    else if S = 'right' then
      Align := aRight
    else if S = 'none' then
      Align := aNone
    else
      Result := False;
  end
  else
    Result := False;
end;

function TProperties.GetClear(var Clr: ThtClearStyle): Boolean;
var
  S: ThtString;
begin
  if (VarIsStr(Props[Clear])) then
  begin
    S := Props[Clear];
    Result := TryStrToClearStyle(S, Clr);
    //Props[Clear] := Unassigned; {allow only one read}
  end
  else
    Result := False;
end;

//-- BG ---------------------------------------------------------- 15.09.2009 --
function TProperties.GetDisplay: ThtDisplayStyle;
begin
  if VarIsStr(Props[piDisplay]) then
    if TryStrToDisplayStyle(Props[piDisplay], Result) then
      exit;
  Result := pdUnassigned;
end;

//-- BG ---------------------------------------------------------- 21.09.2016 --
function TProperties.GetEmSize: Integer;
begin
  Result := Font.EmSize;
end;

//-- BG ---------------------------------------------------------- 21.09.2016 --
function TProperties.GetExSize: Integer;
begin
  Result := Font.ExSize;
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TProperties.GetListStyleType: ThtBulletStyle;
begin
  if VarIsStr(Props[ListStyleType]) then
    if TryStrToBulletStyle(Props[ListStyleType], Result) then
      Exit;
  Result := lbBlank;
end;

function TProperties.GetListStyleImage: ThtString;
begin
  Result := ReadURL(Props[ListStyleImage])
end;

function TProperties.GetPosition: ThtBoxPositionStyle;
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

function TProperties.GetVisibility: ThtVisibilityStyle;
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
      Result := LengthConv(Props[LineHeight], True, EmSize, EmSize, ExSize, -1, PixelsPerInch);
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
      Result := LengthConv(Props[TextIndent], True, 100, 0, 0, 0, PixelsPerInch);
    end
    else
      Result := LengthConv(Props[TextIndent], False, 0, EmSize, EmSize, 0, PixelsPerInch);
  end
  else
    Result := 0;
end;

function TProperties.GetTextTransform: ThtTextTransformStyle;
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

//-- BG ---------------------------------------------------------- 12.03.2011 --
function TProperties.GetBorderStyle(Index: ThtPropIndices; var BorderStyle: ThtBorderStyle): Boolean;
// Returns True, if there is a valid border style property.
begin
  Result := False;
  if VarIsStr(Props[Index]) then
    Result := TryStrToBorderStyle(Props[Index], BorderStyle)
  else if VarType(Props[Index]) in varInt then
    if (Props[Index] >= Low(ThtBorderStyle)) and (Props[Index] <= High(ThtBorderStyle)) then
    begin
      BorderStyle := ThtBorderStyle(Props[Index]);
      Result := True;
    end;
end;

function TProperties.GetBoxSizing(var VBoxSizing: ThtBoxSizing): Boolean;
begin
  Result := TryStrToBoxSizing(Props[BoxSizing], VBoxSizing);
end;

function TProperties.BorderStyleNotBlank: Boolean;
{was a border of some type (including bssNone) requested?}
var
  Dummy: ThtBorderStyle;
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
  Dummy: ThtBorderStyle;
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

function TProperties.GetBorderSpacingHorz: Integer;
var
  V: Double;
  Code: Integer;

begin
  if VarIsStr(Props[BorderSpacingHorz]) then
  begin
    Val(Props[BorderSpacingHorz], V, Code);
    if Code = 0 then {a numerical entry with no 'em', '%', etc.  }
      Result := Round(V)
    else
    {note: 'normal' yields -1 in the next statement}
      Result := LengthConv(Props[BorderSpacingHorz], True, EmSize, EmSize, ExSize, -1, PixelsPerInch);
  end
  else
    Result := -1;
end;

function TProperties.GetBorderSpacingVert: Integer;
var
  V: Double;
  Code: Integer;

begin
  if VarIsStr(Props[BorderSpacingVert]) then
  begin
    Val(Props[BorderSpacingVert], V, Code);
    if Code = 0 then {a numerical entry with no 'em', '%', etc.  }
      Result := Round(V)
    else
    {note: 'normal' yields -1 in the next statement}
      Result := LengthConv(Props[BorderSpacingVert], True, EmSize, EmSize, ExSize, -1, PixelsPerInch);
  end
  else
    Result := -1;
end;

function TProperties.HasBorderSpacing: Boolean;
begin
  Result := not (VarIsIntNull(Props[BorderSpacingHorz]) or VarIsEmpty(Props[BorderSpacingHorz]));
end;

procedure TProperties.SetFontBG;
{called for font tags like <b>, <small>, etc.  Sets the font background color.}
begin
  if (VarType(Props[BackgroundColor]) in varInt) and Originals[BackgroundColor] then
    FontBG := Props[BackgroundColor];
end;

//-- BG ---------------------------------------------------------- 20.01.2013 --
procedure TProperties.SetPropertyDefault(Index: ThtPropIndices; const Value: Variant);
begin
  if (Props[Index] = Unassigned) or ((VarType(Props[Index]) in varInt) and (Props[Index] = IntNull)) then
    Props[Index] := Value;
end;

//-- BG ---------------------------------------------------------- 20.01.2013 --
procedure TProperties.SetPropertyDefaults(Indexes: ThtPropIndexSet; const Value: Variant);
var
  Index: ThtPropIndices;
begin
  for Index := Low(Index) to High(Index) do
    if Index in Indexes then
      SetPropertyDefault(Index, Value);
end;

//-- BG ---------------------------------------------------------- 23.11.2009 --
function TProperties.ShowEmptyCells: Boolean;
begin
{$ifdef EmptyCellsDefaultIsShow}
  Result := not (VarIsStr(Props[piEmptyCells]) and (Props[piEmptyCells] = 'hide'));
{$else}
  Result :=      VarIsStr(Props[piEmptyCells]) and (Props[piEmptyCells] = 'show') ;
{$endif}
end;

procedure ConvVertMargins(const VM: ThtVMarginArray; var CD: ThtConvData; var M: ThtMarginArray; PixelsPerInch: Integer);
begin
{$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod('ConvVertMargins');
  CodeSiteLogging.CodeSite.Send('BaseHeight  = [%d]',[CD.BaseHeight]);
  CodeSiteLogging.CodeSite.Send('EmSize      = [%d]',[CD.EmSize]);
  CodeSiteLogging.CodeSite.Send('ExSize      = [%d]',[CD.ExSize]);
  StyleUn.LogTVMarginArray(VM,'VM');
{$ENDIF}

  ConvMargProp( PaddingTop        , VM, CD, M, PixelsPerInch);
  ConvMargProp( BorderTopWidth    , VM, CD, M, PixelsPerInch);
  ConvMargProp( MarginTop         , VM, CD, M, PixelsPerInch);
  ConvMargProp( piHeight          , VM, CD, M, PixelsPerInch);
  ConvMargProp( piMinHeight       , VM, CD, M, PixelsPerInch);
  ConvMargProp( piMaxHeight       , VM, CD, M, PixelsPerInch);
  ConvMargProp( MarginBottom      , VM, CD, M, PixelsPerInch);
  ConvMargProp( BorderBottomWidth , VM, CD, M, PixelsPerInch);
  ConvMargProp( PaddingBottom     , VM, CD, M, PixelsPerInch);

{$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.Send('Results');
  CodeSiteLogging.CodeSite.ExitMethod('ConvVertMargins');
  StyleUn.LogThtMarginArray(M,'M');
{$ENDIF}
end;

//-- BG ---------------------------------------------------------- 25.04.2012 --
function IsAuto(const Value: Variant): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := Value = Auto;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function VarIsIntNull(const Value: Variant): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := (VarType(Value) in varInt) and (Value = IntNull);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function VarIsAuto(const Value: Variant): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  if VarType(Value) in varInt then
    Result := Value = Auto
  else if VarIsStr(Value) then
    Result := Value = 'auto'
  else
    Result := False;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function VMargToMarg(const Value: Variant; Relative: Boolean; Base, EmSize, ExSize, Default: Integer; PixelsPerInch: Integer): Integer;
 {$ifdef UseInline} inline; {$endif}
begin
  if VarIsStr(Value) then
    Result := LengthConv(Value, Relative, Base, EmSize, ExSize, Default, PixelsPerInch)
  else if (VarType(Value) in varInt) and (Value <> IntNull) then
    Result := Value
  else
    Result := Default;
end;

procedure ApplyBoxWidthSettings(var AMarg : ThtMarginArray; var VMinWidth, VMaxWidth : Integer; const AUseQuirksMode : Boolean);
 {$ifdef UseInline} inline; {$endif}
begin
  {Important!!!

  You have to do this with settings.  This is only for FindWidth methods}
  if not AUseQuirksMode then
  begin
    if AMarg[piMaxWidth] > 0 then
    begin
      AMarg[piWidth] := Min(AMarg[piMaxWidth], AMarg[piWidth]);
      VMaxWidth := Min(AMarg[piMaxWidth], VMaxWidth);
    end;

    if AMarg[piMinWidth] > 0 then
    begin
      AMarg[piWidth] := Max(AMarg[piMinWidth], AMarg[piWidth]);
      VMinWidth := Max(AMarg[piMinWidth], VMinWidth);
    end;
  end;
end;

procedure ApplyBoxSettings(var AMarg : ThtMarginArray; const AUseQuirksMode : Boolean);

  procedure ApplyBorderBoxModel(var AMarg : ThtMarginArray);
  begin
    if AMarg[piWidth] > -1 then
      Dec(AMarg[piWidth], AMarg[BorderLeftWidth] + AMarg[PaddingLeft] + AMarg[PaddingRight] + AMarg[BorderRightWidth]);

    if AMarg[piHeight] > -1 then
      Dec(AMarg[piHeight], AMarg[BorderTopWidth] + AMarg[PaddingTop] + AMarg[PaddingBottom] + AMarg[BorderBottomWidth]);
  end;

begin
  if AUseQuirksMode then
    ApplyBorderBoxModel(AMarg)
  else
  begin
    {JPM:  This test is here to prevent AMarg[piWidth] from being ruined
    if it is set to Auto.  If it is ruined, AutoCount might be incremented
    correctly causing a rendering bug. }

    //min max width
    if AMarg[piWidth] > 0 then
    begin
      if AMarg[piMaxWidth] > 0 then
        AMarg[piWidth] := Min(AMarg[piWidth], AMarg[piMaxWidth]);

      if AMarg[piMinWidth] > 0 then
        AMarg[piWidth] := Max(AMarg[piWidth], AMarg[piMinWidth]);
    end;

//BG, 09.08.2015: this min/max handling is not as described in CSS 2.1
// and tents to bust the document design.
//    //min max height
//    if AMarg[piHeight] > 0 then
//    begin
//      if AMarg[piMaxHeight] > 0 then
//        AMarg[piHeight] := Min(AMarg[piHeight], AMarg[piMaxHeight]);
//
//      if AMarg[piMinHeight] > 0 then
//        AMarg[piHeight] := Max(AMarg[piHeight], AMarg[piMinHeight]);
//    end;

    case ThtBoxSizing(AMarg[BoxSizing]) of
      BorderBox: ApplyBorderBoxModel(AMarg);
    end;
  end;
end;

{----------------ConvMargArray}

//-- BG ---------------------------------------------------------- 16.05.2014 --
function ConvData(
    BaseWidth, BaseHeight: Integer;
    EmSize, ExSize: Integer;
    BorderWidth: Integer;
    AutoCount: Integer = 0): ThtConvData;
begin
  Result.BaseWidth := BaseWidth;
  Result.BaseHeight := BaseHeight;
  Result.EmSize := EmSize;
  Result.ExSize := ExSize;
  Result.BorderWidth := BorderWidth;
  Result.AutoCount := AutoCount;
  Result.IsAutoParagraph := [];
end;

//-- BG ---------------------------------------------------------- 16.05.2014 --
procedure ConvMargProp(I: ThtPropIndices; const VM: ThtVMarginArray; var ConvData: ThtConvData; var M: ThtMarginArray; PixelsPerInch: Integer);

  function Base(I: ThtPropIndices): Integer;
  begin
    case I of
      BorderTopWidth, BorderBottomWidth,
      MarginTop, MarginBottom,
      piMinHeight, piMaxHeight,
      PaddingTop, PaddingBottom,
      LineHeight,
      piHeight, piTop:
        Base := ConvData.BaseHeight
    else
      Base := ConvData.BaseWidth;
    end;
  end;

var
  LBoxSizing: ThtBoxSizing;
  Color: TColor;
begin
  with ConvData do
  begin
    case I of
      BackgroundColor, BorderTopColor..BorderLeftColor:
        begin
          if VarType(VM[I]) <= VarNull then
            M[I] := clNone
          else if VarIsStr(VM[I]) then
          begin
            if TryStrToColor(VM[I], false, Color) then
              M[I] := Color;
          end
          else
            M[I] := VM[I];
        end;

      BorderTopWidth..BorderLeftWidth:
        begin
          if VM[ThtPropIndices(Ord(BorderTopStyle) + (Ord(I) - Ord(BorderTopWidth)))] = bssNone then
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
                M[I] := LengthConv(VM[I], False, Base(I), EmSize, ExSize, BorderWidth, PixelsPerInch); {Auto will be 4}
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

      piMinHeight, piMaxHeight:
        begin
          if VarIsStr(VM[I]) then
          begin
            M[I] := LengthConv(VM[I], False, Base(I), EmSize, ExSize, 0, PixelsPerInch); {Auto will be 0}
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

      piHeight:
        begin
          if VarIsStr(VM[I]) then
          begin
            M[I] := LengthConv(VM[I], False, Base(I), EmSize, ExSize, Auto, PixelsPerInch); {Auto will be Auto}
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
            M[I] := Auto;
        end;

      PaddingTop..PaddingLeft,BorderSpacingHorz,BorderSpacingVert:
        begin
          if VarIsStr(VM[I]) then
          begin
            M[I] := LengthConv(VM[I], False, Base(I), EmSize, ExSize, 0, PixelsPerInch); {Auto will be 0}
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

      piTop..piLeft:
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, Base(I), EmSize, ExSize, Auto, PixelsPerInch) {Auto will be Auto}
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

      BoxSizing:
        if TryStrToBoxSizing(VM[I],LBoxSizing) then begin
           M[I] := Ord(LBoxSizing);
        end else begin
          //assume content-box
           M[I] := 0;
        end;

      MarginRight, MarginLeft:
        begin
          if VarIsStr(VM[I]) then
          begin
            if VM[I] = 'auto' then
            begin
              M[I] := Auto;
              Inc(AutoCount);
            end
            else
              M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0, PixelsPerInch);
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

      MarginTop, MarginBottom:
        begin
          if VarIsStr(VM[I]) then
            M[I] := LengthConv(VM[I], False, BaseHeight, EmSize, ExSize, 0, PixelsPerInch) {Auto will be 0}
          else if VarType(VM[I]) in varInt then
          begin
            if VM[I] = IntNull then
              M[I] := 0
            else if VM[I] = AutoParagraph then
            begin
              M[I] := ParagraphSpace;
              Include(IsAutoParagraph, I);
            end
            else
              M[I] := VM[I];
          end
          else
            M[I] := 0;
        end;

      piMinWidth,
      piMaxWidth:
        begin
          if VarIsStr(VM[I]) then
          begin
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto, PixelsPerInch);
            if Pos('%', VM[I]) > 0 then {include border in % heights}
              M[I] := M[I] - M[BorderLeftWidth] - M[BorderRightWidth] - M[PaddingLeft] - M[PaddingRight];
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

      piWidth:
        begin
          if VarIsStr(VM[I]) then
          begin
            if VM[I] = 'auto' then
              M[I] := Auto
            else
            begin
              M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto, PixelsPerInch);
              if Pos('%', VM[I]) > 0 then {include border in % heights}
                M[I] := M[I] - M[BorderLeftWidth] - M[BorderRightWidth] - M[PaddingLeft] - M[PaddingRight];
            end;
          end
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
    else
      begin
        if VarIsStr(VM[I]) then
          M[I] := LengthConv(VM[I], False, Base(I), EmSize, ExSize, 0, PixelsPerInch)
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
end;

procedure ConvMargArray(const VM: ThtVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize: Integer;
  BorderWidth: Integer; out AutoCount: Integer; var M: ThtMarginArray; PixelsPerInch: Integer);
{This routine does not do MarginTop and MarginBottom as they are done by ConvVertMargins}
var
  I: ThtPropIndices;
  CD: ThtConvData;
begin
{$IFDEF JPM_DEBUGGING_CONV}
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

  CD := ConvData(BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth);
  for I := Low(VM) to High(VM) do
    if not (I in [MarginTop, MarginBottom]) then
      ConvMargProp(I, VM, CD, M, PixelsPerInch);
  AutoCount := CD.AutoCount; {count of 'auto's in width items}

{$IFDEF JPM_DEBUGGING_CONV}
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.Send('Results');
  LogThtMarginArray(M,'M');
  CodeSiteLogging.CodeSite.Send('AutoCount  = [%d]',[AutoCount]);
  CodeSiteLogging.CodeSite.ExitMethod('ConvMargArray');
{$ENDIF}
end;

procedure ConvMargArrayForCellPadding(const VM: ThtVMarginArray; EmSize,
  ExSize: Integer; var M: ThtMarginArray; PixelsPerInch: Integer);
{Return negative for no entry or percent entry}
var
  I: ThtPropIndices;
begin
{$IFDEF JPM_DEBUGGING_CONV}
  CodeSiteLogging.CodeSite.EnterMethod('ConvMargArrayForCellPadding');
  CodeSiteLogging.CodeSite.AddSeparator;
  LogTVMarginArray(VM,'VM');
  CodeSiteLogging.CodeSite.Send('EmSize      = [%d]',[EmSize]);
  CodeSiteLogging.CodeSite.Send('ExSize      = [%d]',[ExSize]);
{$ENDIF}
  for I := PaddingTop to PaddingLeft do
    if VarIsStr(VM[I]) then
      M[I] := LengthConv(VM[I], False, -100, EmSize, ExSize, 0, PixelsPerInch) {Auto will be 0}
    else if VarType(VM[I]) in varInt then
    begin
      if VM[I] = IntNull then
        M[I] := -1
      else
        M[I] := VM[I];
    end
    else
      M[I] := -1;
{$IFDEF JPM_DEBUGGING_CONV}
  CodeSiteLogging.CodeSite.AddSeparator;
  LogThtMarginArray(M,'M');
  CodeSiteLogging.CodeSite.ExitMethod('ConvMargArrayForCellPadding');
{$ENDIF}
end;

{----------------ConvInlineMargArray}

procedure ConvInlineMargArray(const VM: ThtVMarginArray; BaseWidth, BaseHeight, EmSize,
  ExSize, BorderWidth: Integer; {BStyle: ThtBorderStyle;} out M: ThtMarginArray; PixelsPerInch: Integer);
 {$ifdef UseInline} inline; {$endif}
{currently for images, form controls.  BaseWidth/Height and BStyle currently not supported}
var
  I: ThtPropIndices;
begin
{$IFDEF JPM_DEBUGGING_CONV}
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
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto, PixelsPerInch) {Auto will be Auto}
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
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0, PixelsPerInch) {auto is 0}
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
          if VM[ThtPropIndices(Ord(BorderTopStyle) + (Ord(I) - Ord(BorderTopWidth)))] = bssNone then
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
                M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, BorderWidth, PixelsPerInch); {Auto will be BorderWidth}
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
{$IFDEF JPM_DEBUGGING_CONV}
   CodeSiteLogging.CodeSite.AddSeparator;
   LogThtMarginArray(M,'M');
   CodeSiteLogging.CodeSite.ExitMethod('ConvInlineMargArray');
{$ENDIF}
end;

{----------------TProperties.Combine}

//-- BG ---------------------------------------------------------- 28.02.2016 --
procedure TProperties.Combine(Styles: TStyleList; Sym: TElemSymb;
  const Tag, Pseudo: ThtString; Properties: TProperties;
  Attributes: TAttributeList; ParentIndexInPropStack: Integer);
begin
  if Attributes <> nil then
    Combine(Styles, Sym, Tag, Attributes.TheClass, Attributes.TheID, PSeudo, Attributes.TheTitle, Properties, Attributes, ParentIndexInPropStack)
  else
    Combine(Styles, Sym, Tag, '', '', PSeudo, '', Properties, Attributes, ParentIndexInPropStack);
end;

procedure TProperties.Combine(Styles: TStyleList; Sym: TElemSymb;
  const Tag, AClass, AnID, PSeudo, ATitle: ThtString; AProp: TProperties; Attributes: TAttributeList; ParentIndexInPropStack: Integer);
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

    //-- BG ------------------------------------------------------ 12.06.2016 --
    procedure MergeAttrs(const Attributes: TAttributeList);
    {Merge Attributes in this TProperties.}
    var
      I: Integer;
      A: TAttribute;
//      T: TElemSymb;
    begin
      if (Attributes <> nil) and (Attributes.Count > 0) then
      begin
        for I := 0 to Attributes.Count - 1 do
        begin
          A := Attributes[I];
          case A.Which of
            AlignSy:
              case Sym of
                DivSy,
                H1Sy..H6Sy,
                PSy,
                ColSy,
                ColGroupSy,
                THeadSy..TFootSy,
                TDSy, THSy, TRSy:
                  if A.Name = 'char' then
                    Props[TextAlign] := 'right'
                  else
                    Props[TextAlign] := A.Name;
              end;

//            BackgroundSy: Props[BackgroundImage] := 'url(' + A.Name + ')';
//            BGColorSy:    Props[BackgroundColor] := A.Name;

            BorderSy:
              begin
                Props[BorderTopWidth] := A.Name;
                Props[BorderRightWidth] := A.Name;
                Props[BorderBottomWidth] := A.Name;
                Props[BorderLeftWidth] := A.Name;
              end;

            BorderColorSy:
              begin
                Props[BorderTopColor] := A.Name;
                Props[BorderRightColor] := A.Name;
                Props[BorderBottomColor] := A.Name;
                Props[BorderLeftColor] := A.Name;
              end;

//            CellPaddingSy:
//              begin
//                Props[MarginTop] := A.Name;
//                Props[MarginRight] := A.Name;
//                Props[MarginBottom] := A.Name;
//                Props[MarginLeft] := A.Name;
//              end;

            CellSpacingSy:
              begin
                Props[BorderSpacingHorz] := A.Name;
                Props[BorderSpacingVert] := A.Name;
              end;

            //ColorSy:;
            //HeightSy:;
            //HSpaceSy:;
            //LeftMarginSy:;
            //LinkSy:;
            //TopMarginSy:;
            //MarginHeightSy:;
            //MarginWidthSy:;

            //StartSy,
            //TypeSy,

            VAlignSy: Props[VerticalAlign] := A.Name;

            //ValueSy:;
            //VLinkSy:;
            //VSpaceSy:;
            //WidthSy:;
          end;
        end;
      end;
    end;

    procedure Merge(Source: TProperties; Reverse: Boolean = False);
    var
      Index: ThtPropIndices;
      I: FIIndex;
      Wt: Integer;
      S1: ThtString;
    begin
{$ifdef JPM_DEBUGGING_STYLES}
      CodeSiteLogging.CodeSite.EnterMethod('TProperties.Combine CombineX Merge');
      CodeSiteLogging.CodeSite.Send('Parameters');
      CodeSiteLogging.CodeSite.AddSeparator;
      LogProperties(Source,'Source');
{$endif}
      for Index := Low(Index) to High(Index) do
      begin
        if Reverse then
        begin
          if (Props[Index] <> Unassigned) and not VarIsIntNull(Props[Index]) then
            continue;
        end;
        if Important[Index] and not Source.Important[Index] then
          continue;
        if VarIsStr(Source.Props[Index]) and (Source.Props[Index] = 'inherit') then
          continue;
        if (VarType(Source.Props[Index]) <> varEmpty) and (Vartype(Source.Props[Index]) <> varNull) then
          case Index of
            MarginTop..BorderSpacingVert:
              if VarIsStr(Source.Props[Index]) then // if VarType(Source.Props[Index]) = VarString then
              begin
                Props[Index] := Source.Props[Index];
                Important[Index] := Source.Important[Index];
                Originals[Index] := True;
              end
              else if Source.Props[Index] <> IntNull then
              begin
                Props[Index] := Source.Props[Index];
                Important[Index] := Source.Important[Index];
                Originals[Index] := True;
              end;

            TextDecoration:
              begin
                Important[Index] := Source.Important[Index];
                Originals[Index] := True;
                S1 := Props[Index];
                if (S1 = 'none') or (Length(S1) = 0) or (Source.Props[Index] = 'none') then
                  Props[Index] := Source.Props[Index]
                else if Pos(Source.Props[Index], S1) = 0 then
                  Props[Index] := S1 + SpcChar + Source.Props[Index];
                if InLink then
                begin
                  for I := LFont to HVFont do
                    with FIArray.Ar[I] do
                      if Props[Index] = 'none' then
                        iStyle := iStyle - [fsStrikeOut, fsUnderline]
                      else
                      begin
                        if Pos('underline', Props[Index]) > 0 then
                          Include(iStyle, fsUnderline);
                        if Pos('line-through', Props[Index]) > 0 then
                          Include(iStyle, fsStrikeOut);
                      end;
                end;
              end;

            FontFamily, FontSize, FontStyle, FontWeight, Color, BackgroundColor, LetterSpacing:
              begin
                Important[Index] := Source.Important[Index];
                Originals[Index] := True;
                Props[Index] := Source.Props[Index];
                if InLink then
                  for I := LFont to HVFont do
                    with FIArray.Ar[I] do
                      case Index of
                        FontFamily:
                          begin
                            S1 := ReadFontName(Props[Index]);
                            if S1 <> '' then
                              iName := S1;
                          end;

                        FontSize:
                          iSize := FontSizeConv(Props[Index], iSize, DefPointSize, PixelsPerInch, FUseQuirksMode);

                        Color:
                          iColor := Props[Index];

                        BackgroundColor:
                          ibgColor := Props[Index];

                        FontStyle:
                          if (Props[Index] = 'italic') or (Props[Index] = 'oblique') then
                            iStyle := iStyle + [fsItalic]
                          else if Props[Index] = 'normal' then
                            iStyle := iStyle - [fsItalic];

                        FontWeight:
                          if Pos('bold', Props[Index]) > 0 then
                            iStyle := iStyle + [fsBold]
                          else if Pos('normal', Props[Index]) > 0 then
                            iStyle := iStyle - [fsBold]
                          else
                          begin
                            Wt := StrToIntDef(Props[Index], 0);
                            if Wt >= 600 then
                              iStyle := iStyle + [fsBold];
                          end;

                        LetterSpacing:
                          iCharExtra := Props[Index];
                      end;
              end
          else
            begin
              Props[Index] := Source.Props[Index];
              Important[Index] := Source.Important[Index];
              Originals[Index] := True; {it's defined for this item, not inherited}
            end;
          end;
      end;
{$ifdef JPM_DEBUGGING_STYLES}
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
      A: array[1..20] of record
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
        while (I > 0) and (N < 20) do
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

    procedure MergeItems(const Item: ThtString; Reverse: Boolean = False);
    {look up items in the Style list.  If found, merge them in this TProperties.
     Items may be duplicated in which case the last has priority.  Items may be
     simple tags like 'p', 'blockquote', 'em', etc or they may be more complex
     like  p.class, em#id, a.class:link, etc}
    var
      X, Y: Integer;
    begin
      if Styles.Find(Item, X) then
      begin
        if Reverse then
        begin
          // Reverse is used to set unassigned values only.
          Y := X;
          Inc(X);
          while (X < Styles.Count) and (Styles[X] = Item) do
          begin //duplicates, last one has highest priority
            Inc(X);
          end;
          // merge in reverse order
          while X > Y do
          begin
            Dec(X);
            Merge(Styles.Objects[X] as TProperties, Reverse);
          end;
        end
        else
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
    if FUseQuirksMode and ((Tag = 'td') or (Tag = 'th')) then
      OldSize := DefPointSize
    else if (VarType(Props[FontSize]) in VarNum) and (Props[FontSize] > 0.0) then {should be true}
      OldSize := Props[FontSize]
    else
      OldSize := DefPointSize;

  {Some hover and visited items adequately taken care of when link processed}
    NoHoverVisited := (Pseudo = '') or ((Pseudo <> 'hover') and (Pseudo <> 'visited'));

  // in the following, lowest priority on top, highest towards bottom.

    if (Tag = 'a') and ((Pseudo = 'link') or (Pseudo = 'visited')) then
      MergeItems('::' + Pseudo); {default Pseudo definition}

    if NoHoverVisited then
      MergeItems(Tag);

    MergeAttrs(Attributes);

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

    if (Tag = 'a') and not ((Pseudo = 'hover') or (Pseudo = 'active')) then
      // BG, 02.02.2013: github-issue 25: Multiple pseudo elements can apply
      // Just assign defaults for what is still unassigned:
      MergeItems('::' + Pseudo, True); {default Pseudo definition}

    if not (VarType(Props[FontSize]) in varNum) then {if still a ThtString, hasn't been converted}
      Props[FontSize] := FontSizeConv(Props[FontSize], OldSize, FDefFontSizeInPt, PixelsPerInch, FUseQuirksMode);
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
  PropSym := Sym;
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

function TProperties.GetFont: ThtFont;
var
  Font: ThtFontInfo;
begin {call only if all things valid}
  if TheFont = nil then
  begin
    GetSingleFontInfo(Font);
    TheFont := AllMyFonts.GetFontLike(Font, PixelsPerInch);
  end;
  Result := TheFont;
end;

{----------------LowerCaseUnquotedStr}
{Imporant:  In many CSS values, a quoted string or anything
in parathesis should be left in it's original case.
Such a substring may be case-sensitive.

Examples include URI filenames and Base64-encoded data.
}
function LowerCaseUnquotedStr(const S : THtString) : THtString;
var
  Top: ThtChar;
  MoreStack: ThtString;
  LCh : ThtChar;
  idx, len : Integer;

  procedure Push(Ch: ThtChar);
  var
    I: Integer;
  begin
    if Top <> EofChar then
    begin
      I := Length(MoreStack) + 1;
      SetLength(MoreStack, I);
      MoreStack[I] := Top;
    end;
    Top := Ch;
  end;

  procedure Pop;
  var
    I: Integer;
  begin
    I := Length(MoreStack);
    if I > 0 then
    begin
      Top := MoreStack[I];
      SetLength(MoreStack, I - 1);
    end
    else
      Top := EofChar;
  end;

begin
  if (Pos('''',S) = 0) and (Pos('(',S)=0) and (Pos('"',S) = 0) then
  begin
    Result := LowerCase(S);
    exit;
  end;
  len := Length(S);
  SetLength(Result,len);
  for idx := 1 to Len do begin
    LCh := S[idx];
    case LCh of
      '(' :
        begin
          if LCh = Top then
            Pop
        else
            Push(LCh);
        end;
      '''','"' :
        begin
          Push(LCh);
        end;
       ')' :
        begin
          if LCh = Top then
             Pop;
        end;
        'A'..'Z' :
        begin
          if Top = EofChar then
            LCh := ThtChar(Word(LCh) or $0020);
        end;
    end;
    Result[idx] := LCh;
  end;
end;

{----------------RemoveQuotes}

function RemoveQuotes(const S: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
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
    Generic1: array[1..AMax] of ThtString = ('serif'  , 'monospace', 'sans-serif', 'cursive'  , 'helvetica');
    Generic2: array[1..AMax] of ThtString = (FontSerif, FontMono   , FontSans    , FontCursive, FontHelvet );
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
      if htCompareText(Result, Generic1[I]) = 0 then
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
    Done := Screen.Fonts.IndexOf( htStringToString(S1) ) >= 0;
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
  if Pos('underline', Props[TextDecoration]) > 0 then
    Include(Style, fsUnderline);
  if Pos('line-through', Props[TextDecoration]) > 0 then
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
    PropStack.Insert(N, TProperties.Create(PropStack, FUseQuirksMode, FPixelsPerInch));
    PropStack[N].Inherit('', PropStack[N - 1]);
    PropStack[N].Combine(Styles, PropSym, PropTag, PropClass, PropID, Pseudo, PropTitle, PropStyle, nil {PropAttr}, N - 1);
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

procedure TProperties.GetVMarginArrayDefBorder(var MArray: ThtVMarginArray; const ADefColor : Variant);
var
  I: ThtPropIndices;
  BS: ThtBorderStyle;
  NewColor : TColor;
  LVal : THtString;
begin
{$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.GetVMarginArrayDefBorder');
  LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.SendFmtMsg('ADefColor = %s',[LogPropColor( ADefColor )]);
{$ENDIF}
  for I := Low(MArray) to High(MArray) do
    case I of
      BorderTopStyle..BorderLeftStyle:
      begin
        BS := MArray[I];
        GetBorderStyle(I, BS);
        MArray[I] := BS;
      end;

      BorderTopColor..BorderLeftColor:
      begin
        if TryStrToColor(Props[I],False,NewColor) then
          MArray[I] := Props[I]
        else
        begin
          LVal := Props[I];
          if LVal = CurColor_Val then
            // 'currentColor'
            MArray[I] := Props[StyleUn.Color]
          else
            MArray[I] := ADefColor;
        end;

      end
    else
      MArray[I] := Props[I];
    end;
{$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogTVMarginArray(MArray,'MArray');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.GetVMarginArrayDefBorder');
{$ENDIF}
end;

procedure TProperties.GetVMarginArray(var MArray: ThtVMarginArray);
{From: http://www.w3.org/TR/CSS21/box.html#x49

If an element's border color is not specified with a
border property, user agents must use the value of the
element's 'color' property as the computed value for
the border color.
}
begin
{$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.GetVMarginArray');
  LogProperties(Self,'Self');
{$ENDIF}
  GetVMarginArrayDefBorder(MArray,Props[StyleUn.Color]);
{$IFDEF JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  StyleUn.LogTVMarginArray(MArray,'MArray');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.GetVMarginArray');
{$ENDIF}
end;

procedure TProperties.AddPropertyByIndex(Index: ThtPropIndices; PropValue: ThtString; IsImportant: Boolean);
var
  NewColor: TColor;
  WhiteSpaceStyle : ThtWhiteSpaceStyle;
begin
{$ifdef JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.AddPropertyByIndex');
  CodeSiteLogging.CodeSite.Send('Parameters');
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.SendFmtMsg('Index = %s',[PropWords[Index]]);
  LogProperties(Self,'Self');
{$endif}
  case Index of
    BorderTopColor..BorderLeftColor:
      if TryStrToColor(PropValue, False, NewColor) then
        Props[Index] := NewColor
      else if LowerCase(PropValue) = CurColorStr then
        Props[Index] := CurColor_Val;

    Color, BackgroundColor:
      if TryStrToColor(PropValue, False, NewColor) then
        Props[Index] := NewColor
      else if Index = Color then
        Props[Index] := clBlack
      else
        Props[Index] := clNone;

    Visibility:
      if PropValue = 'visible' then
        Props[Visibility] := viVisible
      else if PropValue = 'hidden' then
        Props[Visibility] := viHidden;

    TextTransform:
      if PropValue = 'uppercase' then
        Props[TextTransform] := txUpper
      else if PropValue = 'lowercase' then
        Props[TextTransform] := txLower
      else
        Props[TextTransform] := txNone;

    WordWrap:
      if PropValue = 'break-word' then
        Props[WordWrap] := PropValue
      else
        Props[WordWrap] := 'normal';

    piWhiteSpace:
      if TryStrToWhiteSpace(PropValue,WhiteSpaceStyle) then
        Props[piWhiteSpace] := PropValue;

    FontVariant:
      if PropValue = 'small-caps' then
        Props[FontVariant] := PropValue
      else if PropValue = 'normal' then
        Props[FontVariant] := 'normal';

  else
    Props[Index] := PropValue;
  end;

  Important[Index] := IsImportant;

{$ifdef JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.AddSeparator;
  CodeSiteLogging.CodeSite.Send('Results');
  CodeSiteLogging.CodeSite.AddSeparator;
  LogProperties(Self,'Self');
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TProperties.AddPropertyByIndex');
{$endif}
end;

procedure TProperties.AddPropertyByName(const PropName, PropValue: ThtString; IsImportant: Boolean);
var
  Index: ThtPropIndices;
begin
{$ifdef JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TProperties.AddPropertyByName');
{$endif}
  if TryStrToPropIndex(PropName, Index) then
    AddPropertyByIndex(Index, PropValue, IsImportant);
{$ifdef JPM_DEBUGGING_STYLES}
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
  FUseQuirksMode := False;
end;

constructor TStyleList.Create(AUseQuirksMode: Boolean);
begin
  // Do not call the inherited constructor with 1 boolean parameter!
  // It's purpose is different (OwnsObjects: Boolean)!
  Create;
  FDefProp := nil;
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
begin
  {used to help sort contextual items by entry sequence}
  // BG, 23.05.2013: Without fixed width for the number string entries 12 and 111 are sorted literally:
  // 'ul 111'
  // 'ul 12'.
  // With fixed width the sort order is:
  // 'ul 00012'
  // 'ul 00111'
  Result := htString(Format('%.5d', [SeqNo]));
  Inc(SeqNo);
end;

procedure FixBordProps(AProp, BodyProp : TProperties);
 {$ifdef UseInline} inline; {$endif}
var i : ThtPropIndices;
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
  I := -1;
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

procedure TStyleList.AddModifyProp(const Selector, Prop, Value: ThtString; IsImportant: Boolean);
{strings are all lowercase here}
var
  I: Integer;
  PropIndex: ThtPropIndices;
  Propty: TProperties;
  NewColor: TColor;
  NewProp: Boolean;
begin
{$ifdef JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.EnterMethod(Self,'TStyleList.AddModifyProp');
  CodeSiteLogging.CodeSite.Send('Parameters');

  CodeSiteLogging.CodeSite.SendFmtMsg('Selector = %s',[Selector]);
  CodeSiteLogging.CodeSite.SendFmtMsg('Prop = %s',[Prop]);
  CodeSiteLogging.CodeSite.SendFmtMsg('Value = %s',[Value]);
  CodeSiteLogging.CodeSite.AddSeparator;
  {$endif}
  if TryStrToPropIndex(Prop, PropIndex) then
  begin
    I := -1;
    if not Find(Selector, I) then
    begin
      NewProp := True;
      Propty := TProperties.Create(UseQuirksMode, PixelsPerInch); {newly created property}
      Propty.DefPointSize := FDefFontSizeInPt;
    end
    else
    begin
      Propty := TProperties(Objects[I]); {modify existing property}
      NewProp := False;
    end;

    if PropIndex = Color then
    begin
      if TryStrToColor(Value, False, NewColor) then
      begin
        if Selector = ':link' then
        begin {changed the defaults to be the same as link}
          ModifyLinkColor('hover', NewColor);
          ModifyLinkColor('visited', NewColor);
        end
        else if Selector = ':visited' then
          ModifyLinkColor('hover', NewColor);
      end;
    end;

    Propty.AddPropertyByIndex(PropIndex, Value, IsImportant);

    if NewProp then
      AddObject(Selector, Propty); {it's a newly created property}
    if Pos(':hover', Selector) > 0 then
      LinksActive := True;
    if Selector = 'a' then
    begin
      AddModifyProp('::link', Prop, Value, IsImportant); {also applies to ::link}
    end;
    if UseQuirksMode then begin
      if (Selector = 'body') and (PropIndex = Color) then begin
        FixupTableColor(Propty);
      end;
    end;
  end;
{$ifdef JPM_DEBUGGING_STYLES}
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TStyleList.AddModifyProp');
{$endif}
end;

function TStyleList.AddObject(const S: ThtString; AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
  TProperties(AObject).PropTag := S;
  TProperties(AObject).FDefFontSizeInPt := DefPointSize;
end;

function TStyleList.AddDuplicate(const Tag: ThtString; Prop: TProperties): TProperties;
begin
  Result := TProperties.Create(Prop.PropStack, FUseQuirksMode, FPixelsPerInch);
  Result.Copy(Prop);
  AddObject(Tag, Result);
end;

procedure TStyleList.ModifyLinkColor(Pseudo: ThtString; AColor: TColor);
var
  I: Integer;
begin
  I := -1;
  if Find('::' + Pseudo, I) then {the defaults}
    with TProperties(Objects[I]) do
      Props[Color] := AColor;
end;

procedure TStyleList.Initialize(const FontName, PreFontName: ThtString; PointSize: Integer;
  AColor, AHotspot, AVisitedColor, AActiveColor: TColor; LinkUnderline: Boolean;
  ACodePage: TBuffCodePage; ACharSet: TFontCharSet; MarginHeight, MarginWidth: Integer);
type
  ListTypes = (ul, ol, menu, dir, dl, dd, blockquote);
const
  ListStr: array[Low(ListTypes)..High(ListTypes)] of ThtString =
  ('ul', 'ol', 'menu', 'dir', 'dl', 'dd', 'blockquote');
var
  HIndex: Integer;
  Properties: TProperties;
  J: ListTypes;

begin
  Clear;
  DefPointSize := PointSize;

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.DefFontname := FontName;
  Properties.Props[FontFamily] := FontName;
  Properties.Props[FontSize] := PointSize;
  Properties.Props[FontStyle] := 'none';
  Properties.Props[FontWeight] := 'normal';
  Properties.Props[TextAlign] := 'left';
  Properties.Props[TextDecoration] := 'none';
  Properties.Props[TextTransform] := txNone;
  Properties.Props[WordWrap] := 'normal';
  Properties.Props[piWhiteSpace] := 'normal';
  Properties.Props[FontVariant] := 'normal';
  Properties.Props[Color] := AColor;
  Properties.Props[MarginTop] := MarginHeight;
  Properties.Props[MarginBottom] := MarginHeight;
  Properties.Props[MarginLeft] := MarginWidth;
  Properties.Props[MarginRight] := MarginWidth;
  Properties.Props[Visibility] := viVisible;
  Properties.Props[LetterSpacing] := 0;
  Properties.Props[BoxSizing] := ContentBox;
  Properties.CodePage := ACodePage;
  Properties.CharSet := ACharSet;
  AddObject('default', Properties);
  FDefProp := Properties;

  if UseQuirksMode then begin
    Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
    Properties.Props[FontSize] := PointSize * 1.0;
    Properties.Props[FontStyle] := 'none';
    Properties.Props[FontWeight] := 'normal';
    Properties.Props[Color] := AColor;
    AddObject('td', Properties);
    Properties := AddDuplicate('table', Properties);

    Properties := AddDuplicate('th', Properties);
    Properties.Props[FontWeight] := 'bold';
  end;

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[Color] := AHotSpot or PalRelative;
  if LinkUnderline then
    Properties.Props[TextDecoration] := 'underline'
  else
    Properties.Props[TextDecoration] := 'none';
  AddObject('::link', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[Color] := AVisitedColor or PalRelative;
  AddObject('::visited', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[Color] := AActiveColor or PalRelative;
  AddObject('::hover', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  AddObject('null', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontFamily] := PreFontName;
  Properties.Props[FontSize] := PointSize * 10.0 / 12.0;
  Properties.Props[FontStyle] := 'none';
  Properties.Props[FontWeight] := 'normal';
  Properties.Props[TextDecoration] := 'none';
  Properties.Props[piWhiteSpace] := 'pre';
  AddObject('pre', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[MarginTop] := AutoParagraph;
  Properties.Props[MarginBottom] := AutoParagraph;
  AddObject('p', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[MarginTop] := 0;
  AddObject('p 11pre', Properties);

  for J := Low(ListTypes) to High(ListTypes) do
  begin
    Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
    case J of
      ol, ul, menu, dir:
      begin
        Properties.Props[ListStyleType] := 'blank';
        Properties.Props[MarginTop] := AutoParagraph;
        Properties.Props[MarginBottom] := AutoParagraph;
        Properties.Props[MarginLeft] := IntNull;
        Properties.Props[PaddingLeft] := ListIndent;
      end;

      dl:
      begin
        Properties.Props[ListStyleType] := 'none';
        Properties.Props[MarginLeft] := 0;
        Properties.Props[MarginTop] := 0;
        Properties.Props[MarginBottom] := 0;
        Properties.Props[MarginLeft] := 0;
      end;

      blockquote:
      begin
        Properties.Props[MarginTop] := AutoParagraph;
        Properties.Props[MarginBottom] := ParagraphSpace;
        Properties.Props[MarginLeft] := ListIndent;
      end;

      dd:
      begin
        Properties.Props[MarginTop] := 0;
        Properties.Props[MarginBottom] := 0;
        Properties.Props[MarginLeft] := ListIndent;
      end;
    end;
    AddObject(ListStr[J], Properties);
  end;

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontFamily] := PrefontName;
  Properties.Props[FontSize] := '0.83em'; {10.0 / 12.0;}
  AddObject('code', Properties);
  AddDuplicate('tt', Properties);
  AddDuplicate('kbd', Properties);
  AddDuplicate('samp', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontWeight] := 'bold';
  AddObject('b', Properties);
  AddDuplicate('strong', Properties);
  if UseQuirksMode = False then begin
    AddDuplicate('th', Properties);
    Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
    Properties.Props[TextAlign] := 'none';
    AddObject('table', Properties);
  end;

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontSize] := '0.83em';
  Properties.Props[VerticalAlign] := 'super';
  AddObject('sup', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontSize] := '0.83em';
  Properties.Props[VerticalAlign] := 'sub';
  AddObject('sub', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontSize] := '1.17em';
  AddObject('big', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontSize] := '0.83em';
  AddObject('small', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontStyle] := 'italic';
  AddObject('i', Properties);
  AddDuplicate('em', Properties);
  AddDuplicate('cite', Properties);
  AddDuplicate('var', Properties);
  AddDuplicate('dfn', Properties);

  AddDuplicate('address', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[TextDecoration] := 'underline';
  AddObject('u', Properties);
  AddDuplicate('ins',Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[TextDecoration] := 'line-through';
  AddObject('s', Properties);
  AddDuplicate('strike', Properties);
  AddDuplicate('del',Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[TextAlign] := 'center';
  AddObject('center', Properties);
  AddDuplicate('caption', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
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

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[MarginLeft] := 0;
  Properties.Props[MarginRight] := 0;
  Properties.Props[MarginTop] := 10;
  Properties.Props[MarginBottom] := 10;
  AddObject('hr', Properties);

  for HIndex := 1 to 6 do
  begin
    Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
    case HIndex of
      1: Properties.Props[FontSize] := '2em';
      2: Properties.Props[FontSize] := '1.5em';
      3: Properties.Props[FontSize] := '1.17em';
      4: Properties.Props[FontSize] := '1em';
      5: Properties.Props[FontSize] := '0.83em';
      6: Properties.Props[FontSize] := '0.75em';
    end;
    case HIndex of
      1: Properties.Props[MarginTop] := '0.67em';
      2: Properties.Props[MarginTop] := '0.75em';
      3: Properties.Props[MarginTop] := '0.83em';
      4: Properties.Props[MarginTop] := '1.12em';
      5: Properties.Props[MarginTop] := '1.5em';
      6: Properties.Props[MarginTop] := '1.67em';
    end;
    Properties.Props[MarginBottom] := Properties.Props[MarginTop];
    Properties.Props[FontWeight] := 'bolder';
    AddObject( htString('h' + IntToStr(HIndex)), Properties);
  end;

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[FontStyle] := 'none';
  Properties.Props[BackgroundColor] := $00FFFF;
  Properties.Props[Color] := $000000;
  AddObject('mark', Properties);

  Properties := TProperties.Create(UseQuirksMode, PixelsPerInch);
  Properties.Props[ StyleUn.BorderBottomStyle ] := 'dotted';
  Properties.Props[ StyleUn.BorderBottomWidth ] := '1px';
  AddObject('abbr', Properties);
  AddDuplicate('acronym',Properties);
end;

{ TPropStack }

{$ifdef UseGenerics}
{$else}
function TPropStack.GetProp(Index: Integer): TProperties;
begin
  Result := Get(Index); //TProperties(inherited Items[Index]);
end;
{$endif}

function TPropStack.Last: TProperties;
begin
  Result := TProperties(inherited Items[Count - 1]);
end;

const
  NumColors = 178;
  Colors: array[1..NumColors] of ThtString = (
    'none', 'transparent',

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
    'rebeccapurple',  //CSS4 tribute to Eric Meyer's daughter, Rebecca, died of cancer on her sixth birthday
    'background', 'activecaption', 'inactivecaption', 'menu', 'window',
    'windowframe', 'menutext', 'windowtext', 'captiontext', 'activeborder',
    'inactiveborder', 'appworkSpace', 'highlight', 'hightlighttext', 'buttonface',
    'buttonshadow', 'graytext', 'buttontext', 'inactivecaptiontext', 'buttonhighlight',
    'threeddarkshadow', 'threedlightshadow', 'infotext', 'infobackground', 'scrollbar',
    'threedface', 'threedhighlight', 'threedshadow');

  ColorValues: array[1..NumColors] of TColor = (clNone, clNone,
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
    clGray, $A9A9A9, $4F4F2F, $696969, $D3D3D3, $998877, $908070,
    $663399,
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
 {$ifdef UseInline} inline; {$endif}
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

function TryStrToColor(S: ThtString; NeedPound: Boolean; out Color: TColor): Boolean;
 {$ifdef UseInline} inline; {$endif}
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
      S := Copy(S, 1, J - 1);
      S := Trim(Copy(S, I + 1, 255));
      for K := hue to saturation do
      begin
        I := Pos(',', S);
        A[K] := Trim(Copy(S, 1, I - 1));
        S := Trim(Copy(S, I + 1, 255));
      end;
      I := Pos(',', S);
      if I > 0 then begin
        A[luminance] := Trim(Copy(S, 1, I - 1));
        S := Trim(Copy(S, I + 1, 255));
        VOpacity := OpacityFromStr(S);
      end else begin
        A[luminance] := S;
        VOpacity := 255;
      end;

      C[hue] := StrToIntDef( htStringToString(A[hue]), 0);
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
        C[K] := StrToIntDef( htStringToString(A[K]), 0);
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
            C[K] := Round( StrToFloat( htStringToString(A[K])) * 2.55);
          except
            C[K] := 0;
          end;
        end
        else
          C[K] := StrToIntDef( htStringToString(A[K]), 0);
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
    Idx := -1;
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
      S := Trim(S);
      if Length(S) <= 3 then
        for I := Length(S) downto 1 do
          Insert(S[I], S, I); {Double each character}
      Result := TryStrToInt('$' + htStringToString(S), Int);
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

//BG, 14.07.2010:
function DecodeSize(const Str: ThtString; out V: extended; out U: ThtString): Boolean;
 {$ifdef UseInline} inline; {$endif}
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

function IncFontSize(OldSize: Double; Increment: ThtFontSizeIncrement): Double;
 {$ifdef UseInline} inline; {$endif}
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

function FontSizeConv(const Str: ThtString; OldSize, DefFontSizeInPt: Double; PixelsPerInch: Integer; const AUseQuirksMode : Boolean): Double;
{given a font-size ThtString, return the point size}
var
  V: extended;
  U: ThtString;
  i : Integer;
begin
  if DecodeSize(Str, V, U) then
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
      Result := V * 72.0 / PixelsPerInch
    else if U = 'pc' then
      Result := V * 12.0
    else if U = 'em' then
      Result := V * OldSize
    else if U = 'ex' then
      Result := V * OldSize * 0.5 {1/2 of em}
    else if U = '%' then
      Result := V * OldSize * f_pc
    else if U = '' then
      Result := V * 72.0 / PixelsPerInch {pixels by default}
    else
      Result := DefFontSizeInPt; {error, return 12pt}
  end
  else
  begin
    U := Str;

    if AUseQuirksMode then
      i := 1
    else
      i := 0;

    if U = 'smaller' then
      Result := IncFontSize(OldSize, -1) // CSS1: 0.75 * OldSize
    else if U = 'larger' then
      Result := IncFontSize(OldSize,  1) // CSS1: 1.25 * OldSize
    else if U = 'xx-small' then
      Result := FontConv[1 + i]
    else if U = 'x-small' then
      Result := FontConv[1 + i]         // same size xx-small (IE and Firefox do it). 
    else if U = 'small' then
      Result := FontConv[2 + i]
    else if U = 'medium' then           // 'medium' is the user's preferred font size.
      Result := FontConv[3 + i]
    else if U = 'large' then
      Result := FontConv[4 + i]
    else if U = 'x-large' then
      Result := FontConv[5 + i]
    else if U = 'xx-large' then
      Result := FontConv[6]
    else
      Result := DefFontSizeInPt;
  end;
end;

{----------------LengthConv}

function LengthConv(const Str: ThtString; Relative: Boolean; Base, EmSize, ExSize, Default, PixelsPerInch: Integer): Integer;
 {$ifdef UseInline} inline; {$endif}
{given a length ThtString, return the appropriate pixel value.  Base is the
 base value for percentage. EmSize, ExSize for units relative to the font.
 Relative makes a numerical entry relative to Base.
 Default returned if no match.}
var
  V: Extended;
  U: ThtString;
begin
  if DecodeSize(Str, V, U) then
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
      V := V * PixelsPerInch
    else if U = 'cm' then
      V := V * PixelsPerInch * f_cm
    else if U = 'mm' then
      V := V * PixelsPerInch * f_mm
    else if U = 'pt' then
      V := V * PixelsPerInch * f_pt
    else if U = 'px' then
    else if U = 'pc' then
      V := V * PixelsPerInch * f_px
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

procedure CalcAutoMinMaxConstraints(W, H, MinW, MaxW, MinH, MaxH: Integer; out ResW, ResH: Integer);
// solving min/max constraint violations as described in CSS 2.1

  procedure AdjustMinMax(var Min, Max: Integer);
  begin
    if Min < 0 then
      Min := 0;
    if Max < 0 then
      Max := MaxInt;
    if Max < Min then
      Max := Min;
  end;

var
  FacW, FacH: Double;
begin
  AdjustMinMax(MinW, MaxW);
  AdjustMinMax(MinH, MaxH);

{
  if (w > max-width)
    if (h < min-height)
        max-width 	min-height
    else if (h > max-height)
      if (max-width/w > max-height/h)
        max(max-height * w/h, min-width) 	max-height
      else
        max-width 	max(min-height, max-width * h/w)
    else
        max-width 	max(min-height, max-width * h/w)
}
  if W > MaxW then
  begin
    if H < MinH then
    begin
      ResW := MaxW;
      ResH := MinH;
      Exit;
    end;

    FacW := MaxW / W;
    if H > MaxH then
    begin
      FacH := MaxH / H;
      if FacW > FacH then
      begin
        ResH := MaxH;
        ResW := Max(Round(FacH * W), MinW);
        Exit;
      end;
    end;

    ResW := MaxW;
    ResH := Max(Round(FacW * H), MinH);
    Exit;
  end;

{
  if (w < min-width)
    if (h > max-height)
        min-width 	max-height
    else if (h < min-height)
      if (min-width/w <= min-height/h)
        min(max-width, min-height * w/h) 	min-height
      else
        min-width 	min(min-width * h/w, max-height)
    else
       	min-width 	min(min-width * h/w, max-height)
}
  if W < MinW then
  begin
    if H > MaxH then
    begin
      ResW := MinW;
      ResH := MaxH;
      Exit;
    end;

    FacW := MinW / W;
    if H < MinH then
    begin
      FacH := MinH / H;
      if FacW <= FacH then
      begin
        ResH := MinH;
        ResW := Min(Round(FacH * W), MaxW);
        Exit;
      end;
    end;

    ResW := MinW;
    ResH := Min(Round(FacW * H), MaxH);
    Exit;
  end;

  { h > max-height 	max(max-height * w/h, min-width) 	max-height }
  if H > MaxH then
  begin
    ResH := MaxH;
    ResW := Max((MaxH * W) div H, MinW);
    Exit;
  end;

  { h < min-height 	min(min-height * w/h, max-width) 	min-height }
  if H < MinH then
  begin
    ResH := MinH;
    ResW := Min((MinH * W) div H, MaxW);
    Exit
  end;

  ResW := W;
  ResH := H;
end;

initialization
finalization
  FreeAndNil(ColorStrings);
  FreeAndNil(PropertyStrings);
end.
