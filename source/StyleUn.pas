{
Version   11.5
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

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
  //
  HtmlBuffer,
  HtmlFonts,
  HtmlGlobals,
  Parser,
  StyleTypes;

type
  TPropertyIndex = (
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
    WordWrap, FontVariant, BorderCollapse, OverFlow, piDisplay, piEmptyCells,
    piWhiteSpace,

    // short hands
    MarginX, PaddingX, BorderWidthX, BorderX,
    BorderTX, BorderRX, BorderBX, BorderLX,
    FontX, BackgroundX, ListStyleX, BorderColorX,
    BorderStyleX
  );

  TShortHand = MarginX..BorderStyleX;

  PropIndices = FontFamily..piWhiteSpace;
  TPropertyArray = array [PropIndices] of Variant;

  MarginIndices = BackgroundColor..LeftPos;
  TVMarginArray = array [MarginIndices] of Variant;
  TMarginArray = array [MarginIndices] of Integer;

const
  PropWords: array [TPropertyIndex] of ThtString =
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
    'width', 'height', 'top', 'bottom', 'right', 'left',
    'visibility', 'line-height', 'background-image', 'background-position',
    'background-repeat', 'background-attachment', 'vertical-align', 'position', 'z-index',
    'list-style-type', 'list-style-image', 'float', 'clear', 'text-indent',
    'page-break-before', 'page-break-after', 'page-break-inside', 'text-transform',
    'word-wrap', 'font-variant', 'border-collapse', 'overflow', 'display', 'empty-cells',
    'white-space',

    // short hand names
    'margin', 'padding', 'border-width', 'border',
    'border-top', 'border-right', 'border-bottom', 'border-left',
    'font', 'background', 'list-style', 'border-color',
    'border-style'
  );

type
  AlignmentType = TAlignmentStyle;
  BorderStyleType = TBorderStyle;
  ClearAttrType = TClearStyle;
  TPropDisplay = TDisplayStyle;
  ListBulletType = TBulletStyle;
  PositionType = TBoxPositionStyle;
  TextTransformType = TTextTransformStyle;
  VisibilityType = TVisibilityStyle;

type
  TStyleList = class;
  TPropStack = class;

  TProperties = class
  private
    PropStack: TPropStack; // owner
    TheFont: ThtFont;
    InLink: Boolean;
    DefFontname: ThtString;
    FUseQuirksMode : Boolean;
    procedure AddPropertyByIndex(Index: PropIndices; PropValue: ThtString);
//    procedure AssignCharSet(CS: TFontCharset);
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
    Props: TPropertyArray;
    Originals: array[PropIndices] of Boolean;
    FIArray: TFontInfoArray;
    ID: Integer;
    constructor Create; overload;
    constructor Create(APropStack: TPropStack; const AUseQuirksMode : Boolean); overload; // for use in property stack
    constructor Create(const AUseQuirksMode : Boolean); overload; // for use in style list only
    destructor Destroy; override;
    function BorderStyleNotBlank: Boolean;
    function Collapse: Boolean;
    function GetBackgroundColor: TColor;
    function GetBackgroundImage(var Image: ThtString): Boolean;
    function GetBorderStyle(Index: PropIndices; var BorderStyle: BorderStyleType): Boolean;
    function GetClear(var Clr: ClearAttrType): Boolean;
    function GetDisplay: TPropDisplay; //BG, 15.09.2009
    function GetFloat(var Align: AlignmentType): Boolean;
    function GetFont: ThtFont;
    function GetFontVariant: ThtString;
    function GetLineHeight(NewHeight: Integer): Integer;
    function GetListStyleImage: ThtString;
    function GetListStyleType: ListBulletType;
    function GetOriginalForegroundColor: TColor;
    function GetPosition: PositionType;
    function GetTextIndent(var PC: Boolean): Integer;
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
    property CharSet: TFontCharset read FCharSet; // write AssignCharSet;
    property CodePage: Integer read FCodePage write AssignCodePage;
    property EmSize: Integer read FEmSize;
    property ExSize: Integer read FExSize;
    property UseQuirksMode : Boolean read FUseQuirksMode;
  end;

  TStyleList = class(ThtStringList)
  private
    SeqNo: Integer;
    FDefProp: TProperties;
  protected
    //this must be protected so that the property can be changed in
    //a descendant while being read only.
    FUseQuirksMode : Boolean;
    procedure setLinksActive(Value: Boolean); virtual; abstract;
    property LinksActive: Boolean write setLinksActive;
  public
    constructor Create; overload;
    constructor Create(const AUseQuirksMode : Boolean); overload;
    destructor Destroy; override;
    function AddDuplicate(const Tag: ThtString; Prop: TProperties): TProperties;
    function AddObject(const S: ThtString; AObject: TObject): Integer; override;
    function GetSeqNo: ThtString;
    procedure Clear; override;
    procedure AddModifyProp(const Selector, Prop, Value: ThtString);
{.$IFDEF Quirk}
    procedure FixupTableColor(BodyProp: TProperties);
{.$ENDIF}
    procedure Initialize(const FontName, PreFontName: ThtString;
      PointSize: Integer; AColor, AHotspot, AVisitedColor, AActiveColor: TColor;
      LinkUnderline: Boolean; ACodePage: TBuffCodePage; MarginHeight, MarginWidth: Integer);
    procedure ModifyLinkColor(Pseudo: ThtString; AColor: TColor);
    property UseQuirksMode : Boolean read FUseQuirksMode write FUseQuirksMode;
    property DefProp: TProperties read FDefProp;
  end;

  TPropStack = class(TObjectList)
  private
    function GetProp(Index: Integer): TProperties; {$ifdef UseInline} inline; {$endif}
  public
    function Last: TProperties; {$ifdef UseInline} inline; {$endif}
    property Items[Index: Integer]: TProperties read GetProp; default;
  end;

const
  IntNull = -12345678;
  Auto = -12348765;
  AutoParagraph = -12348766;
  ParagraphSpace = 14; {default spacing between paragraphs, etc.}
  ImageSpace = 3; {extra space for left, right images}
  ListIndent = 40;

  EastEurope8859_2 = 31; {for 8859-2}

// BG, 25.04.2012: added:
function IsAuto(const Value: Variant): Boolean;

//BG, 05.10.2010: added:
function VarIsIntNull(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}
function VarIsAuto(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}

function VMargToMarg(const Value: Variant; Relative: Boolean; Base, EmSize, ExSize, Default: Integer): Integer;

procedure ConvInlineMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize: Integer; var M: TMarginArray);
procedure ConvMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth: Integer; out AutoCount: Integer; var M: TMarginArray);
procedure ConvMargArrayForCellPadding(const VM: TVMarginArray; EmSize, ExSize: Integer; var M: TMarginArray);
procedure ConvVertMargins(const VM: TVMarginArray; BaseHeight, EmSize, ExSize: Integer; var M: TMarginArray; var TopAuto, BottomAuto: Boolean);

function SortedProperties: ThtStringList;

function ReadFontName(S: ThtString): ThtString;

procedure ApplyBoxWidthSettings(var AMarg : TMarginArray; var VMinWidth, VMaxWidth : Integer; const AUseQuirksMode : Boolean);
procedure ApplyBorderBoxModel(var AMarg : TMarginArray);
procedure ApplyBoxSettings(var AMarg : TMarginArray; const AUseQuirksMode : Boolean);

var
  FontConv: array[1..7] of Double;
  PreFontConv: array[1..7] of Double;

implementation

var
  DefPointSize: Double;
  CharsetPerCharset: array [TFontCharset] of record Inited: Boolean; Charset: TFontCharset; end;

function FontSizeConv(const Str: ThtString; OldSize: Double; const AUseQuirksMode : Boolean): Double; forward;
function LengthConv(const Str: ThtString; Relative: Boolean; Base, EmSize, ExSize, Default: Integer): Integer; forward;

//-- BG ---------------------------------------------------------- 15.03.2011 --
var
  PropertyStrings: ThtStringList;

function SortedProperties: ThtStringList;
var
  I: TPropertyIndex;
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
function TryStrToPropIndex(const PropWord: ThtString; var PropIndex: PropIndices): Boolean;
var
  I: Integer;
  P: TPropertyIndex;
begin
  I := SortedProperties.IndexOf(PropWord);
  Result := I >= 0;
  if Result then
  begin
    P := TPropertyIndex(SortedProperties.Objects[I]);
    Result := P in [Low(PropIndices)..High(PropIndices)];
    if Result then
      PropIndex := P;
  end;
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

constructor TProperties.Create(const AUseQuirksMode : Boolean);
begin
  Create;
  FUseQuirksMode := AUseQuirksMode;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
constructor TProperties.Create(APropStack: TPropStack; const AUseQuirksMode : Boolean);
begin
  Create;
  self.PropStack := APropStack;
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
  for I := Low(I) to High(I) do
    Props[I] := Source.Props[I];
end;

{----------------TProperties.CopyDefault}

procedure TProperties.CopyDefault(Source: TProperties);
var
  I: PropIndices;
begin
  for I := Low(I) to High(I) do
    Props[I] := Source.Props[I];
  CodePage := Source.CodePage;
  DefFontname := Source.DefFontname;
  PropTag := 'default';
end;

procedure TProperties.Inherit(Tag: ThtString; Source: TProperties);
{copy the properties that are inheritable}
var
  I: PropIndices;
  Span, HBF: Boolean;
  isTable: Boolean;
begin
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
        piWidth, piHeight,
        TopPos..LeftPos:
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

  FEmSize := Source.FEmSize; {actually this is calculated later }
  FExSize := Source.FExSize; {apparently correlates with what browsers are doing}
end;

{----------------TProperties.Update}

procedure TProperties.Update(Source: TProperties; Styles: TStyleList; I: Integer);
{Change the inherited properties for this item to those of Source}
var
  Index: PropIndices;
begin
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

//procedure TProperties.AssignCharSet(CS: TFontCharset);
//begin
//  AssignCharSetAndCodePage(CS, CharSetToCodePage(CS));
//end;

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
function TProperties.GetDisplay: TDisplayStyle;
begin
  if VarIsStr(Props[piDisplay]) then
    if TryStrToDisplayStyle(Props[piDisplay], Result) then
      exit;
  Result := pdUnassigned;
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TProperties.GetListStyleType: ListBulletType;
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

function TProperties.GetTextIndent(var PC: Boolean): Integer;
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
  var M: TMarginArray; var TopAuto, BottomAuto: Boolean);

  function Convert(V: Variant; var IsAutoParagraph: Boolean): Integer;
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
  M[MarginTop] := Convert(VM[MarginTop], TopAuto);
  M[MarginBottom] := Convert(VM[MarginBottom], BottomAuto);
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
end;

procedure ConvMargArrayForCellPadding(const VM: TVMarginArray; EmSize,
  ExSize: Integer; var M: TMarginArray);
{Return negative for no entry or percent entry}
var
  I: PropIndices;
begin
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
end;

{----------------ConvInlineMargArray}

procedure ConvInlineMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize,
  ExSize: Integer; {BStyle: BorderStyleType;} var M: TMarginArray);
{currently for images, form controls.  BaseWidth/Height and BStyle currently not supported}
var
  I: PropIndices;
begin
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
  {.$IFDEF Quirk}
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
   {.$ENDIF}
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

function TProperties.GetFont: ThtFont;
var
  Font: ThtFontInfo;
begin {call only if all things valid}
  if TheFont = nil then
  begin
    GetSingleFontInfo(Font);
    TheFont := AllMyFonts.GetFontLike(Font);
    FEmSize := TheFont.EmSize;
    FExSize := TheFont.ExSize;
  end;
  Result := ThtFont.Create;
  Result.Assign(TheFont);
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
        if TryStrToColor(Props[I], False, NewColor) then
          MArray[I] := Props[I]
        else if DefaultToColor then
          MArray[I] := Props[StyleUn.Color]
        else
          MArray[I] := IntNull;
      end
    else
      MArray[I] := Props[I];
    end;
end;

procedure TProperties.AddPropertyByIndex(Index: PropIndices; PropValue: ThtString);
var
  NewColor: TColor;
begin
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
      if TryStrToColor(PropValue, False, NewColor) then
        Props[Index] := NewColor;
    Color, BackgroundColor:
      if TryStrToColor(PropValue, False, NewColor) then
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
end;

procedure TProperties.AddPropertyByName(const PropName, PropValue: ThtString);
var
  Index: PropIndices;
begin
  if TryStrToPropIndex(PropName, Index) then
    AddPropertyByIndex(Index, PropValue);
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

constructor TStyleList.Create(const AUseQuirksMode: Boolean);
begin
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
  if Self.UseQuirksMode then begin
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
  if TryStrToPropIndex(Prop, PropIndex) then
  begin
    if not Find(Selector, I) then
    begin
      NewProp := True;
      Propty := TProperties.Create(FUseQuirksMode); {newly created property}
    end
    else
    begin
      Propty := TProperties(Objects[I]); {modify existing property}
      NewProp := False;
    end;
    case PropIndex of
      Color:
        if TryStrToColor(Value, False, NewColor) then
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
        if TryStrToColor(Value, False, NewColor) then
          Propty.Props[PropIndex] := NewColor;
      BackgroundColor:
        if TryStrToColor(Value, False, NewColor) then
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
  LinkUnderline: Boolean; ACodePage: TBuffCodePage; MarginHeight, MarginWidth: Integer);
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
  Properties.CodePage := ACodePage;
  AddObject('default', Properties);
  FDefProp := Properties;

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

{ TPropStack }

function TPropStack.GetProp(Index: Integer): TProperties;
begin
  Result := Get(Index); //TProperties(inherited Items[Index]);
end;

function TPropStack.Last: TProperties;
begin
  Result := Get(Count - 1);
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

initialization
finalization
//  FreeAndNil(ColorStrings);
  FreeAndNil(PropertyStrings);
end.
