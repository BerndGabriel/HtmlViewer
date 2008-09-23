{Version 9.45}
{*********************************************************}
{*                     STYLEUN.PAS                       *}
{*********************************************************}
{
Copyright (c) 1995-2008 by L. David Baldwin

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

Note that the source modules, HTMLGIF1.PAS, PNGZLIB1.PAS, DITHERUNIT.PAS, and
URLCON.PAS are covered by separate copyright notices located in those modules.
}

{$i htmlcons.inc}
                                  
unit StyleUn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  IntNull = -12345678;
  Auto = -12348765;
  AutoParagraph = -12348766;
  ParagraphSpace = 14;   {default spacing between paragraphs, etc.}
  {$ifdef Delphi6_Plus}
  varInt = [varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord];
  {$else}
  varInt = [varInteger];
  {$endif}
  EastEurope8859_2 = 31; {for 8859-2}
  CRLF = #$D#$A;

type
  AlignmentType = (ANone, ATop, AMiddle, ABaseline, ABottom, ALeft, ARight, AJustify, ASub, ASuper);
  BorderStyleType = (bssNone, bssSolid, bssInset, bssOutset, bssGroove, bssRidge,
                     bssDashed, bssDotted, bssDouble);
  ListBulletType = (lbBlank, lbCircle, lbDecimal, lbDisc, lbLowerAlpha, lbLowerRoman,
                 lbNone, lbSquare, lbUpperAlpha, lbUpperRoman);
  ClearAttrType = (clrNone, clLeft, clRight, clAll);
  PositionType = (posStatic, posAbsolute, posRelative);
  VisibilityType = (viInherit, viHidden, viVisible);
  TextTransformType = (txNone, txUpper, txLower, txCaps);   
  PositionRec = record
                  PosType: (pTop, pCenter, pBottom, pLeft, pRight, PPercent, pDim);
                  Value: integer;
                  RepeatD: boolean;
                  Fixed: boolean;    
                  end;
  PtPositionRec = array[1..2] of PositionRec;

{$ifdef Ver90}
  TFontCharSet = integer;  {dummy for Delphi 2}
{$endif}

  ThtFontInfo = class
    iName: string;
    iSize: double;  
    iStyle: TFontStyles;
    iColor: TColor;
    ibgColor: TColor;
    iCharSet: TFontCharSet;
    iCharExtra: Variant;    
    end;
    
  FIIndex = (LFont, VFont, HLFont, HVFont);
  TFontInfoArray = class
    Ar: array[LFont..HVFont] of ThtFontInfo;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TFontInfoArray);
    end;

  SetOfChar = Set of Char;

  TMyFont = class(TFont)
  public
    bgColor: TColor;
    tmHeight, tmDescent, tmExternalLeading, tmAveCharWidth,
       tmMaxCharWidth, tmCharset: integer;
    CharExtra: integer;    
    procedure Assign(Source: TPersistent); override;
    procedure AssignToCanvas(Canvas: TCanvas);    
    destructor Destroy; override;
    constructor Create;
  end;

  PropIndices = (
      FontFamily, FontSize, FontStyle, FontWeight, TextAlign, TextDecoration,
      LetterSpacing, BorderStyle, Color, BackgroundColor, BorderColor,     
      MarginTop, MarginRight, MarginBottom, MarginLeft,
      PaddingTop, PaddingRight, PaddingBottom, PaddingLeft,
      BorderTopWidth, BorderRightWidth, BorderBottomWidth, BorderLeftWidth,
      BorderTopColor, BorderRightColor, BorderBottomColor, BorderLeftColor,
      BorderTopStyle, BorderRightStyle, BorderBottomStyle, BorderLeftStyle,
      Width, Height, TopPos, BottomPos, RightPos, LeftPos, Visibility,   
      LineHeight, BackgroundImage, BackgroundPosition,
      BackgroundRepeat, BackgroundAttachment, VerticalAlign, Position, ZIndex,
      ListStyleType, ListStyleImage, Float, Clear, TextIndent,
      PageBreakBefore, PageBreakAfter, PageBreakInside, TextTransform,
      WordWrap, FontVariant, BorderCollapse, OverFlow, Display);

  TVMarginArray = array[BackgroundColor..LeftPos] of Variant;
  TMarginArray = array[BackgroundColor..LeftPos] of integer;

  TStyleList = class;

  TProperties = class(TObject)
  private
    TheFont: TMyFont;
    InLink: boolean;
    DefFontname: string;     
    procedure AddPropertyByIndex(Index: PropIndices; PropValue: string);
    procedure GetSingleFontInfo(var Font: ThtFontInfo);
    procedure CalcLinkFontInfo(Styles: TStyleList; I: integer);
    procedure CombineX(Styles: TStyleList;
          const Tag, AClass, AnID, PSeudo, ATitle: string; AProp: TProperties);
  public
    PropTag, PropClass, PropID, PropPseudo, PropTitle: string;
    PropStyle: TProperties;
    FontBG: TColor;
    CharSet: TFontCharSet;
    CodePage: integer;
    EmSize, ExSize: integer;   {# pixels for Em and Ex dimensions}
    Props: array[Low(PropIndices)..High(PropIndices)] of Variant;
    Originals: array[Low(PropIndices)..High(PropIndices)] of boolean;
    FIArray: TFontInfoArray;
    ID: integer;

    constructor Create;
    destructor Destroy; override;
    procedure Copy(Source: TProperties);
    procedure CopyDefault(Source: TProperties);
    procedure Inherit(Tag: string; Source: TProperties);  
    procedure Assign(const Item: Variant; Index: PropIndices);
    procedure AssignCharSet(CS: TFontCharset);
    procedure AssignUTF8;
    procedure Combine(Styles: TStyleList;
         const Tag, AClass, AnID, Pseudo, ATitle: string; AProp: TProperties);
    procedure Update(Source: TProperties; Styles: TStyleList; I: integer);
    function GetFont: TMyFont;
    procedure GetFontInfo(AFI: TFontInfoArray);
    procedure GetVMarginArray(var MArray: TVMarginArray);
    function GetBackgroundImage(var Image: string): boolean;
    procedure GetBackgroundPos(EmSize, ExSize: integer; var P: PtPositionRec);
    function GetLineHeight(NewHeight:integer): integer;
    function GetTextIndent(var PC: boolean): integer;
    function GetTextTransform: TextTransformType;
    function GetFontVariant: string;   
    procedure GetPageBreaks(var Before, After, Intact: boolean);
    function GetVertAlign(var Align: AlignmentType): boolean;
    function GetFloat(var Align: AlignmentType): boolean;
    function GetClear(var Clr: ClearAttrType): boolean;
    function GetOriginalForegroundColor: TColor;
    function GetBackgroundColor: TColor;
    function GetBorderStyle: BorderStyleType;
    function BorderStyleNotBlank: boolean;
    function GetListStyleType: ListBulletType;
    function GetListStyleImage: string;
    function GetPosition: PositionType;
    function GetVisibility: VisibilityType;
    function GetZIndex: integer;
    function DisplayNone: boolean;
    function Collapse: boolean;  
    procedure SetFontBG;
    procedure AddPropertyByName(const PropName, PropValue: string);
    function IsOverflowHidden: boolean;      
    end;

  TStyleList = Class(TStringList)
  private
    MasterList: TObject;
    SeqNo: integer;
  public
    DefProp: TProperties;
    constructor Create(AMasterList: TObject);
    destructor Destroy; override;
    procedure Clear; override;
    function GetSeqNo: string;
    procedure Initialize(const FontName, PreFontName: string;
         PointSize: integer; AColor, AHotspot, AVisitedColor, AActiveColor: TColor;
         LinkUnderline: boolean; ACharSet: TFontCharSet; MarginHeight, MarginWidth: integer);
    procedure AddModifyProp(const Selector, Prop, Value: string);
    function AddObject(const S: string; AObject: TObject): Integer; override;
    function AddDuplicate(const Tag: string; Prop: TProperties): TProperties;
    procedure ModifyLinkColor(Psuedo: string; AColor: TColor);
    {$ifdef Quirk}
    procedure FixupTableColor(BodyProp: TProperties);  
    {$endif}
    end;

const                             
  PropWords: array[Low(PropIndices)..High(PropIndices)] of string =
    ('font-family', 'font-size', 'font-style', 'font-weight', 'text-align',
     'text-decoration', 'letter-spacing', 'border-style', 'color', 'background-color',
     'border-color',
     'margin-top', 'margin-right', 'margin-bottom', 'margin-left',
     'padding-top', 'padding-right','padding-bottom', 'padding-left',
     'border-top-width', 'border-right-width','border-bottom-width', 'border-left-width',
     'border-top-color', 'border-right-color','border-bottom-color', 'border-left-color',
     'border-top-style', 'border-right-style','border-bottom-style', 'border-left-style',
     'width', 'height', 'top', 'bottom', 'right', 'left', 'visibility',     
     'line-height', 'background-image', 'background-position',
     'background-repeat', 'background-attachment', 'vertical-align', 'position', 'z-index',
     'list-style-type', 'list-style-image', 'float', 'clear', 'text-indent',
     'page-break-before', 'page-break-after', 'page-break-inside', 'text-transform',
     'word-wrap', 'font-variant', 'border-collapse', 'overflow', 'display');   

procedure ConvMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize,
     ExSize: integer; BStyle: BorderStyleType; var AutoCount: integer;
     var M: TMarginArray);

procedure ConvVertMargins(const VM: TVMarginArray;
     BaseHeight, EmSize, ExSize: Integer;
     var M: TMarginArray; var TopAuto, BottomAuto: boolean);

procedure ConvMargArrayForCellPadding(const VM: TVMarginArray; EmSize,
     ExSize: Integer; var M: TMarginArray);

procedure ConvInlineMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize,
     ExSize: Integer; {BStyle: BorderStyleType;} var M: TMarginArray);

function ColorFromString(S: string; NeedPound: boolean; var Color: TColor): boolean;

function ReadURL(Item: Variant): string;

function ReadFontName(S: string): string; 

function AlignmentFromString(S: string): AlignmentType;   

{$ifndef Ver130}
{$ifndef Delphi6_Plus}
procedure FreeAndNil(var Obj);
{$endif}
{$endif}

implementation

uses
  {$ifdef Delphi6_Plus}
  Variants,
  {$endif}
  htmlsubs, htmlun2, readhtml;

var
  DefPointSize: double;

{$ifndef Ver130}
{$ifndef Delphi6_Plus}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  {clear the reference before destroying the object}
  P.Free;
end;
{$endif}
{$endif}

{----------------AlignmentFromString}
function AlignmentFromString(S: string): AlignmentType;   
begin
S := LowerCase(S);
if S = 'top' then Result := ATop
else if (S = 'middle') or (S = 'absmiddle') or (S = 'center') then Result := AMiddle
else if S = 'left' then Result := ALeft
else if S = 'right' then Result := ARight
else if (S = 'bottom') then Result := ABottom
else if (S = 'baseline') then Result := ABaseline
else if (S = 'justify') then Result := AJustify
else Result := ANone;
end;

function FontSizeConv(const Str: string; OldSize: double): double; forward;
function LengthConv(const Str: string; Relative: boolean; Base, EmSize, ExSize,
                          Default: integer): integer; forward;

function FindPropIndex(const PropWord:string; var PropIndex: PropIndices): boolean;
var
  I: PropIndices;
begin
Result := True;
for I := Low(PropIndices) to High(PropIndices)do
  if PropWord = PropWords[I] then
    begin
    PropIndex := I;
    Exit;
    end;
Result := False;
end;

{----------------ReadURL}
function ReadURL(Item: Variant): string;
var
  I: integer;
  S: string;
begin
Result := '';
if VarType(Item) = VarString then
  begin
  S := Item;
  I := Pos('url(', S);
  if I > 0 then
    begin
    S := System.Copy(S, 5, Length(S));
    I := Pos(')', S);
    if I > 0 then
      S := System.Copy(S, 1, I-1);
    if Length(S) > 2 then
      if S[1] in ['''', '"'] then
        begin
        Delete(S, Length(S), 1);
        Delete(S, 1, 1);
        end;
    Result := S;
    end;
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

constructor TmyFont.Create;
begin
inherited;
end;

var
  Sequence: integer;  

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
AssignCharSet(Source.CharSet);
DefFontname := Source.DefFontname;    
PropTag := 'default';
end;

procedure TProperties.Inherit(Tag: string; Source: TProperties);     
{copy the properties that are inheritable}
var
  I: PropIndices;
  Span, HBF: boolean;
begin
Span := Source.PropTag = 'span';
HBF := (Source.PropTag = 'thead') or (Source.PropTag = 'tbody')	  
       or (Source.PropTag = 'tfoot');
for I := Low(I) to High(I) do
  if Span and (I <> BorderStyle) then   {Borderstyle already acted on}
    Props[I] := Source.Props[I]
  else if HBF then
    begin
    Props[I] := Source.Props[I];   {tr gets them all}
    Originals[I] := Source.Originals[I];
    end
  else if (I = WordWrap) and (Tag = 'table') then  {table doesn't inherit word wrap} 
    Props[WordWrap] := 'normal'
  else
    case I of
      MarginTop..LeftPos:
        Props[I] := IntNull;
      BackgroundColor, BorderColor, BorderStyle,  
      Clear, Float, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundAttachment,
      Position, PageBreakBefore, PageBreakAfter, PageBreakInside, BorderCollapse,
      OverFlow, Display:
          ;  {do nothing}
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

EmSize := Source.EmSize;    {actually this is calculated later }
ExSize := EmSize div 2; {apparently correlates with what browsers are doing}
end;

{----------------TProperties.Update}
procedure TProperties.Update(Source: TProperties; Styles: TStyleList; I: integer);
{Change the inherited properties for this item to those of Source}
var
  Index: PropIndices;
begin
for Index := Low(Index) to High(Index) do
  if not Originals[Index] then
    Props[Index] := Source.Props[Index];
TheFont.Free;   {may no longer be good}  
TheFont := Nil;
if Assigned(FIArray) then
  if Source.Inlink then
    FIArray.Assign(Source.FIArray)
  else if PropPseudo = 'link' then   {an <a href> tag}
    CalcLinkFontInfo(Styles, I)
  else
    begin  {an <a href> tag has been removed}
    FIArray.Free;
    FIArray := Nil;
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

function TProperties.GetBackgroundImage(var Image: string): boolean;
begin
if (VarType(Props[BackgroundImage]) = VarString) then
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
else Result := False;
end;

procedure TProperties.AssignCharSet(CS: TFontCharset);
const
  {EastEurope8859_2 = 31; }
  SetValues: array[1..20] of integer =
    (ANSI_CHARSET, DEFAULT_CHARSET, SYMBOL_CHARSET, MAC_CHARSET, SHIFTJIS_CHARSET,
     HANGEUL_CHARSET, JOHAB_CHARSET, GB2312_CHARSET, CHINESEBIG5_CHARSET,
     GREEK_CHARSET, TURKISH_CHARSET, VIETNAMESE_CHARSET, HEBREW_CHARSET,
     ARABIC_CHARSET, BALTIC_CHARSET, RUSSIAN_CHARSET, THAI_CHARSET,
     EASTEUROPE_CHARSET, OEM_CHARSET, EastEurope8859_2);
 { SetValues: array[1..19] of integer =
    (0, 1, 2, 77, 128, 129, 130, 134, 136, 161, 162, 163, 177, 178,
     186, 204, 222, 238, 255); }
  CodePages: array[1..20] of integer =
    (1252, CP_ACP, 0, CP_MACCP, 932, 949, 1361, 936, 950, 1253, 1254, 1258, 1255, 1256, 1257, 1251,
     874, 1250, CP_OEMCP, 28592);    {28592 for 8859-2, east european}
var
  I: integer;
  Save: THandle;
  tm : TTextmetric;
  DC: HDC;
  Font: TFont;
  IX: FIIndex;
begin
if CS = EastEurope8859_2 then
  begin
  CharSet := EASTEUROPE_CHARSET;
  CodePage := 28592;
  if Assigned(FIArray) then
    for IX := LFont to HVFont do
      FIArray.Ar[IX].iCharset := CharSet;
  Exit;
  end;
{the following makes sure the CharSet is available.  It also translates
 "Default_CharSet" into the actual local character set}
Font := TFont.Create;
Font.Name := '';   
Font.CharSet := CS;
DC := GetDC(0);
try
  Save := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, tm);
  if CS <> Default_Charset then    {leave default as is}
    CharSet := tm.tmCharSet
  else CharSet := Default_CharSet;
  if Assigned(FIArray) then
    for IX := LFont to HVFont do
      FIArray.Ar[IX].iCharset := CharSet;
  SelectObject(DC, Save);
Finally
  ReleaseDC(0, DC);
  Font.Free;
  end;

for I := 1 to 19 do
  if SetValues[I] = tm.tmCharSet then    
    begin
    CodePage := CodePages[I];
    break;
    end;
end;

procedure TProperties.AssignUTF8;
{Called by DoMeta in Readhtml.pas to make the properties using UTF-8 for conversions.}
begin
CodePage := CP_UTF8;
Charset := ANSI_CHARSET;
end;

{----------------TProperties.GetBackgroundPos}
procedure TProperties.GetBackgroundPos(EmSize, ExSize: integer; var P: PtPositionRec);
var
  S: array[1..2] of string;
  Tmp: string;
  I, N, XY: integer;
begin
if (VarType(Props[BackgroundPosition]) <> VarString) then
  begin
  P[1].PosType := pDim;
  P[1].Value := 0;
  P[2] := P[1];
  end
else
  begin
  Tmp := Trim(Props[BackgroundPosition]);
  N := Pos(' ', Tmp);
  if N > 0 then
    begin
    S[1] := System.Copy(Tmp, 1, N-1);
    S[2] := Trim(system.Copy(Tmp, N+1, 255));
    N := 2;
    end
  else
    begin
    S[1] := Tmp;
    N := 1;
    end;
  I := 1;
  XY := 1;  {X}
  while I <= N do
    begin
    P[XY].PosType := pDim;
    if S[I] = 'center' then
      P[XY].PosType := pCenter
    else if Pos('%', S[I]) > 0 then
      P[XY].PosType := pPercent
    else if S[I] = 'left' then
      begin
      if XY = 2 then  {entered in reverse direction}
        P[2] := P[1];
      P[1].PosType := pLeft;
      end
    else if S[I] = 'right' then
      begin
      if XY = 2 then
        P[2] := P[1];
      P[1].PosType := pRight;
      end
    else if S[I] = 'top' then
      begin
      P[2].PosType := pTop;
      if XY = 1 then
        Dec(XY);   {read next one into X}
      end
    else if S[I] = 'bottom' then
      begin
      P[2].PosType := pBottom;
      if XY = 1 then
        Dec(XY);
      end;
    if P[XY].PosType in [pDim, pPercent] then
      begin
      P[XY].Value := LengthConv(S[I], False, 100, EmSize, ExSize, 0);
      end;
    Inc(I);
    Inc(XY);
    end;
  if N = 1 then
    if XY = 2 then
      P[2].PosType := pCenter
    else P[1].PosType := pCenter; {single entry but it was a Y}
  end;
P[1].RepeatD := True;
P[2].RepeatD := True;
if (VarType(Props[BackgroundRepeat]) = VarString) then
  begin
  Tmp := Trim(Props[BackgroundRepeat]);
  if Tmp = 'no-repeat' then
    begin
    P[1].RepeatD := False;
    P[2].RepeatD := False;
    end
  else if Tmp = 'repeat-x' then
    P[2].RepeatD := False
  else if Tmp = 'repeat-y' then
    P[1].RepeatD := False;
  end;
P[1].Fixed := False;
if (VarType(Props[BackgroundAttachment]) = VarString) and
       (Trim(Props[BackgroundAttachment]) = 'fixed') then
  P[1].Fixed := True;
P[2].Fixed := P[1].Fixed;
end;

function TProperties.GetVertAlign(var Align: AlignmentType): boolean;
{note:  'top' should have a catagory of its own}
var
  S: string;
begin
if (VarType(Props[VerticalAlign]) = VarString) then
  begin
  Result := True;
  S := Props[VerticalAlign];
  if (S = 'top') or (S = 'text-top') then Align := ATop
  else if S = 'middle' then Align := AMiddle
  else if S = 'baseline' then Align := ABaseline
  else if (S = 'bottom') then Align := ABottom
  else if (S = 'sub') then Align := ASub
  else if (S = 'super') then Align := ASuper
  else Result := False;
  end
else Result := False;
end;

function TProperties.IsOverflowHidden: boolean;   
begin
Result := (VarType(Props[OverFlow]) = VarString) and (Props[OverFlow] = 'hidden');
end;

function TProperties.GetFloat(var Align: AlignmentType): boolean;
var
  S: string;
begin
if (VarType(Props[Float]) = VarString) then
  begin
  Result := True;
  S := Props[Float];
  if (S = 'left') then Align := ALeft
  else if S = 'right' then Align := ARight
  else if S = 'none' then Align := ANone
  else Result := False;
  end
else Result := False;
end;

function TProperties.GetClear(var Clr: ClearAttrType): boolean;
var
  S: string;
begin
if (VarType(Props[Clear]) = VarString) then
  begin
  Result := True;
  S := Props[Clear];
  if (S = 'left') then Clr := clLeft
  else if S = 'right' then Clr := clRight
  else if S = 'both' then Clr := clAll
  else if S = 'none' then Clr := clrNone
  else Result := False;
  Props[Clear] := Unassigned;   {allow only one read}
  end
else Result := False;
end;

function TProperties.GetListStyleType: ListBulletType;
const
  S: array[Low(ListBulletType)..High(ListBulletType)] of string =
    ('blank', 'circle', 'decimal', 'disc', 'lower-alpha', 'lower-roman',
                 'none', 'square', 'upper-alpha', 'upper-roman');
var
  I: ListBulletType;

begin
if VarType(Props[ListStyleType]) = VarString then
  for I := Low(ListBulletType) to High(ListBulletType) do
    if S[I] = Props[ListStyleType] then
      begin
      Result := I;
      Exit;
      end;
Result := lbBlank;
end;

function TProperties.GetListStyleImage: string;
begin
Result := ReadURL(Props[ListStyleImage])
end;

function TProperties.GetPosition: PositionType;
begin
Result := posStatic;
if VarType(Props[Position]) = VarString then
  begin
  if Props[Position] = 'absolute' then
    Result := posAbsolute
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

function TProperties.GetZIndex: integer;
begin
Result := 0;
if VarType(Props[ZIndex]) in VarInt then    
  Result := Props[ZIndex]
else if VarType(Props[ZIndex]) = VarString then
  Result := StrToIntDef(Props[ZIndex], 0);
end;

function TProperties.DisplayNone: boolean;
begin
Result := (VarType(Props[Display]) = VarString) and (Props[Display] = 'none');
end;

function TProperties.Collapse: boolean;  
begin
Result := (VarType(Props[BorderCollapse]) = VarString) and (Props[BorderCollapse] = 'collapse');
end;

function TProperties.GetLineHeight(NewHeight:integer): integer;
var
  V: double;
  Code: integer;
begin
if VarType(Props[LineHeight]) = varString then
  begin
  Val(Props[LineHeight], V, Code);
  if Code = 0 then   {a numerical entry with no 'em', '%', etc.  Use the new font height}
    Result := Round(V*NewHeight)
  else
    {note: 'normal' yields -1 in the next statement}
    Result := LengthConv(Props[LineHeight], True, EmSize, EmSize, ExSize, -1);   
  end
else Result := -1;     
end;

function TProperties.GetTextIndent(var PC: boolean): integer;
var
  I: integer;
begin
PC := False;
if VarType(Props[TextIndent]) = varString then
  begin
  I := Pos('%', Props[TextIndent]);
  if I > 0 then
    begin
    PC := True;  {return value in percent}
    Result := LengthConv(Props[TextIndent], True, 100, 0, 0, 0);
    end
  else
    Result := LengthConv(Props[TextIndent], False, 0, EmSize, EmSize, 0);
  end
else Result := 0;
end;

function TProperties.GetTextTransform: TextTransformType;
begin
try
  if VarType(Props[TextTransform]) in VarInt then
    Result := Props[TextTransform]
  else Result := txNone;
except
  Result := txNone;
  end;
end;

function TProperties.GetFontVariant: string;    
begin
try
  if VarType(Props[FontVariant]) = varString then
    Result := Props[FontVariant]
  else Result := 'normal';
except
  Result := 'normal';
  end;
end;

procedure TProperties.GetPageBreaks(var Before, After, Intact: boolean);
begin
Before := (VarType(Props[PageBreakBefore]) = varString) and  (Props[PageBreakBefore] = 'always');
After := (VarType(Props[PageBreakAfter]) = varString) and  (Props[PageBreakAfter] = 'always');
Intact := (VarType(Props[PageBreakInside]) = varString) and  (Props[PageBreakInside] = 'avoid');
end;

function TProperties.GetBackgroundColor: TColor;
begin
if (VarType(Props[BackgroundColor]) in varInt) and Originals[BackgroundColor] then
      {Originals to prevent fonts from getting inherited background color}
  Result := Props[BackgroundColor]
else Result := clNone;
end;

function TProperties.GetOriginalForegroundColor: TColor;
begin {return a color only if it hasn't been inherited}
if (VarType(Props[Color]) in varInt) and Originals[Color] then
  Result := Props[Color]
else Result := clNone;
end;

function BorderStyleFromString(const S: string): BorderStyleType;
const
  Ar: array[1..9] of string = ('none', 'solid', 'inset', 'outset','groove', 'ridge',
                              'dashed', 'dotted', 'double');
  Ar1: array[1..9] of BorderStyleType = (bssNone, bssSolid, bssInset, bssOutset, bssGroove, bssRidge,
                     bssDashed, bssDotted, bssDouble);
var
  I: integer;
begin
Result := bssNone;
for I := 1 to 9 do
  if S = Ar[I] then
    begin
    Result := Ar1[I];
    break;
    end;
end;

function TProperties.GetBorderStyle: BorderStyleType;
begin
Result := bssNone;
if VarType(Props[BorderStyle]) = VarString then
  Result := BorderStyleFromString(Props[BorderStyle]);
end;

function TProperties.BorderStyleNotBlank: boolean;
{was a border of some type (including bssNone) requested?}
begin
Result := VarType(Props[BorderStyle]) = VarString;
end;

procedure TProperties.SetFontBG;
{called for font tags like <b>, <small>, etc.  Sets the font background color.}
begin
if (VarType(Props[BackgroundColor]) in varInt) and Originals[BackgroundColor] then
  FontBG := Props[BackgroundColor];
end;

procedure ConvVertMargins(const VM: TVMarginArray;
     BaseHeight, EmSize, ExSize: Integer;
     var M: TMarginArray; var TopAuto, BottomAuto: boolean);

  function Convert(V: Variant; var IsAutoParagraph: boolean): integer;
  begin
  IsAutoParagraph := False;
  if VarType(V) = VarString then
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
    else Result := V;
    end
  else Result := 0;
  end;

begin
M[MarginTop] := Convert(VM[MarginTop], TopAuto);
M[MarginBottom] := Convert(VM[MarginBottom], BottomAuto);
end;

{----------------ConvMargArray}
procedure ConvMargArray(const VM: TVMarginArray; BaseWidth, BaseHeight, EmSize,
     ExSize: Integer; BStyle: BorderStyleType; var AutoCount: integer;
     var M: TMarginArray);
{This routine does not do MarginTop and MarginBottom as they are done by ConvVertMargins}
var
  I: PropIndices;
  Base: integer;
begin
AutoCount := 0;  {count of 'auto's in width items}
for I := Low(VM) to High(VM) do
  begin
  case I of
    Height, TopPos:         
      Base := BaseHeight
    else Base := BaseWidth;
    end;
  case I of
    BackgroundColor, BorderColor:
      begin
      if VarType(VM[I]) <= VarNull then
        M[I] := clNone
      else M[I] := VM[I];
      end;
    BorderTopWidth..BorderLeftWidth:
      begin
      if VM[PropIndices(Ord(BorderTopStyle) + (Ord(I)-Ord(BorderTopWidth)))] = bssNone then
        M[I] := 0
      else
        begin
        if VarType(VM[I]) = VarString then
          begin
          if VM[I] = 'thin' then
            M[I] := 2
          else if  VM[I] = 'medium' then
            M[I] := 4
          else if  VM[I] = 'thick' then
            M[I] := 6
          else
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 4); {Auto will be 4}
          end
        else if (VarType(VM[I]) in varInt) then
          begin
          if (VM[I] = IntNull) then
              M[I] := 4
          else M[I] := VM[I];
          end;
        end;
      end;
    Height, PaddingTop..PaddingLeft:
      begin
      if VarType(VM[I]) = VarString then
        begin
        M[I] := LengthConv(VM[I], False, Base, EmSize, ExSize, 0); {Auto will be 0}
        if (I = Height) and (Pos('%', VM[I]) > 0) then        {include border in % heights}
          M[I] := M[I] - M[BorderTopWidth] - M[BorderBottomWidth] - M[PaddingTop] - M[PaddingBottom]; 
        end
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := 0
        else M[I] := VM[I];
        end
      else M[I] := 0;
      end;
    TopPos, RightPos, BottomPos, LeftPos:
      begin
      if VarType(VM[I]) = VarString then
        M[I] := LengthConv(VM[I], False, Base, EmSize, ExSize, Auto) {Auto will be Auto}
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := Auto
        else M[I] := VM[I];
        end
      else M[I] := Auto;
      end;
    MarginLeft, MarginRight:
      begin
      if VarType(VM[I]) = VarString then
        begin
        if VM[I] = 'auto' then
          begin
          M[I] := Auto;
          Inc(AutoCount);
          end
        else M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0);
        end
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := 0
        else M[I] := VM[I];
        end
      else M[I] := 0;
      end;
    Width:
      begin
      if VarType(VM[I]) = VarString then
        M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto)
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := Auto
        else M[I] := VM[I];
        end
      else M[I] := Auto;
      if M[I] = Auto then
        Inc(AutoCount);
      end;
    MarginTop, MarginBottom:  ;   {do nothing}
    else
      begin
      if VarType(VM[I]) = VarString then
        M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0)
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := 0
        else M[I] := VM[I];
        end
      else M[I] := 0;
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
  if VarType(VM[I]) = VarString then
    M[I] := LengthConv(VM[I], False, -100, EmSize, ExSize, 0) {Auto will be 0}
  else if VarType(VM[I]) in varInt then
    begin
    if VM[I] = IntNull then
      M[I] := -1
    else M[I] := VM[I];
    end
  else M[I] := -1;
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
    Height, Width:
      begin
      if VarType(VM[I]) = VarString then
        M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, Auto) {Auto will be Auto}
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := IntNull
        else M[I] := VM[I];
        end
      else M[I] := IntNull;
      end;
    MarginLeft, MarginRight, MarginTop, MarginBottom:
      begin
      if VarType(VM[I]) = VarString then
        M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 0)  {auto is 0}
      else if VarType(VM[I]) in varInt then
        begin
        if VM[I] = IntNull then
          M[I] := IntNull
        else M[I] := VM[I];
        end
      else M[I] := IntNull;
      end;
    BorderTopWidth..BorderLeftWidth:    
      begin
      if VM[PropIndices(Ord(BorderTopStyle) + (Ord(I)-Ord(BorderTopWidth)))] = bssNone then
        M[I] := 0
      else
        begin
        if VarType(VM[I]) = VarString then
          begin
          if VM[I] = 'thin' then
            M[I] := 2
          else if  VM[I] = 'medium' then
            M[I] := 4
          else if  VM[I] = 'thick' then
            M[I] := 6
          else
            M[I] := LengthConv(VM[I], False, BaseWidth, EmSize, ExSize, 4); {Auto will be 4}
          end
        else if (VarType(VM[I]) in varInt) then
          begin
          if (VM[I] = IntNull) then
              M[I] := 4
          else M[I] := VM[I];
          end;
        end;
      end;
    else
      ;   {remaining items unsupported/unused}
    end;
end;

{----------------TProperties.Combine}
procedure TProperties.Combine(Styles: TStyleList;
        const Tag, AClass, AnID, PSeudo, ATitle: string; AProp: TProperties);  
{When called, this TProperties contains the inherited properties.  Here we
 add the ones relevant to this item. AProp are TProperties gleaned from the
 Style= attribute. AClass may be a multiple class like class="ab.cd"}
 var
   BClass, S: string;
   I: integer;
 begin
 BClass := Trim(AClass);         
 I := Pos('.', BClass);
 if I <= 0 then
   CombineX(Styles, Tag, BClass, AnID, PSeudo, '', AProp)  {0 or 1 Class}
 else
   begin  {more than one class}
   repeat
     S := System.Copy(BClass, 1, I-1);
     CombineX(Styles, Tag, S, AnID, PSeudo, '', Nil);
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
  CalcLinkFontInfo(Styles, PropStack.Count-1);
  InLink := True;
  end;
 end;

 {----------------TProperties.CombineX}
procedure TProperties.CombineX(Styles: TStyleList;
        const Tag, AClass, AnID, PSeudo, ATitle: string; AProp: TProperties);
{When called, this TProperties contains the inherited properties.  Here we
 add the ones relevant to this item. AProp are TProperties gleaned from the
 Style= attribute.}
var
  OldSize: double;
  IX: integer;
  NoHoverVisited: boolean;

  procedure Merge(Source: TProperties);
  var
    Index: PropIndices;
    I: FIIndex;
    Wt: integer;
    S1: string;
  begin
  for Index := Low(Index) to High(PropIndices) do
      if (VarType(Source.Props[Index]) <> varEmpty) and (Vartype(Source.Props[Index]) <> varNull) then
        case Index of
          MarginTop..LeftPos:
            if VarType(Source.Props[Index]) = VarString then
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
                      iSize := FontSizeConv(Props[FontSize], iSize);
                    Color: iColor := Props[Color];
                    BackgroundColor: ibgColor := Props[BackgroundColor];
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
          Originals[Index] := True;   {it's defined for this item, not inherited}
          end;
        end;
  end;

  function CheckForContextual(I: integer): boolean;
  {process contextual selectors}
  var
    J, K, N: integer;
    A: array[1..10] of record
         Tg, Cl, ID, PS: string;
         gt: boolean;     
         end;
    MustMatchParent: Boolean;     

    procedure Split(S: string);
    var
      I, J: integer;
    begin
    N := 1;   {N is number of selectors in contextual string}
    I := Pos(' ', S);
    while (I > 0) and (N < 10) do
      begin
      A[N].Tg := System.Copy(S, 1, I-1);
      Delete(S, 1, I);
      S := Trim(S);
      Inc(N);
      I := Pos(' ', S);
      end;
    A[N].Tg := S;
    if (N >= 2) and (Length(A[2].Tg) > 0) then
      repeat
        Delete(A[2].Tg, 1, 1);   {remove the sort digit}
      until (length(A[2].Tg)=0) or not (A[2].Tg[1] in ['0'..'9']);  
    for I := 1 to N do
      begin
      J := Pos('>', A[I].Tg);   
      if I > 1 then
        A[I-1].gt := J > 0;
      if J > 0 then
        Delete(A[I].Tg, J, 1); 
      J := Pos(':', A[I].Tg);
      if J > 0 then
        begin
        A[I].PS := System.Copy(A[I].Tg, J+1, Length(A[I].Tg));
        A[I].Tg := System.Copy(A[I].Tg, 1, J-1);
        end
      else A[I].PS := '';
      J := Pos('#', A[I].Tg);
      if J > 0 then
        begin
        A[I].ID := System.Copy(A[I].Tg, J+1, Length(A[I].Tg));
        A[I].Tg := System.Copy(A[I].Tg, 1, J-1);
        end
      else A[I].ID := '';
      J := Pos('.', A[I].Tg);
      if J > 0 then
        begin
        A[I].Cl := System.Copy(A[I].Tg, J+1, Length(A[I].Tg));
        A[I].Tg := System.Copy(A[I].Tg, 1, J-1);
        end
      else A[I].Cl := '';
      end;
    end;

    function PartOf(const S1, S2: string): boolean;    
    {see if all classes in S1 are present in S2.  Classes are separated by '.'}
    var
      SL1, SL2: TStringList;
      J, X: integer;

      function FormStringList(S: string): TStringList;
      {construct a TStringList from classes in string S}
      var
        I: integer;
      begin
      Result := TStringList.Create;
      Result.Sorted := True;
      I := Pos('.', S);
      while I >= 1 do
        begin
        Result.Add(System.Copy(S, 1, I-1));
        Delete(S, 1, I);
        I := Pos('.', S);
        end;
      Result.Add(S);
      end;

    begin  {PartOf}
    SL1 := FormStringList(S1);
    try
      SL2 := FormStringList(S2);
      try
        Result := True;   {assume all will be found}
        for J := 0 to SL1.Count-1 do
          If not SL2.Find(SL1[J], X) then
            begin
            Result := False;  {one is missing, return False}
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
  else Result := True;
  if (N > 1)   //it's a contextual selector.  N is count of selectors
        and ((A[1].Tg = Tag) or (A[1].Tg = ''))
        and ((A[1].Cl = AClass) or (A[1].Cl = ''))
        and ((A[1].ID = AnID) or (A[1].ID = ''))
        and ((A[1].PS = PSeudo) or (A[1].PS = '') and (PSeudo = 'link')) then 
    begin //look thru the stack to see if this contextual selector is appropriate
    K := 2;   //K is selector index in the sequence
    J := PropStack.Count-2;  // start on stack item below this one
    MustMatchParent := A[1].gt;   
    while (K <= N) and (J >= 1) do
      begin
      with PropStack[J] do
        if ((A[K].Tg = PropTag) or (A[K].Tg = ''))
              and ((A[K].Cl = PropClass) or (A[K].Cl = '') or PartOf(A[K].Cl, PropClass)) 
              and ((A[K].ID = PropID) or (A[K].ID = ''))
              and ((A[K].PS = PropPseudo) or (A[K].PS = '')) then
          begin
          if K = N then  //all parts of contextual selector match
            Merge(Styles.Objects[I] as TProperties);
          MustMatchParent := A[K].gt;  
          Inc(K);
          end
        else if MustMatchParent then
          Break;  {Didn't match}       
      Dec(J);
      end;
    end
  end;

  procedure MergeItems(Const Item: string);
  {look up items in the Style list.  If found, merge them in this TProperties.
   Items may be duplicated in which case the last has priority.  Items may be
   simple tags like 'p', 'blockquote', 'em', etc or they may be more complex
   like  p.class, em#id, a.class:link, etc}
  var
    X: integer;
  begin
  if Styles.Find(Item, X) then
    begin
    Merge(Styles.Objects[X] as TProperties);
    Inc(X);
    while (X < Styles.Count) and (Styles[X] = Item) do
      begin  //duplicates, last one has highest priority
      Merge(Styles.Objects[X] as TProperties);
      Inc(X);
      end;
    end;
  end;

begin
{$ifdef Quirk}
if (Tag = 'td') or (Tag = 'th') then   
  OldSize := DefPointSize else
{$endif}
if (VarType(Props[FontSize]) = VarDouble) and (Props[FontSize] > 0.0) then    {should be true}
  OldSize := Props[FontSize]
else OldSize := DefPointSize;

{Some hover and visited items adequately taken care of when link processed}
NoHoverVisited := (Pseudo = '') or ((Pseudo <> 'hover') and (Pseudo <> 'visited'));

// in the following, lowest priority on top, highest towards bottom.

if (Tag = 'a') and (Pseudo <> '') then
  MergeItems('::'+Pseudo); {default Pseudo definition}

if NoHoverVisited then
  MergeItems(Tag);

if Pseudo <> '' then
  MergeItems(':'+Pseudo);

if (AClass <> '') and NoHoverVisited then
  MergeItems('.'+AClass);

if (AClass <> '') and NoHoverVisited then
  MergeItems(Tag+'.'+AClass);

if Pseudo <> '' then
  MergeItems(Tag+':'+Pseudo);

if (AClass <> '') and (PSeudo <> '') then
    MergeItems('.'+AClass+':'+Pseudo);

if (AClass <> '') and (Pseudo <> '') then
  MergeItems(Tag+'.'+AClass+':'+Pseudo);

if AnID <> '' then
  begin
  MergeItems('#'+AnID);
  MergeItems(Tag+'#'+AnID);
  if (AClass <> '') then
    MergeItems('.'+AClass+'#'+AnID);
  if (Pseudo <> '') then
    begin
    MergeItems('#'+AnID+':'+Pseudo);
    MergeItems(Tag+'#'+AnID+':'+Pseudo);
    end;
  if (AClass <> '') then
    MergeItems(Tag+'.'+AClass+'#'+AnID);
  MergeItems('.'+AClass+'#'+AnID+':'+Pseudo);
  MergeItems(Tag+'.'+AClass+'#'+AnID+':'+Pseudo);
  end;

{process the entries in Styles to see if they are contextual selectors}
Styles.Find(Tag, IX);    //place to start
while (IX < Styles.Count) and (Pos(Tag, Styles[IX]) = 1) and CheckForContextual(IX) do
  Inc(IX);

Styles.Find('.'+AClass, IX);    //place to start
while (IX < Styles.Count) and (Pos('.'+AClass, Styles[IX]) = 1) and CheckForContextual(IX) do
  Inc(IX);

Styles.Find(':'+PSeudo, IX);    //place to start           
while (IX < Styles.Count) and (Pos(':'+PSeudo, Styles[IX]) = 1) and CheckForContextual(IX) do
  Inc(IX);

If Assigned(AProp) then   //the Style= attribute
  Merge(AProp);

if not ((VarType(Props[FontSize]) = VarDouble) or
      (VarType(Props[FontSize]) in varInt)) then  {if still a string, hasn't been converted}
  Props[FontSize] := FontSizeConv(Props[FontSize], OldSize);
end;

function TProperties.GetFont: TMyFont;
var
  Font: ThtFontInfo;
  Save: THandle;
  SaveCharSet: TFontCharSet;
  tm : TTextmetric;
  DC: HDC;
  V: Variant;
begin      {call only if all things valid}
if not Assigned(TheFont) then
  begin
  Font := ThtFontInfo.Create;
  try
    GetSingleFontInfo(Font);
    TheFont := TMyFont.Create;
    with TheFont, Font do
      begin
      Name := iName; 
      Height := -Round(iSize * Screen.PixelsPerInch / 72);   
      Style := iStyle;
      bgColor := ibgColor;
      Color := iColor;
      Charset := iCharSet;
      V := iCharExtra;
      end;
  finally
    Font.Free;
    end;
  {if this is a Symbol charset, then keep it that way.  To check the font's real
   charset, use Default_Charset} 
  SaveCharSet := TheFont.CharSet;
  TheFont.CharSet := Default_Charset;
  DC := GetDC(0);
  try
    Save := SelectObject(DC, TheFont.Handle);
    try
      GetTextMetrics(DC, tm);
    finally
      SelectObject(DC, Save);
      end;
    if tm.tmCharset = Symbol_Charset then
      TheFont.Charset := Symbol_CharSet
    else
      TheFont.Charset := SaveCharSet;
    {now get the info on the finalized font}
    if TheFont.Charset <> Default_Charset then  {else already have the textmetrics}
      begin
      Save := SelectObject(DC, TheFont.Handle);
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
  EmSize := tm.tmHeight-tm.tmInternalLeading;
  ExSize := EmSize div 2; {apparently correlates with what browsers are doing}
  TheFont.tmHeight := tm.tmHeight;  
  TheFont.tmDescent := tm.tmDescent;
  TheFont.tmExternalLeading := tm.tmExternalLeading;
  TheFont.tmMaxCharWidth := tm.tmMaxCharWidth;
  TheFont.tmAveCharWidth := tm.tmAveCharWidth;
  TheFont.tmCharset := tm.tmCharset;
  if VarType(V) in VarInt then
    TheFont.CharExtra := V
  else if VarType(V) = VarString then
    if V = 'normal' then
      TheFont.CharExtra := 0
    else TheFont.CharExtra := LengthConv(V, False, EmSize, EmSize, ExSize, 0)
  else TheFont.CharExtra := 0;
  end;
Result := TMyFont.Create;
Result.Assign(TheFont);
end; 

{----------------ReadFontName}
function ReadFontName(S: string): string;
const
  AMax = 5;
var
  S1: string;
  Done: boolean;

  function NextFontName: string;
  const
    Generic1: array[1..AMax] of string = ('serif', 'monospace', 'sans-serif', 'cursive', 'Helvetica');
    Generic2: array[1..AMax] of string = ('Times New Roman', 'Courier New', 'Arial', 'Lucida Handwriting', 'Arial');
  var
    I: integer;
  begin
  I := Pos(',', S);        {read up to the comma}
  if I > 0 then
    begin
    Result := Trim(System.Copy(S, 1, I-1));
    Delete(S, 1, I);
    end
  else
    begin   {last item}
    Result := Trim(S);
    S := '';
    end;
  for I := 1 to AMax do
    if CompareText(Result, Generic1[I]) = 0 then
      begin
      Result :=  Generic2[I];
      break;
      end;
  if (Result <> '') and (Result[Length(Result)] in ['"', '''']) then
    SetLength(Result, Length(Result)-1);
  if (Result <> '') and (Result[1] in ['"', '''']) then
    Delete(Result, 1, 1);
  end;

begin
Done := False;
S1 := NextFontName;
while (S1 <> '') and not Done do
  begin
  Done := Screen.Fonts.IndexOf(S1) >= 0;
  if Done then
    Result := S1
  else S1 := NextFontName;
  end;
end;

{----------------TProperties.GetSingleFontInfo}
procedure TProperties.GetSingleFontInfo(var Font: ThtFontInfo);
const
  AMax = 5;
var
  S, S1: string;
  Done: boolean;
  Wt: integer;
  Style: TFontStyles;

  function NextFontName: string;
  const
    Generic1: array[1..AMax] of string = ('serif', 'monospace', 'sans-serif', 'cursive', 'Helvetica');
    Generic2: array[1..AMax] of string = ('Times New Roman', 'Courier New', 'Arial', 'Lucida Handwriting', 'Arial');
  var
    I: integer;
  begin
  I := Pos(',', S);        {read up to the comma}
  if I > 0 then
    begin
    Result := Trim(System.Copy(S, 1, I-1));
    Delete(S, 1, I);
    end
  else
    begin   {last item}
    Result := Trim(S);
    S := '';
    end;
  for I := 1 to AMax do
    if CompareText(Result, Generic1[I]) = 0 then
      begin
      Result :=  Generic2[I];
      break;
      end;
  if (Result <> '') and (Result[Length(Result)] in ['"', '''']) then
    SetLength(Result, Length(Result)-1);
  if (Result <> '') and (Result[1] in ['"', '''']) then
    Delete(Result, 1, 1);
  end;

begin      {call only if all things valid}
Font.ibgColor := FontBG;
Font.iColor := Props[Color];
Style := [];
if Pos('bold', Props[FontWeight]) > 0 then
  Style := [fsBold]
else
  begin
  Wt := StrToIntDef(Props[FontWeight], 0);
  if Wt >= 600 then
    Style := [fsBold];
  end;
if (Props[FontStyle] = 'italic') or (Props[FontStyle] = 'oblique') then
  Style := Style + [fsItalic];
if Props[TextDecoration] = 'underline' then
  Style := Style + [fsUnderline]
else if Props[TextDecoration] = 'line-through' then
  Style := Style + [fsStrikeOut];
Font.iStyle := Style;
Font.iSize := Props[FontSize];
Font.iCharset := CharSet;
Font.iCharExtra := Props[LetterSpacing];  

Done := False;
S := Props[FontFamily];
S1 := NextFontName;
while (S1 <> '') and not Done do
  begin
  Done := Screen.Fonts.IndexOf(S1) >= 0;
  if Done then
    begin
    Font.iName := S1;
    end
  else S1 := NextFontName;
  end;
if Font.iName = '' then        
  Font.iName := DefFontname;  
end;

procedure TProperties.CalcLinkFontInfo(Styles: TStyleList; I: integer);
{I is index in PropStack for this item}    
  procedure InsertNewProp(N: integer; const Pseudo: string);
  begin
  PropStack.Insert(N, TProperties.Create);
  PropStack[N].Inherit('', PropStack[N-1]);   
  PropStack[N].Combine(Styles, PropTag, PropClass, PropID, Pseudo, PropTitle, PropStyle);
  end;

begin
PropStack[I].SetFontBG;    
GetSingleFontInfo(FIArray.Ar[LFont]);
InsertNewProp(I+1, 'visited');
PropStack[I+1].SetFontBG;
PropStack[I+1].GetSingleFontInfo(FIArray.Ar[VFont]);
InsertNewProp(I+2, 'hover');
PropStack[I+2].SetFontBG;
PropStack[I+2].GetSingleFontInfo(FIArray.Ar[HVFont]);
PropStack.Delete(I+2);
PropStack.Delete(I+1);
InsertNewProp(I+1, 'hover');
PropStack[I+1].SetFontBG;
PropStack[I+1].GetSingleFontInfo(FIArray.Ar[HLFont]);
PropStack.Delete(I+1);
end;

procedure TProperties.GetFontInfo(AFI: TFontInfoArray);
begin
AFI.Assign(FIArray);
end;

procedure TProperties.GetVMarginArray(var MArray: TVMarginArray);
var
  I: PropIndices;
begin
for I := Low(Marray) to High(MArray) do
  case I of
    BorderTopStyle..BorderLeftStyle:
      if VarType(Props[I]) = VarString then
        MArray[I] := BorderStyleFromString(Props[I])
      else MArray[I] := bssNone;
    else
      MArray[I] := Props[I];
    end;
end;

procedure TProperties.AddPropertyByIndex(Index: PropIndices; PropValue: string);
var
  NewColor: TColor;
begin
case Index of
  BorderColor:
    if ColorFromString(PropValue, False, NewColor) then
      begin
      Props[BorderColor] := NewColor;
      Props[BorderLeftColor] := NewColor;
      Props[BorderTopColor] := NewColor;
      Props[BorderRightColor] := NewColor;
      Props[BorderBottomColor] := NewColor;
      end;
  BorderTopColor .. BorderLeftColor:
    if ColorFromString(PropValue, False, NewColor) then
      Props[Index] := NewColor;
  Color, BackgroundColor:
    if ColorFromString(PropValue, False, NewColor) then
      Props[Index] := NewColor
    else if Index = Color then
      Props[Index] := clBlack
    else Props[Index] := clNone;
  MarginTop..BorderLeftWidth, Width..LeftPos:
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
    else Props[TextTransform] := txNone;
    end;
  WordWrap:                           
    if PropValue = 'break-word' then
      Props[WordWrap] := PropValue
    else Props[WordWrap] := 'normal';
  FontVariant:
    if PropValue = 'small-caps' then
      Props[FontVariant] := PropValue
    else if PropValue = 'normal' then
      Props[FontVariant] := 'normal';
  BorderTopStyle..BorderLeftStyle:
    begin
    if PropValue <> 'none' then      
      Props[BorderStyle] := PropValue;
    Props[Index] := PropValue;
    end;
  BorderStyle:
    begin
    Props[BorderStyle] := PropValue;
    Props[BorderTopStyle] := PropValue;
    Props[BorderRightStyle] := PropValue;
    Props[BorderBottomStyle] := PropValue;
    Props[BorderLeftStyle] := PropValue;
    end;
  else
    Props[Index] := PropValue;   
  end;
end;

procedure TProperties.AddPropertyByName(const PropName, PropValue: string);
var
  Index: PropIndices;
begin
if FindPropIndex(PropName, Index) then
  AddPropertyByIndex(Index, PropValue);
end;

constructor TStyleList.Create(AMasterList: TObject);
begin
inherited Create;
MasterList := AMasterList;
Sorted := True;
Duplicates := dupAccept;
SeqNo := 10;
end;

destructor TStyleList.Destroy;
begin
Clear;
inherited Destroy;
end;

procedure TStyleList.Clear;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TProperties(Objects[I]).Free;
SeqNo := 10;
inherited;
end;

function TStyleList.GetSeqNo: string;
begin   {used to help sort contextual items by entry sequence}
Result := IntToStr(SeqNo);
Inc(SeqNo);
end;

{----------------TStyleList.AddModifyProp}
procedure TStyleList.AddModifyProp(const Selector, Prop, Value: string);
{strings are all lowercase here}
var
  I: integer;
  PropIndex: PropIndices;
  Propty: TProperties;
  NewColor: TColor;
  NewProp: boolean;
begin
if FindPropIndex(Prop, PropIndex) then
  begin
  if not Find(Selector, I) then
    begin
    NewProp := True;
    Propty := TProperties.Create  {newly created property}
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
          begin      {changed the defaults to be the same as link}
          ModifyLinkColor('hover', NewColor);
          ModifyLinkColor('visited', NewColor);
          end
        else if Selector = ':visited' then
          ModifyLinkColor('hover', NewColor);
        Propty.Props[PropIndex] := NewColor;
        end;
    BorderColor:   
      if ColorFromString(Value, False, NewColor) then
        begin
        Propty.Props[BorderColor] := NewColor;
        Propty.Props[BorderLeftColor] := NewColor;
        Propty.Props[BorderTopColor] := NewColor;
        Propty.Props[BorderRightColor] := NewColor;
        Propty.Props[BorderBottomColor] := NewColor;
        end;
    BorderTopColor .. BorderLeftColor:     
      if ColorFromString(Value, False, NewColor) then
        Propty.Props[PropIndex] := NewColor;
    BackgroundColor:
      if ColorFromString(Value, False, NewColor) then
        Propty.Props[PropIndex] := NewColor
      else Propty.Props[PropIndex] := clNone;
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
      else Propty.Props[TextTransform] := txNone;
      end;
    WordWrap:    
      if Value = 'break-word' then
        Propty.Props[WordWrap] := Value
      else Propty.Props[WordWrap] := 'normal';
    FontVariant:
      if Value = 'small-caps' then
        Propty.Props[FontVariant] := Value
      else if Value = 'normal' then
        Propty.Props[FontVariant] := 'normal';
    BorderTopStyle..BorderLeftStyle:
      begin
      if Value <> 'none' then      
        Propty.Props[BorderStyle] := Value;
      Propty.Props[PropIndex] := Value;
      end;
    BorderStyle:
      begin
      Propty.Props[BorderStyle] := Value;
      Propty.Props[BorderTopStyle] := Value;
      Propty.Props[BorderRightStyle] := Value;
      Propty.Props[BorderBottomStyle] := Value;
      Propty.Props[BorderLeftStyle] := Value;
      end;
    LineHeight:
      Propty.Props[PropIndex] := Value;
    else
      Propty.Props[PropIndex] := Value;
    end;
  if NewProp then
    AddObject(Selector, Propty); {it's a newly created property}
  if Pos(':hover', Selector) > 0 then
    TSectionList(MasterList).LinksActive := True;
  if Selector = 'a' then
    begin
    AddModifyProp('::link', Prop, Value);  {also applies to ::link}
    end;
  {$ifdef Quirk}
  if (Selector = 'body') and (PropIndex = Color) then
    FixupTableColor(Propty);      
  {$endif}
  end;
end;

{$ifdef Quirk}
procedure TStyleList.FixupTableColor(BodyProp: TProperties);    
{if Quirk is set, make sure that the table color is defined the same as the
 body color}
var
  Propty1: TProperties;
  I: integer;
begin
if Find('td', I) then
  begin
  Propty1 := TProperties(Objects[I]);
  Propty1.Props[Color] := BodyProp.Props[Color];
  end;
if Find('th', I) then
  begin
  Propty1 := TProperties(Objects[I]);
  Propty1.Props[Color] := BodyProp.Props[Color];
  end;
end;
{$endif}

function TStyleList.AddObject(const S: string; AObject: TObject): Integer;
begin
Result := inherited AddObject(S, AObject);
TProperties(AObject).PropTag := S;
end;

function TStyleList.AddDuplicate(const Tag: string; Prop: TProperties): TProperties;
begin
Result := TProperties.Create;
Result.Copy(Prop);
AddObject(Tag, Result);
end;

procedure TStyleList.ModifyLinkColor(Psuedo: string; AColor: TColor);
var
  I: integer;
begin
if Find('::'+Psuedo, I) then   {the defaults}
  with TProperties(Objects[I]) do
      Props[Color] := AColor;
end;

procedure TStyleList.Initialize(const FontName, PreFontName: string;
     PointSize: integer; AColor, AHotspot, AVisitedColor, AActiveColor: TColor;
     LinkUnderline: boolean; ACharSet: TFontCharSet; MarginHeight, MarginWidth: integer);
type
  ListTypes = (ul, ol, menu, dir, dl, dd, blockquote);
const
  ListStr: array[Low(ListTypes)..High(ListTypes)] of string =
    ('ul', 'ol', 'menu', 'dir', 'dl', 'dd', 'blockquote'); 
var
  HIndex: integer;
  Properties: TProperties;
  J: ListTypes;
  F: double;

begin
Clear;
DefPointSize := PointSize;

Properties := TProperties.Create;
with Properties do
  begin
  DefFontname := FontName;    
  Props[FontFamily] := FontName;
  Props[FontSize] := PointSize*1.0;
  Props[FontStyle] := 'none';
  Props[FontWeight] := 'normal';
  Props[TextAlign] := 'left';
  Props[TextDecoration] := 'none';
  Props[TextTransform] := txNone;
  Props[WordWrap] := 'normal';
  Props[FontVariant] := 'normal';
  Props[Color] := AColor;
  Props[MarginTop] := MarginHeight;
  Props[MarginBottom] := MarginHeight;
  Props[MarginLeft] := MarginWidth;
  Props[MarginRight] := MarginWidth;
  Props[Visibility] := viVisible;
  Props[LetterSpacing] := 0;   
  CharSet := ACharSet;
  end;
AddObject('default', Properties);
DefProp := Properties;

{$ifdef Quirk}
Properties := TProperties.Create;   
with Properties do
  begin
  Props[FontSize] := PointSize*1.0;
  Props[FontStyle] := 'none';
  Props[FontWeight] := 'normal';
  Props[Color] := AColor;
  end;
AddObject('td', Properties);
Properties := AddDuplicate('th', Properties);  
  Properties.Props[FontWeight] := 'bold';
{$endif}

Properties := TProperties.Create;
with Properties do
  begin
  Props[Color] := AHotSpot or PalRelative;
  if LinkUnderline then
    Props[TextDecoration] := 'underline'
  else
    Props[TextDecoration] := 'none';
  end;
AddObject('::link', Properties);    

Properties := TProperties.Create;
with Properties do
  Props[Color] := AVisitedColor or PalRelative;
AddObject('::visited', Properties);    

Properties := TProperties.Create;
with Properties do
  Props[Color] := AActiveColor or PalRelative;
AddObject('::hover', Properties);    

Properties := TProperties.Create;
AddObject('null', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontFamily] := PreFontName;
  Props[FontSize] := PointSize*10.0 / 12.0;
  Props[FontStyle] := 'none';
  Props[FontWeight] := 'normal';
  Props[TextDecoration] := 'none';
  end;
AddObject('pre', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[MarginTop] := AutoParagraph;
  Props[MarginBottom] := AutoParagraph;  
  end;
AddObject('p', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[MarginTop] := 0;
  end;
AddObject('p 11pre', Properties);

for J := Low(ListTypes) to High(ListTypes) do
  begin
  Properties := TProperties.Create;
  with Properties do
    begin
    Props[PaddingLeft] := 35;   
    Props[MarginTop] := 0;
    Props[MarginBottom] := 0;  
    case J of
      ol, ul, menu, dir:
          begin
          Props[ListStyleType] := 'blank';
          Props[MarginTop] := AutoParagraph;
          Props[MarginBottom] := AutoParagraph;
          end;
      dl: begin
          Props[MarginLeft] := 0;
          Props[PaddingLeft] := 0;   
          Props[ListStyleType] := 'none';
          end;
      blockquote:
          begin
          Props[MarginTop] := AutoParagraph;
          Props[MarginBottom] := ParagraphSpace;
          end;

      dd: ;
      end;
    end;
  AddObject(ListStr[J], Properties);
  end;

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontFamily] := PrefontName;
  Props[FontSize] := '0.833em';  {10.0 / 12.0;} 
  end;
AddObject('code', Properties);
AddDuplicate('tt', Properties);
AddDuplicate('kbd', Properties);
AddDuplicate('samp', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontWeight] := 'bold';
  end;
AddObject('b', Properties);
AddDuplicate('strong', Properties);
{$ifndef Quirk}
AddDuplicate('th', Properties);     
{$endif}

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontSize] := '75%';
  Props[VerticalAlign] := 'super';
  end;
AddObject('sup', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontSize] := '75%';
  Props[VerticalAlign] := 'sub';
  end;
AddObject('sub', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontSize] := '1.25em';
  end;
AddObject('big', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontSize] := '0.75em';
  end;
AddObject('small', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[TextAlign] := 'none';    
  end;
AddObject('table', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontStyle] := 'italic';
  end;
AddObject('i', Properties);
AddDuplicate('em', Properties);
AddDuplicate('cite', Properties);
AddDuplicate('var', Properties);

AddDuplicate('address', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[TextDecoration] := 'underline';
  end;
AddObject('u', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[TextDecoration] := 'line-through';
  end;
AddObject('s', Properties);
AddDuplicate('strike', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[TextAlign] := 'center';
  end;
AddObject('center', Properties);
AddDuplicate('caption', Properties);

Properties := TProperties.Create;
with Properties do
  begin
  Props[FontFamily] := 'Arial Unicode MS, Arial';
  Props[FontSize] := '10pt';
  Props[FontStyle] := 'none';
  Props[FontWeight] := 'normal';
  Props[TextAlign] := 'left';
  Props[TextDecoration] := 'none';
  Props[Color] := AColor;
  end;
AddObject('input', Properties);
AddDuplicate('select', Properties);

Properties := AddDuplicate('textarea', Properties); 
if IsWin32Platform then
  Properties.Props[FontFamily] := 'Arial Unicode MS, Arial'  
else
  Properties.Props[FontFamily] := PreFontName;  

Properties := TProperties.Create;
with Properties do
  begin
  Props[MarginLeft] := 0;
  Props[MarginRight] := 0;
  Props[MarginTop] := 10;
  Props[MarginBottom] := 10;
  end;
AddObject('hr', Properties);

for HIndex := 1 to 6 do
  begin
  Properties := TProperties.Create;
  F := PointSize / 12.0;
  with Properties do
    begin
    case HIndex of
      1:  Props[FontSize] := 24.0*F;
      2:  Props[FontSize] := 18.0*F;
      3:  Props[FontSize] := 14.0*F;
      4:  Props[FontSize] := 12.0*F;
      5:  Props[FontSize] := 10.0*F;
      6:  Props[FontSize] := 8.0*F;
      end;
    Props[MarginTop] := 19;
    Props[MarginBottom] := Props[MarginTop];
    Props[FontWeight] := 'bold';
    end;
  AddObject('h'+IntToStr(HIndex), Properties);
  end;
end;

const
  NumColors = 176;  
  Colors: array[1..NumColors] of string[20] =  ('transparent',
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

  ColorValues: array[1..NumColors] of TColor =   (clNone,
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
     clgray,  $A9A9A9, $4F4F2F, $696969, $D3D3D3, $998877, $908070,    
      clBackground, clActiveCaption, clInactiveCaption, clMenu, clWindow,
      clWindowFrame, clMenuText, clWindowText, clCaptionText, clActiveBorder,
      clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText, clBtnFace,
      clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight,
      cl3DDkShadow, clBtnHighlight, clInfoText, clInfoBk, clScrollBar,
      clBtnFace, cl3DLight, clBtnShadow);

var
  ColorStrings: TStringList;

procedure SortColors;         
var
  I: integer;
begin
// Put the Colors into a sorted StringList for faster access.
if ColorStrings = nil then
  begin
  ColorStrings := TStringList.Create;
    with ColorStrings do
      begin
      for I := 1 to NumColors do
        ColorStrings.AddObject(Colors[I], Pointer(ColorValues[I]));
      Sort;
      end;
  end;
end;

function ColorFromString(S: string; NeedPound: boolean; var Color: TColor): boolean;
{Translate StyleSheet color string to Color.  If NeedPound is true, a '#' sign
 is required to preceed a hexidecimal value.}
const
  LastS: string = '?&%@';
  LastColor: TColor = 0;

var
  I, Rd, Bl: integer;
  S1: string;

  function FindRGBColor(S: string): boolean;
  type
    Colors = (red, green, blue);
  var
    A: array[red..blue] of string;
    C: array[red..blue] of integer;
    I, J: integer;
    K: Colors;
  begin
  I := Pos('(', S);
  J := Pos(')', S);
  if (I > 0) and (J > 0) then
    begin
    S := copy(S, 1, J-1);
    S := Trim(Copy(S, I+1, 255));
    for K := Red to Green do
      begin
      I := Pos(',', S);
      A[K] := Trim(copy(S, 1, I-1));
      S := Trim(Copy(S, I+1, 255));
      end;
    A[Blue] := S;
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
      C[K] := IntMax(0, IntMin(255, C[K]));
      end;
    Color := (C[Blue] shl 16) or (C[Green] shl 8) or C[Red];
    Result := True;
    end
  else Result := False;
  end;

begin
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
if (Length(S) > 0) and (S[1] <> '#') and (Pos('rgb(', S) = 0) then
  if ColorStrings.Find(S, I) then
      begin
      Color := TColor(ColorStrings.Objects[I]);
      Result := True;
      LastS := S;
      LastColor := Color;
      Exit;
      end;
S1 := S;
I := Pos('rgb', S);
if (I > 0) then
  Result := FindRGBColor(Copy(S, I+3, 255))
else
  begin
  try
    I := Pos('#', S);
    if I > 0 then
      while I > 0 do  {sometimes multiple ##}   
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
        Insert(S[I], S, I);     {double each character}
    Color := StrToInt('$'+S);  {but bytes are backwards!}
    Rd := Color and $FF;
    Bl := Color and $FF0000;
    Color := (Color and $00FF00) + (Rd shl 16) + (Bl shr 16) or PalRelative;
    Result := True;
  except
    Result := False;
    end;
  end;
if Result then
  begin
  LastS := S1;
  LastColor := Color;
  end;
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
  begin
  Ar[I].iName := Source.Ar[I].iName;
  Ar[I].iSize := Source.Ar[I].iSize;
  Ar[I].iStyle := Source.Ar[I].iStyle;
  Ar[I].iColor := Source.Ar[I].iColor;
  Ar[I].ibgColor := Source.Ar[I].ibgColor;
  Ar[I].iCharSet := Source.Ar[I].iCharSet;
  Ar[I].iCharExtra := Source.Ar[I].iCharExtra;  
  end;
end;

{$HINTS OFF}
{----------------FontSizeConv}
function FontSizeConv(const Str: string; OldSize: double): double;
{given a font-size string, return the point size}
var
  V, PPI: double;
  S1, S2: string;
  I, J: integer;
begin
PPI := Screen.PixelsPerInch;
Val(Str, V, I);
J := Pos('e', Str);   {'e' would be legal for Val but not for us}
if (J <> 0) and (I > J) then
  I := J;
if I > 0 then
  begin
  S1 := Copy(Str, 1, I-1);
  S2 := Trim(Copy(Str, I, Length(Str)-I+1));  
  end
else
  begin
  S1 := Str;
  S2 := '';
  end;
{S1 has the number, S2 the units}
Val(S1, V, I);
if S2 = 'in' then
  V := 72.0*V
else if S2 = 'cm' then
  V := 72.0*V/2.54
else if S2 = 'mm' then
  V := 72.0*V/25.4
else if S2 = 'pt' then
else if S2 = 'px' then
  V := V*72.0/PPI
else if S2 = 'pc' then
  V := V*12.0
else if S2 = 'em' then
  V := V*OldSize
else if S2 = 'ex' then
  V := V*OldSize/2.0        {1/2 of em}
else if S2 = '%' then
  V := V*OldSize/100.0
else if S2 = '' then
  V := V*72.0/PPI        {pixels by default}
else if S2 = 'smaller' then
  V := 0.75*OldSize        
else if S2 = 'larger' then
  V := 1.25*OldSize
else if S2 = 'xx-small' then
  V := DefPointSize/1.5
else if S2 = 'x-small' then
  V := DefPointSize/1.2
else if S2 = 'small' then
  V := DefPointSize
else if S2 = 'medium' then
  V := DefPointSize*1.2
else if S2 = 'large' then
  V := DefPointSize*1.5
else if S2 = 'x-large' then
  V := DefPointSize*2.0
else if S2 = 'xx-large' then
  V := DefPointSize*3.0
else V := DefPointSize;   {error, return 12pt}
Result := V;  
end;

{----------------LengthConv}
function LengthConv(const Str: string; Relative: boolean; Base, EmSize, ExSize,
                          Default: integer): integer;
{given a length string, return the appropriate pixel value.  Base is the
 base value for percentage. EmSize, ExSize for units relative to the font.
 Relative makes a numerical entry relative to Base.
 Default returned if no match.}
var
  V, PPI: double;
  S1, S2: string;
  I, J: integer;
begin
PPI := Screen.PixelsPerInch;
Val(Str, V, I);
J := Pos('e', Str);   {'e' would be legal for Val but not for us}
if (J <> 0) and (I > J) then
  I := J;
if I > 0 then
  begin
  S1 := Copy(Str, 1, I-1);
  S2 := Trim(Copy(Str, I, Length(Str)-I+1));    
  end
else
  begin
  S1 := Str;
  S2 := '';
  end;
{S1 has the number, S2 the units}
Val(S1, V, I);
if S2 = 'in' then
  V := PPI*V
else if S2 = 'cm' then
  V := PPI*V/2.54
else if S2 = 'mm' then
  V := PPI*V/25.4
else if S2 = 'pt' then
  V := V*PPI/72.0
else if S2 = 'px' then
else if S2 = 'pc' then
  V := V*PPI/6.0
else if S2 = 'em' then
  V := V*EmSize
else if S2 = 'ex' then
  V := V*ExSize
else if S2 = '%' then
  V := V*Base/100.0
else if S2 = '' then
  if Relative then
    V := V*Base      {relative}
  else
    V := V           {same as pixels, at least for margins}
else V := Default;
Result := Round(V);
end;

Initialization

SortColors;

Finalization

ColorStrings.Free;

end.


