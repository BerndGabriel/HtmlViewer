{
Version   12
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

{$I htmlcons.inc}

unit HtmlRenderer;

interface

uses
  Windows, SysUtils,
  Classes, Controls, Contnrs, Graphics, Variants, Math,
  //
  BegaClasses,
  HtmlBoxes,
  HtmlBuffer,
  HtmlDocument,
  HtmlDraw,
  HtmlElements,
  HtmlFonts,
  HtmlGlobals,
  HtmlImages,
  HtmlIndentation,
  HtmlStyles,
  HtmlSymbols,
  Parser,
  StyleTypes,
  UrlSubs;

type

//------------------------------------------------------------------------------
// THtmlControlOfElementMap remembers controls that are used to render elements.
//------------------------------------------------------------------------------
// Some elements like the user interaction elements button, input, etc. and
// objects like frames and iframes are represented by controls of the underlying
// visual component library (VCL, LCL, ...). These must be reused by consecutive
// renderings. This map supports one control per element.
//------------------------------------------------------------------------------

  THtmlControlOfElementMap = class(TBegaCustomMap)
  private
    function GetKey(Index: Integer): THtmlElement;
    function GetValue(Index: Integer): TControl;
  public
    function Get(Key: THtmlElement): TControl; {$ifdef UseInline} inline; {$endif}
    function Put(Key: THtmlElement; Control: TControl): TControl; {$ifdef UseInline} inline; {$endif}
    procedure Clear;
    property Elements[Index: Integer]: THtmlElement read GetKey;
    property Controls[Index: Integer]: TControl read GetValue;
  end;

//------------------------------------------------------------------------------
// THtmlRenderedDocument holds the rendered html document.
//------------------------------------------------------------------------------
// It is a collection of html boxes. A html element produces null or more
// html boxes when rendered.
//
// An element with Display:none produces no box at all.
// A non empty block element produces 1 box.
// A non empty inline element produces 1 or more boxes depending on line wraps
// and space left by aligned elements.
//------------------------------------------------------------------------------

//  THtmlRenderedDocument = class
//  end;
//
//  THtmlVisualizedDocument = class(THtmlRenderedDocument)
//  private
//    FView: THtmlBox;
//  public
//    constructor Create(View: THtmlBox);
//  end;

//------------------------------------------------------------------------------
// THtmlRenderer renders html documents.
//------------------------------------------------------------------------------
// Given source document, media type, destination area, and optionally (if re-
// rendered) the used visual controls, it renders the document for the specified
// media and area.
//------------------------------------------------------------------------------

  TMediaCapabilities = set of (mcFrames, mcScript, mcEmbed);

  THtmlRenderer = class
  private
    FDocument: THtmlDocument; // the source to render to destination
    FMediaType: TMediaType;   // media type of destination
    FCapabilities: TMediaCapabilities;
    FOnGetBuffer: TGetBufferEvent;
    function GetResultingProperties(Element: THtmlElement): TResultingPropertyMap;
    function GetBaseUrl: ThtString;
  protected
    function GetBuffer(const Url: ThtString): ThtBuffer;
  public
    constructor Create(Document: THtmlDocument; MediaType: TMediaType);
    property BaseUrl: ThtString read GetBaseUrl;
    property MediaCapabilities: TMediaCapabilities read FCapabilities write FCapabilities;
    property MediaType: TMediaType read FMediaType;
    property OnGetBuffer: TGetBufferEvent read FOnGetBuffer write FOnGetBuffer;
  end;

  THtmlRendererBlockInfo = record
    Display: TDisplayStyle;
    Position: TBoxPositionStyle;
    Float: TBoxFloatStyle;
  end;

  THtmlVisualRenderer = class(THtmlRenderer)
  private
    //--------------------------------------
    // settings / environment
    //--------------------------------------

    // general
    FWidth: Integer;          // width of destination viewport in pixels
    FHeight: Integer;         // height of destination viewport in pixels
    FControls: THtmlControlOfElementMap;
    // fonts
    FDefaultFont: ThtFont;
    FDefaultFontInfo: ThtFontInfo;
    // images
    FImages: ThtImageCache;
    FOnGetImage: TGetImageEvent;

    //--------------------------------------
    // status
    //--------------------------------------

    // positioning
    FSketchMapStack: TSketchMapStack;

    function GetBorderStyle(Prop: TResultingProperty; Parent, Default: TBorderStyle): TBorderStyle;
    function GetColor(Prop: TResultingProperty; Parent, Default: TColor): TColor;
    function GetControl(const Element: THtmlElement): TControl;
    function DefaultFontSize: Double;
    function GetDisplay(Prop: TResultingProperty; Parent, Default: TDisplayStyle): TDisplayStyle;
    function GetFloat(Prop: TResultingProperty; Parent, Default: TBoxFloatStyle): TBoxFloatStyle;
    function GetImage(Prop: TResultingProperty; Parent, Default: ThtImage): ThtImage;
    function GetLength(Prop: TResultingProperty; Relative: Boolean; Parent, EmBase, Default: Double): Double;
    function GetPosition(Prop: TResultingProperty; Parent, Default: TBoxPositionStyle): TBoxPositionStyle;
    function GetPropValue(Prop: TResultingProperty; Parent, Default: Variant): Variant;
    //procedure SetControls(const Value: THtmlControlOfElementMap);
    procedure SetPropertiesToBox(Element: THtmlElement; Box: THtmlBox; Properties: TResultingPropertyMap);
    procedure GetFontInfo(Props: TResultingPropertyMap; out Font: ThtFontInfo; const Parent, Default: ThtFontInfo);
    //
    procedure Render(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); overload;
  protected
    procedure RenderBody(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
    procedure RenderBox(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
    procedure RenderFrameset(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
    procedure RenderText(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
  public
    constructor Create(
      Document: THtmlDocument;
      ControlMap: THtmlControlOfElementMap;
      ImageCache: ThtImageCache;
      MediaType: TMediaType;
      DefaultFont: ThtFont;
      Width, Height: Integer);
    destructor Destroy; override;
    procedure Render(Owner: TWinControl; ParentBox: THtmlBox); overload;
    property ControlOfElementMap: THtmlControlOfElementMap read FControls; // write SetControls;
    property OnGetImage: TGetImageEvent read FOnGetImage write FOnGetImage;
  end;

implementation

{ THtmlControlOfElementMap }

procedure THtmlControlOfElementMap.Clear;
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
    TControl(Remove(Index)).Free;
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function THtmlControlOfElementMap.Get(Key: THtmlElement): TControl;
begin
  Result := inherited Get(Key);
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function THtmlControlOfElementMap.GetKey(Index: Integer): THtmlElement;
begin
  Result := inherited GetKey(Index);
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function THtmlControlOfElementMap.GetValue(Index: Integer): TControl;
begin
  Result := inherited GetValue(Index);
end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
function THtmlControlOfElementMap.Put(Key: THtmlElement; Control: TControl): TControl;
begin
  Result := inherited Put(Key, Control);
end;

{ THtmlRenderer }

//-- BG ---------------------------------------------------------- 17.04.2011 --
constructor THtmlRenderer.Create(Document: THtmlDocument; MediaType: TMediaType);
begin
  inherited Create;
  FDocument := Document;
  FMediaType := MediaType;
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function THtmlRenderer.GetBaseUrl: ThtString;
begin
  Result := FDocument.BaseUrl;
end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function THtmlRenderer.GetBuffer(const Url: ThtString): ThtBuffer;
var
  Filename: ThtString;
  Stream: TStream;
begin
  if assigned(FOnGetBuffer) then
    Result := FOnGetBuffer(Self, Url)
  else
  begin
    Filename := HTMLToDos(Url);
    if FileExists(Filename) then
    begin
      Stream := TFileStream.Create(Filename, fmOpenRead + fmShareDenyWrite);
      try
        Result := TBuffer.Create(Stream, Filename);
      finally
        Stream.Free;
      end;
    end
    else
      Result := nil;
  end;
end;

//-- BG ---------------------------------------------------------- 17.04.2011 --
function THtmlRenderer.GetResultingProperties(Element: THtmlElement): TResultingPropertyMap;
var
  I: Integer;
  Ruleset: TRuleset;
  Selector: TStyleSelector;
begin
  Result := TResultingPropertyMap.Create;

  // 1) Fill map with properties from style attribute (highest prio),
  Result.UpdateFromAttributes(Element.StyleProperties, True);

  // 2) Walk through ruleset list in reverse order. If a selector of the ruleset matches,
  //    then call UpdateFromProperties() with the ruleset's properties and if the combination has a
  //    higher cascading order, this becomes the new resulting value.
  if FDocument.RuleSets <> nil then
    for I := FDocument.RuleSets.Count - 1 downto 0 do
    begin
      Ruleset := FDocument.RuleSets[I];
      if FMediaType in Ruleset.MediaTypes then
      begin
        Selector := Ruleset.Selectors.First;
        while Selector <> nil do
        begin
          if Element.IsMatching(Selector) then
          begin
            Result.UpdateFromProperties(Ruleset.Properties, Selector);
            break;
          end;
          Selector := Selector.Next;
        end;
      end;
    end;

  // 3) Fill up missing properties with other tag attributes (like color, width, height, ...).
  Result.UpdateFromAttributes(Element.AttributeProperties, False);
end;

//{ THtmlVisualizedDocument }
//
////-- BG ---------------------------------------------------------- 25.04.2011 --
//constructor THtmlVisualizedDocument.Create(View: THtmlBox);
//begin
//  FView := View;
//end;

{ THtmlVisualRenderer }

//-- BG ---------------------------------------------------------- 25.04.2011 --
constructor THtmlVisualRenderer.Create(
  Document: THtmlDocument;
  ControlMap: THtmlControlOfElementMap;
  ImageCache: ThtImageCache;
  MediaType: TMediaType;
  DefaultFont: ThtFont;
  Width, Height: Integer);
begin
  inherited Create(Document, MediaType);
  FSketchMapStack := TSketchMapStack.Create;
  FCapabilities := [mcFrames];
  FControls := ControlMap;
  FImages := ImageCache;
  FDefaultFont := DefaultFont;
  FWidth := Width;
  FHeight := Height;
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function THtmlVisualRenderer.DefaultFontSize: Double;
begin
  if FDefaultFont = nil then
    Result := 12
  else if FDefaultFont.Size < 0 then
    Result := FDefaultFont.Height * 72 / FDefaultFont.PixelsPerInch
  else
    Result := FDefaultFont.Size;
end;

//-- BG ---------------------------------------------------------- 28.08.2011 --
destructor THtmlVisualRenderer.Destroy;
begin
  FSketchMapStack.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function THtmlVisualRenderer.GetBorderStyle(Prop: TResultingProperty; Parent, Default: TBorderStyle): TBorderStyle;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Prop.Value) then
      Result := TBorderStyle(Prop.Prop.Value)
    else if VarIsStr(Prop.Prop.Value) then
      if TryStrToBorderStyle(Prop.Prop.Value, Result) then
        Prop.Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function THtmlVisualRenderer.GetColor(Prop: TResultingProperty; Parent, Default: TColor): TColor;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Prop.Value) then
      Result := TColor(Prop.Prop.Value)
    else if VarIsStr(Prop.Prop.Value) then
      if TryStrToColor(Prop.Prop.Value, False, Result) then
        Prop.Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
function THtmlVisualRenderer.GetControl(const Element: THtmlElement): TControl;
begin
  if FControls <> nil then
    Result := FControls.Get(Element)
  else
    Result := nil;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function THtmlVisualRenderer.GetDisplay(Prop: TResultingProperty; Parent, Default: TDisplayStyle): TDisplayStyle;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Prop.Value) then
      Result := TDisplayStyle(Prop.Prop.Value)
    else if VarIsStr(Prop.Prop.Value) then
      if TryStrToDisplayStyle(Prop.Prop.Value, Result) then
        Prop.Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function THtmlVisualRenderer.GetFloat(Prop: TResultingProperty; Parent, Default: TBoxFloatStyle): TBoxFloatStyle;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Prop.Value) then
      Result := TBoxFloatStyle(Prop.Prop.Value)
    else if VarIsStr(Prop.Prop.Value) then
      if TryStrToBoxFloatStyle(Prop.Prop.Value, Result) then
        Prop.Prop.Value := Integer(Result);
end;

//------------------------------------------------------------------------------
procedure THtmlVisualRenderer.GetFontInfo(Props: TResultingPropertyMap; out Font: ThtFontInfo; const Parent, Default: ThtFontInfo);

  //BG, 03.05.2011:
  function GetFontWeight(Prop: TResultingProperty; Parent, Default: Integer): Integer;
  // CSS 2.1:
  // Parent  | 100 200 300 400 500 600 700 800 900
  // --------+------------------------------------
  // bolder  | 400 400 400 700 700 900 900 900 900
  // lighter | 100 100 100 100 100 400 400 700 700
  begin
    Result := Default;
    if Prop <> nil then
      if Prop.Prop.Value = Inherit then
        Result := Parent
      else if VarIsNumeric(Prop.Prop.Value) then
        Result := Prop.Prop.Value
      else if VarIsStr(Prop.Prop.Value) then
      begin
        if htCompareString(Prop.Prop.Value, 'normal') = 0 then
          Result := 400
        else if htCompareString(Prop.Prop.Value, 'bold') = 0 then
          Result := 700
        else if htCompareString(Prop.Prop.Value, 'bolder') = 0 then
          if Parent <= 300 then
            Result := 400
          else if Parent < 600 then
            Result := 700
          else
            Result := 900
        else if htCompareString(Prop.Prop.Value, 'lighter') = 0 then
          if Parent <= 500 then
            Result := 100
          else if Parent < 800 then
            Result := 400
          else
            Result := 700
        else if htCompareString(Prop.Prop.Value, 'light') = 0 then
          Result := 100;
        Prop.Prop.Value := Result;
      end;
  end;

  //BG, 03.05.2011:
  function IsItalic(Prop: TResultingProperty; Parent, Default: Boolean): Boolean;
  begin                                                                                                         
    Result := Default;
    if Prop <> nil then
      if Prop.Prop.Value = Inherit then
        Result := Parent
      else if VarIsType(Prop.Prop.Value, varBoolean) then
        Result := Prop.Prop.Value
      else if VarIsStr(Prop.Prop.Value) then
      begin
        if htCompareString(Prop.Prop.Value, 'italic') = 0 then
          Result := True
        else if htCompareString(Prop.Prop.Value, 'oblique') = 0 then
          Result := True
        else
          Result := False;
        Prop.Prop.Value := Result;
      end;
  end;

  //BG, 03.05.2011:
  function GetTextDecoration(Prop: TResultingProperty; Parent, Default: TFontStyles): TFontStyles;
  var
    ResultAsByte: Byte absolute Result;
  begin
    Result := Default;
    if Prop <> nil then
      if Prop.Prop.Value = Inherit then
        Result := Parent
      else if VarIsOrdinal(Prop.Prop.Value) then
        ResultAsByte := Prop.Prop.Value
      else if VarIsStr(Prop.Prop.Value) then
      begin
        if htCompareString(Prop.Prop.Value, 'underline') = 0 then
          Result := [fsUnderline]
        else if htCompareString(Prop.Prop.Value, 'line-through') = 0 then
          Result := [fsStrikeOut]
        else
          Result := [];
        Prop.Prop.Value := ResultAsByte;
      end;
  end;

  //BG, 03.05.2011:
  function GetFontName(Prop: TResultingProperty; Parent, Default: ThtString): ThtString;

    function FontFamilyToFontName(Family: ThtString): ThtString;
    begin
      Result := Family;
    end;

  begin
    Result := Default;
    if Prop <> nil then
      if Prop.Prop.Value = Inherit then
        Result := Parent
      else if VarIsStr(Prop.Prop.Value) then
      begin
        Result := FontFamilyToFontName(Prop.Prop.Value);
        Prop.Prop.Value := Result;
      end;
  end;

  //BG, 03.05.2011:
  function GetFontSize(Prop: TResultingProperty; Parent, Default: Double): Double;
  begin
    Result := Default;
    if Prop <> nil then
      if Prop.Prop.Value = Inherit then
        Result := Parent
      else if VarIsNumeric(Prop.Prop.Value) then
        Result := Prop.Prop.Value
      else if VarIsStr(Prop.Prop.Value) then
      begin
        Result := StrToFontSize(Prop.Prop.Value, FontConvBase, DefaultFontSize, Parent, Default);
        Prop.Prop.Value := Result;
      end;
  end;

var
  Style: TFontStyles;
begin
  Font.ibgColor := GetColor(Props[BackgroundColor], Parent.ibgColor, Default.ibgColor);
  Font.iColor := GetColor(Props[Color], Parent.iColor, Default.iColor);

  Style := [];
  if GetFontWeight(Props[FontWeight], Parent.iWeight, Default.iWeight) >= 600 then
    Include(Style, fsBold);
  if IsItalic(Props[FontStyle], fsItalic in Parent.iStyle, fsItalic in Default.iStyle) then
    Include(Style, fsItalic);
  Font.iStyle := Style + GetTextDecoration(Props[TextDecoration],
    Parent.iStyle * [fsUnderline, fsStrikeOut],
    Default.iStyle * [fsUnderline, fsStrikeOut]);

  Font.iSize := GetFontSize(Props[FontSize], Parent.iSize, Default.iSize);
  Font.iCharset := Default.iCharSet;

  Font.iName := GetFontName(Props[FontFamily], Parent.iName, Default.iName);
  Font.iCharExtra := GetPropValue(Props[LetterSpacing], Parent.iCharExtra, Default.iCharExtra);
end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function THtmlVisualRenderer.GetImage(Prop: TResultingProperty; Parent, Default: ThtImage): ThtImage;
var
  ImageName: ThtString;
  ImageIndex: Integer;
  FileName: ThtString;
  Stream: TStream;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else
    begin
      ImageName := Prop.Prop.Value;
      if FImages.Find(ImageName, ImageIndex) then
        Result := FImages.GetImage(ImageIndex)
      else
      begin
        if assigned(FOnGetImage) then
          Result := FOnGetImage(Self, ImageName)
        else
        begin
          FileName := HTMLToDos(ImageName);
          try
            Stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
            try
              Result := LoadImageFromStream(Stream, NotTransp);
            finally
              Stream.Free;
            end;
          except
            Result := nil;
          end;
        end;
        FImages.InsertImage(ImageIndex, ImageName, Result);
      end;
    end;
end;

var tmp: ThtString;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function THtmlVisualRenderer.GetLength(Prop: TResultingProperty; Relative: Boolean; Parent, EmBase, Default: Double): Double;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else if VarIsNumeric(Prop.Prop.Value) then
      Result := Prop.Prop.Value
    else if VarIsStr(Prop.Prop.Value) then
    begin
      Result := StrToLength(Prop.Prop.Value, Relative, Parent, EmBase, Default);
      Prop.Prop.Value := Result;
    end;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function THtmlVisualRenderer.GetPosition(Prop: TResultingProperty; Parent, Default: TBoxPositionStyle): TBoxPositionStyle;
begin
  Result := Default;
  if Prop <> nil then
    if Prop.Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Prop.Value) then
      Result := TBoxPositionStyle(Prop.Prop.Value)
    else if VarIsStr(Prop.Prop.Value) then
      if TryStrToBoxPositionStyle(Prop.Prop.Value, Result) then
        Prop.Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function THtmlVisualRenderer.GetPropValue(Prop: TResultingProperty; Parent, Default: Variant): Variant;
begin
  if Prop = nil then
    Result := Default
  else
  begin
    Result := Prop.Prop.Value;
    if Result = Inherit then
      Result := Parent;
  end;
end;

//-- BG ---------------------------------------------------------- 17.04.2011 --
procedure THtmlVisualRenderer.Render(Owner: TWinControl; ParentBox: THtmlBox);
begin
  Render(Owner, ParentBox, FDocument.Tree);
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.Render(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
begin
  case Element.Symbol of
    FrameSetSy: RenderFrameset(Owner, ParentBox, Element);
    BodySy: RenderBody(Owner, ParentBox, Element);

    TextSy: RenderText(Owner, ParentBox, Element);

    PSy,
    DivSy,
    SpanSy: RenderBox(Owner, ParentBox, Element);
  else
  end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.RenderBody(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
var
  Box: THtmlBodyBox;
  Control: THtmlBodyControl;
  Child: THtmlElement;
  Properties: TResultingPropertyMap;
  Info: THtmlRendererBlockInfo;
begin
  Properties := GetResultingProperties(Element);
  try
    // compute display, position and float:
    Info.Display := GetDisplay(Properties[psDisplay], pdBlock, pdBlock);
    if Info.Display = pdNone then
      exit;

    Info.Position := GetPosition(Properties[psPosition], posStatic, posStatic);
    case Info.Position of
      posAbsolute,
      posFixed:
      begin
        Info.Float := flNone;
        Info.Display := ToRootDisplayStyle(Info.Display);
      end;
    else
      Info.Float := GetFloat(Properties[psFloat], flNone, flNone);
      if Info.Float <> flNone then
        Info.Display := ToRootDisplayStyle(Info.Display);
    end;
    // for the root element:
    Info.Display := ToRootDisplayStyle(Info.Display);

    // get or create the body control
    Control := GetControl(Element) as THtmlBodyControl;
    if Control = nil then
    begin
      Control := THtmlBodyControl.Create(Owner);
      ControlOfElementMap.Put(Element, Control);
    end;
    Box := THtmlBodyBox.Create(ParentBox, Control);
    Box.BoundsRect := ParentBox.BoundsRect;
    Box.Tiled := True;

    // set properties
    SetPropertiesToBox(Element, Box, Properties);

    // render children
    Child := Element.FirstChild;
    while Child <> nil do
    begin
      Render(Control, Box, Child);
      Child := Child.Next;
    end;
  finally
    Properties.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 29.08.2011 --
procedure THtmlVisualRenderer.RenderBox(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
var
  Box: THtmlElementBox;
  Child: THtmlElement;
  Properties: TResultingPropertyMap;
  Info: THtmlRendererBlockInfo;
begin
  Properties := GetResultingProperties(Element);
  try
    // compute display, position and float:
    Info.Display := GetDisplay(Properties[psDisplay], pdBlock, pdBlock);
    if Info.Display = pdNone then
      exit;

    Info.Position := GetPosition(Properties[psPosition], posStatic, posStatic);
    case Info.Position of
      posAbsolute,
      posFixed:
      begin
        Info.Float := flNone;
      end;
    else
      Info.Float := GetFloat(Properties[psFloat], flNone, flNone);
    end;

    Box := THtmlElementBox.Create(ParentBox);
    Box.BoundsRect := ParentBox.BoundsRect;
    Box.Tiled := True;

    // set properties
    SetPropertiesToBox(Element, Box, Properties);

    // render children
    Child := Element.FirstChild;
    while Child <> nil do
    begin
      Render(Owner, Box, Child);
      Child := Child.Next;
    end;
  finally
    Properties.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.RenderFrameset(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
begin

end;

//-- BG ---------------------------------------------------------- 28.08.2011 --
procedure THtmlVisualRenderer.RenderText(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
begin

end;

////-- BG ---------------------------------------------------------- 18.04.2011 --
//procedure THtmlVisualRenderer.SetControls(const Value: THtmlControlOfElementMap);
//begin
//  if FControls <> Value then
//    FControls.Assign(Value);
//end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
procedure THtmlVisualRenderer.SetPropertiesToBox(Element: THtmlElement; Box: THtmlBox; Properties: TResultingPropertyMap);

  function GetLengths(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectIntegers; BaseSize: Integer): TRectIntegers;
  begin
    Result[reLeft]   := Round(GetLength(Properties[Left],   False, Parent[reLeft],   BaseSize, Default[reLeft]  ));
    Result[reTop]    := Round(GetLength(Properties[Top],    False, Parent[reTop],    BaseSize, Default[reTop]   ));
    Result[reRight]  := Round(GetLength(Properties[Right],  False, Parent[reRight],  BaseSize, Default[reRight] ));
    Result[reBottom] := Round(GetLength(Properties[Bottom], False, Parent[reBottom], BaseSize, Default[reBottom]));
  end;

  function GetColors(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectColors): TRectColors;
  begin
    Result[reLeft]   := GetColor(Properties[Left],   Parent[reLeft],   Default[reLeft]  );
    Result[reTop]    := GetColor(Properties[Top],    Parent[reTop],    Default[reTop]   );
    Result[reRight]  := GetColor(Properties[Right],  Parent[reRight],  Default[reRight] );
    Result[reBottom] := GetColor(Properties[Bottom], Parent[reBottom], Default[reBottom]);
  end;

  function GetStyles(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectStyles): TRectStyles;
  begin
    Result[reLeft]   := GetBorderStyle(Properties[Left],   Parent[reLeft],   Default[reLeft]);
    Result[reTop]    := GetBorderStyle(Properties[Top],    Parent[reTop],    Default[reTop]);
    Result[reRight]  := GetBorderStyle(Properties[Right],  Parent[reRight],  Default[reRight]);
    Result[reBottom] := GetBorderStyle(Properties[Bottom], Parent[reBottom], Default[reBottom]);
  end;

const
  NullLengths: TRectIntegers = (0, 0, 0, 0);
  NoneColors: TRectColors = (clNone, clNone, clNone, clNone);
  NoneStyles: TRectStyles = (bssNone, bssNone, bssNone, bssNone);
var
  ParentBox: THtmlBox;
  FontInfo: ThtFontInfo;
  EmBase: Integer;
  Font: ThtFont;
begin
  ParentBox := Box.Parent;
  while ParentBox is THtmlAnonymousBox do
    ParentBox := ParentBox.Parent;

  // Font
  GetFontInfo(Properties, FontInfo, FDefaultFontInfo, FDefaultFontInfo);
  Font := AllMyFonts.GetFontLike(FontInfo);
  Box.Font := Font;
  EmBase := Font.EmSize;
  freeAndNil(Font);

  if ParentBox <> nil then
  begin
    Box.Color         := GetColor(Properties[BackgroundColor], ParentBox.Color, clNone);
    Box.Image         := GetImage(Properties[BackgroundImage], ParentBox.Image, nil);
    Box.Margins       := GetLengths(MarginLeft, MarginTop, MarginRight, MarginBottom, ParentBox.Margins, NullLengths, EmBase);
    Box.Paddings      := GetLengths(PaddingLeft, PaddingTop, PaddingRight, PaddingBottom, ParentBox.Paddings, NullLengths, EmBase);
    Box.BorderWidths  := GetLengths(BorderLeftWidth, BorderTopWidth, BorderRightWidth, BorderBottomWidth, ParentBox.BorderWidths, NullLengths, EmBase);
    Box.BorderColors  := GetColors(BorderLeftColor, BorderTopColor, BorderRightColor, BorderBottomColor, ParentBox.BorderColors, NoneColors);
    Box.BorderStyles  := GetStyles(BorderLeftStyle, BorderTopStyle, BorderRightStyle, BorderBottomStyle, ParentBox.BorderStyles, NoneStyles);
  end
  else
  begin
    Box.Color         := GetColor(Properties[BackgroundColor], clNone, clNone);
    Box.Image         := GetImage(Properties[BackgroundImage], nil, nil);
    Box.Margins       := GetLengths(MarginLeft, MarginTop, MarginRight, MarginBottom, NullLengths, NullLengths, EmBase);
    Box.Paddings      := GetLengths(PaddingLeft, PaddingTop, PaddingRight, PaddingBottom, NullLengths, NullLengths, EmBase);
    Box.BorderWidths  := GetLengths(BorderLeftWidth, BorderTopWidth, BorderRightWidth, BorderBottomWidth, NullLengths, NullLengths, EmBase);
    Box.BorderColors  := GetColors(BorderLeftColor, BorderTopColor, BorderRightColor, BorderBottomColor, NoneColors, NoneColors);
    Box.BorderStyles  := GetStyles(BorderLeftStyle, BorderTopStyle, BorderRightStyle, BorderBottomStyle, NoneStyles, NoneStyles);
  end;
end;

end.
