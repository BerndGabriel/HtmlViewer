{
Version   12
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

unit HtmlRenderer;

interface

uses
  Windows, SysUtils,
  Classes, Controls, Contnrs, Graphics, Variants, Math,
  //
  BegaClasses,
  HtmlBoxes,
  HtmlBuffer,
  HtmlCaches,
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
  EHtmlRendererException = class(Exception);

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

  THtmlVisualRenderer = class(THtmlRenderer)
  private
    //--------------------------------------
    // settings / environment
    //--------------------------------------

    // general
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

    function DefaultFontSize: Double;
    function GetControl(const Element: THtmlElement): TControl;
    function GetImage(Prop: TResultingProperty; Parent, Default: ThtImage): ThtImage;
    function SketchMap: TSketchMap; {$ifdef UseInline} inline; {$endif}
    procedure GetFontInfo(Props: TResultingPropertyMap; out Font: ThtFontInfo; const Parent, Default: ThtFontInfo);
    procedure SetPropertiesToBox(
      Element: THtmlElement;
      Box: THtmlBox;
      Properties: TResultingPropertyMap;
      const Info: THtmlElementBoxingInfo);
    //
    procedure Render(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
    procedure RenderChildren(Owner: TWinControl; Box: THtmlBox; Element: THtmlElement);
  protected
    procedure RenderBody(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
    procedure RenderBox(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
    procedure RenderFrameset(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
  public
    constructor Create(
      Document: THtmlDocument;
      ControlMap: THtmlControlOfElementMap;
      ImageCache: ThtImageCache;
      MediaType: TMediaType;
      DefaultFont: ThtFont;
      Width, Height: Integer);
    destructor Destroy; override;
    procedure RenderDocument(Owner: TWinControl; ParentBox: THtmlBox);
    property ControlOfElementMap: THtmlControlOfElementMap read FControls;
    property OnGetImage: TGetImageEvent read FOnGetImage write FOnGetImage;
  end;

implementation

var
  DefaultBox: THtmlBox;

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
  FSketchMapStack.Push(TSketchMap.Create(Width));
  FCapabilities := [mcFrames];
  FControls := ControlMap;
  FImages := ImageCache;
  FDefaultFont := DefaultFont;
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

//-- BG ---------------------------------------------------------- 25.04.2011 --
function THtmlVisualRenderer.GetControl(const Element: THtmlElement): TControl;
begin
  if FControls <> nil then
    Result := FControls.Get(Element)
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
procedure THtmlVisualRenderer.GetFontInfo(Props: TResultingPropertyMap; out Font: ThtFontInfo; const Parent, Default: ThtFontInfo);
var
  Style: TFontStyles;
begin
  Font.ibgColor := Props[BackgroundColor].GetColor(Parent.ibgColor, Default.ibgColor);
  Font.iColor := Props[Color].GetColor(Parent.iColor, Default.iColor);

  Style := [];
  if Props[FontWeight].GetFontWeight(Parent.iWeight, Default.iWeight) >= 600 then
    Include(Style, fsBold);
  if Props[FontStyle].IsItalic(fsItalic in Parent.iStyle, fsItalic in Default.iStyle) then
    Include(Style, fsItalic);
  Font.iStyle := Style + Props[TextDecoration].GetTextDecoration(
    Parent.iStyle * [fsUnderline, fsStrikeOut],
    Default.iStyle * [fsUnderline, fsStrikeOut]);

  Font.iSize := Props[FontSize].GetFontSize(DefaultFontSize, Parent.iSize, Default.iSize);
  Font.iCharset := Default.iCharSet;

  Font.iName := Props[FontFamily].GetFontName(Parent.iName, Default.iName);
  Font.iCharExtra := Props[LetterSpacing].GetValue(Parent.iCharExtra, Default.iCharExtra);
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
      ImageName := RemoveQuotes(Prop.Prop.Value);
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

//-- BG ---------------------------------------------------------- 17.04.2011 --
procedure THtmlVisualRenderer.RenderDocument(Owner: TWinControl; ParentBox: THtmlBox);
begin
  Render(Owner, ParentBox, FDocument.Tree);
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.Render(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
begin
  case Element.Symbol of
    FrameSetSy: RenderFrameset(Owner, ParentBox, Element);
    BodySy: RenderBody(Owner, ParentBox, Element);
  else
    RenderBox(Owner, ParentBox, Element);
  end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.RenderBody(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);

  function CreateBox(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement): THtmlBodyBox;
  var
    Control: THtmlBodyControl;
  begin
    // get or create the body control
    Control := GetControl(Element) as THtmlBodyControl;
    if Control = nil then
    begin
      Control := THtmlBodyControl.Create(Owner);
      ControlOfElementMap.Put(Element, Control);
    end;

    // create the body box
    Result := THtmlBodyBox.Create(ParentBox, Control);
    Result.BoundsRect := ParentBox.BoundsRect;
    Result.Tiled := True;
  end;

var
  Box: THtmlBodyBox;
  Properties: TResultingPropertyMap;
  Info: THtmlElementBoxingInfo;
begin
  if ParentBox = nil then
    raise EHtmlRendererException.Create('RenderBody() requires a parent box <> nil');
  Properties := GetResultingProperties(Element);
  try
    Info.Display := pdBlock;
    Info.Position := posStatic;
    Info.Float := flNone;
    Properties.GetElementBoxingInfo(Info);
    Info.Display := ToRootDisplayStyle(Info.Display);
    if Info.Display = pdNone then
      exit;

    // create the body box
    Box := CreateBox(Owner, ParentBox, Element);
    SetPropertiesToBox(Element, Box, Properties, Info);

    // create children
    RenderChildren(Box.Control, Box, Element);

    // pass 2: convert the boxes, if necessary
    // - A block container box either contains only block-level boxes or only inline-level boxes.
    // - An inline container ...
  finally
    Properties.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 29.08.2011 --
procedure THtmlVisualRenderer.RenderBox(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
var
  Box: THtmlElementBox;
  Properties: TResultingPropertyMap;
  Info: THtmlElementBoxingInfo;
begin
  if ParentBox = nil then
    raise EHtmlRendererException.Create('RenderBox() requires a parent box <> nil');
  Properties := GetResultingProperties(Element);
  try
    Info.Display := ParentBox.Display;
    Info.Position := ParentBox.Position;
    Info.Float := ParentBox.Float;
    Properties.GetElementBoxingInfo(Info);
    case Info.Display of
      // block level elements
      pdBlock:
        Box := THtmlElementBox.Create(ParentBox);
//      pdListItem: ;
//      pdTable: ;

      // inline level elements
      pdInline:
        Box := THtmlElementBox.Create(ParentBox);
//      pdInlineBlock: ;
//      pdInlineTable: ;

//      pdRunIn:
//        // http://www.w3.org/TR/css3-box/#run-in-boxes:
//        // If the run-in box contains a block box, the run-in box becomes a block box.
//        // If a sibling block box (that does not float and is not absolutely positioned) follows
//        //  the run-in box, the run-in box becomes the first inline box of the block box. A run-in
//        //  cannot run in to a block that already starts with a run-in or that itself is a run-in.
//        // Otherwise, the run-in box becomes a block box.
//        // TODO -oBG, 04.09.2011: pdRunIn --> pdBlock or pdInline
//        ;
//
//      pdTableRowGroup: ;
//      pdTableHeaderGroup: ;
//      pdTableFooterGroup: ;
//      pdTableRow: ;
//      pdTableColumnGroup: ;
//      pdTableColumn: ;
//      pdTableCell: ;
//      pdTableCaption: ;
//
      pdUnassigned:
        raise EHtmlRendererException.Create('No "Display" assigned.');

      pdNone:
        exit;
    else
      //raise EHtmlRendererException.Create('"Display:' + CDisplayStyle[Info.Display] + '" not yet supported.');
      Box := THtmlElementBox.Create(ParentBox);
    end;

    // set properties
    SetPropertiesToBox(Element, Box, Properties, Info);

    // create children
    RenderChildren(Owner, Box, Element);
  finally
    Properties.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2011 --
procedure THtmlVisualRenderer.RenderChildren(Owner: TWinControl; Box: THtmlBox; Element: THtmlElement);
{
 Render the children of the given box/element.
 Owner is the owner control of the box.
 Box is the box rendered from Element.
 Element is the element, whose children to be rendered.
}
var
  Child: THtmlElement;
begin
  Child := Element.FirstChild;
  while Child <> nil do
  begin
    Render(Owner, Box, Child);
    Child := Child.Next;
  end;
  Box.SortChildren;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.RenderFrameset(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
begin

end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
procedure THtmlVisualRenderer.SetPropertiesToBox(
  Element: THtmlElement;
  Box: THtmlBox;
  Properties: TResultingPropertyMap;
  const Info: THtmlElementBoxingInfo);

  procedure GetStaticBounds(psWidth: TPropertySymbol; ParentWidth, EmBase: Double; out Left, Right: Integer);
  begin
    Left := 0;
    if Properties[psWidth].HasValue then
      Right := Left + Trunc(Properties[psWidth].GetLength(0.0, ParentWidth, EmBase, 0.0))
    else
      Right := Left;
  end;

  procedure GetAbsoluteBounds(psLeft, psRight, psWidth: TPropertySymbol; ParentWidth, EmBase: Double; out Left, Right: Integer);
  begin
    // Prefer left over right
    if Properties[psLeft].HasValue then
    begin
      Left := Trunc(Properties[psLeft].GetLength(0.0, ParentWidth, EmBase, 0.0));
      // prefer width over right
      if Properties[psWidth].HasValue then
        Right := Left + Trunc(Properties[psWidth].GetLength(0.0, ParentWidth, EmBase, 0.0))
      else if Properties[psRight].HasValue then
        // right is relative to parent width
        Right := Trunc(ParentWidth - Properties[psRight].GetLength(0.0, ParentWidth, EmBase, 0.0))
      else
        Right := Left;
    end
    else if Properties[psRight].HasValue then
    begin
      Right := Trunc(ParentWidth - Properties[psRight].GetLength(0.0, ParentWidth, EmBase, 0.0));
      if Properties[psWidth].HasValue then
        Left := Right - Trunc(Properties[psWidth].GetLength(0.0, ParentWidth, EmBase, 0.0))
      else
        Left := Right;
    end
    else
    begin
      Left := 0;
      if Properties[psWidth].HasValue then
        Right := Left + Trunc(Properties[psWidth].GetLength(0.0, ParentWidth, EmBase, 0.0))
      else
        Right := Left;
    end;
  end;

  procedure GetRelativeOffset(psLeft, psRight: TPropertySymbol; ParentWidth, EmBase: Double; out Left, Right: Integer);
  begin
    if Properties[psLeft].HasValue then
      Left := Trunc(Properties[psLeft].GetLength(0.0, ParentWidth, EmBase, 0.0))
    else
      Left := 0;

    if Properties[psRight].HasValue then
      Right := Trunc(Properties[psRight].GetLength(0.0, ParentWidth, EmBase, 0.0))
    else
      Right := 0;
  end;

var
  ParentBox: THtmlBox;
  FontInfo: ThtFontInfo;
  EmBase: Integer;
  Font: ThtFont;
  ParentWidth, ParentHeight: Integer;
  Rect: TRect;
begin
  ParentBox := Box.Parent;
  while ParentBox is THtmlAnonymousBox do
    ParentBox := ParentBox.Parent;
  if ParentBox = nil then
    ParentBox := DefaultBox;

  // Font
  GetFontInfo(Properties, FontInfo, FDefaultFontInfo, FDefaultFontInfo);
  Font := AllMyFonts.GetFontLike(FontInfo);
  Box.Font := Font;
  EmBase := Font.EmSize;
  freeAndNil(Font);

  Box.Color         := Properties[BackgroundColor].GetColor(ParentBox.Color, clNone);
  Box.Image         := GetImage(Properties[BackgroundImage], ParentBox.Image, nil);
  Box.Margins       := Properties.GetLengths(MarginLeft, MarginTop, MarginRight, MarginBottom, ParentBox.Margins, NullIntegers, EmBase);
  Box.Paddings      := Properties.GetLengths(PaddingLeft, PaddingTop, PaddingRight, PaddingBottom, ParentBox.Paddings, NullIntegers, EmBase);
  Box.BorderWidths  := Properties.GetLengths(BorderLeftWidth, BorderTopWidth, BorderRightWidth, BorderBottomWidth, ParentBox.BorderWidths, NullIntegers, EmBase);
  Box.BorderColors  := Properties.GetColors(BorderLeftColor, BorderTopColor, BorderRightColor, BorderBottomColor, ParentBox.BorderColors, NoneColors);
  Box.BorderStyles  := Properties.GetStyles(BorderLeftStyle, BorderTopStyle, BorderRightStyle, BorderBottomStyle, ParentBox.BorderStyles, NoneStyles);

  Box.Display := Info.Display;
  Box.Float := Info.Float;
  Box.Position := Info.Position;
  if Element.Symbol <> BodySy then
    case Box.Display of
      pdUnassigned,
      pdNone:
        raise EHtmlRendererException.Create('A box having "Display:' + CDisplayStyle[Box.Display] + '" is not supported.');
    else
      ParentWidth := ParentBox.ContentRect.Right - ParentBox.ContentRect.Left;
      ParentHeight := ParentBox.ContentRect.Bottom - ParentBox.ContentRect.Top;
      case Box.Position of

        posStatic:
        begin
          GetStaticBounds(psWidth,  ParentWidth,  EmBase, Rect.Left, Rect.Right);
          GetStaticBounds(psHeight, ParentHeight, EmBase, Rect.Top, Rect.Bottom);
        end;

        posRelative:
        begin
          GetStaticBounds(psWidth,  ParentWidth,  EmBase, Rect.Left, Rect.Right);
          GetStaticBounds(psHeight, ParentHeight, EmBase, Rect.Top, Rect.Bottom);
          GetRelativeOffset(psLeft, psRight, ParentWidth,  EmBase, Rect.Left, Rect.Right);
          GetRelativeOffset(psTop, psBottom, ParentHeight, EmBase, Rect.Top, Rect.Bottom);
        end;

      else
        GetAbsoluteBounds(psLeft, psRight, psWidth,  ParentWidth,  EmBase, Rect.Left, Rect.Right);
        GetAbsoluteBounds(psTop, psBottom, psHeight, ParentHeight, EmBase, Rect.Top, Rect.Bottom);
      end;
      Box.BoundsRect := Rect;
    end;
end;

//-- BG ---------------------------------------------------------- 04.09.2011 --
function THtmlVisualRenderer.SketchMap: TSketchMap;
begin
  Result := FSketchMapStack.Top;
end;

initialization
  DefaultBox := THtmlBox.Create(nil);
finalization
  DefaultBox.Free;
end.
