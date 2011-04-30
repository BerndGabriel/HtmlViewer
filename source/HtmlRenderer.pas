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
  Classes, Controls, Contnrs,
  BegaClasses,
  HtmlBoxes,
  HtmlDocument,
  HtmlElements,
  HtmlStyles,
  HtmlSymbols,
  StyleTypes;

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
    function Remove(Key: THtmlElement): TControl; {$ifdef UseInline} inline; {$endif}
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

  THtmlRenderedDocument = class
  end;

  THtmlVisualizedDocument = class(THtmlRenderedDocument)
  private
    FView: THtmlBox;
  public
    constructor Create(View: THtmlBox);
  end;

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
    FWidth: Integer;          // width of destination in pixels
    FHeight: Integer;         // height of destination in pixels
    function GetResultingProperties(Element: THtmlElement): TResultingPropertyMap;
  public
    constructor Create(Document: THtmlDocument; MediaType: TMediaType; Width, Height: Integer);
    property MediaCapabilities: TMediaCapabilities read FCapabilities write FCapabilities;
    property MediaType: TMediaType read FMediaType;
  end;

  THtmlVisualRenderer = class(THtmlRenderer)
  private
    FControls: THtmlControlOfElementMap;
    function GetControl(Element: THtmlElement): TControl;
    procedure SetControls(const Value: THtmlControlOfElementMap);
    procedure Render(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); overload;
  protected
    procedure RenderFrameset(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
    procedure RenderBody(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement); virtual;
  public
    constructor Create(Document: THtmlDocument; ControlMap: THtmlControlOfElementMap; MediaType: TMediaType; Width, Height: Integer);
    procedure Render(Owner: TWinControl; ParentBox: THtmlBox); overload;
    property ControlOfElementMap: THtmlControlOfElementMap read FControls write SetControls;
  end;


implementation

{ THtmlControlOfElementMap }

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

//-- BG ---------------------------------------------------------- 18.04.2011 --
function THtmlControlOfElementMap.Remove(Key: THtmlElement): TControl;
begin
  Result := inherited Remove(Key);
end;

{ THtmlRenderer }

constructor THtmlRenderer.Create(Document: THtmlDocument; MediaType: TMediaType; Width, Height: Integer);
begin
  inherited Create;
  FDocument := Document;
  FMediaType := MediaType;
  FWidth := Width;
  FHeight := Height;
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

{ THtmlVisualizedDocument }

//-- BG ---------------------------------------------------------- 25.04.2011 --
constructor THtmlVisualizedDocument.Create(View: THtmlBox);
begin
  FView := View;
end;

{ THtmlVisualRenderer }

//-- BG ---------------------------------------------------------- 25.04.2011 --
constructor THtmlVisualRenderer.Create(Document: THtmlDocument;
  ControlMap: THtmlControlOfElementMap; MediaType: TMediaType; Width, Height: Integer);
begin
  inherited Create(Document, MediaType, Width, Height);
  FCapabilities := [mcFrames];
  FControls := ControlMap;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
function THtmlVisualRenderer.GetControl(Element: THtmlElement): TControl;
begin
  if FControls <> nil then
    Result := FControls.Get(Element)
  else
    Result := nil;
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
  else
  end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.RenderBody(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
var
  Box: THtmlBodyBox;
  Control: THtmlBodyControl;
  Properties: TResultingPropertyMap;
begin
  Control := ControlOfElementMap.Get(Element) as THtmlBodyControl;
  if Control = nil then
  begin
    Control := THtmlBodyControl.Create(Owner);
    ControlOfElementMap.Put(Element, Control);
  end;
  Box := THtmlBodyBox.Create(ParentBox, Control);
  Properties := GetResultingProperties(Element);
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlVisualRenderer.RenderFrameset(Owner: TWinControl; ParentBox: THtmlBox; Element: THtmlElement);
begin

end;

//-- BG ---------------------------------------------------------- 18.04.2011 --
procedure THtmlVisualRenderer.SetControls(const Value: THtmlControlOfElementMap);
begin
  if FControls <> Value then
    FControls.Assign(Value);
end;

end.
