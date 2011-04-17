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
  HtmlTree,
  HtmlDocument,
  HtmlBoxes,
  HtmlStyles,
  StyleTypes;

type

  THtmlRenderedDocument = class
  private
    FBoxes: THtmlBoxList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  THtmlRenderer = class
  private
    FDocument: THtmlDocument; // the source to render to destination
    FMediaType: TMediaType;   // media type of destination
    FWidth: Integer;          // width of destination in pixels
    FHeight: Integer;          // height of destination in pixels
    function getResultingProperties(Element: THtmlElement): TResultingPropertyMap;
  public
    constructor Create(Document: THtmlDocument; MediaType: TMediaType; Width, Height: Integer);
    function Rendered: THtmlRenderedDocument;
  end;

implementation

{ THtmlRenderer }

//-- BG ---------------------------------------------------------- 16.04.2011 --
constructor THtmlRenderer.Create(Document: THtmlDocument; MediaType: TMediaType; Width, Height: Integer);
begin
  inherited Create;
  FDocument := Document;
  FMediaType := MediaType;
  FWidth := Width;
  FHeight := Height;
end;

//-- BG ---------------------------------------------------------- 17.04.2011 --
function THtmlRenderer.getResultingProperties(Element: THtmlElement): TResultingPropertyMap;
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

//-- BG ---------------------------------------------------------- 17.04.2011 --
function THtmlRenderer.Rendered: THtmlRenderedDocument;
begin
  Result := THtmlRenderedDocument.Create;

end;

{ THtmlRenderedDocument }

constructor THtmlRenderedDocument.Create;
begin
  inherited Create;
{$ifdef UseEnhancedRecord}
{$else}
  FBoxes := THtmlBoxList.Create;
{$endif}
end;

destructor THtmlRenderedDocument.Destroy;
begin
{$ifdef UseEnhancedRecord}
{$else}
  FBoxes.Free;
{$endif}
  inherited;
end;

end.
