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

unit HtmlDocument;

interface

uses
  Windows, Classes, Graphics,
  //
  HtmlElements,
  HtmlGlobals,
  HtmlStyles;

type

//------------------------------------------------------------------------------
// THtmlDocument holds the complete element tree of an HTML document.
//------------------------------------------------------------------------------

  THtmlDocument = class
  private
    FRuleSets: TRulesetList;
    FTitle: ThtString;
    FName: ThtString;
    FTree: THtmlElement;
    FBaseUrl: ThtString;
    procedure setTree(const Value: THtmlElement);
  public
    constructor Create;
    destructor Destroy; override;
    property BaseUrl: ThtString read FBaseUrl write FBaseUrl;
    property RuleSets: TRulesetList read FRuleSets;
    property Title: ThtString read FTitle write FTitle;
    property Tree: THtmlElement read FTree write setTree;
    property Name: ThtString read FName write FName;
  end;

implementation

{ THtmlDocument }

//-- BG ---------------------------------------------------------- 16.04.2011 --
constructor THtmlDocument.Create;
begin
  inherited Create;
  FRuleSets := TRulesetList.Create;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
destructor THtmlDocument.Destroy;
begin
  FTree.Free;
  FRulesets.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 29.03.2011 --
procedure THtmlDocument.setTree(const Value: THtmlElement);
begin
  if FTree <> Value then
  begin
    FTree.Free;
    FTree := Value;
  end;
end;

end.
