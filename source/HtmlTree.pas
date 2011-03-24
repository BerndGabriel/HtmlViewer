{
Version   11
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

unit HtmlTree;

interface

uses
  HtmlGlobals,
  HtmlStyles,
  HtmlUn2;

//------------------------------------------------------------------------------
// THtmlNode is base class for all objects in the HTML document tree.
//------------------------------------------------------------------------------

type
  THtmlNode = class(TIDObject)
  private
    FTag: TSymbol;
    FIds: ThtStringArray;
    FClasses: ThtStringArray;
  protected
    function FindAttribute(NameSy: Symb; out Attribute: TAttribute): Boolean; overload; virtual;
    function FindAttribute(Name: ThtString; out Attribute: TAttribute): Boolean; overload; virtual;
    function GetChild(Index: Integer): THtmlNode; virtual;
    function GetParent: THtmlNode; virtual; abstract;
    function GetPseudos: TPseudos; virtual;
  public
    //constructor Create(Parent: THtmlNode; Tag: TSymbol; Attributes: TAttributeList; Properties: TR );
    function IndexOf(Child: THtmlNode): Integer; virtual;
    function IsMatching(Selector: TSelector): Boolean;
    property Parent: THtmlNode read GetParent;
    property Children[Index: Integer]: THtmlNode read GetChild; default;
  end;

implementation

uses
  ReadHtml; //TODO -oBG, 23.03.2011: move SymbToAttributeName() to a unit in a lower hierarchy level!

{ THtmlNode }

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlNode.FindAttribute(NameSy: Symb; out Attribute: TAttribute): Boolean;
begin
  Result := False;
  Attribute := nil;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlNode.FindAttribute(Name: ThtString; out Attribute: TAttribute): Boolean;
begin
  Result := False;
  Attribute := nil;
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlNode.GetChild(Index: Integer): THtmlNode;
begin
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlNode.GetPseudos: TPseudos;
begin
  Result := [];
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlNode.IndexOf(Child: THtmlNode): Integer;
begin
  Result := -1;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlNode.IsMatching(Selector: TSelector): Boolean;

  function IsMatchingSimple: Boolean;

    function IncludesStringArray(S, F: ThtStringArray): Boolean;
    var
      I: Integer;
    begin
      Result := Length(S) <= Length(F);
      if not Result then
        exit;
      for I := Low(S) to High(S) do
        if IndexOfString(F, S[I]) < 0 then
          exit;
      Result := True;
    end;

  var
    Index: Integer;
    Attribute: TAttribute;
    Match: TAttributeMatch;
    S: TSymbol;
  begin
    Result := False;

    // http://www.w3.org/TR/2010/WD-CSS2-20101207/selector.html
    // If all conditions in the selector are true for a certain element, the selector matches the element.

    if Selector.Pseudos <> [] then
      if not (Selector.Pseudos >= GetPseudos) then
        exit;

    // a loop about tags? there is one or none tag in the selector.
    for Index := Low(Selector.Tags) to High(Selector.Tags) do
      if TryStrToReservedWord(Selector.Tags[Index], S) then
        if S <> FTag then
          exit;

    // a loop about ids? CSS 2.1 allows more than 1 ID, but most browsers do not support them.
    if not IncludesStringArray(Selector.Ids, FIds) then
      exit;

    if not IncludesStringArray(Selector.Classes, FClasses) then
      exit;

    for Index := 0 to Selector.AttributeMatchesCount - 1 do
    begin
      Match := Selector.AttributeMatches[Index];
      if not FindAttribute(Match.Name, Attribute) then
        exit;
      case Match.Oper of
        //no more checks here. Attribute it set! amoSet: ;       // [name] : matches, if attr is set and has any value.

        amoEquals:     // [name=value] : matches, if attr equals value.
          if htCompareString(Match.Value, Attribute.AsString) <> 0 then
            break;

        amoContains:   // [name~=value] : matches, if attr is a white space separated list of values and value is one of these values.
          if PosX(Match.Value + ' ', Attribute.AsString + ' ', 1) = 0 then
            break;

        amoStartsWith: // [name|=value] : matches, if attr equals value or starts with value immediately followed by a hyphen.
          if PosX(Match.Value + '-', Attribute.AsString + '-', 1) <> 1 then
            break;
        end;
      end;

    Result := True;
  end;

  function IsChild(Selector: TSelector): Boolean;
  var
    P: THtmlNode;
  begin
    P := Parent;
    Result := (P <> nil) and P.IsMatching(Selector);
  end;

  function IsDescendant(Selector: TSelector): Boolean;
  var
    Node: THtmlNode;
  begin
    Result := False;
    Node := Parent;
    while Node <> nil do
    begin
      Result := Node.IsMatching(Selector);
      if Result then
        break;
      Node := Node.Parent;
    end;
  end;

  function IsFollower(Selector: TSelector): Boolean;
  var
    P: THtmlNode;
    I: Integer;
  begin
    P := Parent;
    Result := P <> nil;
    if Result then
    begin
      I := P.IndexOf(Self);
      if I > 0 then
        Result := P[I - 1].IsMatching(Selector);
    end;
  end;

begin
  Result := IsMatchingSimple;
  if Result then
    if Selector is TCombinedSelector then
      case TCombinedSelector(Selector).Combinator of
        scChild:      Result := IsChild(TCombinedSelector(Selector).LeftHand);
        scDescendant: Result := IsDescendant(TCombinedSelector(Selector).LeftHand);
        scFollower:   Result := IsFollower(TCombinedSelector(Selector).LeftHand);
      end;
end;

end.
