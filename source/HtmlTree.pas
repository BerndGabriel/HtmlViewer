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

unit HtmlTree;

interface

uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  Contnrs,
  //
  HtmlGlobals,
  HtmlStyles,
  HtmlSymbols;

//------------------------------------------------------------------------------
// THtmlAttribute is class for all attributes of all HTML elements
//------------------------------------------------------------------------------

type
  THtmlAttribute = class
  public
    FSymbol: THtmlAttributeSymbol;
    FName: ThtString;
    FValue: ThtString;
    constructor Create(Symbol: THtmlAttributeSymbol; const Name, Value: ThtString);
    property Symbol: THtmlAttributeSymbol read FSymbol;
    property AsString: ThtString read FValue;
  end;

  THtmlAttributeList = class(TObjectList) {a list of tag attributes,(TAttributes)}
  private
    function GetAttribute(Index: Integer): THtmlAttribute; {$ifdef UseInline} inline; {$endif}
  public
    function Find(Name: ThtString; out Attribute: THtmlAttribute): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    function Find(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    property Items[Index: Integer]: THtmlAttribute read GetAttribute; default;
  end;

//------------------------------------------------------------------------------
// THtmlElement is base class for all elements in the HTML document tree.
//------------------------------------------------------------------------------

type
  THtmlElement = class
  private
    FParent: THtmlElement;
    FChildren: TObjectList;
    FSymbol: THtmlElementSymbol;
    FAttributes: THtmlAttributeList;

    FIds: ThtStringArray;
    FClasses: ThtStringArray;
  protected
    function FindAttribute(Name: ThtString; out Attribute: THtmlAttribute): Boolean; overload; virtual;
    function FindAttribute(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean; overload; virtual;
    function GetChild(Index: Integer): THtmlElement;
    function GetPseudos: TPseudos; virtual;
    procedure AddChild(Child: THtmlElement);
    procedure ExtractChild(Child: THtmlElement);
    procedure SetParent(Parent: THtmlElement);
  public
    constructor Create(Parent: THtmlElement; Symbol: THtmlElementSymbol; Attributes: THtmlAttributeList);
    destructor Destroy; override;
    function IndexOf(Child: THtmlElement): Integer; virtual;
    function IsMatching(Selector: TSelector): Boolean;
    property Children[Index: Integer]: THtmlElement read GetChild; default;
    property Parent: THtmlElement read FParent;
  end;

implementation

{ THtmlAttribute }

//-- BG ---------------------------------------------------------- 26.03.2011 --
constructor THtmlAttribute.Create(Symbol: THtmlAttributeSymbol; const Name, Value: ThtString);
begin
  inherited Create;
  FSymbol := Symbol;
  FName := Name;
  FValue := Value;
end;

{ THtmlAttributeList }

//-- BG ---------------------------------------------------------- 26.03.2011 --
function THtmlAttributeList.Find(Name: ThtString; out Attribute: THtmlAttribute): Boolean;
var
  Symbol: THtmlAttributeSymbol;
begin
  if TryStrToAttributeSymbol(Name, Symbol) then
    Result := Find(Symbol, Attribute)
  else
  begin
    Attribute := nil;
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function THtmlAttributeList.Find(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Attribute := GetAttribute(I);
    Result := Attribute.Symbol = Symbol;
    if Result then
      exit;
  end;
  Attribute := nil;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function THtmlAttributeList.GetAttribute(Index: Integer): THtmlAttribute;
begin
  Result := Get(Index);
end;

{ THtmlElement }

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure THtmlElement.AddChild(Child: THtmlElement);
begin
  FChildren.Add(Child);
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
constructor THtmlElement.Create(Parent: THtmlElement; Symbol: THtmlElementSymbol; Attributes: THtmlAttributeList);
begin
  inherited Create;
  FSymbol := Symbol;
  FAttributes := Attributes;
  SetParent(Parent);
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
destructor THtmlElement.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure THtmlElement.ExtractChild(Child: THtmlElement);
begin
  FChildren.Extract(Child);
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.FindAttribute(Name: ThtString; out Attribute: THtmlAttribute): Boolean;
begin
  if FAttributes <> nil then
    Result := FAttributes.Find(Name, Attribute)
  else
  begin
    Attribute := nil;
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.FindAttribute(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean;
begin
  if FAttributes <> nil then
    Result := FAttributes.Find(Symbol, Attribute)
  else
  begin
    Attribute := nil;
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlElement.GetChild(Index: Integer): THtmlElement;
begin
  Result := THtmlElement(FChildren[Index]);
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.GetPseudos: TPseudos;
begin
  Result := [];
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlElement.IndexOf(Child: THtmlElement): Integer;
begin
  Result := FChildren.IndexOf(Child);
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.IsMatching(Selector: TSelector): Boolean;

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
    Attribute: THtmlAttribute;
    Match: TAttributeMatch;
    S: THtmlElementSymbol;
  begin
    Result := False;

    // http://www.w3.org/TR/2010/WD-CSS2-20101207/selector.html
    // If all conditions in the selector are true for a certain element, the selector matches the element.

    if Selector.Pseudos <> [] then
      if not (Selector.Pseudos >= GetPseudos) then
        exit;

    // a loop about tags? there is one or none tag in the selector.
    for Index := Low(Selector.Tags) to High(Selector.Tags) do
      if TryStrToElementSymbol(Selector.Tags[Index], S) then
        if S <> FSymbol then
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
    P: THtmlElement;
  begin
    P := Parent;
    Result := (P <> nil) and P.IsMatching(Selector);
  end;

  function IsDescendant(Selector: TSelector): Boolean;
  var
    Node: THtmlElement;
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
    P: THtmlElement;
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

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure THtmlElement.SetParent(Parent: THtmlElement);
begin
  if FParent <> Parent then
  begin
    if FParent <> nil then
      FParent.ExtractChild(Self);
    FParent := Parent;
    if FParent <> nil then
      FParent.AddChild(Self);
  end;
end;

end.
