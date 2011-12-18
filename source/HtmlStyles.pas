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

unit HtmlStyles;

interface

uses
  Windows, Classes, Contnrs, Variants, SysUtils, Graphics,
  //
  HtmlGlobals,
  HtmlSymbols,
  Parser,
  StyleTypes;

//------------------------------------------------------------------------------
// style properties
//------------------------------------------------------------------------------

type
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/cascade.html#cascade
  TPropertyOrigin = (
    poDefault,// default value
    poAgent,  // user agent
    poUser,   // user preferences
    poStyle,  // author by style attribute
    poAuthor  // author resp. document
  );

  // http://www.w3.org/TR/2010/WD-CSS2-20101207/cascade.html#cascade
  // ascending precedence:
  TPropertyPrecedence = (
    ppDefault,          // default value of object type
    ppInherited,        // inherited from parent object
    ppAgent,            // default property by user agent
    ppUserNormal,       // normal user property preference
    ppHtmlAttribute,    // by html attribute
    ppStyleNormal,      // normal author resp. document property by style attribute
    ppAuthorNormal,     // normal author resp. document property
    ppAuthorImportant,  // important author propery
    ppStyleImportant,   // important author resp. document property by style attribute
    ppUserImportant     // important user property preference
  );

const
  CPropertyPrecedence: array [TPropertyPrecedence] of ThtString = (
    'Default',          // default value of object type
    'Inherited',        // inherited from parent object
    'Agent',            // default property by user agent
    'User Normal',      // normal user property preference
    'Html Attribute',   // by html attribute
    'Style Normal',     // normal author resp. document property by style attribute
    'Author Normal',    // normal author resp. document property
    'Author Important', // important author propery
    'Style Important',  // important author resp. document property by style attribute
    'User Important'    // important user property preference
  );

  //
  CPropertyPrecedenceOfOrigin: array [Boolean, TPropertyOrigin] of TPropertyPrecedence = (
    (ppdefault, ppAgent, ppUserNormal, ppStyleNormal, ppAuthorNormal),
    (ppdefault, ppAgent, ppUserImportant, ppStyleImportant, ppAuthorImportant)
  );

// custom variant types range from $110 (272) to $7FF (2047)
const
  varInherit = $123; // a value of this variant type indicates, that this value is to be inherited from parent object.

function Inherit: Variant; //{$ifdef UseInline} inline; {$endif}
function VarIsInherit(const Value: Variant): Boolean; //{$ifdef UseInline} inline; {$endif}

type
  TStyleProperty = class
  private
    FPrev: TStyleProperty;
    FNext: TStyleProperty;
    FSymbol: TStylePropertySymbol;
    FPrecedence: TPropertyPrecedence;
    FValue: Variant;
  public
    constructor Create(Symbol: TStylePropertySymbol; Precedence: TPropertyPrecedence; const Value: Variant); overload;
    function ToString: ThtString; {$ifdef UseToStringOverride} override; {$endif}
    destructor Destroy; override;
    property Symbol: TStylePropertySymbol read FSymbol;
    property Precedence: TPropertyPrecedence read FPrecedence;
    property Value: Variant read FValue write FValue;
    property Next: TStyleProperty read FNext;
    property Prev: TStyleProperty read FPrev;
  end;


  TStylePropertyList = {$ifdef UseEnhancedRecord} record {$else} class {$endif}
  private
    FFirst: TStyleProperty;
    FLast: TStyleProperty;
    function GetItem(Index: TStylePropertySymbol): TStyleProperty;
  public
    function IsEmpty: Boolean;
    function ToString: ThtString; {$ifdef UseEnhancedRecord} {$else} {$ifdef UseToStringOverride} override; {$endif} {$endif}
    procedure Add(Prop: TStyleProperty);
    procedure Assign(const List: TStylePropertyList);
    procedure Clear;
{$ifdef UseEnhancedRecord}
    procedure Free;
{$endif}
    procedure Init;
    procedure Remove(Prop: TStyleProperty);
    property First: TStyleProperty read FFirst;
    property Last: TStyleProperty read FLast;
    property Items[Index: TStylePropertySymbol]: TStyleProperty read GetItem; default;
  end;

//------------------------------------------------------------------------------
// style selectors
//------------------------------------------------------------------------------

type

  TAttributeMatchOperator = (
    amoSet,        // [name] : matches, if attr is set and has any value.
    amoEquals,     // [name=value] : matches, if attr equals value.
    amoContains,   // [name~=value] : matches, if attr is a white space separated list of values and value is one of these values.
    amoStartsWith  // [name|=value] : matches, if attr equals value or starts with value immediately followed by a hyphen.
  );

const
  CAttributeMatchOperator: array [TAttributeMatchOperator] of string = (
    '',     // [name] : matches, if attr is set and has any value.
    '=',    // [name=value] : matches, if attr equals value.
    '~=',   // [name~=value] : matches, if attr is a white space separated list of values and value is one of these values.
    '|='    // [name|=value] : matches, if attr equals value or starts with value immediately followed by a hyphen.
  );

type
  TAttributeMatch = class
  private
    FAttr: THtmlAttributeSymbol;
    FOper: TAttributeMatchOperator;
    FValue: ThtString;
  public
    constructor Create(Attribute: THtmlAttributeSymbol; Oper: TAttributeMatchOperator; const Value: ThtString);
    function ToString: ThtString; {$ifdef UseToStringOverride} override; {$endif}
    property Attribute: THtmlAttributeSymbol read FAttr;
    property Oper: TAttributeMatchOperator read FOper;
    property Value: ThtString read FValue;
  end;

  TAttributeMatchList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAttributeMatch;
  public
    function Same(List: TAttributeMatchList): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    function ToString: ThtString; {$ifdef UseToStringOverride} override; {$endif}
    procedure Sort; overload; {$ifdef UseInline} inline; {$endif}
    property Items[Index: Integer]: TAttributeMatch read GetItem; default;
  end;

function CompareAttributeMatches(P1, P2: Pointer): Integer;
function TryStrToAttributeMatchOperator(Str: ThtString; out Oper: TAttributeMatchOperator): Boolean;

type
  TPseudo = (
    pcNone,

    // pseudo classes
    pcLink,
    pcVisited,
    pcActive,
    pcHover,
    pcFocus,
    pcFirstChild,
    //TODO: pcLanguage,  requires additional data

    // pseudo elements
    peFirstLine,
    peFirstLetter,
    peBefore,
    peAfter
  );
  TPseudos = set of TPseudo;
  TPseudoClass = pcLink..pcFirstChild;
  TPseudoElement = peFirstLine..peAfter;

const
  CPseudo: array[TPseudo] of ThtString = (
    // pseudo classes
    '',
    'link',
    'visited',
    'active',
    'hover',
    'focus',
    'first-child',
    //TODO: 'language(...)'

    // pseudo elements
    'first-line',
    'first-letter',
    'before',
    'after');

  PseudoClasses = [
    pcLink,
    pcVisited,
    pcActive,
    pcHover,
    pcFocus,
    pcFirstChild
    //TODO: pcLanguage,  requires additional data
  ];

  PseudoElements = [
    peFirstLine,
    peFirstLetter,
    peBefore,
    peAfter
  ];

function TryStrToPseudo(const Str: ThtString; out Pseudo: TPseudo): Boolean;

type

  TStyleCombinator = (
    scNone,       // F      : matches any F
    scDescendant, // E F    : matches F, if it is descendant of E
    scChild,      // E > F  : matches F, if it is child of E
    scFollower    // E + F  : matches F, if it immediately follows sibling E
  );

  TStyleSelectorState = set of (
    ssSorted
  );

  // a simple selector
  TStyleSelector = class
  private
    FPrev: TStyleSelector;
    FNext: TStyleSelector;
    FTags: ThtStringArray;
    FClasses: ThtStringArray;
    FIds: ThtStringArray;
    FPseudos: TPseudos;
    FAttributeMatches: TAttributeMatchList;
    FIsFromStyleAttr: Boolean;
    FSelectorState: TStyleSelectorState;
    FNumberOfElements: Integer;
    FNumberOfNonIds: Integer;
    function NumberOfIDs: Integer; {$ifdef UseInline} inline; {$endif}
    procedure Sort; {$ifdef UseInline} inline; {$endif}
    property NumberOfElements: Integer read FNumberOfElements;
    property NumberOfNonIDs: Integer read FNumberOfNonIds;
  public
    destructor Destroy; override;
    function AttributeMatchesCount: Integer; {$ifdef UseInline} inline; {$endif}
    function Same(Selector: TStyleSelector): Boolean; virtual;
    function ToString: ThtString; {$ifdef UseToStringOverride} override; {$else} virtual; {$endif}
    function CompareSpecificity(ASelector: TStyleSelector): Integer;
    procedure AddAttributeMatch(AAttributeMatch: TAttributeMatch); {$ifdef UseInline} inline; {$endif}
    procedure AddClass(AClass: ThtString); {$ifdef UseInline} inline; {$endif}
    procedure AddId(AId: ThtString); {$ifdef UseInline} inline; {$endif}
    procedure AddPseudo(APseudo: TPseudo); {$ifdef UseInline} inline; {$endif}
    procedure AddTag(ATag: ThtString);
    property Tags: ThtStringArray read FTags;
    property Classes: ThtStringArray read FClasses;
    property Ids: ThtStringArray read FIds;
    property Pseudos: TPseudos read FPseudos;
    property AttributeMatches: TAttributeMatchList read FAttributeMatches;
    property Next: TStyleSelector read FNext;
    property Prev: TStyleSelector read FPrev;
  end;

  // a compound selector
  TCombinedSelector = class(TStyleSelector)
  private
    FLeftHand: TStyleSelector;
    FCombinator: TStyleCombinator;
  public
    constructor Create(LeftHand: TStyleSelector; Combinator: TStyleCombinator);
    destructor Destroy; override;
    function ToString: ThtString; override;
    property Combinator: TStyleCombinator read FCombinator;
    property LeftHand: TStyleSelector read FLeftHand;
  end;

  TStyleSelectorList = {$ifdef UseEnhancedRecord} record {$else} class {$endif}
  private
    FFirst: TStyleSelector;
    FLast: TStyleSelector;
  public
    function IsEmpty: Boolean;
    function ToString: ThtString; {$ifdef UseEnhancedRecord} {$else} {$ifdef UseToStringOverride} override; {$endif} {$endif}
    procedure Init;
    procedure Add(Sele: TStyleSelector);
    procedure Assign(const List: TStyleSelectorList);
    procedure Clear;
{$ifdef UseEnhancedRecord}
    procedure Free;
{$endif}
    procedure Remove(Prop: TStyleSelector);
    property First: TStyleSelector read FFirst;
    property Last: TStyleSelector read FLast;
  end;

//------------------------------------------------------------------------------
// style rule sets
//------------------------------------------------------------------------------

type

  TRuleset = class
  private
    FMediaTypes: TMediaTypes;
  public
    Properties: TStylePropertyList;
    Selectors: TStyleSelectorList;
    constructor Create(MediaTypes: TMediaTypes);
    destructor Destroy; override;
    function ToString: ThtString; {$ifdef UseToStringOverride} override; {$endif}
    property MediaTypes: TMediaTypes read FMediaTypes;
  end;

  TRulesetList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRuleset;
  public
    function ToString: ThtString; {$ifdef UseToStringOverride} override; {$endif}
    procedure InsertList(Index: Integer; Rulesets: TRulesetList);
    property Items[Index: Integer]: TRuleset read GetItem; default;
  end;

//------------------------------------------------------------------------------
// resulting properties
//------------------------------------------------------------------------------
// A temporary collection of properties while computing the effective values.
//------------------------------------------------------------------------------

  TResultingProperty = class
  // references data only, does not own it.
  private
    FProperty: TStyleProperty;
    FSelector: TStyleSelector;
  public
    function GetBorderStyle(Parent, Default: TBorderStyle): TBorderStyle;
    function GetColor(Parent, Default: TColor): TColor;
    function GetDisplay(Parent, Default: TDisplayStyle): TDisplayStyle;
    function GetFloat(Parent, Default: TBoxFloatStyle): TBoxFloatStyle;
    function GetFontName(Parent, Default: ThtString): ThtString;
    function GetFontSize(DefaultFontSize, Parent, Default: Double): Double;
    function GetFontWeight(Parent, Default: Integer): Integer;
    function GetLength(Parent, Base, EmBase, Default: Double): Double;
    function GetPosition(Parent, Default: TBoxPositionStyle): TBoxPositionStyle;
    function GetTextDecoration(Parent, Default: TFontStyles): TFontStyles;
    function GetValue(Parent, Default: Variant): Variant;
    function HasValue: Boolean;
    function IsItalic(Parent, Default: Boolean): Boolean;
    //
    function Compare(const AProperty: TStyleProperty; const ASelector: TStyleSelector): Integer; {$ifdef UseInline} inline; {$endif}
    procedure Update(const AProperty: TStyleProperty; const ASelector: TStyleSelector); {$ifdef UseInline} inline; {$endif}
    property Prop: TStyleProperty read FProperty;
    property Sele: TStyleSelector read FSelector;
  end;

  TResultingProperties = array [TStylePropertySymbol] of TResultingProperty;

  THtmlElementBoxingInfo = record
    Display: TDisplayStyle;
    Position: TBoxPositionStyle;
    Float: TBoxFloatStyle;
  end;

  // How to compute the resulting properties:
  // 1) Fill map with properties from style attribute (highest prio) using UpdateFromStyleAttribute(),
  // 2) Walk through ruleset list in reverse order. If a selector of the ruleset matches,
  //    then call UpdateFromProperties() with the ruleset's properties and if the combination has a
  //    higher cascading order, this becomes the new resulting value.
  // 3) Fill up missing properties with other tag attributes (like color, width, height, ...).
  //
  // Using reverse order minimizes comparing and update effort.
  TResultingPropertyMap = class
  private
    FProps: TResultingProperties; // the resulting properties
    procedure Update(const AProperty: TStyleProperty; const ASelector: TStyleSelector);
  public
    destructor Destroy; override;
    function Get(Index: TStylePropertySymbol): TResultingProperty; {$ifdef UseInline} inline; {$endif}
    function GetColors(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectColors): TRectColors;
    function GetLengths(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectIntegers; BaseSize: Integer): TRectIntegers;
    function GetStyles(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectStyles): TRectStyles;
    procedure GetElementBoxingInfo(var Info: THtmlElementBoxingInfo);
    procedure UpdateFromAttributes(const Properties: TStylePropertyList; IsFromStyleAttr: Boolean); {$ifdef UseInline} inline; {$endif}
    procedure UpdateFromProperties(const Properties: TStylePropertyList; const ASelector: TStyleSelector); {$ifdef UseInline} inline; {$endif}
    property Properties[Index: TStylePropertySymbol]: TResultingProperty read Get; default;
  end;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

function GetCssDefaults: TStream;

implementation

var
  StyleAttributeSelector: TStyleSelector;
  StyleTagSelector: TStyleSelector;

{$R css_defaults.res}

//-- BG ---------------------------------------------------------- 20.03.2011 --
function GetCssDefaults: TStream;
begin
  Result := TResourceStream.Create(hInstance, 'CSS_DEFAULTS', RT_RCDATA);
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function TryStrToPseudo(const Str: ThtString; out Pseudo: TPseudo): Boolean;
var
  I: TPseudo;
begin
  for I := low(I) to high(I) do
    if CPseudo[I] = Str then
    begin
      Result := True;
      Pseudo := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 21.03.2011 --
function Inherit: Variant;
// returns a variant of type varInherit
begin
  TVarData(Result).VType := varEmpty; //varInherit;
end;

//-- BG ---------------------------------------------------------- 21.03.2011 --
function VarIsInherit(const Value: Variant): Boolean;
// returns true, if Value is of type varInherit
begin
  Result := VarType(Value) = varEmpty; //varInherit;
end;


{ TStyleProperty }

//-- BG ---------------------------------------------------------- 13.03.2011 --
constructor TStyleProperty.Create(Symbol: TStylePropertySymbol; Precedence: TPropertyPrecedence; const Value: Variant);
begin
  inherited Create;
  FSymbol := Symbol;
  FPrecedence := Precedence;
  FValue := Value;
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
destructor TStyleProperty.Destroy;
begin
  if Prev <> nil then
    Prev.FNext := Next;
  if Next <> nil then
    Next.FPrev := Prev;
  inherited;
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TStyleProperty.ToString: ThtString;

  function VariantToStr(Value: Variant): ThtString;
  begin
    if VarIsNull(Value) then
      Result := ''
    else if VarIsStr(Value) then
      Result := '"' + Value + '"'
    else if VarIsInherit(Value) then
      Result := '"inherit"'
    else
      Result := Value;
  end;

begin
  Result :=
    PropertySymbolToStr(FSymbol) + ': ' + VariantToStr(Value) +
    ' /* ' + CPropertyPrecedence[FPrecedence] + ' */';
end;

{ TStylePropertyList }

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStylePropertyList.Add(Prop: TStyleProperty);
var
  Prev: TStyleProperty;
  Next: TStyleProperty;
begin
  assert(Prop.Next = nil, 'Don''t add chained links twice. Prop.Next is not nil.');
  assert(Prop.Prev = nil, 'Don''t add chained links twice. Prop.Prev is not nil.');

  // find neighbors
  Prev := nil;
  Next := First;
  while (Next <> nil) and (Next.Symbol < Prop.Symbol) do
  begin
    Prev := Next;
    Next := Prev.Next;
  end;

  // link to prev
  if Prev = nil then
    FFirst := Prop
  else
    Prev.FNext := Prop;
  Prop.FPrev := Prev;

  // link to next
  if Next = nil then
    FLast := Prop
  else
    Next.FPrev := Prop;
  Prop.FNext := Next;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStylePropertyList.Assign(const List: TStylePropertyList);
begin
  FFirst := List.FFirst;
  FLast := List.FLast;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure TStylePropertyList.Clear;
var
  Link: TStyleProperty;
begin
  FLast := nil;
  while First <> nil do
  begin
    Link := First;
    FFirst := Link.Next;
    Link.Free;
  end;
end;

{$ifdef UseEnhancedRecord}
//-- BG ---------------------------------------------------------- 16.04.2011 --
procedure TStylePropertyList.Free;
begin
  Clear;
end;
{$endif}

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TStylePropertyList.GetItem(Index: TStylePropertySymbol): TStyleProperty;
begin
  Result := First;
  while Result <> nil do
  begin
    if Result.Symbol = Index then
      exit;
    if Result.Symbol > Index then
    begin
      Result := nil;
      exit;
    end;
    Result := Result.Next;
  end;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStylePropertyList.Init;
begin
  FFirst := nil;
  FLast := nil;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
function TStylePropertyList.IsEmpty: Boolean;
begin
  Result := First = nil;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStylePropertyList.Remove(Prop: TStyleProperty);
begin
//TODO -1 -oBG, 02.04.2011
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
function TStylePropertyList.ToString: ThtString;
var
  Prop: TStyleProperty;
begin
  Result := '';
  Prop := First;
  if Prop <> nil then
  begin
    Result := CrLfTab + Prop.ToString;
    Prop := Prop.Next;
    while Prop <> nil do
    begin
      Result := Result + ';' + CrLfTab + Prop.ToString;
      Prop := Prop.Next;
    end;
  end;
end;

{ TAttributeMatch }

//-- BG ---------------------------------------------------------- 14.03.2011 --
constructor TAttributeMatch.Create(Attribute: THtmlAttributeSymbol; Oper: TAttributeMatchOperator; const Value: ThtString);
begin
  inherited Create;
  FAttr := Attribute;
  FOper := Oper;
  FValue := Value;
end;

//-- BG ---------------------------------------------------------- 21.03.2011 --
function TAttributeMatch.ToString: ThtString;
var
  Name: ThtString;
begin
  Name := AttributeSymbolToStr(Attribute);
  case Oper of
    amoSet: Result := '[' + Name + ']';
  else
    Result := '[' + Name + CAttributeMatchOperator[Oper] + Value + ']';
  end;
end;

{ TAttributeMatchList }

//-- BG ---------------------------------------------------------- 14.03.2011 --
function TAttributeMatchList.GetItem(Index: Integer): TAttributeMatch;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function CompareAttributeMatches(P1, P2: Pointer): Integer;
var
  A1: TAttributeMatch absolute P1;
  A2: TAttributeMatch absolute P2;
begin
  Result := Ord(A1.Attribute) - Ord(A2.Attribute);
  if Result <> 0 then
    exit;
  Result := Ord(A1.Oper) - Ord(A2.Oper);
  if Result <> 0 then
    exit;
  Result := htCompareString(A1.Value, A2.Value);
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
function TryStrToAttributeMatchOperator(Str: ThtString; out Oper: TAttributeMatchOperator): Boolean;
var
  I: TAttributeMatchOperator;
begin
  for I := low(I) to high(I) do
    if CAttributeMatchOperator[I] = Str then
    begin
      Result := True;
      Oper := I;
      exit;
    end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TAttributeMatchList.Same(List: TAttributeMatchList): Boolean;
var
  I, N: Integer;
begin
  N := Count;
  Result := N = List.Count;
  if Result then
    for I := 0 To N - 1 do
      if CompareAttributeMatches(Self[I], List[I]) <> 0 then
      begin
        Result := False;
        break;
      end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure TAttributeMatchList.Sort;
begin
  inherited Sort(CompareAttributeMatches);
end;

//-- BG ---------------------------------------------------------- 21.03.2011 --
function TAttributeMatchList.ToString: ThtString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Self[I].ToString; 
end;

{ TStyleSelector }

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TStyleSelector.AddAttributeMatch(AAttributeMatch: TAttributeMatch);
begin
  if FAttributeMatches = nil then
    FAttributeMatches := TAttributeMatchList.Create(True);
  FAttributeMatches.Add(AAttributeMatch);
  Inc(FNumberOfNonIds);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TStyleSelector.AddClass(AClass: ThtString);
var
  Index: Integer;
begin
  Index := Length(FClasses);
  SetLength(FClasses, Index + 1);
  FClasses[Index] := AClass;
  Inc(FNumberOfNonIds);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TStyleSelector.AddId(AId: ThtString);
var
  Index: Integer;
begin
  Index := Length(FIds);
  SetLength(FIds, Index + 1);
  FIds[Index] := AId;
  Inc(FNumberOfElements);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TStyleSelector.AddPseudo(APseudo: TPseudo);
begin
  if APseudo in FPseudos then
    exit;

  Include(FPseudos, APseudo);
  if APseudo in PseudoElements then
    Inc(FNumberOfElements)
  else //if APseudo in PseudoClasses then
    Inc(FNumberOfNonIds);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TStyleSelector.AddTag(ATag: ThtString);
var
  Index: Integer;
begin
  Index := Length(FTags);
  SetLength(FTags, Index + 1);
  FTags[Index] := ATag;
  Inc(FNumberOfElements);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TStyleSelector.AttributeMatchesCount: Integer;
begin
  if FAttributeMatches <> nil then
    Result := FAttributeMatches.Count
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TStyleSelector.CompareSpecificity(ASelector: TStyleSelector): Integer;
// http://www.w3.org/TR/2010/WD-CSS2-20101207/cascade.html#specificity
begin
  Result := Ord(FIsFromStyleAttr) - Ord(ASelector.FIsFromStyleAttr);
  if Result <> 0 then
    exit;

  Result := NumberOfIDs - ASelector.NumberOfIDs;
  if Result <> 0 then
    exit;

  Result := NumberOfNonIDs - ASelector.NumberOfNonIDs;
  if Result <> 0 then
    exit;

  Result := NumberOfElements - ASelector.NumberOfElements;
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
destructor TStyleSelector.Destroy;
begin
  FAttributeMatches.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TStyleSelector.NumberOfIDs: Integer;
begin
  Result := Length(FIds);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TStyleSelector.Same(Selector: TStyleSelector): Boolean;
begin
  Sort;
  Selector.Sort;
  Result := False;
  if Length(FTags) = Length(Selector.FTags) then
    if Length(FClasses) = Length(Selector.FClasses) then
      if Length(FIds) = Length(Selector.FIds) then
        if AttributeMatchesCount = Selector.AttributeMatchesCount then
          if FPseudos = Selector.FPseudos then
            if SameStringArray(FClasses, Selector.FClasses) then
              if SameStringArray(FTags, Selector.FTags) then
                Result := FAttributeMatches.Same(Selector.FAttributeMatches);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure TStyleSelector.Sort;
begin
  if not (ssSorted in FSelectorState) then
  begin
    SortStringArray(FTags);
    SortStringArray(FClasses);
    SortStringArray(FIds);
    if FAttributeMatches <> nil then
      FAttributeMatches.Sort;
    Include(FSelectorState, ssSorted);
  end;
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TStyleSelector.ToString: ThtString;
var
  I: Integer;
  P: TPseudo;
begin
  if Length(FTags) = 1 then
    Result := FTags[0]
  else
    Result := '';

  for I := Low(FClasses) to High(FClasses) do
    Result := Result + '.' + FClasses[I];

  for I := Low(FIds) to High(FIds) do
    Result := Result + '#' + FIds[I];

  for P := Low(TPseudo) to High(TPseudo) do
    if P in FPseudos then
      Result := Result + ':' + CPseudo[P];

  if Result = '' then
    Result := '*';

  if FAttributeMatches <> nil then
    Result := Result + FAttributeMatches.ToString;
end;

{ TCombinedSelector }

//-- BG ---------------------------------------------------------- 15.03.2011 --
constructor TCombinedSelector.Create(LeftHand: TStyleSelector; Combinator: TStyleCombinator);
begin
  inherited Create;
  FLeftHand := LeftHand;
  FCombinator := Combinator;
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
destructor TCombinedSelector.Destroy;
begin
  FLeftHand.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TCombinedSelector.ToString: ThtString;
var
  CmbStr: ThtString;
begin
  case FCombinator of
    scDescendant: CmbStr := ' ';
    scChild: CmbStr := ' > ';
    scFollower: CmbStr := ' + ';
  else
    CmbStr := ' ? ';
  end;
  Result := FLeftHand.ToString + CmbStr + inherited ToString;
end;

{ TStyleSelectorList }

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStyleSelectorList.Add(Sele: TStyleSelector);
var
  Prev: TStyleSelector;
begin
  assert(Sele.Next = nil, 'Don''t add chained links twice. Sele.Next is not nil.');
  assert(Sele.Prev = nil, 'Don''t add chained links twice. Sele.Prev is not nil.');

  Prev := Last;

  // link to prev
  if Prev = nil then
    FFirst := Sele
  else
    Prev.FNext := Sele;
  Sele.FPrev := Prev;

  // link to next
  FLast := Sele;
  Sele.FNext := nil;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStyleSelectorList.Assign(const List: TStyleSelectorList);
begin
  FFirst := List.FFirst;
  FLast := List.FLast;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure TStyleSelectorList.Clear;
var
  Link: TStyleSelector;
begin
  FLast := nil;
  while First <> nil do
  begin
    Link := First;
    FFirst := Link.Next;
    Link.Free;
  end;
end;

{$ifdef UseEnhancedRecord}
//-- BG ---------------------------------------------------------- 16.04.2011 --
procedure TStyleSelectorList.Free;
begin
  Clear;
end;
{$endif}

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStyleSelectorList.Init;
begin
  FFirst := nil;
  FLast := nil;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
function TStyleSelectorList.IsEmpty: Boolean;
begin
  Result := First = nil;
end;

//-- BG ---------------------------------------------------------- 02.04.2011 --
procedure TStyleSelectorList.Remove(Prop: TStyleSelector);
begin
//TODO -1 -oBG, 02.04.2011
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TStyleSelectorList.ToString: ThtString;
var
  Sele: TStyleSelector;
begin
  Result := '';
  Sele := First;
  if Sele <> nil then
  begin
    Result := Sele.ToString;
    Sele := Sele.Next;
    while Sele <> nil do
    begin
      Result := Result + ', ' + Sele.ToString;
      Sele := Sele.Next;
    end;
  end;
end;

{ TRuleset }

//-- BG ---------------------------------------------------------- 15.03.2011 --
constructor TRuleset.Create(MediaTypes: TMediaTypes);
begin
  inherited Create;
  FMediaTypes := MediaTypes;
{$ifdef UseEnhancedRecord}
{$else}
  Selectors := TStyleSelectorList.Create;
  Properties := TStylePropertyList.Create;
{$endif}
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
destructor TRuleset.Destroy;
begin
  Properties.Free;
  Selectors.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TRuleset.ToString: ThtString;
var
  Strings: ThtStringList;
  Indent: ThtString;
  Sele: TStyleSelector;
  Prop: TStyleProperty;
begin
  Strings := ThtStringList.Create;
  Indent := '';
  try
    if FMediaTypes = [] then
      Strings.Add('@media none {')
    else if not ((mtAll in FMediaTypes) or (FMediaTypes = AllMediaTypes)) then
      Strings.Add('@media ' + MediaTypesToStr(FMediaTypes) + ' {');
    if Strings.Count > 0 then
      Indent := TabChar;

    if Selectors.First <> nil then
    begin
      Sele := Selectors.First;
      while Sele.Next <> nil do
      begin
        Strings.Add(Indent + Sele.ToString + ',');
        Sele := Sele.Next;
      end;
      Strings.Add(Indent + Sele.ToString + ' {');

      if Properties.First <> nil then
      begin
        Indent := Indent + TabChar;
        Prop := Properties.First;
        while Prop.Next <> nil do
        begin
          Strings.Add(Indent + Prop.ToString + ';');
          Prop := Prop.Next;
        end;
        Strings.Add(Indent + Prop.ToString);
        SetLength(Indent, Length(Indent) - 1);
      end;
      Strings.Add(Indent + '}');
    end;
    if Indent <> '' then
      Strings.Add('}');
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

{ TRulesetList }

//-- BG ---------------------------------------------------------- 15.03.2011 --
function TRulesetList.GetItem(Index: Integer): TRuleset;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
procedure TRulesetList.InsertList(Index: Integer; Rulesets: TRulesetList);
var
  InsCount: Integer;
  NewCount: Integer;
begin
  InsCount := Rulesets.Count;
  if InsCount > 0 then
  begin
    NewCount := Count + InsCount;
    if Capacity < NewCount then
      Capacity := NewCount;
    if Index < Count then
      System.Move(List[Index], List[Index + InsCount], (Count - Index) * SizeOf(Pointer));

    // Make sure that Rulesets no longer owns the objects:
    Rulesets.OwnsObjects := False;
    System.Move(Rulesets.List[0], List[Index], InsCount * SizeOf(Pointer));
  end;

end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TRulesetList.ToString: ThtString;
var
  Strings: TStrings;
  I: Integer;
begin
  Strings := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      Strings.Add(Self[I].ToString);
    Result := Strings.Text;
{$ifdef DEBUG}
    if Length(Result) > 0 then
      Strings.SaveToFile('C:\Temp\Rulesets.css');
{$endif}      
  finally
    Strings.Free;
  end;
end;

{ TResultingProperty }

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TResultingProperty.Compare(const AProperty: TStyleProperty; const ASelector: TStyleSelector): Integer;
begin
  Result := Ord(FProperty.Precedence) - Ord(AProperty.Precedence);
  if Result <> 0 then
    exit;

  Result := FSelector.CompareSpecificity(ASelector);
end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function TResultingProperty.GetBorderStyle(Parent, Default: TBorderStyle): TBorderStyle;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Value) then
      Result := TBorderStyle(Prop.Value)
    else if VarIsStr(Prop.Value) then
      if TryStrToBorderStyle(Prop.Value, Result) then
        Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingProperty.GetColor(Parent, Default: TColor): TColor;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Value) then
      Result := TColor(Prop.Value)
    else if VarIsStr(Prop.Value) then
      if TryStrToColor(Prop.Value, False, Result) then
        Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingProperty.GetDisplay(Parent, Default: TDisplayStyle): TDisplayStyle;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Value) then
      Result := TDisplayStyle(Prop.Value)
    else if VarIsStr(Prop.Value) then
      if TryStrToDisplayStyle(Prop.Value, Result) then
        Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingProperty.GetFloat(Parent, Default: TBoxFloatStyle): TBoxFloatStyle;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Value) then
      Result := TBoxFloatStyle(Prop.Value)
    else if VarIsStr(Prop.Value) then
      if TryStrToBoxFloatStyle(Prop.Value, Result) then
        Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function TResultingProperty.GetFontName(Parent, Default: ThtString): ThtString;

  function FontFamilyToFontName(Family: ThtString): ThtString;
  begin
    //TODO -oBG, 03.05.2011: translate font famliy to font name
    Result := Family;
  end;

begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsStr(Prop.Value) then
    begin
      Result := FontFamilyToFontName(Prop.Value);
      Prop.Value := Result;
    end;
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function TResultingProperty.GetFontSize(DefaultFontSize, Parent, Default: Double): Double;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsNumeric(Prop.Value) then
      Result := Prop.Value
    else if VarIsStr(Prop.Value) then
    begin
      Result := StrToFontSize(Prop.Value, FontConvBase, DefaultFontSize, Parent, Default);
      Prop.Value := Result;
    end;
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function TResultingProperty.GetFontWeight(Parent, Default: Integer): Integer;
// CSS 2.1:
// Parent  | 100 200 300 400 500 600 700 800 900
// --------+------------------------------------
// bolder  | 400 400 400 700 700 900 900 900 900
// lighter | 100 100 100 100 100 400 400 700 700
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsNumeric(Prop.Value) then
      Result := Prop.Value
    else if VarIsStr(Prop.Value) then
    begin
      if htCompareString(Prop.Value, 'normal') = 0 then
        Result := 400
      else if htCompareString(Prop.Value, 'bold') = 0 then
        Result := 700
      else if htCompareString(Prop.Value, 'bolder') = 0 then
        if Parent <= 300 then
          Result := 400
        else if Parent < 600 then
          Result := 700
        else
          Result := 900
      else if htCompareString(Prop.Value, 'lighter') = 0 then
        if Parent <= 500 then
          Result := 100
        else if Parent < 800 then
          Result := 400
        else
          Result := 700
      else if htCompareString(Prop.Value, 'light') = 0 then
        Result := 100;
      Prop.Value := Result;
    end;
end;

//-- BG ---------------------------------------------------------- 30.04.2011 --
function TResultingProperty.GetLength(Parent, Base, EmBase, Default: Double): Double;
{
 Return the calculated length.
 if Relative is true,
}
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsNumeric(Prop.Value) then
      Result := Prop.Value
    else if VarIsStr(Prop.Value) then
    begin
      Result := StrToLength(Prop.Value, False, Parent, EmBase, Default);
      Prop.Value := Result;
    end;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingProperty.GetPosition(Parent, Default: TBoxPositionStyle): TBoxPositionStyle;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Value) then
      Result := TBoxPositionStyle(Prop.Value)
    else if VarIsStr(Prop.Value) then
      if TryStrToBoxPositionStyle(Prop.Value, Result) then
        Prop.Value := Integer(Result);
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function TResultingProperty.GetTextDecoration(Parent, Default: TFontStyles): TFontStyles;
var
  ResultAsByte: Byte absolute Result;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsOrdinal(Prop.Value) then
      ResultAsByte := Prop.Value
    else if VarIsStr(Prop.Value) then
    begin
      if htCompareString(Prop.Value, 'underline') = 0 then
        Result := [fsUnderline]
      else if htCompareString(Prop.Value, 'line-through') = 0 then
        Result := [fsStrikeOut]
      else
        Result := [];
      Prop.Value := ResultAsByte;
    end;
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function TResultingProperty.GetValue(Parent, Default: Variant): Variant;
begin
  if Self = nil then
    Result := Default
  else
  begin
    Result := Prop.Value;
    if Result = Inherit then
      Result := Parent;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2011 --
function TResultingProperty.HasValue: Boolean;
begin
  Result := (Self <> nil) and (Prop.Value <> Unassigned);
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
function TResultingProperty.IsItalic(Parent, Default: Boolean): Boolean;
begin
  Result := Default;
  if Self <> nil then
    if Prop.Value = Inherit then
      Result := Parent
    else if VarIsType(Prop.Value, varBoolean) then
      Result := Prop.Value
    else if VarIsStr(Prop.Value) then
    begin
      if htCompareString(Prop.Value, 'italic') = 0 then
        Result := True
      else if htCompareString(Prop.Value, 'oblique') = 0 then
        Result := True
      else
        Result := False;
      Prop.Value := Result;
    end;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingProperty.Update(const AProperty: TStyleProperty; const ASelector: TStyleSelector);
begin
  FProperty := AProperty;
  FSelector := ASelector;
end;

{ TResultingPropertyMap }

destructor TResultingPropertyMap.Destroy;
var
  I: TStylePropertySymbol;
begin
  for I := Low(I) to High(I) do
    FProps[I].Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TResultingPropertyMap.Get(Index: TStylePropertySymbol): TResultingProperty;
begin
  Result := FProps[Index];
end;

//-- BG ---------------------------------------------------------- 03.09.2011 --
procedure TResultingPropertyMap.GetElementBoxingInfo(var Info: THtmlElementBoxingInfo);
{
 Get the calculated values of the boxing properties.
 On enter Info must contain the parent's properties.
 On exit Info returns the calculated values of this property map.
}
begin
  Info.Display := Properties[psDisplay].GetDisplay(Info.Display, pdInline);
  case Info.Display of
    pdNone:
    begin
      Info.Position := posAbsolute;
      Info.Float := flNone;
    end;
  else
    Info.Position := Properties[psPosition].GetPosition(Info.Position, posStatic);
    case Info.Position of
      posAbsolute,
      posFixed:
      begin
        Info.Float := flNone;
        Info.Display := ToRootDisplayStyle(Info.Display);
      end;
    else
      Info.Float := Properties[psFloat].GetFloat(Info.Float, flNone);
      case Info.Float of
        flNone:;
      else
        Info.Display := ToRootDisplayStyle(Info.Display);
      end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingPropertyMap.GetLengths(Left, Top, Right, Bottom: TPropertySymbol;
  const Parent, Default: TRectIntegers; BaseSize: Integer): TRectIntegers;
var
  PercentBase: Double;
begin
  PercentBase := Parent[reRight] - Parent[reLeft];
  Result[reLeft]   := Round(Properties[Left].  GetLength(Parent[reLeft],   PercentBase, BaseSize, Default[reLeft]  ));
  Result[reRight]  := Round(Properties[Right]. GetLength(Parent[reRight],  PercentBase, BaseSize, Default[reRight] ));
  PercentBase := Parent[reBottom] - Parent[reTop];
  Result[reTop]    := Round(Properties[Top].   GetLength(Parent[reTop],    PercentBase, BaseSize, Default[reTop]   ));
  Result[reBottom] := Round(Properties[Bottom].GetLength(Parent[reBottom], PercentBase, BaseSize, Default[reBottom]));
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingPropertyMap.GetColors(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectColors): TRectColors;
begin
  Result[reLeft]   := Properties[Left].  GetColor(Parent[reLeft],   Default[reLeft]  );
  Result[reTop]    := Properties[Top].   GetColor(Parent[reTop],    Default[reTop]   );
  Result[reRight]  := Properties[Right]. GetColor(Parent[reRight],  Default[reRight] );
  Result[reBottom] := Properties[Bottom].GetColor(Parent[reBottom], Default[reBottom]);
end;

//-- BG ---------------------------------------------------------- 01.05.2011 --
function TResultingPropertyMap.GetStyles(Left, Top, Right, Bottom: TPropertySymbol; const Parent, Default: TRectStyles): TRectStyles;
begin
  Result[reLeft]   := Properties[Left].  GetBorderStyle(Parent[reLeft],   Default[reLeft]);
  Result[reTop]    := Properties[Top].   GetBorderStyle(Parent[reTop],    Default[reTop]);
  Result[reRight]  := Properties[Right]. GetBorderStyle(Parent[reRight],  Default[reRight]);
  Result[reBottom] := Properties[Bottom].GetBorderStyle(Parent[reBottom], Default[reBottom]);
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingPropertyMap.Update(const AProperty: TStyleProperty; const ASelector: TStyleSelector);
begin
  if FProps[AProperty.Symbol] = nil then
  begin
    FProps[AProperty.Symbol] := TResultingProperty.Create;
    FProps[AProperty.Symbol].Update(AProperty, ASelector);
  end
  else if FProps[AProperty.Symbol].Compare(AProperty, ASelector) < 0 then
    FProps[AProperty.Symbol].Update(AProperty, ASelector);
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingPropertyMap.UpdateFromProperties(const Properties: TStylePropertyList; const ASelector: TStyleSelector);
var
  Prop: TStyleProperty;
begin
  Prop := Properties.First;
  while Prop <> nil do
  begin
    Update(Prop, ASelector);
    Prop := Prop.Next;
  end;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingPropertyMap.UpdateFromAttributes(const Properties: TStylePropertyList; IsFromStyleAttr: Boolean);
begin
  if IsFromStyleAttr then
    UpdateFromProperties(Properties, StyleAttributeSelector)
  else
    UpdateFromProperties(Properties, StyleTagSelector);
end;

initialization
  StyleAttributeSelector := TStyleSelector.Create;
  StyleAttributeSelector.FIsFromStyleAttr := True;
  StyleTagSelector := TStyleSelector.Create;
finalization
  FreeAndNil(StyleAttributeSelector);
  FreeAndNil(StyleTagSelector);
end.
