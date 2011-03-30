{
HtmlViewer Version 12
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

unit HtmlStyles;

interface

uses
  Windows, Classes, Contnrs, Variants,
  //
  HtmlGlobals,
  HtmlSymbols;

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
    'Style Normal',     // normal author resp. document property by style attribute
    'Author Normal',    // normal author resp. document property
    'Author Important', // important author propery
    'Style Important',  // important author resp. document property by style attribute
    'User Important'    // important user property preference
  );

  CPropertyPrecedenceOfOrigin: array [Boolean, TPropertyOrigin] of TPropertyPrecedence = (
    (ppdefault, ppAgent, ppUserNormal, ppStyleNormal, ppAuthorNormal),
    (ppdefault, ppAgent, ppUserImportant, ppStyleImportant, ppAuthorImportant)
  );

// custom variant types range from $110 (272) to $7FF (2047)
const
  varInherit = $123; // a value of this variant type indicates, that this value is to be inherited from parent object.

function Inherit: Variant; {$ifdef UseInline} inline; {$endif}
function VarIsInherit(const Value: Variant): Boolean; {$ifdef UseInline} inline; {$endif}

type
  TProperty = class
  private
    FSymbol: TStylePropertySymbol;
    FPrecedence: TPropertyPrecedence;
    FSpecifiedValue: Variant;
    FCalculatedValue: Variant;
  public
    constructor Create(Symbol: TStylePropertySymbol; Precedence: TPropertyPrecedence; SpecifiedValue: Variant); overload;
    constructor Create(Symbol: TStylePropertySymbol; Precedence: TPropertyPrecedence; SpecifiedValue, CalculatedValue: Variant); overload;
    function ToString: ThtString;
    property Symbol: TStylePropertySymbol read FSymbol;
    property Precedence: TPropertyPrecedence read FPrecedence;
    property SpecifiedValue: Variant read FSpecifiedValue;
    property CalculatedValue: Variant read FCalculatedValue write FCalculatedValue;
  end;

  TPropertyList = class(TObjectList)
  private
    function GetItem(Index: Integer): TProperty;
  public
    function ToString: ThtString;
    property Items[Index: Integer]: TProperty read GetItem; default;
  end;

//------------------------------------------------------------------------------
// style selectors
//------------------------------------------------------------------------------

type

//  TAttributeList = class(ThtStringList)
//  public
//  end;

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
    FName: ThtString;
    FOper: TAttributeMatchOperator;
    FValue: ThtString;
  public
    constructor Create(const Name: ThtString; Oper: TAttributeMatchOperator; const Value: ThtString);
    function ToString: ThtString;
    property Name: ThtString read FName;
    property Oper: TAttributeMatchOperator read FOper;
    property Value: ThtString read FValue;
  end;

  TAttributeMatchList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAttributeMatch;
  public
    function Same(List: TAttributeMatchList): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    function ToString: ThtString;
    procedure Sort; overload; {$ifdef UseInline} inline; {$endif}
    property Items[Index: Integer]: TAttributeMatch read GetItem; default;
  end;

function CompareAttributeMatches(P1, P2: Pointer): Integer;

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

  TCombinator = (
    scNone,       // F      : matches any F
    scDescendant, // E F    : matches F, if it is descendant of E
    scChild,      // E > F  : matches F, if it is child of E
    scFollower    // E + F  : matches F, if it immediately follows sibling E
  );

  TSelectorState = set of (
    ssSorted
  );

  // a simple selector
  TSelector = class
  private
    FTags: ThtStringArray;
    FClasses: ThtStringArray;
    FIds: ThtStringArray;
    FPseudos: TPseudos;
    FAttributeMatches: TAttributeMatchList;
    FIsFromStyleAttr: Boolean;
    FSelectorState: TSelectorState;
    FNumberOfElements: Integer;
    FNumberOfNonIds: Integer;
    function NumberOfIDs: Integer; {$ifdef UseInline} inline; {$endif}
    procedure Sort; {$ifdef UseInline} inline; {$endif}
    property NumberOfElements: Integer read FNumberOfElements;
    property NumberOfNonIDs: Integer read FNumberOfNonIds;
  public
    destructor Destroy; override;
    function AttributeMatchesCount: Integer; {$ifdef UseInline} inline; {$endif}
    function Same(Selector: TSelector): Boolean; virtual;
    function ToString: ThtString; virtual;
    function CompareSpecificity(ASelector: TSelector): Integer;
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
  end;

  // a compound selector
  TCombinedSelector = class(TSelector)
  private
    FLeftHand: TSelector;
    FCombinator: TCombinator;
  public
    constructor Create(LeftHand: TSelector; Combinator: TCombinator);
    destructor Destroy; override;
    function ToString: ThtString; override;
    property Combinator: TCombinator read FCombinator;
    property LeftHand: TSelector read FLeftHand;
  end;

  TSelectorList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSelector;
  public
    function ToString: ThtString;
    property Items[Index: Integer]: TSelector read GetItem; default;
  end;

//------------------------------------------------------------------------------
// media types
//------------------------------------------------------------------------------

type
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/media.html
  TMediaType = (
    mtAll,        // Suitable for all devices.
    mtBraille,    // Intended for braille tactile feedback devices.
    mtEmbossed,   // Intended for paged braille printers.
    mtHandheld,   // Intended for handheld devices (typically small screen, limited bandwidth).
    mtPrint,      // Intended for paged material and for documents viewed on screen in print preview mode. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtProjection, // Intended for projected presentations, for example projectors. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtScreen,     // Intended primarily for color computer screens.
    mtSpeech,     // Intended for speech synthesizers. Note: CSS2 had a similar media type called 'aural' for this purpose. See the appendix on aural style sheets for details.
    mtTty,        // Intended for media using a fixed-pitch character grid (such as teletypes, terminals, or portable devices with limited display capabilities). Authors should not use pixel units with the "tty" media type.
    mtTv          // Intended for television-type devices (low resolution, color, limited-scrollability screens, sound available).
  );
  TMediaTypes = set of TMediaType;

const
  CMediaTypes: array[TMediaType] of ThtString = (
    'all',
    'braille',
    'embossed',
    'handheld',
    'print',
    'projection',
    'screen',
    'speech',
    'tty',
    'tv'
  );
  AllMediaTypes: TMediaTypes = [
    //mtAll,        // Suitable for all devices.
    mtBraille,    // Intended for braille tactile feedback devices.
    mtEmbossed,   // Intended for paged braille printers.
    mtHandheld,   // Intended for handheld devices (typically small screen, limited bandwidth).
    mtPrint,      // Intended for paged material and for documents viewed on screen in print preview mode. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtProjection, // Intended for projected presentations, for example projectors. Please consult the section on paged media for information about formatting issues that are specific to paged media.
    mtScreen,     // Intended primarily for color computer screens.
    mtSpeech,     // Intended for speech synthesizers. Note: CSS2 had a similar media type called 'aural' for this purpose. See the appendix on aural style sheets for details.
    mtTty,        // Intended for media using a fixed-pitch character grid (such as teletypes, terminals, or portable devices with limited display capabilities). Authors should not use pixel units with the "tty" media type.
    mtTv          // Intended for television-type devices (low resolution, color, limited-scrollability screens, sound available).
  ];

function MediaTypesToStr(const MediaTypes: TMediaTypes): ThtString;
function TryStrToMediaType(const Str: ThtString; out MediaType: TMediaType): Boolean;

//------------------------------------------------------------------------------
// style rule sets
//------------------------------------------------------------------------------

type

  TRuleset = class
  private
    FProperties: TPropertyList;
    FSelectors: TSelectorList;
    FMediaTypes: TMediaTypes;
  public
    constructor Create(MediaTypes: TMediaTypes);
    destructor Destroy; override;
    function ToString: ThtString;
    property Properties: TPropertyList read FProperties;
    property Selectors: TSelectorList read FSelectors;
  end;

  TRulesetList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRuleset;
  public
    function ToString: ThtString;
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
    FProperty: TProperty;
    FSelector: TSelector;
  public
    function Compare(const AProperty: TProperty; const ASelector: TSelector): Integer; {$ifdef UseInline} inline; {$endif}
    procedure Update(const AProperty: TProperty; const ASelector: TSelector); {$ifdef UseInline} inline; {$endif}
  end;

  TResultingProperties = array [TStylePropertySymbol] of TResultingProperty;

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
    FStyle: TSelector; // the selector for updating from style attributes
    FProps: TResultingProperties; // the resulting properties
    procedure Update(const AProperty: TProperty; const ASelector: TSelector); {$ifdef UseInline} inline; {$endif}
  public
    destructor Destroy; override;
    function Get(Index: TStylePropertySymbol): TResultingProperty; {$ifdef UseInline} inline; {$endif}
    procedure UpdateFromAttributes(const Properties: TPropertyList; IsFromStyleAttr: Boolean); {$ifdef UseInline} inline; {$endif}
    procedure UpdateFromProperties(const Properties: TPropertyList; const ASelector: TSelector); {$ifdef UseInline} inline; {$endif}
    property ResultingProperties[Index: TStylePropertySymbol]: TResultingProperty read Get; default;
  end;

//------------------------------------------------------------------------------
// 
//------------------------------------------------------------------------------

function GetCssDefaults: TStream;

implementation

{$R css_defaults.res}

//-- BG ---------------------------------------------------------- 20.03.2011 --
function GetCssDefaults: TStream;
begin
  Result := TResourceStream.Create(hInstance, 'CSS_DEFAULTS', RT_RCDATA);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function MediaTypesToStr(const MediaTypes: TMediaTypes): ThtString;
var
  I: TMediaType;
begin
  SetLength(Result, 0);
  for I := low(I) to high(I) do
    if I in MediaTypes then
    begin
      if Length(Result) = 0 then
        Result := CMediaTypes[I]
      else
        Result := Result + ', ' + CMediaTypes[I];
    end;
end;


//-- BG ---------------------------------------------------------- 15.03.2011 --
function TryStrToMediaType(const Str: ThtString; out MediaType: TMediaType): Boolean;
var
  I: TMediaType;
begin
  for I := low(I) to high(I) do
    if CMediaTypes[I] = Str then
    begin
      Result := True;
      MediaType := I;
      exit;
    end;
  Result := False;
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
  TVarData(Result).VType := varInherit;
end;

//-- BG ---------------------------------------------------------- 21.03.2011 --
function VarIsInherit(const Value: Variant): Boolean;
// returns true, if Value is of type varInherit
begin
  Result := VarType(Value) = varInherit;
end;


{ TProperty }

//-- BG ---------------------------------------------------------- 13.03.2011 --
constructor TProperty.Create(Symbol: TStylePropertySymbol; Precedence: TPropertyPrecedence; SpecifiedValue: Variant);
begin
  inherited Create;
  FSymbol := Symbol;
  FPrecedence := Precedence;
  FSpecifiedValue := SpecifiedValue;
  FCalculatedValue := Null;
end;

//-- BG ---------------------------------------------------------- 13.03.2011 --
constructor TProperty.Create(Symbol: TStylePropertySymbol; Precedence: TPropertyPrecedence; SpecifiedValue, CalculatedValue: Variant);
begin
  inherited Create;
  FSymbol := Symbol;
  FPrecedence := Precedence;
  FSpecifiedValue := SpecifiedValue;
  FCalculatedValue := CalculatedValue;
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TProperty.ToString: ThtString;

  function VariantToStr(Value: Variant): ThtString;
  begin
    if VarIsNull(Value) then
      Result := ''
    else
      Result := Value;
  end;

begin
  Result := PropertySymbolToStr(FSymbol) + ': ' + VariantToStr(SpecifiedValue) +
    ' /* ' + CPropertyPrecedence[FPrecedence] + ', ' + VariantToStr(CalculatedValue) + ' */';
end;

{ TPropertyList }

//-- BG ---------------------------------------------------------- 13.03.2011 --
function TPropertyList.GetItem(Index: Integer): TProperty;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TPropertyList.ToString: ThtString;
var
  I: Integer;
begin
  Result := '';
  if Self <> nil then
    if Count > 0 then
    begin
      Result := CrLf + TabChar + Self[0].ToString;
      for I := 1 to Count - 1 do
        Result := Result + ';' + CrLf + TabChar + Self[I].ToString;
    end;
end;

{ TAttributeMatch }

//-- BG ---------------------------------------------------------- 14.03.2011 --
constructor TAttributeMatch.Create(const Name: ThtString; Oper: TAttributeMatchOperator;
  const Value: ThtString);
begin
  inherited Create;
end;

//-- BG ---------------------------------------------------------- 21.03.2011 --
function TAttributeMatch.ToString: ThtString;
begin
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
  Result := htCompareString(A1.Name, A2.Name);
  if Result <> 0 then
    exit;
  Result := Ord(A1.Oper) - Ord(A2.Oper);
  if Result <> 0 then
    exit;
  Result := htCompareString(A1.Value, A2.Value);
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

{ TSelector }

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TSelector.AddAttributeMatch(AAttributeMatch: TAttributeMatch);
begin
  if FAttributeMatches = nil then
    FAttributeMatches := TAttributeMatchList.Create(True);
  FAttributeMatches.Add(AAttributeMatch);
  Inc(FNumberOfNonIds);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TSelector.AddClass(AClass: ThtString);
var
  Index: Integer;
begin
  Index := Length(FClasses);
  SetLength(FClasses, Index + 1);
  FClasses[Index] := AClass;
  Inc(FNumberOfNonIds);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TSelector.AddId(AId: ThtString);
var
  Index: Integer;
begin
  Index := Length(FIds);
  SetLength(FIds, Index + 1);
  FIds[Index] := AId;
  Inc(FNumberOfElements);
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure TSelector.AddPseudo(APseudo: TPseudo);
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
procedure TSelector.AddTag(ATag: ThtString);
var
  Index: Integer;
begin
  Index := Length(FTags);
  SetLength(FTags, Index + 1);
  FTags[Index] := ATag;
  Inc(FNumberOfElements);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TSelector.AttributeMatchesCount: Integer;
begin
  if FAttributeMatches <> nil then
    Result := FAttributeMatches.Count
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TSelector.CompareSpecificity(ASelector: TSelector): Integer;
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
destructor TSelector.Destroy;
begin
  FAttributeMatches.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TSelector.NumberOfIDs: Integer;
begin
  Result := Length(FIds);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TSelector.Same(Selector: TSelector): Boolean;
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
procedure TSelector.Sort;
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
function TSelector.ToString: ThtString;
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
constructor TCombinedSelector.Create(LeftHand: TSelector; Combinator: TCombinator);
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

{ TSelectorList }

//-- BG ---------------------------------------------------------- 15.03.2011 --
function TSelectorList.GetItem(Index: Integer): TSelector;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TSelectorList.ToString: ThtString;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    Result := Self[0].ToString;
    for I := 1 to Count - 1 do
      Result := Result + ', ' + Self[I].ToString;
  end;
end;

{ TRuleset }

//-- BG ---------------------------------------------------------- 15.03.2011 --
constructor TRuleset.Create(MediaTypes: TMediaTypes);
begin
  inherited Create;
  FProperties := TPropertyList.Create;
  FSelectors := TSelectorList.Create;
  FMediaTypes := MediaTypes
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
destructor TRuleset.Destroy;
begin
  FProperties.Free;
  FSelectors.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function TRuleset.ToString: ThtString;
var
  Strings: ThtStringList;
  Indent: ThtString;
  I: Integer;
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

    if Selectors.Count > 0 then
    begin
      I := 0;
      while I < Selectors.Count - 1 do
      begin
        Strings.Add(Indent + Selectors[I].ToString + ',');
        Inc(I);
      end;
      Strings.Add(Indent + Selectors[I].ToString + ' {');

      if Properties.Count > 0 then
      begin
        Indent := Indent + TabChar;
        I := 0;
        while I < Properties.Count - 1 do
        begin
          Strings.Add(Indent + Properties[I].ToString + ';');
          Inc(I);
        end;
        Strings.Add(Indent + Properties[I].ToString);
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
    if Length(Result) > 0 then
      Strings.SaveToFile('C:\Temp\Rulesets.css');
  finally
    Strings.Free;
  end;
end;

{ TResultingProperty }

function TResultingProperty.Compare(const AProperty: TProperty; const ASelector: TSelector): Integer;
begin
  Result := Ord(FProperty.Precedence) - Ord(AProperty.Precedence);
  if Result <> 0 then
    exit;

  Result := FSelector.CompareSpecificity(ASelector);
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingProperty.Update(const AProperty: TProperty; const ASelector: TSelector);
begin
  FProperty := AProperty;
  FSelector := ASelector;
end;

{ TResultingPropertyMap }

destructor TResultingPropertyMap.Destroy;
var
  I: TStylePropertySymbol;
begin
  FStyle.Free;
  for I := Low(I) to High(I) do
    FProps[I].Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
function TResultingPropertyMap.Get(Index: TStylePropertySymbol): TResultingProperty;
begin
  Result := FProps[Index];
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingPropertyMap.Update(const AProperty: TProperty; const ASelector: TSelector);
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
procedure TResultingPropertyMap.UpdateFromProperties(const Properties: TPropertyList; const ASelector: TSelector);
var
  I: Integer;
begin
  for I := 0 to Properties.Count - 1 do
    Update(Properties[I], ASelector);
end;

//-- BG ---------------------------------------------------------- 22.03.2011 --
procedure TResultingPropertyMap.UpdateFromAttributes(const Properties: TPropertyList; IsFromStyleAttr: Boolean);
begin
  if FStyle = nil then
    FStyle := TSelector.Create;
  FStyle.FIsFromStyleAttr := IsFromStyleAttr;
  UpdateFromProperties(Properties, FStyle);
end;

end.
