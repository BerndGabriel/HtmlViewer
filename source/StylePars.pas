{
Version   11.3
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2012 by HtmlViewer Team

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

unit StylePars;

interface

uses
{$ifdef VCL}
  Windows,  // needed to expand inline function htUpCase 
{$endif}
  Classes, Graphics, SysUtils,
  HtmlGlobals, HtmlBuffer, UrlSubs, StyleUn;


{---------  Detect Shorthand syntax }
type
  TShortHand = (
    MarginX, PaddingX, BorderWidthX, BorderX,
    BorderTX, BorderRX, BorderBX, BorderLX,
    FontX, BackgroundX, ListStyleX, BorderColorX,
    BorderStyleX);

  EParseError = class(Exception);

  //TProcessProc = procedure(Obj: TObject; Selectors: ThtStringList; Prop, Value: ThtString);

  { THtmlStyleParser }

  THtmlStyleParser = class
  private
    Doc: TBuffer;
    LCh: ThtChar;
    LinkPath: ThtString;
    FUseQuirksMode : Boolean;
    // parser methods
    procedure GetCh;
    function GetIdentifier(out Identifier: ThtString): Boolean;
    procedure SkipWhiteSpace;
    function GetString(out Str: ThtString): Boolean;
    //
    function AddPath(S: ThtString): ThtString;
    procedure ProcessShortHand(Index: TShortHand; const Prop, OrigValue, StrippedValue: ThtString);
  protected
    constructor Create(const AUseQuirksMode : Boolean);
    procedure ProcessProperty(const Prop, Value: ThtString); virtual; abstract;
  end;

  THtmlStyleTagParser = class(THtmlStyleParser)
  private
    Selectors: ThtStringList;
    Styles: TStyleList;
    procedure GetCollection;
    procedure GetSelectors;
  protected
    procedure ProcessProperty(const Prop, Value: ThtString); override;
  public
    constructor Create(const AUseQuirksMode : Boolean);
    destructor Destroy; override;
    procedure DoStyle(Styles: TStyleList; var C: ThtChar; Doc: TBuffer; const APath: ThtString; FromLink: boolean);
  end;

  THtmlStyleAttrParser = class(THtmlStyleParser)
  private
    Propty: TProperties;
  protected
    procedure ProcessProperty(const Prop, Value: ThtString); override;
  public
    procedure ParseProperties(Doc: TBuffer; Propty: TProperties);
  end;

procedure DoStyle(Styles: TStyleList; var C: ThtChar; Doc: TBuffer; const APath: ThtString; FromLink: boolean; const AUseQuirksMode : Boolean);
procedure ParsePropertyStr(PropertyStr: ThtString; Propty: TProperties);
function SortContextualItems(S: ThtString): ThtString;

implementation

const
  ShortHands: array[Low(TShortHand)..High(TShortHand)] of ThtString = (
    'margin', 'padding', 'border-width', 'border',
    'border-top', 'border-right', 'border-bottom', 'border-left',
    'font', 'background', 'list-style', 'border-color',
    'border-style');

const
  NeedPound = True;

//-- BG ---------------------------------------------------------- 26.12.2010 --
procedure DoStyle(Styles: TStyleList; var C: ThtChar; Doc: TBuffer; const APath: ThtString; FromLink: boolean; const AUseQuirksMode : Boolean);
var
  Parser: THtmlStyleTagParser;
begin
  Parser := THtmlStyleTagParser.Create(AUseQuirksMode);
  try
    Parser.DoStyle(Styles, C, Doc, APath, FromLink);
  finally
    Parser.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 26.12.2010 --
procedure ParseProperties(Doc: TBuffer; Propty: TProperties);
var
  Parser: THtmlStyleAttrParser;
begin
  Parser := THtmlStyleAttrParser.Create(Propty.UseQuirksMode);
  try
    Parser.ParseProperties(Doc, Propty);
  finally
    Parser.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 26.12.2010 --
procedure ParsePropertyStr(PropertyStr: ThtString; Propty: TProperties);
var
  Doc: TBuffer;
begin
  Doc := TBuffer.Create(PropertyStr);
  try
    ParseProperties(Doc, Propty);
  finally
    Doc.Free;
  end;
end;

constructor THtmlStyleParser.Create(const AUseQuirksMode: Boolean);
begin
  inherited Create;
  FUseQuirksMode := AUseQuirksMode;
end;

procedure THtmlStyleParser.GetCh;
var
  LastCh: ThtChar;
begin
  LCh := Doc.NextChar;
  case LCh of
    ThtChar(^M),
    ThtChar(^J),
    ThtChar(^I),
    ThtChar(^L):
      LCh := SpcChar;

    ThtChar('/'):
      if Doc.PeekChar = '*' then
      begin
        repeat
          LastCh := LCh;
          LCh := Doc.NextChar;
          if LCh = EofChar then
            raise EParseError.Create('Unterminated comment in style file: ' + Doc.Name);
        until (LCh = '/') and (LastCh = '*');
        LCh := SpcChar;
      end;
  end;
end;

//-- BG ---------------------------------------------------------- 13.03.2011 --
function THtmlStyleParser.GetIdentifier(out Identifier: ThtString): Boolean;
begin
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/syndata.html#value-def-identifier

  // can contain only the characters [a-zA-Z0-9] and ISO 10646 characters U+00A0 and higher,
  // plus the hyphen (-) and the underscore (_);
  // Identifiers can also contain escaped characters and any ISO 10646 character as a numeric code
  // (see next item). For instance, the identifier "B&W?" may be written as "B\&W\?" or "B\26 W\3F".

  Result := True;
  SetLength(Identifier, 0);

  // they cannot start with a digit, two hyphens, or a hyphen followed by a digit.
  case LCh of
    '0'..'9':
      Result := False;

    '-':
    begin
      case Doc.PeekChar of
        '0'..'9', '-':
          Result := False;
      else
        SetLength(Identifier, Length(Identifier) + 1);
        Identifier[Length(Identifier)] := LCh;
        GetCh;
      end;
    end;
  end;

  // loop through all allowed charaters:
  while Result do
  begin
    case LCh of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_': ;
    else
      if LCh < #$A0 then
        break;
    end;
    SetLength(Identifier, Length(Identifier) + 1);
    Identifier[Length(Identifier)] := LCh;
    GetCh;
  end;

  if Result then
    Result := Length(Identifier) > 0;
end;


//-- BG ---------------------------------------------------------- 13.03.2011 --
function THtmlStyleParser.GetString(out Str: ThtString): Boolean;
// Must start and end with single or double quote.
// Returns string incl. quotes and with the original escape sequences.
var
  Esc: Boolean;
  Term: ThtChar;
begin
  Term := #0; // valium for the compiler
  SetLength(Str, 0);
  case LCh of
    '''', '"':
    begin
      SetLength(Str, Length(Str) + 1);
      Str[Length(Str)] := LCh;
      Term := LCh;
      Result := True;
    end;
  else
    Result := False;
  end;

  Esc := False;
  while Result do
  begin
    GetCh;
    case LCh of
      '\':
      begin
        SetLength(Str, Length(Str) + 1);
        Str[Length(Str)] := LCh;
        Esc := True;
      end;

      LfChar:
      begin
        Result := False;
        break;
      end;
    else
      SetLength(Str, Length(Str) + 1);
      Str[Length(Str)] := LCh;
      if (LCh = Term) and not Esc then
      begin
        GetCh;
        break;
      end;
      Esc := False;
    end;
  end;
end;

{-------------SkipWhiteSpace}

procedure THtmlStyleParser.SkipWhiteSpace;
begin
  while LCh = ' ' do
    GetCh;
end;

{----------------AddPath}

function THtmlStyleParser.AddPath(S: ThtString): ThtString;
{for <link> styles, the path is relative to that of the stylesheet directory
 and must be added now}
begin
  S := ReadUrl(S); {extract the info from url(....) }
  if (Pos('://', LinkPath) > 0) then {it's TFrameBrowser and URL}
    if not IsFullUrl(S) then
      Result := CombineURL(LinkPath, S)
    else
      Result := S
  else
  begin
    S := HTMLToDos(S);
    if (Pos(':', S) <> 2) and (Pos('\\', Result) <> 1) then
      Result := LinkPath + S
    else
      Result := S;
  end;
{
IMPORTANT!!!

You must enclose the URL in quotation marks to prevent the code choking
on paths specifiers that contain spaces.  Spaces are legal filename characters.
}
  Result := 'url("' + Result + '")';
end;

function FindShortHand(S: ThtString; out Index: TShortHand): boolean;
var
  I: TShortHand;
begin
  for I := Low(TShortHand) to High(TShortHand) do
    if S = ShortHands[I] then
    begin
      Result := True;
      Index := I;
      Exit;
    end;
  Result := False;
end;

procedure SplitString(Src: ThtString; out Dest: array of ThtString; out Count: integer);
{Split a Src ThtString into pieces returned in the Dest ThtString array.  Splitting
 is on spaces with spaces within quotes being ignored.  ThtString containing a '/'
 are also split to allow for the "size/line-height" Font construct. }
var
  I, Q, Q1, N: integer;
  Z: ThtString;
  Done: boolean;
  Match: ThtChar;
begin
  Src := Trim(Src);
  I := Pos('  ', Src);
  while I > 0 do {simplify operation by removing extra white space}
  begin
    Delete(Src, I + 1, 1);
    I := Pos('  ', Src);
  end;
  I := Pos(', ', Src);
  while I > 0 do {simplify operation by removing spaces after commas}
  begin
    Delete(Src, I + 1, 1);
    I := Pos(', ', Src);
  end;

  N := 0;
  while (N <= High(Dest)) and (Src <> '') do
  begin
    Z := '';
    repeat
      Done := True;
      I := Pos(' ', Src);
      Q := Pos('"', Src);
      Q1 := Pos('''', Src);
      if (Q1 > 0) and ((Q > 0) and (Q1 < Q) or (Q = 0)) then
      begin
        Q := Q1;
        Match := ''''; {the matching quote ThtChar}
      end
      else
        Match := '"';
      if I = 0 then
      begin
        Z := Z + Src;
        Src := '';
      end
      else if (Q = 0) or (I < Q) then
      begin
        Z := Z + Copy(Src, 1, I - 1);
        Delete(Src, 1, I);
      end
      else {Q<I} {quoted ThtString found}
      begin
        Z := Z + Copy(Src, 1, Q); {copy to quote}
        Delete(Src, 1, Q);
        Q := Pos(Match, Src); {find next quote}
        if Q > 0 then
        begin
          Z := Z + Copy(Src, 1, Q); {copy to second quote}
          Delete(Src, 1, Q);
          Done := False; {go back and find the space}
        end
        else {oops, missing second quote, copy remaining}
        begin
          Z := Z + Src;
          Src := '';
        end;
      end;
    until Done;
    I := Pos('/', Z); {look for splitter for Line-height}
    if I >= 2 then
    begin {this part is font size}
      Dest[N] := Copy(Z, 1, I - 1);
      Delete(Z, 1, I - 1);
      Inc(N);
    end;
    if N <= High(Dest) then
      Dest[N] := Z;
    Inc(N);
  end;
  Count := N;
end;

procedure ExtractParn(var Src: ThtString; var Dest: array of ThtString; var Count: integer);
{Look for strings in parenthesis like "url(....)" or rgb(...)".  Return these in
 Dest Array.  Return Src without the extracted ThtString}
var
  I, J: integer;

begin
  Count := 0;
  while (Count <= High(Dest)) and (Src <> '') do
  begin
    I := Pos('url(', Src);
    if I = 0 then
      I := Pos('rgb(', Src);
    if I = 0 then
      I := Pos('rgba(', Src);
    if I = 0 then
      I := Pos('hsla(', Src);
    if I = 0 then
      I := Pos('hsl(', Src);
    if I = 0 then
      Exit;
    J := Pos(')', Src);
    if (J = 0) or (J < I) then
      Exit;
    Dest[Count] := Copy(Src, I, J - I + 1);
    Delete(Src, I, J - I + 1);
    Inc(Count);
  end;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleParser.ProcessShortHand(Index: TShortHand; const Prop, OrigValue, StrippedValue: ThtString);

  procedure DoBackground(Value: ThtString);
  { do the Background shorthand property specifier }
  type
    TShortHandedProps = (shColor, shImage, shRepeat, shAttachment, shPosition);
  var
    S: array[0..6] of ThtString;
    S1: ThtString;
    Count, I, N: integer;
    Dummy: TColor;
    Values: array [TShortHandedProps] of ThtString;
  begin
    // http://www.w3.org/TR/CSS21/colors.html#propdef-background :
    // Given a valid declaration, the 'background' property first sets all the individual background
    // properties to their initial values, then assigns explicit values given in the declaration.
    Values[shColor] := 'transparent';
    Values[shImage] := 'none';
    Values[shRepeat] := 'repeat';
    Values[shAttachment] := 'scroll';
    Values[shPosition] := '0% 0%';

    // process color and image
    ExtractParn(Value, S, Count);
    for I := 0 to Count - 1 do
    begin
      if Pos('rgb(', S[I]) > 0 then
        Values[shColor] := S[I]
      else if Pos('rgba(', S[I]) > 0 then
        Values[shColor] := S[I]
      else if Pos('hsla(', S[I]) > 0 then
        Values[shColor] := S[I]
      else if Pos('hsl(', S[I]) > 0 then
        Values[shColor] := S[I]
      else if (Pos('url(', S[I]) > 0) then
      begin
        if LinkPath <> '' then {path added now only for <link...>}
          S[I] := AddPath(S[I]);
        Values[shImage] := S[I];
      end;
    end;

    SplitString(Value, S, Count);
    for I := 0 to Count - 1 do
      if ColorFromString(S[I], NeedPound, Dummy) then
      begin
        Values[shColor] := S[I];
        S[I] := '';
      end
      else if S[I] = 'none' then
      begin
        Values[shImage] := S[I];
        S[I] := '';
      end;

    ProcessProperty('background-color', Values[shColor]);
    ProcessProperty('background-image', Values[shImage]);

    // process repeat
    for I := 0 to Count - 1 do
      if Pos('repeat', S[I]) > 0 then
      begin
        Values[shRepeat] := S[I];
        S[I] := '';
      end;
    ProcessProperty('background-repeat', Values[shRepeat]);

    // process attachment
    for I := 0 to Count - 1 do
      if (S[I] = 'fixed') or (S[I] = 'scroll') then
      begin
        Values[shAttachment] := S[I];
        S[I] := '';
      end;
    ProcessProperty('background-attachment', Values[shAttachment]);

    // process position
    N := 0;
    S1 := ''; {any remaining are assumed to be position info}
    for I := Count - 1 downto 0 do
      if S[I] <> '' then
      begin
        S1 := S[I] + ' ' + S1;
        Inc(N);
        if N >= 2 then
          Break; {take only last two}
      end;
    if S1 <> '' then
      Values[shPosition] := S1;
    ProcessProperty('background-position', Values[shPosition]);
  end;

  procedure DoBorder(Prop, Value: ThtString);
  { do the Border, Border-Top/Right/Bottom/Left shorthand properties.  However, there
    currently is only one style and color supported for all border sides }
  type
    TShortHandedProps = (shWidth, shStyle, shColor);
  var
    S: array[0..6] of ThtString;
    Count, I: integer;
    Dummy: TColor;
    Values: array [TShortHandedProps] of ThtString;

    function FindStyle(const S: ThtString): boolean;
    const
      Ar: array[1..9] of ThtString = ('none', 'solid', 'dashed', 'dotted', 'double', 'groove',
        'inset', 'outset', 'ridge');
    var
      I: integer;
    begin
      for I := 1 to 9 do
        if S = Ar[I] then
        begin
          Result := True;
          Exit;
        end;
      Result := False;
    end;

    procedure ProcessBorderProperty(const Erty, Value: ThtString);
    begin
      if Prop = 'border' then
      begin
        ProcessProperty(Prop + '-top' + Erty, Value);
        ProcessProperty(Prop + '-right' + Erty, Value);
        ProcessProperty(Prop + '-bottom' + Erty, Value);
        ProcessProperty(Prop + '-left' + Erty, Value);
      end
      else
        ProcessProperty(Prop + Erty, Value);
    end;

  begin
    Values[shWidth] := 'medium';
    Values[shStyle] := 'none';
    // TODO: BG, 04.01.2012: set default color from 'color' property
    Values[shColor] := '';

    ExtractParn(Value, S, Count);
    for I := 0 to Count - 1 do
      if ColorFromString(S[I], NeedPound, Dummy) then
        Values[shColor] := S[I];

    SplitString(Value, S, Count);
    for I := 0 to Count - 1 do
    begin
      if ColorFromString(S[I], NeedPound, Dummy) then
        Values[shColor] := S[I]
      else if FindStyle(S[I]) then
        Values[shStyle] := S[I]
      else
        Values[shWidth] := S[I];
    end;

    if Values[shColor] <> '' then
      ProcessBorderProperty('-color', Values[shColor]);
    ProcessBorderProperty('-width',  Values[shWidth]);
    ProcessBorderProperty('-style',  Values[shStyle]);
  end;

  procedure DoFont(const Value: ThtString);
  { do the Font shorthand property specifier }
  type
    TShortHandedProps = (shStyle, shVariant, shWeight, shSize, shHeight, shFamily);
    FontEnum = (
      // font-style
      italic, oblique,
      // font-weight
      normal, bolder, lighter, bold,
      // font-variant
      smallcaps,
      // font-size
      larger, smaller, xxsmall, xsmall, small, medium, large, xlarge, xxlarge
    );
  const
    FontWords: array[italic..xxlarge] of ThtString = (
      'italic', 'oblique',
      'normal', 'bolder', 'lighter', 'bold',
      'small-caps',
      'larger', 'smaller', 'xx-small', 'x-small', 'small', 'medium', 'large', 'x-large', 'xx-large'
    );
  var
    S: array[0..6] of ThtString;
    Count, I, J: integer;
    Index: FontEnum;
    Values: array [TShortHandedProps] of ThtString;

    function FindWord(const S: ThtString; var Index: FontEnum): boolean;
    var
      I: FontEnum;
    begin
      Result := False;
      for I := Low(FontEnum) to High(FontEnum) do
        if FontWords[I] = S then
        begin
          Result := True;
          Index := I;
          Exit;
        end;
    end;

  begin
    // initial values
    Values[shStyle] := 'normal';
    Values[shVariant] := 'normal';
    Values[shWeight] := 'normal';
    if Self.FUseQuirksMode then begin
      Values[shSize] := 'small';
    end else begin
      Values[shSize] := 'medium';
    end;
    Values[shHeight] := 'normal';
    // TODO: BG, 04.01.2012: set default font as set in THtmlViewer:
    Values[shFamily] := '';

    // specified values
    SplitString(Value, S, Count);
    for I := 0 to Count - 1 do
    begin
      case S[I, 1] of
        '/':
          Values[shHeight] := Copy(S[I], 2, Length(S[I]) - 1);

        '0'..'9':
          begin
            // BG, 04.01.2012: syntax is <font-size>[/<line-height>]?
            // Therefore find '/' at the end of font-size:
            J := pos('/', S[I]);
            if J > 1 then
            begin
              Values[shSize] := Copy(S[I], 1, J - 1);
              Values[shHeight] := Copy(S[I], J + 1, Length(S[I]) - J);
            end
            else
            {the following will pass 100pt, 100px, but not 100 or larger}
              if StrToIntDef(S[I], -1) < 100 then
                Values[shSize] := S[I];
          end;
      else
        if FindWord(S[I], Index) then
        begin
          case Index of
            italic, oblique:  Values[shStyle] := S[I];
            normal..bold:     Values[shWeight] := S[I];
            smallcaps:        Values[shVariant] := S[I];
            larger..xxlarge:  Values[shSize] := S[I];
          end;
        end
        else
          Values[shFamily] := S[I];
      end;
    end;

    // set values to properties
    ProcessProperty('font-style', Values[shStyle]);
    ProcessProperty('font-variant', Values[shVariant]);
    ProcessProperty('font-weight', Values[shWeight]);
    ProcessProperty('font-size', Values[shSize]);
    ProcessProperty('line-height', Values[shHeight]);
    // TODO: BG, 04.01.2012: set default font as set in THtmlViewer:
    if Values[shFamily] <> '' then
      ProcessProperty('font-family', Values[shFamily]);
  end;

  procedure DoListStyle(const Value: ThtString);
  { do the List-Style shorthand property specifier }
  type
    TShortHandedProps = (shType, shPosition, shImage);
  var
    S: array[0..6] of ThtString;
    Count, I: integer;
    Values: array [TShortHandedProps] of ThtString;
  begin
    Values[shType] := 'disc';
    if Self.FUseQuirksMode then begin
      Values[shPosition] := 'inside';
    end else begin
      Values[shPosition] := 'outside';
    end;
    Values[shImage] := 'none';

    SplitString(Value, S, Count);
    for I := 0 to Count - 1 do
      if S[I] = 'none' then
      begin
        Values[shType] := S[I];
        S[I] := '';
      end;

    for I := 0 to Count - 1 do
    begin
      if Pos('url(', S[I]) > 0 then
      begin
        if LinkPath <> '' then {path added now only for <link...>}
          S[I] := AddPath(S[I]);
        Values[shImage] := S[I];
      end
      else if (S[I] = 'inside') or (S[I] = 'outside') then
        Values[shPosition] := S[I]
      else if S[I] <> '' then
        Values[shType] := S[I];
    end;

    ProcessProperty('list-style-type', Values[shType]);
    ProcessProperty('list-style-position', Values[shPosition]);
    ProcessProperty('list-style-image', Values[shImage]);
  end;

  procedure DoMarginItems(X: TShortHand; const Value: ThtString);
  { Do the Margin, Border, Padding shorthand property specifiers}
  var
    S: array[0..3] of ThtString;
    I, Count: integer;
    Index: array[0..3] of PropIndices;

  begin
    if Value = '' then
      Exit;

    SplitString(Value, S, Count); {split Value into parts}

    case X of
      MarginX: Index[0] := MarginTop;
      PaddingX: Index[0] := PaddingTop;
      BorderWidthX: Index[0] := BorderTopWidth;
      BorderColorX: Index[0] := BorderTopColor;
      BorderStyleX: Index[0] := BorderTopStyle;
    end;

    for I := 1 to 3 do
      Index[I] := Succ(Index[I - 1]);

    ProcessProperty(PropWords[Index[0]], S[0]);
    case Count of
      1: for I := 1 to 3 do
          ProcessProperty(PropWords[Index[I]], S[0]);
      2:
        begin
          ProcessProperty(PropWords[Index[2]], S[0]);
          ProcessProperty(PropWords[Index[1]], S[1]);
          ProcessProperty(PropWords[Index[3]], S[1]);
        end;
      3:
        begin
          ProcessProperty(PropWords[Index[2]], S[2]);
          ProcessProperty(PropWords[Index[1]], S[1]);
          ProcessProperty(PropWords[Index[3]], S[1]);
        end;
      4:
        begin
          ProcessProperty(PropWords[Index[1]], S[1]);
          ProcessProperty(PropWords[Index[2]], S[2]);
          ProcessProperty(PropWords[Index[3]], S[3]);
        end;
    end;
  end;

begin
  case Index of
    MarginX, BorderWidthX, PaddingX, BorderColorX, BorderStyleX:
      DoMarginItems(Index, StrippedValue);

    FontX:
      DoFont(OrigValue);

    BackgroundX:
      DoBackground(StrippedValue);

    BorderX..BorderLX:
      DoBorder(Prop, StrippedValue);

    ListStyleX:
      DoListStyle(StrippedValue);
  end;
end;

{----------------SortContextualItems}

function SortContextualItems(S: ThtString): ThtString;
{Put a ThtString of contextual items in a standard form for comparison purposes.
 div.ghi#def:hover.abc
   would become
 div.abc.ghi:hover#def
 Enter with S as lowercase
}
const
  Eos = #0;
var
  Ch, C: ThtChar;
  SS: ThtString;
  SL: ThtStringList;
  I: integer;

  procedure GetCh;
  begin
    if I <= Length(S) then
    begin
      Ch := S[I];
      Inc(I);
    end
    else
      Ch := Eos;
  end;

begin
  Result := '';
  SL := ThtStringList.Create; {ThtStringList to do sorting}
  try
    SL.Sorted := True;
    I := 1;
    GetCh;
    while Ch <> Eos do
    begin
      case Ch of {add digit to sort item}
        '.': C := '1';
        ':': C := '2';
        '#': C := '3';
      else
        C := '0';
      end;
      SetLength(SS, 2);
      SS[1] := C;
      SS[2] := Ch;
      GetCh;
      while True do
        case Ch of
          'a'..'z', '0'..'9', '_', '-':
          begin
            htAppendChr(SS, Ch);
            GetCh;
          end;
        else
          break;
        end;
      SL.Add(SS);
    end;
    for I := 0 to SL.Count - 1 do
      Result := Result + Copy(SL.Strings[I], 2, Length(SL.Strings[I]) - 1);
  finally
    SL.Free;
  end;
end;

{ THtmlStyleTagParser }

//-- BG ---------------------------------------------------------- 29.12.2010 --
constructor THtmlStyleTagParser.Create(const AUseQuirksMode: Boolean);
begin
  inherited Create(AUseQuirksMode);
  Selectors := ThtStringList.Create;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
destructor THtmlStyleTagParser.Destroy;
begin
  Selectors.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleTagParser.DoStyle(Styles: TStyleList; var C: ThtChar; Doc: TBuffer; const APath: ThtString; FromLink: boolean);
var
  AvailableMedia: TMediaTypes;

  procedure ReadAt;
  {read @import and @media}

    function GetMediaTypes: TMediaTypes;
    var
      Identifier: ThtString;
      MediaType: TMediaType;
    begin
      Result := [];
      SkipWhiteSpace;
      if not GetIdentifier(Identifier) then
        exit;
      repeat
        if TryStrToMediaType(Identifier, MediaType) then
          Include(Result, MediaType);
        SkipWhiteSpace;
        if LCh <> ',' then
          break;
        GetCh;
        SkipWhiteSpace;
        if not GetIdentifier(Identifier) then
          break;
      until False;
    end;

    procedure SkipRule(Depth: Integer);
    begin
      repeat
        GetCh;
        case LCh of
          '{':
            Inc(Depth);

          '}':
            begin
              Dec(Depth);
              if Depth = 0 then
              begin
                GetCh;
                break;
              end;
            end;

          #0, '<':
            break;
        end;
      until False;
    end;

    procedure DoMedia;
    var
      Media: TMediaTypes;
    begin
      Media := GetMediaTypes;
      if Media = [] then
        Include(Media, mtAll);
      case LCh of
        '{':
        begin
          if Media * AvailableMedia <> [] then
          begin {parse @media screen}
            GetCh;
            repeat
              Selectors.Clear;
              GetSelectors;
              GetCollection;
              SkipWhiteSpace;

              case LCh of
                '}':
                begin
                  GetCh;
                  break;
                end;

                '<', EofChar:
                  break;
              end;
            until False;
          end
          else
            SkipRule(1);
        end;

        ';':
          GetCh;
      else
        // BG, 07.01.2012: following J. Peter Mugaas' fix.
        // CSS 2.1 skips unknown or malformed entries.
        // I.e. you may see media queries defined by http://www.w3.org/TR/css3-mediaqueries/
        SkipRule(0);
      end;
    end;

    procedure DoImport;
    var
      Result: Boolean;
      URL: ThtString;
      Media: TMediaTypes;
    begin
      Result := False;
      SkipWhiteSpace;
      case LCh of
        '"':
          Result := GetString(URL);

        'u':
          if GetIdentifier(URL) then
            if LowerCase(URL) = 'url' then
              if LCh = '(' then
              begin
                GetCh;
                SkipWhiteSpace;
                if GetString(URL) then
                begin
                  SkipWhiteSpace;
                  Result := LCh = ')';
                  if Result then
                    GetCh;
                end;
              end;
      end;

      if Result then
      begin
        Media := GetMediaTypes;
        if Media = [] then
          Include(Media, mtAll);
        if Media * AvailableMedia <> [] then
          // TODO -oBG, 13.03.2011: read style sheet from import
          // The import style sheet parser must return the list of rulesets.
          // I must insert the imported rulesets at the beginning of my list of rulesets
          // to gain a lower precedence than my rulesets of the same selectors.
          ;
      end;
      case LCh of
        ';':
          GetCh;
      else
        // BG, 07.01.2012: following J. Peter Mugaas' fix.
        // CSS 2.1 skips unknown or malformed entries.
        // I.e. you may see media queries defined by http://www.w3.org/TR/css3-mediaqueries/
        SkipRule(0);
      end;
    end;

  var
    AtRule: ThtString;
  begin
    GetCh; // skip the '@';
    if GetIdentifier(AtRule) then
    begin
      AtRule := LowerCase(AtRule);
      if AtRule = 'media' then
        DoMedia
      else if AtRule = 'import' then
        DoImport;
    end;
  end;

begin
  AvailableMedia := [mtAll, mtScreen];
  Self.Doc := Doc;
  Self.Styles := Styles;
  LinkPath := APath;
{enter with the first character in C}
  if C = ^M then
    C := ' ';

  LCh := ' '; {This trick is needed if the first ThtChar is part of comment, '/*'}

  while (LCh = ' ') or (LCh = '<') or (LCh = '>') or (LCh = '!') or (LCh = '-') do {'<' will probably be present from <style>}
    GetCh;
  repeat
    if LCh = '@' then
      ReadAt
    else if LCh = '<' then
    begin {someone left a tag here, ignore it}
      repeat
        GetCh;
      until (LCh = ' ') or (LCh = EOFChar);
      SkipWhiteSpace;
    end
    else
    begin
      Selectors.Clear;
      GetSelectors;
      GetCollection;
    end;
    while (LCh = ' ') or (LCh = '-') or (LCh = '>') do
      GetCh;
  until (LCh = EOFChar) or ((LCh = '<') and not FromLink);
  C := LCh;
  Self.Styles := nil;
  Self.Doc := nil;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleTagParser.GetCollection;
//Read a series of property, value pairs such as "Text-Align: Center;" between
//  '{', '}'  brackets. Add these to the Styles list for the specified selectors
var
  Top: ThtChar;
  MoreStack: ThtString;

  procedure Push(Ch: ThtChar);
  var
    I: Integer;
  begin
    if Top <> EofChar then
    begin
      I := Length(MoreStack) + 1;
      SetLength(MoreStack, I);
      MoreStack[I] := Top;
    end;
    Top := Ch;
  end;

  procedure Pop;
  var
    I: Integer;
  begin
    I := Length(MoreStack);
    if I > 0 then
    begin
      Top := MoreStack[I];
      SetLength(MoreStack, I - 1);
    end
    else
      Top := EofChar;
  end;

  function IsTermChar(LCh: ThtChar): Boolean;
  begin
    case LCh of
      '}', '<', EofChar:
        Result := True;
    else
      Result := False;
    end;
  end;

var
  Prop, Value, Value1: ThtString;
  Index: TShortHand;
begin
  if LCh = '{' then
  begin
    GetCh;
    repeat
      SkipWhiteSpace;
      if GetIdentifier(Prop) then
      begin
        SkipWhiteSpace;
        if (LCh = ':') or (LCh = '=') then
        begin
          GetCh;
          SkipWhiteSpace;

          SetLength(Value, 0);
          while not ((LCh = ';') or IsTermChar(LCh)) do
          begin
            SetLength(Value, Length(Value) + 1);
            Value[Length(Value)] := LCh;
            GetCh;
          end;
          Value1 := LowerCase(Trim(Value)); {leave quotes on for font:}
          Value := RemoveQuotes(Value1);

          Prop := LowerCase(Prop);
          if FindShortHand(Prop, Index) then
            ProcessShortHand(Index, Prop, Value, Value1)
          else
          begin
            if (LinkPath <> '') and (Pos('url(', Value) > 0) then
              Value := AddPath(Value);
            ProcessProperty(Prop, Value);
          end;
        end;
      end;

      // Skip trailing ';' or any erroneous/unknown syntax observing the rules
      // for matching pairs of (), [], {}, "", and '' until and including next ';'
      while True do
      begin
        case LCh of

          ';':
            if Top = EofChar then
            begin
              GetCh;
              break;
            end;

          '{': Push('}');
          '(': Push(')');
          '[': Push(']');

          '"', '''':
            if LCh = Top then
              Pop
            else
              Push(LCh);

          '}':
          begin
            if LCh = Top then
              Pop;
            if Top = EofChar then
              break;
          end;

          '<':
            if Top = EofChar then
              break;

          EofChar:
            break;

        else
          if LCh = Top then
            Pop;
        end;
        GetCh;
      end;
    until IsTermChar(LCh);
    if LCh = '}' then
      GetCh;
  end;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleTagParser.GetSelectors;
{Get a series of selectors seperated by ',', like:  H1, H2, .foo }
var
  S: ThtString;
  Sort: Boolean;
  Cnt: integer;

  function FormatContextualSelector(S: ThtString; Sort: boolean): ThtString;
  {Takes a contextual selector and reverses the order.  Ex: 'div p em' will
   change to 'em Np div'.   N is a number added.  The first digit of N is
   the number of extra selector items.  The remainder of the number is a sequence
   number which serves to sort entries by time parsed.}
  var
    I, Cnt: integer;
    //Tmp: ThtString;

    function DoSort(St: ThtString): ThtString;
    begin
      if Sort then
        Result := SortContextualItems(St)
      else
        Result := St;
    end;

  begin
    Result := '';
    Cnt := 0;
  {make sure a space follows '>' and there are none preceding a '>'}
    I := 1;
    while I <= Length(S) do
    begin
      if S[I] = '>' then
        if (I = 1) or (I = Length(S)) then
        begin
          Delete(S, I, 1);
          Dec(I);
        end
        else
        begin
          if S[I + 1] <> ' ' then
            Insert(' ', S, I + 1);
          while (I > 1) and (S[I - 1] = ' ') do
          begin
            Delete(S, I - 1, 1);
            Dec(I);
          end;
        end;
      Inc(I);
    end;

    I := Pos(' ', S);
    if (I > 0) and (Cnt <= 8) then
    begin
      while I > 0 do
      begin
        Inc(Cnt);
        Insert(DoSort(Copy(S, 1, I - 1)) + ' ', Result, 1);
        S := Trim(Copy(S, I + 1, Length(S)));
        I := Pos(' ', S);
      end;
      if S <> '' then
        Result := DoSort(S) + ' ' + Result;
      I := Pos(' ', Result);
      // Str(Cnt, Tmp);
      Insert(IntToStr(Cnt) + Styles.GetSeqNo, Result, I + 1);
    end
    else
      Result := DoSort(S);
  end;

var
  Ignore: Boolean;
begin
  repeat
    SkipWhiteSpace;
    S := '';
    Sort := False;
    Ignore := False;
    Cnt := 0;
    repeat
      case LCh of
        'A'..'Z',
        'a'..'z',
        '0'..'9',
        '_', '-', '>': ;

        '+': Ignore := True; // ignore these otherwize e1 is selected // e1 + e2 --> selects e2, if it follows directly e1
        // '>': ; // e1 > e2 --> selects e2, if it is 1 level below e1 (e2 is child of e1)
        // '*': ; // e1 * e2 --> selects e2, if it is at least 2 levels below e1 (e2 is at least grandchild of e1)

        '.', ':', '#': {2 or more of these in an item will require a sort to put in standard form}
          begin
            Inc(Cnt);
            if Cnt = 2 then
              Sort := True;
          end;
        ' ': Cnt := 0;
        '*': LCh := ' ';
      else
        break;
      end;
      SetLength(S, Length(S) + 1);
      S[Length(S)] := LCh;
      GetCh;
    until false;
    if not Ignore then
    begin
      S := FormatContextualSelector(Lowercase(Trim(S)), Sort);
      Selectors.Add(S);
    end;
    if LCh <> ',' then
      break;
    GetCh;
  until False;
  while not ((LCh = '{') or (LCh = '<') or (LCh = EofChar)) do
    GetCh;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleTagParser.ProcessProperty(const Prop, Value: ThtString);
var
  I: integer;
begin
  for I := 0 to Selectors.Count - 1 do
    Styles.AddModifyProp(Selectors[I], Prop, Value);
end;

{ THtmlStyleAttrParser }

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleAttrParser.ParseProperties(Doc: TBuffer; Propty: TProperties);
var
  Prop, Value, Value1: ThtString;
  Index: TShortHand;
begin
  Self.Doc := Doc;
  Self.Propty := Propty;
  LinkPath := '';
  GetCh;
  repeat
    Prop := '';
    SkipWhiteSpace;
    while True do
      case LCh of
        'A'..'Z', 'a'..'z', '0'..'9', '-':
          begin
            Prop := Prop + LCh;
            GetCh;
          end;
      else
        break;
      end;
    Prop := LowerCase(Trim(Prop));
    SkipWhiteSpace;
    if (LCh = ':') or (LCh = '=') then
    begin
      GetCh;
      Value := '';
      while not ((LCh = ';') or (LCh = EofChar)) do
      begin
        Value := Value + LCh;
        GetCh;
      end;
      Value1 := LowerCase(Trim(Value)); {leave quotes on for font}
      Value := RemoveQuotes(Value1);

      if FindShortHand(Prop, Index) then
        ProcessShortHand(Index, Prop, Value, Value1)
      else
        ProcessProperty(Prop, Value);
    end;
    SkipWhiteSpace;
    if LCh = ';' then
      GetCh;
    while True do
      case LCh of
        'A'..'Z', 'a'..'z', '0'..'9', '-', EofChar:
          break;
      else
        GetCh;
      end;
  until LCh = EofChar;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleAttrParser.ProcessProperty(const Prop, Value: ThtString);
begin
  Propty.AddPropertyByName(Prop, Value);
end;

end.
