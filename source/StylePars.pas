{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2016 by HtmlViewer Team

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
{$ifdef LCL}
  variants,
{$endif}
  Classes, Graphics, SysUtils,
  HtmlGlobals, HtmlBuffer, URLSubs, StyleUn, StyleTypes;


{---------  Detect Shorthand syntax }
type
  EParseError = class(Exception);

  ThtTermCharKind = (tckNone, tckGood, tckBad);

  { THtmlStyleParser }

  THtmlStyleParser = class
  private
    Doc: TBuffer;
    LCh: ThtChar;
    LinkPath: ThtString;
    FUseQuirksMode : Boolean;
    // parser methods
    procedure GetCh(WantLfChar: Boolean = False);
    procedure SkipWhiteSpace(WantLfChar: Boolean = False);
    function GetIdentifier(out Identifier: ThtString): Boolean;
    function GetString(out Str: ThtString): Boolean;
    //
    function AddPath(const S: ThtString): ThtString;
    procedure ProcessShortHand(Index: TShortHand; const Prop, OrigValue, StrippedValue: ThtString; IsImportant: Boolean);
  protected
    constructor Create(const AUseQuirksMode : Boolean);

    procedure GetCollection(const GoodTermChars, BadTermChars: ThtString);
    procedure ProcessProperty(const Prop, Value: ThtString; IsImportant: Boolean); virtual; abstract;
    procedure ProcessPropertyOrShortHand(Prop, Value: ThtString; IsImportant: Boolean);
  end;

  THtmlStyleTagParser = class(THtmlStyleParser)
  private
    FOnMatchMediaQuery: ThtMediaQueryEvent;
    Selectors: ThtStringList;
    Styles: TStyleList;
    procedure GetCollection;
    procedure GetSelectors;
  protected
    procedure ProcessProperty(const Prop, Value: ThtString; IsImportant: Boolean); override;
  public
    constructor Create(const AUseQuirksMode : Boolean);
    destructor Destroy; override;
    procedure DoStyle(Styles: TStyleList; var C: ThtChar; Doc: TBuffer; const APath, AMedia: ThtString; FromLink: boolean);
    property OnMatchMediaQuery: ThtMediaQueryEvent read FOnMatchMediaQuery write FOnMatchMediaQuery;
  end;

  THtmlStyleAttrParser = class(THtmlStyleParser)
  private
    Propty: TProperties;
  protected
    procedure ProcessProperty(const Prop, Value: ThtString; IsImportant: Boolean); override;
  public
    procedure ParseProperties(Doc: TBuffer; Propty: TProperties);
  end;

procedure ParsePropertyStr(const PropertyStr: ThtString; Propty: TProperties);
function SortContextualItems(const S: ThtString): ThtString;

implementation

const
  NeedPound = True;

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
procedure ParsePropertyStr(const PropertyStr: ThtString; Propty: TProperties);
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

procedure THtmlStyleParser.GetCh(WantLfChar: Boolean);
var
  LastCh: ThtChar;
begin
  LCh := Doc.NextChar;
  case LCh of
    CrChar:
      if Doc.PeekChar = LfChar then
        GetCh(WantLfChar)
      else if WantLfChar then
        LCh := LfChar
      else
        LCh := SpcChar;

    FfChar,
    LfChar:
      if WantLfChar then
        LCh := LfChar
      else
        LCh := SpcChar;

    TabChar:
      LCh := SpcChar;

    ThtChar('/'):
      if Doc.PeekChar = '*' then
      begin
        repeat
          LastCh := LCh;
          LCh := Doc.NextChar;
          if LCh = EofChar then
            raise EParseError.Create('Unterminated comment in style file: ' + htStringToString(Doc.Name));
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


//-- BG ---------------------------------------------------------- 02.03.2016 --
function GetTermCharKind(LCh: ThtChar; const GoodTermChars, BadTermChars: ThtString): ThtTermCharKind;
begin
  if Pos(LCh, GoodTermChars) > 0 then
    Result := tckGood
  else if Pos(LCh, BadTermChars) > 0 then
    Result := tckBad
  else if LCh = EofChar then
    Result := tckBad
  else
    Result := tckNone;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleParser.GetCollection(const GoodTermChars, BadTermChars: ThtString);
//Read a series of property/value pairs such as "Text-Align: Center;" between
//  '{', '}'  brackets. Add these to the Styles list for the specified selectors
var
  Top: ThtChar;
  MoreStack: ThtString;
  Strings: Integer;
  InString: Boolean;

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

  function TryPop(LCh: ThtChar): Boolean;
  var
    I: Integer;
  begin
    Result := Top = LCh;
    if Result then
      Pop
    else
    begin
      for I := Length(MoreStack) downto 1 do
      begin
        Result := MoreStack[I] = LCh;
        if Result then
        begin
          SetLength(MoreStack, I-1);
          Pop;
          Break;
        end;
      end;
    end;
  end;

  procedure PopStrings;
  begin
    TryPop('''');
    TryPop('"');
    Strings := 0;
    InString := False;
  end;

var
  Prop, Value, Important: ThtString;
  ImportantSepPos: Integer;
  IsImportant: Boolean;
  TermCharKind: ThtTermCharKind;
begin
  Top := EofChar;
  Strings := 0;
  InString := False;
  GetCh;
  repeat
    SkipWhiteSpace;
    if GetIdentifier(Prop) then
    begin
      SkipWhiteSpace;
      if (LCh = ':') or (LCh = '=') then
      begin
        GetCh(True);
        SkipWhiteSpace(True);

        ImportantSepPos := 0;
        SetLength(Value, 0);
        { The ';' inside a quotation should not end a CSS value.  }
        while not ((LCh = ';') and (Top = EofChar)) and (GetTermCharKind(LCh, GoodTermChars, BadTermChars) = tckNone) do
        begin
          case LCh of
            '"', '''':
              if Top = LCh then
              begin
                Pop;
                Dec(Strings);
                if Strings <= 0 then
                begin
                  InString := False;
                  Strings := 0;
                end;
              end
              else
              begin
                Push(LCh);
                Inc(Strings);
                InString := True;
              end;

            '!':
              if not InString then
              begin
                ImportantSepPos := Length(Value) + 1;
                break;
              end;

            LfChar:
              if InString then
              begin
                PopStrings;
                Break;
              end;

            '(' :
              if not InString then
                Push(')');

            ')' :
              if LCh = Top then
                Pop;
          end;
          SetLength(Value, Length(Value) + 1);
          Value[Length(Value)] := LCh;
          GetCh(True);
        end;

        // support 'important'
        IsImportant := False;
        if ImportantSepPos > 0 then
        begin
          GetCh;
          SkipWhiteSpace;
          if GetIdentifier(Important) then
          begin
            SkipWhiteSpace;
            IsImportant := htLowerCase(Important) = 'important';
            if IsImportant then
              SetLength(Value, ImportantSepPos - 1);
          end
        end;
        ProcessPropertyOrShortHand(Prop, Value, IsImportant);
      end;
    end;

    // Skip trailing ';' or any erroneous/unknown syntax observing the rules
    // for matching pairs of (), [], {}, "", and '' until and including next ';'
    TermCharKind := GetTermCharKind(LCh, GoodTermChars, BadTermChars);
    while LCh <> EofChar do
    begin
      case TermCharKind of
        tckGood:
          if not InString then
          begin
            if LCh = Top then
              Pop;
            if Top = EofChar then
              break;
          end;

        tckBad:
          if not InString then
            if Top = EofChar then
              break;
      else
        case LCh of
          ';':
            if Top = EofChar then
            begin
              GetCh;
              break;
            end;

          '{':
            if not InString then
              Push('}');

          '(':
            if not InString then
              Push(')');

          '[':
            if not InString then
              Push(']');

          '"', '''':
            if Top = LCh then
            begin
              Pop;
              Dec(Strings);
              if Strings <= 0 then
              begin
                InString := False;
                Strings := 0;
              end;
            end
            else
            begin
              Push(LCh);
              Inc(Strings);
              InString := True;
            end;

          LfChar:
            if InString then
            begin
              PopStrings;
              Break;
            end;

          EofChar:
            break;

        else
          if LCh = Top then
            Pop;
        end;
      end;
      GetCh(True);
      TermCharKind := GetTermCharKind(LCh, GoodTermChars, BadTermChars);
    end;
  until TermCharKind <> tckNone;
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

procedure THtmlStyleParser.SkipWhiteSpace(WantLfChar: Boolean);
begin
  while (LCh = SpcChar) or (LCh = LfChar) do
    GetCh;
end;

{----------------AddPath}

function THtmlStyleParser.AddPath(const S: ThtString): ThtString;
{for <link> styles, the path is relative to that of the stylesheet directory
 and must be added now}
var
  I: integer;
begin
  Result := ReadUrl(S); {extract the info from url(....) }
  if (Pos('://', LinkPath) > 0) then {it's TFrameBrowser and URL}
  begin
    if not IsFullUrl(Result) then
      Result := CombineURL(LinkPath, Result);
  end
  else
  begin
    I := 1;
    if not FindSchemeSep(Result, I) or (htLowerCase(Copy(Result, 1, I)) = 'file:') then
    begin
      Result := HTMLToDos(Result);
      if not IsAbsolutePath(Result) then
        Result := CombineDos(LinkPath, Result);
    end;
  end;
{
IMPORTANT!!!

You must enclose the URL in quotation marks to prevent the code choking
on paths specifiers that contain spaces.  Spaces are legal filename characters.
}
  Result := 'url("' + Result + '")';
end;

//-- BG ---------------------------------------------------------- 26.11.2012 --
procedure THtmlStyleParser.ProcessPropertyOrShortHand(Prop, Value: ThtString; IsImportant: Boolean);

  function FindShortHand(S: ThtString; out Index: TShortHand): boolean;
  var
    I: TShortHand;
  begin
    for I := Low(TShortHand) to High(TShortHand) do
      if S = PropWords[I] then
      begin
        Result := True;
        Index := I;
        Exit;
      end;
    Result := False;
  end;

var
  Value1: ThtString;
  Index: TShortHand;
begin
  Value1 := LowerCaseUnquotedStr(htTrim(Value)); // leave quotes on for font
  Value := RemoveQuotes(Value1);
  Prop := htLowerCase(Prop);
  if FindShortHand(Prop, Index) then
    ProcessShortHand(Index, Prop, Value, Value1, IsImportant)
  else if Prop = 'font-family' then
    ProcessProperty(Prop, htLowerCase(Value1), IsImportant)
  else
  begin
    if (LinkPath <> '') and (Pos('url(', Value) > 0) then
      Value := AddPath(Value);
    ProcessProperty(Prop, Value, IsImportant);
  end;
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

procedure ExtractParn(var Src: ThtString; var Dest: array of ThtString; out Count: integer);
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
procedure THtmlStyleParser.ProcessShortHand(Index: TShortHand; const Prop, OrigValue, StrippedValue: ThtString; IsImportant: Boolean);

  //ProcessShortHand
  procedure DoBorderSpacing(Value: THtString);
  var
    S : array[0..1] of THtString;
    Count : Integer;
  begin
    SplitString(Value,S,Count);
    case Count of
      1:
      begin
        ProcessProperty('thv-border-spacing-horz', S[0], IsImportant);
        ProcessProperty('thv-border-spacing-vert', S[0], IsImportant);
      end;

      2:
      begin
        ProcessProperty('thv-border-spacing-horz', S[0], IsImportant);
        ProcessProperty('thv-border-spacing-vert', S[1], IsImportant);
      end;
    end;
  end;

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
      if S[I] = 'none' then
      begin
        Values[shImage] := S[I];
        S[I] := '';
      end
      else if TryStrToColor(S[I], NeedPound, Dummy) then
      begin
        Values[shColor] := S[I];
        S[I] := '';
      end;

    ProcessProperty('background-color', Values[shColor], IsImportant);
    ProcessProperty('background-image', Values[shImage], IsImportant);

    // process repeat
    for I := 0 to Count - 1 do
      if Pos('repeat', S[I]) > 0 then
      begin
        Values[shRepeat] := S[I];
        S[I] := '';
      end;
    ProcessProperty('background-repeat', Values[shRepeat], IsImportant);

    // process attachment
    for I := 0 to Count - 1 do
      if (S[I] = 'fixed') or (S[I] = 'scroll') then
      begin
        Values[shAttachment] := S[I];
        S[I] := '';
      end;
    ProcessProperty('background-attachment', Values[shAttachment], IsImportant);

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
    ProcessProperty('background-position', Values[shPosition], IsImportant);
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
        ProcessProperty(Prop + '-top' + Erty, Value, IsImportant);
        ProcessProperty(Prop + '-right' + Erty, Value, IsImportant);
        ProcessProperty(Prop + '-bottom' + Erty, Value, IsImportant);
        ProcessProperty(Prop + '-left' + Erty, Value, IsImportant);
      end
      else
        ProcessProperty(Prop + Erty, Value, IsImportant);
    end;

  begin
    Values[shWidth] := 'medium';
    Values[shStyle] := 'none';
    // TODO: BG, 04.01.2012: set default color from 'color' property
    Values[shColor] := '';

    ExtractParn(Value, S, Count);
    for I := 0 to Count - 1 do
      if TryStrToColor(S[I], NeedPound, Dummy) then
        Values[shColor] := S[I];

    SplitString(Value, S, Count);
    for I := 0 to Count - 1 do
    begin
      if TryStrToColor(S[I], NeedPound, Dummy) then
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
    Index := normal;
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
              if StrToIntDef( htStringToString(S[I]), -1) < 100 then
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

    // BG, 25.05.2013: You MUST set a font otherwize the property is malformed and ignored:
    // see: http://www.w3.org/TR/CSS21/fonts.html#propdef-font
    if Values[shFamily] <> '' then
    begin
      // set values to properties
      ProcessProperty('font-style', Values[shStyle], IsImportant);
      ProcessProperty('font-variant', Values[shVariant], IsImportant);
      ProcessProperty('font-weight', Values[shWeight], IsImportant);
      ProcessProperty('font-size', Values[shSize], IsImportant);
      ProcessProperty('line-height', Values[shHeight], IsImportant);
      ProcessProperty('font-family', Values[shFamily], IsImportant);
    end;
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

    ProcessProperty('list-style-type', Values[shType], IsImportant);
    ProcessProperty('list-style-position', Values[shPosition], IsImportant);
    ProcessProperty('list-style-image', Values[shImage], IsImportant);
  end;

  procedure DoMarginItems(X: TShortHand; const Value: ThtString);
  { Do the Margin, Border, Padding shorthand property specifiers}
  var
    S: array[0..3] of ThtString;
    I, Count: integer;
    Index: array[0..3] of ThtPropIndices;

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

    ProcessProperty(PropWords[Index[0]], S[0], IsImportant);
    case Count of
      1: for I := 1 to 3 do
          ProcessProperty(PropWords[Index[I]], S[0], IsImportant);
      2:
        begin
          ProcessProperty(PropWords[Index[2]], S[0], IsImportant);
          ProcessProperty(PropWords[Index[1]], S[1], IsImportant);
          ProcessProperty(PropWords[Index[3]], S[1], IsImportant);
        end;
      3:
        begin
          ProcessProperty(PropWords[Index[2]], S[2], IsImportant);
          ProcessProperty(PropWords[Index[1]], S[1], IsImportant);
          ProcessProperty(PropWords[Index[3]], S[1], IsImportant);
        end;
      4:
        begin
          ProcessProperty(PropWords[Index[1]], S[1], IsImportant);
          ProcessProperty(PropWords[Index[2]], S[2], IsImportant);
          ProcessProperty(PropWords[Index[3]], S[3], IsImportant);
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
    BorderSpacing:
      DoBorderSpacing(StrippedValue);
  end;
end;

{----------------SortContextualItems}

function SortContextualItems(const S: ThtString): ThtString;
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

//-- BG ---------------------------------------------------------- 24.10.2016 --
procedure GetMediaQueries(const MediaQuery: ThtString; out Queries: ThtMediaQueries);
var
  ICh, NCh: Integer;
  LCh: ThtChar;

  function PeekCh: ThtChar;
  begin
    if ICh <= NCh then
      Result := MediaQuery[ICh]
    else
      Result := EofChar;
  end;

  procedure GetCh;
  begin
    if ICh < NCh then
    begin
      Inc(ICh);
      LCh := MediaQuery[ICh];
    end
    else
      LCh := EofChar;
  end;

  procedure SkipWhiteSpace;
  begin
    if ICh < NCh then
      ICh := StyleUn.SkipWhiteSpace(MediaQuery, ICh, NCh);
    if ICh < NCh then
      LCh := MediaQuery[ICh]
    else
      LCh := EofChar;
  end;

  //-- BG ---------------------------------------------------------- 13.03.2011 --
  function GetIdentifier(out Identifier: ThtString): Boolean;
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
        case PeekCh of
          '0'..'9', '-':
            Result := False;
        else
          SetLength(Identifier, Length(Identifier) + 1);
          Identifier[Length(Identifier)] := LCh;
          GetCh;
        end;
      end;

//      '\':
//      begin
//        if PeekCh <> EofChar then
//        begin
//          GetCh;
//          SetLength(Identifier, Length(Identifier) + 1);
//          Identifier[Length(Identifier)] := LCh;
//          GetCh;
//        end;
//      end;
    end;

    // loop through all allowed charaters:
    while Result do
    begin
      case LCh of
        'A'..'Z', 'a'..'z', '0'..'9', '-', '_': ;

//        '\':
//        begin
//          if PeekCh = EofChar then
//            break;
//          GetCh;
//        end;

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

  function GetExpression(out Value: ThtString; const GoodTermChars, BadTermChars: ThtString): Boolean;
  begin
    Result := False;
    repeat
      case GetTermCharKind(LCh, GoodTermChars, BadTermChars) of
        tckGood:
          begin
            GetCh;
            Result := True;
            break;
          end;

        tckBad:
          break;
      else
        // tckNone:
        SetLength(Value, Length(Value) + 1);
        Value[Length(Value)] := LCh;
        GetCh;
      end;
    until False;
  end;

  function TryGetMediaQuery(out Query: ThtMediaQuery): Boolean;
  var
    Identifier, IdentLow, X: ThtString;
    IsIdentifier, NeedsAnd: Boolean;
    Expression: ThtMediaExpression;
    I: Integer;
  begin
    Result := False;
    SetLength(Query.Expressions, 0);

    SkipWhiteSpace;
    IsIdentifier := GetIdentifier(Identifier);

    // optional: 'not' or 'only'
    Query.Negated := False;
    if IsIdentifier then
    begin
      IdentLow := htLowerCase(Identifier);

      Query.Negated := IdentLow = 'not';
      if Query.Negated or (IdentLow = 'only') then
      begin
        SkipWhiteSpace;
        IsIdentifier := GetIdentifier(Identifier);
        if IsIdentifier then
          IdentLow := htLowerCase(Identifier);
      end;
    end;

    // optional: media type
    Query.MediaType := mtAll;
    NeedsAnd := IsIdentifier;
    if IsIdentifier then
    begin
      // This identifier identifies a media type
      if not TryStrToMediaType(Identifier, Query.MediaType) then
        // media type is unknown thus query must evaluate to false:
        Query.Negated := True; // 'media type is not mtAll' will always evaluate to false!

      SkipWhiteSpace;
      IsIdentifier := GetIdentifier(Identifier);
      if IsIdentifier then
        IdentLow := htLowerCase(Identifier);

      // At this point Query is valid: [ONLY | NOT]? S* media_type
      Result := True;
    end;

    // optional: media features expressions separated by 'and'
    repeat
      // if there is an identifier it must be 'and'
      if IsIdentifier then
      begin
        Result := IdentLow = 'and';
        if not Result then
          break;
        NeedsAnd := False;
        IsIdentifier := False;
        SkipWhiteSpace;
      end;

      case LCh of
        '(': // media feature expression
          begin
            Result := False;
            if NeedsAnd then
              // Required 'and' was missing!
              break;

            // Get feature
            GetCh;
            SkipWhiteSpace;
            IsIdentifier := GetIdentifier(Identifier);
            if not IsIdentifier then
              // no feature name
              break;
            IdentLow := htLowerCase(Identifier);

            if not TryStrToMediaFeature(IdentLow, Expression.Feature, Expression.Oper) then
            begin
              Expression.Oper := moUnknown;
              Expression.Feature := mfUnknown;
            end;

            VarClear(Expression.Expression);
            SkipWhiteSpace;
            case LCh of
              ':':
                begin
                  GetCh;
                  SkipWhiteSpace;
                  Result := GetExpression(X, ')', ',{<');
                  if not Result then
                    Break;
                  Expression.Expression := X;
                end;

              ')':
                begin
                  GetCh;
                  SkipWhiteSpace;
                  Expression.Oper := moIs;
                  Result := True;
                end;
            else
              break;
            end;

            I := Length(Query.Expressions);
            SetLength(Query.Expressions, I + 1);
            Query.Expressions[I] := Expression;
            NeedsAnd := True;

            SkipWhiteSpace;
            IsIdentifier := GetIdentifier(Identifier);
            if not IsIdentifier then
              // no feature name
              break;
            IdentLow := htLowerCase(Identifier);
          end;

// breaking on any unexpected char avoids endless loop:
//        ',',
//        '{',
//        '<',
//        EofChar:
        else
          break;
      end;
    until False;
  end;

var
  I: Integer;
begin
  NCh := Length(MediaQuery);
  ICh := 0;
  GetCh;

  I := 0;
  repeat
    Inc(I);
    SetLength(Queries, I);
    if not TryGetMediaQuery(Queries[I - 1]) then
      break;
    if LCh = ',' then
      GetCh;
  until False;
  Dec(I);

  if I = 0 then
  begin
    // if no Queries, then all media
    Queries[0].Negated := False;
    Queries[0].MediaType := mtAll;
    SetLength(Queries[0].Expressions, 0);
  end
  else
    SetLength(Queries, I);
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleTagParser.DoStyle(Styles: TStyleList; var C: ThtChar; Doc: TBuffer; const APath, AMedia: ThtString; FromLink: Boolean);

  //TODO -oBG, 02.03.2016: match with media properties
  //TODO -oBG, 02.03.2016: remember relevant styles and reapply in case media properties changed
  function MediaMatches(const Queries: ThtMediaQueries): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(Queries) to High(Queries) do
    begin
      if Assigned(FOnMatchMediaQuery) then
        FOnMatchMediaQuery(Self, Queries[I], Result)
      else
        Result := (Queries[I].MediaType in [mtAll, mtScreen]) xor Queries[I].Negated;

      if Result then
        break;
    end;
  end;

  procedure ReadAt;
  {read @import and @media}

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
      Media: ThtString;
      Queries: ThtMediaQueries;
    begin
      repeat
        case LCh of
          '{',
          ';',
          '<',
          EofChar:
            break;
        else
          htAppendChr(Media, LCh);
        end;
        GetCh;
      until False;

      GetMediaQueries(Media, Queries);
      case LCh of
        '{':
        begin
          if MediaMatches(Queries) then
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
      URL, Media: ThtString;
      Queries: ThtMediaQueries;
    begin
      Result := False;
      SkipWhiteSpace;
      case LCh of
        '"':
          Result := GetString(URL);

        'u':
          if GetIdentifier(URL) then
            if htLowerCase(URL) = 'url' then
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
        repeat
          case LCh of
            ';',
            '<',
            EofChar:
              break;
          else
            htAppendChr(Media, LCh);
          end;
          GetCh;
        until False;

        GetMediaQueries(Media, Queries);
        if MediaMatches(Queries) then
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
      AtRule := htLowerCase(AtRule);
      if AtRule = 'media' then
        DoMedia
      else if AtRule = 'import' then
        DoImport;
    end;
  end;

var
  MayCloseCommment, OK: Boolean;
  Pos: Integer;
  Queries: ThtMediaQueries;
  CommentState: (htsBefore, htsOpen, htsExcl, htsDash, htsInComment);
begin
  Ok := Length(AMedia) = 0;
  if not OK then
  begin
    GetMediaQueries(AMedia, Queries);
    OK := MediaMatches(Queries);
  end;
  if OK then
  begin {parse @media screen}
    Self.Doc := Doc;
    Self.Styles := Styles;
    try
      LinkPath := APath;

      // param C is the '>' of the opening <style> tag. Just ignore it.
      // skip HTML comment opener '<!--' immediately following the <style> tag.
      Pos := Doc.Position;
      CommentState := htsBefore;
      repeat
        GetCh;
        case LCh of
          ' ':
            continue;

          '<':
            if CommentState = htsBefore then
            begin
              CommentState := htsOpen;
              continue;
            end
            else
              break;

          '!':
            if CommentState = htsOpen then
            begin
              CommentState := htsExcl;
              continue;
            end
            else
              break;

          '-':
            case CommentState of
              htsExcl:
              begin
                CommentState := htsDash;
                continue;
              end;

              htsDash:
              begin
                CommentState := htsInComment;
                break;
              end;
            else
              break;
            end
        else
          break;
        end;
      until false;

      if CommentState in [htsOpen, htsExcl, htsDash] then
      begin
        Doc.Position := Pos;
        LCh := '>';
      end;

      // read the styles up to single HTML comment closer or start of a HTML tag
      repeat
        case LCh of
          ' ', '-', '>':
            GetCh;

          EOFChar:
            break;

          '@':
            ReadAt;

          '<':
          begin
            Pos := Doc.Position;
            GetCh;
            case LCh of
              '!', '-':
              begin
                MayCloseCommment := False;
                repeat
                  GetCh;
                  case LCh of
                    EOFChar:
                      break;

                    '-':
                      MayCloseCommment := True;

                    '>':
                    begin
                      if MayCloseCommment then
                        break;
                      MayCloseCommment := False;
                    end;
                  else
                    MayCloseCommment := False;
                  end;
                until false;
              end;

            else
              if not FromLink then
              begin
                Doc.Position := Pos;
                LCh := '<';
                break;
              end;
            end;
          end;

        else
          // read a style
          Selectors.Clear;
          GetSelectors;
          GetCollection;
        end;
      until false;
      C := LCh;
    finally
      Self.Styles := nil;
      Self.Doc := nil;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleTagParser.GetCollection;
begin
  if LCh = '{' then
  begin
    inherited GetCollection('}', '<');
    if LCh = '}' then
      GetCh;
    if LCh = LfChar then
      LCh := SpcChar;
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
      Insert( htString(IntToStr(Cnt)) + Styles.GetSeqNo, Result, I + 1);
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

        '*':
          begin
            // ignore unexpected end of comment.
            if Doc.PeekChar = '/' then
              GetCh;
            LCh := ' ';
          end;

        '[', ']':
            // not yet supported.
            // skip and ignore style.
            Ignore := True;
      else
        break;
      end;
      SetLength(S, Length(S) + 1);
      S[Length(S)] := LCh;
      GetCh;
    until false;
    if not Ignore then
    begin
      S := FormatContextualSelector(htLowercase(Trim(S)), Sort);
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
procedure THtmlStyleTagParser.ProcessProperty(const Prop, Value: ThtString; IsImportant: Boolean);
var
  I: integer;
begin
  for I := 0 to Selectors.Count - 1 do
    Styles.AddModifyProp(Selectors[I], Prop, Value, IsImportant);
end;

{ THtmlStyleAttrParser }

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleAttrParser.ParseProperties(Doc: TBuffer; Propty: TProperties);
begin
  Self.Doc := Doc;
  Self.Propty := Propty;
  try
    LinkPath := '';
    GetCollection(EofChar, EofChar);
  finally
    Self.Doc := nil;
    Self.Propty := nil;
  end;
end;

//-- BG ---------------------------------------------------------- 29.12.2010 --
procedure THtmlStyleAttrParser.ProcessProperty(const Prop, Value: ThtString; IsImportant: Boolean);
begin
  Propty.AddPropertyByName(Prop, Value, IsImportant);
end;

end.
