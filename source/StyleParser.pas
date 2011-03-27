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

unit StyleParser;

interface

uses
  Graphics, Classes, SysUtils, Variants,
  //
  UrlSubs,
  Parser,
  HtmlBuffer,
  HtmlGlobals,
  HtmlSymbols,
  HtmlStyles;

type
  EParseError = class(Exception);

  // about parsing CSS 2.1 style sheets:
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/syndata.html
  // http://www.w3.org/TR/2010/WD-CSS2-20101207/grammar.html

  TGetDocumentEvent = function(Sender: TObject; const Url: ThtString): TBuffer of object;

  THtmlStyleParser = class
  private
    FOrigin: TPropertyOrigin;
    FDoc: TBuffer;
    FLinkPath: ThtString;
    FOnGetDocument: TGetDocumentEvent;
    FSupportedMediaTypes: TMediaTypes;
    //
    LCh: ThtChar;
    FCanCharset, FCanImport: Boolean;
    FMediaTypes: TMediaTypes;

    // debug stuff
    LIdentPos, LMediaPos, LWhiteSpacePos, LSelectorPos: Integer;
    procedure checkPosition(var Pos: Integer);

    // AddPath() is used to form an absolute path name for imported style sheets by prefixing relative names with LinkPath.
    function AddPath(const S: ThtString): ThtString;
    // AddPath() is used to form an absolute path name for property value by prefixing relative names with LinkPath.
    // The result is enclose in 'url(' and ')'.
    function AddUrlPath(const S: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}

    // basic parser methods
    function IsWhiteSpace: Boolean; {$ifdef UseInline} inline; {$endif}
    procedure GetCh;
    procedure GetChSkipWhiteSpace; {$ifdef UseInline} inline; {$endif}
    procedure SkipComment;
    procedure SkipWhiteSpace;

    // token retrieving methods. They do not skip trailing white spaces.
    function GetDocument(Url: ThtString): TBuffer;
    function GetIdentifier(out Identifier: ThtString): Boolean;
    function GetString(out Str: ThtString): Boolean;
    function GetUrl(out Url: ThtString): Boolean;

    // syntax parsing methods. They skip trailing white spaces.
    function ParseDeclaration(out Name: ThtString; out Terms: ThtStringArray; out Important: Boolean): Boolean;
    function ParseExpression(out Terms: ThtStringArray): Boolean;
    function ParseProperties(Properties: TPropertyList): Boolean;
    function ParseRuleset(out Ruleset: TRuleset): Boolean;
    function ParseSelectors(Selectors: TSelectorList): Boolean;
    procedure ParseAtRule(Rulesets: TRulesetList);
    procedure ParseSheet(Rulesets: TRulesetList);
    procedure setMediaTypes(const Value: TMediaTypes);
    procedure setSupportedMediaTypes(const Value: TMediaTypes);
  public
    class procedure ParseCssDefaults(Rulesets: TRulesetList); overload;
    constructor Create(Origin: TPropertyOrigin; Doc: TBuffer; const LinkPath: ThtString = '');

    // ParseProperties() parses any tag's style attribute. If starts with quote, then must end with same quote.
    function ParseStyleProperties(var LCh: ThtChar; Properties: TPropertyList): Boolean;

    // ParseStyleSheet() parses an entire style sheet document:
    procedure ParseStyleSheet(Rulesets: TRulesetList);

    // ParseStyleTag() parses a style tag of an html document:
    procedure ParseStyleTag(var LCh: ThtChar; Rulesets: TRulesetList);

    property SupportedMediaTypes: TMediaTypes read FSupportedMediaTypes write setSupportedMediaTypes;
    property MediaTypes: TMediaTypes read FMediaTypes write setMediaTypes;
    // used to retrieve imported style sheets. If OnGetDocument not given tries to load file from local file system.
    property LinkPath: ThtString read FLinkPath write FLinkPath;
    property OnGetDocument: TGetDocumentEvent read FOnGetDocument write FOnGetDocument;
  end;

implementation

//-- BG ---------------------------------------------------------- 20.03.2011 --
function GetCssDefaults: TBuffer; overload;
var
  Stream: TStream;
begin
  Stream := HtmlStyles.GetCssDefaults;
  try
    Result := TBuffer.Create(Stream, 'css-defaults');
  finally
    Stream.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function TranslateMediaTypes(const MediaTypes: TMediaTypes): TMediaTypes;
begin
  if mtAll in MediaTypes then
    Result := AllMediaTypes
  else
    Result := MediaTypes;
end;

{----------------AddPath}
function THtmlStyleParser.AddPath(const S: ThtString): ThtString;
begin
  if (Pos('://', FLinkPath) > 0) then
    if not IsFullUrl(S) then
      Result := CombineURL(FLinkPath, S)
    else
      Result := S
  else
  begin
    Result := HTMLToDos(S);
    if (Pos(':', Result) <> 2) and (Pos('\\', Result) <> 1) then
      Result := FLinkPath + Result;
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function THtmlStyleParser.AddUrlPath(const S: ThtString): ThtString;
{for <link> styles, the path is relative to that of the stylesheet directory and must be added now}
begin
  Result := 'url(' + AddPath(ReadUrl(S)) + ')';
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
procedure THtmlStyleParser.checkPosition(var Pos: Integer);
  procedure stopHere();
  begin
  end;
begin
  if Pos = FDoc.Position then
    stopHere();
  Pos := FDoc.Position;
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
constructor THtmlStyleParser.Create(Origin: TPropertyOrigin; Doc: TBuffer;
  const LinkPath: ThtString);
begin
  inherited Create;
  FOrigin := Origin;
  FDoc := Doc;
  FLinkPath := LinkPath;
  SupportedMediaTypes := AllMediaTypes;
end;


//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure THtmlStyleParser.GetCh;
begin
  LCh := FDoc.NextChar;
  case LCh of
    #13, #12:
      LCh := LfChar;

    #9:
      LCh := SpcChar;

    '/':
      if FDoc.PeekChar = '*' then
      begin
        SkipComment;
        LCh := SpcChar;
      end;
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure THtmlStyleParser.GetChSkipWhiteSpace;
begin
  LCh := FDoc.NextChar;
  SkipWhiteSpace;
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
function THtmlStyleParser.GetDocument(Url: ThtString): TBuffer;
var
  Filename: ThtString;
  Stream: TStream;
begin
  if assigned(FOnGetDocument) then
    Result := FOnGetDocument(Self, Url)
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

//-- BG ---------------------------------------------------------- 13.03.2011 --
function THtmlStyleParser.GetIdentifier(out Identifier: ThtString): Boolean;
begin
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
      case FDoc.PeekChar of
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
    Result := Length(Identifier) > 0
  else
    checkPosition(LIdentPos);
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

//-- BG ---------------------------------------------------------- 20.03.2011 --
function THtmlStyleParser.GetUrl(out Url: ThtString): Boolean;

  procedure GetUrlRest;
  begin
    repeat
      case LCh of
        SpcChar:
        begin
          SkipWhiteSpace;
          Result := LCh = ')';
          if Result then
            GetCh;
          break;
        end;

        ')':
        begin
          Result := True;
          GetCh;
          break;
        end;

        EofChar:
          break;

      end;
      SetLength(Url, Length(Url) + 1);
      Url[Length(Url)] := LCh;
      GetCh;
    until False;
  end;

begin
  Result := False;
  case LCh of
    '"':
      Result := GetString(URL);

    'u':
    begin
      if GetIdentifier(URL) then
        if LowerCase(URL) = 'url' then
          if LCh = '(' then
          begin
            GetChSkipWhiteSpace;
            if GetString(URL) then
            begin
              SkipWhiteSpace;
              Result := LCh = ')';
              if Result then
                GetCh;
            end
            else
              GetUrlRest;
          end;
    end;
  else
    SetLength(Url, 0);
    GetUrlRest;
  end;
end;

//-- BG ---------------------------------------------------------- 19.03.2011 --
function THtmlStyleParser.IsWhiteSpace: Boolean;
begin
  case LCh of
    SpcChar, LfChar, CrChar, FfChar, TabChar:
      Result := True;
  else
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
procedure THtmlStyleParser.ParseAtRule(Rulesets: TRulesetList);

  function GetMediaTypes: TMediaTypes;
  var
    Identifier: ThtString;
    MediaType: TMediaType;
  begin
    Result := [];
    if not GetIdentifier(Identifier) then
      exit;
    repeat
      checkPosition(LMediaPos);
      if TryStrToMediaType(Identifier, MediaType) then
        Include(Result, MediaType);
      SkipWhiteSpace;
      if LCh <> ',' then
        break;
      GetChSkipWhiteSpace;
      if not GetIdentifier(Identifier) then
        break;
    until False;
  end;

  procedure DoMedia;
  var
    Media: TMediaTypes;
    Ruleset: TRuleset;
  begin
    Media := GetMediaTypes;
    case LCh of
      '{':
      begin
        GetChSkipWhiteSpace;
        MediaTypes := Media;
        try
          repeat
            case LCh of
              '}':
              begin
                GetChSkipWhiteSpace;
                break;
              end;

              EofChar, '<':
                break;
            else
              if not ParseRuleset(Ruleset) then
                break;
              Rulesets.Add(Ruleset);
            end;
          until False;
        finally
          MediaTypes := SupportedMediaTypes;
        end;
      end;
    end;
  end;

  procedure DoImport;
  var
    Result: Boolean;
    URL: ThtString;
    LinkUrl: ThtString;
    Media: TMediaTypes;
    Inclusion: TBuffer;
    Parser: THtmlStyleParser;
  begin
    Result := GetUrl(URL);
    if Result then
      if FCanImport then
      begin
        SkipWhiteSpace;
        if Length(Url) > 2 then
          case Url[1] of
            '"', '''':
              Url := Copy(Url, 2, Length(Url) - 2);
          end;
        Media := GetMediaTypes;
        if Media = [] then
          Media := AllMediaTypes;
        Media := Media * SupportedMediaTypes;
        if Media <> [] then
        begin
          LinkUrl := AddPath(Url);
          Inclusion := GetDocument(LinkUrl);
          if Inclusion <> nil then
            try
              Parser := THtmlStyleParser.Create(FOrigin, Inclusion, FLinkPath);
              Parser.SupportedMediaTypes := Media;
              try
                Parser.ParseStyleSheet(Rulesets);
              finally
                Parser.Free;
              end;
            finally
              Inclusion.Free;
            end;
        end;
      end;
    repeat
      case LCh of
        ';':
        begin
          GetChSkipWhiteSpace;
          break;
        end;
        '@', EofChar, '<':
          break;
      end;
      GetChSkipWhiteSpace;
    until False;
  end;

  procedure DoCharset;
  var
    Charset: ThtString;
    Info: TBuffCharSetCodePageInfo;
  begin
    if GetString(Charset) then
      if FCanCharset then
      begin
        Info := GetCharSetCodePageInfo(Charset);
        if Info <> nil then
        begin
          FDoc.CharSet := Info.CharSet;
          FDoc.CodePage := Info.CodePage;
        end;
        SkipWhiteSpace;
      end;
    repeat
      case LCh of
        ';':
        begin
          GetChSkipWhiteSpace;
          break;
        end;
        '@', EofChar, '<':
          break;
      end;
      GetChSkipWhiteSpace;
    until False;
  end;

var
  AtRule: ThtString;
begin
  GetCh; // skip the '@';
  if GetIdentifier(AtRule) then
  begin
    SkipWhiteSpace;
    AtRule := LowerCase(AtRule);
    if AtRule = 'media' then
      DoMedia
    else if AtRule = 'import' then
      DoImport
    else if AtRule = 'charset' then
      DoCharset;
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
class procedure THtmlStyleParser.ParseCssDefaults(Rulesets: TRulesetList);
var
  CssDefaults: TBuffer;
begin
  CssDefaults := GetCssDefaults;
  try
    with THtmlStyleParser.Create(poDefault, CssDefaults) do
      try
        ParseStyleSheet(Rulesets);
      finally
        Free;
      end;
  finally
    CssDefaults.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function THtmlStyleParser.ParseDeclaration(out Name: ThtString; out Terms: ThtStringArray; out Important: Boolean): Boolean;

  function GetImportant(out Important: Boolean): Boolean;
  var
    Id: ThtString;
  begin
    Important := False;
    Result := LCh <> '!'; // '!important' is optional, thus it's OK, not to find '!'
    if not Result then
    begin
      GetChSkipWhiteSpace;
      if GetIdentifier(Id) then
      begin
        SkipWhiteSpace;
        Important := LowerCase(Id) = 'important';
        Result := True;
      end;
    end;
  end;

begin
  Result := GetIdentifier(Name);
  if Result then
  begin
    SkipWhiteSpace;
    Result := (LCh = ':') or (LCh = '=');
    if Result then
    begin
      GetChSkipWhiteSpace;
      Result := ParseExpression(Terms);
      if Result then
        Result := GetImportant(Important);
    end;
  end;
  // correct end of declaration or error recovery: find end of declaration
  repeat
    case LCh of
      ';':
      begin
        GetChSkipWhiteSpace;
        break;
      end;

      '}', EofChar:
        break;
    else
      GetChSkipWhiteSpace;
    end;
  until false;
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
function THtmlStyleParser.ParseExpression(out Terms: ThtStringArray): Boolean;

  function GetTerm(out Term: ThtString): Boolean;

    function GetTilEnd(): Boolean;
    begin
      repeat
        SetLength(Term, Length(Term) + 1);
        Term[Length(Term)] := LCh;
        GetCh;
        case LCh of
          SpcChar, TabChar, LfChar, CrChar, FfChar:
          begin
            SkipWhiteSpace;
            break;
          end;

          ';', '}', '!', ',', EofChar:
            break;
        end;
      until False;
      Result := Length(Term) > 0;
    end;

    function GetParams(): Boolean;
    var
      Level: Integer;
      Str: ThtString;
    begin
      Level := 1;
      repeat
        SetLength(Term, Length(Term) + 1);
        Term[Length(Term)] := LCh;
        GetCh;
        case LCh of
          ';', '}', '!', ',', EofChar:
            break;

          '"', '''':
          begin
            Result := GetString(Str);
            if not Result then
              exit;
            Term := Term + Str;
          end;

          ')':
          begin
            Dec(Level);
            if Level = 0 then
            begin
              SetLength(Term, Length(Term) + 1);
              Term[Length(Term)] := LCh;
              GetCh;
              break;
            end;
          end;

          '(': Inc(Level);
        end;
      until False;
      Result := Length(Term) > 0;
    end;

  var
    Str: ThtString;
  begin
    SetLength(Term, 0);
    repeat
      case LCh of
        '+', '-':
          case FDoc.PeekChar of
            '0'..'9': Result := GetTilEnd;
          else
            Result := False;
            break;
          end;

        '0'..'9', '#': Result := GetTilEnd;

        '"', '''':
        begin
          Result := GetString(Str);
          if Result then
            if Length(Term) > 0 then
              Term := Term + ' ' + Str
            else
              Term := Str;
        end;
      else
        Result := GetIdentifier(Str);
        if Result then
        begin
          if Length(Term) > 0 then
            Term := Term + ' ' + Str
          else
            Term := Str;
          case LCh of
            '(':
            begin
              SetLength(Term, Length(Term) + 1);
              Term[Length(Term)] := LCh;
              GetChSkipWhiteSpace;
              Result := GetParams;
            end;
          end;
        end;
      end;
      SkipWhiteSpace;
      if LCh <> ',' then
        break;
      SetLength(Term, Length(Term) + 1);
      Term[Length(Term)] := LCh;
      GetChSkipWhiteSpace;
    until False;
  end;

var
  Term: ThtString;
begin
  SetLength(Terms, 0);
  repeat
    if not GetTerm(Term) then
      break;
    SetLength(Terms, Length(Terms) + 1);
    Terms[High(Terms)] := Term;
    case LCh of
      ';', '!':
        break;
    end;
  until False;
  Result := Length(Terms) > 0;
end;


//-- BG ---------------------------------------------------------- 15.03.2011 --
function THtmlStyleParser.ParseProperties(Properties: TPropertyList): Boolean;

  function GetPrecedence(Important: Boolean; Origin: TPropertyOrigin): TPropertyPrecedence; {$ifdef UseInline} inline; {$endif}
  begin
    Result := CPropertyPrecedenceOfOrigin[Important, Origin];
  end;

var
  Precedence: TPropertyPrecedence;

  procedure ProcessProperty(Prop: TStylePropertySymbol; const Value: Variant);
  begin
    if FMediaTypes * FSupportedMediaTypes <> [] then
      if VarIsStr(Value) and (Value = 'inherit') then
        Properties.Add(TProperty.Create(Prop, Precedence, Inherit))
      else
        Properties.Add(TProperty.Create(Prop, Precedence, Value));
  end;

var
  Values: ThtStringArray;

  procedure DoBackground;
  { do the Background shorthand property specifier }
  var
    S: array [0..1] of ThtString;
    I, N: Integer;
    Color: TColor;
  begin
    N := 0;
    for I := 0 to Length(Values) - 1 do
      // image
      if (Pos('url(', Values[I]) > 0) then
      begin
        if FLinkPath <> '' then {path added now only for <link...>}
          Values[I] := AddUrlPath(Values[I]);
        ProcessProperty(BackgroundImage, Values[I]);
      end
      else if Values[I] = 'none' then
      begin
        ProcessProperty(BackgroundImage, Values[I]);
        ProcessProperty(BackgroundColor, 'transparent'); {9.41}
      end
      // color
      else if Values[I] = 'transparent' then
        ProcessProperty(BackgroundColor, Values[I])
      else if TryStrToColor(Values[I], True, Color) then
        ProcessProperty(BackgroundColor, Color)
      // repeat
      else if Pos('repeat', Values[I]) > 0 then
        ProcessProperty(BackgroundRepeat, Values[I])
      // attachment
      else if (Values[I] = 'fixed') or (Values[I] = 'scroll') then
        ProcessProperty(BackgroundAttachment, Values[I])
      // position (2 values: horz and vert).
      else if N < 2 then
      begin
        S[N] := Values[I];
        Inc(N);
      end
      else
      begin
        // only process last 2 values of malformed background property
        S[0] := S[1];
        S[1] := Values[I];
      end;

    case N of
      1: ProcessProperty(BackgroundPosition, S[0]);
      2: ProcessProperty(BackgroundPosition, S[0] + ' ' + S[1]);
    end;
  end;

  procedure DoBorder(WidthProp, StyleProp, ColorProp: TStylePropertySymbol);
  { do the Border, Border-Top/Right/Bottom/Left shorthand properties.}
  var
    I: Integer;
    Color: TColor;
    Style: TBorderStyle;
  begin
    for I := 0 to Length(Values) - 1 do
    begin
      if TryStrToColor(Values[I], True, Color) then
        ProcessProperty(ColorProp, Color)
      else if TryStrToBorderStyle(Values[I], Style) then
        ProcessProperty(StyleProp, Style)
      else
        ProcessProperty(WidthProp, Values[I]);
    end;
  end;

  procedure DoFont;
  { do the Font shorthand property specifier }
  type
    FontEnum =
      (italic, oblique, normal, bolder, lighter, bold, smallcaps,
      larger, smaller, xxsmall, xsmall, small, medium, large,
      xlarge, xxlarge);

    function FindWord(const S: ThtString; var Index: FontEnum): boolean;
    const
      FontWords: array[FontEnum] of ThtString = (
        // style
        'italic', 'oblique',
        // weight
        'normal', 'bolder', 'lighter', 'bold',
        // variant
        'small-caps',
        // size
        'larger', 'smaller', 'xx-small', 'x-small', 'small', 'medium', 'large', 'x-large', 'xx-large'
      );
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

  var
    I: integer;
    Index: FontEnum;
  begin
    for I := 0 to Length(Values) - 1 do
    begin
      if Values[I, 1] = '/' then
        ProcessProperty(LineHeight, Copy(Values[I], 2, Length(Values[I]) - 1))
      else if FindWord(Values[I], Index) then
      begin
        case Index of
          italic..oblique:  ProcessProperty(FontStyle, Values[I]);
          normal..bold:     ProcessProperty(FontWeight, Values[I]);
          smallcaps:        ProcessProperty(FontVariant, Values[I]);
        else
        {larger..xxlarge:}  ProcessProperty(FontSize, Values[I]);
        end;
      end
      else
        case Values[I, 1] of
          '0'..'9':
          {the following will pass 100pt, 100px, but not 100 or larger}
            if StrToIntDef(Values[I], -1) < 100 then
              ProcessProperty(FontSize, Values[I]);
        else
          ProcessProperty(FontFamily, Values[I]);
        end;
    end;
  end;

  procedure DoListStyle;
  { do the List-Style shorthand property specifier }
  var
    I: integer;
  begin
    for I := 0 to Length(Values) - 1 do
    begin
      if Pos('url(', Values[I]) > 0 then
      begin
        if FLinkPath <> '' then {path added now only for <link...>}
          Values[I] := AddPath(Values[I]);
        ProcessProperty(ListStyleImage, Values[I]);
      end
      else
        ProcessProperty(ListStyleType, Values[I]);
    {TODO: should also do List-Style-Position }
    end;
  end;

  procedure DoMarginItems(Prop: TStylePropertySymbol);
  { Do the Margin, Border, Padding shorthand property specifiers}
  const
    Index: array[1..4,0..3] of Integer = (
      (0, 0, 0, 0),
      (0, 1, 0, 1),
      (0, 1, 2, 1),
      (0, 1, 2, 3)
    );
  var
    I, N: integer;
  begin
    N := Length(Values);
    if (N > 0) and (N <= 4) then
      for I := 0 to 3 do
        ProcessProperty(TStylePropertySymbol(Ord(Prop) + I), Values[Index[N, I]]);
  end;

var
  Term: ThtChar;
  Name: ThtString;
  Index: TPropertySymbol;
  Important: Boolean;
begin
  case LCh of
    '{':
    begin
      Term := '}';
      GetChSkipWhiteSpace;
    end;

    '''', '"':
    begin
      Term := LCh;
      GetChSkipWhiteSpace;
    end;
  else
    Term := EofChar;
  end;
  repeat
    if LCh = Term then
    begin
      GetChSkipWhiteSpace;
      Result := True;
      exit;
    end;
    if ParseDeclaration(Name, Values, Important) then
      if TryStrToPropertySymbol(Name, Index) then
      begin
        Precedence := GetPrecedence(Important, FOrigin);
        case Index of
          MarginX:      DoMarginItems(MarginTop);
          PaddingX:     DoMarginItems(PaddingTop);
          BorderWidthX: DoMarginItems(BorderTopWidth);
          BorderColorX: DoMarginItems(BorderTopColor);
          BorderStyleX: DoMarginItems(BorderTopStyle);

          FontX:        DoFont;

          BorderX:    begin
                        DoBorder(BorderTopWidth, BorderTopStyle, BorderTopColor);
                        DoBorder(BorderRightWidth, BorderRightStyle, BorderRightColor);
                        DoBorder(BorderBottomWidth, BorderBottomStyle, BorderBottomColor);
                        DoBorder(BorderLeftWidth, BorderLeftStyle, BorderLeftColor);
                      end;
          BorderTX:     DoBorder(BorderTopWidth, BorderTopStyle, BorderTopColor);
          BorderRX:     DoBorder(BorderRightWidth, BorderRightStyle, BorderRightColor);
          BorderBX:     DoBorder(BorderBottomWidth, BorderBottomStyle, BorderBottomColor);
          BorderLX:     DoBorder(BorderLeftWidth, BorderLeftStyle, BorderLeftColor);

          BackgroundX:  DoBackground;

          ListStyleX:   DoListStyle;
        else
          if Length(Values) > 0 then
            ProcessProperty(Index, Values[0]);
        end;
      end;
  until LCh = EofChar;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function THtmlStyleParser.ParseRuleset(out Ruleset: TRuleset): Boolean;
begin
  Ruleset := TRuleset.Create(MediaTypes);
  Result := ParseSelectors(Ruleset.Selectors);
  if Result then
    Result := ParseProperties(Ruleset.Properties);

  if (Ruleset.Selectors.Count = 0) or (Ruleset.Properties.Count = 0) then
  begin
    Result := False;
    FreeAndNil(Ruleset);
  end;
end;

//-- BG ---------------------------------------------------------- 15.03.2011 --
function THtmlStyleParser.ParseSelectors(Selectors: TSelectorList): Boolean;

  function GetSelector(out Selector: TSelector): Boolean;

    function GetSimpleSelector(Selector: TSelector): Boolean;

      function GetElementName(out Name: ThtString): Boolean;
      begin
        case LCh of
          '*':
          begin
            Result := True;
            Name := LCh;
            GetCh;
          end;
        else
          Result := GetIdentifier(Name);
        end;
      end;

    var
      Name: ThtString;
      Pseudo: TPseudo;
    begin
      Result := GetElementName(Name);
      if Result then
        Selector.AddTag(Name);
      repeat
        case LCh of
          '#':
          begin
            GetCh;
            Result := GetIdentifier(Name);
            if Result then
              Selector.AddId(Name)
            else
              exit;
          end;

          ':':
          begin
            GetCh;
            Result := GetIdentifier(Name) and TryStrToPseudo(Name, Pseudo);
            if Result then
              Selector.AddPseudo(Pseudo)
            else
              exit;
          end;

          '.':
          begin
            GetCh;
            Result := GetIdentifier(Name);
            if Result then
              Selector.AddClass(Name)
            else
              exit;
          end;

          '[':
          begin
            GetCh;
            //TODO -oBG, 21.03.2011: get attribute match
            repeat
              case LCh of
                ']':
                begin
                  GetCh;
                  Result := True;
                  break;
                end;

                EofChar, '{':
                begin
                  Result := False;
                  break;
                end;
              end;
            until False;
          end;

        else
          break;
        end;
      until False;
    end;

  var
    Combinator: TCombinator;
  begin
    Selector := TSelector.Create;
    repeat
      checkPosition(LSelectorPos);
      Result := GetSimpleSelector(Selector);
      if not Result then
        break;

      if IsWhiteSpace then
        Combinator := scDescendant
      else
        Combinator := scNone;
      SkipWhiteSpace;
      case LCh of
        '>':
        begin
          Combinator := scChild;
          GetChSkipWhiteSpace;
        end;

        '+':
        begin
          Combinator := scFollower;
          GetChSkipWhiteSpace;
        end;

        '{': break;
      end;
      if Combinator = scNone then
        break;
      Selector := TCombinedSelector.Create(Selector, Combinator);
    until False;
    if not Result then
      FreeAndNil(Selector);
  end;

var
  Selector: TSelector;
begin
  repeat
    if GetSelector(Selector) then
      Selectors.Add(Selector);
    // correct end of selector or error recovery: find end of selector
    repeat
      case LCh of
        ',':
        begin
          GetChSkipWhiteSpace;
          break;
        end;

        '{':
        begin
          Result := True;
          exit;
        end;

        '}', EofChar:
        begin
          GetChSkipWhiteSpace;
          Result := False;
          exit;
        end;
      end;
      GetCh;
    until False;
  until False;
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
procedure THtmlStyleParser.ParseSheet(Rulesets: TRulesetList);
var
  Ruleset: TRuleset;
begin
  repeat
    case LCh of
      '@': ParseAtRule(Rulesets);

      '<', EofChar: break;
    else
      FCanImport := False;
      if not ParseRuleset(Ruleset) then
        break;
      Rulesets.Add(Ruleset);
    end;
    FCanCharset := False;
  until False;
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
function THtmlStyleParser.ParseStyleProperties(var LCh: ThtChar; Properties: TPropertyList): Boolean;
begin
  FCanCharset := False;
  FCanImport := False;
  Self.LCh := LCh;
  SkipWhiteSpace;
  Result := ParseProperties(Properties);
  LCh := Self.LCh;
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
procedure THtmlStyleParser.ParseStyleSheet(Rulesets: TRulesetList);
begin
  FCanCharset := True;
  FCanImport := True;
  GetChSkipWhiteSpace;
  ParseSheet(Rulesets);
end;

//-- BG ---------------------------------------------------------- 17.03.2011 --
procedure THtmlStyleParser.ParseStyleTag(var LCh: ThtChar; Rulesets: TRulesetList);
begin
  Self.LCh := LCh;
  FCanCharset := False;
  FCanImport := False;
  SkipWhiteSpace;
  ParseSheet(Rulesets);
  LCh := Self.LCh;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure THtmlStyleParser.setMediaTypes(const Value: TMediaTypes);
begin
  FMediaTypes := TranslateMediaTypes(Value);
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure THtmlStyleParser.setSupportedMediaTypes(const Value: TMediaTypes);
begin
  FSupportedMediaTypes := TranslateMediaTypes(Value);
  FMediaTypes := FSupportedMediaTypes;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure THtmlStyleParser.SkipComment;
var
  LastCh: ThtChar;
begin
  LCh := FDoc.NextChar; // skip '/'
  LCh := FDoc.NextChar; // skip '*'
  repeat
    LastCh := LCh;
    LCh := FDoc.NextChar;
    if LCh = EofChar then
      raise EParseError.Create('Unterminated comment in style file: ' + FDoc.Name);
  until (LCh = '/') and (LastCh = '*');
end;

//-- BG ---------------------------------------------------------- 14.03.2011 --
procedure THtmlStyleParser.SkipWhiteSpace;
begin
  repeat
    checkPosition(LWhiteSpacePos);
    case LCh of
      SpcChar,
      TabChar,
      LfChar,
      FfChar,
      CrChar: ;

      '/':
        if FDoc.PeekChar = '*' then
          SkipComment
        else
          break;
    else
      break;
    end;
    LCh := FDoc.NextChar;
  until False;
end;

end.
