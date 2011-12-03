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

{$undef UseInline}

unit HtmlParser;

interface

uses
  Classes, Contnrs, SysUtils, TypInfo,
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc,
{$else}
  Windows,
{$endif}
  //
  HtmlBuffer,
  HtmlDocument,
  HtmlElements,
  HtmlGlobals,
  HtmlStyles,
  HtmlSymbols,
  Parser,
  StyleParser,
  StyleTypes;

const
  MaxTokenlength = 300;

type
  TIncludeEvent = procedure(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer) of object;
  TProgressEvent = procedure (Sender: TObject; Position, Maximum: Integer) of object;

//------------------------------------------------------------------------------
// parser
//------------------------------------------------------------------------------

  TCharBufferState = set of (cbsDirty);

  TCharBuffer = class
  private
    FDocPos: Integer; // byte position of FBuffer[1] in document
    FBuffer: ThtCharArray;
    FChrPos: ThtIntegerArray;
    FCapacity: Integer;
    FCount: Integer;
    //
    FState: TCharBufferState;
    FString: ThtString;
    function GetString: ThtString;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetChrPos(Index: Integer; Pos: Integer);
  public
    procedure Add(Ch: ThtChar); overload;
    procedure Add(Ch: ThtChar; DocPos: Integer); overload; {$ifdef UseInline} inline; {$endif}
    procedure Add(S: ThtString); overload;
    procedure Add(S: ThtString; DocPos: Integer); overload; {$ifdef UseInline} inline; {$endif}
    procedure Add(S: TCharBuffer); overload;
    procedure Init(DocPos: Integer); virtual;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property DocPos: Integer read FDocPos;
    property S: ThtString read GetString;
  end;

  THtmlToken = class(TCharBuffer)
  private
    FSymbol: THtmlElementSymbol;
    FEd: THtmlElementDescription;
    FAttributes: THtmlAttributeList;
    procedure SetSymbol(const Value: THtmlElementSymbol);
    property Ed: THtmlElementDescription read FEd;
  public
{$ifdef UseEnhancedRecord}
{$else}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
{$endif}
    function CreateElement(Parent: THtmlElement): THtmlElement;
    procedure Init(DocPos: Integer); override;
    procedure StrToElement;
    procedure StrToEndSym;
    property Attributes: THtmlAttributeList read FAttributes;
    property Symbol: THtmlElementSymbol read FSymbol write SetSymbol;
  end;

  THtmlParserState = set of (hpsInComment, hpsInScript, hpsInStyle);

  THtmlParser = class(TCustomParser)
  private
    // state
    FState: THtmlParserState;
    FProgress: Integer;
    LCh: ThtChar;
    FDocPos: Integer; // byte position of LCh in document
    LCToken: THtmlToken;

    // events
    FOnInclude: TIncludeEvent;
    FOnProgress: TProgressEvent;

    // output
    FDocument: THtmlDocument;

{$ifdef DebugIt}
    // debug
    LIdentPos: Integer;
    procedure CheckPosition(var Pos: Integer);
{$endif}

    function CreateRootElement(const Ed: THtmlElementDescription): THtmlElement;
    function GetCodePage: Integer;
    function GetEntityStr(CodePage: Integer): ThtString;
    function GetIdentifier(out Identifier: ThtString): Boolean;
    procedure GetChildren(Parent: THtmlElement);
    procedure GetCh;
    procedure GetChSkipWhiteSpace; {$ifdef UseInline} inline; {$endif}
    procedure SkipWhiteSpace;
    procedure GetChBasic;
    procedure First; {$ifdef UseInline} inline; {$endif}
    procedure Next;
    property CodePage: Integer read GetCodePage;
  public
    constructor Create(Doc: TBuffer; const Base: ThtString = '');
    destructor Destroy; override;
    procedure ParseHtmlDocument(Document: THtmlDocument);
    property OnGetBuffer;
    property OnInclude: TIncludeEvent read FOnInclude write FOnInclude;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TCharBuffer }

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.Add(Ch: ThtChar);
begin
  if Count >= Capacity then
    SetCapacity(Capacity + 50);
  FBuffer[FCount] := Ch;
  Inc(FCount);
  Include(FState, cbsDirty);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.Add(Ch: ThtChar; DocPos: Integer);
begin
  SetChrPos(Count, DocPos);
  Add(Ch);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.Add(S: ThtString);
var
  K: Integer;
begin
  K := Count + Length(S);
  if K >= Capacity then
    SetCapacity(K + 50);
  Move(S[1], FBuffer[Count], Length(S) * Sizeof(ThtChar));
  FCount := K;
  Include(FState, cbsDirty);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.Add(S: ThtString; DocPos: Integer);
begin
  SetChrPos(Count, DocPos);
  Add(S);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.Add(S: TCharBuffer);
var
  K: Integer;
begin
  if Count = 0 then
    FDocPos := S.DocPos;
  K := Count + S.Count;
  if K >= Capacity then
    SetCapacity(K + 50);
  Move(S.FBuffer[0], FBuffer[Count], S.Count * Sizeof(ThtChar));
  FCount := K;
  Include(FState, cbsDirty);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.Init(DocPos: Integer);
begin
  FCount := 0;
  FDocPos := DocPos;
  SetLength(FString, 0);
  Exclude(FState, cbsDirty);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function TCharBuffer.GetString: ThtString;
begin
  if cbsDirty in FState then
  begin
    htSetString(FString, @FBuffer[0], FCount);
    Exclude(FState, cbsDirty);
  end;
  Result := FString;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    SetLength(FBuffer, NewCapacity);
    FCapacity := NewCapacity;
    if NewCapacity < Count then
    begin
      FCount := NewCapacity;
      if not (cbsDirty in FState) then
        SetLength(FString, FCount);
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure TCharBuffer.SetChrPos(Index, Pos: Integer);
begin
  //BG, 27.03.2011: set FChrPos only, if actually used.
  if Index >= FCapacity then
    SetCapacity(Index + 50);
  if Length(FChrPos) <> FCapacity then
    SetLength(FChrPos, FCapacity);
  FChrPos[Index] := Pos;
end;

{ THtmlToken }

{$ifdef UseEnhancedRecord}
{$else}

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlToken.AfterConstruction;
begin
  inherited;
  FAttributes := THtmlAttributeList.Create;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlToken.BeforeDestruction;
begin
  FAttributes.Free;
  inherited;
end;

{$endif}

//-- BG ---------------------------------------------------------- 31.03.2011 --
function THtmlToken.CreateElement(Parent: THtmlElement): THtmlElement;
var
  Attribute: THtmlAttribute;
begin
  Result := THtmlElement.Create(Parent, FSymbol, FDocPos);
  Attribute := Attributes.First;
  while Attribute <> nil do
  begin
    Result.Attributes[Attribute.Symbol] := Attribute.Value;
    Attribute := Attribute.Next;
  end;
end;

//-- BG ---------------------------------------------------------- 28.03.2011 --
procedure THtmlToken.Init(DocPos: Integer);
begin
  inherited;
  FSymbol := UnknownSy;
  FEd := UnknownEd;
  FAttributes.Clear;
end;

//-- BG ---------------------------------------------------------- 29.03.2011 --
procedure THtmlToken.SetSymbol(const Value: THtmlElementSymbol);
begin
  FSymbol := Value;
  FEd := ElementSymbolToElementDescription(Value);
  if Fed.Symbol = UnknownSy then
  begin
    assert(False, 'Element symbol "' + GetEnumName(TypeInfo(THtmlElementSymbol), Ord(Value)) + '" without description!');
    FEd.Symbol := Value;
    FEd.Name := GetEnumName(TypeInfo(THtmlElementSymbol), Ord(Value));
  end
end;

//-- BG ---------------------------------------------------------- 31.03.2011 --
procedure THtmlToken.StrToElement;
begin
  if TryStrToElementSymbol(htUpperCase(S), FSymbol) then
    FEd := ElementSymbolToElementDescription(FSymbol);
end;

//-- BG ---------------------------------------------------------- 31.03.2011 --
procedure THtmlToken.StrToEndSym;
begin
  if TryStrToElementEndSym(htUpperCase(S), FSymbol) then
    FEd := ElementSymbolToElementDescription(FSymbol);
end;

{ THtmlParser }

{$ifdef DebugIt}

//-- BG ---------------------------------------------------------- 19.03.2011 --
procedure THtmlParser.CheckPosition(var Pos: Integer);
  procedure stopHere();
  begin
  end;
begin
  if Pos = Doc.Position then
    stopHere();
  Pos := Doc.Position;
end;
{$endif}

//-- BG ---------------------------------------------------------- 27.03.2011 --
constructor THtmlParser.Create(Doc: TBuffer; const Base: ThtString);
begin
  inherited Create(Doc, Base);
  LCToken := THtmlToken.Create;
end;

//-- BG ---------------------------------------------------------- 31.03.2011 --
function THtmlParser.CreateRootElement(const Ed: THtmlElementDescription): THtmlElement;
begin
  Result := THtmlElement.Create(nil, Ed.Symbol, LCToken.DocPos);
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
destructor THtmlParser.Destroy;
begin
  LCToken.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 28.03.2011 --
procedure THtmlParser.First;
begin
  FState := [];
  GetCh;
  Next;
end;

//------------------------------------------------------------------------------
procedure THtmlParser.GetCh;
{Return next ThtChar in LCh. Skip comments, but process Server Side Includes}

  procedure DoDashDash; {do the comment after a <!--. Find corresponding --> }
  var
    DashCount: Integer;
  begin
    DashCount := 0;
    repeat
      case LCh of
        '-': Inc(DashCount);

        '!':
          // accept --!> also.
          if Doc.PeekChar <> '>' then
            DashCount := 0;

        '>':
          if DashCount >= 2 then
            break;

        EofChar:
          break;
      else
        DashCount := 0;
      end;
      GetChBasic;
    until False;
    Exclude(FState, hpsInComment);
  end;

  procedure ReadToGT; {read to the next GreaterChar }
  begin
    repeat
      case LCh of
        '>':
          break;
        EofChar:
          break;
      end;
      GetChBasic;
    until False;
    Exclude(FState, hpsInComment);
  end;

  procedure DoInclude;

    function GetNameValueParameter(var Name, Value: ThtString): Boolean;

      function GetQuotedString(var S: ThtString): Boolean;
      {get a quoted ThtString but strip the quotes}
      var
        Term: ThtChar;
      begin
        Term := LCh;
        GetChBasic;
        Result := True;

        repeat
          case LCh of
            EofChar:
              break;

            CrChar:
            begin
              htAppendChr(S, SpcChar);
              GetChBasic;
            end;

            AmperChar:
              htAppendStr(S, GetEntityStr(Doc.CodePage));

          else
            if LCh = Term then
            begin
              GetChBasic;
              break;
            end;
            htAppendChr(S, LCh);
            GetChBasic;
          end;
        until False;
      end;

    begin
      Result := False;
      SkipWhiteSpace;
      if GetIdentifier(Name) then
      begin
        SkipWhiteSpace;
        Value := '';
        Result := True; {at least have an ID}
        if LCh <> '=' then
          Exit;
        GetCh;

        SkipWhiteSpace;
        case LCh of
          '"', '''': GetQuotedString(Value);
        else
          {in case quotes left off ThtString}
          repeat
            case LCh of
              SpcChar,
              TabChar,
              CrChar,
              MinusChar, {need to exclude '-' to find '-->'}
              GreaterChar,
              EofChar:
                break;
            end;
            htAppendChr(Value, LCh);
            GetCh;
          until False;
        end;
      end;
    end;

  var
    S, Name, Value: ThtString;
    Include: TBuffer;
    SL: ThtStringList;
  begin
    if assigned(FOnInclude) then
    begin
      GetChBasic;
      if GetIdentifier(S) then
      begin
        SL := ThtStringList.Create;
        while GetNameValueParameter(Name, Value) do
          SL.Add(Name + '=' + Value);
        DoDashDash;
        Include := nil;
        FOnInclude(Self, S, SL, Include);
        if Include <> nil then
          PushDoc(Include);
      end;
    end
    else
      DoDashDash;
  end;

var
  Comment: Boolean;
begin {Getch}
  repeat {in case a comment immediately follows another comment}
   {comments may be either '<! stuff >' or '<!-- stuff -->'  }
    Comment := False;
    GetChBasic;
    if (LCh = LessChar) and (FState * [hpsInScript, hpsInStyle] = []) then
    begin
      case Doc.PeekChar of
        '!':
        begin
          GetChBasic;
          Comment := True;
          Include(FState, hpsInComment);
          GetChBasic;
          if LCh = '-' then
          begin
            GetChBasic;
            if LCh = '-' then
            begin
              GetChBasic;
              if LCh = '#' then
                DoInclude
              else
                DoDashDash; {a <!-- comment -->}
            end
            else
              ReadToGT;
          end
          else
            ReadToGT;
        end;

        '%': { <%....%> regarded as comment }
        begin
          GetChBasic;
          Comment := True;
          repeat
            GetChBasic;
          until (LCh = '%') and (Doc.PeekChar = GreaterChar) or (LCh = EOFChar);
          GetChBasic;
        end;
      end;
    end;
  until not Comment;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure THtmlParser.GetChBasic; {read a character}

  function ReadChar: ThtChar;
  begin
    repeat
      if not DocStack.AtLeast(1) then
        // update document position only for outmost document
        FDocPos := Doc.Position;
      Result := Doc.NextChar;
      if (Result = EofChar) and DocStack.AtLeast(1) then
        PopDoc
      else
        break;
    until false;

    if assigned(FOnProgress) then
    begin
      Inc(FProgress);
      if (Result = EofChar) or (FProgress and $FFF = 0) {about every 4000 chars} then
        FOnProgress(Self, Doc.Position, Doc.Size);
    end;
  end;

begin
  LCh := ReadChar;
  case LCh of
    CrChar:
      if Doc.PeekChar = LfChar then
        ReadChar;

    TabChar:
      LCh := SpcChar;

    FfChar,
    LfChar:
      LCh := CrChar;
  end;
end;

//-- BG ---------------------------------------------------------- 29.03.2011 --
procedure THtmlParser.GetChildren(Parent: THtmlElement);
// Parses the document tree below parent.

  function IsAncestorContent(Symbol: THtmlElementSymbol; Element: THtmlElement): Boolean;
  begin
    while Element <> nil do
    begin
      if Symbol in ElementSymbolToElementDescription(Element.Symbol).Content then
      begin
        Result := True;
        exit;
      end;
      Element := Element.Parent;
    end;
    Result := False;
  end;

  function IsAncestorSymbol(Symbol: THtmlElementSymbol; Element: THtmlElement): Boolean;
  begin
    while Element <> nil do
    begin
      if Symbol = Element.Symbol then
      begin
        Result := True;
        exit;
      end;
      Element := Element.Parent;
    end;
    Result := False;
  end;

var
  ParentEd: THtmlElementDescription;
  Element: THtmlElement;
begin
  ParentEd := ElementSymbolToElementDescription(Parent.Symbol);
  repeat
    if LCToken.Symbol in ParentEd.Content then
    begin
      // this is expected content
      if LCToken.Ed.IsNode then
      begin
        // this is a child node's start tag
        Element := LCToken.CreateElement(Parent);
        if LCToken.Ed.EndSym <> NoEndSy then
        begin
          // this element has an end tag and thus may have children as well
          Next;
          GetChildren(Element);
        end
        else
          Next;
      end
      else
        // start tag without node
        case LCToken.Symbol of
          ScriptSy:
            Next;
        else
          // unprocessed tag
          Next;
        end;
    end
    else if LCToken.Symbol = ParentEd.EndSym then
    begin
      // this is parent's end tag.
      Next;
      break;
    end
    else if LCToken.Symbol = LCToken.Ed.EndSym then
    begin
      // this is any end tag
      if IsAncestorSymbol(LCToken.Ed.Symbol, Parent) then
        // this is an anchestor's end tag
        break;
      Next;
    end
    else if LCToken.Symbol = EofSy then
      break
    else
      // this is an unexpected start tag.
      if IsAncestorContent(LCToken.Ed.Symbol, Parent.Parent) then
        // this is an anchestor's content
        break
      else
        // this is completely unexpected, ignore it.
        Next;
  until False;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure THtmlParser.GetChSkipWhiteSpace;
begin
  GetChBasic;
  SkipWhiteSpace;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function THtmlParser.GetCodePage: Integer;
begin
  Result := Doc.CodePage;
end;


//------------------------------------------------------------------------------
function THtmlParser.GetEntityStr(CodePage: Integer): ThtString;
{read an entity and return it as a ThtString.}

  procedure AddNumericChar(I: Integer; ForceUnicode: Boolean);
  // Adds the given value as new ThtChar to the ThtString.
  var
    Buf: array[0..10] of ThtChar;
  begin
    if I = 9 then
      Result := SpcChar
    else if I < ord(SpcChar) then {control ThtChar}
      Result := '?' {is there an error symbol to use here?}
    else if (I >= 127) and (I <= 159) and not ForceUnicode then
    begin
      {127 to 159 not valid Unicode}
      if MultiByteToWideChar(CodePage, 0, @I, 1, @Buf, SizeOf(Buf)) = 0 then
        Buf[0] := ThtChar(I);
      SetString(Result, Buf, 1);
    end
    else
      Result := WideChar(I);
  end;

var
  Collect: ThtString;

  procedure NextCh;
  begin
    htAppendChr(Collect, LCh);
    GetCh;
  end;

var
  I, N: Integer;
  Entity: ThtString;
begin
  if LCh = AmperChar then
  begin
  // A mask character. This introduces special characters and must be followed
  // by a '#' ThtChar or one of the predefined (named) entities.
    Collect := '';
    NextCh;
    case LCh of
      '#': // Numeric value.
      begin
        NextCh;
        N := 0;
        I := 0;
        case LCh of
          'x', 'X':
          begin
            // Hex digits given.
            NextCh;
            repeat
              case LCh of
                '0'..'9': I := 16 * I + (Ord(LCh) - Ord('0'));
                'A'..'Z': I := 16 * I + (Ord(LCh) - Ord('A') + 10);
                'a'..'z': I := 16 * I + (Ord(LCh) - Ord('a') + 10);
              else
                break;
              end;
              Inc(N);
              NextCh;
            until False;
          end;
        else
          // Decimal digits given.
          repeat
            case LCh of
              '0'..'9': I := 10 * I + (Ord(LCh) - Ord('0'));
            else
              break;
            end;
            Inc(N);
            NextCh;
          until False;
        end;
        if N > 0 then
        begin
          AddNumericChar(I, False);
          // Skip the trailing semicolon.
          if LCh = ';' then
            GetCh;
        end
        else
          Result := Collect;
      end;
    else
      // Must be a predefined (named) entity.
      Entity := '';
      N := 0;
      // Pick up the entity name.
      repeat
        case LCh of
          'a'..'z',
          'A'..'Z',
          '0'..'9':
            htAppendChr(Entity, LCh);
        else
          break;
        end;
        Inc(N);
        NextCh;
      until N > 10;

      // Now convert entity ThtString into a character value. If there is no
      // entity with that name simply add all characters as they are.
      if TryStrToEntity(Entity, I) then
      begin
        if LCh = ';' then
        begin
          AddNumericChar(I, True);
          // Advance current pointer to first character after the semicolon.
          NextCh;
        end
        else if I <= 255 then
          AddNumericChar(I, True);
      end
      else
        Result := Collect;
    end; {case}
  end; {while}
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function THtmlParser.GetIdentifier(out Identifier: ThtString): Boolean;
begin
  // An identifier can contain only the characters a..z, A..Z, 0..9, -, and _
  // and start with a..z, A..Z or _;
  SetLength(Identifier, 0);
  case LCh of
    'A'..'Z', 'a'..'z', '_':
      Result := True;
  else
    Result := False;
  end;

  // loop through all allowed characters:
  while Result do
  begin
    case LCh of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '-': ;
    else
      break;
    end;
    htAppendChr(Identifier, LCh);
    GetCh;
  end;

  if Result then
    Result := Length(Identifier) > 0
{$ifdef DebugIt}
  else
    CheckPosition(LIdentPos)
{$endif}
  ;
end;

//------------------------------------------------------------------------------
procedure THtmlParser.Next;

  procedure GetTag;
  {Pick up a Tag or pass a single LessChar}

    function GetAttribute(out Attribute: THtmlAttribute): Boolean;

      procedure GetQuotedString(var S: ThtString; WantCrLf: Boolean; Sym: THtmlAttributeSymbol);
      {get a quoted ThtString but strip the quotes}
      var
        Term: ThtChar;
      begin
        Term := LCh;
        GetCh;
        repeat
          case LCh of
            AmperChar:
              htAppendStr(S, GetEntityStr(CodePage));

            CrChar:
            begin
              if WantCrLf then
                htAppendStr(S, CrLf);
              GetCh;
            end;

            EofChar:
              break;
          else
            if LCh = Term then
            begin
              GetChSkipWhiteSpace;
              break;
            end;
            htAppendChr(S, LCh);
            GetCh;
          end;
        until False;
      end;

      procedure GetValue(var S: ThtString);
      {read a numeric.  Also reads a ThtString if it looks like a numeric initially}
      begin
        case LCh of
          '-', '+':
          begin
            S := LCh;
            GetCh;
          end;
        else
          S := '';
        end;

        repeat
          case LCh of
            SpcChar,
            TabChar,
            CrChar:
            begin
              SkipWhiteSpace;
              break;
            end;

            GreaterChar,
            EofChar:
              break;

            PercentChar:
            begin
              htAppendChr(S, LCh);
              GetChSkipWhiteSpace;
              break;
            end;

            AmperChar:
              htAppendStr(S, GetEntityStr(CodePage));
          else
            htAppendChr(S, LCh);
            GetCh;
          end;
        until False;
      end;

    var
      Name: ThtString;
      Symbol: THtmlAttributeSymbol;
      Value: ThtString;
    begin
      SkipWhiteSpace;
      Result := GetIdentifier(Name);
      if Result then
      begin
        TryStrToAttributeSymbol(htUpperCase(Name), Symbol);
        SkipWhiteSpace;
        if LCh = '=' then
        begin
          GetChSkipWhiteSpace;
          case LCh of
            '"', '''':
              GetQuotedString(Value, Symbol in [TitleAttr, AltAttr], Symbol);

            '-', '+', '0'..'9':
              GetValue(Value);
          else
            repeat
              case LCh of
                SpcChar,
                TabChar,
                CrChar,
                GreaterChar,
                EofChar:
                  break;

                AmperChar:
                  htAppendStr(Value, GetEntityStr(CodePage));
              else
                htAppendChr(Value, LCh);
                GetCh;
              end;
            until False;
          end;
        end
        else
          Value := Name;
        if Symbol = UnknownAttr then
          Value := Value + ' <!--name=' + Name + '-->';
        Attribute := THtmlAttribute.Create(Symbol, Value);
      end;
    end;

  var
    EndTag: Boolean;
    Attribute: THtmlAttribute;
  begin
    GetCh; // skip '<'
    EndTag := LCh = '/';
    if EndTag then
      GetCh;
    case LCh of
      'a'..'z', 'A'..'Z', '?': ;
    else
      {an odd LessChar}
      LCToken.Symbol := TextSy;
      LCToken.Add(LessChar);
      if EndTag then
        LCToken.Add(ThtChar('/'));
      GetChSkipWhiteSpace;
      Exit;
    end;

    // get the element's name
    while True do
      case LCh of

        'a'..'z', 'A'..'Z', '0'..'9', '_':
        begin
          LCToken.Add(LCh);
          GetCh;
        end;

        '/':
        begin
          if LCToken.Count > 0 then {allow xhtml's <br/>, etc }
            break;
          LCToken.Add(LCh);
          GetCh;
        end;
      else
        break;
      end;

    if LCToken.Count > 0 then
    begin
      if EndTag then
        LCToken.StrToEndSym
      else
        LCToken.StrToElement;

      while GetAttribute(Attribute) do
        LCToken.Attributes.Add(Attribute);
    end;

    //TODO -2 -oBG, 30.03.2011: skipping any white space is too much.
    // A single space must represent all spaces and tabs, but ignore line feeds outside
    // preformatted text
    while (LCh <> GreaterChar) and (LCh <> EofChar) do
      GetChSkipWhiteSpace;
    if LCToken.Symbol in [StyleSy, ScriptSy] then
      GetChBasic {don't skip whitespace etc. in case <!-- comment immediately follows}
    else
      GetChSkipWhiteSpace;
  end;

  procedure CollectText;
  // Considers the current data as pure text and collects everything until
  // the input ends or one of the reserved tokens is found.
  begin
    repeat
      case LCh of
        AmperChar:
          LCToken.Add(GetEntityStr(CodePage), FDocPos);

        SpcChar,
        CrChar,
        LfChar,
        FfChar,
        TabChar:
        begin
          // add 1 whitespace character per whitespace sequence only:
          LCToken.Add(SpcChar, FDocPos);
          GetChSkipWhiteSpace;
        end;

        EOFChar..ThtChar(#8),
        LessChar:
          break;
      else
        LCToken.Add(LCh);
        GetCh;
      end;
    until False;
  end;

{Get the next token}
begin {already have fresh character loaded here}
  LCToken.Init(FDocPos);
  case LCh of
    EofChar:
      LCToken.Symbol := EofSy;

    '<':
      GetTag;

    ThtChar(#1)..ThtChar(#8):
    begin
      LCToken.Symbol := TextSy;
      LCh := '?';
    end;

  else
    LCToken.Symbol := TextSy;
    CollectText;
  end;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure THtmlParser.ParseHtmlDocument(Document: THtmlDocument);

  procedure ParseStyle;
  var
    StyleParser: THtmlStyleParser;
  begin
    StyleParser := THtmlStyleParser.Create(poAuthor, Doc, LinkPath);
    try
      StyleParser.ParseStyleTag(LCh, Document.RuleSets);
    finally
      StyleParser.Free;
    end;
  end;

  procedure ParseTree;
  begin
    GetChildren(Document.Tree);
  end;

  procedure ProcessBase;
  var
    HRef: THtmlAttribute;
  begin
    if LCToken.Attributes.Find(HRefAttr, HRef) then
      LinkPath := HRef.Value;
  end;

  procedure ProcessLink;

    procedure ProcessStylesheet(const HRef: ThtString; const MediaTypes: TMediaTypes);
    var
      Buffer: TBuffer;
      Parser: THtmlStyleParser;
    begin
      Buffer := GetBuffer(HRef);
      if Buffer <> nil then
      begin
        Parser := THtmlStyleParser.Create(poAuthor, Buffer, LinkPath);
        try
          Parser.SupportedMediaTypes := MediaTypes;
          Parser.ParseStyleSheet(Document.Rulesets);
        finally
          Parser.Free;
          Buffer.Free;
        end;
      end;
    end;

  var
    Rel, HRef, Media: THtmlAttribute;
    Url: ThtString;
    LinkType: THtmlLinkType;
    MediaTypes: TMediaTypes;
  begin
    if LCToken.Attributes.Find(HRefAttr, HRef) then
    begin
      Url := AddPath(HRef.Value);
      if LCToken.Attributes.Find(RelAttr, Rel) then
        if TryStrToLinkType(Rel.Value, LinkType) then
          case LinkType of
            ltStylesheet:
              if LCToken.Attributes.Find(MediaAttr, Media) then
                if TryStrToMediaTypes(Media.Value, MediaTypes) then
                  ProcessStylesheet(Url, MediaTypes);

          end;
    end;
  end;

  procedure ProcessMeta;
  begin
    //TODO -1 -oBG, 16.04.2011: process <meta>
  end;

  procedure ProcessTitle;
  begin
    //TODO -1 -oBG, 16.04.2011: process <title>
  end;

  procedure ParseHead;
  begin
    //TODO -5 -oBG, 29.03.2011: process the attributes of element 'head'
    Next;
    repeat
      case LCToken.Symbol of
        BaseSy:
        begin
          ProcessBase;
          Next;
        end;

        IsIndexSy:
          Next;

        LinkSy:
        begin
          ProcessLink;
          Next;
        end;

        MetaSy:
        begin
          ProcessMeta;
          Next;
        end;

        ObjectSy:
          Next;

        ScriptSy:
          Next;

        StyleSy:
        begin
          ParseStyle;
          Next;
          if LCToken.Symbol = StyleEndSy then
            Next;
        end;

        TitleSy:
        begin
          ProcessTitle;
          Next;
          if LCToken.Symbol = TitleEndSy then
            Next;
        end;

        HeadEndSy:
        begin
          Next;
          break;
        end;

        BodySy,
        FrameSetSy,
        EofSy:
          // oops, HeadEndSy is missing.
          break;
      else
        Next;
      end;
    until False;
  end;

var
  Ed: THtmlElementDescription;
begin
  FDocument := Document;
  FDocument.Name := Doc.Name;
  try
    THtmlStyleParser.ParseCssDefaults(FDocument.RuleSets);
    try
      First;
      repeat
        case LCToken.Symbol of
          HtmlSy:
          begin
            //TODO -5 -oBG, 28.03.2011: process the attributes of element 'html'
            Next;
          end;

          HeadSy:
            ParseHead;

          UnknownSy,
          HeadEndSy:
            // ignore redundant </head> and any unknown tag
            Next;

          HtmlEndSy:
          begin
            // correct end of document
            Next;
            break;
          end;

          EofSy:
            // sudden end of document
            break;

          BodySy,
          FrameSetSy:
          begin
            // the main document starts now:
            Document.Tree := LCToken.CreateElement(nil);
            Next;
            ParseTree;
            break;
          end;

        else
          if (LCToken.Symbol <> TextSy) or (Length(Trim(LCToken.S)) > 0) then
          begin
            // html document without explicit <body> or <frameset> tag.
            // We found an unempty text element or any other element.
            // Embed the document into the appropriate root element:
            Ed := ElementSymbolToElementDescription(BodySy);
            if LCToken.Symbol in Ed.Content then
            begin
              Document.Tree := CreateRootElement(Ed);
              ParseTree;
              break;
            end;
            Ed := ElementSymbolToElementDescription(FrameSetSy);
            if LCToken.Symbol in Ed.Content then
            begin
              Document.Tree := CreateRootElement(Ed);
              ParseTree;
              break;
            end;
          end;
          // We are here because LCToken meets one of the following conditions:
          // - the element is a text element without text.
          // - the element is containable by neither <body> nor <frameset>.
          //BG, 30.03.2011: skip this token
          Next;
        end;
      until False;
    except
      on E: Exception do
        Assert(False, E.Message);
    end;
  finally
    FDocument := nil;
  end;
end;

//------------------------------------------------------------------------------
procedure THtmlParser.SkipWhiteSpace;
begin
  repeat
    case LCh of
      SpcChar,
      TabChar,
      CrChar,
      LfChar,
      FfChar: ;
    else
      break;
    end;
    GetCh;
  until False;
end;

end.
