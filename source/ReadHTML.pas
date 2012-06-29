{
Version   11.3
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2012 by HtmlViewer Team

*********************************************************
*                                                       *
*           Thanks to Mike Lischke for his              *
*        assistance with the Unicode conversion         *
*                                                       *
*********************************************************

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

{
This module contains the parser which reads thru the document.
It divides it into sections storing the pertinent information in Section objects.
The document itself is then a TList of section objects.
See the HTMLSubs unit for the definition of the section objects.

Key Variables:

  Sy:
      An enumerated type which indicates what the current token is.  For
      example, a value of TextSy would indicate a hunk of text, PSy that a <P>
      tag was encountered, etc.
  LCh:
      The next character in the stream to be analyzed.  In mixed case.
  LCToken:
      A ThtString which is associated with the current token.  If Sy is TextSy,
      then LCToken contains the text.
  Attributes:
      A list of TAttribute's for tokens such as <img>, <a>, which have
      attributes.
  Section:
      The current section being built.
  SectionList:
      The list of sections which form the document.  When in a Table,
      SectionList will contain the list that makes up the current cell.

Key Routines:

  GetCh:
      Gets the next character from the stream.  Fills LCh.  Skips comments.
  Next:
      Gets the next token.  Fills Sy, LCToken, Attributes.  Calls GetCh so the
      next character after the present token is available.  Each part of the
      parser is responsible for calling Next after it does its thing.

ANGUS March 2012 - fixed THtmlParser.DoMeta to handle meta without http-equiv="Content-Type"
         <meta charset="utf-8"> (used by Goggle)

}

{$define DO_LI_INLINE}
{$ifdef DO_LI_INLINE}
{$else}
{$endif}

unit ReadHTML;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc,
{$else}
  Windows,
{$endif}
  SysUtils, Math, Variants, Classes, Graphics, Controls, Contnrs,
  HtmlGlobals,
  HtmlBuffer,
  HTMLUn2,
  HTMLSubs,
  HtmlSbs1,
  StyleUn;

type
  LoadStyleType = (lsFile, lsString, lsInclude);
  SymbSet = set of Symb;

  { THtmlParser }

  THtmlParser = class
  private
    TitleStart: Integer;
    TitleEnd: Integer;
    FBase: ThtString;
    FBaseTarget: ThtString;

    LCh: ThtChar;
    LastChar: (lcOther, lcCR, lcLF);
    HeadingLevel: Integer;
    LCToken: TokenObj;

    Doc: TBuffer;
    DocStack: TStack;
    CharCount: Integer;

    Sy: Symb;
    Attributes: TAttributeList;

    BaseFontSize: Integer;
    BodyBlock: TBodyBlock;
    Section: TSection;
    SectionList: TCellBasic;
    CurrentURLTarget: TURLTarget;
    TableLevel: Integer;
    TagIndex: Integer;

    InComment: Boolean;
    InHref: Boolean;
    InScript: Boolean; {when in a <SCRIPT>}
    LinkSearch: Boolean;
    ListLevel: Integer;

    IncludeEvent: TIncludeType;
    CallingObject: TViewerBase;
    SoundEvent: TSoundType;
    MetaEvent: TMetaType;
    LinkEvent: TLinkType;

    FUseQuirksMode : Boolean;
    FPropStack: THtmlPropStack;
    FNoBreak : Boolean;
    procedure SetNoBreak(const AValue : Boolean);
    procedure GetCh;

    function DoCharSet(Content: ThtString): Boolean;
    function DoObjectTag(out C: ThtChar; out N, IX: Integer): Boolean;
    function FindAlignment: ThtString;
    function GetEntityStr(CodePage: Integer): ThtString;
    function GetIdentifier(out Identifier: ThtString): Boolean;
//    function GetValue(var S: ThtString; var Value: Integer): Boolean;
    procedure CheckForAlign;
    procedure DoAEnd;
    procedure DoBase;
    procedure DoBody(const TermSet: SymbSet);
    procedure DoBr(const TermSet: SymbSet);
    procedure DoCommonSy;
    procedure DoDivEtc(Sym: Symb; const TermSet: SymbSet);
    procedure DoFrameSet(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: ThtString);
    procedure DoListItem(
      {$ifdef DO_LI_INLINE}var LiBlock: TBlockLi; var LiSection: TSection;{$endif}
      BlockType, Sym: Symb; LineCount: Integer; Index: ThtChar; Plain: Boolean; const TermSet: SymbSet);
    procedure DoLists(Sym: Symb; const TermSet: SymbSet);
    procedure DoMap;
    procedure DoMeta(Sender: TObject);
    procedure DoP(const TermSet: SymbSet);
    procedure DoScript(Ascript: TScriptEvent);
    procedure DoSound;
    procedure DoStyleLink;
    procedure DoTable;
    procedure DoText;
    procedure DoTextArea(TxtArea: TTextAreaFormControlObj);
    procedure DoTitle;
    procedure GetOptions(Select: TOptionsFormControlObj);
    procedure Next;
    procedure ParseInit(ASectionList: ThtDocument; AIncludeEvent: TIncludeType);
    procedure SkipWhiteSpace;
    procedure PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProp: TProperties); {$ifdef UseInline} inline; {$endif}
    procedure PopProp;
    procedure PopAProp(const Tag: ThtString); {$ifdef UseInline} inline; {$endif}
    function Peek: ThtChar;
    function GetTitle: ThtString;
    function PropStackIndex: Integer;
  public
    constructor Create(Doc: TBuffer);
    destructor Destroy; override;
    function IsFrame(FrameViewer: TFrameViewerBase): Boolean;
    procedure ParseFrame(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: ThtString; AMetaEvent: TMetaType);
    procedure ParseHtml(ASectionList: ThtDocument; AIncludeEvent: TIncludeType; ASoundEvent: TSoundType; AMetaEvent: TMetaType; ALinkEvent: TLinkType);
    procedure ParseText(ASectionList: ThtDocument);
    function ShouldUseQuirksMode: Boolean;

    property Base: ThtString read FBase;
    property BaseTarget: ThtString read FBaseTarget;
    property Title: ThtString read GetTitle;
    property UseQuirksMode : Boolean read FUseQuirksMode;
    property PropStack: THtmlPropStack read FPropStack;
    property NoBreak : Boolean read FNoBreak write SetNoBreak;
  end;

implementation

uses
  HtmlView, FramView, StylePars, UrlSubs;

const
  TableTermSet = [
    TableEndSy,
    TBodySy, TBodyEndSy,
    TFootSy, TFootEndSy,
    THeadSy, THeadEndSy,
    TDSy, TDEndSy,
    THSy, THEndSy,
    TRSy, TREndSy,
    CaptionSy, CaptionEndSy,
    ColgroupSy, ColSy];

type
  PEntity = ^TEntity;
  TEntity = record
    Name: ThtString;
    Value: Integer;
  end;

  PSymbol = ^TSymbolRec;
  TSymbolRec = record
    Name: ThtString;
    Value: Symb;
  end;

  PResWord = ^TResWord;
  TResWord = record
    Name: ThtString;
    Symbol: Symb;
    EndSym: Symb; // CommandSy == no end symbol
  end;

var
  Entities: ThtStringList;
  ReservedWords: ThtStringList;
  ReservedWordsIndex: array [Symb] of Integer;
  AttributeNames: ThtStringList;
  SymbolNames: array [Symb] of ThtString;


function SymbToStr(Sy: Symb): ThtString; {$ifdef UseInline} inline; {$endif}
begin
  Result := SymbolNames[Sy];
end;

function EndSymbToStr(Sy: Symb): ThtString; {$ifdef UseInline} inline; {$endif}
begin
  Result := SymbolNames[Sy];
end;

function EndSymbFromSymb(Sy: Symb): Symb; {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  I := ReservedWordsIndex[Sy];
  if I >= 0 then
    Result := PResWord(ReservedWords.Objects[I]).EndSym
  else
    Result := CommandSy; // no match
end;

{ THtmlParser }

constructor THtmlParser.Create(Doc: TBuffer);
begin
  inherited Create;
  LCToken := TokenObj.Create;
  DocStack := TStack.Create;
  Self.Doc := Doc;
end;

destructor THtmlParser.Destroy;
begin
  DocStack.Free;
  LCToken.Free;
  inherited;
end;

function THtmlParser.PropStackIndex: Integer;
begin
  Result := FPropStack.Count - 1;
end;

//-- BG ---------------------------------------------------------- 26.12.2010 --
function THtmlParser.Peek: ThtChar; {take a look at the next ThtChar}
begin
  Result := Doc.PeekChar;
  while (Result = EofChar) and DocStack.AtLeast(1) do
  begin
    Doc.Free;
    Doc := DocStack.Pop;
    Result := Doc.PeekChar;
  end;
end;

{-------------GetCh}

procedure THtmlParser.GetCh;
{Return next ThtChar in Lch.  Ignore comments}

  procedure GetChBasic; {read a character}

    function ReadChar: ThtChar;
    begin
      repeat
        if DocStack.Count = 0 then
          // update document position only for outmost document
          PropStack.SIndex := Doc.Position;
        Result := Doc.NextChar;
        if (Result = EofChar) and DocStack.AtLeast(1) then
        begin
          Doc.Free;
          Doc := DocStack.Pop;
        end
        else
          break;
      until false;

      if not LinkSearch and (PropStack.MasterList <> nil) then
      begin
        Inc(CharCount);
        if (Result = EofChar) or (CharCount and $FFF = 0) {about every 4000 chars} then
          if Doc.Size > 0 then
            THtmlViewerBase(CallingObject).htProgress((Doc.Position * PropStack.MasterList.ProgressStart) div Doc.Size);
      end;
    end;

  begin
    LCh := ReadChar;
    case LCh of {skip a LfChar after a CrChar or a CrChar after a LfChar}
      ThtChar(^M): if LastChar = lcLF then
          LCh := ReadChar;
      ThtChar(^J): if LastChar = lcCR then
          LCh := ReadChar;
    end;
    case LCh of
      ThtChar(^I):
        LCh := SpcChar;

      ThtChar(^J):
        begin
          LastChar := lcLF;
          LCh := CrChar;
        end;

      ThtChar(^M):
        LastChar := lcCR;
    else
      begin
        LastChar := lcOther;
        if LCh = TabChar then
          LCh := SpcChar;
      end;
    end;
    if (LCh = EofChar) and InComment then
      raise EParseError.Create('Open Comment at End of HTML File');
  end;

var
  Done, Comment: Boolean;

  procedure DoDashDash; {do the comment after a <!-- }
  begin
    repeat
      while LCh <> '-' do
        GetChBasic; {get first '-'}
      GetChBasic;
      if LCh = '-' then {second '-'}
      begin
        while LCh = '-' do
          GetChBasic; {any number of '-'}
        while (LCh = SpcChar) or (LCh = CrChar) do
          GetChBasic; {eat white space}
        if LCh = '!' then
          GetChBasic; {accept --!> also}
        Done := LCh = GreaterChar;
      end
      else
        Done := False;
    until Done;
    InComment := False;
  end;

  procedure ReadToGT; {read to the next GreaterChar }
  begin
    while LCh <> GreaterChar do
      GetChBasic;
    InComment := False;
  end;

  procedure DoInclude;

    function GetNameValueParameter(out Name, Value: ThtString): Boolean;

      function GetQuotedValue(var S: ThtString): Boolean;
      {get a quoted ThtString but strip the quotes}
      var
        Term: ThtChar;
        SaveSy: Symb;
      begin
        Result := False;
        Term := LCh;
        if (Term <> ThtChar('"')) and (Term <> ThtChar('''')) then
          Exit;
        Result := True;
        SaveSy := Sy;
        GetCh;
        while (LCh <> Term) and (LCh <> EofChar) do
        begin
          if LCh = AmperChar then
            htAppendStr(S, GetEntityStr(FPropStack.Last.CodePage))
          else
          begin
            if LCh = CrChar then
              htAppendChr(S, SpcChar)
            else
              htAppendChr(S, LCh);
            GetCh;
          end;
        end;
        if LCh = Term then
          GetCh; {pass termination ThtChar}
        Sy := SaveSy;
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
        if not GetQuotedValue(Value) then
        {in case quotes left off ThtString}
          while True do
            case LCh of
              SpcChar, TabChar, CrChar, MinusChar, GreaterChar, EofChar: {need to exclude '-' to find '-->'}
                break;
            else
              htAppendChr(Value, LCh);
              GetCh;
            end;
      end;
    end;

  var
    S, Name, Value: ThtString;
    Include: TBuffer;
    Params: ThtStringList;
    SaveLCToken: TokenObj;
  begin
    S := '';
    SaveLCToken := LCToken;
    LCToken := TokenObj.Create;
    try
      GetChBasic;
      GetIdentifier(S);
      // BG, 15.12.2011: Issue 88: DoInclude and FreeAndNil of SL
      // Now freeing SL (renamed to Params) here and not
      // relying on cooperative event doing it for us.
      Params := ThtStringList.Create;
      try
        while GetNameValueParameter(Name, Value) do
          Params.Add(Name + '=' + Value);
        DoDashDash;
        Include := nil;
        IncludeEvent(CallingObject, S, Params, Include);
      finally
        Params.Free;
      end;
      if Include <> nil then
      begin
        DocStack.Push(Doc);
        Doc := Include;
      end;
    finally
      LCToken.Free;
      LCToken := SaveLCToken;
    end;
  end;

begin {Getch}
  repeat {in case a comment immediately follows another comment}
   {comments may be either '<! stuff >' or '<!-- stuff -->'  }
    Comment := False;
    GetchBasic;
    if (LCh = LessChar) and not InScript then
    begin
      case Peek of
        '!':
        begin
          GetChBasic;
          Comment := True;
          InComment := True;
          GetChBasic;
          if LCh = '-' then
          begin
            GetChBasic;
            if LCh = '-' then
            begin
              GetChBasic;
              if Assigned(IncludeEvent) and (LCh = '#') then
                DoInclude
              else
                DoDashDash; {a <!-- comment}
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
          until (LCh = '%') and (Peek = GreaterChar) or (LCh = EOFChar);
          GetChBasic;
        end;
      end;
    end;
  until not Comment;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function THtmlParser.GetIdentifier(out Identifier: ThtString): Boolean;
begin
  // An identifier can contain only the characters a..z, A..Z, 0..9, -, and _
  // and start with a..z, A..Z or _] or underscore;
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
    Result := Length(Identifier) > 0;
end;

function IsWhiteSpace(Ch: ThtChar): Boolean;
begin
  case Ch of
    SpcChar,
    TabChar,
    CrChar,
    LfChar,
    FfChar:
      Result := True;
  else
    Result := False;
  end;
end;

function THtmlParser.ShouldUseQuirksMode: Boolean;
{
This is not in ParseHTML because quirks mode effects
the CSS property initialization which is done earlier than
the parsing is and because it can dictate how HTML is parsed.
In addition, you may want to skip this detection if:

1) The document was served as "application/xhtml+xml" or is known to be
XHTML.  In those cases, the document should ALWAYS be displayed in a "standards"
mode. In ideal situations, XHTML should be parsed in a stricter manner than regular
HTML and use XML rules.
2) You want to force THTMLViewer to display the document in "quirks mode"
3) You want to force THTMLViewer to display the document in a "standards"
non-quirks mode

Scan for the following DOCTYPE declarations:

<!DOCTYPE html>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
   "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN"
    "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd">
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN"
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
   "http://www.w3.org/TR/html4/frameset.dtd">

}
var LId : ThtString;

  procedure GetChBasic;
  begin
     LCh := Doc.NextChar;
  end;

  procedure ReadToGT; {read to the next GreaterChar }
  begin
    while (LCh <> GreaterChar) and (LCh <> EofChar) do
      GetChBasic;
    InComment := False;
  end;

  procedure ReadToLT;  {read to the next LessChar }
  begin
    if not InComment then begin
      while (LCh <> LessChar) and (LCh <> EofChar) do
        GetChBasic;
    end;
  end;

  procedure ScanDTDIdentifier(out Identifier : ThtString);
  begin
    SetLength(Identifier, 0);
    case LCh of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '/', '-','.':
        Result := True;
    else
      Result := False;
    end;
  // loop through all allowed characters:
    while Result do
    begin
      case LCh of
         'A'..'Z', 'a'..'z', '0'..'9', '_', '/', '-','.': ;
      else
        break;
      end;
      htAppendChr(Identifier, LCh);
      GetChBasic;
    end;

    if Result then
      Result := Length(Identifier) > 0;
  end;

  function ScanDTD : Boolean;
  var
    LPart : ThtString;
  begin
    Result := False;
    SkipWhiteSpace;

    ScanDTDIdentifier(LPart);
    if htUpperCase(LPart) = htUpperCase('HTML') then
    begin
      GetChBasic;
      if LCh = GreaterChar then
      begin
        //HTML5 - don't use quirks mode
        Result := True;
        exit;
      end;
      ScanDTDIdentifier(LPart);
      if htUpperCase(LPart) <> htUpperCase('PUBLIC') then
        exit;
      SkipWhiteSpace;
      if LCh = '"' then
        GetChBasic;
      SkipWhiteSpace;
      ScanDTDIdentifier(LPart);
      if htUpperCase(LPart) <> htUpperCase('-//W3C//DTD') then
        exit;
      SkipWhiteSpace;
      ScanDTDIdentifier(LPart);
      LPart := htUpperCase(LPart);
      if LPart = htUpperCase('HTML') then
      begin
        SkipWhiteSpace;
        ScanDTDIdentifier(LPart);
        Result := (LPart = '4.01');
        exit;
      end;
      if LPart = 'XHTML' then
      begin
        SkipWhiteSpace;
        ScanDTDIdentifier(LPart);
        if htUpperCase(LPart) = htUpperCase('BASIC') then
        begin
          SkipWhiteSpace;
          ScanDTDIdentifier(LPart);
          if LPart = '1.1' then
            Result := True;
        end
        else
        begin
          Result := (LPart = '1.0') or (LPart = '1.1')
        end;
      end;
    end;
  end;

var
  OldPos: Integer;
begin
  FPropStack := THTMLPropStack.Create;
  try
    OldPos := Doc.Position;
    Result := True;
    repeat
      ReadToLT;
      GetChBasic;
      case LCh of
        '!':
        begin
          GetChBasic;
          GetIdentifier(LId);
          if htUpperCase(LId) <> 'DOCTYPE' then
          begin
            InComment := True;
            ReadToGT;
          end
          else
          begin
            if ScanDTD then
            begin
              Result := False;
              break;
            end;
          end;
        end;

        EofChar:
          break;
      end;
      GetIdentifier(LId);
      SkipWhiteSpace;
      LId := htUpperCase(LId);
      if (LId = 'HTML') or (LId = 'HEAD') or (LId = 'BODY') then
        break;
    until False;
    Doc.Position := OldPos;
  finally
    FreeAndNil(FPropStack);
  end;
end;

{-------------SkipWhiteSpace}

procedure THtmlParser.SkipWhiteSpace;
begin
  while IsWhiteSpace(LCh) do
    GetCh;
end;

{----------------GetValue}

//-- BG ---------------------------------------------------------- 31.01.2011 --
function THtmlParser.GetTitle: ThtString;
begin
  if TitleEnd > TitleStart then
    Result := Doc.GetString(TitleStart, TitleEnd)
  else
    Result := '';
end;

{-----------Next}

procedure THtmlParser.Next;
{Get the next token}

  procedure GetTag;
  {Pick up a Tag or pass a single LessChar}

    function GetAttribute(out Sym: Symb; out St: ThtString; out S: ThtString; out Val: Integer): Boolean;

      function GetID(out S: ThtString): Boolean;
      begin
        S := '';
        while True do
          case LCh of
            'a'..'z', 'A'..'Z', '-', '$', '0'..'9':
              begin
                htAppendChr(S, LCh);
                GetCh;
              end;
          else
            break;
          end;
        Result := Length(S) > 0;
        if Result then
          S := htUpperCase(S);
      end;

      function GetQuotedStr(var S: ThtString; WantCrLf: Boolean; Sym: Symb): Boolean;
      {get a quoted ThtString but strip the quotes, check to see if it is numerical}
      var
        Term: ThtChar;
        SaveSy: Symb;
      begin
        Result := (LCh = '"') or (LCh = '''');
        if not Result then
          Exit;
        Term := LCh;
        SaveSy := Sy;
        GetCh;
        while (LCh <> Term) and (LCh <> EofChar) do
        begin
          case LCh of
            CrChar:
              begin
                if WantCrLf then
                  htAppendStr(S, ^M^J);
                GetCh;
              end;

            AmperChar:
              htAppendStr(S, GetEntityStr(PropStack.Last.CodePage));

          else
            htAppendChr(S, LCh);
            GetCh;
          end;
        end;
        if LCh = Term then
          GetCh; {pass termination char}

        Sy := SaveSy;
      end;

      procedure StrToInteger(const S: ThtString; var Value: Integer);
      var
        S1: ThtString;
        I, Code: Integer;
        ValD: Double;
      begin
        S1 := Trim(S);
        I := Length(S1);
        if I > 0 then
        begin
          case S1[I] of
            PercentChar:
            begin
              SetLength(S1, Length(S1) - 1);
              Dec(I);
            end;

            StarChar:
            begin
              SetLength(S1, Length(S1) - 1);
              Dec(I);
              if I = 0 then
                Value := 1;
            end;
          end;

          if I > 0 then
            case S1[1] of
              '0'..'9', '+', '-', '.':
                try
                  System.Val(S1, ValD, Code);
                  Value := Round(ValD);
                except
                end;
            end;
        end;
      end;

    var
      I: Integer;
    begin
      Sym := OtherAttribute;
      Result := False;
      SkipWhiteSpace;
      St := '';
      if not GetID(St) then
        Exit; {no ID}

      if AttributeNames.Find(St, I) then
        Sym := PSymbol(AttributeNames.Objects[I]).Value;
      SkipWhiteSpace;

      S := '';
      if Sym = BorderSy then
        Val := 1
      else
        Val := 0;
      Result := True; {at least have an ID}
      if LCh <> '=' then
        Exit;
      GetCh;

      SkipWhiteSpace;
      if not GetQuotedStr(S, Sym in [TitleSy, AltSy], Sym) then {either it's a quoted ThtString or a number}
        while True do
          case LCh of
            SpcChar, TabChar, CrChar:
            begin
              SkipWhiteSpace;
              break;
            end;

            GreaterChar, EofChar:
              break;

            AmperChar:
              htAppendStr(S, GetEntityStr(FPropStack.Last.CodePage));
          else
            htAppendChr(S, LCh);
            GetCh;
          end;

      StrToInteger(S, Val);

      if (Sym = IDSy) and (S <> '') and Assigned(PropStack.MasterList) and not LinkSearch then
        PropStack.MasterList.AddChPosObjectToIDNameList(S, PropStack.SIndex);
    end;

  var
    EndTag: Boolean;
    Compare: ThtString;
    SymStr: ThtString;
    AttrStr: ThtString;
    I: Integer;
    L: Integer;
    Save: Integer;
    Sym: Symb;
  begin
    Save := PropStack.SIndex;
    TagIndex := PropStack.SIndex;
    GetCh;
    case LCh of
      '/':
      begin
        EndTag := True;
        GetCh;
      end;

      'a'..'z', 'A'..'Z', '?':
      begin
        EndTag := False;
      end;
    else
      {an odd LessChar}
      Sy := TextSy;
      LCToken.AddUnicodeChar('<', Save);
      Exit;
    end;
    Sy := CommandSy;
    Compare := '';
    while True do
      case LCh of
        '/':
        begin
          if Length(Compare) > 0 then {allow xhtml's <br/>, etc }
            break;
          // faster than: Compare := Compare + LCh;
          SetLength(Compare, Length(Compare) + 1);
          Compare[Length(Compare)] := LCh;
          GetCh;
        end;

        'a'..'z', 'A'..'Z', '_':
        begin
          // faster than: Compare := Compare + LCh;
          SetLength(Compare, Length(Compare) + 1);
          Compare[Length(Compare)] := LCh;
          GetCh;
        end;

        '0'..'9':
        begin
          if (Compare = 'H') or (Compare = 'h') then
            break;
          // faster than: Compare := Compare + LCh;
          SetLength(Compare, Length(Compare) + 1);
          Compare[Length(Compare)] := LCh;
          GetCh;
        end;
      else
        break;
      end;

    if Length(Compare) > 0 then
    begin
      if ReservedWords.Find(htUpperCase(Compare), I) then
        if not EndTag then
          Sy := PResWord(ReservedWords.Objects[I]).Symbol
        else
        begin
          Sy := PResWord(ReservedWords.Objects[I]).EndSym;
          if Sy = HtmlSy then
            Sy := CommandSy;
        end;
    end;

    SkipWhiteSpace;
    HeadingLevel := 0;
    case Sy of
      HeadingSy,
      HeadingEndSy:
        case LCh of
          '1'..'6':
            begin
              HeadingLevel := ord(LCh) - ord('0');
              GetCh;
            end;
        end;
    end;

    Attributes.Clear;
    while GetAttribute(Sym, SymStr, AttrStr, L) do
      Attributes.Add(TAttribute.Create(Sym, L, SymStr, AttrStr, PropStack.Last.Codepage));

    while (LCh <> GreaterChar) and (LCh <> EofChar) do
      GetCh;
    if not (Sy in [StyleSy, ScriptSy]) then {in case <!-- comment immediately follows}
      GetCh;
  end;

  procedure CollectText;
  // Considers the current data as pure text and collects everything until
  // the input end or one of the reserved tokens is found.
  var
    Buffer: TCharCollection;
    CodePage, SaveIndex: Integer;
    Entity: ThtString;
  begin
    CodePage := PropStack.Last.CodePage;
    Buffer := TCharCollection.Create;
    try
      while True do
      begin
        case LCh of
          #1..#8, EOFChar, LessChar:
            break;

          AmperChar:
            begin
              SaveIndex := PropStack.SIndex;
              Entity := GetEntityStr(CodePage);
              if not LinkSearch then
//                if Length(Entity) = 1 then
//                  Buffer.Add(Entity[1], SaveIndex)
//                else
                  Buffer.Add(Entity, SaveIndex);
            end;

          SpcChar, CrChar, LfChar, TabChar:
            begin
              if not LinkSearch then
                Buffer.Add(ThtChar(SpcChar), PropStack.SIndex);
              GetCh;
              // Skip other white spaces.
              SkipWhiteSpace;
            end;
        else
          if not LinkSearch then
            Buffer.Add(LCh, PropStack.SIndex);
          GetCh;
        end;
      end;
      if Buffer.Size > 0 then
        LCToken.AddString(Buffer);
    finally
      Buffer.Free;
    end;
  end;

begin {already have fresh character loaded here}
  LCToken.Clear;
  case LCh of
    '<':
      GetTag;

    #1..#8:
      begin
        Sy := TextSy;
        LCh := '?';
      end;

    EofChar:
      Sy := EofSy;
  else
    Sy := TextSy;
    CollectText;
  end;
end;

{ Add a TProperties to the PropStack. }
procedure THtmlParser.PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProp: TProperties);
begin
  PropStack.PushNewProp(Tag, AClass, AnID, APseudo, ATitle, AProp);
end;

procedure THtmlParser.PopProp;
{pop and free a TProperties from the Prop stack}
begin
  FPropStack.PopProp;
end;

procedure THtmlParser.PopAProp(const Tag: ThtString);
begin
  FPropStack.PopAProp(Tag);
end;

procedure THtmlParser.SetNoBreak(const AValue : Boolean);
begin
  FNoBreak := AValue;
  if Assigned(FPropStack) and Assigned(FPropStack.MasterList ) then begin
    FPropStack.MasterList.NoBreak := AValue;
  end;
end;

procedure THtmlParser.DoTextArea(TxtArea: TTextAreaFormControlObj);
{read and save the text for a TextArea form control}
var
  S: ThtString;
  Token: ThtString;

  procedure Next1;
    {Special Next routine to get the next token}

    procedure GetTag1; {simplified Pick up a Tag routine}
    begin
      Token := LessChar;
      GetCh;
      Sy := CommandSy;
      while True do
        case LCh of
          SpcChar, CrChar, TabChar, GreaterChar:
            break;
        else
          htAppendChr(Token, LCh);
          GetCh;
        end;
      if CompareText(Token, '</textarea') = 0 then
        Sy := TextAreaEndSy
      else
        Sy := CommandSy; {anything else}
    end;

    function IsText1: Boolean;
    begin
      while Length(Token) < 100 do
        case LCh of
          CrChar, LessChar, AmperChar, EofChar:
            break;
        else
          htAppendChr(Token, LCh);
          GetCh;
        end;

      Result := Length(Token) > 0;
    end;

  begin {already have fresh character loaded here}
    Token := '';
    LCToken.Clear;
    case LCh of
      EofChar:
        Sy := EofSy;

      CrChar:
      begin
        Sy := EolSy;
        GetCh;
      end;

      LessChar:
        GetTag1;

      AmperChar:
      begin
        htAppendStr(Token, GetEntityStr(PropStack.Last.CodePage));
        Sy := CommandSy;
      end;

    else
      if IsText1 then
        Sy := TextSy
      else
      begin
        Sy := OtherChar;
        Token := LCh;
        GetCh;
      end;
    end;
  end;

begin
  Next1;
  SetLength(S, 0);
  while (Sy <> TextAreaEndSy) and (Sy <> EofSy) do
  begin
    case Sy of
      EolSy:
        begin
          TxtArea.AddStr(S + ^M^J);
          SetLength(S, 0);
        end;
    else
      htAppendStr(S, Token);
    end;
    Next1;
  end;
  while True do
    case LCh of
      GreaterChar, EofChar:
        break;
    else
      GetCh; {remove chars to and past GreaterChar}
    end;
  GetCh;
  if Length(S) > 0 then
    TxtArea.AddStr(S);
  TxtArea.ResetToValue;
end;

function THtmlParser.FindAlignment: ThtString; {pick up Align= attribute}
var
  T: TAttribute;
  S: ThtString;
begin
  Result := '';
  if Attributes.Find(AlignSy, T) then
  begin
    S := LowerCase(T.Name);
    if (S = 'left') or (S = 'center') or (S = 'right') or (S = 'justify') then
      Result := S
    else if S = 'middle' then
      Result := 'center';
  end;
end;

procedure THtmlParser.CheckForAlign;
var
  S: ThtString;
begin
  S := FindAlignment;
  if S <> '' then
    PropStack.Last.Assign(S, TextAlign);
end;

procedure THtmlParser.DoAEnd; {do the </a>}
begin
  if InHref then {see if we're in an href}
  begin
    CurrentUrlTarget.SetLast(ThtmlViewer(CallingObject).LinkList, PropStack.SIndex);
    CurrentUrlTarget.Clear;
    InHref := False;
  end;
  PopAProp('a');
  if Assigned(Section) then
    Section.HRef(AEndSy, PropStack.MasterList, CurrentUrlTarget, nil, PropStack.Last);
end;

procedure THtmlParser.DoDivEtc(Sym: Symb; const TermSet: SymbSet);
var
  FormBlock, DivBlock: TBlock;
  FieldsetBlock: TFieldsetBlock;
  IsFieldsetLegend: Boolean;
  IsInline: Boolean;
begin
  IsInline := PropStack.Last.Display = pdInline;
  case Sym of
    DivSy, HeaderSy, NavSy, SectionSy, ArticleSy, AsideSy, FooterSy, HGroupSy :
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp(SymbToStr(Sym), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        CheckForAlign;
        if not IsInline then
        begin
          DivBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
          SectionList.Add(DivBlock, TagIndex);
          SectionList := DivBlock.MyCell;
        end
        else
          DivBlock := nil;
        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, SectionList, not IsInline);
        Next;
        DoBody([EndSymbFromSymb(Sym)] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp(SymbToStr(Sym));
        if not IsInline then
        begin
          if SectionList.CheckLastBottomMargin then
          begin
            DivBlock.MargArray[MarginBottom] := ParagraphSpace;
            DivBlock.BottomAuto := True;
          end;
          SectionList := DivBlock.OwnerCell;
        end;
        Section := nil; // TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, SectionList, False);
        if Sy = EndSymbFromSymb(Sym) then
          Next;
      end;

    FieldsetSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp('fieldset', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        CheckForAlign;

        FieldsetBlock := TFieldsetBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
        SectionList.Add(FieldsetBlock, TagIndex);
        SectionList := FieldsetBlock.MyCell;

        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, SectionList, True);
        Next;
        DoBody([FieldsetEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp('fieldset');
        if SectionList.CheckLastBottomMargin then
        begin
          FieldsetBlock.MargArray[MarginBottom] := ParagraphSpace;
          FieldsetBlock.BottomAuto := True;
        end;
        SectionList := FieldsetBlock.OwnerCell;

        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, SectionList, True);
        if Sy = FieldsetEndSy then
          Next;
      end;

    LegendSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp('legend', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        CheckForAlign;

        FieldsetBlock := nil; // valium for the compiler
        IsFieldsetLegend := SectionList.Owner is TFieldsetBlock;
        if IsFieldsetLegend then
        begin
          FieldsetBlock := TFieldsetBlock(SectionList.Owner);
          SectionList := FieldsetBlock.Legend;
        end;
        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
        Next;
        DoBody([LegendEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp('legend');
        if IsFieldsetLegend then
        begin
          SectionList := FieldsetBlock.MyCell;
        end;
        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
        if Sy = LegendEndSy then
          Next;
      end;

    CenterSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp('center', '', '', '', '', nil);
        Section := nil;
        Next;
        DoBody([CenterEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp('center');
        Section := nil;
        if Sy = CenterEndSy then
          Next;
      end;
    FormSy:
      repeat
        SectionList.Add(Section, TagIndex);
        Section := nil;
        PushNewProp('form', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        FormBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
        SectionList.Add(FormBlock, TagIndex);
        SectionList := FormBlock.MyCell;

        CurrentForm := ThtmlForm.Create(PropStack.MasterList, Attributes);

        Next;
        DoBody(TermSet + [FormEndSy, FormSy]);

        SectionList.Add(Section, TagIndex);
        Section := nil;
        PopAProp('form');
        if SectionList.CheckLastBottomMargin then
        begin
          FormBlock.MargArray[MarginBottom] := ParagraphSpace;
          FormBlock.BottomAuto := True;
        end;
        SectionList := FormBlock.OwnerCell;
        if Sy = FormEndSy then
        begin
          CurrentForm := nil;
          Next;
        end;
      until Sy <> FormSy; {in case <form> terminated by andother <form>}
    BlockQuoteSy, AddressSy:
      begin
        SectionList.Add(Section, TagIndex);
        Section := nil;
        DoLists(Sy, TermSet + [BlockQuoteEndSy, AddressEndSy]);
        if Sy in [BlockQuoteEndSy, AddressEndSy] then
          Next;
      end;
  else
    Next;
  end;
end;

type
  TCellManager = class(ThtStringList)
    Table: ThtmlTable;
    constructor Create(ATable: ThtmlTable);
    function FindColNum(Row: Integer): Integer;
    procedure AddCell(Row: Integer; CellObj: TCellObj);
  end;
{TCellManager is used to keep track of the column where the next table cell is
 going when handling the <col> tag.  Because of colspan and rowspan attributes,
 this can be a messy process.  A StringList is used with a ThtString for each
 row.  Initially, the ThtString is filled with 'o's.  As each cell is added, 'o's
 are changed to 'x's in accordance with the sixe of the cell.
}
{----------------TCellManager.Create}

constructor TCellManager.Create(ATable: ThtmlTable);
begin
  inherited Create;
  Table := ATable;
end;

function TCellManager.FindColNum(Row: Integer): Integer;
{given the row of insertion, returns the column number where the next cell will
 go or -1 if out of range.  Columns beyond any <col> definitions are ignored}
begin
  if Row = Count then
    Add(StringOfChar('o', Table.ColSpecs.Count));
  Result := Pos('o', Strings[Row]) - 1;
end;

procedure TCellManager.AddCell(Row: Integer; CellObj: TCellObj);
{Adds this cell to the specified row}
var
  I, J, K, Span: Integer;
  S1: ThtString;
begin
{make sure there's enough rows to handle any RowSpan for this cell}
  while Count < Row + CellObj.RowSpan do
    Add(StringOfChar('o', Table.ColSpecs.Count));
  I := Pos('o', Strings[Row]); {where we want to enter this cell}
  K := I;
  if I > 0 then {else it's beyond the ColInfo and we're not interested}
    for J := Row to Row + CellObj.RowSpan - 1 do {do for all rows effected}
    begin
      I := K;
      Span := CellObj.ColSpan; {need this many columns for this cell}
      S1 := Strings[J];
      repeat
        if S1[I] = 'o' then
        begin
          S1[I] := 'x';
          Inc(I);
          Dec(Span);
        end
        else
          Break;
      until Span = 0;
      Strings[J] := S1;
      if Span > 0 then {there's a conflict, adjust ColSpan to a practical value}
        CellObj.ColSpan := CellObj.ColSpan - Span;
    end;
end;

function THtmlParser.DoCharSet(Content: ThtString): Boolean;
var
  Info: TBuffCharSetCodePageInfo;
begin
  Info := GetCharSetCodePageInfo(Content);
  Result := Info <> nil;
  if Result then
  begin
    case Info.CodePage of
      CP_UTF8,
      CP_UTF16LE,
      CP_UTF16BE:
        PropStack.Last.CodePage := Info.CodePage;
    else
      PropStack.Last.CharSet := Info.CharSet;
    end;
    Doc.CharSet := PropStack.Last.CharSet;
    Doc.CodePage := PropStack.Last.CodePage;
  end;
end;

{----------------DoTable}

procedure THtmlParser.DoTable;

  procedure DoColGroup(Table: ThtmlTable; ColOK: Boolean);
  {reads the <colgroup> and <col> tags.  Put the info in ThtmlTable's ConInfo list}

    procedure ReadColAttributes(var Spec: TSpecWidth; var Valign: AlignmentType; var Align: ThtString; var Span: Integer);

      function AlignmentFromString(S: ThtString): AlignmentType;
      begin
        S := LowerCase(S);
        if S = 'top' then
          Result := ATop
        else if (S = 'middle') or (S = 'absmiddle') or (S = 'center') then
          Result := AMiddle
        else if S = 'left' then
          Result := ALeft
        else if S = 'right' then
          Result := ARight
        else if (S = 'bottom') then
          Result := ABottom
        else if (S = 'baseline') then
          Result := ABaseline
        else if (S = 'justify') then
          Result := AJustify
        else
          Result := ANone;
      end;

    var
      I: Integer;
      Algn: AlignmentType;
    begin
      for I := 0 to Attributes.Count - 1 do
        with Attributes[I] do
          case Which of
            WidthSy:
              if Pos('%', Name) > 0 then
                Spec := SpecWidth(Max(0, Min(100, Value)) * 10, wtPercent)
              else if Pos('*', Name) > 0 then
                Spec := SpecWidth(Value, wtRelative)
              else
                Spec := SpecWidth(Value, wtAbsolute);

            AlignSy:
              begin
                Algn := AlignmentFromString(Name);
                if Algn in [ALeft, AMiddle, ARight, AJustify] then
                  Align := Lowercase(Name);
              end;

            VAlignSy:
              begin
                Algn := AlignmentFromString(Name);
                if Algn in [ATop, AMiddle, ABottom, ABaseLine] then
                  VAlign := Algn;
              end;

            SpanSy:
              Span := Max(1, Value);
          end;
    end;

  var
    xSpan, cSpan: Integer;
    xWidth, cWidth: TSpecWidth;
    xVAlign, cVAlign: AlignmentType;
    xAlign, cAlign: ThtString;
  begin
    xWidth := SpecWidth(0, wtNone);
    xVAlign := ANone;
    xAlign := '';
    xSpan := 1;
    if Sy = ColGroupSy then
    begin
      if ColOk then
        ReadColAttributes(xWidth, xVAlign, xAlign, xSpan);
      SkipWhiteSpace;
      Next;
    end;
    if Sy = ColSy then
    begin
      while Sy = ColSy do
      begin
        if ColOK then
        begin
        {any new attributes in <col> will have priority over the <colgroup> items just read}
          cWidth := xWidth; {the default values}
          cVAlign := xVAlign;
          cAlign := xAlign;
          cSpan := 1; // ignore xSpan, if there is at least 1 <col> tag.
          ReadColAttributes(cWidth, cVAlign, cAlign, cSpan);
          Table.DoColumns(cSpan, cWidth, cVAlign, cAlign);
        end;
        SkipWhiteSpace;
        Next;
      end
    end
    else
    begin
      if ColOK then
        Table.DoColumns(xSpan, xWidth, xVAlign, xAlign);
    end;
    if Sy = ColGroupEndSy then
      Next;
  end;

var
  Table: ThtmlTable;
  SaveSectionList, JunkSaveSectionList: TCellBasic;
  SaveStyle: TFontStyles;
  SaveNoBreak: Boolean;
  SaveListLevel: Integer;
  RowVAlign, VAlign: AlignmentType;
  Row: TCellList;
  CellObj: TCellObj;
  T: TAttribute;
  RowStack: Integer;
  NewBlock: TTableBlock;
  SetJustify: JustifyType;
  CM: TCellManager;
  CellNum: Integer;
  TdTh: ThtString;
  ColOK: Boolean;
  CaptionBlock: TBlock;
  CombineBlock: TTableAndCaptionBlock;
  TopCaption: Boolean;
  RowType: TRowType;
  HFStack: Integer;
  FootList: TList;
  I: Integer;
  TrDisplay: TPropDisplay; // Yunqa.de.
  S: PropIndices;
  V: Variant;

  function GetVAlign(Default: AlignmentType): AlignmentType;
  var
    S: ThtString;
    T: TAttribute;
  begin
    Result := Default;
    if Attributes.Find(VAlignSy, T) then
    begin
      S := LowerCase(T.Name);
      if (S = 'top') or (S = 'baseline') then
        Result := ATop
      else if S = 'middle' then
        Result := AMiddle
      else if (S = 'bottom') then
        Result := ABottom;
    end;
  end;

  procedure AddSection;
  begin
    if Assigned(SectionList) then
    begin
      SectionList.Add(Section, TagIndex);
      Section := nil;
      if CellObj.Cell = SectionList then
      begin
        SectionList.CheckLastBottomMargin;
        Row.Add(CellObj);
        if Assigned(CM) then
          CM.AddCell(Table.Rows.Count, CellObj);
      end
{$IFDEF DebugIt}
      else
        //ShowMessage('Table cell error, ReadHTML.pas, DoTable')
{$ENDIF}
        ;
      SectionList := nil;
    end;
  end;

  procedure AddRow;
  begin
    if InHref then
      DoAEnd;
    if Assigned(Row) then
    begin
      AddSection;
      if TrDisplay <> pdNone then
      begin
        Row.RowType := RowType;
        if RowType = TFoot then
          FootList.Add(Row)
        else
          Table.Rows.Add(Row);
      end
      else
        Row.Free;
      Row := nil;
      while PropStackIndex > RowStack do
        PopProp;
    end;
  end;

  function HasBorderProps(const P: TProperties): Boolean;
  var
    I: PropIndices;
  begin
    Result := False;
    if P <> nil then
      for I := BorderTopWidth to BorderLeftStyle do
        if not ((VarType(P.Props[I]) in varInt) and (P.Props[I] = IntNull)) then
        begin
          Result := True;
          break;
        end;
  end;

begin
  Inc(TableLevel);
  if TableLevel > 10 then
  begin
    Next;
    Exit;
  end;
  if InHref then
    DoAEnd; {terminate <a>}
  SectionList.Add(Section, TagIndex);
  Section := nil;
  SaveSectionList := SectionList;
  SaveStyle := CurrentStyle;
  SaveNoBreak := NoBreak;
  SaveListLevel := ListLevel;
  SectionList := nil;
  CaptionBlock := nil;
  TopCaption := True;
  if PropStack.Last.Props[TextAlign] = 'center' then
    SetJustify := Centered
  else if PropStack.Last.Props[TextAlign] = 'right' then
    SetJustify := Right
  else
    SetJustify := NoJustify;
  PushNewProp('table', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
  Table := ThtmlTable.Create(PropStack.MasterList, Attributes, PropStack.Last);
  NewBlock := TTableBlock.Create(PropStack.MasterList, PropStack.Last, SaveSectionList, Table, Attributes, TableLevel);
  if (NewBlock.Justify <> Centered) and not (NewBlock.FloatLR in [ALeft, ARight]) then
    NewBlock.Justify := SetJustify;
  NewBlock.MyCell.Add(Table, TagIndex); {the only item in the cell}
  CombineBlock := TTableAndCaptionBlock.Create(PropStack.MasterList, PropStack.Last, SaveSectionList, Attributes, NewBlock); {will be needed if Caption found}
  CM := nil;
  ColOK := True; {OK to add <col> info}
  FootList := TList.Create;
  try
    Row := nil;
    RowVAlign := AMiddle;
    RowStack := PropStackIndex; {to prevent warning message}
    HFStack := 9999999;
    RowType := TBody;
    Next;
    while (Sy <> TableEndSy) and (Sy <> EofSy) and (Sy <> CaptionEndSy) do
      case Sy of
        TDSy, THSy:
          begin
            ColOK := False; {no more <colgroup> and <col> tags processed}
            if InHref then
              DoAEnd;
            CurrentStyle := SaveStyle;
            ListLevel := 0;
            if not Assigned(Row) then {in case <tr> is missing}
            begin
              RowVAlign := AMiddle;
              RowStack := PropStackIndex;
              PushNewProp('tr', '', '', '', '', nil);
              Row := TCellList.Create(nil, PropStack.Last);
            end
            else
            begin
              AddSection;
              while PropStackIndex > RowStack + 1 do
                PopProp; {back stack off to Row item}
            end;
            if Sy = THSy then
              TdTh := 'th'
            else
              TdTh := 'td';
            PushNewProp(TdTh, Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
            VAlign := GetVAlign(RowVAlign);
            if Assigned(CM) then
            begin
              CellNum := CM.FindColNum(Table.Rows.Count);
              if CellNum >= 0 then
                with Table.ColSpecs[CellNum] do
                begin
                  if colAlign <> '' then {<col> alignments added here}
                    PropStack.Last.Assign(colAlign, TextAlign);
                  if colVAlign <> ANone then
                    VAlign := colVAlign;
                end;
            end;
            CheckForAlign; {see if there is Align override}
            if PropStack.Last.Props[TextAlign] = 'none' then
              if Sy = ThSy then
                PropStack.Last.Assign('center', TextAlign) {th}
              else
                PropStack.Last.Assign('left', TextAlign); {td}

            // BG, 02.02.2012: border
            for S := BorderTopStyle to BorderLeftStyle do
            begin
              V := PropStack.Last.Props[S];
              if (VarType(V) in varInt) and (V = IntNull) then
                if VarType(NewBlock.MargArrayO[S]) in varInt then
                  case BorderStyleType(NewBlock.MargArrayO[S]) of
                    bssInset:   PropStack.Last.Props[S] := bssOutset;
                    bssOutset:  PropStack.Last.Props[S] := bssInset;
                  else
                    PropStack.Last.Props[S] := BorderStyleType(NewBlock.MargArrayO[S]);
                  end;
            end;

            for S := BorderTopWidth to BorderLeftWidth do
            begin
              V := PropStack.Last.Props[S];
              if (VarType(V) in varInt) and (V = IntNull) then
              begin
                if Table.brdWidthAttr <= 0 then
                  if Table.HasBorderWidth then
                  else
                    PropStack.Last.Props[S] := 3
                else
                  PropStack.Last.Props[S] := 1
              end;
            end;

            for S := BorderTopColor to BorderLeftColor do
            begin
              V := PropStack.Last.Props[S];
              if (VarType(V) in varInt) and (V = IntNull) then
                PropStack.Last.Props[S] := Table.BorderColor;
            end;

            CellObj := TCellObj.Create(PropStack.MasterList, VAlign, Attributes, PropStack.Last);
            SectionList := CellObj.Cell;
            if ((CellObj.SpecWd.Value = 0) or (CellObj.SpecWd.VType <> wtAbsolute)) and Attributes.Find(NoWrapSy, T) then
              NoBreak := True {this seems to be what IExplorer does}
            else
              NoBreak := False;
            SkipWhiteSpace;
            Next;
            DoBody(TableTermSet);
          end;
          
        CaptionSy:
          begin
            if InHref then
              DoAEnd;
            CurrentStyle := SaveStyle;
            NoBreak := False;
            AddSection;
            if Attributes.Find(AlignSy, T) then
              TopCaption := Lowercase(T.Name) <> 'bottom';
            PushNewProp('caption', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
            if not Assigned(CaptionBlock) then
              CaptionBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SaveSectionList, Attributes);
            SectionList := CaptionBlock.MyCell;
            Next;
            DoBody(TableTermSet);

            SectionList.Add(Section, TagIndex);
            PopAProp('caption');
            Section := nil;
            SectionList := nil;
            if Sy = CaptionEndSy then
              Next; {else it's TDSy, THSy, etc}
          end;

        THeadSy, TBodySy, TFootSy, THeadEndSy, TBodyEndSy, TFootEndSy:
          begin
            AddRow; {if it hasn't been added already}
            while PropStackIndex > HFStack do
              PopProp;
            HFStack := PropStackIndex;
            TdTh := '';
            case Sy of
              THeadSy:
                if Table.Rows.Count = 0 then
                begin
                  RowType := THead;
                  TdTh := 'thead';
                end
                else
                  RowType := TBody;

              TBodySy:
                begin
                  RowType := TBody;
                  TdTh := 'tbody';
                end;

              TFootSy:
                begin
                  RowType := TFoot;
                  TdTh := 'tfoot';
                end;

              THeadEndSy, TBodyEndSy, TFootEndSy:
                RowType := TBody;
            end;
            if TdTh <> '' then
              PushNewProp(TdTh, Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
            Next;
          end;

        TREndSy:
          begin
            AddRow;
            Next;
          end;

        TRSy:
          begin
            AddRow; {if it is still assigned}
            RowStack := PropStackIndex;
            PushNewProp('tr', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
            TrDisplay := PropStack.Last.Display; // Yunqa.de.
            CheckForAlign;
            Row := TCellList.Create(Attributes, PropStack.Last);
            RowVAlign := GetVAlign(AMiddle);
            Next;
          end;

        TDEndSy, THEndSy:
          begin
            AddSection;
            Next;
          end;

        ColSy, ColGroupSy:
          begin
            DoColGroup(Table, ColOK);
            if not Assigned(CM) and Assigned(Table.ColSpecs) then
              CM := TCellManager.Create(Table);
          end;
      else
        begin
          if ((Sy = TextSy) and (LCToken.S = SpcChar)) or (Sy = CommandSy) then
            Next {discard single spaces here}
          else
          begin
            JunkSaveSectionList := SectionList;
            SectionList := SaveSectionList; {the original one}
            DoBody(TableTermSet);
            SectionList.Add(Section, TagIndex);
            Section := nil;
            SectionList := JunkSaveSectionList;
          end;
        end;
      end;
    if InHref then
      DoAEnd;
    AddSection;
    AddRow;
    while PropStackIndex > HFStack do
      PopProp;
    for I := 0 to FootList.Count - 1 do {put TFoot on end of table}
      Table.Rows.Add(FootList[I]);
  finally
    FootList.Free;
    SectionList := SaveSectionList;
    if Assigned(CaptionBlock) then
    begin
      CombineBlock.TopCaption := TopCaption;
      CombineBlock.CaptionBlock := CaptionBlock;
      with CombineBlock.MyCell do
        if TopCaption then
        begin
          Add(CaptionBlock, TagIndex);
          Add(NewBlock, TagIndex);
        end
        else
        begin
          Add(NewBlock, TagIndex);
          Add(CaptionBlock, TagIndex);
        end;
      SectionList.Add(CombineBlock, TagIndex);
      NewBlock.OwnerCell := CombineBlock.MyCell;
    end
    else
    begin
      CombineBlock.CancelUsage;
      CombineBlock.Free; {wasn't needed}
      SectionList.Add(NewBlock, TagIndex);
    end;
    PopaProp('table');
    CurrentStyle := SaveStyle;
    NoBreak := SaveNoBreak;
    ListLevel := SaveListLevel;
    Dec(TableLevel);
    CM.Free;
  end;
  Next;
end;

procedure THtmlParser.GetOptions(Select: TOptionsFormControlObj);
 {get the <option>s for Select form control}
var
  InOption, Selected: Boolean;
  WS: WideString;
  SaveNoBreak: Boolean;
  CodePage: Integer;
  Attr: ThtStringList;
  T: TAttribute;
begin
  SaveNoBreak := NoBreak;
  NoBreak := False;
  CodePage := PropStack.Last.CodePage;
  Next;
  WS := '';
  InOption := False;
  Selected := False;
  Attr := nil;
  while not (Sy in [SelectEndSy, InputSy, PSy, EofSy] + TableTermSet) do
  begin
    case Sy of
      OptionSy, OptionEndSy:
        begin
          WS := WideTrim(WS);
          if InOption then
            Select.AddStr(WS, Selected, Attr, CodePage);
          Selected := False;
          WS := '';
          InOption := Sy = OptionSy;
          if InOption then
          begin
            Selected := Attributes.Find(SelectedSy, T);
            Attr := Attributes.CreateStringList;
          end;
        end;
      TextSy: if InOption then
          WS := WS + LCToken.S;
    end;
    Next;
  end;
  if InOption then
  begin
    WS := WideTrim(WS);
    Select.AddStr(WS, Selected, Attr, CodePage);
  end;
  Select.ResetToValue;
  NoBreak := SaveNoBreak;
end;

{----------------DoMap}

procedure THtmlParser.DoMap;
var
  Item: TMapItem;
  T: TAttribute;
  ErrorCnt: Integer;
begin
  Item := TMapItem.Create;
  ErrorCnt := 0;
  try
    if Attributes.Find(NameSy, T) then
      Item.MapName := Uppercase(T.Name);
    Next;
    while (Sy <> MapEndSy) and (Sy <> EofSy) and (ErrorCnt < 3) do
    begin
      if Sy = AreaSy then
        Item.AddArea(Attributes)
      else if Sy <> TextSy then
        Inc(ErrorCnt);
      Next;
    end;
    if Sy = MapEndSy then
      PropStack.MasterList.MapList.Add(Item)
    else
      Item.Free;
  except
    Item.Free;
    raise;
  end;
  Next;
end;

procedure THtmlParser.DoScript(Ascript: TScriptEvent);
var
  Text: ThtString;

  procedure Next1;
    {Special Next routine to get the next token}

    procedure GetTag1; {simplified 'Pick up a Tag' routine}

      function IsTagChar(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
      begin
        case Ch of
          'a'..'z', 'A'..'Z', '/':
            Result := True;
        else
          Result := False;
        end;
      end;

    var
      Count: Integer;
    begin
      Text := LessChar;
      GetCh;
      if not IsTagChar(LCh) then
      begin
        Sy := TextSy;
        Exit;
      end;
      Sy := CommandSy; {catch all}
      while IsTagChar(LCh) do
      begin
        htAppendChr(Text, LCh);
        GetCh;
      end;
      if CompareText(Text, '</script') = 0 then
        Sy := ScriptEndSy;
      Count := 0;
      while Count < 6 do
      begin
        case LCh of
          CrChar, TabChar:
            htAppendChr(Text, SpcChar);

          GreaterChar, EofChar:
            break;
        else
          htAppendChr(Text, LCh);
        end;
        GetCh;
        Inc(Count);
      end;
      if LCh = GreaterChar then
      begin
        Text := Text + GreaterChar;
        if Sy = ScriptEndSy then
          InScript := False;
        GetCh;
      end;
    end;

  begin {already have fresh character loaded here}
    Text := '';
    case LCh of
      EofChar:
        Sy := EofSy;

      CrChar:
        begin
          Sy := EolSy;
          GetCh;
        end;

      LessChar:
        GetTag1;

    else
      Sy := TextSy;
      while True do
        case LCh of
          CrChar, LessChar, EofChar:
            break;
        else
          htAppendChr(Text, LCh);
          GetCh;
        end;
    end;
  end;

var
  Lang, Name: ThtString;
  T: TAttribute;
  S, Src: ThtString;
begin
  {on entry, do not have the next character for <script>}
  if Assigned(AScript) then
  begin
    InScript := True;
    try
      GetCh; {get character here with Inscript set to allow immediate comment}

      if Attributes.Find(TypeSy, T) then
        Lang := T.Name
      else if Attributes.Find(LanguageSy, T) then
        Lang := T.Name
      else
        Lang := '';

      if Attributes.Find(NameSy, T) then
        Name := T.Name
      else
        Name := '';

      if Attributes.Find(SrcSy, T) then
        Src := T.Name
      else
        Src := '';

      S := '';
      Next1;
      while (Sy <> ScriptEndSy) and (Sy <> EofSy) do
      begin
        if Sy = EolSy then
          S := S + CrChar + LfChar
        else
          S := S + Text;
        Next1;
      end;
      AScript(CallingObject, Name, Lang, Src, S);
    finally
      InScript := False;
    end;
  end
  else
  begin
    GetCh; {make up for not having next character on entry}
    repeat
      Next1;
    until Sy in [ScriptEndSy, EofSy];
  end;
end;

function THtmlParser.DoObjectTag(out C: ThtChar; out N, IX: Integer): Boolean;

  procedure SavePosition;
  begin
    C := LCh;
    N := Doc.Position;
    IX := PropStack.SIndex;
  end;

  procedure Next1;
  begin
    SavePosition;
    Next;
  end;

var
  WantPanel: Boolean;
  SL, Params: ThtStringList;
  Prop: TProperties;
  PO: TPanelObj;
  S: ThtString;
  T: TAttribute;
begin
  Result := False;
  if Assigned(CallingObject) then
  begin
    if Assigned(ThtmlViewer(CallingObject).OnObjectTag) then
    begin
      SL := Attributes.CreateStringList;
      Result := True;
      if not Assigned(Section) then
        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, SectionList, True);
      PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
      Prop := PropStack.Last;
      PO := Section.CreatePanel(Attributes, SectionList);
      PO.ProcessProperties(PropStack.Last);
      WantPanel := False;
      Params := ThtStringList.Create;
      Params.Sorted := False;
      repeat
        SavePosition;
        SkipWhiteSpace;
        Next;
        if Sy = ParamSy then
          with Attributes do
            if Find(NameSy, T) then
            begin
              S := T.Name;
              if Find(ValueSy, T) then
                S := S + '=' + T.Name;
              Params.Add(S);
            end;
      until (Sy <> ParamSy);
      try
        ThtmlViewer(CallingObject).OnObjectTag(CallingObject, PO.Panel, SL, Params, WantPanel);
      finally
        SL.Free;
        Params.Free;
      end;
      if WantPanel then
      begin
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
        Section.AddPanel1(PO, TagIndex);
        PopAProp('object');
        while not (Sy in [ObjectEndSy, EofSy]) do
          Next1;
      end
      else
      begin
        PropStack.MasterList.PanelList.Remove(PO);
        PopAProp('object');
        PO.Free;
      end;
    end
    else
      Next1;
  end
  else
    Next1;
end;

{----------------DoCommonSy}

procedure THtmlParser.DoCommonSy;
var
  I: Integer;
  TxtArea: TTextAreaFormControlObj;
  FormControl: TFormControlObj;
  T: TAttribute;
  Tmp: ThtString;
  HeadingBlock: TBlock;
  HRBlock: THRBlock;
  HorzLine: THorzLine;
  HeadingStr, Link: ThtString;
  Done, FoundHRef: Boolean;
  IO: TFloatingObj;
  Page: TPage;
  SaveSy: Symb;
  Prop: TProperties;
  C: ThtChar;
  N, IX: Integer;

  procedure ChangeTheFont(Sy: Symb; Pre: Boolean);
  var
    FaceName: ThtString;
    CharSet: TFontCharSet;
    CodePage: Integer;
    NewColor: TColor;
    NewSize, I: Integer;
    FontResults: set of (Face, Colr, Siz, CharS);
    DNewSize: double;
    Prop: TProperties;
  begin
    FontResults := [];
    NewSize := 0; {get rid of warning}
    CodePage := CP_UNKNOWN;
    CharSet := DEFAULT_CHARSET;
    for I := 0 to Attributes.Count - 1 do
      with TAttribute(Attributes[I]) do
        case Which of
          SizeSy:
            begin
              if (Length(Name) >= 2) and ((Name[1] = ThtChar('+')) or (Name[1] = ThtChar('-'))) then
                Value := BaseFontSize + Value;
              NewSize := Max(1, Min(7, Value)); {limit 1..7}
              if (Sy = BaseFontSy) then
                BaseFontSize := NewSize;
              Include(FontResults, Siz);
            end;
          ColorSy:
            if ColorFromString(Name, False, NewColor) then
              Include(FontResults, Colr);
          FaceSy:
            if (Sy <> BaseFontSy) and (Name <> '') then
            begin
              FaceName := Name;
              if FaceName <> '' then
                Include(FontResults, Face);
            end;
          CharSetSy:
            if DoCharSet(Name) then
            begin
              Include(FontResults, CharS);
              CharSet := PropStack.Last.CharSet;
              CodePage := PropStack.Last.CodePage;
            end;
        end;
    PushNewProp('font', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
    Prop := TProperties(PropStack.Last);
    Prop.SetFontBG;
    if Prop.HasBorderStyle then {start of inline border}
      PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
    if Colr in FontResults then
    begin
      PropStack.Last.Assign(NewColor or PalRelative, StyleUn.Color);
    end;
    if Siz in FontResults then
    begin
      if Pre then
        DNewSize := PreFontConv[NewSize]
      else
        DNewSize := FontConv[NewSize];
      PropStack.Last.Assign(double(DNewSize), FontSize);
    end;
    if Face in FontResults then
    begin
      PropStack.Last.Assign(ReadFontName(FaceName), FontFamily);
    end;
    if CharS in FontResults then
      PropStack.Last.AssignCharSetAndCodePage(CharSet, CodePage);
  end;

  procedure DoPreSy;
  var
    S: TokenObj;
    Tmp, Link: ThtString;
    Done, InForm, InP: Boolean;
    I, InitialStackIndex: Integer;
    PreBlock, FormBlock, PBlock: TBlock;
    SaveSy: Symb;
    FoundHRef: Boolean;
    Prop: TProperties;
    C: ThtChar;
    N, IX: Integer;
    Before, After, Intact: Boolean;

    procedure CollectPreText;
    // Considers the current data as pure text and collects everything until
    // the input ends or one of the reserved tokens is found.
    var
      Buffer: TCharCollection;
      CodePage, SaveIndex: Integer;
      Entity: ThtString;
    begin
      CodePage := PropStack.Last.CodePage;
      Buffer := TCharCollection.Create;
      try
        while True do
          case LCh of
            #1..#8, EOFChar, LessChar, CrChar:
              break;

            AmperChar:
              begin
                SaveIndex := PropStack.SIndex;
                Entity := GetEntityStr(CodePage);
                if not LinkSearch then
                  Buffer.Add(Entity, SaveIndex);
              end;

          else
            {Get any normal text, including spaces}
            if not LinkSearch then
              Buffer.Add(LCh, PropStack.SIndex);
            GetCh;
          end;

        if Buffer.Size > 0 then
          S.AddString(Buffer);
      finally
        Buffer.Free;
      end;
    end;

    procedure FormEnd;
    begin
      CurrentForm := nil;
      if Assigned(Section) then
      begin
        Section.AddTokenObj(S);
        SectionList.Add(Section, TagIndex);
      end;
      S.Clear;
      Section := nil;
      PopAProp('form');
      SectionList := FormBlock.OwnerCell;
      InForm := False;
    end;

    procedure PEnd;
    begin
      Section.AddTokenObj(S);
      S.Clear;
      if Section.Len > 0 then
        SectionList.Add(Section, TagIndex)
      else
      begin
        Section.CheckFree;
        Section.Free;
      end;
      Section := nil;
      PopAProp('p');
      SectionList := PBlock.OwnerCell;
      InP := False;
    end;

    procedure NewSection;
    begin
      Section.AddTokenObj(S);
      S.Clear;
      SectionList.Add(Section, TagIndex);
      Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
        CurrentUrlTarget, SectionList, False);
    end;

  begin
    InForm := False;
    InP := False;
    S := TokenObj.Create;
    FormBlock := nil;
    try
      SectionList.Add(Section, TagIndex);
      PushNewProp('pre', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
      InitialStackIndex := PropStackIndex;
      PreBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
      SectionList.Add(PreBlock, TagIndex);
      SectionList := PreBlock.MyCell;
      Section := TPreformated.Create(PropStack.MasterList, nil, PropStack.Last,
        CurrentUrlTarget, SectionList, True);
      Done := False;
      while not Done do
        case LCh of
          '<':
            begin
              Next;
              case Sy of
                TextSy: {this would be an isolated LessChar}
                  S.AddUnicodeChar('<', PropStack.SIndex);
                BRSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    SectionList.Add(Section, TagIndex);
               {look for page-break}
                    PushNewProp('br', Attributes.TheClass, '', '', '', Attributes.TheStyle);
                    PropStack.Last.GetPageBreaks(Before, After, Intact);
                    if Before or After then
                      SectionList.Add(TPage.Create(PropStack.MasterList, PropStack.Last), TagIndex);
                    PopAProp('br');
                    Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
                      CurrentUrlTarget, SectionList, False);
                    if LCh = CrChar then
                      GetCh;
                  end;
                PSy:
                  begin
                    if InP then
                      PEnd
                    else if S.Count <> 0 then
                    begin
                      Section.AddTokenObj(S);
                      S.Clear;
                      SectionList.Add(Section, TagIndex);
                    end
                    else
                    begin
                      Section.CheckFree;
                      Section.Free;
                    end;
                    if LCh = CrChar then
                      GetCh;
                    PushNewProp('p', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                    PBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
                    SectionList.Add(PBlock, TagIndex);
                    SectionList := PBlock.MyCell;
                    Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
                      CurrentUrlTarget, SectionList, True);
                    InP := True;
                  end;
                PEndSy:
                  begin
                    if InP then
                    begin
                      PEnd;
                      Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
                        CurrentUrlTarget, SectionList, True);
                    end;
                  end;

                PreEndSy, TDEndSy, THEndSy, TableSy:
                  Done := True;

                  MarkSy, MarkEndSy,
                BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
                  USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
                  SSy, SEndSy, StrikeSy, StrikeEndSy, SpanSy, SpanEndSy,
                  SubSy, SubEndSy, SupSy, SupEndSy, BigSy, BigEndSy, SmallSy, SmallEndSy,
                  LabelSy, LabelEndSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    case Sy of
                      MarkSy,
                      BSy, ISy, StrongSy, EmSy, CiteSy, VarSy, USy, SSy, StrikeSy, SpanSy,
                        SubSy, SupSy, BigSy, SmallSy, LabelSy:
                        begin
                          PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                          Prop := TProperties(PropStack.Last);
                          Prop.SetFontBG;
                          if Prop.HasBorderStyle then {start of inline border}
                            PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
                        end;
                      MarkEndSy,
                      BEndSy, IEndSy, StrongEndSy, EmEndSy, CiteEndSy, VarEndSy, UEndSy,
                        SEndSy, StrikeEndSy, SpanEndSy,
                        SubEndSy, SupEndSy, SmallEndSy, BigEndSy, LabelEndSy:
                        PopAProp(EndSymbToStr(Sy));
                    end;

                    Section.ChangeFont(PropStack.Last);
                  end;

                FontSy, BaseFontSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    ChangeTheFont(Sy, True);
                    Section.ChangeFont(PropStack.Last);
                  end;
                FontEndSy:
                  if PropStackIndex > InitialStackIndex then
                  begin
                    PopAProp('font');
                    Section.AddTokenObj(S);
                    S.Clear;
                    Section.ChangeFont(PropStack.Last);
                  end;
                ASy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    FoundHRef := False;
                    Link := '';

                    for I := 0 to Attributes.Count - 1 do
                      with TAttribute(Attributes[I]) do
                        if (Which = HRefSy) then
                        begin
                          FoundHRef := True;
                          if InHref then
                            DoAEnd;
                          InHref := True;
                          if Attributes.Find(TargetSy, T) then
                            CurrentUrlTarget.Assign(Name, T.Name, Attributes, PropStack.SIndex)
                          else
                            CurrentUrlTarget.Assign(Name, '', Attributes, PropStack.SIndex);
                          if Attributes.Find(TabIndexSy, T) then
                            CurrentUrlTarget.TabIndex := T.Value;
                          Link := 'link';
                          Break;
                        end;
                    PushNewProp('a', Attributes.TheClass, Attributes.TheID, Link,
                      Attributes.TheTitle, Attributes.TheStyle);
                    Prop := TProperties(PropStack.Last);
                    Prop.SetFontBG;
                    if Prop.HasBorderStyle then {start of inline border}
                      PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
                    Section.ChangeFont(PropStack.Last);

                    if Attributes.Find(NameSy, T) then
                    begin
                      Tmp := UpperCase(T.Name);
                 {Author may have added '#' by mistake}
                      if (Length(Tmp) > 0) and (Tmp[1] = '#') then
                        Delete(Tmp, 1, 1);
                      PropStack.MasterList.AddChPosObjectToIDNameList(Tmp, PropStack.SIndex);
                      Section.AnchorName := True;
                    end;
                    if FoundHRef then
                      Section.HRef(HRefSy, PropStack.MasterList, CurrentUrlTarget, Attributes, PropStack.Last);
                  end;
                AEndSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    DoAEnd;
                  end;
                ImageSy:
                  begin
                    Section.AddTokenObj(S);
                    PushNewProp('img', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                    IO := Section.AddImage(Attributes, SectionList, TagIndex);
                    IO.ProcessProperties(PropStack.Last);
                    PopAProp('img');
                    S.Clear;
                  end;
                PanelSy:
                  begin
                    Section.AddTokenObj(S);
                    PushNewProp('panel', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                    IO := Section.AddPanel(Attributes, SectionList, TagIndex);
                    IO.ProcessProperties(PropStack.Last);
                    PopAProp('panel');
                    S.Clear;
                  end;
                ObjectSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    C := LCh;
                    N := Doc.Position;
                    IX := PropStack.SIndex;
                    DoObjectTag(C, N, IX);
                    LCh := C;
                    Doc.Position := N;
                    PropStack.SIndex := IX;
                    if LCh = CrChar then
                      GetCh;
                  end;
                PageSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    SectionList.Add(Section, TagIndex);
                    SectionList.Add(TPage.Create(PropStack.MasterList, PropStack.Last), TagIndex);
                    Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
                      CurrentUrlTarget, SectionList, False);
                  end;
                InputSy, SelectSy:
                  begin
                    SaveSy := Sy;
                    Section.AddTokenObj(S);
                    PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                    FormControl := Section.AddFormControl(Sy, PropStack.MasterList,
                      Attributes, SectionList, TagIndex, PropStack.Last);
                    FormControl.ProcessProperties(PropStack.Last);
                    if Sy = SelectSy then
                      GetOptions(FormControl as TOptionsFormControlObj);
                    PopAProp(SymbToStr(SaveSy));
                    S.Clear; ;
                  end;
                TextAreaSy:
                  begin
                    Section.AddTokenObj(S);
                    PushNewProp('textarea', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                    TxtArea := Section.AddFormControl(TextAreaSy, PropStack.MasterList,
                      Attributes, SectionList, TagIndex, PropStack.Last) as TTextAreaFormControlObj;
                    DoTextArea(TxtArea);
                    TxtArea.ProcessProperties(PropStack.Last);
                    PopAProp('textarea');
                    S.Clear;
                  end;
                FormSy:
                  begin
                    if InP then
                      PEnd;
                    if InForm then
                      FormEnd
                    else if Assigned(Section) then
                    begin
                      Section.AddTokenObj(S);
                      S.Clear;
                      SectionList.Add(Section, TagIndex);
                    end;

                    PushNewProp('form', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                    FormBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
                    SectionList.Add(FormBlock, TagIndex);
                    SectionList := FormBlock.MyCell;
                    CurrentForm := ThtmlForm.Create(PropStack.MasterList, Attributes);
                    Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
                      CurrentUrlTarget, SectionList, True);
                    InForm := True;
                  end;
                FormEndSy:
                  begin
                    if InP then
                      PEnd;
                    if InForm then
                      FormEnd;
                    if not Assigned(Section) then
                      Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
                        CurrentUrlTarget, SectionList, True);
                  end;
                MapSy: DoMap;
                ScriptSy: DoScript(PropStack.MasterList.ScriptEvent);
              end;
            end;

          ^M:
            begin NewSection; GetCh; end;

          #0:
            Done := True;
        else
          case LCh of
            #1..#8, EOFChar, LessChar, CrChar:
              GetCh;
          else
            CollectPreText;
          end;
        end;
      if InForm then
        FormEnd
      else
      begin
        Section.AddTokenObj(S);
        SectionList.Add(Section, TagIndex);
      end;

      // BG, 15.12.2011: Issue 103: Extra whitespace in preformatted blocks
      // In case <pre> and </pre> are written in separate lines, the first and
      // last lines are empty and should not be shown. Thus remove them:
      // (Don't use Trim(), anything else than newline shows the lines.)
      if SectionList[0].Len = 0 then
        SectionList.Delete(0);
      I := SectionList.Count - 1;
      if (I >= 0) and (SectionList[I].Len = 0) then
        SectionList.Delete(I);

      Section := nil;
      while PropStackIndex >= InitialStackIndex do
        PopProp;
      SectionList := PreBlock.OwnerCell;
      if Sy = PreEndSy then
        Next;
    finally
      S.Free;
    end;
  end;

begin
  case Sy of
    TextSy:
      begin
        if not Assigned(Section) then
        begin {don't create a section for a single space}
          if (LCToken.Count >= 1) and (LCToken.S <> SpcChar) then
          begin
            Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
              CurrentUrlTarget, SectionList, True);
            Section.AddTokenObj(LCToken);
          end;
        end
        else
          Section.AddTokenObj(LCToken);
        Next;
      end;
    ImageSy, PanelSy:
      begin
        if not Assigned(Section) then
          Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
            CurrentUrlTarget, SectionList, True);
        PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        Prop := PropStack.Last;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
        if Sy = ImageSy then
          IO := Section.AddImage(Attributes, SectionList, TagIndex)
        else
          IO := Section.AddPanel(Attributes, SectionList, TagIndex);
        IO.ProcessProperties(PropStack.Last);
        PopAProp(SymbToStr(Sy));
        Next;
      end;
    ObjectSy:
      begin
        DoObjectTag(C, N, IX);
      end;
    ObjectEndSy:
      begin
        Next;
      end;
    InputSy, SelectSy:
      begin
        if not Assigned(Section) then
          Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
            CurrentUrlTarget, SectionList, True);
        SaveSy := Sy;
        PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        FormControl := Section.AddFormControl(Sy, PropStack.MasterList, Attributes,
          SectionList, TagIndex, PropStack.Last);
        if Sy = SelectSy then
          GetOptions(FormControl as TOptionsFormControlObj);
        FormControl.ProcessProperties(PropStack.Last);
        PopAProp(SymbToStr(SaveSy));
        Next;
      end;
    TextAreaSy:
      begin
        if not Assigned(Section) then
          Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
            CurrentUrlTarget, SectionList, True);
        PushNewProp('textarea', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        TxtArea := Section.AddFormControl(TextAreaSy, PropStack.MasterList,
          Attributes, SectionList, TagIndex,
          PropStack.Last) as TTextAreaFormControlObj;
        DoTextArea(TxtArea);
        TxtArea.ProcessProperties(PropStack.Last);
        PopAProp('textarea');
        Next;
      end;
    TextAreaEndSy: {a syntax error but shouldn't hang}
      Next;
    PageSy:
      begin
        SectionList.Add(Section, TagIndex);
        Section := nil;
        Page := TPage.Create(PropStack.MasterList, PropStack.Last);
        SectionList.Add(Page, TagIndex);
        Next;
      end;
    BRSy:
      DoBr([]);
    NoBrSy, NoBrEndSy:
      begin
        if Assigned(Section) then
          Section.AddTokenObj(LCToken);
        NoBreak := Sy = NoBrSy;
        Next;
      end;
    WbrSy:
      begin
        if Assigned(Section) then
          Section.AddTokenObj(LCToken);
        Section.AddOpBrk;
        Next;
      end;
    MarkSy,  MarkEndSy,
    BSy, BEndSy, ISy, IEndSy, StrongSy, StrongEndSy, EmSy, EmEndSy,
      CiteSy, CiteEndSy, VarSy, VarEndSy, USy, UEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy:
      begin
        case Sy of
          MarkSy,
          BSy, ISy, StrongSy, EmSy, CiteSy, VarSy, USy, SSy, StrikeSy:
            begin
              PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
              Prop := TProperties(PropStack.Last);
              Prop.SetFontBG;
              if Prop.HasBorderStyle then {start of inline border}
                PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
            end;
          MarkEndSy,
          BEndSy, IEndSy, StrongEndSy, EmEndSy, CiteEndSy, VarEndSy, UEndSy, SEndSy, StrikeEndSy:
            PopAProp(EndSymbToStr(Sy));
        end;
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;

    SubSy, SubEndSy, SupSy, SupEndSy, BigSy, BigEndSy, SmallSy, SmallEndSy:
      begin
        case Sy of
          SubEndSy, SupEndSy, SmallEndSy, BigEndSy:
            begin
              PopAProp(EndSymbToStr(Sy));
            end;
          SubSy, SupSy, BigSy, SmallSy:
            begin
              if not Assigned(Section) then
                Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
                  CurrentUrlTarget, SectionList, True);
              PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
              Prop := TProperties(PropStack.Last);
              Prop.SetFontBG;
              if Prop.HasBorderStyle then
                PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
            end;
        end;

        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;
    CodeSy, TTSy, KbdSy, SampSy, CodeEndSy, TTEndSy, KbdEndSy, SampEndSy,
      SpanSy, SpanEndSy, LabelSy, LabelEndSy:
      begin
        case Sy of
          CodeSy, TTSy, KbdSy, SampSy, SpanSy, LabelSy:
            begin
              PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
              Prop := TProperties(PropStack.Last);
              Prop.SetFontBG;
              if Prop.HasBorderStyle then
                PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
            end;
          CodeEndSy, TTEndSy, KbdEndSy, SampEndSy, SpanEndSy, LabelEndSy:
            PopAProp(EndSymbToStr(Sy));
        end;
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;
    FontEndSy:
      begin
        PopAProp('font');
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;
    FontSy, BaseFontSy:
      begin
        ChangeTheFont(Sy, False);
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;
    ASy:
      begin
        FoundHRef := False;
        Link := '';
        for I := 0 to Attributes.Count - 1 do
          with TAttribute(Attributes[I]) do
            if (Which = HRefSy) then
            begin
              FoundHRef := True;
              if InHref then
                DoAEnd;
              InHref := True;
              if Attributes.Find(TargetSy, T) then
                CurrentUrlTarget.Assign(Name, T.Name, Attributes, PropStack.SIndex)
              else
                CurrentUrlTarget.Assign(Name, '', Attributes, PropStack.SIndex);
              if Attributes.Find(TabIndexSy, T) then
                CurrentUrlTarget.TabIndex := T.Value;
              Link := 'link';
              Break;
            end;
        PushNewProp('a', Attributes.TheClass, Attributes.TheID, Link, Attributes.TheTitle, Attributes.TheStyle);
        Prop := TProperties(PropStack.Last);
        Prop.SetFontBG;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
        if not Assigned(Section) then
          Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
            CurrentUrlTarget, SectionList, True)
        else
          Section.ChangeFont(PropStack.Last);

        if Attributes.Find(NameSy, T) then
        begin
          Tmp := UpperCase(T.Name);
      {Author may have added '#' by mistake}
          if (Length(Tmp) > 0) and (Tmp[1] = '#') then
            Delete(Tmp, 1, 1);
          PropStack.MasterList.AddChPosObjectToIDNameList(Tmp, PropStack.SIndex);
          Section.AnchorName := True;
        end;
        if FoundHRef then
          Section.HRef(HRefSy, PropStack.MasterList, CurrentUrlTarget, Attributes, PropStack.Last);
        Next;
      end;
    AEndSy:
      begin
        DoAEnd;
        Next;
      end;
    HeadingSy:
      if (HeadingLevel in [1..6]) then
      begin
        SectionList.Add(Section, TagIndex);
        HeadingStr := 'h' + IntToStr(HeadingLevel);
        PushNewProp(HeadingStr, Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        CheckForAlign;
        SkipWhiteSpace;
        Next;
        if Sy = CenterSy then
        begin
          PropStack.Last.Assign('center', TextAlign);
          Next;
        end;
        HeadingBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
        SectionList.Add(HeadingBlock, TagIndex);
        SectionList := HeadingBlock.MyCell;

        Section := TSection.Create(PropStack.MasterList, Attributes, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
        Done := False;
        while not Done do
          case Sy of
            MarkSy,
            TextSy, BrSy, NoBrSy, NoBrEndSy, WbrSy, BSy, ISy, BEndSy, IEndSy,
              EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, CiteSy,
              CiteEndSy, VarSy, VarEndSy, SubSy, SubEndSy, SupSy, SupEndSy,
              SSy, SEndSy, StrikeSy, StrikeEndSy, TTSy, CodeSy, KbdSy, SampSy,
              TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, BigEndSy,
              SmallEndSy, BigSy, SmallSy, ASy, AEndSy, SpanSy, SpanEndSy,
              InputSy, TextAreaSy, TextAreaEndSy, SelectSy,
              ImageSy, FontSy, FontEndSy, BaseFontSy, LabelSy, LabelEndSy,
              ScriptSy, ScriptEndSy, PanelSy, HRSy, ObjectSy, ObjectEndSy:
              DoCommonSy;
            CommandSy:
              Next;
            PSy: DoP([]);
            DivSy, HeaderSy, NavSy, ArticleSy,AsideSy,FooterSy, HGroupSy:
              DoDivEtc(Sy, [HeadingEndSy]);
          else
            Done := True;
          end;
        SectionList.Add(Section, TagIndex);
        Section := nil;
        PopAProp(HeadingStr);
        SectionList := HeadingBlock.OwnerCell;
        if Sy = HeadingEndSy then
          Next;
      end
      else
        Next;
    HeadingEndSy: Next; {in case of extra entry}

    HRSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp('hr', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
      {Create Horzline first as it effects the PropStack}
        HorzLine := THorzLine.Create(PropStack.MasterList, Attributes, PropStack.Last);
        HRBlock := THRBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
        HRBlock.MyHRule := Horzline;
        HRBlock.Align := Horzline.Align;
        SectionList.Add(HRBlock, TagIndex);
        SectionList := HRBlock.MyCell;

        SectionList.Add(HorzLine, TagIndex);
        SectionList := HRBlock.OwnerCell;
        PopAProp('hr');
        Section := nil;
        Next;
      end;
    PreSy:
      if not Attributes.Find(WrapSy, T) then
        DoPreSy
      else
      begin
        SectionList.Add(Section, TagIndex);
        Section := nil;
        PushNewProp('pre', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        Next;
      end;
    PreEndSy:
      begin
        PopAProp('pre');
        Next;
      end;
    TableSy: DoTable;
    MapSy: DoMap;
    ScriptSy:
      begin
        DoScript(PropStack.MasterList.ScriptEvent);
        Next;
      end;
  else
    begin
      Assert(False, 'DoCommon can''t handle <' + SymbToStr(Sy) + GreaterChar);
      Next; {as loop protection}
    end;
  end;
end; {DoCommon}

{----------------DoP}

procedure THtmlParser.DoP(const TermSet: SymbSet);
var
  NewBlock: TBlock;
  LastAlign, LastClass, LastID, LastTitle: ThtString;
  LastStyle: TProperties;
begin
  if PSy in TermSet then
    Exit;
  SectionList.Add(Section, TagIndex);
  Section := nil;
  SkipWhiteSpace;
  LastAlign := FindAlignment;
  LastClass := Attributes.TheClass;
  LastID := Attributes.TheID;
  LastStyle := Attributes.TheStyle;
  LastTitle := Attributes.TheTitle;
  Next;
  while Sy in [PSy, PEndSy] do
  begin {recognize only the first <p>}
    if Sy = PSy then
    begin
      LastAlign := FindAlignment; {if a series of <p>, get last alignment}
      LastClass := Attributes.TheClass;
      LastID := Attributes.TheID;
      LastStyle := Attributes.TheStyle;
      LastTitle := Attributes.TheTitle;
    end;
    SkipWhiteSpace;
    Next;
  end;
{at this point have the 'next' attributes, so use 'Last' items here}
  PushNewProp('p', LastClass, LastID, '', LastTitle, LastStyle);
  if LastAlign <> '' then
    PropStack.Last.Assign(LastAlign, TextAlign);

  NewBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
  SectionList.Add(NewBlock, TagIndex);
  SectionList := NewBlock.MyCell;

  while not (Sy in Termset) and
    (Sy in [TextSy, NoBrSy, NoBrEndSy, WbrSy, MarkSy, MarkEndSy, BSy, ISy, BEndSy, IEndSy,
    EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, CiteSy,
      CiteEndSy, VarSy, VarEndSy, SubSy, SubEndSy, SupSy, SupEndSy,
      SSy, SEndSy, StrikeSy, StrikeEndSy, TTSy, CodeSy, KbdSy, SampSy,
      TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy,
      SmallEndSy, BigSy, SmallSy, ASy, AEndSy, SpanSy, SpanEndSy,
      InputSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
      ImageSy, FontSy, BaseFontSy, BRSy, ObjectSy, ObjectEndSy,
      MapSy, PageSy, ScriptSy, ScriptEndSy, PanelSy, CommandSy])
  do
    if Sy <> CommandSy then
      DoCommonSy
    else
      Next; {unknown tag}
  if Sy = TableSy then
    NewBlock.MargArray[MarginBottom] := 0; {open paragraph followed by table, no space}
  SectionList.Add(Section, TagIndex);
  Section := nil;
  PopAProp('p');
  SectionList := NewBlock.OwnerCell;
  if Sy = PEndSy then
    Next;
end;

{----------------DoBr}

procedure THtmlParser.DoBr(const TermSet: SymbSet);
var
  T: TAttribute;
  Before, After, Intact: Boolean;
begin
  if BRSy in TermSet then
    Exit;
  if Attributes.Find(ClearSy, T) then
  begin
    if Assigned(Section) then
      SectionList.Add(Section, TagIndex);
    Section := TSection.Create(PropStack.MasterList, Attributes, PropStack.Last,
      CurrentUrlTarget, SectionList, False);
    PushNewProp('br', Attributes.TheClass, '', '', '', Attributes.TheStyle);
    PropStack.Last.GetPageBreaks(Before, After, Intact);
    PopAProp('br');
    if Before or After then
    begin
      SectionList.Add(Section, TagIndex);
      SectionList.Add(TPage.Create(PropStack.MasterList, PropStack.Last), TagIndex);
      Section := TSection.Create(PropStack.MasterList, Attributes, PropStack.Last, CurrentUrlTarget, SectionList, False);
    end;
  end
  else
  begin
    if not Assigned(Section) then
      Section := TSection.Create(PropStack.MasterList, Attributes, PropStack.Last, CurrentUrlTarget, SectionList, False);
    Section.AddChar(#8, TagIndex);
    SectionList.Add(Section, TagIndex);
    PushNewProp('br', Attributes.TheClass, '', '', '', Attributes.TheStyle);
    PropStack.Last.GetPageBreaks(Before, After, Intact);
    PopAProp('br');
    if Before or After then
      SectionList.Add(TPage.Create(PropStack.MasterList, PropStack.Last), TagIndex);
    Section := TSection.Create(PropStack.MasterList, Attributes, PropStack.Last, CurrentUrlTarget, SectionList, False);
  end;
  Next;
end;

procedure THtmlParser.DoListItem(
  {$ifdef DO_LI_INLINE}var LiBlock: TBlockLi; var LiSection: TSection;{$endif}
  BlockType, Sym: Symb; LineCount: Integer; Index: ThtChar; Plain: Boolean; const TermSet: SymbSet);
var
{$ifdef DO_LI_INLINE}
  IsInline: Boolean;
  IsFirst: Boolean;
{$else}
  LiBlock: TBlock;
  LISection: TSection;
{$endif}
begin
  SectionList.Add(Section, TagIndex);
  PushNewProp(SymbToStr(Sym), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
{$ifdef DO_LI_INLINE}
  IsInline := PropStack.Last.Display = pdInline;
  IsFirst := not IsInline or (LiSection = nil) or not (LiSection.Display = pdInline);
  if IsFirst then
{$endif}
  begin
    LiBlock := TBlockLI.Create(PropStack.MasterList, PropStack.Last, SectionList, BlockType, Plain, Index, LineCount, ListLevel, Attributes);
    SectionList.Add(LiBlock, TagIndex);
    LiSection := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, LiBlock.MyCell, True);
    Section := LISection;
{$ifdef DO_LI_INLINE}
  end
  else
  begin
    Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, LiBlock.MyCell, True);
{$endif}
  end;
  SectionList := LiBlock.MyCell;

  SkipWhiteSpace;
  Next;
  while true do {handle second part like after a <p>}
    case Sy of
      TextSy, NoBrSy, NoBrEndSy, WbrSy, MarkSy, MarkEndSy, BSy, ISy, BEndSy, IEndSy,
      EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, CiteSy,
      CiteEndSy, VarSy, VarEndSy, SubSy, SubEndSy, SupSy, SupEndSy,
      SSy, SEndSy, StrikeSy, StrikeEndSy, TTSy, CodeSy, KbdSy, SampSy,
      TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy,
      SmallEndSy, BigSy, SmallSy, ASy, AEndSy, SpanSy, SpanEndSy,
      InputSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
      ImageSy, FontSy, BaseFontSy, BrSy, HeadingSy,
      MapSy, PageSy, ScriptSy, ScriptEndSy, PanelSy, ObjectSy, ObjectEndSy:
        DoCommonSy;

      PSy:
        if BlockType in [OLSy, ULSy, DirSy, MenuSy, DLSy] then
          DoP([])
        else
          break; {else terminate lone <li>s on <p>}

      PEndSy,
      CommandSy:
        Next;

      DivSy, HeaderSy, NavSy, ArticleSy, AsideSy, FooterSy, HGroupSy,
      CenterSy, FormSy, AddressSy, BlockquoteSy, FieldsetSy:
        DoDivEtc(Sy, TermSet);

      OLSy, ULSy, DirSy, MenuSy, DLSy:
        begin
          DoLists(Sy, TermSet);
          LiBlock.MyCell.CheckLastBottomMargin;
          Next;
        end;

      TableSy:
        DoTable;
    else
      break;
    end;

  if Assigned(Section) and (Section = LISection) and (Section.Len = 0) then
    Section.AddChar(WideChar(160), TagIndex); {so that bullet will show on blank <li>}
  SectionList.Add(Section, TagIndex);
  Section := nil;
  SectionList.CheckLastBottomMargin;
  PopAProp(SymbToStr(Sym));
  SectionList := LiBlock.OwnerCell;
{$ifdef DO_LI_INLINE}
  if not IsInline then
  begin
    LiBlock := nil;
    LiSection := nil;
  end;
{$endif}
end;

{-------------DoLists}

procedure THtmlParser.DoLists(Sym: Symb; const TermSet: SymbSet);
var
  T: TAttribute;
  LineCount: Integer;
  Plain: Boolean;
  Index: ThtChar;
  NewBlock: TBlock;
  EndSym: Symb;
{$ifdef DO_LI_INLINE}
  LiBlock: TBlockLi;
  LiSection: TSection;
{$endif}
begin
  LineCount := 1;
  Index := '1';
  EndSym := EndSymbFromSymb(Sym);
  if EndSym = CommandSy then
    EndSym := HtmlSy;
  Plain := False;
  case Sym of
    OLSy:
      begin
        if Attributes.Find(StartSy, T) then
          if T.Value >= 0 then
           LineCount := T.Value;
        if Attributes.Find(TypeSy, T) and (T.Name <> '') then
         Index := T.Name[1];
      end;
    ULSy:
      Plain := Attributes.Find(PlainSy, T)
           or (Attributes.Find(TypeSy, T) and ((Lowercase(T.Name) = 'none') or (Lowercase(T.Name) = 'plain')));
  end;
  SectionList.Add(Section, TagIndex);
  Section := nil;
  PushNewProp(SymbToStr(Sym), Attributes.TheClass, Attributes.TheID, '',
    Attributes.TheTitle, Attributes.TheStyle);

  NewBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
// BG, 25.03.2012: unused:  NewBlock.IsListBlock := not (Sym in [AddressSy, BlockquoteSy, DLSy]);
  SectionList.Add(NewBlock, TagIndex);
  SectionList := NewBlock.MyCell;
  Next;
  if Sy in [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, BlockQuoteEndSy] then
  begin {guard against <ul></ul> and similar combinations}
    PopAProp(EndSymbToStr(Sy));
    SectionList := NewBlock.OwnerCell;
    Exit;
  end;
  if Sym in [ULSy, OLSy, DirSy, MenuSy, DLSy] then
    Inc(ListLevel);
  repeat
    case Sy of
      LISy, DDSy, DTSy:
        begin
          if (Sy = LiSy) and Attributes.Find(ValueSy, T) and (T.Value <> 0) then
            LineCount := T.Value;
          DoListItem({$ifdef DO_LI_INLINE}LiBlock, LiSection, {$endif}Sym, Sy, LineCount, Index, Plain, TermSet);
          Inc(LineCount);
        end;
      OLSy, ULSy, DirSy, MenuSy, DLSy:
        begin
          DoLists(Sy, TermSet);
          if not (Sy in TermSet) then
            Next;
        end;

      PSy:
        DoP(TermSet);

      BlockQuoteSy, AddressSy:
        DoDivEtc(Sy, TermSet);
      DivSy, HeaderSy, NavSy, ArticleSy, AsideSy, FooterSy, HGroupSy,
      CenterSy, FormSy:
        DoDivEtc(Sy, [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, LISy, DDSy, DTSy, EofSy] + TermSet);

      TextSy, BRSy, HRSy, TableSy,
        MarkSy, MarkEndSy,
        BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy,
        TTSy, CodeSy, KbdSy, SampSy, TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
        NameSy, HRefSy, ASy, AEndSy, SpanSy, SpanEndSy,
        HeadingSy, HeadingEndSy, PreSy,
        InputSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
        ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
        SmallEndSy, MapSy, PageSy, ScriptSy, PanelSy, NoBrSy, NoBrEndSy, WbrSy,
        ObjectSy, ObjectEndSy:
        DoCommonSy;
    else
      if Sy in TermSet then {exit below}
      else
        Next;
    end;
  until (Sy in [EndSym, EofSy]) or (Sy in TermSet);
  if Sym in [ULSy, OLSy, DirSy, MenuSy, DLSy] then
    Dec(ListLevel);
  SectionList.Add(Section, TagIndex);
  if SectionList.CheckLastBottomMargin then
  begin
    NewBlock.MargArray[MarginBottom] := ParagraphSpace;
    NewBlock.BottomAuto := True;
  end;
  Section := nil;
  PopAProp(SymbToStr(Sym)); {maybe save stack position}
  SectionList := NewBlock.OwnerCell;
end;

{----------------DoBase}

procedure THtmlParser.DoBase;
var
  I: Integer;
begin
  with Attributes do
    for I := 0 to Count - 1 do
      with TAttribute(Attributes[I]) do
        if Which = HrefSy then
          FBase := Name
        else if Which = TargetSy then
          FBaseTarget := Name;
  Next;
end;

{----------------DoSound}

procedure THtmlParser.DoSound;
var
  Loop: Integer;
  T, T1: TAttribute;
begin
  if Assigned(SoundEvent) and Attributes.Find(SrcSy, T) then
  begin
    if Attributes.Find(LoopSy, T1) then
      Loop := T1.Value
    else
      Loop := 1;
    SoundEvent(CallingObject, T.Name, Loop, False);
  end;
  Next;
end;

{----------------DoMeta}

procedure THtmlParser.DoMeta(Sender: TObject);
var
  T: TAttribute;
  HttpEq, Name, Content: ThtString;
begin
  if Attributes.Find(HttpEqSy, T) then
    HttpEq := T.Name
  else
    HttpEq := '';
  if Attributes.Find(NameSy, T) then
    Name := T.Name
  else
    Name := '';
  if Attributes.Find(ContentSy, T) then
    Content := T.Name
  else if Attributes.Find(CharSetSy, T) then begin // ANGUS    <meta charset="utf-8"> from HTML5
    HttpEq := 'content-type';
    Content := T.Name;
  end
  else
    Content := '';
  if (Sender is ThtmlViewer) and (CompareText(HttpEq, 'content-type') = 0) then
  begin
    DoCharset(Content);
    if CallingObject is ThtmlViewer then
    begin
      ThtmlViewer(CallingObject).Charset := PropStack.Last.Charset;
      ThtmlViewer(CallingObject).CodePage := PropStack.Last.CodePage;
    end;
  end;
  if Assigned(MetaEvent) then
    MetaEvent(Sender, HttpEq, Name, Content);
  Next;
end;

{----------------DoTitle}

procedure THtmlParser.DoTitle;
begin
  TitleStart := PropStack.SIndex;
  TitleEnd := TitleStart;
  Next;
  while Sy = TextSy do
  begin
    TitleEnd := PropStack.SIndex;
    Next;
  end;
end;

procedure THtmlParser.DoStyleLink; {handle <link> for stylesheets}
var
  Style: TBuffer;
  C: ThtChar;
  I: Integer;
  Url, Rel, Rev: ThtString;
  OK: Boolean;
  Request: TGetStreamEvent;
  DStream: TStream;
  RStream: TMemoryStream;
  Viewer: ThtmlViewer;
  Path: ThtString;
  FreeDStream: Boolean;
begin
  OK := False;
  for I := 0 to Attributes.Count - 1 do
    with TAttribute(Attributes[I]) do
      case Which of
        RelSy:
          begin
            Rel := Name;
            if CompareText(Rel, 'stylesheet') = 0 then
              OK := True;
          end;

        RevSy:
          Rev := Name;

        HRefSy:
          Url := Name;

        MediaSy:
          //BG, 12.02.2011: currently we cannot distinguish media, therefore process 'screen' only.
          if (Pos('screen', Name) = 0) and (Pos('all', Name) = 0) then
          begin
            OK := False;
            break;
          end;
      end;
  if OK and (Url <> '') then
  begin
    DStream := nil;
    FreeDStream := True;
    try
      try
        Viewer := (CallingObject as ThtmlViewer);
        Request := Viewer.OnHtStreamRequest;
        if Assigned(Request) then
        begin
          RStream := nil;
          if Assigned(Viewer.OnExpandName) then
          begin {must be using TFrameBrowser}
            Viewer.OnExpandName(Viewer, Url, Url);
            Path := GetURLBase(Url);
            Request(Viewer, Url, RStream);
            DStream := RStream;
            FreeDStream := False;
          end
          else
          begin
            Path := ''; {for TFrameViewer requests, don't know path}
            Request(Viewer, Url, RStream);
            if Assigned(RStream) then
            begin
              DStream := RStream;
              FreeDStream := False;
            end
            else
            begin {try it as a file}
              Url := Viewer.HTMLExpandFilename(Url);
              Path := ExtractFilePath(Url);
              if FileExists(Url) then
                DStream := TFileStream.Create(Url, fmOpenRead or fmShareDenyWrite);
            end;
          end;
        end
        else {assume it's a file}
        begin
          Url := Viewer.HTMLExpandFilename(Url);
          Path := ExtractFilePath(Url);
          if FileExists(Url) then
            DStream := TFileStream.Create(Url, fmOpenRead or fmShareDenyWrite);
        end;
        if DStream <> nil then
        begin
          DStream.Position := 0;
          Style := TBuffer.Create(DStream, Url);
          try
            C := SpcChar;
            DoStyle(FPropStack.MasterList.Styles, C, Style, Path, True, FUseQuirksMode);
          finally
            Style.Free;
          end;
        end;
      except
      end;
    finally
      if FreeDStream then
        DStream.Free;
    end;
  end;
  if Assigned(LinkEvent) then
    LinkEvent(CallingObject, Rel, Rev, Url);
  Next;
end;

{-------------DoBody}

procedure THtmlParser.DoBody(const TermSet: SymbSet);
var
  I: Integer;
  Val: TColor;
  AMarginHeight, AMarginWidth: Integer;
{$ifdef DO_LI_INLINE}
  LiBlock: TBlockLi;
  LiSection: TSection;
{$endif}
begin
  repeat
    if Sy in TermSet then
      Exit;
    case Sy of
      TextSy, BRSy, HRSy,
        NameSy, HRefSy, ASy, AEndSy,
        MarkSy, MarkEndSy,
        BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy,
        TTSy, CodeSy, KbdSy, SampSy, TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, SpanSy, SpanEndSy,
        HeadingSy, HeadingEndSy, PreSy, TableSy,
        InputSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
        ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
        SmallEndSy, MapSy, PageSy, ScriptSy, PanelSy, NoBrSy, NoBrEndSy, WbrSy,
        ObjectSy, ObjectEndSy:
        DoCommonSy;
      BodySy:
        begin
          if (BodyBlock.MyCell.Count = 0) and (TableLevel = 0) then {make sure we're at beginning}
          begin
            PropStack.MasterList.ClearLists;
            if Assigned(Section) then
            begin
              Section.CheckFree;
              Section.Free; {Will start with a new section}
            end;
            PushNewProp('body', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
            AMarginHeight := (CallingObject as ThtmlViewer).MarginHeight;
            AMarginWidth := (CallingObject as ThtmlViewer).MarginWidth;
            for I := 0 to Attributes.Count - 1 do
              with TAttribute(Attributes[I]) do
                case Which of
                  BackgroundSy: PropStack.Last.Assign('url(' + Name + ')', BackgroundImage);
                  TextSy:
                    if ColorFromString(Name, False, Val) then
                      PropStack.Last.Assign(Val or PalRelative, Color);
                  BGColorSy:
                    if ColorFromString(Name, False, Val) then
                      PropStack.Last.Assign(Val or PalRelative, BackgroundColor);
                  LinkSy:
                    if ColorFromString(Name, False, Val) then
                      PropStack.MasterList.Styles.ModifyLinkColor('link', Val);
                  VLinkSy:
                    if ColorFromString(Name, False, Val) then
                      PropStack.MasterList.Styles.ModifyLinkColor('visited', Val);
                  OLinkSy:
                    if ColorFromString(Name, False, Val) then
                    begin
                      PropStack.MasterList.Styles.ModifyLinkColor('hover', Val);
                      PropStack.MasterList.LinksActive := True;
                    end;
                  MarginWidthSy, LeftMarginSy:
                    AMarginWidth := Min(Max(0, Value), 200);
                  MarginHeightSy, TopMarginSy:
                    AMarginHeight := Min(Max(0, Value), 200);
                  BGPropertiesSy:
                    if CompareText(Name, 'fixed') = 0 then
                      PropStack.Last.Assign('fixed', BackgroundAttachment);
                end;
            if FUseQuirksMode then begin
              FPropStack.MasterList.Styles.FixupTableColor(FPropStack.Last);
            end;
            PropStack.Last.Assign(AMarginWidth, MarginLeft);
            PropStack.Last.Assign(AMarginWidth, MarginRight);
            PropStack.Last.Assign(AMarginHeight, MarginTop);
            PropStack.Last.Assign(AMarginHeight, MarginBottom);

            SectionList := BodyBlock.OwnerCell;
            SectionList.Remove(BodyBlock);
            BodyBlock := TBodyBlock.Create(FPropStack.MasterList, FPropStack.Last, SectionList, Attributes);
            SectionList.Add(BodyBlock, TagIndex);
            SectionList := BodyBlock.MyCell;

            Section := TSection.Create(FPropStack.MasterList, nil, FPropStack.Last, nil, SectionList, True);
          end;
          Next;
        end;
      OLSy, ULSy, DirSy, MenuSy, DLSy:
        begin
          DoLists(Sy, TermSet);
          if not (Sy in TermSet) then
            Next;
        end;

      LISy:
        DoListItem({$ifdef DO_LI_INLINE}LiBlock, LiSection, {$endif}LiAloneSy, Sy, 1, '1', False, TermSet);

      DDSy, DTSy:
        DoListItem({$ifdef DO_LI_INLINE}LiBlock, LiSection, {$endif}DLSy, Sy, 1, '1', False, TermSet);

      PSy: DoP(TermSet);

      FormEndSy:
        begin
          CurrentForm := nil;
          Next;
        end;

      DivSy, HeaderSy, NavSy, ArticleSy, AsideSy, FooterSy, HGroupSy,
       CenterSy, FormSy, BlockQuoteSy, AddressSy, FieldsetSy, LegendSy:
        DoDivEtc(Sy, TermSet);

      TitleSy:
        DoTitle;

      LinkSy:
        DoStyleLink;

      StyleSy:
        begin
          DoStyle(FPropStack.MasterList.Styles, LCh, Doc, '', False,FUseQuirksMode);
          Next;
        end;

      BgSoundSy:
        DoSound;
      MetaSy:
        DoMeta(CallingObject);
      BaseSy:
        DoBase;
    else
      Next;
    end;
  until (Sy = EofSy);
  Next;
end;

procedure THtmlParser.DoFrameSet(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: ThtString);
var
  NewFrameSet: TObject;
begin
  FrameViewer.DoAttributes(FrameSet, Attributes);
  Next;
  while (Sy <> FrameSetEndSy) and (Sy <> EofSy) do
  begin
    case Sy of
      FrameSy:
        begin
          FrameViewer.AddFrame(FrameSet, Attributes, FName);
        end;
      FrameSetSy:
        begin
          NewFrameSet := FrameViewer.CreateSubFrameSet(FrameSet);
          DoFrameSet(FrameViewer, NewFrameSet, FName);
        end;
      NoFramesSy:
        begin
          repeat
            Next;
          until (Sy = NoFramesEndSy) or (Sy = EofSy);
        end;
      ScriptSy:
        begin
          DoScript(FrameViewer.OnScript);
          Next;
        end;
    end;
    Next;
  end;
  TSubFrameSetBase(FrameSet).Parsed(Title, Base, BaseTarget);
end;

{----------------ParseInit}

procedure THtmlParser.ParseInit(ASectionList: ThtDocument; AIncludeEvent: TIncludeType);
begin
  SectionList := ASectionList;
  FUseQuirksMode := ASectionList.UseQuirksMode;
  FPropStack.MasterList := ASectionList;
  CallingObject := ASectionList.TheOwner;
  IncludeEvent := AIncludeEvent;
  FPropStack.Clear;
  FPropStack.Add(TProperties.Create(FPropStack,FUseQuirksMode));
  FPropStack[0].CopyDefault(FPropStack.MasterList.Styles.DefProp);
  FPropStack.SIndex := -1;

  if CallingObject is ThtmlViewer then
    ThtmlViewer(CallingObject).CodePage := PropStack[0].CodePage;

  BodyBlock := TBodyBlock.Create(PropStack.MasterList, PropStack[0], SectionList, nil);
  SectionList.Add(BodyBlock, TagIndex);
  SectionList := BodyBlock.MyCell;

  CurrentURLTarget := TUrlTarget.Create;
  InHref := False;
  BaseFontSize := 3;

  FBase := '';
  FBaseTarget := '';
  CurrentStyle := [];
  CurrentForm := nil;
  Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, nil, SectionList, True);
  Attributes := TAttributeList.Create;
  InScript := False;
  NoBreak := False;
  InComment := False;
  ListLevel := 0;
  TableLevel := 0;
  LinkSearch := False;
end;

//-- BG ---------------------------------------------------------- 27.12.2010 --
procedure THtmlParser.ParseHtml(ASectionList: ThtDocument;
  AIncludeEvent: TIncludeType; ASoundEvent: TSoundType;
  AMetaEvent: TMetaType; ALinkEvent: TLinkType);
{$IFNDEF NoTabLink}
const
  MaxTab = 400; {maximum number of links before tabbing of links aborted}
var
  TabCount, SaveSIndex, SavePosition: Integer;
  T: TAttribute;
{$ENDIF}
begin
  Self.Doc := Doc;
  FPropStack := ASectionList.PropStack;
  try
    IncludeEvent := AIncludeEvent;
    ParseInit(ASectionList,IncludeEvent);

    try
{$IFNDEF NoTabLink}
      SaveSIndex := PropStack.SIndex;
      SavePosition := Doc.Position;
      LinkSearch := True;
      SoundEvent := nil;
      MetaEvent := nil;
      LinkEvent := nil;
      TabCount := 0;
      try
        GetCh; {get the reading started}
        Next;
        while Sy <> EofSy do
        begin
          if (Sy = ASy) and Attributes.Find(HrefSy, T) then
          begin
            Inc(TabCount);
            if TabCount > MaxTab then
              break;
          end;
          Next;
        end;
        ASectionList.StopTab := TabCount > MaxTab;
      except
      end;
  {reset a few things}
      PropStack.SIndex := SaveSIndex;
      Doc.Position := SavePosition;
{$ENDIF}

      LinkSearch := False;
      SoundEvent := ASoundEvent;
      MetaEvent := AMetaEvent;
      LinkEvent := ALinkEvent;
      try
        GetCh; {get the reading started}
        Next;
        DoBody([]);
      except
        on E: Exception do
          Assert(False, E.Message);
      end;

    finally
      Attributes.Free;
      if Assigned(Section) then
        SectionList.Add(Section, TagIndex);
      FPropStack.Clear;
      CurrentURLTarget.Free;
    end; {finally}
  finally
    FPropStack := nil;
  end;
end;

{----------------DoText}

procedure THtmlParser.DoText;
var
  S: TokenObj;
  Done: Boolean;
  PreBlock: TBlock;

  procedure NewSection;
  begin
    Section.AddTokenObj(S);
    S.Clear;
    SectionList.Add(Section, TagIndex);
    Section := TPreFormated.Create(PropStack.MasterList, nil, PropStack.Last,
      CurrentUrlTarget, SectionList, False);
  end;

begin
  S := TokenObj.Create;
  try
    SectionList.Add(Section, TagIndex);
    PushNewProp('pre', Attributes.TheClass, Attributes.TheID, '', '', Attributes.TheStyle);
    PreBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
    SectionList.Add(PreBlock, TagIndex);
    SectionList := PreBlock.MyCell;
    Section := TPreformated.Create(PropStack.MasterList, nil, PropStack.Last,
      CurrentUrlTarget, SectionList, False);
    Done := False;
    while not Done do
      case LCh of
        ^M:
          begin NewSection; GetCh; end;
        #0: Done := True;
      else
        begin {all other chars}
          S.AddUnicodeChar(WideChar(LCh), PropStack.SIndex);
          if S.Count > 200 then
          begin
            Section.AddTokenObj(S);
            S.Clear;
          end;
          GetCh;
        end;
      end;
    Section.AddTokenObj(S);
    SectionList.Add(Section, TagIndex);
    Section := nil;
    PopAProp('pre');
    SectionList := PreBlock.OwnerCell;
  finally
    S.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 27.12.2010 --
procedure THtmlParser.ParseText(ASectionList: ThtDocument);
begin
  FPropStack := ASectionList.PropStack;
  try
  Self.Doc := Doc;
  ParseInit(ASectionList, nil);
  InScript := True;

  try
    GetCh; {get the reading started}
    DoText;

  finally
    Attributes.Free;
    if Assigned(Section) then
      SectionList.Add(Section, TagIndex);
    PropStack.Clear;
    CurrentUrlTarget.Free;
  end; {finally}
  finally
    FPropStack := nil;
  end;
end;

{-------------FrameParseString}

procedure THtmlParser.ParseFrame(FrameViewer: TFrameViewerBase; FrameSet: TObject;
  const FName: ThtString; AMetaEvent: TMetaType);

  procedure Parse;
  var
    SetExit: Boolean;
  begin
    SetExit := False;
    PropStack.Clear;
    PropStack.Add(TProperties.Create(FPropStack,False));
    GetCh; {get the reading started}
    Next;
    repeat
      case Sy of
        FrameSetSy:
          begin
            DoFrameSet(FrameViewer, FrameSet, FName);
          end;
        BaseSy: DoBase;
        TitleSy: DoTitle;
        BgSoundSy: DoSound;
        ScriptSy:
          begin DoScript(FrameViewer.OnScript); Next; end;
        NoFramesSy:
          begin
            repeat
              Next;
            until (Sy = NoFramesEndSy) or (Sy = EofSy);
            Next;
          end;
        MetaSy: DoMeta(FrameSet);
        BodySy, HeadingSy, HRSy, TableSy, ImageSy, OLSy, ULSy, MenuSy, DirSy,
          PSy, PreSy, FormSy, AddressSy, BlockQuoteSy, DLSy:
          SetExit := True;
      else
        Next;
      end;
    until SetExit or (Sy = EofSy);
    PropStack.Clear;
  end;

begin
  FPropStack := THTMLPropStack.Create;
  try
  Self.Doc := Doc;
  FPropStack.MasterList := nil;
  CallingObject := FrameViewer;
  IncludeEvent := FrameViewer.OnInclude;
  SoundEvent := FrameViewer.OnSoundRequest;
  MetaEvent := AMetaEvent;
  LinkEvent := FrameViewer.OnLink;

  FBase := '';
  FBaseTarget := '';
  InScript := False;
  NoBreak := False;
  InComment := False;
  ListLevel := 0;

  Attributes := TAttributeList.Create;
  try
    try
      Parse;
    except {ignore error}
      on E: Exception do
        Assert(False, E.Message);
    end;
  finally
    Attributes.Free;
  end;
  finally
    FreeAndNil(FPropStack);
  end;
end;

{----------------IsFrameString}

function THtmlParser.IsFrame(FrameViewer: TFrameViewerBase): Boolean;

  function Parse: Boolean;
  var
    SetExit: Boolean;

  begin
    Result := False;
    FPropStack.Clear;
    FPropStack.Add(TProperties.Create(FPropStack, FUseQuirksMode ));
    SetExit := False;
    GetCh; {get the reading started}
    Next;
    repeat
      case Sy of
        FrameSetSy:
          begin
            Result := True;
            break;
          end;
        ScriptSy:
          begin DoScript(nil); Next; end; {to skip the script stuff}

        BodySy, HeadingSy, HRSy, TableSy, ImageSy, OLSy, ULSy, MenuSy, DirSy,
          PSy, PreSy, FormSy, AddressSy, BlockQuoteSy, DLSy:
          SetExit := True;
      else
        Next;
      end;
    until SetExit or (Sy = EofSy);
    FPropStack.Clear;
  end;

var
  Pos: Integer;
begin
  if Doc = nil then
  begin
    Result := False;
    exit;
  end;
  FPropStack := THTMLPropStack.Create;
  try
    FPropStack.MasterList := nil;
    CallingObject := FrameViewer;
    SoundEvent := nil;

    FBase := '';
    FBaseTarget := '';
    Result := False;
    InScript := False;
    NoBreak := False;
    InComment := False;

    Pos := Doc.Position;
    Attributes := TAttributeList.Create;
    try
      Self.Doc := Doc;
      try
        Result := Parse;
      except {ignore error}
        on E: Exception do
          Assert(False, E.Message);
      end;
    finally
      Attributes.Free;
      Doc.Position := Pos;
    end;
  finally
    FreeAndNil( FPropStack );
  end;
end;

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
    SetLength(Collect, Length(Collect) + 1);
    Collect[Length(Collect)] := LCh;
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
          begin
            SetLength(Entity, Length(Entity) + 1);
            Entity[Length(Entity)] := LCh;
          end;
        else
          break;
        end;
        Inc(N);
        NextCh;
      until N > 10;

      // Now convert entity ThtString into a character value. If there is no
      // entity with that name simply add all characters as they are.
      if Entities.Find(Entity, I) then
      begin
        I := PEntity(Entities.Objects[I]).Value;
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

// Taken from http://www.w3.org/TR/REC-html40/sgml/entities.html.

const
  // Note: the entities will be sorted into a ThtStringList to make binary search possible.
  EntityDefinitions: array[1..253] of TEntity = (
    // ISO 8859-1 characters
    (Name: 'nbsp'; Value: 160), // no-break space = non-breaking space, U+00A0 ISOnum
    (Name: 'iexcl'; Value: 161), // inverted exclamation mark, U+00A1 ISOnum
    (Name: 'cent'; Value: 162), // cent sign, U+00A2 ISOnum
    (Name: 'pound'; Value: 163), // pound sign, U+00A3 ISOnum
    (Name: 'curren'; Value: 164), // currency sign, U+00A4 ISOnum
    (Name: 'yen'; Value: 165), // yen sign = yuan sign, U+00A5 ISOnum
    (Name: 'brvbar'; Value: 166), // broken bar = broken vertical bar, U+00A6 ISOnum
    (Name: 'sect'; Value: 167), // section sign, U+00A7 ISOnum
    (Name: 'uml'; Value: 168), // diaeresis = spacing diaeresis, U+00A8 ISOdia
    (Name: 'copy'; Value: 169), // copyright sign, U+00A9 ISOnum
    (Name: 'ordf'; Value: 170), // feminine ordinal indicator, U+00AA ISOnum
    (Name: 'laquo'; Value: 171), // left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum
    (Name: 'not'; Value: 172), // not sign, U+00AC ISOnum
    (Name: 'shy'; Value: 173), // soft hyphen = discretionary hyphen, U+00AD ISOnum
    (Name: 'reg'; Value: 174), // registered sign = registered trade mark sign, U+00AE ISOnum
    (Name: 'macr'; Value: 175), // macron = spacing macron = overline = APL overbar, U+00AF ISOdia
    (Name: 'deg'; Value: 176), // degree sign, U+00B0 ISOnum
    (Name: 'plusmn'; Value: 177), // plus-minus sign = plus-or-minus sign, U+00B1 ISOnum
    (Name: 'sup2'; Value: 178), // superscript two = superscript digit two = squared, U+00B2 ISOnum
    (Name: 'sup3'; Value: 179), // superscript three = superscript digit three = cubed, U+00B3 ISOnum
    (Name: 'acute'; Value: 180), // acute accent = spacing acute, U+00B4 ISOdia
    (Name: 'micro'; Value: 181), // micro sign, U+00B5 ISOnum
    (Name: 'para'; Value: 182), // pilcrow sign = paragraph sign, U+00B6 ISOnum
    (Name: 'middot'; Value: 183), // middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum
    (Name: 'cedil'; Value: 184), // cedilla = spacing cedilla, U+00B8 ISOdia
    (Name: 'sup1'; Value: 185), // superscript one = superscript digit one, U+00B9 ISOnum
    (Name: 'ordm'; Value: 186), // masculine ordinal indicator, U+00BA ISOnum
    (Name: 'raquo'; Value: 187), // right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum
    (Name: 'frac14'; Value: 188), // vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum
    (Name: 'frac12'; Value: 189), // vulgar fraction one half = fraction one half, U+00BD ISOnum
    (Name: 'frac34'; Value: 190), // vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum
    (Name: 'iquest'; Value: 191), // inverted question mark = turned question mark, U+00BF ISOnum
    (Name: 'Agrave'; Value: 192), // latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1
    (Name: 'Aacute'; Value: 193), // latin capital letter A with acute, U+00C1 ISOlat1
    (Name: 'Acirc'; Value: 194), // latin capital letter A with circumflex, U+00C2 ISOlat1
    (Name: 'Atilde'; Value: 195), // latin capital letter A with tilde, U+00C3 ISOlat1
    (Name: 'Auml'; Value: 196), // latin capital letter A with diaeresis, U+00C4 ISOlat1
    (Name: 'Aring'; Value: 197), // latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1
    (Name: 'AElig'; Value: 198), // latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1
    (Name: 'Ccedil'; Value: 199), // latin capital letter C with cedilla, U+00C7 ISOlat1
    (Name: 'Egrave'; Value: 200), // latin capital letter E with grave, U+00C8 ISOlat1
    (Name: 'Eacute'; Value: 201), // latin capital letter E with acute, U+00C9 ISOlat1
    (Name: 'Ecirc'; Value: 202), // latin capital letter E with circumflex, U+00CA ISOlat1
    (Name: 'Euml'; Value: 203), // latin capital letter E with diaeresis, U+00CB ISOlat1
    (Name: 'Igrave'; Value: 204), // latin capital letter I with grave, U+00CC ISOlat1
    (Name: 'Iacute'; Value: 205), // latin capital letter I with acute, U+00CD ISOlat1
    (Name: 'Icirc'; Value: 206), // latin capital letter I with circumflex, U+00CE ISOlat1
    (Name: 'Iuml'; Value: 207), // latin capital letter I with diaeresis, U+00CF ISOlat1
    (Name: 'ETH'; Value: 208), // latin capital letter ETH, U+00D0 ISOlat1
    (Name: 'Ntilde'; Value: 209), // latin capital letter N with tilde, U+00D1 ISOlat1
    (Name: 'Ograve'; Value: 210), // latin capital letter O with grave, U+00D2 ISOlat1
    (Name: 'Oacute'; Value: 211), // latin capital letter O with acute, U+00D3 ISOlat1
    (Name: 'Ocirc'; Value: 212), // latin capital letter O with circumflex, U+00D4 ISOlat1
    (Name: 'Otilde'; Value: 213), // latin capital letter O with tilde, U+00D5 ISOlat1
    (Name: 'Ouml'; Value: 214), // latin capital letter O with diaeresis, U+00D6 ISOlat1
    (Name: 'times'; Value: 215), // multiplication sign, U+00D7 ISOnum
    (Name: 'Oslash'; Value: 216), // latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1
    (Name: 'Ugrave'; Value: 217), // latin capital letter U with grave, U+00D9 ISOlat1
    (Name: 'Uacute'; Value: 218), // latin capital letter U with acute, U+00DA ISOlat1
    (Name: 'Ucirc'; Value: 219), // latin capital letter U with circumflex, U+00DB ISOlat1
    (Name: 'Uuml'; Value: 220), // latin capital letter U with diaeresis, U+00DC ISOlat1
    (Name: 'Yacute'; Value: 221), // latin capital letter Y with acute, U+00DD ISOlat1
    (Name: 'THORN'; Value: 222), // latin capital letter THORN, U+00DE ISOlat1
    (Name: 'szlig'; Value: 223), // latin small letter sharp s = ess-zed, U+00DF ISOlat1
    (Name: 'agrave'; Value: 224), // latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1
    (Name: 'aacute'; Value: 225), // latin small letter a with acute, U+00E1 ISOlat1
    (Name: 'acirc'; Value: 226), // latin small letter a with circumflex, U+00E2 ISOlat1
    (Name: 'atilde'; Value: 227), // latin small letter a with tilde, U+00E3 ISOlat1
    (Name: 'auml'; Value: 228), // latin small letter a with diaeresis, U+00E4 ISOlat1
    (Name: 'aring'; Value: 229), // latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1
    (Name: 'aelig'; Value: 230), // latin small letter ae = latin small ligature ae, U+00E6 ISOlat1
    (Name: 'ccedil'; Value: 231), // latin small letter c with cedilla, U+00E7 ISOlat1
    (Name: 'egrave'; Value: 232), // latin small letter e with grave, U+00E8 ISOlat1
    (Name: 'eacute'; Value: 233), // latin small letter e with acute, U+00E9 ISOlat1
    (Name: 'ecirc'; Value: 234), // latin small letter e with circumflex, U+00EA ISOlat1
    (Name: 'euml'; Value: 235), // latin small letter e with diaeresis, U+00EB ISOlat1
    (Name: 'igrave'; Value: 236), // latin small letter i with grave, U+00EC ISOlat1
    (Name: 'iacute'; Value: 237), // latin small letter i with acute, U+00ED ISOlat1
    (Name: 'icirc'; Value: 238), // latin small letter i with circumflex, U+00EE ISOlat1
    (Name: 'iuml'; Value: 239), // latin small letter i with diaeresis, U+00EF ISOlat1
    (Name: 'eth'; Value: 240), // latin small letter eth, U+00F0 ISOlat1
    (Name: 'ntilde'; Value: 241), // latin small letter n with tilde, U+00F1 ISOlat1
    (Name: 'ograve'; Value: 242), // latin small letter o with grave, U+00F2 ISOlat1
    (Name: 'oacute'; Value: 243), // latin small letter o with acute, U+00F3 ISOlat1
    (Name: 'ocirc'; Value: 244), // latin small letter o with circumflex, U+00F4 ISOlat1
    (Name: 'otilde'; Value: 245), // latin small letter o with tilde, U+00F5 ISOlat1
    (Name: 'ouml'; Value: 246), // latin small letter o with diaeresis, U+00F6 ISOlat1
    (Name: 'divide'; Value: 247), // division sign, U+00F7 ISOnum
    (Name: 'oslash'; Value: 248), // latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1
    (Name: 'ugrave'; Value: 249), // latin small letter u with grave, U+00F9 ISOlat1
    (Name: 'uacute'; Value: 250), // latin small letter u with acute, U+00FA ISOlat1
    (Name: 'ucirc'; Value: 251), // latin small letter u with circumflex, U+00FB ISOlat1
    (Name: 'uuml'; Value: 252), // latin small letter u with diaeresis, U+00FC ISOlat1
    (Name: 'yacute'; Value: 253), // latin small letter y with acute, U+00FD ISOlat1
    (Name: 'thorn'; Value: 254), // latin small letter thorn, U+00FE ISOlat1
    (Name: 'yuml'; Value: 255), // latin small letter y with diaeresis, U+00FF ISOlat1

    // symbols, mathematical symbols, and Greek letters
    // Latin Extended-B
    (Name: 'fnof'; Value: 402), // latin small f with hook = function = florin, U+0192 ISOtech

    // Greek
    (Name: 'Alpha'; Value: 913), // greek capital letter alpha, U+0391
    (Name: 'Beta'; Value: 914), // greek capital letter beta, U+0392
    (Name: 'Gamma'; Value: 915), // greek capital letter gamma, U+0393 ISOgrk3
    (Name: 'Delta'; Value: 916), // greek capital letter delta, U+0394 ISOgrk3
    (Name: 'Epsilon'; Value: 917), // greek capital letter epsilon, U+0395
    (Name: 'Zeta'; Value: 918), // greek capital letter zeta, U+0396
    (Name: 'Eta'; Value: 919), // greek capital letter eta, U+0397
    (Name: 'Theta'; Value: 920), // greek capital letter theta, U+0398 ISOgrk3
    (Name: 'Iota'; Value: 921), // greek capital letter iota, U+0399
    (Name: 'Kappa'; Value: 922), // greek capital letter kappa, U+039A
    (Name: 'Lambda'; Value: 923), // greek capital letter lambda, U+039B ISOgrk3
    (Name: 'Mu'; Value: 924), // greek capital letter mu, U+039C
    (Name: 'Nu'; Value: 925), // greek capital letter nu, U+039D
    (Name: 'Xi'; Value: 926), // greek capital letter xi, U+039E ISOgrk3
    (Name: 'Omicron'; Value: 927), // greek capital letter omicron, U+039F
    (Name: 'Pi'; Value: 928), // greek capital letter pi, U+03A0 ISOgrk3
    (Name: 'Rho'; Value: 929), // greek capital letter rho, U+03A1
    (Name: 'Sigma'; Value: 931), // greek capital letter sigma, U+03A3 ISOgrk3,
                                     // there is no Sigmaf, and no U+03A2 character either
    (Name: 'Tau'; Value: 932), // greek capital letter tau, U+03A4
    (Name: 'Upsilon'; Value: 933), // greek capital letter upsilon, U+03A5 ISOgrk3
    (Name: 'Phi'; Value: 934), // greek capital letter phi, U+03A6 ISOgrk3
    (Name: 'Chi'; Value: 935), // greek capital letter chi, U+03A7
    (Name: 'Psi'; Value: 936), // greek capital letter psi, U+03A8 ISOgrk3
    (Name: 'Omega'; Value: 937), // greek capital letter omega, U+03A9 ISOgrk3
    (Name: 'alpha'; Value: 945), // greek small letter alpha, U+03B1 ISOgrk3
    (Name: 'beta'; Value: 946), // greek small letter beta, U+03B2 ISOgrk3
    (Name: 'gamma'; Value: 947), // greek small letter gamma, U+03B3 ISOgrk3
    (Name: 'delta'; Value: 948), // greek small letter delta, U+03B4 ISOgrk3
    (Name: 'epsilon'; Value: 949), // greek small letter epsilon, U+03B5 ISOgrk3
    (Name: 'zeta'; Value: 950), // greek small letter zeta, U+03B6 ISOgrk3
    (Name: 'eta'; Value: 951), // greek small letter eta, U+03B7 ISOgrk3
    (Name: 'theta'; Value: 952), // greek small letter theta, U+03B8 ISOgrk3
    (Name: 'iota'; Value: 953), // greek small letter iota, U+03B9 ISOgrk3
    (Name: 'kappa'; Value: 954), // greek small letter kappa, U+03BA ISOgrk3
    (Name: 'lambda'; Value: 955), // greek small letter lambda, U+03BB ISOgrk3
    (Name: 'mu'; Value: 956), // greek small letter mu, U+03BC ISOgrk3
    (Name: 'nu'; Value: 957), // greek small letter nu, U+03BD ISOgrk3
    (Name: 'xi'; Value: 958), // greek small letter xi, U+03BE ISOgrk3
    (Name: 'omicron'; Value: 959), // greek small letter omicron, U+03BF NEW
    (Name: 'pi'; Value: 960), // greek small letter pi, U+03C0 ISOgrk3
    (Name: 'rho'; Value: 961), // greek small letter rho, U+03C1 ISOgrk3
    (Name: 'sigmaf'; Value: 962), // greek small letter final sigma, U+03C2 ISOgrk3
    (Name: 'sigma'; Value: 963), // greek small letter sigma, U+03C3 ISOgrk3
    (Name: 'tau'; Value: 964), // greek small letter tau, U+03C4 ISOgrk3
    (Name: 'upsilon'; Value: 965), // greek small letter upsilon, U+03C5 ISOgrk3
    (Name: 'phi'; Value: 966), // greek small letter phi, U+03C6 ISOgrk3
    (Name: 'chi'; Value: 967), // greek small letter chi, U+03C7 ISOgrk3
    (Name: 'psi'; Value: 968), // greek small letter psi, U+03C8 ISOgrk3
    (Name: 'omega'; Value: 969), // greek small letter omega, U+03C9 ISOgrk3
    (Name: 'thetasym'; Value: 977), // greek small letter theta symbol, U+03D1 NEW
    (Name: 'upsih'; Value: 978), // greek upsilon with hook symbol, U+03D2 NEW
    (Name: 'piv'; Value: 982), // greek pi symbol, U+03D6 ISOgrk3
   // General Punctuation
    (Name: 'apos'; Value: 8217), // curly apostrophe,
    (Name: 'bull'; Value: 8226), // bullet = black small circle, U+2022 ISOpub,
                                      // bullet is NOT the same as bullet operator, U+2219
    (Name: 'hellip'; Value: 8230), // horizontal ellipsis = three dot leader, U+2026 ISOpub
    (Name: 'prime'; Value: 8242), // prime = minutes = feet, U+2032 ISOtech
    (Name: 'Prime'; Value: 8243), // double prime = seconds = inches, U+2033 ISOtech
    (Name: 'oline'; Value: 8254), // overline = spacing overscore, U+203E NEW
    (Name: 'frasl'; Value: 8260), // fraction slash, U+2044 NEW
    // Letterlike Symbols
    (Name: 'weierp'; Value: 8472), // script capital P = power set = Weierstrass p, U+2118 ISOamso
    (Name: 'image'; Value: 8465), // blackletter capital I = imaginary part, U+2111 ISOamso
    (Name: 'real'; Value: 8476), // blackletter capital R = real part symbol, U+211C ISOamso
    (Name: 'trade'; Value: 8482), // trade mark sign, U+2122 ISOnum
    (Name: 'alefsym'; Value: 8501), // alef symbol = first transfinite cardinal, U+2135 NEW
                                      // alef symbol is NOT the same as hebrew letter alef, U+05D0 although the same
                                      // glyph could be used to depict both characters
    // Arrows
    (Name: 'larr'; Value: 8592), // leftwards arrow, U+2190 ISOnum
    (Name: 'uarr'; Value: 8593), // upwards arrow, U+2191 ISOnu
    (Name: 'rarr'; Value: 8594), // rightwards arrow, U+2192 ISOnum
    (Name: 'darr'; Value: 8595), // downwards arrow, U+2193 ISOnum
    (Name: 'harr'; Value: 8596), // left right arrow, U+2194 ISOamsa
    (Name: 'crarr'; Value: 8629), // downwards arrow with corner leftwards = carriage return, U+21B5 NEW
    (Name: 'lArr'; Value: 8656), // leftwards double arrow, U+21D0 ISOtech
                                      // ISO 10646 does not say that lArr is the same as the 'is implied by' arrow but
                                      // also does not have any other charater for that function. So ? lArr can be used
                                      // for 'is implied by' as ISOtech sugg
    (Name: 'uArr'; Value: 8657), // upwards double arrow, U+21D1 ISOamsa
    (Name: 'rArr'; Value: 8658), // rightwards double arrow, U+21D2 ISOtech
                                      // ISO 10646 does not say this is the 'implies' character but does not have another
                                      // character with this function so ? rArr can be used for 'implies' as ISOtech suggests
    (Name: 'dArr'; Value: 8659), // downwards double arrow, U+21D3 ISOamsa
    (Name: 'hArr'; Value: 8660), // left right double arrow, U+21D4 ISOamsa
    // Mathematical Operators
    (Name: 'forall'; Value: 8704), // for all, U+2200 ISOtech
    (Name: 'part'; Value: 8706), // partial differential, U+2202 ISOtech
    (Name: 'exist'; Value: 8707), // there exists, U+2203 ISOtech
    (Name: 'empty'; Value: 8709), // empty set = null set = diameter, U+2205 ISOamso
    (Name: 'nabla'; Value: 8711), // nabla = backward difference, U+2207 ISOtech
    (Name: 'isin'; Value: 8712), // element of, U+2208 ISOtech
    (Name: 'notin'; Value: 8713), // not an element of, U+2209 ISOtech
    (Name: 'ni'; Value: 8715), // contains as member, U+220B ISOtech
    (Name: 'prod'; Value: 8719), // n-ary product = product sign, U+220F ISOamsb
                                      // prod is NOT the same character as U+03A0 'greek capital letter pi' though the
                                      // same glyph might be used for both
    (Name: 'sum'; Value: 8721), // n-ary sumation, U+2211 ISOamsb
                                      //  sum is NOT the same character as U+03A3 'greek capital letter sigma' though the
                                      // same glyph might be used for both
    (Name: 'minus'; Value: 8722), // minus sign, U+2212 ISOtech
    (Name: 'lowast'; Value: 8727), // asterisk operator, U+2217 ISOtech
    (Name: 'radic'; Value: 8730), // square root = radical sign, U+221A ISOtech
    (Name: 'prop'; Value: 8733), // proportional to, U+221D ISOtech
    (Name: 'infin'; Value: 8734), // infinity, U+221E ISOtech
    (Name: 'ang'; Value: 8736), // angle, U+2220 ISOamso
    (Name: 'and'; Value: 8743), // logical and = wedge, U+2227 ISOtech
    (Name: 'or'; Value: 8744), // logical or = vee, U+2228 ISOtech
    (Name: 'cap'; Value: 8745), // intersection = cap, U+2229 ISOtech
    (Name: 'cup'; Value: 8746), // union = cup, U+222A ISOtech
    (Name: 'int'; Value: 8747), // integral, U+222B ISOtech
    (Name: 'there4'; Value: 8756), // therefore, U+2234 ISOtech
    (Name: 'sim'; Value: 8764), // tilde operator = varies with = similar to, U+223C ISOtech
                                      // tilde operator is NOT the same character as the tilde, U+007E, although the same
                                      // glyph might be used to represent both
    (Name: 'cong'; Value: 8773), // approximately equal to, U+2245 ISOtech
    (Name: 'asymp'; Value: 8776), // almost equal to = asymptotic to, U+2248 ISOamsr
    (Name: 'ne'; Value: 8800), // not equal to, U+2260 ISOtech
    (Name: 'equiv'; Value: 8801), // identical to, U+2261 ISOtech
    (Name: 'le'; Value: 8804), // less-than or equal to, U+2264 ISOtech
    (Name: 'ge'; Value: 8805), // greater-than or equal to, U+2265 ISOtech
    (Name: 'sub'; Value: 8834), // subset of, U+2282 ISOtech
    (Name: 'sup'; Value: 8835), // superset of, U+2283 ISOtech
                                      // note that nsup, 'not a superset of, U+2283' is not covered by the Symbol font
                                      // encoding and is not included.
    (Name: 'nsub'; Value: 8836), // not a subset of, U+2284 ISOamsn
    (Name: 'sube'; Value: 8838), // subset of or equal to, U+2286 ISOtech
    (Name: 'supe'; Value: 8839), // superset of or equal to, U+2287 ISOtech
    (Name: 'oplus'; Value: 8853), // circled plus = direct sum, U+2295 ISOamsb
    (Name: 'otimes'; Value: 8855), // circled times = vector product, U+2297 ISOamsb
    (Name: 'perp'; Value: 8869), // up tack = orthogonal to = perpendicular, U+22A5 ISOtech
    (Name: 'sdot'; Value: 8901), // dot operator, U+22C5 ISOamsb
                                      // dot operator is NOT the same character as U+00B7 middle dot
    // Miscellaneous Technical
    (Name: 'lceil'; Value: 8968), // left ceiling = apl upstile, U+2308 ISOamsc
    (Name: 'rceil'; Value: 8969), // right ceiling, U+2309 ISOamsc
    (Name: 'lfloor'; Value: 8970), // left floor = apl downstile, U+230A ISOamsc
    (Name: 'rfloor'; Value: 8971), // right floor, U+230B ISOamsc
    (Name: 'lang'; Value: 9001), // left-pointing angle bracket = bra, U+2329 ISOtech
                                      // lang is NOT the same character as U+003C 'less than' or U+2039 'single
                                      // left-pointing angle quotation mark'
    (Name: 'rang'; Value: 9002), // right-pointing angle bracket = ket, U+232A ISOtech
                                      // rang is NOT the same character as U+003E 'greater than' or U+203A 'single
                                      // right-pointing angle quotation mark'
    // Geometric Shapes
    (Name: 'loz'; Value: 9674), // lozenge, U+25CA ISOpub
    // Miscellaneous Symbols
    (Name: 'spades'; Value: 9824), // black spade suit, U+2660 ISOpub
                                     // black here seems to mean filled as opposed to hollow
    (Name: 'clubs'; Value: 9827), // black club suit = shamrock, U+2663 ISOpub
    (Name: 'hearts'; Value: 9829), // black heart suit = valentine, U+2665 ISOpub
    (Name: 'diams'; Value: 9830), // black diamond suit, U+2666 ISOpub

    // markup-significant and internationalization characters
    // C0 Controls and Basic Latin
    (Name: 'quot'; Value: 34), // quotation mark = APL quote, U+0022 ISOnum
    (Name: 'amp'; Value: 38), // ampersand, U+0026 ISOnum
    (Name: 'lt'; Value: 60), // less-than sign, U+003C ISOnum
    (Name: 'gt'; Value: 62), // greater-than sign, U+003E ISOnum
    // Latin Extended-A
    (Name: 'OElig'; Value: 338), // latin capital ligature OE, U+0152 ISOlat2
    (Name: 'oelig'; Value: 339), // latin small ligature oe, U+0153 ISOlat2
                                     // ligature is a misnomer, this is a separate character in some languages
    (Name: 'Scaron'; Value: 352), // latin capital letter S with caron, U+0160 ISOlat2
    (Name: 'scaron'; Value: 353), // latin small letter s with caron, U+0161 ISOlat2
    (Name: 'Yuml'; Value: 376), // latin capital letter Y with diaeresis, U+0178 ISOlat2
    // Spacing Modifier Letters
    (Name: 'circ'; Value: 710), // modifier letter circumflex accent, U+02C6 ISOpub
    (Name: 'tilde'; Value: 732), // small tilde, U+02DC ISOdia
    // General Punctuation
    (Name: 'ensp'; Value: 8194), // en space, U+2002 ISOpub
    (Name: 'emsp'; Value: 8195), // em space, U+2003 ISOpub
    (Name: 'thinsp'; Value: 8201), // thin space, U+2009 ISOpub
    (Name: 'zwnj'; Value: 8204), // zero width non-joiner, U+200C NEW RFC 2070
    (Name: 'zwj'; Value: 8205), // zero width joiner, U+200D NEW RFC 2070
    (Name: 'lrm'; Value: 8206), // left-to-right mark, U+200E NEW RFC 2070
    (Name: 'rlm'; Value: 8207), // right-to-left mark, U+200F NEW RFC 2070
    (Name: 'ndash'; Value: 8211), // en dash, U+2013 ISOpub
    (Name: 'mdash'; Value: 8212), // em dash, U+2014 ISOpub
    (Name: 'lsquo'; Value: 8216), // left single quotation mark, U+2018 ISOnum
    (Name: 'rsquo'; Value: 8217), // right single quotation mark, U+2019 ISOnum
    (Name: 'sbquo'; Value: 8218), // single low-9 quotation mark, U+201A NEW
    (Name: 'ldquo'; Value: 8220), // left double quotation mark, U+201C ISOnum
    (Name: 'rdquo'; Value: 8221), // right double quotation mark, U+201D ISOnum
    (Name: 'bdquo'; Value: 8222), // double low-9 quotation mark, U+201E NEW
    (Name: 'dagger'; Value: 8224), // dagger, U+2020 ISOpub
    (Name: 'Dagger'; Value: 8225), // double dagger, U+2021 ISOpub
    (Name: 'permil'; Value: 8240), // per mille sign, U+2030 ISOtech
    (Name: 'lsaquo'; Value: 8249), // single left-pointing angle quotation mark, U+2039 ISO proposed
                                     // lsaquo is proposed but not yet ISO standardized
    (Name: 'rsaquo'; Value: 8250), // single right-pointing angle quotation mark, U+203A ISO proposed
                                     // rsaquo is proposed but not yet ISO standardized
    (Name: 'euro'; Value: 8364) //  euro sign, U+20AC NEW
    );

procedure InitEntities;
var
  I: Integer;
begin
  // Put the Entities into a sorted StringList for faster access.
  if Entities = nil then
  begin
    Entities := ThtStringList.Create;
    Entities.CaseSensitive := True;
    for I := low(EntityDefinitions) to high(EntityDefinitions) do
      Entities.AddObject(EntityDefinitions[I].Name, @EntityDefinitions[I]);
    Entities.Sort;
  end;
end;


const
  ResWordDefinitions: array[1..90] of TResWord = (
    (Name: 'HTML';        Symbol: HtmlSy;       EndSym: HtmlEndSy),
    (Name: 'TITLE';       Symbol: TitleSy;      EndSym: TitleEndSy),
    (Name: 'BODY';        Symbol: BodySy;       EndSym: BodyEndSy),
    (Name: 'HEAD';        Symbol: HeadSy;       EndSym: HeadEndSy),
    (Name: 'B';           Symbol: BSy;          EndSym: BEndSy),
    (Name: 'I';           Symbol: ISy;          EndSym: IEndSy),
    (Name: 'H';           Symbol: HeadingSy;    EndSym: HeadingEndSy),
    (Name: 'EM';          Symbol: EmSy;         EndSym: EmEndSy),
    (Name: 'STRONG';      Symbol: StrongSy;     EndSym: StrongEndSy),
    (Name: 'U';           Symbol: USy;          EndSym: UEndSy),
    (Name: 'CITE';        Symbol: CiteSy;       EndSym: CiteEndSy),
    (Name: 'VAR';         Symbol: VarSy;        EndSym: VarEndSy),
    (Name: 'TT';          Symbol: TTSy;         EndSym: TTEndSy),
    (Name: 'CODE';        Symbol: CodeSy;       EndSym: CodeEndSy),
    (Name: 'KBD';         Symbol: KbdSy;        EndSym: KbdEndSy),
    (Name: 'SAMP';        Symbol: SampSy;       EndSym: SampEndSy),
    (Name: 'OL';          Symbol: OLSy;         EndSym: OLEndSy),
    (Name: 'UL';          Symbol: ULSy;         EndSym: ULEndSy),
    (Name: 'DIR';         Symbol: DirSy;        EndSym: DirEndSy),
    (Name: 'MENU';        Symbol: MenuSy;       EndSym: MenuEndSy),
    (Name: 'DL';          Symbol: DLSy;         EndSym: DLEndSy),
    (Name: 'A';           Symbol: ASy;          EndSym: AEndSy),
    (Name: 'ADDRESS';     Symbol: AddressSy;    EndSym: AddressEndSy),
    (Name: 'BLOCKQUOTE';  Symbol: BlockQuoteSy; EndSym: BlockQuoteEndSy),
    (Name: 'PRE';         Symbol: PreSy;        EndSym: PreEndSy),
    (Name: 'CENTER';      Symbol: CenterSy;     EndSym: CenterEndSy),
    (Name: 'TABLE';       Symbol: TableSy;      EndSym: TableEndSy),
    (Name: 'TD';          Symbol: TDsy;         EndSym: TDEndSy),
    (Name: 'TH';          Symbol: THSy;         EndSym: THEndSy),
    (Name: 'CAPTION';     Symbol: CaptionSy;    EndSym: CaptionEndSy),
    (Name: 'FORM';        Symbol: FormSy;       EndSym: FormEndSy),
    (Name: 'TEXTAREA';    Symbol: TextAreaSy;   EndSym: TextAreaEndSy),
    (Name: 'SELECT';      Symbol: SelectSy;     EndSym: SelectEndSy),
    (Name: 'OPTION';      Symbol: OptionSy;     EndSym: OptionEndSy),
    (Name: 'FONT';        Symbol: FontSy;       EndSym: FontEndSy),
    (Name: 'SUB';         Symbol: SubSy;        EndSym: SubEndSy),
    (Name: 'SUP';         Symbol: SupSy;        EndSym: SupEndSy),
    (Name: 'BIG';         Symbol: BigSy;        EndSym: BigEndSy),
    (Name: 'SMALL';       Symbol: SmallSy;      EndSym: SmallEndSy),
    (Name: 'P';           Symbol: PSy;          EndSym: PEndSy),
    (Name: 'MAP';         Symbol: MapSy;        EndSym: MapEndSy),
    (Name: 'FRAMESET';    Symbol: FrameSetSy;   EndSym: FrameSetEndSy),
    (Name: 'NOFRAMES';    Symbol: NoFramesSy;   EndSym: NoFramesEndSy),
    (Name: 'SCRIPT';      Symbol: ScriptSy;     EndSym: ScriptEndSy),
    (Name: 'DIV';         Symbol: DivSy;        EndSym: DivEndSy),
    (Name: 'S';           Symbol: SSy;          EndSym: SEndSy),
    (Name: 'STRIKE';      Symbol: StrikeSy;     EndSym: StrikeEndSy),
    (Name: 'TR';          Symbol: TRSy;         EndSym: TREndSy),
    (Name: 'NOBR';        Symbol: NoBrSy;       EndSym: NoBrEndSy),
    (Name: 'STYLE';       Symbol: StyleSy;      EndSym: StyleEndSy),
    (Name: 'SPAN';        Symbol: SpanSy;       EndSym: SpanEndSy),
    (Name: 'COLGROUP';    Symbol: ColGroupSy;   EndSym: ColGroupEndSy),
    (Name: 'LABEL';       Symbol: LabelSy;      EndSym: LabelEndSy),
    (Name: 'THEAD';       Symbol: THeadSy;      EndSym: THeadEndSy),
    (Name: 'TBODY';       Symbol: TBodySy;      EndSym: TBodyEndSy),
    (Name: 'TFOOT';       Symbol: TFootSy;      EndSym: TFootEndSy),
    (Name: 'OBJECT';      Symbol: ObjectSy;     EndSym: ObjectEndSy),
    (Name: 'DD';          Symbol: DDSy;         EndSym: DDEndSy),
    (Name: 'DT';          Symbol: DTSy;         EndSym: DTEndSy),
    (Name: 'LI';          Symbol: LISy;         EndSym: LIEndSy),
    (Name: 'FIELDSET';    Symbol: FieldsetSy;   EndSym: FieldsetEndSy),
    (Name: 'LEGEND';      Symbol: LegendSy;     EndSym: LegendEndSy),
    (Name: 'BR';          Symbol: BRSy;         EndSym: CommandSy),
    (Name: 'HR';          Symbol: HRSy;         EndSym: CommandSy),
    (Name: 'IMG';         Symbol: ImageSy;      EndSym: CommandSy),
    (Name: 'BASE';        Symbol: BaseSy;       EndSym: CommandSy),
    (Name: 'BUTTON';      Symbol: ButtonSy;     EndSym: CommandSy),
    (Name: 'INPUT';       Symbol: InputSy;      EndSym: CommandSy),
    (Name: 'SELECTED';    Symbol: SelectedSy;   EndSym: CommandSy),
    (Name: 'BASEFONT';    Symbol: BaseFontSy;   EndSym: CommandSy),
    (Name: 'AREA';        Symbol: AreaSy;       EndSym: CommandSy),
    (Name: 'FRAME';       Symbol: FrameSy;      EndSym: CommandSy),
    (Name: 'PAGE';        Symbol: PageSy;       EndSym: CommandSy),
    (Name: 'BGSOUND';     Symbol: BgSoundSy;    EndSym: CommandSy),
    (Name: 'WRAP';        Symbol: WrapSy;       EndSym: CommandSy),
    (Name: 'META';        Symbol: MetaSy;       EndSym: CommandSy),
    (Name: 'PANEL';       Symbol: PanelSy;      EndSym: CommandSy),
    (Name: 'WBR';         Symbol: WbrSy;        EndSym: CommandSy),
    (Name: 'LINK';        Symbol: LinkSy;       EndSym: CommandSy),
    (Name: 'COL';         Symbol: ColSy;        EndSym: CommandSy),
    (Name: 'PARAM';       Symbol: ParamSy;      EndSym: CommandSy),
    (Name: 'READONLY';    Symbol: ReadonlySy;   EndSym: CommandSy),
    {HTML5 }
    (Name: 'HEADER';      Symbol: HeaderSy;     EndSym: HeaderEndSy),
    (Name: 'SECTION';     Symbol: SectionSy;    EndSym: SectionEndSy),
    (Name: 'NAV';         Symbol: NavSy;        EndSym: NavEndSy),
    (Name: 'ARTICLE';     Symbol: ArticleSy;    EndSym: ArticleEndSy),
    (Name: 'ASIDE';       Symbol: AsideSy;      EndSym: AsideEndSy),
    (Name: 'FOOTER';      Symbol: FooterSy;     EndSym: FooterEndSy),
    (Name: 'HGROUP';      Symbol: HGroupSy;     EndSym: HGroupEndSy),
    (Name: 'MARK';        Symbol: MarkSy;       EndSym: MarkEndSy));

procedure SetSymbolName(Sy: Symb; Name: ThtString);
begin
  Name := htLowerCase(Name);
  if SymbolNames[Sy] <> '' then
    assert(SymbolNames[Sy] = Name, 'Different names for same symbol!');
  SymbolNames[Sy] := Name;
end;

procedure InitReservedWords;
var
  I: Integer;
  P: PResWord;
  S: Symb;
begin
  // Put the Attributes into a sorted StringList for faster access.
  if ReservedWords = nil then
  begin
    ReservedWords := ThtStringList.Create;
    ReservedWords.CaseSensitive := True;
    for I := low(ResWordDefinitions) to high(ResWordDefinitions) do
      ReservedWords.AddObject(ResWordDefinitions[I].Name, @ResWordDefinitions[I]);
    ReservedWords.Sort;

    // initialize ReservedWordsIndex and SymbolNames
    for S := low(Symb) to high(Symb) do
      ReservedWordsIndex[S] := -1;
    for I := 0 to ReservedWords.Count - 1 do
    begin
      P := PResWord(ReservedWords.Objects[I]);
      ReservedWordsIndex[P.Symbol] := I;
      SetSymbolName(P.Symbol, P.Name);
      if P.EndSym <> CommandSy then
      begin
        ReservedWordsIndex[P.EndSym] := I;
        SetSymbolName(P.EndSym, P.Name);
      end;
    end;
  end;
end;

const
  AttribDefinitions: array[1..85] of TSymbolRec = (
    (Name: 'HREF';              Value: HrefSy),
    (Name: 'NAME';              Value: NameSy),
    (Name: 'SRC';               Value: SrcSy),
    (Name: 'ALT';               Value: AltSy),
    (Name: 'ALIGN';             Value: AlignSy),
    (Name: 'TEXT';              Value: TextSy),
    (Name: 'BGCOLOR';           Value: BGColorSy),
    (Name: 'LINK';              Value: LinkSy),
    (Name: 'BACKGROUND';        Value: BackgroundSy),
    (Name: 'COLSPAN';           Value: ColSpanSy),
    (Name: 'ROWSPAN';           Value: RowSpanSy),
    (Name: 'BORDER';            Value: BorderSy),
    (Name: 'CELLPADDING';       Value: CellPaddingSy),
    (Name: 'CELLSPACING';       Value: CellSpacingSy),
    (Name: 'VALIGN';            Value: VAlignSy),
    (Name: 'WIDTH';             Value: WidthSy),
    (Name: 'START';             Value: StartSy),
    (Name: 'VALUE';             Value: ValueSy),
    (Name: 'TYPE';              Value: TypeSy),
    (Name: 'CHECKBOX';          Value: CheckBoxSy),
    (Name: 'RADIO';             Value: RadioSy),
    (Name: 'METHOD';            Value: MethodSy),
    (Name: 'ACTION';            Value: ActionSy),
    (Name: 'CHECKED';           Value: CheckedSy),
    (Name: 'SIZE';              Value: SizeSy),
    (Name: 'MAXLENGTH';         Value: MaxLengthSy),
    (Name: 'COLS';              Value: ColsSy),
    (Name: 'ROWS';              Value: RowsSy),
    (Name: 'MULTIPLE';          Value: MultipleSy),
    (Name: 'VALUE';             Value: ValueSy),
    (Name: 'SELECTED';          Value: SelectedSy),
    (Name: 'FACE';              Value: FaceSy),
    (Name: 'COLOR';             Value: ColorSy),
    (Name: 'TRANSP';            Value: TranspSy),
    (Name: 'CLEAR';             Value: ClearSy),
    (Name: 'ISMAP';             Value: IsMapSy),
    (Name: 'BORDERCOLOR';       Value: BorderColorSy),
    (Name: 'USEMAP';            Value: UseMapSy),
    (Name: 'SHAPE';             Value: ShapeSy),
    (Name: 'COORDS';            Value: CoordsSy),
    (Name: 'NOHREF';            Value: NoHrefSy),
    (Name: 'HEIGHT';            Value: HeightSy),
    (Name: 'PLAIN';             Value: PlainSy),
    (Name: 'TARGET';            Value: TargetSy),
    (Name: 'NORESIZE';          Value: NoResizeSy),
    (Name: 'SCROLLING';         Value: ScrollingSy),
    (Name: 'HSPACE';            Value: HSpaceSy),
    (Name: 'LANGUAGE';          Value: LanguageSy),
    (Name: 'FRAMEBORDER';       Value: FrameBorderSy),
    (Name: 'MARGINWIDTH';       Value: MarginWidthSy),
    (Name: 'MARGINHEIGHT';      Value: MarginHeightSy),
    (Name: 'LOOP';              Value: LoopSy),
    (Name: 'ONCLICK';           Value: OnClickSy),
    (Name: 'WRAP';              Value: WrapSy),
    (Name: 'NOSHADE';           Value: NoShadeSy),
    (Name: 'HTTP-EQUIV';        Value: HttpEqSy),
    (Name: 'CONTENT';           Value: ContentSy),
    (Name: 'ENCTYPE';           Value: EncTypeSy),
    (Name: 'VLINK';             Value: VLinkSy),
    (Name: 'OLINK';             Value: OLinkSy),
    (Name: 'ACTIVE';            Value: ActiveSy),
    (Name: 'VSPACE';            Value: VSpaceSy),
    (Name: 'CLASS';             Value: ClassSy),
    (Name: 'ID';                Value: IDSy),
    (Name: 'STYLE';             Value: StyleSy),
    (Name: 'REL';               Value: RelSy),
    (Name: 'REV';               Value: RevSy),
    (Name: 'NOWRAP';            Value: NoWrapSy),
    (Name: 'BORDERCOLORLIGHT';  Value: BorderColorLightSy),
    (Name: 'BORDERCOLORDARK';   Value: BorderColorDarkSy),
    (Name: 'CHARSET';           Value: CharSetSy),
    (Name: 'RATIO';             Value: RatioSy),
    (Name: 'TITLE';             Value: TitleSy),
    (Name: 'ONFOCUS';           Value: OnFocusSy),
    (Name: 'ONBLUR';            Value: OnBlurSy),
    (Name: 'ONCHANGE';          Value: OnChangeSy),
    (Name: 'SPAN';              Value: SpanSy),
    (Name: 'TABINDEX';          Value: TabIndexSy),
    (Name: 'BGPROPERTIES';      Value: BGPropertiesSy),
    (Name: 'DISABLED';          Value: DisabledSy),
    (Name: 'TOPMARGIN';         Value: TopMarginSy),
    (Name: 'LEFTMARGIN';        Value: LeftMarginSy),
    (Name: 'LABEL';             Value: LabelSy),
    (Name: 'READONLY';          Value: ReadonlySy),
    (Name: 'MEDIA';             Value: MediaSy));

procedure InitAttributes;
var
  I: Integer;
  P: PSymbol;
begin
  // Put the Attributes into a sorted StringList for faster access.
  if AttributeNames = nil then
  begin
    AttributeNames := ThtStringList.Create;
    AttributeNames.CaseSensitive := True;
    for I := low(AttribDefinitions) to high(AttribDefinitions) do
    begin
      P := @AttribDefinitions[I];
      AttributeNames.AddObject(P.Name, Pointer(P));
      SetSymbolName(P.Value, P.Name);
    end;
    AttributeNames.Sort;
  end;
end;

initialization
  InitEntities;
  InitAttributes;
  InitReservedWords;
finalization
  Entities.Free;
  AttributeNames.Free;
  ReservedWords.Free;
end.
