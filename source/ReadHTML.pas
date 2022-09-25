{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2022 by HtmlViewer Team

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
      example, a value of StringSy would indicate a hunk of text, PSy that a <P>
      tag was encountered, etc.
  LCh:
      The next character in the stream to be analyzed.  In mixed case.
  LCToken:
      A ThtString which is associated with the current token.  If Sy is StringSy,
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

{-$define DEBUGGING_PARSER}

{-$define DO_LI_INLINE}
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
{$ifdef DEBUGGING_PARSER}
  CodeSiteLogging,
{$endif}
  HtmlGlobals,
  HtmlBuffer,
  HtmlSymb,
  HTMLUn2,
  Htmlsbs1,
  HTMLSubs,
  StyleTypes,
  StyleUn;

type
  TLoadStyleType = (lsFile, lsString, lsInclude);

  { THtmlParser }

  THtmlParser = class
  private
    TitleStart: Integer;
    TitleEnd: Integer;
    FBase: ThtString;
    FBaseTarget: ThtString;

    LCh: ThtChar;
    LastChar: (lcOther, lcCR, lcLF);
    LCToken: TTokenObj;

    Doc: TBuffer;
    DocStack: TStack;
    CharCount: Integer;

    Sy: TElemSymb;
    IsXhtmlEndSy: Boolean; // current symbol is an xhtml combined start/end tag like <tag [attr="value" ...] />
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
    MatchMediaQuery: ThtMediaQueryEvent;

    FPixelsPerInch: Integer;
    FUseQuirksMode : Boolean;
    FPropStack: THtmlPropStack;
    FNoBreak : Boolean;
    FIsXHTML : Boolean;
    procedure SetNoBreak(const AValue : Boolean);
    procedure GetCh;

    function DoCharSet(const Content: ThtString): Boolean;
    function FindAlignment: ThtString;
    function GetEntityStr(CodePage: Integer): ThtString;
    function GetIdentifier(out Identifier: ThtString): Boolean;
//    function GetValue(var S: ThtString; var Value: Integer): Boolean;
    procedure CheckForAlign;
    procedure DoAEnd;
    procedure DoBase;
    procedure DoBody(const TermSet: TElemSymbSet);
    procedure DoBr(const TermSet: TElemSymbSet);
    procedure DoCommonSy;
    procedure DoDivEtc(Sym: TElemSymb; const TermSet: TElemSymbSet);
    procedure DoFrameSet(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: ThtString);
    procedure DoListItem(
      {$ifdef DO_LI_INLINE}var LiBlock: TBlockLi; var LiSection: TSection;{$endif}
      BlockType, Sym: TElemSymb; LineCount: Integer; Index: ThtChar; Plain: Boolean; const TermSet: TElemSymbSet);
    procedure DoLists(Sym: TElemSymb; const TermSet: TElemSymbSet);
    procedure DoMap;
    procedure DoMeta(Sender: TObject);
    procedure DoObjectTag(var C: ThtChar; var N, IX: Integer);
    procedure DoP(const TermSet: TElemSymbSet);
    procedure DoScript(Ascript: TScriptEvent);
    procedure DoSound;
    procedure DoStyle(var C: ThtChar; Doc: TBuffer; const APath, AMedia: ThtString; FromLink: Boolean);
    procedure DoStyleLink;
    procedure DoTable;
    procedure DoText;
    procedure DoTextArea(TxtArea: TTextAreaFormControlObj);
    procedure DoTitle;
    procedure GetOptions(Select: TOptionsFormControlObj);
    procedure Next;
    procedure ParseInit(ASectionList: ThtDocument; AIncludeEvent: TIncludeType);
    procedure SkipWhiteSpace; {$ifdef UseInline} inline; {$endif}
    procedure PushNewProp(Sy: TElemSymb; Attributes: TAttributeList; const APseudo: ThtString = ''); {$ifdef UseInline} inline; {$endif}
    procedure PopAProp(Sy: TElemSymb); {$ifdef UseInline} inline; {$endif}
    function Peek: ThtChar;
    function GetTitle: ThtString;
    function PropStackIndex: Integer;
    procedure PopProp; 
    function ExtractCharsetFromXMLProlog : Boolean;
    procedure CollectPreText(S: TTokenObj);
    procedure CollectNormalText(S: TTokenObj);
    procedure CollectText(S: TTokenObj; AWhiteSpace : ThtWhiteSpaceStyle);
  public
    constructor Create(Doc: TBuffer);
    destructor Destroy; override;
    function ShouldUseQuirksMode: Boolean;
    function IsFrame(FrameViewer: TFrameViewerBase): Boolean;
    procedure ParseFrame(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: ThtString; AMetaEvent: TMetaType);
    procedure ParseHtml(ASectionList: ThtDocument; AIncludeEvent: TIncludeType; ASoundEvent: TSoundType; AMetaEvent: TMetaType; ALinkEvent: TLinkType; AMatchMediaQuery: ThtMediaQueryEvent);
    procedure ParseText(ASectionList: ThtDocument);
    property Base: ThtString read FBase;
    property BaseTarget: ThtString read FBaseTarget;
    property Title: ThtString read GetTitle;
    property UseQuirksMode : Boolean read FUseQuirksMode;
    property PixelsPerInch: Integer read FPixelsPerInch;
    property PropStack: THtmlPropStack read FPropStack;
    property NoBreak : Boolean read FNoBreak write SetNoBreak;
    property IsXHTML : Boolean read FIsXHTML write FIsXHTML;
  end;

function IsWhiteSpace(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
function ReplaceEntities(const Str: ThtString; CodePage: Integer): ThtString;

implementation

uses
{$ifdef Compiler24_Plus}
  System.Types,
{$endif}
  HtmlView, FramView, StylePars, URLSubs;

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


{ THtmlParser }

constructor THtmlParser.Create(Doc: TBuffer);
begin
  inherited Create;
  LCToken := TTokenObj.Create;
  DocStack := TStack.Create;
  Self.Doc := Doc;
  FIsXHTML := False;
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
{Return next ThtChar in LCh.  Ignore comments}

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

      if not LinkSearch and (PropStack.Document <> nil) then
      begin
        Inc(CharCount);
        if (Result = EofChar) or (CharCount and $FFF = 0) {about every 4000 chars} then
          if Doc.Size > 0 then
            THtmlViewerBase(CallingObject).htProgress((Doc.Position * PropStack.Document.ProgressStart) div Doc.Size);
      end;
    end;

  begin
    LCh := ReadChar;
    case LCh of {skip a LfChar after a CrChar or a CrChar after a LfChar}
      CrChar: if LastChar = lcLF then
          LCh := ReadChar;
      LfChar: if LastChar = lcCR then
          LCh := ReadChar;
    end;
    case LCh of
      TabChar:
        LCh := SpcChar;

      LfChar:
        begin
          LastChar := lcLF;
          LCh := CrChar;
        end;

     CrChar:
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
    while (LCh <> GreaterChar) and (LCh <> EofChar) do
      GetChBasic;
    InComment := False;
  end;

  procedure DoInclude;

    function GetNameValueParameter(out Name, Value: ThtString): Boolean;

      function GetQuotedValue(var S: ThtString): Boolean;
      {get a quoted ThtString but strip the quotes}
      var
        Term: ThtChar;
        SaveSy: TElemSymb;
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
    SaveLCToken: TTokenObj;
    L: Integer;
  begin
    S := '';
    SaveLCToken := LCToken;
    LCToken := TTokenObj.Create;
    try
      GetChBasic;
      GetIdentifier(S);
      if LCh = '>' then
      begin
        L := Length(S);
        if (L > 2) and (S[L-1] = '-') and (S[L] = '-') then
        begin
          Doc.Position := Doc.Position - 3;
          GetChBasic;
          SetLength(S, L - 2);
        end;
      end;
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
    GetChBasic;
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
 {$ifdef UseInline} inline; {$endif}
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

function THtmlParser.ExtractCharsetFromXMLProlog : Boolean;

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

  procedure ScanRestOfUnquotedString(out Identifier : ThtString);
  begin
    SetLength(Identifier, 0);
    if LCh = '"' then begin
      GetChBasic;
    end;
    repeat
      if (LCh = EofChar) then begin
        break;
      end;
      if (LCh = '"') then begin
        break;
      end else begin
        htAppendChr(Identifier, LCh);
      end;
      GetChBasic;
    until False;
  end;

  procedure ParseXMLProlog;
  var LCharset : THtString;
    CP: TBuffCodePage;

  begin
    repeat
      GetChBasic;
      GetIdentifier(LId);
      if LId = 'encoding' then begin
        if LCh='=' then begin
          GetChBasic;
          if LCh='"' then begin
            ScanRestOfUnquotedString(LCharset);
            //Do not use DoCharSet(LCharset) here because the stack might not be set up
            CP := StrToCodePage(LCharset);
            Result := CP <> CP_UNKNOWN;
            if Result then
            begin
              Doc.CodePage := CP;
            end;
            exit;
          end;
        end;
      end else begin
        if LCh='=' then begin
          GetChBasic;
          if LCh='"' then begin
            ScanRestOfUnquotedString(LCharset);
          end;
        end;
      end;
        if LCh='?' then begin
           GetChBasic;
        end;
        if (LCh = GreaterChar) or (LCh = EofChar) then begin
          exit;
        end;
    until False;
  end;

 var
  OldPos: Integer;

begin
  FPropStack := THTMLPropStack.Create;
  try
    OldPos := Doc.Position;
    Result := True;
    repeat
      //loop where parsing code goes
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

            Result := False;
            break;
          end;
        end;
        '?':
        begin
          GetChBasic;
          GetIdentifier(LId);
          if LId = 'xml' then begin
            //scan for charset
            ParseXMLProlog;
            break;
          end;
        end;
        EofChar:
          break;

      end;
      GetIdentifier(LId);
      SkipWhiteSpace;
      LId := htUpperCase(LId);
      if (LId = 'HTML') or (LId = 'HEAD') or (LId = 'BODY') then begin
        break;
      end;
    until False;
    Doc.Position := OldPos;
  finally
    FreeAndNil(FPropStack);
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
<!DOCTYPE html SYSTEM "about:legacy-compat">
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

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">
<!DOCTYPE HTML SYSTEM "http://www.w3.org/TR/REC-html40/strict.dtd">
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

  procedure ScanRestOfUnquotedString(out Identifier : ThtString);
  begin
    SetLength(Identifier, 0);
    if LCh = '"' then begin
      GetChBasic;
    end;
    repeat
      if (LCh = EofChar) then begin
        break;
      end;
      if (LCh = '"') then begin
        break;
      end else begin
        htAppendChr(Identifier, LCh);
      end;
      GetChBasic;
    until False;
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
    if htUpperCase(LPart) = 'HTML' then
    begin
      GetChBasic;
      if LCh = GreaterChar then
      begin
        //HTML5 - don't use quirks mode
        Result := True;
        exit;
      end;
      ScanDTDIdentifier(LPart);
      if (htUpperCase(LPart) <> 'PUBLIC') and (htUpperCase(LPart) <> 'SYSTEM') then
        exit;
      SkipWhiteSpace;
      if LCh = '"' then
        GetChBasic;
      SkipWhiteSpace;
      ScanDTDIdentifier(LPart);
      if (htUpperCase(LPart) = 'ABOUT') then begin
        //HTML5 - don't use quirks mode
        Result := True;
        exit;
      end;
      if (htUpperCase(LPart) = 'HTTP') then begin
      //probably a URL pointing to a DTD
        ScanRestOfUnquotedString(LPart);
        Result := TextEndsWith('html40/strict.dtd',LPart) or
          TextEndsWith('xhtml1-strict.dtd',LPart) or
          TextEndsWith('xhtml-basic11.dtd',LPart) or
          TextEndsWith('xhtml1-transitional.dtd',LPart) or
          TextEndsWith('xhtml1-frameset.dtd',LPart) or
          TextEndsWith('xhtml11.dtd',LPart);
        if Result then
          exit;
      end;
      if (htUpperCase(LPart) <> '-//W3C//DTD') then
        exit;
      SkipWhiteSpace;
      ScanDTDIdentifier(LPart);

      LPart := htUpperCase(LPart);
      if LPart = 'HTML' then
      begin
        SkipWhiteSpace;
        ScanDTDIdentifier(LPart);
        Result := TextStartsWith('4.01',LPart);
        if Result then begin
           exit;
        end;
        Result := TextStartsWith('4.0/',LPart);
        if Result then begin
          exit;
        end;
        Result := TextStartsWith('4.',LPart);
        SkipWhiteSpace;
      end;
      if LPart = 'XHTML' then
      begin
        SkipWhiteSpace;
        ScanDTDIdentifier(LPart);
        if htUpperCase(LPart) = 'BASIC' then
        begin
          SkipWhiteSpace;
          ScanDTDIdentifier(LPart);
          if TextStartsWith('1.',LPart) then
            Result := True;
        end
        else
        begin
          Result := TextStartsWith('1.',LPart);
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
    Result := ReplaceEntities(Doc.GetString(TitleStart, TitleEnd), Doc.CodePage)
  else
    Result := '';
end;

{-----------Next}

procedure THtmlParser.Next;
{Get the next token}

  function GetTag: Boolean;
  {Pick up a Tag or pass a single LessChar}

    function GetAttribute(out Sym: TAttrSymb; out St: ThtString; out S: ThtString; out Val: Integer): Boolean;

      function GetID(out S: ThtString): Boolean;
      begin
        S := '';
        while True do
          case LCh of
            'a'..'z', 'A'..'Z', '-', '$', '0'..'9', ':':
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

      function GetQuotedStr(var S: ThtString; WantCrLf: Boolean; Sym: TAttrSymb): Boolean;
      {get a quoted ThtString but strip the quotes, check to see if it is numerical}
      var
        Term: ThtChar;
        SaveSy: TElemSymb;
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
                  htAppendStr(S, CrLf) //^M^J);
                else
                  htAppendChr(S, SpcChar);
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

      function StrToInteger(const S: ThtString; var Value: Integer): Boolean;
      var
        S1: ThtString;
        I, Code: Integer;
        ValD: Double;
      begin
        Result := False;
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
                  if Code = 0 then
                    if ValD < -MaxInt then
                      Value := -MaxInt
                    else if ValD > MaxInt then
                      Value := MaxInt
                    else
                      Value := Round(ValD);
                  Result := True;
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

      I := -1;
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

      if not StrToInteger(S, Val) then
        case Sym of
          BorderSy:
            if htLowerCase(S) = 'none' then
              Val := 0;
        end;

      if (Sym = IDSy) and (S <> '') and Assigned(PropStack.Document) and not LinkSearch then
        PropStack.Document.AddChPosObjectToIDNameList(S, PropStack.SIndex);
    end;

  var
    //EndTag: Boolean;
    Compare: ThtString;
    SymStr: ThtString;
    AttrStr: ThtString;
    I: Integer;
    L: Integer;
    Save: Integer;
    Sym: TAttrSymb;
  begin
    Result := False;
    Save := PropStack.SIndex;
    TagIndex := PropStack.SIndex;
    GetCh;
    Compare := '';
    case LCh of
      '/':
      begin
        Result := True;
        GetCh;
      end;

      'a'..'z', 'A'..'Z', '?', '!':
      begin
        Result := False;
        SetLength(Compare, Length(Compare) + 1);
        Compare[Length(Compare)] := LCh;
        GetCh;
      end;
    else
      {an odd LessChar}
      Sy := StringSy;
      LCToken.AddUnicodeChar('<', Save);
      Exit;
    end;
    Sy := CommandSy;
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

        'a'..'z', 'A'..'Z', '0'..'9', '_':
        begin
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
      I := -1;
      if ElementNames.Find(htUpperCase(Compare), I) then
        if not Result then
          Sy := PResWord(ElementNames.Objects[I]).Symbol
        else
        begin
          Sy := PResWord(ElementNames.Objects[I]).EndSym;
          if Sy = HtmlSy then
            Sy := CommandSy;
        end;
    end;

    SkipWhiteSpace;

    Attributes.Clear;
    while GetAttribute(Sym, SymStr, AttrStr, L) do
      Attributes.Add(TAttribute.Create(Sym, L, SymStr, AttrStr, PropStack.Last.Codepage));

    while True do
    begin
      case LCh of
        GreaterChar,
        EofChar:
          break;
        '/':
          IsXhtmlEndSy := True;
      end;
      GetCh;
    end;
    if not (Sy in [StyleSy, ScriptSy]) then {in case <!-- comment immediately follows}
      GetCh;
  end;

{$ifdef DEBUGGING_PARSER}
var
  EndTag: Boolean;
{$endif}
begin {already have fresh character loaded here}
  LCToken.Clear;
  IsXhtmlEndSy := False;
{$ifdef DEBUGGING_PARSER}
  EndTag := False;
{$endif}
  case LCh of
    '<':
{$ifdef DEBUGGING_PARSER}
      EndTag :=
{$endif}
      GetTag;

    #1..#8:
      begin
        Sy := StringSy;
        LCh := '?';
      end;

    EofChar:
      Sy := EofSy;
  else
    Sy := StringSy;
    CollectNormalText(LCToken);
  end;

{$ifdef DEBUGGING_PARSER}
xxx
  case Sy of
    StringSy: CodeSite.SendMsg('THtmlParser.Next: ''' + LcToken.S + '''');
    EofSy:    CodeSite.SendMsg('THtmlParser.Next: EOF');
  else
    if EndTag then
      CodeSite.SendMsg('THtmlParser.Next: </' + SymbToStr(Sy) + '>')
    else if IsXhtmlEndSy then
      CodeSite.SendMsg('THtmlParser.Next: <' + SymbToStr(Sy) + '/>')
    else
      CodeSite.SendMsg('THtmlParser.Next: <' + SymbToStr(Sy) + '>');
  end;
{$endif}
end;

{ Add Properties to the PropStack. }
procedure THtmlParser.PushNewProp(Sy: TElemSymb; Attributes: TAttributeList; const APseudo: ThtString = '');
var
  T: TAttribute;
  PropertiesOfStyleAttribute: TProperties;
begin
  if Attributes <> nil then
  begin
    PropertiesOfStyleAttribute := nil;
    T := nil;
    if Attributes.Find(StyleAttrSy, T) then
    begin
      PropertiesOfStyleAttribute := TProperties.Create;
      ParsePropertyStr(T.Name, PropertiesOfStyleAttribute);
    end;
    try
      PropStack.PushNewProp(Sy, PropertiesOfStyleAttribute, Attributes, APseudo)
    finally
      PropertiesOfStyleAttribute.Free;
    end;
  end
  else
    PropStack.PushNewProp(Sy, nil, nil, APseudo);
end;

procedure THtmlParser.PopProp;
{pop and free a TProperties from the Prop stack}
begin
  FPropStack.PopProp;
end;

procedure THtmlParser.PopAProp(Sy: TElemSymb);
begin
  PropStack.PopAProp(Sy);
end;

procedure THtmlParser.SetNoBreak(const AValue : Boolean);
begin
  FNoBreak := AValue;
  if Assigned(FPropStack) and Assigned(PropStack.Document) then begin
    PropStack.Document.NoBreak := AValue;
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
      if htCompareText(Token, '</textarea') = 0 then
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
        Sy := StringSy
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
          TxtArea.AddStr(S + CrLf); //^M^J);
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
  T := nil;
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
    CurrentUrlTarget.SetLast(THtmlViewer(CallingObject).LinkList, PropStack.SIndex);
    CurrentUrlTarget.Clear;
    InHref := False;
  end;
  PopAProp(ASy);
  if Assigned(Section) then
    Section.HRef(false, PropStack.Document, CurrentUrlTarget, nil, PropStack.Last);
end;

procedure THtmlParser.DoDivEtc(Sym: TElemSymb; const TermSet: TElemSymbSet);
var
  FormBlock, DivBlock: TBlock;
  FieldsetBlock: TFieldsetBlock;
  IsFieldsetLegend: Boolean;
//TODO -oBG, 15.03.2014: support display:inline
{$ifdef DO_PD_INLINE}
  IsInline: Boolean;
{$endif}
begin
  case Sym of
    DivSy, MainSy, HeaderSy, NavSy, SectionSy, ArticleSy, AsideSy, FooterSy, HGroupSy, BlockQuoteSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp(Sym, Attributes);
        CheckForAlign;

{$ifdef DO_PD_INLINE}
        IsInline := PropStack.Last.Display = pdInline;
        if not IsInline then
        begin
{$endif}
          DivBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
          SectionList.Add(DivBlock, TagIndex);
          SectionList := DivBlock.MyCell;
{$ifdef DO_PD_INLINE}
        end
        else
          DivBlock := nil;
{$endif}
        Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, SectionList.Count = 0);
        Next;
        DoBody([EndSymbFromSymb(Sym)] + TermSet);
        SectionList.Add(Section, TagIndex);
        if InHref then
          DoAEnd;
        PopAProp(Sym);
{$ifdef DO_PD_INLINE}
        if not IsInline then
        begin
{$endif}
          if SectionList.CheckLastBottomMargin then
          begin
            DivBlock.MargArray[MarginBottom] := ParagraphSpace;
            DivBlock.BottomAuto := True;
          end;
          DivBlock.CollapseNestedMargins;
          SectionList := DivBlock.OwnerCell;
{$ifdef DO_PD_INLINE}
        end;
{$endif}
        Section := nil; // TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, False);
        if Sy = EndSymbFromSymb(Sym) then
          Next;
      end;

    FieldsetSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp(Sym, Attributes);
        CheckForAlign;

        FieldsetBlock := TFieldsetBlock.Create(SectionList, Attributes, PropStack.Last);
        SectionList.Add(FieldsetBlock, TagIndex);
        SectionList := FieldsetBlock.MyCell;

        Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
        Next;
        DoBody([FieldsetEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp(Sym);
        if SectionList.CheckLastBottomMargin then
        begin
          FieldsetBlock.MargArray[MarginBottom] := ParagraphSpace;
          FieldsetBlock.BottomAuto := True;
        end;
        FieldsetBlock.CollapseNestedMargins;
        SectionList := FieldsetBlock.OwnerCell;

        Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
        if Sy = FieldsetEndSy then
          Next;
      end;

    LegendSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp(Sym, Attributes);
        CheckForAlign;

        FieldsetBlock := nil; // valium for the compiler
        IsFieldsetLegend := SectionList.OwnerBlock is TFieldsetBlock;
        if IsFieldsetLegend then
        begin
          FieldsetBlock := TFieldsetBlock(SectionList.OwnerBlock);
          SectionList := FieldsetBlock.Legend;
        end;
        Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
        Next;
        DoBody([LegendEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp(Sym);
        if IsFieldsetLegend then
        begin
          SectionList := FieldsetBlock.MyCell;
        end;
        Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
        if Sy = LegendEndSy then
          Next;
      end;

    CenterSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp(Sym, nil);
        Section := nil;
        Next;
        DoBody([CenterEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp(Sym);
        Section := nil;
        if Sy = CenterEndSy then
          Next;
      end;

    FormSy:
      repeat
        SectionList.Add(Section, TagIndex);
        Section := nil;
        PushNewProp(Sym, Attributes);
        FormBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
        SectionList.Add(FormBlock, TagIndex);
        SectionList := FormBlock.MyCell;
        if Assigned(PropStack.Document) then
          PropStack.Document.CurrentForm := ThtmlForm.Create(PropStack.Document, Attributes);
        Next;
        DoBody(TermSet + [FormEndSy, FormSy]);

        SectionList.Add(Section, TagIndex);
        Section := nil;
        PopAProp(Sym);
        if SectionList.CheckLastBottomMargin then
        begin
          FormBlock.MargArray[MarginBottom] := ParagraphSpace;
          FormBlock.BottomAuto := True;
        end;
        FormBlock.CollapseNestedMargins;
        SectionList := FormBlock.OwnerCell;
        if Sy = FormEndSy then
        begin
          if Assigned(PropStack.Document) then
            PropStack.Document.CurrentForm := nil;
          Next;
        end;
      until Sy <> FormSy; {in case <form> terminated by andother <form>}

    AddressSy:
      begin
        SectionList.Add(Section, TagIndex);
        Section := nil;
        DoLists(Sy, TermSet + [AddressEndSy]);
        if Sy in [AddressEndSy] then
          Next;
      end;

    StyleSy:
      begin
        DoStyle(LCh, Doc, '', '', False);
        Next;
      end;

    StyleEndSy:
      // Nothing to do
      Next;

  else
    Next;
  end;
end;

type
  TCellManager = class(ThtStringList)
    Table: THtmlTable;
    constructor Create(ATable: THtmlTable);
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

constructor TCellManager.Create(ATable: THtmlTable);
begin
  inherited Create;
  Table := ATable;
end;

function TCellManager.FindColNum(Row: Integer): Integer;
{given the row of insertion, returns the column number where the next cell will
 go or -1 if out of range.  Columns beyond any <col> definitions are ignored}
begin
  while Count <= Row do
    Add( htString( StringOfChar('o', Table.ColSpecs.Count)));
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
    Add( htString( StringOfChar('o', Table.ColSpecs.Count)));
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

function THtmlParser.DoCharSet(const Content: ThtString): Boolean;
var
  CP: TBuffCodePage;
begin
  CP := StrToCodePage(Content);
  Result := CP <> CP_UNKNOWN;
  if Result then
  begin
    PropStack.Last.CodePage := CP;
    Doc.CodePage := PropStack.Last.CodePage;
  end;
end;

{----------------DoTable}

procedure THtmlParser.DoTable;

  procedure DoColGroup(Table: THtmlTable; ColOK: Boolean);
  {reads the <colgroup> and <col> tags.  Put the info in THtmlTable's Cols list}

    procedure ReadColAttributes(var Spec: TSpecWidth; var Valign: ThtAlignmentStyle; var Align: ThtString; var Span: Integer);

      function AlignmentFromString(S: ThtString): ThtAlignmentStyle;
      begin
        S := htLowerCase(S);
        if TryStrToAlignmentStyle(S, Result) then
          exit;

        if (S = 'absmiddle') or (S = 'center') then
          Result := AMiddle
        else
          Result := ANone;
      end;

    var
      I: Integer;
      Algn: ThtAlignmentStyle;
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

            SpanAttrSy:
              Span := Max(1, Value);
          end;
    end;

  var
    xSpan, cSpan: Integer;
    xWidth, cWidth: TSpecWidth;
    xVAlign, cVAlign: ThtAlignmentStyle;
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
  Table: THtmlTable;
  SaveSectionList, JunkSaveSectionList: TCellBasic;
  SaveStyle: TFontStyles;
  SaveNoBreak: Boolean;
  SaveListLevel: Integer;
  RowVAlign, VAlign: ThtAlignmentStyle;
  Row: TCellList;
  CellObj: TCellObj;
  T: TAttribute;
  RowStack: Integer;
  NewBlock: TTableBlock;
  SetJustify: ThtJustify;
  CM: TCellManager;
  CellNum: Integer;
  TdTh: TElemSymb;
  ColOK: Boolean;
  CaptionBlock: TBlock;
  CombineBlock: TTableAndCaptionBlock;
  TopCaption: Boolean;
  RowType: TRowType;
  HFStack: Integer;
  FootList: TList;
  I: Integer;
  TrDisplay: ThtDisplayStyle; // Yunqa.de.
  S: ThtPropIndices;
  V: Variant;

  function GetVAlign(Default: ThtAlignmentStyle): ThtAlignmentStyle;
  var
    S: ThtString;
    T: TAttribute;
  begin
    Result := Default;
    T := nil;
    if Attributes.Find(VAlignSy, T) then
    begin
      S := htLowerCase(T.Name);
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
      if Assigned(Section) then
      begin
        // Do not add empty section
        if Length(Section.XP) <> 0 then
          SectionList.Add(Section, TagIndex)
        else
          Section.Free;
        Section := nil;
      end;
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
    I: ThtPropIndices;
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

  function GetDefaultCellBorderStyle(const TableBorderStyle: Variant; DefaultStyle: ThtBorderStyle): ThtBorderStyle;
  begin
    if (TableBorderStyle = Unassigned) or ((VarType(TableBorderStyle) in varInt) and (TableBorderStyle = IntNull)) then
      Result := DefaultStyle
    else
    begin
      Result := TableBorderStyle;
      case (Result) of
        bssInset: Result := bssOutset;
        bssOutset: Result := bssInset;
      end;
    end;
  end;

begin
  if TableLevel > 50 then
  begin
    Next;
    Exit;
  end;
  Inc(TableLevel);
  try
    if InHref then
      DoAEnd; {terminate <a>}
    SectionList.Add(Section, TagIndex);
    Section := nil;
    SaveSectionList := SectionList;
    SaveNoBreak := False;
    if Assigned(PropStack.Document) then 
    begin
      SaveStyle := PropStack.Document.FCurrentStyle;
      SaveNoBreak := PropStack.Document.NoBreak;
    end;
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
    PushNewProp(TableSy, Attributes);
    NewBlock := TTableBlock.Create(SaveSectionList, Attributes, PropStack.Last, TableLevel);
    if (NewBlock.Justify <> Centered) and not (NewBlock.Floating in [ALeft, ARight]) then
      NewBlock.Justify := SetJustify;
    Table := NewBlock.Table;
    NewBlock.MyCell.Add(Table, TagIndex); {the only item in the cell}
    CombineBlock := TTableAndCaptionBlock.Create(SaveSectionList, Attributes, PropStack.Last, NewBlock); {will be needed if Caption found}
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
      while True do
        case Sy of
          TableEndSy,
          EofSy,
          CaptionEndSy:
            break;
          
          TDSy, THSy:
            begin
              ColOK := False; {no more <colgroup> and <col> tags processed}
              if InHref then
                DoAEnd;
              if Assigned(PropStack.Document) then 
                PropStack.Document.CurrentStyle := SaveStyle;
              ListLevel := 0;
              if not Assigned(Row) then {in case <tr> is missing}
              begin
                RowVAlign := AMiddle;
                RowStack := PropStackIndex;
                PushNewProp(TrSy, nil);
                Row := TCellList.Create(nil, PropStack.Last);
              end
              else
              begin
                AddSection;
                while PropStackIndex > RowStack + 1 do
                  PopProp; {back stack off to Row item}
              end;
              TdTh := Sy;
              PushNewProp(TdTh, Attributes);
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

              // BG, 20.01.2013: translate Rules to cell property defaults:
              case Table.Rules of
                trAll:
                begin
                  PropStack.Last.SetPropertyDefault(BorderBottomStyle, GetDefaultCellBorderStyle(NewBlock.MargArrayO[BorderBottomStyle], bssInset));
                  PropStack.Last.SetPropertyDefault(BorderRightStyle , GetDefaultCellBorderStyle(NewBlock.MargArrayO[BorderRightStyle ], bssInset));
                  PropStack.Last.SetPropertyDefault(BorderTopStyle   , GetDefaultCellBorderStyle(NewBlock.MargArrayO[BorderTopStyle   ], bssInset));
                  PropStack.Last.SetPropertyDefault(BorderLeftStyle  , GetDefaultCellBorderStyle(NewBlock.MargArrayO[BorderLeftStyle  ], bssInset));
                end;

                trRows:
                begin
                  PropStack.Last.SetPropertyDefaults([BorderTopStyle, BorderBottomStyle], bssSolid);
                  PropStack.Last.SetPropertyDefaults([BorderLeftStyle, BorderRightStyle], bssNone);
                end;

                trCols:
                begin
                  PropStack.Last.SetPropertyDefaults([BorderTopStyle, BorderBottomStyle], bssNone);
                  PropStack.Last.SetPropertyDefaults([BorderLeftStyle, BorderRightStyle], bssSolid);
                end;

                trGroups:
                  ; // not yet supported
              end;

              for S := BorderTopWidth to BorderLeftWidth do
              begin
                V := PropStack.Last.Props[S];
                if (VarType(V) in varInt) and (V = IntNull) then
                begin
                  if Table.BorderWidth > 0 then
                    PropStack.Last.Props[S] := 1 //Table.BorderWidth
                  else
                    PropStack.Last.Props[S] := 3
                end;
              end;

              for S := BorderTopColor to BorderLeftColor do
              begin
                V := PropStack.Last.Props[S];
                if (VarType(V) in varInt) and (V = IntNull) then
                  PropStack.Last.Props[S] := Table.BorderColor;
              end;

              CellObj := TCellObj.Create(NewBlock, VAlign, Attributes, PropStack.Last);
              SectionList := CellObj.Cell;
              T := nil;
              if ((CellObj.SpecWd.Value = 0) or (CellObj.SpecWd.VType <> wtAbsolute))
                and (Attributes.Find(NoWrapSy, T) or (PropStack.Last.Props[piWhiteSpace] = 'nowrap')) then
                NoBreak := True {this seems to be what IExplorer does}
              else
                NoBreak := False;
              Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, True);
              SkipWhiteSpace;
              Next;
              DoBody(TableTermSet);
            end;
          
          CaptionSy:
            begin
              if InHref then
                DoAEnd;
              if Assigned(PropStack.Document) then
                PropStack.Document.CurrentStyle := SaveStyle;
              NoBreak := False;
              AddSection;
              if Attributes.Find(AlignSy, T) then
                TopCaption := Lowercase(T.Name) <> 'bottom';
              PushNewProp(CaptionSy, Attributes);
              if not Assigned(CaptionBlock) then
                CaptionBlock := TBlock.Create(SaveSectionList, Attributes, PropStack.Last);
              SectionList := CaptionBlock.MyCell;
              Next;
              DoBody(TableTermSet);

              SectionList.Add(Section, TagIndex);
              PopAProp(CaptionSy);
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
              TdTh := OtherChar;
              case Sy of
                THeadSy:
                  if Table.Rows.Count = 0 then
                  begin
                    RowType := THead;
                    TdTh := Sy;
                  end
                  else
                    RowType := TBody;

                TBodySy:
                  begin
                    RowType := TBody;
                    TdTh := Sy;
                  end;

                TFootSy:
                  begin
                    RowType := TFoot;
                    TdTh := Sy;
                  end;

                THeadEndSy, TBodyEndSy, TFootEndSy:
                  RowType := TBody;
              end;
              if TdTh <> OtherChar then
                PushNewProp(TdTh, Attributes);
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
              PushNewProp(Sy, Attributes);
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
            if ((Sy = StringSy) and (LCToken.S = SpcChar)) or (Sy = CommandSy) then
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
      PopAProp(TableSy);
      if Assigned(PropStack.Document) then begin
        PropStack.Document.CurrentStyle := SaveStyle;
      end;
      NoBreak := SaveNoBreak;
      ListLevel := SaveListLevel;
      CM.Free;
    end;
    Next;
  finally
    Dec(TableLevel);
  end;
end;

procedure THtmlParser.GetOptions(Select: TOptionsFormControlObj);
 {get the <option>s for Select form control}
var
  InOption, Selected: Boolean;
  WS: UnicodeString;
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
          WS := htTrim(WS);
          if InOption then
            Select.AddStr(WS, Selected, Attr, CodePage);
          Selected := False;
          WS := '';
          InOption := Sy = OptionSy;
          if InOption then
          begin
            T := nil;
            Selected := Attributes.Find(SelectedSy, T);
            Attr := Attributes.CreateStringList;
          end;
        end;
      StringSy: if InOption then
          WS := WS + LCToken.S;
    end;
    Next;
  end;
  if InOption then
  begin
    WS := htTrim(WS);
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
    T := nil;
    if Attributes.Find(NameSy, T) then
      Item.MapName := Uppercase(T.Name);
    Next;
    while (Sy <> MapEndSy) and (Sy <> EofSy) and (ErrorCnt < 3) do
    begin
      if Sy = AreaSy then
        Item.AddArea(Attributes)
      else if Sy <> StringSy then
        Inc(ErrorCnt);
      Next;
    end;
    if Sy = MapEndSy then
      PropStack.Document.MapList.Add(Item)
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

  procedure Next;
    {Special Next routine to get the next token}

    procedure GetTag; {simplified 'Pick up a Tag' routine}

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
        Sy := StringSy;
        Exit;
      end;
      Sy := CommandSy; {catch all}
      while IsTagChar(LCh) do
      begin
        htAppendChr(Text, LCh);
        GetCh;
      end;
      if htCompareText(Text, '</script') = 0 then
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
        htAppendChr(Text, GreaterChar);
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
        GetTag;

    else
      Sy := StringSy;
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
  if not IsXhtmlEndSy then
    if Assigned(AScript) then
    begin
      InScript := True;
      try
        GetCh; {get character here with Inscript set to allow immediate comment}
        T := nil;

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
        Next;
        while (Sy <> ScriptEndSy) and (Sy <> EofSy) do
        begin
          if Sy = EolSy then
          begin
            htAppendChr(S, CrChar);
            htAppendChr(S, LfChar);
          end
          else
            htAppendStr(S, Text);
          Next;
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
        Next;
      until Sy in [ScriptEndSy, EofSy];
    end;
end;

procedure THtmlParser.DoObjectTag(var C: ThtChar; var N, IX: Integer);

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
  Sym: TElemSymb;
begin
  if CallingObject is THtmlViewer then
  begin
    if Assigned(THtmlViewer(CallingObject).OnObjectTag) then
    begin
      Sym := Sy;

      SL := Attributes.CreateStringList;
      try
        if not Assigned(Section) then
          Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
        PushNewProp(Sym, Attributes);
        Prop := PropStack.Last;
        PO := Section.CreatePanel(Attributes, SectionList, Prop);
        PO.ProcessProperties(PropStack.Last);
        WantPanel := False;

        Params := ThtStringList.Create;
        try
          T := nil;
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
          THtmlViewer(CallingObject).OnObjectTag(CallingObject, PO.Panel, SL, Params, WantPanel);
        finally
          Params.Free;
        end;
      finally
        SL.Free;
      end;

      if WantPanel then
      begin
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        Section.AddPanel1(PO, TagIndex);
        PopAProp(Sym);
        while not (Sy in [ObjectEndSy, EofSy]) do
          Next1;
      end
      else
      begin
        PropStack.Document.PanelList.Remove(PO);
        PopAProp(Sym);
        PO.Free;
      end;
    end
    else
      Next1;
  end
  else
    Next1;
end;

procedure THtmlParser.CollectNormalText(S: TTokenObj);
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
//              if Length(Entity) = 1 then
//                Buffer.Add(Entity[1], SaveIndex)
//              else
                Buffer.Add(Entity, SaveIndex);
          end;

        SpcChar, CrChar, LfChar, TabChar:
          begin
            if not LinkSearch then
              Buffer.Add(SpcChar, PropStack.SIndex);
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
      S.AddString(Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure THtmlParser.CollectPreText(S: TTokenObj);
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

procedure THtmlParser.CollectText(S: TTokenObj; AWhiteSpace : ThtWhiteSpaceStyle);
begin
  case AWhiteSpace of
    wsNormal : CollectNormalText(S);
    wsPre : CollectPreText(S);
    //TODO:  research these. see also TSection.AddTokenObj()
    wsNoWrap : CollectNormalText(S);
    wsPreWrap : CollectNormalText(S);
    wsPreLine : CollectNormalText(S);
  end;
end;

{----------------DoCommonSy}

procedure THtmlParser.DoCommonSy;

  procedure ChangeTheFont(Sy: TElemSymb; Pre: Boolean);
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
      with Attributes[I] do
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
            if TryStrToColor(Name, False, NewColor) then
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
    PushNewProp(FontSy, Attributes);
    Prop := TProperties(PropStack.Last);
    Prop.SetFontBG;
    if Prop.HasBorderStyle then {start of inline border}
      PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
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

  procedure DoA;
  var
    FoundHRef: Boolean;
    Link, Tmp: ThtString;
    I: Integer;
    T: TAttribute;
    Prop: TProperties;
  begin
    if InHref then
      DoAEnd;
    FoundHRef := False;
    Link := '';
    T := nil;
    for I := 0 to Attributes.Count - 1 do
      with Attributes[I] do
        if Which = HRefSy then
        begin
          FoundHRef := True;
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
    PushNewProp(ASy, Attributes, Link);
    Prop := PropStack.Last;
    Prop.SetFontBG;
    if Prop.HasBorderStyle then {start of inline border}
      PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
    if not Assigned(Section) then
      Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True)
    else
      Section.ChangeFont(PropStack.Last);

    if Attributes.Find(NameSy, T) then
    begin
      Tmp := UpperCase(T.Name);
  {Author may have added '#' by mistake}
      if (Length(Tmp) > 0) and (Tmp[1] = '#') then
        Delete(Tmp, 1, 1);
      PropStack.Document.AddChPosObjectToIDNameList(Tmp, PropStack.SIndex);
      Section.AnchorName := True;
    end;
    if FoundHRef then
      Section.HRef(true, PropStack.Document, CurrentUrlTarget, Attributes, PropStack.Last);
    if IsXhtmlEndSy then
      DoAEnd;
  end;

  procedure DoImage();
  var
    IO: TSizeableObj;
  begin
    IO := Section.AddImage(Attributes, SectionList, TagIndex, PropStack.Last);
    IO.ProcessProperties(PropStack.Last);
  end;

  procedure DoPanel();
  var
    IO: TSizeableObj;
  begin
    IO := Section.AddPanel(Attributes, SectionList, TagIndex, PropStack.Last);
    IO.ProcessProperties(PropStack.Last);
  end;

  procedure DoIFrame();
  var
    IO: TSizeableObj;
  begin
    IO := Section.AddFrame(Attributes, SectionList, TagIndex, PropStack.Last);
    IO.ProcessProperties(PropStack.Last);
  end;

  procedure DoProgress();
  var
    IO: TSizeableObj;
  begin
    IO := Section.AddProgress(Attributes, SectionList, TagIndex, PropStack.Last);
    IO.ProcessProperties(PropStack.Last);
  end;

  procedure DoMeter();
  var
    IO: TSizeableObj;
  begin
    IO := Section.AddMeter(Attributes, SectionList, TagIndex, PropStack.Last);
    IO.ProcessProperties(PropStack.Last);
  end;

  procedure DoTextArea();
  var
    FormControl: TFormControlObj;
  begin
    FormControl := Section.AddFormControl(TextAreaSy, PropStack.Document, Attributes, SectionList, TagIndex, PropStack.Last);
    Self.DoTextArea(FormControl as TTextAreaFormControlObj);
    FormControl.ProcessProperties(PropStack.Last);
  end;

  procedure DoInput();
  var
    FormControl: TFormControlObj;
  begin
    FormControl := Section.AddFormControl(InputSy, PropStack.Document, Attributes, SectionList, TagIndex, PropStack.Last);
    FormControl.ProcessProperties(PropStack.Last);
  end;
  
  procedure DoSelect();
  var
    FormControl: TFormControlObj;
  begin
    FormControl := Section.AddFormControl(SelectSy, PropStack.Document, Attributes, SectionList, TagIndex, PropStack.Last);
    GetOptions(FormControl as TOptionsFormControlObj);
    FormControl.ProcessProperties(PropStack.Last);
  end;

  procedure DoPre;
  var
    S: TTokenObj;
    InForm, InP: Boolean;
    PreBlock, FormBlock, PBlock: TBlock;

    procedure FormEnd;
    begin
      PropStack.Document.CurrentForm := nil;
      if Assigned(Section) then
      begin
        Section.AddTokenObj(S);
        SectionList.Add(Section, TagIndex);
      end;
      S.Clear;
      Section := nil;
      PopAProp(FormSy);
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
      PopAProp(PSy);
      SectionList := PBlock.OwnerCell;
      InP := False;
    end;

    procedure NewSection;
    begin
      Section.AddTokenObj(S);
      S.Clear;
      SectionList.Add(Section, TagIndex);
      Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, False);
    end;

    procedure DoBeforeSy(Sy: TElemSymb);
    begin
      Section.AddTokenObj(S);
      S.Clear;
      PushNewProp(Sy, Attributes);
    end;

    procedure DoAfterSy(Sy: TElemSymb);
    begin
      PopAProp(Sy);
      S.Clear;
    end;

    procedure DoAfterEndSy(Sy: TElemSymb);
    begin
      Section.AddTokenObj(S);
      S.Clear;
      PopAProp(EndSymbToSymb(Sy));
    end;

  var
    I: Integer;
    Done: Boolean;
    InitialStackIndex: Integer;
    SaveSy: TElemSymb;
    Prop: TProperties;
    C: ThtChar;
    N, IX: Integer;
    Before, After, Intact: Boolean;
    LW : ThtWhiteSpaceStyle;

  begin
    InForm := False;
    InP := False;
    S := TTokenObj.Create;
    FormBlock := nil;
    try
      SectionList.Add(Section, TagIndex);
      PushNewProp(PreSy, Attributes);
      InitialStackIndex := PropStackIndex;
      PreBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
      if not StyleTypes.TryStrToWhiteSpace(PropStack.Last.Props[piWhiteSpace],LW) then
        LW :=  wsNormal;
      SectionList.Add(PreBlock, TagIndex);
      SectionList := PreBlock.MyCell;
      Section := TPreformated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
      Done := False;
      while not Done do
        case LCh of
          '<':
            begin
              Next;
              SaveSy := Sy;
              case SaveSy of
                StringSy: {this would be an isolated LessChar}
                  S.AddUnicodeChar('<', PropStack.SIndex);

                BRSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    SectionList.Add(Section, TagIndex);
               {look for page-break}
                    //PushNewProp(SaveSy, Attributes.TheClass, '', '', '', Attributes.TheStyle);
                    PushNewProp(SaveSy, Attributes);
                    PropStack.Last.GetPageBreaks(Before, After, Intact);
                    if Before or After then
                      SectionList.Add(TPage.Create(SectionList, nil, PropStack.Last), TagIndex);
                    PopAProp(SaveSy);
                    Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, False);
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
                    PushNewProp(SaveSy, Attributes);
                    PBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
                    SectionList.Add(PBlock, TagIndex);
                    SectionList := PBlock.MyCell;
                    Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
                    InP := True;
                  end;

                PEndSy:
                  begin
                    if InP then
                    begin
                      PEnd;
                      Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
                    end;
                  end;

                PreEndSy, TDEndSy, THEndSy, TableSy:
                  Done := True;

                MarkSy, TimeSy, BSy, ISy, StrongSy, EmSy, InsSy, DelSy,
                CiteSy, VarSy, USy, SSy, StrikeSy, SpanSy, SubSy, SupSy,
                BigSy, SmallSy, LabelSy, AbbrSy, AcronymSy, DfnSy,
                CodeSy, TTSy, KbdSy, SampSy:
                  begin
                    DoBeforeSy(SaveSy);
                    Prop := PropStack.Last;
                    Prop.SetFontBG;
                    if Prop.HasBorderStyle then {start of inline border}
                      PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
                    Section.ChangeFont(PropStack.Last);
                  end;

                MarkEndSy, TimeEndSy, BEndSy, IEndSy, StrongEndSy, EmEndSy, InsEndSy, DelEndSy,
                CiteEndSy, VarEndSy, UEndSy, SEndSy, StrikeEndSy, SpanEndSy, SubEndSy, SupEndSy,
                BigEndSy, SmallEndSy, LabelEndSy, AbbrEndSy, AcronymEndSy, DfnEndSy,
                CodeEndSy, TTEndSy, KbdEndSy, SampEndSy:
                  begin
                    DoAfterEndSy(Sy);
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
                    PopAProp(FontSy);
                    Section.AddTokenObj(S);
                    S.Clear;
                    Section.ChangeFont(PropStack.Last);
                  end;

                ASy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    DoA;
                  end;

                AEndSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    DoAEnd;
                  end;

                ImageSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoImage;
                    DoAfterSy(SaveSy);
                  end;

                PanelSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoPanel;
                    DoAfterSy(SaveSy);
                  end;

                IFrameSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoIFrame;
                    DoAfterSy(SaveSy);
                  end;

                ProgressSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoProgress;
                    DoAfterSy(SaveSy);
                  end;
                MeterSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoMeter;
                    DoAfterSy(SaveSy);
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
                    SectionList.Add(TPage.Create(SectionList, nil, PropStack.Last), TagIndex);
                    Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, False);
                  end;
                ButtonSy:
                  begin
                    DoBeforeSy(SaveSy);
                    Prop := PropStack.Last;
                    Prop.SetFontBG;
                    if Prop.HasBorderStyle then {start of inline border}
                      PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
                    Section.ChangeFont(PropStack.Last);
                  end;
                ButtonEndSy:
                  begin
                    DoAfterEndSy(Sy);
                    Section.ChangeFont(PropStack.Last);
                  end;
                InputSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoInput;
                    DoAfterSy(SaveSy);
                  end;

                SelectSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoSelect;
                    DoAfterSy(SaveSy);
                  end;

                TextAreaSy:
                  begin
                    DoBeforeSy(SaveSy);
                    DoTextArea;
                    DoAfterSy(SaveSy);
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

                    PushNewProp(SaveSy, Attributes);
                    FormBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
                    SectionList.Add(FormBlock, TagIndex);
                    SectionList := FormBlock.MyCell;
                    if Assigned(PropStack.Document) then begin
                      PropStack.Document.CurrentForm := ThtmlForm.Create(PropStack.Document, Attributes);
                    end;
                    Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
                    InForm := True;
                  end;

                FormEndSy:
                  begin
                    if InP then
                      PEnd;
                    if InForm then
                      FormEnd;
                    if not Assigned(Section) then
                      Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
                  end;

                MapSy:
                  DoMap;

                ScriptSy:
                  DoScript(PropStack.Document.ScriptEvent);

                StyleSy:
                  begin
                    DoStyle(LCh, Doc, '', '', False);
                    Next;
                  end;

                StyleEndSy:
                  // Nothing to do
                  Next;
              end;
            end;

          CrChar:
            begin
              NewSection;
              GetCh;
            end;

          #1..#8:
            GetCh;

          #0:
            Done := True;
        else
          CollectText(S, LW);
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
      if (Sy = PreEndSy) or (LCh = #0) then
        Next;
    finally
      S.Free;
    end;
  end;

  procedure DoBeforeSy(Sy: TElemSymb);
  begin
    if not Assigned(Section) then
      Section := TSection.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, True);
    PushNewProp(Sy, Attributes);
  end;

  procedure DoAfterSy(Sy: TElemSymb);
  begin
    PopAProp(Sy);
    Next;
  end;

  procedure DoAfterEndSy(Sy: TElemSymb);
  begin
    PopAProp(EndSymbToSymb(Sy));
    Next;
  end;

var
  SaveSy, SaveEndSy: TElemSymb;
  N, IX: Integer;
  T: TAttribute;
  HeadingBlock: TBlock;
  HRBlock: THRBlock;
  HorzLine: THorzLine;
  Done: Boolean;
  Page: TPage;
  Prop: TProperties;
  C: ThtChar;

begin
  SaveSy := Sy;
  case SaveSy of

    StringSy:
      begin
        if not Assigned(Section) then
        begin {don't create a section for a single space}
          if (LCToken.Count >= 1) and (LCToken.S <> SpcChar) then
          begin
            Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, True);
            Section.AddTokenObj(LCToken);
          end;
        end
        else
          Section.AddTokenObj(LCToken);
        Next;
      end;

    ImageSy:
      begin
        DoBeforeSy(SaveSy);
        Prop := PropStack.Last;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        DoImage;
        DoAfterSy(SaveSy);
      end;

    PanelSy:
      begin
        DoBeforeSy(SaveSy);
        Prop := PropStack.Last;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        DoPanel;
        DoAfterSy(SaveSy);
      end;

    IFrameSy:
      begin
        DoBeforeSy(SaveSy);
        Prop := PropStack.Last;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        DoIFrame;
        DoAfterSy(SaveSy);
      end;

    ProgressSy:
      begin
        DoBeforeSy(SaveSy);
        Prop := PropStack.Last;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        DoProgress;
        DoAfterSy(SaveSy);
      end;
    MeterSy:
      begin
        DoBeforeSy(SaveSy);
        Prop := PropStack.Last;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        DoMeter;
        DoAfterSy(SaveSy);
      end;
    
    ObjectSy:
      begin
        C := LCh;
        N := Doc.Position;
        IX := PropStack.SIndex;
        DoObjectTag(C, N, IX);
      end;

    ObjectEndSy:
      Next;

    InputSy:
      begin
        DoBeforeSy(SaveSy);
        DoInput;
        DoAfterSy(SaveSy);
      end;
    SelectSy:
      begin
        DoBeforeSy(SaveSy);
        DoSelect;
        DoAfterSy(SaveSy);
      end;
    ButtonSy:
      begin
        PushNewProp(Sy, Attributes);
        Prop := TProperties(PropStack.Last);
        Prop.SetFontBG;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;
    ButtonEndSy:
      begin
        PopAProp(EndSymbToSymb(Sy));
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;

    TextAreaSy:
      begin
        DoBeforeSy(SaveSy);
        DoTextArea;
        DoAfterSy(SaveSy);
      end;

    TextAreaEndSy: {a syntax error but shouldn't hang}
      Next;

    PageSy:
      begin
        SectionList.Add(Section, TagIndex);
        Section := nil;
        Page := TPage.Create(SectionList, nil, PropStack.Last);
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

    MarkSy, TimeSy, BSy, ISy, StrongSy, EmSy, InsSy, DelSy,
    CiteSy, VarSy, USy, SSy, StrikeSy, SpanSy, SubSy, SupSy,
    BigSy, SmallSy, LabelSy, AbbrSy, AcronymSy, DfnSy,
    CodeSy, TTSy, KbdSy, SampSy:
      begin
        PushNewProp(Sy, Attributes);
        Prop := TProperties(PropStack.Last);
        Prop.SetFontBG;
        if Prop.HasBorderStyle then {start of inline border}
          PropStack.Document.ProcessInlines(PropStack.SIndex, Prop, True);
        if Assigned(Section) then
          Section.ChangeFont(PropStack.Last);
        Next;
      end;

    MarkEndSy, TimeEndSy, BEndSy, IEndSy, StrongEndSy, EmEndSy, InsEndSy, DelEndSy,
    CiteEndSy, VarEndSy, UEndSy, SEndSy, StrikeEndSy, SpanEndSy, SubEndSy, SupEndSy,
    BigEndSy, SmallEndSy, LabelEndSy, AbbrEndSy, AcronymEndSy, DfnEndSy,
    CodeEndSy, TTEndSy, KbdEndSy, SampEndSy,
    FontEndSy:
      begin
        PopAProp(EndSymbToSymb(Sy));
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
        DoA;
        Next;
      end;

    AEndSy:
      begin
        DoAEnd;
        Next;
      end;

    H1Sy..H6Sy:
      begin
        SaveEndSy := EndSymbFromSymb(SaveSy);
        SectionList.Add(Section, TagIndex);
        PushNewProp(SaveSy, Attributes);
        CheckForAlign;
        SkipWhiteSpace;
        Next;
        if Sy = CenterSy then
        begin
          PropStack.Last.Assign('center', TextAlign);
          Next;
        end;
        HeadingBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
        SectionList.Add(HeadingBlock, TagIndex);
        SectionList := HeadingBlock.MyCell;

        Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, True);
        Done := False;
        while not Done do
          case Sy of
            MarkSy, TimeSy, BSy, ISy, StrongSy, EmSy, InsSy, DelSy,
            CiteSy, VarSy, USy, SSy, StrikeSy, SpanSy, SubSy, SupSy,
            BigSy, SmallSy, LabelSy, AbbrSy, AcronymSy, DfnSy,
            CodeSy, TTSy, KbdSy, SampSy,
            MarkEndSy, TimeEndSy, BEndSy, IEndSy, StrongEndSy, EmEndSy, InsEndSy, DelEndSy,
            CiteEndSy, VarEndSy, UEndSy, SEndSy, StrikeEndSy, SpanEndSy, SubEndSy, SupEndSy,
            BigEndSy, SmallEndSy, LabelEndSy, AbbrEndSy, AcronymEndSy, DfnEndSy,
            CodeEndSy, TTEndSy, KbdEndSy, SampEndSy,
            StringSy, ASy, AEndSy, BrSy, NoBrSy, NoBrEndSy, WbrSy,
            InputSy, ButtonSy, ButtonEndSy, ProgressSy, ProgressEndSy, MeterSy, MeterEndSy,
            TextAreaSy, TextAreaEndSy, SelectSy, ImageSy, FontSy, FontEndSy, BaseFontSy,
            ScriptSy, ScriptEndSy, StyleSy, StyleEndSy, PanelSy, HRSy, ObjectSy, ObjectEndSy:
              DoCommonSy;

            CommandSy:
              Next;

            PSy:
              DoP([]);

            DivSy, MainSy, HeaderSy, NavSy, SectionSy, ArticleSy, AsideSy, FooterSy, HGroupSy:
              DoDivEtc(Sy, [SaveEndSy]);
          else
            Done := True;
          end;
        SectionList.Add(Section, TagIndex);
        Section := nil;
        PopAProp(SaveSy);
        SectionList := HeadingBlock.OwnerCell;
        if Sy = SaveEndSy then
          Next;
      end;

    H1EndSy..H6EndSy:
      Next; {in case of extra entry}

    HRSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp(SaveSy, Attributes);
      {Create Horzline first as it effects the PropStack}
        HorzLine := THorzLine.Create(SectionList, Attributes, PropStack.Last);
        HRBlock := THRBlock.Create(SectionList, Attributes, PropStack.Last);
        HRBlock.MyHRule := Horzline;
        HRBlock.Align := Horzline.Align;
        SectionList.Add(HRBlock, TagIndex);
        SectionList := HRBlock.MyCell;

        SectionList.Add(HorzLine, TagIndex);
        SectionList := HRBlock.OwnerCell;
        PopAProp(SaveSy);
        Section := nil;
        Next;
      end;

    PreSy:
      begin
        T := nil;
        if not Attributes.Find(WrapSy, T) then
          DoPre
        else
        begin
          SectionList.Add(Section, TagIndex);
          Section := nil;
          PushNewProp(SaveSy, Attributes);
          Next;
        end;
      end;

    PreEndSy:
      DoAfterEndSy(SaveSy);

    TableSy:
      DoTable;

    MapSy:
      DoMap;

    ScriptSy:
      begin
        DoScript(PropStack.Document.ScriptEvent);
        Next;
      end;

    StyleSy:
      begin
        DoStyle(LCh, Doc, '', '', False);
        Next;
      end;

    StyleEndSy:
      // Nothing to do
      Next;
  else
    begin
      Assert(False, 'DoCommon can''t handle <' + htStringToString(SymbToStr(Sy) + GreaterChar));
      Next; {as loop protection}
    end;
  end;
end; {DoCommon}

{----------------DoP}

procedure THtmlParser.DoP(const TermSet: TElemSymbSet);
var
  NewBlock: TBlock;
//  LastAlign, LastClass, LastID, LastTitle: ThtString;
//  LastStyle: TProperties;
begin
  if PSy in TermSet then
    Exit;
  SectionList.Add(Section, TagIndex);
  Section := nil;

//BG, 28.02.2016: don't skip empty paragraphs any longer for correctly not
//  collapsing margins, if paragraph has padding or border
//  SkipWhiteSpace;
//  LastAlign := FindAlignment;
//  LastClass := Attributes.TheClass;
//  LastID := Attributes.TheID;
//  LastStyle := Attributes.TheStyle;
//  LastTitle := Attributes.TheTitle;
//  Next;
//  while Sy in [PSy, PEndSy] do
//  begin {recognize only the first <p>}
//    if Sy = PSy then
//    begin
//      LastAlign := FindAlignment; {if a series of <p>, get last alignment}
//      LastClass := Attributes.TheClass;
//      LastID := Attributes.TheID;
//      LastStyle := Attributes.TheStyle;
//      LastTitle := Attributes.TheTitle;
//    end;
//    SkipWhiteSpace;
//    Next;
//  end;
//{at this point have the 'next' attributes, so use 'Last' items here}
//  PushNewProp(PSy, LastClass, LastID, '', LastTitle, LastStyle);
//  PushNewProp(PSy, LastClass, LastID, '', LastTitle, LastStyle);
//  if LastAlign <> '' then
//    PropStack.Last.Assign(LastAlign, TextAlign);
//  if LastAlign <> '' then
//    PropStack.Last.Assign(LastAlign, TextAlign);

  PushNewProp(PSy, Attributes);
  NewBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
  SectionList.Add(NewBlock, TagIndex);
  SectionList := NewBlock.MyCell;
  //BG, 17.01.2018: we need a section to remember initial attributes:
  Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, True);

  SkipWhiteSpace;
  Next;
  while not (Sy in Termset) and
    (Sy in [StringSy, NoBrSy, NoBrEndSy, WbrSy, MarkSy, MarkEndSy, TimeSy, TimeEndSy, BSy, ISy, BEndSy, IEndSy,
      AbbrSy, AbbrEndSy, AcronymSy, AcronymEndSy, DfnSy, DfnEndSy,
      EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, InsSy, InsEndSy, DelSy, DelEndSy, CiteSy,
      CiteEndSy, VarSy, VarEndSy, SubSy, SubEndSy, SupSy, SupEndSy,
      SSy, SEndSy, StrikeSy, StrikeEndSy, TTSy, CodeSy, KbdSy, SampSy,
      TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy,
      SmallEndSy, BigSy, SmallSy, ASy, AEndSy, SpanSy, SpanEndSy,

      InputSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
      ImageSy, FontSy, BaseFontSy, BRSy,
      ObjectSy, ObjectEndSy, IFrameSy, IFrameEndSy, ButtonSy, ButtonEndSy,
      ProgressSy, ProgressEndSy, MeterSy, MeterEndSy, 
      MapSy, PageSy, ScriptSy, ScriptEndSy, StyleSy, StyleEndSy, PanelSy, CommandSy])
  do
    if Sy <> CommandSy then
      DoCommonSy
    else
      Next; {unknown tag}
  if Sy = TableSy then
    NewBlock.MargArray[MarginBottom] := 0; {open paragraph followed by table, no space}
  SectionList.Add(Section, TagIndex);
  Section := nil;
  if InHref then
    DoAEnd;
  PopAProp(PSy);
  SectionList := NewBlock.OwnerCell;
  if Sy = PEndSy then
    Next;
end;

{----------------DoBr}

procedure THtmlParser.DoBr(const TermSet: TElemSymbSet);
var
  T: TAttribute;
  Before, After, Intact: Boolean;
  HasClear: Boolean;
  L: TLineBreak;
begin
  if BRSy in TermSet then
    Exit;

  T := nil;
  L := nil;
  PushNewProp(BRSy, Attributes);
  HasClear := Attributes.Find(ClearSy, T) or VarIsStr(PropStack.Last.Props[Clear]);
  if HasClear then
    L := TLineBreak.Create(SectionList, Attributes, PropStack.Last);
  PropStack.Last.GetPageBreaks(Before, After, Intact);
  PopAProp(BRSy);

  if HasClear then
  begin
    if Assigned(Section) then
    begin
      SectionList.Add(Section, TagIndex);
      Section := nil;
    end;
//    Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, False);
//    PushNewProp(BRSy, Attributes.TheClass, '', '', '', Attributes.TheStyle);
//    PropStack.Last.GetPageBreaks(Before, After, Intact);
//    PopAProp(BRSy);
    SectionList.Add(L, TagIndex);
    if Before or After then
    begin
//      SectionList.Add(Section, TagIndex);
      SectionList.Add(TPage.Create(SectionList, nil, PropStack.Last), TagIndex);
      Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, False);
    end;
  end
  else
  begin
    if not Assigned(Section) then
      Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, False);
    Section.AddChar(BrkCh, TagIndex);
    SectionList.Add(Section, TagIndex);
//    PushNewProp(BRSy, Attributes.TheClass, '', '', '', Attributes.TheStyle);
//    PropStack.Last.GetPageBreaks(Before, After, Intact);
//    PopAProp(BRSy);
    if Before or After then
      SectionList.Add(TPage.Create(SectionList, nil, PropStack.Last), TagIndex);
    Section := TSection.Create(SectionList, Attributes, PropStack.Last, CurrentUrlTarget, False);
  end;
  Next;
end;

procedure THtmlParser.DoListItem(
  {$ifdef DO_LI_INLINE}var LiBlock: TBlockLi; var LiSection: TSection;{$endif}
  BlockType, Sym: TElemSymb; LineCount: Integer; Index: ThtChar; Plain: Boolean; const TermSet: TElemSymbSet);
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
  PushNewProp(Sym, Attributes);
{$ifdef DO_LI_INLINE}
  IsInline := PropStack.Last.Display = pdInline;
  IsFirst := not IsInline or (LiSection = nil) or not (LiSection.Display = pdInline);
  if IsFirst then
{$endif}
  begin
    LiBlock := TBlockLI.Create(SectionList, Attributes, PropStack.Last, BlockType, Plain, Index, LineCount, ListLevel);
    SectionList.Add(LiBlock, TagIndex);
    LiSection := TSection.Create(LiBlock.MyCell, nil, PropStack.Last, CurrentUrlTarget, True);
    Section := LISection;
{$ifdef DO_LI_INLINE}
  end
  else
  begin
    Section := TSection.Create(LiBlock.MyCell, nil, PropStack.Last, CurrentUrlTarget, True);
{$endif}
  end;
  SectionList := LiBlock.MyCell;

  SkipWhiteSpace;
  Next;
  while true do {handle second part like after a <p>}
    case Sy of
      AbbrSy,  AbbrEndSy, AcronymSy, AcronymEndSy, DfnSy, DfnEndSy,
      StringSy, NoBrSy, NoBrEndSy, WbrSy, MarkSy, MarkEndSy, TimeSy, TimeEndSy, BSy, ISy, BEndSy, IEndSy,
      EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, InsSy, InsEndSy, DelSy, DelEndSy, CiteSy,
      CiteEndSy, VarSy, VarEndSy, SubSy, SubEndSy, SupSy, SupEndSy,
      SSy, SEndSy, StrikeSy, StrikeEndSy, TTSy, CodeSy, KbdSy, SampSy,
      TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy,
      SmallEndSy, BigSy, SmallSy, ASy, AEndSy, SpanSy, SpanEndSy,
      InputSy, ButtonSy, ButtonEndSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
      ImageSy, FontSy, BaseFontSy, BrSy, H1Sy..H6Sy,
      MapSy, PageSy, ScriptSy, ScriptEndSy, StyleSy, StyleEndSy, PanelSy,
      ObjectSy, ObjectEndSy, IFrameSy, IFrameEndSy, ProgressSy, ProgressEndSy, MeterSy, MeterEndSy:
        DoCommonSy;

      PSy:
        if BlockType in [OLSy, ULSy, DirSy, MenuSy, DLSy] then
          DoP([])
        else
          break; {else terminate lone <li>s on <p>}

      PEndSy,
      CommandSy:
        Next;

      DivSy, MainSy, HeaderSy, NavSy, SectionSy, ArticleSy, AsideSy, FooterSy, HGroupSy,
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
  LiBlock.CollapseNestedMargins;
  PopAProp(Sym);
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

procedure THtmlParser.DoLists(Sym: TElemSymb; const TermSet: TElemSymbSet);
var
  T: TAttribute;
  LineCount: Integer;
  Plain: Boolean;
  Index: ThtChar;
  NewBlock: TListBlock;
  EndSym: TElemSymb;
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
  T := nil;
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
      begin
        Plain := Attributes.Find(PlainSy, T)
             or (Attributes.Find(TypeSy, T) and ((Lowercase(T.Name) = 'none') or (Lowercase(T.Name) = 'plain')));

        if Attributes.Find(TypeSy, T) then
          if LowerCase(T.Name) = 'disc' then  Index := 'd'
          else if LowerCase(T.Name) = 'circle' then  Index := 'c'
          else if LowerCase(T.Name) = 'square' then  Index := 's';
      end;       
  end;
  SectionList.Add(Section, TagIndex);
  Section := nil;
  PushNewProp(Sym, Attributes);

  NewBlock := TListBlock.Create(SectionList, Attributes, PropStack.Last);
// BG, 25.03.2012: unused:  NewBlock.IsListBlock := not (Sym in [AddressSy, BlockquoteSy, DLSy]);
  SectionList.Add(NewBlock, TagIndex);
  SectionList := NewBlock.MyCell;
  Next;
  if Sy in [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, BlockQuoteEndSy] then
  begin {guard against <ul></ul> and similar combinations}
    PopAProp(EndSymbToSymb(Sy));
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

      DivSy, MainSy, HeaderSy, NavSy, SectionSy, ArticleSy, AsideSy, FooterSy, HGroupSy, CenterSy, FormSy:
        DoDivEtc(Sy, [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, LISy, DDSy, DTSy, EofSy] + TermSet);

      AbbrSy, AbbrEndSy, AcronymSy, AcronymEndSy, DfnSy, DfnEndSy,
      StringSy, BRSy, HRSy, TableSy, MarkSy, MarkEndSy, TimeSy, TimeEndSy,
      BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
      USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
      SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy,
      TTSy, CodeSy, KbdSy, SampSy, TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
      ASy, AEndSy, SpanSy, SpanEndSy,
      H1Sy..H6Sy, H1EndSy..H6EndSy, PreSy,
      InputSy, ButtonSy, ButtonEndSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
      ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
      SmallEndSy, MapSy, PageSy, ScriptSy, StyleSy, StyleEndSy, PanelSy, NoBrSy, NoBrEndSy, WbrSy,
      ObjectSy, ObjectEndSy, IFrameSy, IFrameEndSy, ProgressSy, ProgressEndSy, MeterSy, MeterEndSy:
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
  NewBlock.CollapseNestedMargins;
  Section := nil;
  if InHref then
    DoAEnd;
  PopAProp(Sym); {maybe save stack position}
  SectionList := NewBlock.OwnerCell;
end;

{----------------DoBase}

procedure THtmlParser.DoBase;
var
  I: Integer;
begin
  with Attributes do
    for I := 0 to Count - 1 do
      with Attributes[I] do
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
  T := nil;
  if Assigned(SoundEvent) and Attributes.Find(SrcSy, T) then
  begin
    T1 := nil;
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
  T := nil;
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
  if (Sender is THtmlViewer) and (htCompareText(HttpEq, 'content-type') = 0) then
  begin
    DoCharset(Content);
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
  while Sy = StringSy do
  begin
    TitleEnd := PropStack.SIndex;
    Next;
  end;
end;

//-- BG ---------------------------------------------------------- 29.09.2016 --
procedure THtmlParser.DoStyle(var C: ThtChar; Doc: TBuffer; const APath, AMedia: ThtString; FromLink: Boolean);
var
  IsCss: Boolean;
  I: Integer;
  Parser: THtmlStyleTagParser;
begin
  IsCss := True;
  for I := 0 to  Attributes.Count - 1 do
    with Attributes[I] do
      case Which of
        TypeSy:
          IsCss := htLowerCase(Name) = 'text/css';
      end;

  if IsCss then
  begin
    Parser := THtmlStyleTagParser.Create(FUseQuirksMode);
    Parser.OnMatchMediaQuery := MatchMediaQuery;
    try
      Parser.DoStyle(PropStack.Document.Styles, C, Doc, APath, AMedia, FromLink);
    finally
      Parser.Free;
    end;
  end
  else if not IsXhtmlEndSy and not FromLink then
  begin
    GetCh; {make up for not having next character on entry}
    repeat
      Next;
    until Sy in [StyleEndSy, EofSy];
  end;
end;

procedure THtmlParser.DoStyleLink; {handle <link> for stylesheets}
var
  Style: TBuffer;
  C: ThtChar;
  I: Integer;
  Url, Rel, Rev, Media: ThtString;
  IsStyleSheet: Boolean;
  Stream: TStream;
  Viewer: THtmlViewer;
  Path: ThtString;
begin
  IsStyleSheet := False;
  for I := 0 to Attributes.Count - 1 do
    with Attributes[I] do
      case Which of
        RelSy:
          begin
            Rel := Name;
            if htCompareText(Rel, 'stylesheet') = 0 then
              IsStyleSheet := True;
          end;

        RevSy:
          Rev := Name;

        HRefSy:
          Url := Name;

        MediaSy:
          Media := Name;
      end;

  if IsStyleSheet and (Length(Url) > 0) then
  begin
    Stream := nil;
    Viewer := CallingObject as THtmlViewer;
    try
      Viewer.htStreamRequest(Url, Stream, Path);
      try
        if Stream <> nil then
        begin
          Stream.Position := 0;
          Style := TBuffer.Create(Stream, Url);
          try
            C := SpcChar;

            DoStyle(C, Style, Path, Media, True);
          finally
            Style.Free;
          end;
        end;
      finally
        Viewer.htStreamRequested(Url, Stream);
      end;
    except
    end;
  end;
  if Assigned(LinkEvent) then
    LinkEvent(CallingObject, Rel, Rev, Url);
  Next;
end;

{-------------DoBody}

procedure THtmlParser.DoBody(const TermSet: TElemSymbSet);
var
  HtmlAttributes: TAttributeList;

  procedure PushHtmlPropsIfAny;
  var
    PRec: PtPositionRec;
    Image: ThtString;
    Val: TColor;
  begin
    if HtmlAttributes <> nil then
    begin
      PushNewProp(HtmlSy, HtmlAttributes);
      FreeAndNil(HtmlAttributes);

      // set document background
      if PropStack.Last.GetBackgroundImage(Image) and (Image <> '') then
      begin
        PropStack.Last.GetBackgroundPos(0, 0, PRec);
        PropStack.Document.SetBackgroundImage(Image, PRec);
      end;
      Val := PropStack.Last.GetBackgroundColor;
      if Val <> clNone then
        PropStack.Document.SetBackGround(Val or PalRelative);
    end;
  end;

var
  I: Integer;
  Val: TColor;
  AMarginHeight, AMarginWidth: Integer;
  CP: TBuffCodePage;
{$ifdef DO_LI_INLINE}
  LiBlock: TBlockLi;
  LiSection: TSection;
{$endif}
begin
  try
    HtmlAttributes := nil;
    repeat
      if Sy in TermSet then
        Exit;
      case Sy of
        StringSy:
          DoCommonSy;

        BRSy, HRSy,
        //NameSy, HRefSy,
        ASy, AEndSy,
        AbbrSy, AbbrEndSy, AcronymSy, AcronymEndSy, DfnSy, DfnEndSy,
        MarkSy, MarkEndSy, TimeSy, TimeEndSy,
        BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, InsSy, InsEndSy, DelSy, DelEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy,
        TTSy, CodeSy, KbdSy, SampSy, TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, SpanSy, SpanEndSy,
        H1Sy..H6Sy, H1EndSy..H6EndSy, PreSy, TableSy,
        InputSy, ButtonSy, ButtonEndSy, TextAreaSy, TextAreaEndSy, SelectSy, LabelSy, LabelEndSy,
        ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
        SmallEndSy, MapSy, PageSy, ScriptSy, ScriptEndSy, StyleSy, StyleEndSy, PanelSy, NoBrSy, NoBrEndSy, WbrSy,
        ObjectSy, ObjectEndSy, IFrameSy, IFrameEndSy, ProgressSy, ProgressEndSy, MeterSy, MeterEndSy:
          begin
            PushHtmlPropsIfAny;
            DoCommonSy;
          end;

        XmlSy:
          begin
            IsXHTML := True;
            for I := 0 to Attributes.Count - 1 do
              with Attributes[I] do
                case Which of
                  //VersionSy:;
                  EncodingSy:
                    begin
                      CP := StrToCodePage(Name);
                      if CP <> CP_UNKNOWN then
                        Doc.CodePage := CP;
                    end;
                end;
            Next;
          end;

        HtmlSy:
          begin
            // BG, 27.01.2013: cannot push html attributes here as html styles may be read later in a style tag before body.
            if (BodyBlock.MyCell.Count = 0) and (TableLevel = 0) then {make sure we're at beginning}
              HtmlAttributes := TAttributeList.CreateCopy(Attributes);
            Next;
          end;

        BodySy:
          begin
            PushHtmlPropsIfAny;
            if (BodyBlock.MyCell.Count = 0) and (TableLevel = 0) then {make sure we're at beginning}
            begin
              PropStack.Document.ClearLists;
              if Assigned(Section) then
              begin
                Section.CheckFree;
                Section.Free; {Will start with a new section}
              end;
              PushNewProp(Sy, Attributes);
              AMarginHeight := (CallingObject as THtmlViewer).MarginHeight;
              AMarginWidth := (CallingObject as THtmlViewer).MarginWidth;
              for I := 0 to Attributes.Count - 1 do
                with Attributes[I] do
                  case Which of
                    BackgroundSy:
                      PropStack.Last.Assign('url(' + Name + ')', BackgroundImage);

                    TextSy:
                      if TryStrToColor(Name, False, Val) then
                        PropStack.Last.Assign(Val or PalRelative, Color);

                    BGColorSy:
                      if TryStrToColor(Name, False, Val) then
                        PropStack.Last.Assign(Val or PalRelative, BackgroundColor);

                    LinkSy:
                      if TryStrToColor(Name, False, Val) then
                        PropStack.Document.Styles.ModifyLinkColor('link', Val);

                    VLinkSy:
                      if TryStrToColor(Name, False, Val) then
                        PropStack.Document.Styles.ModifyLinkColor('visited', Val);

                    OLinkSy:
                      if TryStrToColor(Name, False, Val) then
                      begin
                        PropStack.Document.Styles.ModifyLinkColor('hover', Val);
                        PropStack.Document.LinksActive := True;
                      end;
                    MarginWidthSy, LeftMarginSy:
                      AMarginWidth := Min(Max(0, Value), 200);
                    MarginHeightSy, TopMarginSy:
                      AMarginHeight := Min(Max(0, Value), 200);
                    BGPropertiesSy:
                      if htCompareText(Name, 'fixed') = 0 then
                        PropStack.Last.Assign('fixed', BackgroundAttachment);
                  end;
              if FUseQuirksMode then
                FPropStack.Document.Styles.FixupTableColor(FPropStack.Last);

              PropStack.Last.Assign(AMarginWidth, MarginLeft);
              PropStack.Last.Assign(AMarginWidth, MarginRight);
              PropStack.Last.Assign(AMarginHeight, MarginTop);
              PropStack.Last.Assign(AMarginHeight, MarginBottom);

              SectionList := BodyBlock.OwnerCell;
              SectionList.Remove(BodyBlock);
              BodyBlock := TBodyBlock.Create(SectionList, Attributes, PropStack.Last);
              SectionList.Add(BodyBlock, TagIndex);
              SectionList := BodyBlock.MyCell;

              Section := TSection.Create(SectionList, nil, PropStack.Last, nil, True);
            end;
            Next;
          end;

        OLSy, ULSy, DirSy, MenuSy, DLSy:
          begin
            PushHtmlPropsIfAny;
            DoLists(Sy, TermSet);
            if not (Sy in TermSet) then
              Next;
          end;

        LISy:
          begin
            PushHtmlPropsIfAny;
            DoListItem({$ifdef DO_LI_INLINE}LiBlock, LiSection, {$endif}LiAloneSy, Sy, 1, '1', False, TermSet);
          end;

        DDSy, DTSy:
          begin
            PushHtmlPropsIfAny;
            DoListItem({$ifdef DO_LI_INLINE}LiBlock, LiSection, {$endif}DLSy, Sy, 1, '1', False, TermSet);
          end;

        PSy:
          begin
            PushHtmlPropsIfAny;
            DoP(TermSet);
          end;

        FormEndSy:
          begin
            PropStack.Document.CurrentForm := nil;
            Next;
          end;

        DivSy, MainSy, HeaderSy, NavSy, SectionSy, ArticleSy, AsideSy, FooterSy, HGroupSy,
        CenterSy, FormSy, BlockQuoteSy, AddressSy, FieldsetSy, LegendSy:
          begin
            PushHtmlPropsIfAny;
            DoDivEtc(Sy, TermSet);
          end;

        TitleElemSy:
          begin
            DoTitle;
          end;

        LinkElemSy:
          begin
            DoStyleLink;
          end;

        BgSoundSy:
          begin
            PushHtmlPropsIfAny;
            DoSound;
          end;

        MetaSy:
          begin
            DoMeta(CallingObject);
          end;

        BaseSy:
          begin
            DoBase;
          end;
      else
        Next;
      end;
    until (Sy = EofSy);
    Next;
  finally
    FreeAndNil(HtmlAttributes);
  end;
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
  FPixelsPerInch := ASectionList.PixelsPerInch;
  FPropStack.Document := ASectionList;
  CallingObject := ASectionList.TheOwner;
  IncludeEvent := AIncludeEvent;
  FPropStack.Clear;
  FPropStack.Add(TProperties.Create(FPropStack, FUseQuirksMode, FPixelsPerInch));
  FPropStack[0].CopyDefault(FPropStack.Document.Styles.DefProp);
  FPropStack.SIndex := -1;

  if CallingObject is THtmlViewer then
    THtmlViewer(CallingObject).CodePage := PropStack[0].CodePage;

  BodyBlock := TBodyBlock.Create(SectionList, nil, FPropStack[0]);
  SectionList.Add(BodyBlock, TagIndex);
  SectionList := BodyBlock.MyCell;

  CurrentURLTarget := TUrlTarget.Create;
  InHref := False;
  BaseFontSize := 3;

  FBase := '';
  FBaseTarget := '';
  PropStack.Document.CurrentStyle := [];
  PropStack.Document.CurrentForm := nil;
  Section := TSection.Create(SectionList, nil, PropStack.Last, nil, True);
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
  AMetaEvent: TMetaType; ALinkEvent: TLinkType; AMatchMediaQuery: ThtMediaQueryEvent);
{$IFNDEF NoTabLink}
const
  MaxTab = 400; {maximum number of links before tabbing of links aborted}
var
  TabCount, SaveSIndex, SavePosition: Integer;
  T: TAttribute;
{$ENDIF}
begin
  {Todo:
  Precedence rules

  In the case of conflict between multiple encoding declarations, precedence rules apply to determine
  which declaration wins out. For XHTML and HTML, the precedence is as follows, with 1 being the highest.

    HTTP Content-Type header
    byte-order mark (BOM)
    XML declaration
    meta element
    link charset attribute

  From: http://www.w3.org/International/questions/qa-html-encoding-declarations

  Note that:

  Using the XML declaration for XHTML served as HTML. XHTML served as HTML is parsed as HTML, even though it
  is based on XML syntax, and therefore an XML declaration should not be recognized by the browser. It is for
  this reason that you should use a pragma directive to specify the encoding when serving XHTML in this way*.
   }
  if Self.IsXHTML then
    ExtractCharsetFromXMLProlog;
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
        T := nil;
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
      Sy := OtherChar;
      InHref := False;
      BaseFontSize := 3;
      FBase := '';
      FBaseTarget := '';
      PropStack.Document.CurrentStyle := [];
      PropStack.Document.CurrentForm := nil;
      InScript := False;
      NoBreak := False;
      InComment := False;
      ListLevel := 0;
      TableLevel := 0;
      LinkSearch := False;
{$ENDIF}

      LinkSearch := False;
      SoundEvent := ASoundEvent;
      MetaEvent := AMetaEvent;
      LinkEvent := ALinkEvent;
      MatchMediaQuery := AMatchMediaQuery;
      try
        GetCh; {get the reading started}
        //Next;
        DoBody([]);
      except
        on E: Exception do
          Assert(False, E.Message);
      end;

    finally
      FreeAndNil(Attributes);
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
  S: TTokenObj;
  Done: Boolean;
  PreBlock: TBlock;

  procedure NewSection;
  begin
    Section.AddTokenObj(S);
    S.Clear;
    SectionList.Add(Section, TagIndex);
    Section := TPreFormated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, False);
  end;

begin
  S := TTokenObj.Create;
  try
    SectionList.Add(Section, TagIndex);
//    PushNewProp(PreSy, Attributes.TheClass, Attributes.TheID, '', '', Attributes.TheStyle);
    PushNewProp(PreSy, Attributes);
    PreBlock := TBlock.Create(SectionList, Attributes, PropStack.Last);
    SectionList.Add(PreBlock, TagIndex);
    SectionList := PreBlock.MyCell;
    Section := TPreformated.Create(SectionList, nil, PropStack.Last, CurrentUrlTarget, False);
    Done := False;
    while not Done do
      case LCh of
        CrChar:
          begin
            NewSection;
            GetCh;
          end;
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
    PopAProp(PreSy);
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
    ParseInit(ASectionList, nil);
    InScript := True;

    try
      GetCh; {get the reading started}
      DoText;
    finally
      FreeAndNil(Attributes);
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
    PropStack.Add(TProperties.Create(FPropStack, FUseQuirksMode, FPixelsPerInch));
    GetCh; {get the reading started}
    Next;
    repeat
      case Sy of
        FrameSetSy:
          begin
            DoFrameSet(FrameViewer, FrameSet, FName);
          end;

        BaseSy:
          DoBase;

        TitleElemSy:
          DoTitle;

        BgSoundSy:
          DoSound;

        ScriptSy:
          begin
            DoScript(FrameViewer.OnScript);
            Next;
          end;

        NoFramesSy:
          begin
            repeat
              Next;
            until (Sy = NoFramesEndSy) or (Sy = EofSy);
            Next;
          end;

        MetaSy:
          DoMeta(FrameSet);

        BodySy, H1Sy..H6Sy, HRSy, TableSy, ImageSy, OLSy, ULSy, MenuSy, DirSy,
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
    PropStack.Document := nil;
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
      FreeAndNil(Attributes);
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
    PropStack.Clear;
    PropStack.Add(TProperties.Create(FPropStack, FUseQuirksMode, FPixelsPerInch));
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

        XmlSy, BodySy, H1Sy..H6Sy, HRSy, TableSy, ImageSy, OLSy, ULSy, MenuSy, DirSy,
        PSy, PreSy, FormSy, AddressSy, BlockQuoteSy, DLSy:
          SetExit := True;
      else
        Next;
      end;
    until SetExit or (Sy = EofSy);
    PropStack.Clear;
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
    PropStack.Document := nil;
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
      FreeAndNil(Attributes);
      Doc.Position := Pos;
    end;
  finally
    FreeAndNil( FPropStack );
  end;
end;

//-- BG ---------------------------------------------------------- 04.02.2015 --
function TryShorterEntity(Entity: ThtString; out I: Integer; var Collect: ThtString): Boolean;
var
  J: Integer;
begin
  I := -1;
  J := Length(Entity);
  while J > 2 do
  begin
    SetLength(Entity, J - 1);
    if Entities.Find(Entity, I) then
    begin
      I := PEntity(Entities.Objects[I]).Value;
      if I <= 255 then // only these entities may be found without trailing ';'
      begin
        Collect := Copy(Collect, J + 1, MaxInt);
        Result := True;
        Exit;
      end;
    end;
    Dec(J);
  end;
  Result := False;
end;

function THtmlParser.GetEntityStr(CodePage: Integer): ThtString;
{read an entity and return it as a ThtString.}

  procedure AddNumericChar(I: Integer; ForceUnicode: Boolean);
  // Adds the given value as new ThtChar to the ThtString.
  var
    Buf: array[0..10] of ThtChar;
  begin
    case I of
      9, 10, 13:
        Result := SpcChar
    else
      if I < ord(SpcChar) then {control ThtChar}
        Result := '?' {is there an error symbol to use here?}
      else
      if (I >= 127) and (I <= 159) and not ForceUnicode then
      begin
        {127 to 159 not valid Unicode}
        if MultiByteToWideChar(CodePage, 0, @I, 1, @Buf, SizeOf(Buf)) = 0 then
          Buf[0] := ThtChar(I);
        SetString(Result, Buf, 1);
      end
      else
        Result := WideChar(I);
    end;
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
  // by '#' or one of the predefined (named) entities.
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
          AddNumericChar(I, True)
        else
          Result := Collect;
      end
      else if TryShorterEntity(Entity, I, Collect) then
      begin
        AddNumericChar(I, True);
        htAppendStr(Result, Collect);
      end
      else
        Result := Collect;
    end; {case}
  end; {while}
end;

//-- BG ---------------------------------------------------------- 27.04.2014 --
function ReplaceEntities(const Str: ThtString; CodePage: Integer): ThtString;
{Result: Str with all entities converted to widechars.}
var
  PPos, PEnd: PhtChar;
  LCh: ThtChar;

  procedure GetCh;
  begin
    if PPos <= PEnd then
    begin
      LCh := PPos^;
      Inc(PPos);
    end
    else
      LCh := #0;
  end;

  function GetEntityStr: ThtString;

    procedure AddNumericChar(I: Integer; ForceUnicode: Boolean);
    // Adds the given value as new ThtChar to the ThtString.
    var
      Buf: array[0..10] of ThtChar;
    begin
      case I of
        9, 10, 13:
          Result := SpcChar;
      else
        if I < ord(SpcChar) then {control ThtChar}
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
          AddNumericChar(I, True)
        else
          Result := Collect;
      end
      else if TryShorterEntity(Entity, I, Collect) then
      begin
        AddNumericChar(I, True);
        htAppendStr(Result, Collect);
      end
      else
        Result := Collect;
    end; {case}
  end;

begin
  SetLength(Result, 0);
  if Length(Str) > 0 then
  begin
    PPos := @Str[1];
    PEnd := PPos + Length(Str) - 1;
    GetCh;
    while true do
    begin
      case LCh of
        #0:
          break;

        AmperChar:
          htAppendStr(Result, GetEntityStr);
      else
        htAppendChr(Result, LCh);
        GetCh;
      end;
    end;
  end;
end;

end.

