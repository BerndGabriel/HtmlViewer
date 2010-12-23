{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
  Ch:
      The same character in upper case.
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
      Gets the next character from the stream.  Fills Ch and LCh.  Skips
      comments.
  Next:
      Gets the next token.  Fills Sy, LCToken, Attributes.  Calls GetCh so the
      next character after the present token is available.  Each part of the
      parser is responsible for calling Next after it does its thing.
}

unit Readhtml;

interface
uses
  Messages, Classes, Graphics, Controls,
  HtmlGlobals, HtmlBuffer, HtmlUn2, HtmlSubs, StyleUn;

type
  LoadStyleType = (lsFile, lsString, lsInclude);

procedure ParseHTMLString(const S: ThtString; ASectionList: TList;
  AIncludeEvent: TIncludeType;
  ASoundEvent: TSoundType; AMetaEvent: TMetaType; ALinkEvent: TLinkType);
procedure ParseTextString(const S: ThtString; ASectionList: TList);

procedure FrameParseString(FrameViewer: TFrameViewerBase; FrameSet: TObject;
  ALoadStyle: LoadStyleType; const FName, S: ThtString; AMetaEvent: TMetaType);
function IsFrameString(ALoadStyle: LoadStyleType; const FName, S: ThtString;
  FrameViewer: TFrameViewerBase): boolean;
function TranslateCharset(const Content: ThtString; var Charset: TFontCharset; var CodePage: Integer): boolean;
procedure PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProp: TProperties);
procedure PopAProp(const Tag: ThtString);

implementation

uses
  Windows, SysUtils, Math, Variants, HtmlSbs1, HtmlView, StylePars, UrlSubs;

var
  Sy: Symb;
  Section: TSection;
  SectionList: TCellBasic;
  CurrentURLTarget: TURLTarget;
  InHref: boolean;
  Attributes: TAttributeList;
  BaseFontSize: integer;
  InScript: boolean; {when in a <SCRIPT>}
  TagIndex: integer;
  BodyBlock: TBodyBlock;
  ListLevel: integer;
  TableLevel: integer;
  Entities: ThtStringList;
  InComment: boolean;
  LinkSearch: boolean;
  IsUTF8: boolean;


const
  MaxRes = 82;
  MaxEndRes = 62;
  ResWords: array[1..MaxRes] of ThtString = (
    'HTML', 'TITLE', 'BODY', 'HEAD', 'B', 'I', 'H', 'EM', 'STRONG', 'U',
    'CITE', 'VAR', 'TT', 'CODE', 'KBD', 'SAMP', 'OL', 'UL', 'DIR', 'MENU',
    'DL', 'A', 'ADDRESS', 'BLOCKQUOTE', 'PRE', 'CENTER', 'TABLE', 'TD', 'TH', 'CAPTION',
    'FORM', 'TEXTAREA', 'SELECT', 'OPTION', 'FONT', 'SUB', 'SUP', 'BIG', 'SMALL', 'P',
    'MAP', 'FRAMESET', 'NOFRAMES', 'SCRIPT', 'DIV', 'S', 'STRIKE', 'TR', 'NOBR', 'STYLE',
    'SPAN', 'COLGROUP', 'LABEL', 'THEAD', 'TBODY', 'TFOOT', 'OBJECT', 'DD', 'DT', 'LI',
    'FIELDSET', 'LEGEND',

    'BR', 'HR', 'IMG', 'BASE', 'BUTTON', 'INPUT', 'SELECTED', 'BASEFONT',
    'AREA', 'FRAME', 'PAGE', 'BGSOUND', 'WRAP', 'META', 'PANEL', 'WBR', 'LINK', 'COL',
    'PARAM', 'READONLY');

  ResSy: array[1..MaxRes] of Symb = (
    htmlSy, TitleSy, BodySy, HeadSy, BSy, ISy, HeadingSy, EmSy, StrongSy, USy,
    CiteSy, VarSy, TTSy, CodeSy, KbdSy, SampSy, OLSy, ULSy, DirSy, MenuSy,
    DLSy, ASy, AddressSy, BlockQuoteSy, PreSy, CenterSy, TableSy, TDsy, THSy, CaptionSy,
    FormSy, TextAreaSy, SelectSy, OptionSy, FontSy, SubSy, SupSy, BigSy, SmallSy, PSy,
    MapSy, FrameSetSy, NoFramesSy, ScriptSy, DivSy, SSy, StrikeSy, TRSy, NoBrSy, StyleSy,
    SpanSy, ColGroupSy, LabelSy, THeadSy, TBodySy, TFootSy, ObjectSy, DDSy, DTSy, LISy,
    FieldsetSy, LegendSy,

    BRSy, HRSy, ImageSy, BaseSy, ButtonSy, InputSy, SelectedSy, BaseFontSy,
    AreaSy, FrameSy, PageSy, BgSoundSy, WrapSy, MetaSy, PanelSy, WbrSy, LinkSy, ColSy,
    ParamSy, ReadonlySy);

  {keep these in order with those above}
  EndResSy: array[1..MaxEndRes] of Symb = (
    HtmlEndSy, TitleEndSy, BodyEndSy, HeadEndSy, BEndSy,
    IEndSy, HeadingEndSy, EmEndSy, StrongEndSy, UEndSy,
    CiteEndSy, VarEndSy, TTEndSy, CodeEndSy, KbdEndSy,
    SampEndSy, OLEndSy, ULEndSy, DirEndSy, MenuEndSy,
    DLEndSy, AEndSy, AddressEndSy, BlockQuoteEndSy, PreEndSy,
    CenterEndSy, TableEndSy, TDEndSy, THEndSy, CaptionEndSy,
    FormEndSy, TextAreaEndSy, SelectEndSy, OptionEndSy, FontEndSy,
    SubEndSy, SupEndSy, BigEndSy, SmallEndSy, PEndSy,
    MapEndSy, FrameSetEndSy, NoFramesEndSy, ScriptEndSy, DivEndSy,
    SEndSy, StrikeEndSy, TREndSy, NoBrEndSy, StyleEndSy,
    SpanEndSy, ColGroupEndSy, LabelEndSy, THeadEndSy, TBodyEndSy,
    TFootEndSy, ObjectEndSy, DDEndSy, DTEndSy, LIEndSy,
    FieldsetEndSy, LegendEndSy);

type
  EParseError = class(Exception);
var
  LCh, Ch: ThtChar;
  LastChar: (lcOther, lcCR, lcLF);
  Value: integer;
  LCToken: TokenObj;
  LoadStyle: LoadStyleType;
  Buff, BuffEnd: PhtChar;
  DocS: ThtString;
  HaveTranslated: boolean;

  IBuff, IBuffEnd: PhtChar;
  SIBuff: ThtString;
  IncludeEvent: TIncludeType;
  CallingObject: TViewerBase;
  SaveLoadStyle: LoadStyleType;
  SoundEvent: TSoundType;
  MetaEvent: TMetaType;
  LinkEvent: TLinkType;

function PropStackIndex: integer;
begin
  Result := PropStack.Count - 1;
end;

function SymbToStr(Sy: Symb): ThtString;
var
  I: integer;
begin
  for I := 1 to MaxRes do
    if ResSy[I] = Sy then
    begin
      Result := Lowercase(ResWords[I]);
      Exit;
    end;
  Result := '';
end;

function EndSymbToStr(Sy: Symb): ThtString;
var
  I: integer;
begin
  for I := 1 to MaxEndRes do
    if EndResSy[I] = Sy then
    begin
      Result := Lowercase(ResWords[I]);
      Exit;
    end;
  Result := '';
end;

function EndSymbFromSymb(Sy: Symb): Symb;
var
  I: integer;
begin
  for I := 1 to MaxEndRes do
    if ResSy[I] = Sy then
    begin
      Result := EndResSy[I];
      Exit;
    end;
  Result := HtmlSy; {won't match}
end;

function StrToSymb(const S: ThtString): Symb;
var
  I: integer;
  S1: ThtString;
begin
  S1 := UpperCase(S);
  for I := 1 to MaxRes do
    if ResWords[I] = S1 then
    begin
      Result := ResSy[I];
      Exit;
    end;
  Result := OtherSy;
end;

function GetNameValueParameter(var Name, Value: ThtString): boolean; forward;

function ReadChar: ThtChar;
begin
  case LoadStyle of
    lsString:
      begin
        if Buff < BuffEnd then
        begin
          Result := Buff^;
          Inc(Buff);
          Inc(PropStack.SIndex);
        end
        else
          Result := EOFChar;
      end;

    lsInclude:
      if IBuff < IBuffEnd then
      begin
        Result := IBuff^;
        Inc(IBuff);
      end
      else
      begin
        IBuff := nil; {reset for next include}
        LoadStyle := SaveLoadStyle;
        Result := ReadChar;
      end;
  else
    Result := #0; {to prevent warning msg}
  end;
  if (Cardinal(Buff) and $FFF = 0) {about every 4000 chars}
    and not LinkSearch and Assigned(PropStack.MasterList) and (DocS <> '') then
    THtmlViewerBase(CallingObject).htProgress(((Buff - PhtChar(DocS)) * PropStack.MasterList.ProgressStart) div (BuffEnd - PhtChar(DocS)));
end;

{----------------GetchBasic; }

function GetchBasic: ThtChar; {read a character}
begin
  LCh := ReadChar;
  case LCh of {skip a ^J after a CrChar or a CrChar after a ^J}
    ^M: if LastChar = lcLF then
        LCh := ReadChar;
    ^J: if LastChar = lcCR then
        LCh := ReadChar;
  end;
  case LCh of
    ^M: LastChar := lcCR;
    ^J:
      begin
        LastChar := lcLF;
        LCh := CrChar;
      end;
  else
    begin
      LastChar := lcOther;
      if LCh = TabChar then
        LCh := SpcChar;
    end;
  end;
  Ch := htUpCase(LCh);
  if (LCh = EofChar) and InComment then
    raise EParseError.Create('Open Comment at End of HTML File');
  Result := LCh
end;

{-------------GetCh}

procedure GetCh;
{Return next ThtChar in Lch, its uppercase value in Ch.  Ignore comments}
var
  Done, Comment: boolean;

  function Peek: ThtChar; {take a look at the next ThtChar}
  begin
    case LoadStyle of
      lsString:
        begin
          if Buff < BuffEnd then
            Result := Buff^
          else
            Result := EOFChar;
        end;

      lsInclude:
        if IBuff < IBuffEnd then
          Result := IBuff^
        else
        begin
          IBuff := nil;
          LoadStyle := SaveLoadStyle;
          Result := Peek;
        end;
    else
      Result := #0; {to prevent warning msg}
    end;
  end;

  procedure DoDashDash; {do the comment after a <!-- }
  begin
    repeat
      while Ch <> '-' do
        GetChBasic; {get first '-'}
      GetChBasic;
      if Ch = '-' then {second '-'}
      begin
        while Ch = '-' do
          GetChBasic; {any number of '-'}
        while (Ch = SpcChar) or (Ch = CrChar) do
          GetChBasic; {eat white space}
        if Ch = '!' then
          GetChBasic; {accept --!> also}
        Done := Ch = GreaterChar;
      end
      else
        Done := False;
    until Done;
    InComment := False;
  end;

  procedure ReadToGT; {read to the next GreaterChar }
  begin
    while Ch <> GreaterChar do
      GetChBasic;
    InComment := False;
  end;

  procedure DoInclude;
  {recursive suggestions by Ben Geerdes}
  var
    S, Name, Value: ThtString;
    Rest: ThtString;
    SL: ThtStringList;
    SaveLCToken: TokenObj;
  begin
    S := '';
    SaveLCToken := LCToken;
    LCToken := TokenObj.Create;
    try
      GetChBasic;
      while Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('_'), ThtChar('0')..ThtChar('9')] do
      begin
        S := S + LCh;
        GetChBasic;
      end;
      SL := ThtStringList.Create;
      while GetNameValueParameter(Name, Value) do
        SL.Add(Name + '=' + Value);
      DoDashDash;
      Rest := IBuff;
      SIBuff := '';
      IncludeEvent(CallingObject, S, SL, SIBuff);
      if Length(SIBuff) > 0 then
      begin
        if LoadStyle <> lsInclude then
          SaveLoadStyle := LoadStyle;
        LoadStyle := lsInclude;
        SIBuff := SIBuff + Rest;
        IBuff := PhtChar(SIBuff);
        IBuffEnd := IBuff + Length(SIBuff);
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
    if (Ch = LessChar) and not InScript then
    begin
      if Peek = '!' then
      begin
        GetChBasic;
        Comment := True;
        InComment := True;
        GetChBasic;
        if Ch = '-' then
        begin
          GetChBasic;
          if Ch = '-' then
          begin
            GetChBasic;
            if Assigned(IncludeEvent) and (Ch = '#') then
              DoInclude
            else
              DoDashDash; {a <!-- comment}
          end
          else
            ReadToGT;
        end
        else
          ReadToGT;
      end
      else if Peek = '%' then { <%....%> regarded as comment }
      begin
        Comment := True;
        GetChBasic;
        repeat
          GetChBasic;
        until (Ch = '%') and (Peek = GreaterChar) or (Ch = EOFChar);
        GetChBasic;
      end;
    end;
  until not Comment;
end;

{-------------SkipWhiteSpace}

procedure SkipWhiteSpace;
begin
  while (LCh in [SpcChar, TabChar, CrChar]) do
    GetCh;
end;

procedure GetEntity(T: TokenObj; CodePage: integer); forward;
function GetEntityStr(CodePage: integer): ThtString; forward;

function GetQuotedValue(var S: ThtString): boolean;
{get a quoted ThtString but strip the quotes}
var
  Term: ThtChar;
  SaveSy: Symb;
begin
  Result := False;
  Term := Ch;
  if (Term <> '"') and (Term <> '''') then
    Exit;
  Result := True;
  SaveSy := Sy;
  GetCh;
  while not (Ch in [Term, EofChar]) do
  begin
    if LCh = AmperChar then
      S := S + GetEntityStr(CP_ACP)
    else
    begin
      if LCh = CrChar then
        S := S + SpcChar
      else
        S := S + LCh;
      GetCh;
    end;
  end;
  if Ch = Term then
    GetCh; {pass termination ThtChar}
  Sy := SaveSy;
end;

{----------------GetNameValueParameter}

function GetNameValueParameter(var Name, Value: ThtString): boolean;
begin
  Result := False;
  SkipWhiteSpace;
  Name := '';
  if not (Ch in [ThtChar('A')..ThtChar('Z')]) then
    Exit;
  while Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('_'), ThtChar('0')..ThtChar('9')] do
  begin
    Name := Name + LCh;
    GetCh;
  end;

  SkipWhiteSpace;
  Value := '';
  Result := True; {at least have an ID}
  if Ch <> '=' then
    Exit;
  GetCh;

  SkipWhiteSpace;
  if not GetQuotedValue(Value) then
  {in case quotes left off ThtString}
    while not (Ch in [SpcChar, TabChar, CrChar, MinusChar, GreaterChar, EofChar]) do {need to exclude '-' to find '-->'}
    begin
      Value := Value + LCh;
      GetCh;
    end;
end;

{----------------GetValue}

function GetValue(var S: ThtString; var Value: integer): boolean;
{read a numeric.  Also reads a ThtString if it looks like a numeric initially}
var
  Code: integer;
  ValD: double;
begin
  Result := Ch in [ThtChar('-'), ThtChar('+'), ThtChar('0')..ThtChar('9')];
  if not Result then
    Exit;
  Value := 0;
  if Ch in [ThtChar('-'), ThtChar('+')] then
  begin
    S := Ch;
    GetCh;
  end
  else
    S := '';
  while not (Ch in [SpcChar, TabChar, CrChar, GreaterChar, PercentChar, EofChar]) do
    if LCh = AmperChar then
      S := S + GetEntityStr(PropStack.Last.CodePage)
    else
    begin
      S := S + LCh;
      GetCh;
    end;
  SkipWhiteSpace;
{see if a numerical value is appropriate.
 avoid the exception that happens when the likes of 'e1234' occurs}
  try
    Val(S, ValD, Code);
    Value := Round(ValD);
  except
  end;

  if LCh = '%' then
  begin
    S := S + '%';
    GetCh;
  end;
end;

{----------------GetQuotedStr}

function GetQuotedStr(var S: ThtString; var Value: integer; WantCrLf: boolean; Sym: Symb): boolean;
{get a quoted ThtString but strip the quotes, check to see if it is numerical}
var
  Term: ThtChar;
  S1: ThtString;
  Code: integer;
  ValD: double;
  SaveSy: Symb;
begin
  Result := False;
  Term := Ch;
  if (Term <> '"') and (Term <> '''') then
    Exit;
  Result := True;
  SaveSy := Sy;
  GetCh;
  while not (Ch in [Term, EofChar]) do
  begin
    if LCh <> CrChar then
    begin
      if LCh = AmperChar then
      begin
{$ifdef UNICODE}
{$else}
        if (Sym = ValueSy) and UnicodeControls then
          S := S + GetEntityStr(PropStack.Last.CodePage)
        else
{$endif}
          S := S + GetEntityStr(PropStack.Last.CodePage); //CP_ACP);
      end
      else
      begin
        S := S + LCh;
        GetCh;
      end;
    end
    else if WantCrLf then
    begin
      S := S + CrChar + ^J;
      GetCh;
    end
    else
      GetCh;
  end;
  if Ch = Term then
    GetCh; {pass termination ThtChar}
  S1 := Trim(S);
  if Pos('%', S1) = Length(S1) then
    SetLength(S1, Length(S1) - 1);
{see if S1 evaluates to a numerical value.  Note that something like
 S1 = 'e8196' can give exception because of the 'e'}
  Value := 0;
  if (Length(S1) > 0) and (S1[1] in [ThtChar('0')..ThtChar('9'), ThtChar('+'), ThtChar('-'), ThtChar('.')]) then
  try
    Val(S1, ValD, Code);
    Value := Round(ValD);
  except
  end;
  Sy := SaveSy;
end;

{----------------GetSomething}

procedure GetSomething(var S: ThtString);
begin
  while not (Ch in [SpcChar, TabChar, CrChar, GreaterChar, EofChar]) do
    if LCh = AmperChar then
      S := S + GetEntityStr(PropStack.Last.CodePage)
    else
    begin
      S := S + LCh;
      GetCh;
    end;
end;

{----------------GetID}

function GetID(var S: ThtString): boolean;

begin
  Result := False;
  if not (Ch in [ThtChar('A')..ThtChar('Z')]) then
    Exit;
  while Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('-'), ThtChar('0')..ThtChar('9')] do
  begin
    S := S + Ch;
    GetCh;
  end;
  Result := True;
end;

{----------------GetAttribute}

function GetAttribute(var Sym: Symb; var St: ThtString;
  var S: ThtString; var Val: integer): boolean;

const
  MaxAttr = 84;
  Attrib: array[1..MaxAttr] of ThtString =
  ('HREF', 'NAME', 'SRC', 'ALT', 'ALIGN', 'TEXT', 'BGCOLOR', 'LINK',
    'BACKGROUND', 'COLSPAN', 'ROWSPAN', 'BORDER', 'CELLPADDING',
    'CELLSPACING', 'VALIGN', 'WIDTH', 'START', 'VALUE', 'TYPE',
    'CHECKBOX', 'RADIO', 'METHOD', 'ACTION', 'CHECKED', 'SIZE',
    'MAXLENGTH', 'COLS', 'ROWS', 'MULTIPLE', 'VALUE', 'SELECTED',
    'FACE', 'COLOR', 'TRANSP', 'CLEAR', 'ISMAP', 'BORDERCOLOR',
    'USEMAP', 'SHAPE', 'COORDS', 'NOHREF', 'HEIGHT', 'PLAIN', 'TARGET',
    'NORESIZE', 'SCROLLING', 'HSPACE', 'LANGUAGE', 'FRAMEBORDER',
    'MARGINWIDTH', 'MARGINHEIGHT', 'LOOP', 'ONCLICK', 'WRAP', 'NOSHADE',
    'HTTP-EQUIV', 'CONTENT', 'ENCTYPE', 'VLINK', 'OLINK', 'ACTIVE',
    'VSPACE', 'CLASS', 'ID', 'STYLE', 'REL', 'REV', 'NOWRAP',
    'BORDERCOLORLIGHT', 'BORDERCOLORDARK', 'CHARSET', 'RATIO',
    'TITLE', 'ONFOCUS', 'ONBLUR', 'ONCHANGE', 'SPAN', 'TABINDEX',
    'BGPROPERTIES', 'DISABLED', 'TOPMARGIN', 'LEFTMARGIN', 'LABEL',
    'READONLY');
  AttribSym: array[1..MaxAttr] of Symb =
  (HrefSy, NameSy, SrcSy, AltSy, AlignSy, TextSy, BGColorSy, LinkSy,
    BackgroundSy, ColSpanSy, RowSpanSy, BorderSy, CellPaddingSy,
    CellSpacingSy, VAlignSy, WidthSy, StartSy, ValueSy, TypeSy,
    CheckBoxSy, RadioSy, MethodSy, ActionSy, CheckedSy, SizeSy,
    MaxLengthSy, ColsSy, RowsSy, MultipleSy, ValueSy, SelectedSy,
    FaceSy, ColorSy, TranspSy, ClearSy, IsMapSy, BorderColorSy,
    UseMapSy, ShapeSy, CoordsSy, NoHrefSy, HeightSy, PlainSy, TargetSy,
    NoResizeSy, ScrollingSy, HSpaceSy, LanguageSy, FrameBorderSy,
    MarginWidthSy, MarginHeightSy, LoopSy, OnClickSy, WrapSy, NoShadeSy,
    HttpEqSy, ContentSy, EncTypeSy, VLinkSy, OLinkSy, ActiveSy,
    VSpaceSy, ClassSy, IDSy, StyleSy, RelSy, RevSy, NoWrapSy,
    BorderColorLightSy, BorderColorDarkSy, CharSetSy, RatioSy,
    TitleSy, OnFocusSy, OnBlurSy, OnChangeSy, SpanSy, TabIndexSy,
    BGPropertiesSy, DisabledSy, TopMarginSy, LeftMarginSy, LabelSy,
    ReadonlySy);
var
  I: integer;
begin
  Sym := OtherAttribute;
  Result := False;
  SkipWhiteSpace;
  St := '';
  if GetID(St) then
  begin
    for I := 1 to MaxAttr do
      if St = Attrib[I] then
      begin
        Sym := AttribSym[I];
        Break;
      end;
  end
  else
    Exit; {no ID}
  SkipWhiteSpace;
  S := '';
  if Sym = BorderSy then
    Val := 1
  else
    Val := 0;
  Result := True; {at least have an ID}
  if Ch <> '=' then
    Exit;
  GetCh;

  SkipWhiteSpace;
  if not GetQuotedStr(S, Val, Sym in [TitleSy, AltSy], Sym) then {either it's a quoted ThtString or a number}
    if not GetValue(S, Val) then
      GetSomething(S); {in case quotes left off ThtString}
  if (Sym = IDSy) and (S <> '') and Assigned(PropStack.MasterList) and not LinkSearch then
    PropStack.MasterList.IDNameList.AddChPosObject(S, PropStack.SIndex);
end;

{-------------GetTag}

function GetTag: boolean; {Pick up a Tag or pass a single LessChar}
var
  Done, EndTag: Boolean;
  Compare: ThtString;
  SymStr: ThtString;
  AttrStr: ThtString;
  I: Integer;
  L: integer;
  Save: integer;
  Sym: Symb;
begin
  if Ch <> LessChar then
  begin
    Result := False;
    Exit;
  end
  else
    Result := True;
  Save := PropStack.SIndex;
  TagIndex := PropStack.SIndex;
  Compare := '';
  GetCh;
  if Ch = '/' then
  begin
    EndTag := True;
    GetCh;
  end
  else
  case Ch of
    'A'..'Z', '?':
      EndTag := False;

  else
    {an odd LessChar}
    Sy := TextSy;
    LCToken.AddUnicodeChar('<', Save);
    Exit;
  end;
  Sy := CommandSy;
  Done := False;
  while not Done do
    case Ch of
      'A'..'Z', '0'..'9', '/', '_':
        begin
          if (Ch = '/') and (Length(Compare) > 0) then {allow xhtml's <br/>, etc }
            Done := True
          else if Length(Compare) < 255 then
          begin
//              Inc(Compare[0]);
//              Compare[Length(Compare)] := Ch;
            Compare := Compare + Ch;
          end;
          GetCh;
          Done := Done or (Ch in [ThtChar('1')..ThtChar('6')]) and (Compare = 'H');
        end;
    else
      Done := True;
    end;
  for I := 1 to MaxRes do
    if Compare = ResWords[I] then
    begin
      if not EndTag then
        Sy := ResSy[I]
      else if I <= MaxEndRes then
        Sy := EndResSy[I]; {else Sy  := CommandSy}
      Break;
    end;
  SkipWhiteSpace;
  Value := 0;
  if ((Sy = HeadingSy) or (Sy = HeadingEndSy)) and (Ch in [ThtChar('1')..ThtChar('6')]) then
  begin
    Value := ord(Ch) - ord('0');
    GetCh;
  end;

  Attributes.Clear;
  while GetAttribute(Sym, SymStr, AttrStr, L) do
    Attributes.Add(TAttribute.Create(Sym, L, SymStr, AttrStr, PropStack.Last.Codepage));

  while (Ch <> GreaterChar) and (Ch <> EofChar) do
    GetCh;
  if not (Sy in [StyleSy, ScriptSy]) then {in case <!-- comment immediately follows}
    GetCh;
end;

function CollectText: boolean;
// Considers the current data as pure text and collects everything until
// the input end or one of the reserved tokens is found.
var
  SaveIndex: Integer;
  Buffer: TCharCollection;
  CodePage: Integer;
begin
  Sy := TextSy;
  CodePage := PropStack.Last.CodePage;
  Buffer := TCharCollection.Create;
  try
    Result := not (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar]);
    while not (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar]) do
    begin
      while LCh = AmperChar do
        GetEntity(LCToken, CodePage);

      // Get any normal text.
      repeat
        SaveIndex := PropStack.SIndex;
        // Collect all leading white spaces.
        if LCh in [SpcChar, CrChar, LfChar, TabChar] then
        begin
          if not LinkSearch then
            Buffer.Add(SpcChar, SaveIndex);
          // Skip other white spaces.
          repeat
            GetCh;
          until not (LCh in [SpcChar, CrChar, LfChar, TabChar]);
        end;
        // Collect any non-white space characters which are not special.
        while not (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar, AmperChar, SpcChar, CrChar, LfChar, TabChar]) do
        begin
          if not LinkSearch then
            Buffer.Add(LCh, PropStack.SIndex);
          GetCh;
        end;
      until (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar, AmperChar]);
      if Buffer.Size > 0 then
      begin
        LCToken.AddString(Buffer, CodePage);
        Buffer.Clear;
      end;
    end;

    // Flush any pending ANSI ThtString data.
    if Buffer.Size > 0 then
      LCToken.AddString(Buffer, CodePage);
  finally
    Buffer.Free;
  end;
end;

{-----------Next}

procedure Next;
  {Get the next token}
begin {already have fresh character loaded here}
  LCToken.Clear;
  if LCh = EofChar then
    Sy := EofSy
  else if not GetTag then
    if not CollectText then
      if LCh in [ThtChar(#0)..ThtChar(#8)] then
        LCh := '?';
end;

{ Add a TProperties to the PropStack. }
procedure PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProp: TProperties);
begin
  PropStack.PushNewProp(Tag, AClass, AnID, APseudo, ATitle, AProp);
end;

procedure PopProp;
{pop and free a TProperties from the Prop stack}
begin
  PropStack.PopProp;
end;

procedure PopAProp(const Tag: ThtString);
{pop and free a TProperties from the Prop stack.  It should be on top but in
 case of a nesting error, find it anyway}
//var
//  I, J: integer;
begin
//  for I := PropStackIndex downto 1 do
//    if PropStack[I].Proptag = Tag then
//    begin
//      if PropStack[I].GetBorderStyle <> bssNone then
//      {this would be the end of an inline border}
//        PropStack.MasterList.ProcessInlines(PropStack.SIndex, PropStack[I], False);
//      PropStack.Delete(I);
//      if I > 1 then {update any stack items which follow the deleted one}
//        for J := I to PropStackIndex do
//          PropStack[J].Update(PropStack[J - 1], PropStack.MasterList.Styles, J);
//      Break;
//    end;
  PropStack.PopAProp(Tag);
end;

procedure DoTextArea(TxtArea: TTextAreaFormControlObj);
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
      while not (LCh in [SpcChar, CrChar, TabChar, GreaterChar]) do
      begin
        Token := Token + LCh;
        GetCh;
      end;
      if CompareText(Token, '</textarea') = 0 then
        Sy := TextAreaEndSy
      else
        Sy := CommandSy; {anything else}
    end;

    function IsText1: boolean;
    begin
      while (Length(Token) < 100) and not (LCh in [CrChar, LessChar, AmperChar, EofChar]) do
      begin
        Token := Token + LCh;
        GetCh;
      end;
      if Length(Token) > 0 then
      begin
        Sy := TextSy;
        IsText1 := True;
      end
      else
        IsText1 := False;
    end;

  begin {already have fresh character loaded here}
    Token := '';
    LCToken.Clear;
    if LCh = EofChar then
      Sy := EofSy
    else if LCh = CrChar then
    begin
      Sy := EolSy;
      GetCh;
    end
    else if LCh = LessChar then
    begin GetTag1; Exit; end
    else if LCh = AmperChar then
    begin
{$ifdef UNICODE}
{$else}
      if UnicodeControls then
        Token := Token + GetEntityStr(PropStack.Last.CodePage)
      else
{$endif}
        Token := Token + GetEntityStr(PropStack.Last.CodePage); //CP_ACP);
      Sy := CommandSy;
    end
    else if IsText1 then
    else
    begin
      Sy := OtherChar;
      Token := LCh;
      GetCh;
    end;
  end;

begin
  Next1;
  S := '';
  while (Sy <> TextAreaEndSy) and (Sy <> EofSy) do
  begin
    case Sy of
      TextSy: S := S + Token;
      EolSy:
        begin
          S := S + CrChar + ^J;
          TxtArea.AddStr(S);
          S := '';
        end;
    else
      S := S + Token;
    end;
    Next1;
  end;
  while not (LCh in [GreaterChar, EofChar]) do
    GetCh; {remove chars to and past GreaterChar}
  GetCh;
  if S <> '' then
    TxtArea.AddStr(S);
  TxtArea.ResetToValue;
end;

function FindAlignment: ThtString; {pick up Align= attribute}
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

procedure CheckForAlign;
var
  S: ThtString;
begin
  S := FindAlignment;
  if S <> '' then
    PropStack.Last.Assign(S, TextAlign);
end;

type
  SymbSet = set of Symb;
const
  TableTermSet = [TableEndSy, TDSy, TRSy, TREndSy, THSy, THEndSy, TDEndSy,
    CaptionSy, CaptionEndSy, ColSy, ColgroupSy];

procedure DoBody(const TermSet: SymbSet); forward;

procedure DoLists(Sym: Symb; const TermSet: SymbSet); forward;

procedure DoAEnd; {do the </a>}
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

procedure DoDivEtc(Sym: Symb; const TermSet: SymbSet);
var
  FormBlock, DivBlock: TBlock;
  FieldsetBlock: TFieldsetBlock;
  IsFieldsetLegend: Boolean;
begin
  case Sym of
    DivSy:
      begin
        SectionList.Add(Section, TagIndex);
        PushNewProp('div', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
        CheckForAlign;

        DivBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
        SectionList.Add(DivBlock, TagIndex);
        SectionList := DivBlock.MyCell;

        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
        Next;
        DoBody([DivEndSy] + TermSet);
        SectionList.Add(Section, TagIndex);
        PopAProp('div');
        if SectionList.CheckLastBottomMargin then
        begin
          DivBlock.MargArray[MarginBottom] := ParagraphSpace;
          DivBlock.BottomAuto := True;
        end;
        SectionList := DivBlock.OwnerCell;

        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
        if Sy = DivEndSy then
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

        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
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

        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
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
    function FindColNum(Row: integer): integer;
    procedure AddCell(Row: integer; CellObj: TCellObj);
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

function TCellManager.FindColNum(Row: integer): integer;
{given the row of insertion, returns the column number where the next cell will
 go or -1 if out of range.  Columns beyond any <col> definitions are ignored}
begin
  if Row = Count then
    Add(StringOfChar('o', Table.ColInfo.Count));
  Result := Pos('o', Strings[Row]) - 1;
end;

procedure TCellManager.AddCell(Row: integer; CellObj: TCellObj);
{Adds this cell to the specified row}
var
  I, J, K, Span: integer;
  S1: ThtString;
begin
{make sure there's enough rows to handle any RowSpan for this cell}
  while Count < Row + CellObj.RowSpan do
    Add(StringOfChar('o', Table.ColInfo.Count));
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
        Dec(CellObj.ColSpan, Span);
    end;
end;


{----------------DoColGroup}

procedure DoColGroup(Table: ThtmlTable; ColOK: boolean);
{reads the <colgroup> and <col> tags.  Put the info in ThtmlTable's ConInfo list}
var
  I, Span: integer;
  xWidth, cWidth: integer;
  xAsPercent, cAsPercent: boolean;
  xVAlign, cVAlign: AlignmentType;
  xAlign, cAlign: ThtString;
  Algn: AlignmentType;

  procedure ReadColAttributes(var Width: integer; var AsPercent: boolean;
    var Valign: AlignmentType; var Align: ThtString; var Span: integer);
  var
    I: integer;
  begin
    for I := 0 to Attributes.Count - 1 do
      with TAttribute(Attributes[I]) do
        case Which of
          WidthSy:
            if Pos('%', Name) > 0 then
            begin
              if (Value > 0) and (Value <= 100) then
                Width := Value * 10;
              AsPercent := True;
            end
            else
              Width := Value;
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

begin
  xWidth := 0;
  xAsPercent := False;
  xVAlign := ANone;
  xAlign := '';
  if Sy = ColGroupSy then
  begin
    if ColOk then
      ReadColAttributes(xWidth, xAsPercent, xVAlign, xAlign, Span);
    SkipWhiteSpace;
    Next;
  end;
  while Sy = ColSy do
  begin
    if ColOK then
    begin
    {any new attributes in <col> will have priority over the <colgroup> items just read}
      cWidth := xWidth; {the default values}
      cAsPercent := xAsPercent;
      cVAlign := xVAlign;
      cAlign := xAlign;
      Span := 1;
      ReadColAttributes(cWidth, cAsPercent, cVAlign, cAlign, Span);
      for I := 1 to Min(Span, 100) do
        Table.DoColumns(cWidth, cAsPercent, cVAlign, cAlign);
    end;
    SkipWhiteSpace;
    Next;
  end;
  if Sy = ColGroupEndSy then
    Next;
end;

{----------------DoTable}

procedure DoTable;
var
  Table: ThtmlTable;
  SaveSectionList, JunkSaveSectionList: TCellBasic;
  SaveStyle: TFontStyles;
  SaveNoBreak: boolean;
  SaveListLevel: integer;
  RowVAlign, VAlign: AlignmentType;
  Row: TCellList;
  CellObj: TCellObj;
  T: TAttribute;
  RowStack: integer;
  NewBlock: TTableBlock;
  SetJustify: JustifyType;
  CM: TCellManager;
  CellNum: integer;
  TdTh: ThtString;
  ColOK: boolean;
  CaptionBlock: TBlock;
  CombineBlock: TTableAndCaptionBlock;
  TopCaption: boolean;
  RowType: TRowType;
  HFStack: integer;
  FootList: TList;
  I: integer;
  TrDisplay: TPropDisplay; // Yunqa.de.
  S: PropIndices;
  V: Variant;
  CellBorderStyle: BorderStyleType;

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
      else
{$IFDEF DebugIt}
        ShowMessage('Table cell error, ReadHTML.pas, DoTable')
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
        if RowType <> TFoot then
          Table.Rows.Add(Row)
        else
          FootList.Add(Row);
        Row.RowType := RowType;
      end
      else
        Row.Free;
      Row := nil;
      while PropStackIndex > RowStack do
        PopProp;
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
    SetJustify := centered
  else if PropStack.Last.Props[TextAlign] = 'right' then
    SetJustify := Right
  else
    SetJustify := NoJustify;
  PushNewProp('table', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
  Table := ThtmlTable.Create(PropStack.MasterList, Attributes, PropStack.Last);
  NewBlock := TTableBlock.Create(PropStack.MasterList, PropStack.Last,
    SaveSectionList, Table, Attributes, TableLevel);
  if (NewBlock.Justify <> Centered) and not (NewBlock.FloatLR in [ALeft, ARight]) then
    NewBlock.Justify := SetJustify;
  NewBlock.MyCell.Add(Table, TagIndex); {the only item in the cell}
  CombineBlock := TTableAndCaptionBlock.Create(PropStack.MasterList, PropStack.Last,
    SaveSectionList, Attributes, NewBlock); {will be needed if Caption found}
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
            ColOK := False; {no more <col> tags processed}
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
                with TColObj(Table.ColInfo[CellNum]) do
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
            if (Attributes.TheStyle = nil) and (Table.BorderWidth > 0) then
            begin
              if NewBlock.BorderStyle = bssOutset then
                CellBorderStyle := bssInset
              else if NewBlock.BorderStyle = bssInset then
                CellBorderStyle := bssOutset
              else
                CellBorderStyle := NewBlock.BorderStyle;

              for S := BorderTopStyle to BorderLeftStyle do
              begin
                V := PropStack.Last.Props[S];
                if (VarType(V) in varInt) and (V = IntNull) then
                  PropStack.Last.Props[S] := CellBorderStyle;
              end;
              for S := BorderTopWidth to BorderLeftWidth do
              begin
                V := PropStack.Last.Props[S];
                if (VarType(V) in varInt) and (V = IntNull) then
                  PropStack.Last.Props[S] := 1;
              end;
              for S := BorderTopColor to BorderLeftColor do
              begin
                V := PropStack.Last.Props[S];
                if (VarType(V) in varInt) and (V = IntNull) then
                  PropStack.Last.Props[S] := Table.BorderColor;
              end;
            end;
            CellObj := TCellObj.Create(PropStack.MasterList, VAlign, Attributes, PropStack.Last);
            SectionList := CellObj.Cell;
            if ((CellObj.WidthAttr = 0) or CellObj.AsPercent) and Attributes.Find(NoWrapSy, T) then
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
              CaptionBlock := TBlock.Create(PropStack.MasterList, PropStack.Last,
                SaveSectionList, Attributes);
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
          begin AddSection; Next; end;
        ColSy, ColGroupSy:
          begin
            DoColGroup(Table, ColOK);
            if not Assigned(CM) and Assigned(Table.ColInfo) then
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
      Table.Rows.Add(TCellList(FootList[I]));
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

procedure GetOptions(Select: TListBoxFormControlObj);
 {get the <option>s for Select form control}
var
  InOption, Selected: boolean;
  WS: WideString;
  SaveNoBreak: boolean;
  CodePage: integer;
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

procedure DoMap;
var
  Item: TMapItem;
  T: TAttribute;
  ErrorCnt: integer;
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

procedure DoScript(Ascript: TScriptEvent);
var
  Text: ThtString;

  procedure Next1;
    {Special Next routine to get the next token}

    procedure GetTag1; {simplified 'Pick up a Tag' routine}
    var
      Count: integer;
    begin
      Text := LessChar;
      GetCh;
      if not (Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('/')]) then
      begin
        Sy := TextSy;
        Exit;
      end;
      Sy := CommandSy; {catch all}
      while (Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('/')]) do
      begin
        Text := Text + LCh;
        GetCh;
      end;
      if CompareText(Text, '</script') = 0 then
        Sy := ScriptEndSy;
      Count := 0;
      while not (LCh in [GreaterChar, EofChar]) and (Count < 6) do
      begin
        if LCh = CrChar then
          Text := Text + SpcChar
        else
          Text := Text + LCh;
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
    if LCh = EofChar then
      Sy := EofSy
    else if LCh = CrChar then
    begin
      Sy := EolSy;
      GetCh;
    end
    else if LCh = LessChar then
      GetTag1
    else
    begin
      Sy := TextSy;
      while not (LCh in [CrChar, LessChar, EofChar]) do
      begin
        Text := Text + LCh;
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

procedure DoP(const TermSet: SymbSet); forward;
procedure DoBr(const TermSet: SymbSet); forward;

function DoObjectTag(var C: ThtChar; var N, IX: integer): boolean;
var
  WantPanel: boolean;
  SL, Params: ThtStringList;
  Prop: TProperties;
  PO: TPanelObj;
  S: ThtString;
  T: TAttribute;

  procedure SavePosition;
  begin
    C := LCh;
    N := Buff - PhtChar(DocS);
    IX := PropStack.SIndex;
  end;

  procedure Next1;
  begin
    SavePosition;
    Next;
  end;
begin
  Result := False;
  if Assigned(CallingObject) then
  begin
    if Assigned(ThtmlViewer(CallingObject).OnObjectTag) then
    begin
      SL := Attributes.CreateStringList;
      Result := True;
      if not Assigned(Section) then
        Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last,
          CurrentUrlTarget, SectionList, True);
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
        if Prop.GetBorderStyle <> bssNone then {start of inline border}
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

procedure DoCommonSy;
var
  I: integer;
  TxtArea: TTextAreaFormControlObj;
  FormControl: TFormControlObj;
  T: TAttribute;
  Tmp: ThtString;
  HeadingBlock: TBlock;
  HRBlock: THRBlock;
  HorzLine: THorzLine;
  HeadingStr, Link: ThtString;
  Done, FoundHRef: boolean;
  IO: TFloatingObj;
  Page: TPage;
  SaveSy: Symb;
  Prop: TProperties;
  C: ThtChar;
  N, IX: integer;

  procedure ChangeTheFont(Sy: Symb; Pre: boolean);
  var
    FaceName: ThtString;
    CharSet: TFontCharSet;
    CodePage: Integer;
    NewColor: TColor;
    NewSize, I: integer;
    FontResults: set of (Face, Colr, Siz, CharS);
    DNewSize: double;
    Prop: TProperties;
  begin
    FontResults := [];
    NewSize := 0; {get rid of warning}
    CodePage := CP_UNKNOWN;
    for I := 0 to Attributes.Count - 1 do
      with TAttribute(Attributes[I]) do
        case Which of
          SizeSy:
            begin
              if (Length(Name) >= 2) and (Name[1] in [ThtChar('+'), ThtChar('-')]) then
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
            if not IsUTF8 and TranslateCharSet(Name, CharSet, CodePage) then
              Include(FontResults, CharS);
        end;
    PushNewProp('font', Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
    Prop := TProperties(PropStack.Last);
    Prop.SetFontBG;
    if Prop.GetBorderStyle <> bssNone then {start of inline border}
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
      case CodePage of
        CP_UTF8:
        begin
          IsUTF8 := True;
          PropStack.Last.AssignUTF8;
        end;
      else
        PropStack.Last.AssignCharset(CharSet);
      end;
  end;

  procedure DoPreSy;
  var
    S: TokenObj;
    Tmp, Link: ThtString;
    Done, InForm, InP: boolean;
    I, InitialStackIndex: integer;
    PreBlock, FormBlock, PBlock: TBlock;
    SaveSy: Symb;
    FoundHRef: boolean;
    Prop: TProperties;
    C: ThtChar;
    N, IX: integer;
    Before, After, Intact: boolean;

    function CollectPreText: boolean;
    // Considers the current data as pure text and collects everything until
    // the input end or one of the reserved tokens is found.
    var
      Buffer: TCharCollection;
      CodePage: Integer;
    begin
      Sy := TextSy;
      CodePage := PropStack.Last.CodePage;
      Buffer := TCharCollection.Create;
      try
        Result := not (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar, CrChar]);
        while not (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar, CrChar]) do
        begin
          while LCh = AmperChar do {look for entities}
            GetEntity(S, CodePage);

        {Get any normal text, includein spaces}
          while not (LCh in [ThtChar(#1)..ThtChar(#8), EOFChar, LessChar, AmperChar, CrChar]) do
          begin
            Buffer.Add(LCh, PropStack.SIndex);
            GetCh;
          end;
          if Buffer.Size > 0 then
          begin
            S.AddString(Buffer, CodePage);
            Buffer.Clear;
          end;
        end;
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
        case Ch of
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
                    if Ch = CrChar then
                      GetCh;
                  end;
                PSy:
                  begin
                    if InP then
                      PEnd
                    else if S.Leng <> 0 then
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
                    if Ch = CrChar then
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

                BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
                  USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
                  SSy, SEndSy, StrikeSy, StrikeEndSy, SpanSy, SpanEndSy,
                  SubSy, SubEndSy, SupSy, SupEndSy, BigSy, BigEndSy, SmallSy, SmallEndSy,
                  LabelSy, LabelEndSy:
                  begin
                    Section.AddTokenObj(S);
                    S.Clear;
                    case Sy of
                      BSy, ISy, StrongSy, EmSy, CiteSy, VarSy, USy, SSy, StrikeSy, SpanSy,
                        SubSy, SupSy, BigSy, SmallSy, LabelSy:
                        begin
                          PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
                          Prop := TProperties(PropStack.Last);
                          Prop.SetFontBG;
                          if Prop.GetBorderStyle <> bssNone then {start of inline border}
                            PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
                        end;
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
                    if Prop.GetBorderStyle <> bssNone then {start of inline border}
                      PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
                    Section.ChangeFont(PropStack.Last);

                    if Attributes.Find(NameSy, T) then
                    begin
                      Tmp := UpperCase(T.Name);
                 {Author may have added '#' by mistake}
                      if (Length(Tmp) > 0) and (Tmp[1] = '#') then
                        Delete(Tmp, 1, 1);
                      PropStack.MasterList.IDNameList.AddChPosObject(Tmp, PropStack.SIndex);
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
                    N := Buff - PhtChar(DocS);
                    IX := PropStack.SIndex;
                    DoObjectTag(C, N, IX);
                    LCh := C;
                    Ch := htUpCase(LCh);
                    Buff := PhtChar(DocS) + N;
                    PropStack.SIndex := IX;
                    if Ch = CrChar then
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
                      GetOptions(FormControl as TListBoxFormControlObj);
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
          begin {all other chars}
            if not CollectPreText then
              GetCh;
          end;
        end;
      if InForm then
        FormEnd
      else
      begin
        Section.AddTokenObj(S);
        SectionList.Add(Section, TagIndex);
      end;
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
          if (LCToken.Leng >= 1) and (LCToken.S <> SpcChar) then
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
        if Prop.GetBorderStyle <> bssNone then {start of inline border}
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
          GetOptions(FormControl as TListBoxFormControlObj);
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
    BSy, BEndSy, ISy, IEndSy, StrongSy, StrongEndSy, EmSy, EmEndSy,
      CiteSy, CiteEndSy, VarSy, VarEndSy, USy, UEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy:
      begin
        case Sy of
          BSy, ISy, StrongSy, EmSy, CiteSy, VarSy, USy, SSy, StrikeSy:
            begin
              PushNewProp(SymbToStr(Sy), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
              Prop := TProperties(PropStack.Last);
              Prop.SetFontBG;
              if Prop.GetBorderStyle <> bssNone then {start of inline border}
                PropStack.MasterList.ProcessInlines(PropStack.SIndex, Prop, True);
            end;
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
              if Prop.GetBorderStyle <> bssNone then
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
              if Prop.GetBorderStyle <> bssNone then
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
        if Prop.GetBorderStyle <> bssNone then {start of inline border}
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
          PropStack.MasterList.IDNameList.AddChPosObject(Tmp, PropStack.SIndex);
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
      if (Value in [1..6]) then
      begin
        SectionList.Add(Section, TagIndex);
        HeadingStr := 'h' + IntToStr(Value);
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
            DivSy: DoDivEtc(DivSy, [HeadingEndSy]);
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

procedure DoP(const TermSet: SymbSet);
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
    (Sy in [TextSy, NoBrSy, NoBrEndSy, WbrSy, BSy, ISy, BEndSy, IEndSy,
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

procedure DoBr(const TermSet: SymbSet);
var
  T: TAttribute;
  Before, After, Intact: boolean;
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

procedure DoListItem(var LiBlock: TBlockLi; var LiSection: TSection; BlockType, Sym: Symb; LineCount: integer; Index: ThtChar; Plain: boolean; const TermSet: SymbSet);
var
  Done: boolean;
  //LiBlock: TBlock;
  //LISection: TSection;
  IsInline: Boolean;
  IsFirst: Boolean;
begin
  PushNewProp(SymbToStr(Sym), Attributes.TheClass, Attributes.TheID, '', Attributes.TheTitle, Attributes.TheStyle);
  IsInline := PropStack.Last.Display = pdInline;
  IsFirst := not IsInline or (LiSection = nil) or not (LiSection.Display = pdInline);
  if IsFirst then
  begin
    SectionList.Add(Section, TagIndex);
    LiBlock := TBlockLI.Create(PropStack.MasterList, PropStack.Last, SectionList, BlockType, Plain, Index, LineCount, ListLevel, Attributes);
    SectionList.Add(LiBlock, TagIndex);
    LiSection := TSection.Create(PropStack.MasterList, nil, PropStack.Last, CurrentUrlTarget, LiBlock.MyCell, True);
  end;
  SectionList := LiBlock.MyCell;
  Section := LISection;

  SkipWhiteSpace;
  Next;
  Done := False;
  while not Done do {handle second part like after a <p>}
    case Sy of
      TextSy, NoBrSy, NoBrEndSy, WbrSy, BSy, ISy, BEndSy, IEndSy,
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
          Done := True; {else terminate lone <li>s on <p>}
      PEndSy: Next;
      DivSy, CenterSy, FormSy, AddressSy, BlockquoteSy, FieldsetSy:
        DoDivEtc(Sy, TermSet);
      OLSy, ULSy, DirSy, MenuSy, DLSy:
        begin
          DoLists(Sy, TermSet);
          LiBlock.MyCell.CheckLastBottomMargin;
          Next;
        end;
      CommandSy: Next;
      TableSy: DoTable;
    else
      Done := True;
    end;

  if Assigned(Section) and (Section = LISection) and (Section.Len = 0) then
    Section.AddChar(WideChar(160), TagIndex); {so that bullet will show on blank <li>}
  if not IsInline then
  begin
    SectionList.Add(Section, TagIndex);
    Section := nil;
  end;
  SectionList.CheckLastBottomMargin;
  PopAProp(SymbToStr(Sym));
  SectionList := LiBlock.OwnerCell;
  if not IsInline then
  begin
    LiBlock := nil;
    LiSection := nil;
  end;
end;

{-------------DoLists}

procedure DoLists(Sym: Symb; const TermSet: SymbSet);
var
  T: TAttribute;
  LineCount: integer;
  Plain: boolean;
  Index: ThtChar;
  NewBlock: TBlock;
  EndSym: Symb;
  LiBlock: TBlockLi;
  LiSection: TSection;
begin
  LiBlock := nil;
  LiSection := nil ;
  LineCount := 1;
  Index := '1';
  EndSym := EndSymbFromSymb(Sym);
  Plain := False;
  if (Sym = OLSy) then
  begin
    if Attributes.Find(StartSy, T) then
      if T.Value >= 0 then
        LineCount := T.Value;
    if Attributes.Find(TypeSy, T) and (T.Name <> '') then
      Index := T.Name[1];
  end
  else if Sym = ULSy then
    Plain := Attributes.Find(PlainSy, T) or (Attributes.Find(TypeSy, T) and
      ((Lowercase(T.Name) = 'none') or (Lowercase(T.Name) = 'plain')));
  SectionList.Add(Section, TagIndex);
  Section := nil;
  PushNewProp(SymbToStr(Sym), Attributes.TheClass, Attributes.TheID, '',
    Attributes.TheTitle, Attributes.TheStyle);

  NewBlock := TBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
  NewBlock.IsListBlock := not (Sym in [AddressSy, BlockquoteSy, DLSy]);
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
          DoListItem(LiBlock, LiSection, Sym, Sy, LineCount, Index, Plain, TermSet);
          Inc(LineCount);
        end;
      OLSy, ULSy, DirSy, MenuSy, DLSy:
        begin
          DoLists(Sy, TermSet);
          if not (Sy in TermSet) then
            Next;
        end;
      PSy: DoP(TermSet);
      BlockQuoteSy, AddressSy:
        DoDivEtc(Sy, TermSet);
      DivSy, CenterSy, FormSy:
        DoDivEtc(Sy, [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy,
          LISy, DDSy, DTSy, EofSy] + TermSet);

      TextSy, BRSy, HRSy, TableSy,
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
    else if Sy in TermSet then {exit below}
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

procedure DoBase;
var
  I: integer;
begin
  with Attributes do
    for I := 0 to Count - 1 do
      with TAttribute(Attributes[I]) do
        if Which = HrefSy then
          Base := Name
        else if Which = TargetSy then
          BaseTarget := Name;
  Next;
end;

{----------------DoSound}

procedure DoSound;
var
  Loop: integer;
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

{$ifdef UseUnicode}
{$else}

{----------------IsIso2022JP:}

function IsIso2022JP: boolean;
{look for iso-2022-jp Japanese file}
var
  I, J, K, L: integer;
begin
  Result := False;
  I := Pos(#$1b'$@', DocS); {look for starting sequence}
  J := Pos(#$1b'$B', DocS);
  I := Max(I, J); {pick a positive value}
  if I > 0 then
  begin {now look for ending sequence after the start}
    K := PosX(#$1b'(J', DocS, I);
    L := PosX(#$1b'(B', DocS, I);
    K := Max(K, L); {pick a positive value}
    if K > 0 then {start and end sequence found}
      Result := True;
  end;
end;

function EUCToShiftJis(const E: string): string;
var
  i, j, k, s, t: integer;
  WhichByte: 0..2;
begin
  Result := '';
  WhichByte := 0;
  j := 0; {prevent warning}
  for I := 1 to Length(E) do
    if Ord(E[I]) <= $A0 then
    begin
      WhichByte := 0;
      Result := Result + E[I];
    end
    else if WhichByte in [0, 2] then
    begin {first byte}
      WhichByte := 1;
      j := ord(E[I]) and $7F; {-128}
      if (j in [33..96]) then
        s := (j + 1) div 2 + 112
      else
        s := (j + 1) div 2 + 176;
      Result := Result + ThtChar(s);
    end
    else
    begin {second byte}
      WhichByte := 2;
      k := ord(E[I]) and $7F; {-128}
      if odd(j) then
      begin
        t := k + 31;
        if k > 95 then
          inc(t);
      end
      else
        t := k + 126;
      Result := Result + ThtChar(t);
    end;
end;

function JISToShiftJis(const E: string): string;
var
  i, j, k, s, t, Len: integer;
  WhichByte: 0..2;
  C: Char;
begin
  Len := Length(E);
  i := 1;
  WhichByte := 0;
  j := 0; {prevent warning}
  Result := '';
  while i <= Len do
  begin
    C := chr(ord(E[i]) and $7F);
    if (C = chr($1B)) and (i <= Len - 2) then
    begin
      if (E[I + 1] = '(') and (E[i + 2] in ['B', 'J']) then
      begin
        WhichByte := 0;
        Inc(I, 3);
        Continue;
      end
      else if (E[I + 1] = '$') and (E[I + 2] in ['@', 'B']) then
      begin
        WhichByte := 1;
        Inc(I, 3);
        Continue;
      end;
    end;
    case WhichByte of
      0: Result := Result + C;
      1:
        begin
          j := ord(C) and $7F; {and $7F just for safety}
          if (j in [33..96]) then
            s := (j + 1) div 2 + 112
          else
            s := (j + 1) div 2 + 176;
          Result := Result + Char(s);
          WhichByte := 2;
        end;
      2:
        begin
          k := ord(C) and $7F; {and $7F just for safety}
          if odd(j) then
          begin
            t := k + 31;
            if k > 95 then
              inc(t);
          end
          else
            t := k + 126;
          Result := Result + Char(t);
          WhichByte := 1;
        end;
    end;
    Inc(i);
  end;
end;
{$endif}

function TranslateCharset(const Content: ThtString; var Charset: TFontCharset; var CodePage: Integer): boolean;
var
  Info: TBuffCharSetCodePageInfo;
{$ifdef UseUnicode}
{$else}
  N: Integer;
{$endif}
begin
  Info := GetCharSetCodePageInfo(Content);
  Result := Info <> nil;
  if Result then
  begin
    Charset := Info.CharSet;
    CodePage := Info.CodePage;
{$ifdef UseUnicode}
{$else}
    if CharSet = EUCJP_CharSet then
    begin
      if not HaveTranslated then
      begin
        N := Buff - PhtChar(DocS);
        DocS := EUCToShiftJis(DocS); {translate to ShiftJis}
        Buff := PhtChar(DocS) + N; {DocS probably moves}
        BuffEnd := PhtChar(Docs) + Length(DocS);
        HaveTranslated := True;
      end;
      CharSet := SHIFTJIS_CHARSET;
    end;
{$endif}
  end;
end;

{----------------DoMeta}

procedure DoMeta(Sender: TObject);
var
  T: TAttribute;
  HttpEq, Name, Content: ThtString;
  CharSet: TFontCharset;
  CodePage: Integer;
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
  else
    Content := '';
  if not IsUTF8 and (Sender is ThtmlViewer) and (CompareText(HttpEq, 'content-type') = 0) then
  begin
    if TranslateCharset(Content, CharSet, CodePage) then
      case CodePage of
        CP_UTF8:
          PropStack.Last.AssignUTF8;
      else
        PropStack.Last.AssignCharset(Charset);
      end;
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

procedure DoTitle;
begin
  Title := '';
  Next;
  while Sy = TextSy do
  begin
    Title := Title + LCToken.S;
    Next;
  end;
end;

var
  slS: ThtString;
  slI: integer;

function slGet: ThtChar;

function Get: ThtChar;
  begin
    if slI <= Length(slS) then
    begin
      Result := slS[slI];
      Inc(slI);
    end
    else
      Result := EofChar;
  end;
begin
  repeat
    Result := Get;
  until (Result <> ^J);
  if Result = TabChar then
    Result := SpcChar;
end;

procedure DoStyleLink; {handle <link> for stylesheets}
var
  Style: ThtString;
  C: ThtChar;
  I: integer;
  Url, Rel, Rev: ThtString;
  OK: boolean;
  Request: TGetStreamEvent;
  RStream: TMemoryStream;
  Viewer: ThtmlViewer;
  Path: ThtString;
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
        RevSy: Rev := Name;
        HRefSy:
          Url := Name;
      end;
  if OK and (Url <> '') then
  begin
    Style := '';
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
          if Assigned(RStream) then
            Style := LoadStringFromStream(RStream);
        end
        else
        begin
          Path := ''; {for TFrameViewer requests, don't know path}
          Request(Viewer, Url, RStream);
          if Assigned(RStream) then
            Style := LoadStringFromStream(RStream)
          else
          begin {try it as a file}
            Url := Viewer.HTMLExpandFilename(Url);
            Path := ExtractFilePath(Url);
            if FileExists(Url) then
              Style := LoadStringFromFile(Url);
          end;
        end;
      end
      else {assume it's a file}
      begin
        Url := Viewer.HTMLExpandFilename(Url);
        Path := ExtractFilePath(Url);
        Style := LoadStringFromFile(Url);
      end;
      if Style <> '' then
      begin
        slS := AdjustLineBreaks(Style); {put in uniform CRLF format}
        slI := 1;
        C := slGet;
        DoStyle(PropStack.MasterList.Styles, C, slGet, Path, True);
      end;
      slS := '';
    except
      slS := '';
    end;
  end;
  if Assigned(LinkEvent) then
    LinkEvent(CallingObject, Rel, Rev, Url);
  Next;
end;

{-------------DoBody}

procedure DoBody(const TermSet: SymbSet);
var
  I: integer;
  Val: TColor;
  AMarginHeight, AMarginWidth: integer;
  LiBlock: TBlockLi;
  LiSection: TSection;
begin
  LiBlock := nil;
  LiSection := nil;
  repeat
    if Sy in TermSet then
      Exit;
    case Sy of
      TextSy, BRSy, HRSy,
        NameSy, HRefSy, ASy, AEndSy,
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
{$IFDEF Quirk}
            PropStack.MasterList.Styles.FixupTableColor(PropStack.Last);
{$ENDIF}
            PropStack.Last.Assign(AMarginWidth, MarginLeft);
            PropStack.Last.Assign(AMarginWidth, MarginRight);
            PropStack.Last.Assign(AMarginHeight, MarginTop);
            PropStack.Last.Assign(AMarginHeight, MarginBottom);

            SectionList := BodyBlock.OwnerCell;
            SectionList.Remove(BodyBlock);
            BodyBlock := TBodyBlock.Create(PropStack.MasterList, PropStack.Last, SectionList, Attributes);
            SectionList.Add(BodyBlock, TagIndex);
            SectionList := BodyBlock.MyCell;

            Section := TSection.Create(PropStack.MasterList, nil, PropStack.Last, nil, SectionList, True);
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
        DoListItem(LiBlock, LiSection, LiAloneSy, Sy, 1, '1', False, TermSet);

      DDSy, DTSy:
        DoListItem(LiBlock, LiSection, DLSy, Sy, 1, '1', False, TermSet);

      PSy: DoP(TermSet);

      FormEndSy:
        begin
          CurrentForm := nil;
          Next;
        end;

      DivSy, CenterSy, FormSy, BlockQuoteSy, AddressSy, FieldsetSy, LegendSy:
        DoDivEtc(Sy, TermSet);

      TitleSy:
        DoTitle;

      LinkSy:
        DoStyleLink;

      StyleSy:
        begin
          DoStyle(PropStack.MasterList.Styles, LCh, GetChBasic, '', False);
          Ch := htUpCase(LCh); {LCh is returned so next ThtChar is available}
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

procedure DoFrameSet(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: ThtString);
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
  FrameViewer.EndFrameSet(FrameSet);
end;

{----------------ParseInit}

procedure ParseInit(ASectionList: TList; AIncludeEvent: TIncludeType);
const
  NullsAllowed = 100;
var
  I, Num: integer;
begin
  LoadStyle := lsString;
  SectionList := TSectionList(ASectionList);
  PropStack.MasterList := TSectionList(SectionList);
  CallingObject := TSectionList(ASectionList).TheOwner;
  IncludeEvent := AIncludeEvent;
  PropStack.Clear;
  PropStack.Add(TProperties.Create);
  PropStack[0].CopyDefault(PropStack.MasterList.Styles.DefProp);
  PropStack.SIndex := -1;

  HaveTranslated := False;
  IsUTF8 := False;
  Num := 0;
  I := Pos(#0, DocS);
  while (I > 0) and (Num < NullsAllowed) do
  begin {be somewhat forgiving if there are a few nulls}
    DocS[I] := SpcChar;
    I := Pos(#0, DocS);
    Inc(Num);
  end;
  if I > 0 then
    SetLength(DocS, I - 1); {file has a problem, too many Nulls}
{$ifdef UseUnicode}
{$else}
{look for UTF-8 marker}
  if (Length(DocS) > 3) and (DocS[1] = #$EF) and (DocS[2] = #$BB) and (DocS[3] = #$BF) then
  begin
    PropStack[0].AssignUTF8;
    Delete(DocS, 1, 3);
    PropStack.SIndex := 2;
    IsUTF8 := True;
  end
  else
  {look for iso-2022-jp Japanese file} if IsIso2022Jp then
    begin
      DocS := JISToShiftJis(DocS); {watch it, changes PhtChar(DocS) }
      HaveTranslated := True;
      PropStack[0].AssignCharSet(ShiftJIS_CharSet);
    end;
{$endif}    
  if CallingObject is ThtmlViewer then
    ThtmlViewer(CallingObject).CodePage := PropStack[0].CodePage;
  Buff := PhtChar(DocS);
  BuffEnd := Buff + Length(DocS);
  IBuff := nil;

  BodyBlock := TBodyBlock.Create(PropStack.MasterList, PropStack[0], SectionList, nil);
  SectionList.Add(BodyBlock, TagIndex);
  SectionList := BodyBlock.MyCell;

  CurrentURLTarget := TUrlTarget.Create;
  InHref := False;
  BaseFontSize := 3;

  Title := '';
  Base := '';
  BaseTarget := '';
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

{----------------ParseHTMLString}

procedure ParseHTMLString(const S: ThtString; ASectionList: TList;
  AIncludeEvent: TIncludeType;
  ASoundEvent: TSoundType; AMetaEvent: TMetaType; ALinkEvent: TLinkType);
{$IFNDEF NoTabLink}
const
  MaxTab = 400; {maximum number of links before tabbing of links aborted}
var
  TabCount, SaveSIndex: integer;
  T: TAttribute;
{$ENDIF}
begin
  DocS := S;
  ParseInit(ASectionList, nil);

  try
{$IFNDEF NoTabLink}
    SaveSIndex := PropStack.SIndex;
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
      TSectionList(ASectionList).StopTab := TabCount > MaxTab;
    except
    end;
  {reset a few things}
    PropStack.SIndex := SaveSIndex;
    Buff := PhtChar(DocS);
{$ENDIF}

    LinkSearch := False;
    SoundEvent := ASoundEvent;
    MetaEvent := AMetaEvent;
    LinkEvent := ALinkEvent;
    IncludeEvent := AIncludeEvent;
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
    PropStack.Clear;
    CurrentURLTarget.Free;
    DocS := '';
  end; {finally}
end;

{----------------DoText}

procedure DoText;
var
  S: TokenObj;
  Done: boolean;
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
      case Ch of
        ^M:
          begin NewSection; GetCh; end;
        #0: Done := True;
      else
        begin {all other chars}
          S.AddUnicodeChar(WideChar(LCh), PropStack.SIndex);
          if S.Leng > 200 then
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

{----------------ParseTextString}

procedure ParseTextString(const S: ThtString; ASectionList: TList);
begin
  DocS := S;
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
end;

{-------------FrameParseString}

procedure FrameParseString(FrameViewer: TFrameViewerBase; FrameSet: TObject;
  ALoadStyle: LoadStyleType; const FName, S: ThtString; AMetaEvent: TMetaType);

  procedure Parse;
  var
    SetExit: boolean;
  begin
    SetExit := False;
    PropStack.Clear;
    PropStack.Add(TProperties.Create);
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
  PropStack.MasterList := nil;
  if (ALoadStyle <> lsFile) and (S = '') then
    Exit;
  CallingObject := FrameViewer;
  IncludeEvent := FrameViewer.OnInclude;
  SoundEvent := FrameViewer.OnSoundRequest;
  MetaEvent := AMetaEvent;
  LinkEvent := FrameViewer.OnLink;
  Attributes := TAttributeList.Create;
  Title := '';
  Base := '';
  BaseTarget := '';
  InScript := False;
  NoBreak := False;
  InComment := False;
  ListLevel := 0;

  try
    if ALoadStyle = lsFile then
      DocS := LoadStringFromFile(FName)
    else
      DocS := S;
    LoadStyle := lsString;
  {look for iso-2022-jp Japanese file}
{$ifdef UseUnicode}
{$else}
    if IsIso2022Jp then
    begin
      DocS := JISToShiftJis(DocS);
      HaveTranslated := True;
    end
    else
      HaveTranslated := False;
{$endif}

    Buff := PhtChar(DocS);
    BuffEnd := Buff + Length(DocS);
    try
      Parse;                                          
    except {ignore error}
      on E: Exception do
        Assert(False, E.Message);
    end;
  finally
    Attributes.Free;
    DocS := '';
  end;
end;

{----------------IsFrameString}

function IsFrameString(ALoadStyle: LoadStyleType; const FName, S: ThtString;
  FrameViewer: TFrameViewerBase): boolean;

  function Parse: boolean;
  var
    SetExit: boolean;
  begin
    Result := False;
    PropStack.Clear;
    PropStack.Add(TProperties.Create);
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
    PropStack.Clear;
  end;

begin
  PropStack.MasterList := nil;
  LoadStyle := lsString;
  Result := False;
  if (ALoadStyle <> lsFile) and (S = '') then
    Exit;
  CallingObject := FrameViewer;
  SoundEvent := nil;
  Attributes := TAttributeList.Create;
  Title := '';
  Base := '';
  BaseTarget := '';
  Result := False;
  InScript := False;
  NoBreak := False;
  InComment := False;

  try
    if ALoadStyle = lsFile then
      Docs := LoadStringFromFile(FName)
    else
      DocS := S;
    LoadStyle := lsString;
    Buff := PhtChar(DocS);
    BuffEnd := Buff + Length(DocS);
    try
      Result := Parse;
    except {ignore error}
    end;
  finally
    Attributes.Free;
    DocS := '';
  end;
end;

{----------------GetEntity}

procedure GetEntity(T: TokenObj; CodePage: integer);
var
  I, N: Integer;
  SaveIndex: Integer;
  Buffer,
    Entity: TCharCollection;
  X: ThtChar;

  procedure AddNumericChar(I: Integer; ForceUnicode: Boolean);
  // Adds the given value as new ThtChar to the buffer.
  begin
    // If the given value is less than 256 then it is considered as a character which
    // must be converted to Unicode, otherwise it is already a Unicode character.
    if I = 9 then
      Buffer.Add(SpcChar, SaveIndex)
    else if I < ord(SpcChar) then {control ThtChar}
      Buffer.Add('?', SaveIndex) {is there an error symbol to use here?}
    else if (I >= 127) and (I <= 159) and not ForceUnicode then
      Buffer.Add(ThtChar(I), SaveIndex) {127 to 159 not valid Unicode}
    else
    begin
      // Unicode character. Flush any pending ANSI ThtString data before storing it.
      if Buffer.Size > 0 then
      begin
        T.AddString(Buffer, CodePage);
        Buffer.Clear;
      end;
      T.AddUnicodeChar(WideChar(I), SaveIndex);
    end;
  end;

begin
  Buffer := TCharCollection.Create;
  try
    begin
    // A mask character. This introduces special characters and must be followed
    // by a '#' ThtChar or one of the predefined (named) entities.
      SaveIndex := PropStack.SIndex;
      GetCh;
      case LCh of
        '#': // Numeric value.
          begin
            GetCh;
            if Ch = 'X' then
            begin
            // Hex digits given.
              X := LCh; {either 'x' or 'X', save in case of error}
              GetCh;
              if Ch in [ThtChar('A')..ThtChar('F'), ThtChar('0')..ThtChar('9')] then
              begin
                I := 0;
                while Ch in [ThtChar('A')..ThtChar('F'), ThtChar('0')..ThtChar('9')] do
                begin
                  if Ch in [ThtChar('0')..ThtChar('9')] then
                    I := 16 * I + (Ord(Ch) - Ord('0'))
                  else
                    I := 16 * I + (Ord(Ch) - Ord('A') + 10);
                  GetCh;
                end;
                AddNumericChar(I, False);
              // Skip the trailing semicolon.
                if Ch = ';' then
                  GetCh;
              end
              else
              begin
                Buffer.Add(AmperChar, SaveIndex);
                Buffer.Add(X, SaveIndex + 1);
              end;
            end
            else
            begin
            // Decimal digits given.
              if Ch in [ThtChar('0')..ThtChar('9')] then
              begin
                I := 0;
                while Ch in [ThtChar('0')..ThtChar('9')] do
                begin
                  I := 10 * I + (Ord(Ch) - Ord('0'));
                  GetCh;
                end;
                AddNumericChar(I, False);
              // Skip the trailing semicolon.
                if Ch = ';' then
                  GetCh;
              end
              else
                Buffer.Add(AmperChar, SaveIndex);
            end;
          end;
      else
        // Must be a predefined (named) entity.
        Entity := TCharCollection.Create;
        try
          N := 0;
          // Pick up the entity name.
          while (Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('0')..ThtChar('9')]) and (N <= 10) do
          begin
            Entity.Add(LCh, PropStack.SIndex);
            GetCh;
            Inc(N);
          end;
          // Now convert entity ThtString into a character value. If there is no
          // entity with that name simply add all characters as they are.
          if Entities.Find(Entity.AsString, I) then
          begin
            AddNumericChar(Integer(Entities.Objects[I]), True);
            // Advance current pointer to first character after the semicolon.
            if Ch = ';' then
              GetCh;
          end
          else
          begin
            Buffer.Add(AmperChar, SaveIndex);
            Buffer.Concat(Entity);
          end;
        finally
          Entity.Free;
        end;
      end; {case}
    end; {while}
    if Buffer.Size > 0 then
      T.AddString(Buffer, CodePage);
  finally
    Buffer.Free;
  end;
end;

function GetEntityStr(CodePage: integer): ThtString;
{read an entity and return it as a ThtString.}
var
  I, N: Integer;
  Collect,
    Entity: ThtString;

  procedure AddNumericChar(I: Integer; ForceUnicode: Boolean);
  // Adds the given value as new ThtChar to the ThtString.
  var
    W: WideChar;
    Buffer: array[0..10] of ThtChar;
  begin
    if I = 9 then
      Result := SpcChar
    else if I < ord(SpcChar) then {control ThtChar}
      Result := '?' {is there an error symbol to use here?}
    else if (I >= 127) and (I <= 159) and not ForceUnicode then
      Result := Chr(I)
    else
    begin
    // Unicode character, Convert to this Code Page.
      W := WideChar(I);
      SetString(Result, Buffer, WideCharToMultiByte(CodePage, 0,
        @W, 1, @Buffer, SizeOf(Buffer), nil, nil))
    end;
  end;

  procedure NextCh;
  begin
    Collect := Collect + LCh;
    GetCh;
  end;

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
          if Ch = 'X' then
          begin
          // Hex digits given.
            NextCh;
            if Ch in [ThtChar('A')..ThtChar('F'), ThtChar('0')..ThtChar('9')] then
            begin
              I := 0;
              while Ch in [ThtChar('A')..ThtChar('F'), ThtChar('0')..ThtChar('9')] do
              begin
                if Ch in [ThtChar('0')..ThtChar('9')] then
                  I := 16 * I + (Ord(Ch) - Ord('0'))
                else
                  I := 16 * I + (Ord(Ch) - Ord('A') + 10);
                GetCh;
              end;
              AddNumericChar(I, False);
            // Skip the trailing semicolon.
              if Ch = ';' then
                NextCh;
            end
            else
              Result := Collect;
          end
          else
          begin
          // Decimal digits given.
            if Ch in [ThtChar('0')..ThtChar('9')] then
            begin
              I := 0;
              while Ch in [ThtChar('0')..ThtChar('9')] do
              begin
                I := 10 * I + (Ord(Ch) - Ord('0'));
                NextCh;
              end;
              AddNumericChar(I, False);
            // Skip the trailing semicolon.
              if Ch = ';' then
                NextCh;
            end
            else
              Result := Collect;
          end;
        end;
    else
      // Must be a predefined (named) entity.
      Entity := '';
      N := 0;
      // Pick up the entity name.
      while (Ch in [ThtChar('A')..ThtChar('Z'), ThtChar('0')..ThtChar('9')]) and (N <= 10) do
      begin
        Entity := Entity + LCh;
        NextCh;
        Inc(N);
      end;

      // Now convert entity ThtString into a character value. If there is no
      // entity with that name simply add all characters as they are.
      if Entities.Find(Entity, I) and ((Ch = ';') or (Integer(Entities.Objects[I]) <= 255)) then
      begin
        AddNumericChar(Integer(Entities.Objects[I]), True);
        // Advance current pointer to first character after the semicolon.
        if Ch = ';' then
          NextCh;
      end
      else
        Result := Collect;
    end; {case}
  end; {while}
end;

// Taken from http://www.w3.org/TR/REC-html40/sgml/entities.html.

type
  TEntity = record
    Name: ThtString;
    Value: WideChar;
  end;

const
  EntityCount = 253;
  // Note: the entities will be sorted into a ThtString list to make binary search possible.
  EntityDefinitions: array[0..EntityCount - 1] of TEntity = (
    // ISO 8859-1 characters
    (Name: 'nbsp'; Value: #160), // no-break space = non-breaking space, U+00A0 ISOnum
    (Name: 'iexcl'; Value: #161), // inverted exclamation mark, U+00A1 ISOnum
    (Name: 'cent'; Value: #162), // cent sign, U+00A2 ISOnum
    (Name: 'pound'; Value: #163), // pound sign, U+00A3 ISOnum
    (Name: 'curren'; Value: #164), // currency sign, U+00A4 ISOnum
    (Name: 'yen'; Value: #165), // yen sign = yuan sign, U+00A5 ISOnum
    (Name: 'brvbar'; Value: #166), // broken bar = broken vertical bar, U+00A6 ISOnum
    (Name: 'sect'; Value: #167), // section sign, U+00A7 ISOnum
    (Name: 'uml'; Value: #168), // diaeresis = spacing diaeresis, U+00A8 ISOdia
    (Name: 'copy'; Value: #169), // copyright sign, U+00A9 ISOnum
    (Name: 'ordf'; Value: #170), // feminine ordinal indicator, U+00AA ISOnum
    (Name: 'laquo'; Value: #171), // left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum
    (Name: 'not'; Value: #172), // not sign, U+00AC ISOnum
    (Name: 'shy'; Value: #173), // soft hyphen = discretionary hyphen, U+00AD ISOnum
    (Name: 'reg'; Value: #174), // registered sign = registered trade mark sign, U+00AE ISOnum
    (Name: 'macr'; Value: #175), // macron = spacing macron = overline = APL overbar, U+00AF ISOdia
    (Name: 'deg'; Value: #176), // degree sign, U+00B0 ISOnum
    (Name: 'plusmn'; Value: #177), // plus-minus sign = plus-or-minus sign, U+00B1 ISOnum
    (Name: 'sup2'; Value: #178), // superscript two = superscript digit two = squared, U+00B2 ISOnum
    (Name: 'sup3'; Value: #179), // superscript three = superscript digit three = cubed, U+00B3 ISOnum
    (Name: 'acute'; Value: #180), // acute accent = spacing acute, U+00B4 ISOdia
    (Name: 'micro'; Value: #181), // micro sign, U+00B5 ISOnum
    (Name: 'para'; Value: #182), // pilcrow sign = paragraph sign, U+00B6 ISOnum
    (Name: 'middot'; Value: #183), // middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum
    (Name: 'cedil'; Value: #184), // cedilla = spacing cedilla, U+00B8 ISOdia
    (Name: 'sup1'; Value: #185), // superscript one = superscript digit one, U+00B9 ISOnum
    (Name: 'ordm'; Value: #186), // masculine ordinal indicator, U+00BA ISOnum
    (Name: 'raquo'; Value: #187), // right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum
    (Name: 'frac14'; Value: #188), // vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum
    (Name: 'frac12'; Value: #189), // vulgar fraction one half = fraction one half, U+00BD ISOnum
    (Name: 'frac34'; Value: #190), // vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum
    (Name: 'iquest'; Value: #191), // inverted question mark = turned question mark, U+00BF ISOnum
    (Name: 'Agrave'; Value: #192), // latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1
    (Name: 'Aacute'; Value: #193), // latin capital letter A with acute, U+00C1 ISOlat1
    (Name: 'Acirc'; Value: #194), // latin capital letter A with circumflex, U+00C2 ISOlat1
    (Name: 'Atilde'; Value: #195), // latin capital letter A with tilde, U+00C3 ISOlat1
    (Name: 'Auml'; Value: #196), // latin capital letter A with diaeresis, U+00C4 ISOlat1
    (Name: 'Aring'; Value: #197), // latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1
    (Name: 'AElig'; Value: #198), // latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1
    (Name: 'Ccedil'; Value: #199), // latin capital letter C with cedilla, U+00C7 ISOlat1
    (Name: 'Egrave'; Value: #200), // latin capital letter E with grave, U+00C8 ISOlat1
    (Name: 'Eacute'; Value: #201), // latin capital letter E with acute, U+00C9 ISOlat1
    (Name: 'Ecirc'; Value: #202), // latin capital letter E with circumflex, U+00CA ISOlat1
    (Name: 'Euml'; Value: #203), // latin capital letter E with diaeresis, U+00CB ISOlat1
    (Name: 'Igrave'; Value: #204), // latin capital letter I with grave, U+00CC ISOlat1
    (Name: 'Iacute'; Value: #205), // latin capital letter I with acute, U+00CD ISOlat1
    (Name: 'Icirc'; Value: #206), // latin capital letter I with circumflex, U+00CE ISOlat1
    (Name: 'Iuml'; Value: #207), // latin capital letter I with diaeresis, U+00CF ISOlat1
    (Name: 'ETH'; Value: #208), // latin capital letter ETH, U+00D0 ISOlat1
    (Name: 'Ntilde'; Value: #209), // latin capital letter N with tilde, U+00D1 ISOlat1
    (Name: 'Ograve'; Value: #210), // latin capital letter O with grave, U+00D2 ISOlat1
    (Name: 'Oacute'; Value: #211), // latin capital letter O with acute, U+00D3 ISOlat1
    (Name: 'Ocirc'; Value: #212), // latin capital letter O with circumflex, U+00D4 ISOlat1
    (Name: 'Otilde'; Value: #213), // latin capital letter O with tilde, U+00D5 ISOlat1
    (Name: 'Ouml'; Value: #214), // latin capital letter O with diaeresis, U+00D6 ISOlat1
    (Name: 'times'; Value: #215), // multiplication sign, U+00D7 ISOnum
    (Name: 'Oslash'; Value: #216), // latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1
    (Name: 'Ugrave'; Value: #217), // latin capital letter U with grave, U+00D9 ISOlat1
    (Name: 'Uacute'; Value: #218), // latin capital letter U with acute, U+00DA ISOlat1
    (Name: 'Ucirc'; Value: #219), // latin capital letter U with circumflex, U+00DB ISOlat1
    (Name: 'Uuml'; Value: #220), // latin capital letter U with diaeresis, U+00DC ISOlat1
    (Name: 'Yacute'; Value: #221), // latin capital letter Y with acute, U+00DD ISOlat1
    (Name: 'THORN'; Value: #222), // latin capital letter THORN, U+00DE ISOlat1
    (Name: 'szlig'; Value: #223), // latin small letter sharp s = ess-zed, U+00DF ISOlat1
    (Name: 'agrave'; Value: #224), // latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1
    (Name: 'aacute'; Value: #225), // latin small letter a with acute, U+00E1 ISOlat1
    (Name: 'acirc'; Value: #226), // latin small letter a with circumflex, U+00E2 ISOlat1
    (Name: 'atilde'; Value: #227), // latin small letter a with tilde, U+00E3 ISOlat1
    (Name: 'auml'; Value: #228), // latin small letter a with diaeresis, U+00E4 ISOlat1
    (Name: 'aring'; Value: #229), // latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1
    (Name: 'aelig'; Value: #230), // latin small letter ae = latin small ligature ae, U+00E6 ISOlat1
    (Name: 'ccedil'; Value: #231), // latin small letter c with cedilla, U+00E7 ISOlat1
    (Name: 'egrave'; Value: #232), // latin small letter e with grave, U+00E8 ISOlat1
    (Name: 'eacute'; Value: #233), // latin small letter e with acute, U+00E9 ISOlat1
    (Name: 'ecirc'; Value: #234), // latin small letter e with circumflex, U+00EA ISOlat1
    (Name: 'euml'; Value: #235), // latin small letter e with diaeresis, U+00EB ISOlat1
    (Name: 'igrave'; Value: #236), // latin small letter i with grave, U+00EC ISOlat1
    (Name: 'iacute'; Value: #237), // latin small letter i with acute, U+00ED ISOlat1
    (Name: 'icirc'; Value: #238), // latin small letter i with circumflex, U+00EE ISOlat1
    (Name: 'iuml'; Value: #239), // latin small letter i with diaeresis, U+00EF ISOlat1
    (Name: 'eth'; Value: #240), // latin small letter eth, U+00F0 ISOlat1
    (Name: 'ntilde'; Value: #241), // latin small letter n with tilde, U+00F1 ISOlat1
    (Name: 'ograve'; Value: #242), // latin small letter o with grave, U+00F2 ISOlat1
    (Name: 'oacute'; Value: #243), // latin small letter o with acute, U+00F3 ISOlat1
    (Name: 'ocirc'; Value: #244), // latin small letter o with circumflex, U+00F4 ISOlat1
    (Name: 'otilde'; Value: #245), // latin small letter o with tilde, U+00F5 ISOlat1
    (Name: 'ouml'; Value: #246), // latin small letter o with diaeresis, U+00F6 ISOlat1
    (Name: 'divide'; Value: #247), // division sign, U+00F7 ISOnum
    (Name: 'oslash'; Value: #248), // latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1
    (Name: 'ugrave'; Value: #249), // latin small letter u with grave, U+00F9 ISOlat1
    (Name: 'uacute'; Value: #250), // latin small letter u with acute, U+00FA ISOlat1
    (Name: 'ucirc'; Value: #251), // latin small letter u with circumflex, U+00FB ISOlat1
    (Name: 'uuml'; Value: #252), // latin small letter u with diaeresis, U+00FC ISOlat1
    (Name: 'yacute'; Value: #253), // latin small letter y with acute, U+00FD ISOlat1
    (Name: 'thorn'; Value: #254), // latin small letter thorn, U+00FE ISOlat1
    (Name: 'yuml'; Value: #255), // latin small letter y with diaeresis, U+00FF ISOlat1

    // symbols, mathematical symbols, and Greek letters
    // Latin Extended-B
    (Name: 'fnof'; Value: #402), // latin small f with hook = function = florin, U+0192 ISOtech

    // Greek
    (Name: 'Alpha'; Value: #913), // greek capital letter alpha, U+0391
    (Name: 'Beta'; Value: #914), // greek capital letter beta, U+0392
    (Name: 'Gamma'; Value: #915), // greek capital letter gamma, U+0393 ISOgrk3
    (Name: 'Delta'; Value: #916), // greek capital letter delta, U+0394 ISOgrk3
    (Name: 'Epsilon'; Value: #917), // greek capital letter epsilon, U+0395
    (Name: 'Zeta'; Value: #918), // greek capital letter zeta, U+0396
    (Name: 'Eta'; Value: #919), // greek capital letter eta, U+0397
    (Name: 'Theta'; Value: #920), // greek capital letter theta, U+0398 ISOgrk3
    (Name: 'Iota'; Value: #921), // greek capital letter iota, U+0399
    (Name: 'Kappa'; Value: #922), // greek capital letter kappa, U+039A
    (Name: 'Lambda'; Value: #923), // greek capital letter lambda, U+039B ISOgrk3
    (Name: 'Mu'; Value: #924), // greek capital letter mu, U+039C
    (Name: 'Nu'; Value: #925), // greek capital letter nu, U+039D
    (Name: 'Xi'; Value: #926), // greek capital letter xi, U+039E ISOgrk3
    (Name: 'Omicron'; Value: #927), // greek capital letter omicron, U+039F
    (Name: 'Pi'; Value: #928), // greek capital letter pi, U+03A0 ISOgrk3
    (Name: 'Rho'; Value: #929), // greek capital letter rho, U+03A1
    (Name: 'Sigma'; Value: #931), // greek capital letter sigma, U+03A3 ISOgrk3,
                                     // there is no Sigmaf, and no U+03A2 character either
    (Name: 'Tau'; Value: #932), // greek capital letter tau, U+03A4
    (Name: 'Upsilon'; Value: #933), // greek capital letter upsilon, U+03A5 ISOgrk3
    (Name: 'Phi'; Value: #934), // greek capital letter phi, U+03A6 ISOgrk3
    (Name: 'Chi'; Value: #935), // greek capital letter chi, U+03A7
    (Name: 'Psi'; Value: #936), // greek capital letter psi, U+03A8 ISOgrk3
    (Name: 'Omega'; Value: #937), // greek capital letter omega, U+03A9 ISOgrk3
    (Name: 'alpha'; Value: #945), // greek small letter alpha, U+03B1 ISOgrk3
    (Name: 'beta'; Value: #946), // greek small letter beta, U+03B2 ISOgrk3
    (Name: 'gamma'; Value: #947), // greek small letter gamma, U+03B3 ISOgrk3
    (Name: 'delta'; Value: #948), // greek small letter delta, U+03B4 ISOgrk3
    (Name: 'epsilon'; Value: #949), // greek small letter epsilon, U+03B5 ISOgrk3
    (Name: 'zeta'; Value: #950), // greek small letter zeta, U+03B6 ISOgrk3
    (Name: 'eta'; Value: #951), // greek small letter eta, U+03B7 ISOgrk3
    (Name: 'theta'; Value: #952), // greek small letter theta, U+03B8 ISOgrk3
    (Name: 'iota'; Value: #953), // greek small letter iota, U+03B9 ISOgrk3
    (Name: 'kappa'; Value: #954), // greek small letter kappa, U+03BA ISOgrk3
    (Name: 'lambda'; Value: #955), // greek small letter lambda, U+03BB ISOgrk3
    (Name: 'mu'; Value: #956), // greek small letter mu, U+03BC ISOgrk3
    (Name: 'nu'; Value: #957), // greek small letter nu, U+03BD ISOgrk3
    (Name: 'xi'; Value: #958), // greek small letter xi, U+03BE ISOgrk3
    (Name: 'omicron'; Value: #959), // greek small letter omicron, U+03BF NEW
    (Name: 'pi'; Value: #960), // greek small letter pi, U+03C0 ISOgrk3
    (Name: 'rho'; Value: #961), // greek small letter rho, U+03C1 ISOgrk3
    (Name: 'sigmaf'; Value: #962), // greek small letter final sigma, U+03C2 ISOgrk3
    (Name: 'sigma'; Value: #963), // greek small letter sigma, U+03C3 ISOgrk3
    (Name: 'tau'; Value: #964), // greek small letter tau, U+03C4 ISOgrk3
    (Name: 'upsilon'; Value: #965), // greek small letter upsilon, U+03C5 ISOgrk3
    (Name: 'phi'; Value: #966), // greek small letter phi, U+03C6 ISOgrk3
    (Name: 'chi'; Value: #967), // greek small letter chi, U+03C7 ISOgrk3
    (Name: 'psi'; Value: #968), // greek small letter psi, U+03C8 ISOgrk3
    (Name: 'omega'; Value: #969), // greek small letter omega, U+03C9 ISOgrk3
    (Name: 'thetasym'; Value: #977), // greek small letter theta symbol, U+03D1 NEW
    (Name: 'upsih'; Value: #978), // greek upsilon with hook symbol, U+03D2 NEW
    (Name: 'piv'; Value: #982), // greek pi symbol, U+03D6 ISOgrk3
   // General Punctuation
    (Name: 'apos'; Value: #8217), // curly apostrophe,
    (Name: 'bull'; Value: #8226), // bullet = black small circle, U+2022 ISOpub,
                                      // bullet is NOT the same as bullet operator, U+2219
    (Name: 'hellip'; Value: #8230), // horizontal ellipsis = three dot leader, U+2026 ISOpub
    (Name: 'prime'; Value: #8242), // prime = minutes = feet, U+2032 ISOtech
    (Name: 'Prime'; Value: #8243), // double prime = seconds = inches, U+2033 ISOtech
    (Name: 'oline'; Value: #8254), // overline = spacing overscore, U+203E NEW
    (Name: 'frasl'; Value: #8260), // fraction slash, U+2044 NEW
    // Letterlike Symbols
    (Name: 'weierp'; Value: #8472), // script capital P = power set = Weierstrass p, U+2118 ISOamso
    (Name: 'image'; Value: #8465), // blackletter capital I = imaginary part, U+2111 ISOamso
    (Name: 'real'; Value: #8476), // blackletter capital R = real part symbol, U+211C ISOamso
    (Name: 'trade'; Value: #8482), // trade mark sign, U+2122 ISOnum
    (Name: 'alefsym'; Value: #8501), // alef symbol = first transfinite cardinal, U+2135 NEW
                                      // alef symbol is NOT the same as hebrew letter alef, U+05D0 although the same
                                      // glyph could be used to depict both characters
    // Arrows
    (Name: 'larr'; Value: #8592), // leftwards arrow, U+2190 ISOnum
    (Name: 'uarr'; Value: #8593), // upwards arrow, U+2191 ISOnu
    (Name: 'rarr'; Value: #8594), // rightwards arrow, U+2192 ISOnum
    (Name: 'darr'; Value: #8595), // downwards arrow, U+2193 ISOnum
    (Name: 'harr'; Value: #8596), // left right arrow, U+2194 ISOamsa
    (Name: 'crarr'; Value: #8629), // downwards arrow with corner leftwards = carriage return, U+21B5 NEW
    (Name: 'lArr'; Value: #8656), // leftwards double arrow, U+21D0 ISOtech
                                      // ISO 10646 does not say that lArr is the same as the 'is implied by' arrow but
                                      // also does not have any other charater for that function. So ? lArr can be used
                                      // for 'is implied by' as ISOtech sugg
    (Name: 'uArr'; Value: #8657), // upwards double arrow, U+21D1 ISOamsa
    (Name: 'rArr'; Value: #8658), // rightwards double arrow, U+21D2 ISOtech
                                      // ISO 10646 does not say this is the 'implies' character but does not have another
                                      // character with this function so ? rArr can be used for 'implies' as ISOtech suggests
    (Name: 'dArr'; Value: #8659), // downwards double arrow, U+21D3 ISOamsa
    (Name: 'hArr'; Value: #8660), // left right double arrow, U+21D4 ISOamsa
    // Mathematical Operators
    (Name: 'forall'; Value: #8704), // for all, U+2200 ISOtech
    (Name: 'part'; Value: #8706), // partial differential, U+2202 ISOtech
    (Name: 'exist'; Value: #8707), // there exists, U+2203 ISOtech
    (Name: 'empty'; Value: #8709), // empty set = null set = diameter, U+2205 ISOamso
    (Name: 'nabla'; Value: #8711), // nabla = backward difference, U+2207 ISOtech
    (Name: 'isin'; Value: #8712), // element of, U+2208 ISOtech
    (Name: 'notin'; Value: #8713), // not an element of, U+2209 ISOtech
    (Name: 'ni'; Value: #8715), // contains as member, U+220B ISOtech
    (Name: 'prod'; Value: #8719), // n-ary product = product sign, U+220F ISOamsb
                                      // prod is NOT the same character as U+03A0 'greek capital letter pi' though the
                                      // same glyph might be used for both
    (Name: 'sum'; Value: #8721), // n-ary sumation, U+2211 ISOamsb
                                      //  sum is NOT the same character as U+03A3 'greek capital letter sigma' though the
                                      // same glyph might be used for both
    (Name: 'minus'; Value: #8722), // minus sign, U+2212 ISOtech
    (Name: 'lowast'; Value: #8727), // asterisk operator, U+2217 ISOtech
    (Name: 'radic'; Value: #8730), // square root = radical sign, U+221A ISOtech
    (Name: 'prop'; Value: #8733), // proportional to, U+221D ISOtech
    (Name: 'infin'; Value: #8734), // infinity, U+221E ISOtech
    (Name: 'ang'; Value: #8736), // angle, U+2220 ISOamso
    (Name: 'and'; Value: #8743), // logical and = wedge, U+2227 ISOtech
    (Name: 'or'; Value: #8744), // logical or = vee, U+2228 ISOtech
    (Name: 'cap'; Value: #8745), // intersection = cap, U+2229 ISOtech
    (Name: 'cup'; Value: #8746), // union = cup, U+222A ISOtech
    (Name: 'int'; Value: #8747), // integral, U+222B ISOtech
    (Name: 'there4'; Value: #8756), // therefore, U+2234 ISOtech
    (Name: 'sim'; Value: #8764), // tilde operator = varies with = similar to, U+223C ISOtech
                                      // tilde operator is NOT the same character as the tilde, U+007E, although the same
                                      // glyph might be used to represent both
    (Name: 'cong'; Value: #8773), // approximately equal to, U+2245 ISOtech
    (Name: 'asymp'; Value: #8776), // almost equal to = asymptotic to, U+2248 ISOamsr
    (Name: 'ne'; Value: #8800), // not equal to, U+2260 ISOtech
    (Name: 'equiv'; Value: #8801), // identical to, U+2261 ISOtech
    (Name: 'le'; Value: #8804), // less-than or equal to, U+2264 ISOtech
    (Name: 'ge'; Value: #8805), // greater-than or equal to, U+2265 ISOtech
    (Name: 'sub'; Value: #8834), // subset of, U+2282 ISOtech
    (Name: 'sup'; Value: #8835), // superset of, U+2283 ISOtech
                                      // note that nsup, 'not a superset of, U+2283' is not covered by the Symbol font
                                      // encoding and is not included.
    (Name: 'nsub'; Value: #8836), // not a subset of, U+2284 ISOamsn
    (Name: 'sube'; Value: #8838), // subset of or equal to, U+2286 ISOtech
    (Name: 'supe'; Value: #8839), // superset of or equal to, U+2287 ISOtech
    (Name: 'oplus'; Value: #8853), // circled plus = direct sum, U+2295 ISOamsb
    (Name: 'otimes'; Value: #8855), // circled times = vector product, U+2297 ISOamsb
    (Name: 'perp'; Value: #8869), // up tack = orthogonal to = perpendicular, U+22A5 ISOtech
    (Name: 'sdot'; Value: #8901), // dot operator, U+22C5 ISOamsb
                                      // dot operator is NOT the same character as U+00B7 middle dot
    // Miscellaneous Technical
    (Name: 'lceil'; Value: #8968), // left ceiling = apl upstile, U+2308 ISOamsc
    (Name: 'rceil'; Value: #8969), // right ceiling, U+2309 ISOamsc
    (Name: 'lfloor'; Value: #8970), // left floor = apl downstile, U+230A ISOamsc
    (Name: 'rfloor'; Value: #8971), // right floor, U+230B ISOamsc
    (Name: 'lang'; Value: #9001), // left-pointing angle bracket = bra, U+2329 ISOtech
                                      // lang is NOT the same character as U+003C 'less than' or U+2039 'single
                                      // left-pointing angle quotation mark'
    (Name: 'rang'; Value: #9002), // right-pointing angle bracket = ket, U+232A ISOtech
                                      // rang is NOT the same character as U+003E 'greater than' or U+203A 'single
                                      // right-pointing angle quotation mark'
    // Geometric Shapes
    (Name: 'loz'; Value: #9674), // lozenge, U+25CA ISOpub
    // Miscellaneous Symbols
    (Name: 'spades'; Value: #9824), // black spade suit, U+2660 ISOpub
                                     // black here seems to mean filled as opposed to hollow
    (Name: 'clubs'; Value: #9827), // black club suit = shamrock, U+2663 ISOpub
    (Name: 'hearts'; Value: #9829), // black heart suit = valentine, U+2665 ISOpub
    (Name: 'diams'; Value: #9830), // black diamond suit, U+2666 ISOpub

    // markup-significant and internationalization characters
    // C0 Controls and Basic Latin
    (Name: 'quot'; Value: #34), // quotation mark = APL quote, U+0022 ISOnum
    (Name: 'amp'; Value: #38), // ampersand, U+0026 ISOnum
    (Name: 'lt'; Value: #60), // less-than sign, U+003C ISOnum
    (Name: 'gt'; Value: #62), // greater-than sign, U+003E ISOnum
    // Latin Extended-A
    (Name: 'OElig'; Value: #338), // latin capital ligature OE, U+0152 ISOlat2
    (Name: 'oelig'; Value: #339), // latin small ligature oe, U+0153 ISOlat2
                                     // ligature is a misnomer, this is a separate character in some languages
    (Name: 'Scaron'; Value: #352), // latin capital letter S with caron, U+0160 ISOlat2
    (Name: 'scaron'; Value: #353), // latin small letter s with caron, U+0161 ISOlat2
    (Name: 'Yuml'; Value: #376), // latin capital letter Y with diaeresis, U+0178 ISOlat2
    // Spacing Modifier Letters
    (Name: 'circ'; Value: #710), // modifier letter circumflex accent, U+02C6 ISOpub
    (Name: 'tilde'; Value: #732), // small tilde, U+02DC ISOdia
    // General Punctuation
    (Name: 'ensp'; Value: #8194), // en space, U+2002 ISOpub
    (Name: 'emsp'; Value: #8195), // em space, U+2003 ISOpub
    (Name: 'thinsp'; Value: #8201), // thin space, U+2009 ISOpub
    (Name: 'zwnj'; Value: #8204), // zero width non-joiner, U+200C NEW RFC 2070
    (Name: 'zwj'; Value: #8205), // zero width joiner, U+200D NEW RFC 2070
    (Name: 'lrm'; Value: #8206), // left-to-right mark, U+200E NEW RFC 2070
    (Name: 'rlm'; Value: #8207), // right-to-left mark, U+200F NEW RFC 2070
    (Name: 'ndash'; Value: #8211), // en dash, U+2013 ISOpub
    (Name: 'mdash'; Value: #8212), // em dash, U+2014 ISOpub
    (Name: 'lsquo'; Value: #8216), // left single quotation mark, U+2018 ISOnum
    (Name: 'rsquo'; Value: #8217), // right single quotation mark, U+2019 ISOnum
    (Name: 'sbquo'; Value: #8218), // single low-9 quotation mark, U+201A NEW
    (Name: 'ldquo'; Value: #8220), // left double quotation mark, U+201C ISOnum
    (Name: 'rdquo'; Value: #8221), // right double quotation mark, U+201D ISOnum
    (Name: 'bdquo'; Value: #8222), // double low-9 quotation mark, U+201E NEW
    (Name: 'dagger'; Value: #8224), // dagger, U+2020 ISOpub
    (Name: 'Dagger'; Value: #8225), // double dagger, U+2021 ISOpub
    (Name: 'permil'; Value: #8240), // per mille sign, U+2030 ISOtech
    (Name: 'lsaquo'; Value: #8249), // single left-pointing angle quotation mark, U+2039 ISO proposed
                                     // lsaquo is proposed but not yet ISO standardized
    (Name: 'rsaquo'; Value: #8250), // single right-pointing angle quotation mark, U+203A ISO proposed
                                     // rsaquo is proposed but not yet ISO standardized
    (Name: 'euro'; Value: #8364) //  euro sign, U+20AC NEW
    );

procedure SortEntities;
var
  I: integer;
begin
// Put the Entities into a sorted StringList for faster access.
  if Entities = nil then
  begin
    Entities := ThtStringList.Create;
    with Entities do
    begin
      CaseSensitive := True;
      for I := 0 to EntityCount - 1 do
        Entities.AddObject(EntityDefinitions[I].Name, Pointer(EntityDefinitions[I].Value));
      Sort;
    end;
  end;
end;

initialization
  LCToken := TokenObj.Create;
  PropStack := THtmlPropStack.Create;
  SortEntities;

finalization

  LCToken.Free;
  PropStack.Free;
  Entities.Free;
end.
