{
Version   11.7
Copyright (c) 2014-2016 by HtmlViewer Team

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

unit HtmlSymb;

interface

uses
{$ifdef MSWINDOWS}
  Windows, SysUtils,
{$endif}
  HtmlGlobals;

type
  TElemSymb = (
    OtherChar, CommandSy, StringSy {was TextSy}, EolSy, EofSy,

    { doc type tags }
    XmlSy,     DocTypeSy,

    { html elements }

    HtmlSy,    HeadSy,
    HtmlEndSy, HeadEndSy,

    { frame set elements }

    FrameSetSy, FrameSetEndSy,
    FrameSy,
    NoFramesSy, NoFramesEndSy,

    { metadata elements }

    TitleElemSy, TitleEndSy,
    BaseSy,
    LinkElemSy,
    MetaSy,
    StyleSy, StyleEndSy,
    ScriptSy, ScriptEndSy,

    { misc elements }

    BaseFontSy,
    BgSoundSy,

    HRSy,
    BRSy,

    ImageSy,
    PanelSy,
    IFrameSy,   IFrameEndSy,
    ProgressSy, ProgressEndSy,
    MeterSy,    MeterEndSy,

    MapSy, MapEndSy,
    AreaSy,

    PageSy,

    ObjectSy, ObjectEndSy,
    ParamSy,

    { inline elements }

    SpanSy,    NoBrSy,
    SpanEndSy, NoBrEndSy,

    WbrSy,

    FontSy,    BSy,    ISy,    SSy,    StrikeSy,    USy,    SubSy,    SupSy,    BigSy,    SmallSy,    TTSy,
    FontEndSy, BEndSy, IEndSy, SEndSy, StrikeEndSy, UEndSy, SubEndSy, SupEndSy, BigEndSy, SmallEndSy, TTEndSy,

    EmSy,    StrongSy,    CodeSy,    KbdSy,    SampSy,    CiteSy,    VarSy,    AbbrSy,    AcronymSy,    DfnSy,    
    EmEndSy, StrongEndSy, CodeEndSy, KbdEndSy, SampEndSy, CiteEndSy, VarEndSy, AbbrEndSy, AcronymEndSy, DfnEndSy,

    DelSy,    InsSy,    MarkSy,    TimeSy,
    DelEndSy, InsEndSy, MarkEndSy, TimeEndSy,

    ASy,
    AEndSy,

    { block elements }

    BodySy,    PSy,    DivSy,    CenterSy,
    BodyEndSy, PEndSy, DivEndSy, CenterEndSy,

    ArticleSy,    SectionSy,    MainSy, NavSy,    AsideSy,
    ArticleEndSy, SectionEndSy, MainEndSy, NavEndSy, AsideEndSy,

    {Keep order} H1Sy,    H2Sy,    H3Sy,    H4Sy,    H5Sy,    H6Sy,    {end order}
    {Keep order} H1EndSy, H2EndSy, H3EndSy, H4EndSy, H5EndSy, H6EndSy, {end order}

    HGroupSy,    HeaderSy,    FooterSy,    AddressSy,    BlockQuoteSy,    PreSy,
    HGroupEndSy, HeaderEndSy, FooterEndSy, AddressEndSy, BlockQuoteEndSy, PreEndSy,

    OLSy,    LISy,    ULSy,    DirSy,    MenuSy,    DLSy,    DDSy,    DTSy,
    OLEndSy, LIEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, DDEndSy, DTEndSy,

    liAloneSy,

    { table elements }

    TableSy, TableEndSy,
    ColGroupSy, ColGroupEndSy, ColSy,
    CaptionSy, CaptionEndSy,
    THeadSy,    TBodySy,    TFootSy,
    THeadEndSy, TBodyEndSy, TFootEndSy,
    TRSy, TREndSy,
    THSy,    TDSy,
    THEndSy, TDEndSy,

    { form elements }

    FormSy, FormEndSy,
    FieldsetSy, FieldsetEndSy,
    LegendSy, LegendEndSy,
    LabelSy, LabelEndSy,
    TextAreaSy,    SelectSy,     OptionSy,    ButtonSy, ButtonEndSy, InputSy,
    TextAreaEndSy, SelectEndSy,  OptionEndSy
  );
  TElemSymbSet = set of TElemSymb;

  TAttrSymb = (
    OtherAttribute,
    ActionSy,
    ActiveSy,
    AlignSy,
    AltSy,
    BackgroundSy,
    BGColorSy,
    BGPropertiesSy,
    BorderSy,
    BorderColorSy,
    BorderColorDarkSy,
    BorderColorLightSy,
    CellPaddingSy,
    CellSpacingSy,
    CharSetSy,
    CheckBoxSy,
    CheckedSy,
    ClassSy,
    ClearSy,
    ColorSy,
    ColsSy,
    ColSpanSy,
    ContentSy,
    CoordsSy,
    DisabledSy,
    EncodingSy,
    EncTypeSy,
    FaceSy,
    FrameAttrSy,
    FrameBorderSy,
    HeightSy,
    HighSy,
    HrefSy,
    HSpaceSy,
    HttpEqSy,
    IDSy,
    IsMapSy,
    LabelAttrSy,
    LanguageSy,
    LeftMarginSy,
    LinkSy,
    LoopSy,
    LowSy,
    MarginHeightSy,
    MarginWidthSy,
    MaxSy,
    MaxLengthSy,
    MediaSy,
    MethodSy,
    MinSy,
    MultipleSy,
    NameSy,
    NoHrefSy,
    NoResizeSy,
    NoShadeSy,
    NoWrapSy,
    OLinkSy,
    OnBlurSy,
    OnChangeSy,
    OnClickSy,
    OnFocusSy,
    OptimumSy,
    PlaceholderSy,
    PlainSy,
    RadioSy,
    RatioSy,
    ReadonlySy,
    RelSy,
    RevSy,
    RowsSy,
    RowSpanSy,
    RulesSy,
    ScrollingSy,
    SelectedSy,
    ShapeSy,
    SizeSy,
    SpanAttrSy,
    SpellCheckSy,
    SrcSy,
    StartSy,
    StyleAttrSy,
    TabIndexSy,
    TargetSy,
    TextSy,
    TitleSy,
    TopMarginSy,
    TranspSy,
    TypeSy,
    UseMapSy,
    VAlignSy,
    ValueSy,
    VersionSy,
    VLinkSy,
    VSpaceSy,
    WidthSy,
    WrapSy);
  TAttrSymbSet = set of TAttrSymb;

  PEntity = ^TEntity;
  TEntity = record
    Name: ThtString;
    Value: Integer;
  end;

  PResWord = ^TResWord;
  TResWord = record
    Name: ThtString;
    Symbol: TElemSymb;
    EndSym: TElemSymb; // CommandSy == no end symbol
  end;

  PSymbol = ^TSymbolRec;
  TSymbolRec = record
    Name: ThtString;
    Value: TAttrSymb;
  end;

var
  Entities: ThtStringList;
  ElementNames: ThtStringList;
  AttributeNames: ThtStringList;

procedure SetSymbolName(Sy: TElemSymb; Name: ThtString); overload;
procedure SetSymbolName(Sy: TAttrSymb; Name: ThtString); overload;
function SymbToStr(Sy: TElemSymb): ThtString; overload;
function SymbToStr(Sy: TAttrSymb): ThtString; overload;
function EndSymbToStr(Sy: TElemSymb): ThtString;
function EndSymbToSymb(Sy: TElemSymb): TElemSymb;
function EndSymbFromSymb(Sy: TElemSymb): TElemSymb;

implementation

var
  ElementNamesIndex: array [TElemSymb] of Integer;
  ElemSymbolNames: array [TElemSymb] of ThtString;

  AttrSymbolNames: array [TAttrSymb] of ThtString;

procedure SetSymbolName(Sy: TElemSymb; Name: ThtString); overload;
begin
  Name := htLowerCase(Name);
  if ElemSymbolNames[Sy] <> '' then
    assert(ElemSymbolNames[Sy] = Name, 'Different names for same element symbol!');
  ElemSymbolNames[Sy] := Name;
end;

procedure SetSymbolName(Sy: TAttrSymb; Name: ThtString); overload;
begin
  Name := htLowerCase(Name);
  if AttrSymbolNames[Sy] <> '' then
    assert(AttrSymbolNames[Sy] = Name, 'Different names for same attribute symbol!');
  AttrSymbolNames[Sy] := Name;
end;

function SymbToStr(Sy: TElemSymb): ThtString;
begin
  Result := ElemSymbolNames[Sy];
end;

function SymbToStr(Sy: TAttrSymb): ThtString;
begin
  Result := AttrSymbolNames[Sy];
end;

function EndSymbToStr(Sy: TElemSymb): ThtString;
begin
  Result := ElemSymbolNames[Sy];
end;

function EndSymbToSymb(Sy: TElemSymb): TElemSymb;
var
  I: Integer;
begin
  I := ElementNamesIndex[Sy];
  if I >= 0 then
    Result := PResWord(ElementNames.Objects[I]).Symbol
  else
    Result := CommandSy; // no match
end;

function EndSymbFromSymb(Sy: TElemSymb): TElemSymb;
var
  I: Integer;
begin
  I := ElementNamesIndex[Sy];
  if I >= 0 then
    Result := PResWord(ElementNames.Objects[I]).EndSym
  else
    Result := CommandSy; // no match
end;

procedure InitEntities;
const
  // Taken from http://www.w3.org/TR/REC-html40/sgml/entities.html.
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
    (Name: 'euro'; Value: 8364)); //  euro sign, U+20AC NEW
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


procedure InitElements;
const
  ElementDefinitions: array[1..104] of TResWord = (
    (Name: '?XML';        Symbol: XmlSy;        EndSym: CommandSy),
    (Name: '!DOCTYPE';    Symbol: DocTypeSy;    EndSym: CommandSy),
    {HTML}
    (Name: 'HTML';        Symbol: HtmlSy;       EndSym: HtmlEndSy),
    (Name: 'TITLE';       Symbol: TitleElemSy;  EndSym: TitleEndSy),
    (Name: 'BODY';        Symbol: BodySy;       EndSym: BodyEndSy),
    (Name: 'HEAD';        Symbol: HeadSy;       EndSym: HeadEndSy),
    (Name: 'B';           Symbol: BSy;          EndSym: BEndSy),
    (Name: 'I';           Symbol: ISy;          EndSym: IEndSy),
    (Name: 'H1';          Symbol: H1Sy;         EndSym: H1EndSy),
    (Name: 'H2';          Symbol: H2Sy;         EndSym: H2EndSy),
    (Name: 'H3';          Symbol: H3Sy;         EndSym: H3EndSy),
    (Name: 'H4';          Symbol: H4Sy;         EndSym: H4EndSy),
    (Name: 'H5';          Symbol: H5Sy;         EndSym: H5EndSy),
    (Name: 'H6';          Symbol: H6Sy;         EndSym: H6EndSy),
    (Name: 'EM';          Symbol: EmSy;         EndSym: EmEndSy),
    (Name: 'STRONG';      Symbol: StrongSy;     EndSym: StrongEndSy),
    (Name: 'U';           Symbol: USy;          EndSym: UEndSy),
    //
    (Name: 'INS';         Symbol: InsSy;        EndSym: InsEndSy),
    (Name: 'DEL';         Symbol: DelSy;        EndSym: DelEndSy),
    //
    (Name: 'CITE';        Symbol: CiteSy;       EndSym: CiteEndSy),
    (Name: 'VAR';         Symbol: VarSy;        EndSym: VarEndSy),
    (Name: 'ABBR';        Symbol: AbbrSy;       EndSym: AbbrEndSy),
    (Name: 'ACRONYM';     Symbol: AcronymSy;    EndSym: AcronymEndSy),
    (Name: 'DFN';         Symbol: DfnSy;        EndSym: DfnEndSy),

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
    (Name: 'IFRAME';      Symbol: IFrameSy;     EndSym: IFrameEndSy),
    (Name: 'BASE';        Symbol: BaseSy;       EndSym: CommandSy),
    (Name: 'BUTTON';      Symbol: ButtonSy;     EndSym: ButtonEndSy),
    (Name: 'INPUT';       Symbol: InputSy;      EndSym: CommandSy),
    (Name: 'BASEFONT';    Symbol: BaseFontSy;   EndSym: CommandSy),
    (Name: 'AREA';        Symbol: AreaSy;       EndSym: CommandSy),
    (Name: 'FRAME';       Symbol: FrameSy;      EndSym: CommandSy),
    (Name: 'PAGE';        Symbol: PageSy;       EndSym: CommandSy),
    (Name: 'BGSOUND';     Symbol: BgSoundSy;    EndSym: CommandSy),
    (Name: 'META';        Symbol: MetaSy;       EndSym: CommandSy),
    (Name: 'PANEL';       Symbol: PanelSy;      EndSym: CommandSy),
    (Name: 'WBR';         Symbol: WbrSy;        EndSym: CommandSy),
    (Name: 'LINK';        Symbol: LinkElemSy;   EndSym: CommandSy),
    (Name: 'COL';         Symbol: ColSy;        EndSym: CommandSy),
    (Name: 'PARAM';       Symbol: ParamSy;      EndSym: CommandSy),
    {HTML5 }
    (Name: 'MAIN';        Symbol: MainSy;       EndSym: MainEndSy),
    (Name: 'HEADER';      Symbol: HeaderSy;     EndSym: HeaderEndSy),
    (Name: 'SECTION';     Symbol: SectionSy;    EndSym: SectionEndSy),
    (Name: 'NAV';         Symbol: NavSy;        EndSym: NavEndSy),
    (Name: 'ARTICLE';     Symbol: ArticleSy;    EndSym: ArticleEndSy),
    (Name: 'ASIDE';       Symbol: AsideSy;      EndSym: AsideEndSy),
    (Name: 'FOOTER';      Symbol: FooterSy;     EndSym: FooterEndSy),
    (Name: 'HGROUP';      Symbol: HGroupSy;     EndSym: HGroupEndSy),
    (Name: 'MARK';        Symbol: MarkSy;       EndSym: MarkEndSy),
    (Name: 'TIME';        Symbol: TimeSy;       EndSym: TimeEndSy),
    (Name: 'PROGRESS';    Symbol: ProgressSy;   EndSym: ProgressEndSy),
    (Name: 'METER';       Symbol: MeterSy;      EndSym: MeterEndSy));
var
  I: Integer;
  P: PResWord;
  S: TElemSymb;
begin
  // Put the Attributes into a sorted StringList for faster access.
  if ElementNames = nil then
  begin
    ElementNames := ThtStringList.Create;
    ElementNames.CaseSensitive := True;
    for I := low(ElementDefinitions) to high(ElementDefinitions) do
      ElementNames.AddObject(ElementDefinitions[I].Name, @ElementDefinitions[I]);
    ElementNames.Sort;

    // initialize ElementNamesIndex and ElemSymbolNames
    for S := low(TElemSymb) to high(TElemSymb) do
      ElementNamesIndex[S] := -1;
    for I := 0 to ElementNames.Count - 1 do
    begin
      P := PResWord(ElementNames.Objects[I]);
      ElementNamesIndex[P.Symbol] := I;
      SetSymbolName(P.Symbol, P.Name);
      if P.EndSym <> CommandSy then
      begin
        ElementNamesIndex[P.EndSym] := I;
        SetSymbolName(P.EndSym, P.Name);
      end;
    end;
  end;
end;


procedure InitAttributes;
const
  AttribDefinitions: array[1..95] of TSymbolRec = (
    (Name: 'ACTION';            Value: ActionSy),
    (Name: 'ACTIVE';            Value: ActiveSy),
    (Name: 'ALIGN';             Value: AlignSy),
    (Name: 'ALT';               Value: AltSy),
    (Name: 'BACKGROUND';        Value: BackgroundSy),
    (Name: 'BGCOLOR';           Value: BGColorSy),
    (Name: 'BGPROPERTIES';      Value: BGPropertiesSy),
    (Name: 'BORDER';            Value: BorderSy),
    (Name: 'BORDERCOLOR';       Value: BorderColorSy),
    (Name: 'BORDERCOLORDARK';   Value: BorderColorDarkSy),
    (Name: 'BORDERCOLORLIGHT';  Value: BorderColorLightSy),
    (Name: 'CELLPADDING';       Value: CellPaddingSy),
    (Name: 'CELLSPACING';       Value: CellSpacingSy),
    (Name: 'CHARSET';           Value: CharSetSy),
    (Name: 'CHECKBOX';          Value: CheckBoxSy),
    (Name: 'CHECKED';           Value: CheckedSy),
    (Name: 'CLASS';             Value: ClassSy),
    (Name: 'CLEAR';             Value: ClearSy),
    (Name: 'COLOR';             Value: ColorSy),
    (Name: 'COLS';              Value: ColsSy),
    (Name: 'COLSPAN';           Value: ColSpanSy),
    (Name: 'CONTENT';           Value: ContentSy),
    (Name: 'COORDS';            Value: CoordsSy),
    (Name: 'DISABLED';          Value: DisabledSy),
    (Name: 'ENCODING';          Value: EncodingSy),
    (Name: 'ENCTYPE';           Value: EncTypeSy),
    (Name: 'FACE';              Value: FaceSy),
    (Name: 'FRAME';             Value: FrameAttrSy),
    (Name: 'FRAMEBORDER';       Value: FrameBorderSy),
    (Name: 'HEIGHT';            Value: HeightSy),
    (Name: 'HIGH';              Value: HighSy),
    (Name: 'HREF';              Value: HrefSy),
    (Name: 'HSPACE';            Value: HSpaceSy),
    (Name: 'HTTP-EQUIV';        Value: HttpEqSy),
    (Name: 'ID';                Value: IDSy),
    (Name: 'ISMAP';             Value: IsMapSy),
    (Name: 'LABEL';             Value: LabelAttrSy),
    (Name: 'LANGUAGE';          Value: LanguageSy),
    (Name: 'LEFTMARGIN';        Value: LeftMarginSy),
    (Name: 'LINK';              Value: LinkSy),
    (Name: 'LOOP';              Value: LoopSy),
    (Name: 'LOW';               Value: LowSy),
    (Name: 'MARGINHEIGHT';      Value: MarginHeightSy),
    (Name: 'MARGINWIDTH';       Value: MarginWidthSy),
    (Name: 'MAX';               Value: MaxSy),
    (Name: 'MAXLENGTH';         Value: MaxLengthSy),
    (Name: 'MEDIA';             Value: MediaSy),
    (Name: 'METHOD';            Value: MethodSy),
    (Name: 'MIN';               Value: MinSy),
    (Name: 'MULTIPLE';          Value: MultipleSy),
    (Name: 'NAME';              Value: NameSy),
    (Name: 'NOHREF';            Value: NoHrefSy),
    (Name: 'NORESIZE';          Value: NoResizeSy),
    (Name: 'NOSHADE';           Value: NoShadeSy),
    (Name: 'NOWRAP';            Value: NoWrapSy),
    (Name: 'OLINK';             Value: OLinkSy),
    (Name: 'ONBLUR';            Value: OnBlurSy),
    (Name: 'ONCHANGE';          Value: OnChangeSy),
    (Name: 'ONCLICK';           Value: OnClickSy),
    (Name: 'ONFOCUS';           Value: OnFocusSy),
    (Name: 'OPTIMUM';           Value: OptimumSy),
    (Name: 'PLACEHOLDER';       Value: PlaceholderSy),
    (Name: 'PLAIN';             Value: PlainSy),
    (Name: 'RADIO';             Value: RadioSy),
    (Name: 'RATIO';             Value: RatioSy),
    (Name: 'READONLY';          Value: ReadonlySy),
    (Name: 'REL';               Value: RelSy),
    (Name: 'REV';               Value: RevSy),
    (Name: 'ROWS';              Value: RowsSy),
    (Name: 'ROWSPAN';           Value: RowSpanSy),
    (Name: 'RULES';             Value: RulesSy),
    (Name: 'SCROLLING';         Value: ScrollingSy),
    (Name: 'SELECTED';          Value: SelectedSy),
    (Name: 'SHAPE';             Value: ShapeSy),
    (Name: 'SIZE';              Value: SizeSy),
    (Name: 'SPAN';              Value: SpanAttrSy),
    (Name: 'SPELLCHECK';        Value: spellcheckSy),
    (Name: 'SRC';               Value: SrcSy),
    (Name: 'START';             Value: StartSy),
    (Name: 'STYLE';             Value: StyleAttrSy),
    (Name: 'TABINDEX';          Value: TabIndexSy),
    (Name: 'TARGET';            Value: TargetSy),
    (Name: 'TEXT';              Value: TextSy),
    (Name: 'TITLE';             Value: TitleSy),
    (Name: 'TOPMARGIN';         Value: TopMarginSy),
    (Name: 'TRANSP';            Value: TranspSy),
    (Name: 'TYPE';              Value: TypeSy),
    (Name: 'USEMAP';            Value: UseMapSy),
    (Name: 'VALIGN';            Value: VAlignSy),
    (Name: 'VALUE';             Value: ValueSy),
    (Name: 'VERSION';           Value: VersionSy),
    (Name: 'VLINK';             Value: VLinkSy),
    (Name: 'VSPACE';            Value: VSpaceSy),
    (Name: 'WIDTH';             Value: WidthSy),
    (Name: 'WRAP';              Value: WrapSy));
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
  InitElements;
finalization
  Entities.Free;
  AttributeNames.Free;
  ElementNames.Free;
end.
