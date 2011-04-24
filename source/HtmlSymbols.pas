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

unit HtmlSymbols;

{
This implementation bases on the W3C recommendation

http://www.w3.org/TR/1999/REC-html401-19991224/

}
interface

uses
  Graphics,
  HtmlGlobals;

type

  ThtScrollInfo = record
    BWidth: Integer;   // single border width of paintpanel
    BWidth2: Integer;  // double border width of paintpanel
    HWidth: Integer;   // width of paintpanel
    VHeight: Integer;  // height of paintpanel
    HBar: Boolean;     // show horizontal scrollbar
    VBar: Boolean;     // show vertical scrollbar
  end;

  THtmlLinkType = (
    ltUnknown,
    ltAlternate,
    ltStylesheet,
    ltStart,
    ltNext,
    ltPrev,
    ltContents,
    ltIndex,
    ltGlossary,
    ltCopyright,
    ltChapter,
    ltSection,
    ltSubsection,
    ltAppendix,
    ltHelp,
    ltBookmark,
    ltShortcutIcon);
const
  CHtmlLinkType: array [THtmlLinkType] of ThtString = (
    '',
    'alernate',
    'stylesheet',
    'start',
    'next',
    'prev',
    'contents',
    'index',
    'glossary',
    'copyright',
    'chapter',
    'section',
    'subsection',
    'appendix',
    'help',
    'bookmark',
    'shortcut icon');

type
  THtmlElementSymbol = (
    // virtual html elements
    NoEndSy,      // indicates in TElementDescription, that this element has no end tag.
    UnknownSy,    // indicates in TryStrToElement(), that the given string does not match any known element.
    TextSy,       // indicates in THtmlParser.GetTag(), that this is simple element content.
    EofSy,
    EolSy,

    // real html elements
    ASy,          AEndSy,
    AbbrSy,       AbbrEndSy,      // since12
    AcronymSy,    AcronymEndSy,   // since12
    AddressSy,    AddressEndSy,
    AppletSy,     AppletEndSy,    // since12
    AreaSy,
    BSy,          BEndSy,
    BaseSy,
    BaseFontSy,
    BgSoundSy,                    // extension
    BdoSy,        BdoEndSy,       // since12
    BigSy,        BigEndSy,
    BlockQuoteSy, BlockQuoteEndSy,
    BodySy,       BodyEndSy,
    BRSy,
    ButtonSy,
    CaptionSy,    CaptionEndSy,
    CenterSy,     CenterEndSy,
    CiteSy,       CiteEndSy,
    CodeSy,       CodeEndSy,
    ColSy,
    ColGroupSy,   ColGroupEndSy,
    DDSy,         DDEndSy,
    DelSy,        DelEndSy,       // since12
    DfnSy,        DfnEndSy,       // since12
    DirSy,        DirEndSy,
    DivSy,        DivEndSy,
    DLSy,         DLEndSy,
    DTSy,         DTEndSy,
    EmSy,         EmEndSy,
    EmbedSy,      EmbedEndSy,     // extension, since12 
    FieldsetSy,   FieldsetEndSy,
    FontSy,       FontEndSy,
    FormSy,       FormEndSy,
    FrameSy,
    FrameSetSy,   FrameSetEndSy,
    H1Sy,         H1EndSy,
    H2Sy,         H2EndSy,
    H3Sy,         H3EndSy,
    H4Sy,         H4EndSy,
    H5Sy,         H5EndSy,
    H6Sy,         H6EndSy,
    HeadSy,       HeadEndSy,
    HRSy,
    HtmlSy,       HtmlEndSy,
    ISy,          IEndSy,
    IFrameSy,     IFrameEndSy,    // since12
    ImageSy,
    InputSy,
    InsSy,        InsEndSy,       // since12
    IsIndexSy,                    // since12
    KbdSy,        KbdEndSy,
    LabelSy,      LabelEndSy,
    LegendSy,     LegendEndSy,
    LISy,         LIEndSy,
    LinkSy,
    MapSy,        MapEndSy,
    MenuSy,       MenuEndSy,
    MetaSy,
    NoBrSy,       NoBrEndSy,      // extension
    NoEmbedSy,    NoEmbedEndSy,   // extension, since12
    NoFramesSy,   NoFramesEndSy,
    NoScriptSy,   NoScriptEndSy,  // since12
    ObjectSy,     ObjectEndSy,
    OLSy,         OLEndSy,
    OptGroupSy,   OptGroupEndSy,  // since12
    OptionSy,     OptionEndSy,
    PSy,          PEndSy,
    PageSy,                       // extension
    PanelSy,                      // extension
    ParamSy,
    PreSy,        PreEndSy,
    QSy,          QEndSy,         // since12
    ReadonlySy,                   // extension
    SSy,          SEndSy,
    SampSy,       SampEndSy,
    ScriptSy,     ScriptEndSy,
    SelectSy,     SelectEndSy,
    SelectedSy,                   // extension
    SmallSy,      SmallEndSy,
    SpanSy,       SpanEndSy,
    StrikeSy,     StrikeEndSy,
    StrongSy,     StrongEndSy,
    StyleSy,      StyleEndSy,
    SubSy,        SubEndSy,
    SupSy,        SupEndSy,
    TableSy,      TableEndSy,
    TBodySy,      TBodyEndSy,
    TDSy,         TDEndSy,
    TextAreaSy,   TextAreaEndSy,
    TFootSy,      TFootEndSy,
    THSy,         THEndSy,
    THeadSy,      THeadEndSy,
    TitleSy,      TitleEndSy,
    TRSy,         TREndSy,
    TTSy,         TTEndSy,
    USy,          UEndSy,
    ULSy,         ULEndSy,
    VarSy,        VarEndSy,
    WbrSy,                        // extension
    WrapSy);                      // extension

  THtmlElementSymbols = set of THtmlElementSymbol;

  THtmlAttributeSymbol = (
    UnknownAttr,

    AbbrAttr,
    AccectCharsetAttr,
    AcceptAttr,
    AccessKeyAttr,
    ActionAttr,
    ActiveAttr,
    AlignAttr,
    AltAttr,
    ArchiveAttr, // extension
    AxisAttr,
    BGPropertiesAttr, // extension
    BorderColorDarkAttr,  // extension
    BorderColorLightAttr, // extension
    BorderColorAttr, // extension
    BorderAttr,
    CharAttr,
    CharOffAttr,
    CharSetAttr,
    CheckBoxAttr, // extension
    CheckedAttr,
    CiteAttr,
    ClassIdAttr,
    CodeAttr,
    CodeBaseAttr,
    CodeTypeAttr,
    ColsAttr,
    ColSpanAttr,
    CompactAttr,
    CommandAttr, // extension
    ContentAttr,
    CoordsAttr,
    DataAttr,
    DateTimeAttr,
    DeclareAttr,
    DeferAttr,
    DirAttr,
    DisabledAttr,
    EncTypeAttr,
    ForAttr,
    FrameAttr,
    FrameBorderAttr,
    HeadersAttr,
    HRefAttr,
    HRegLangAttr,
    HttpEquivAttr,
    IsMapAttr,
    LabelAttr,
    LangAttr,
    LanguageAttr,
    LeftMarginAttr,
    liAloneAttr, // extension
    LongDescAttr,
    LoopAttr, // extension
    MaxLengthAttr,
    MediaAttr,
    MethodAttr,
    MultipleAttr,
    NameAttr,
    NoHRefAttr,
    NoResizeAttr,
    NoShadeAttr,
    NoWrapAttr,
    ObjectAttr,
    PlainAttr, // extension
    ProfileAttr,
    PromptAttr,
    RadioAttr, // extension
    RatioAttr, // extension
    ReadOnlyAttr,
    RelAttr,
    RevAttr,
    RowsAttr,
    RowSpanAttr,
    RulesAttr,
    SchemeAttr,
    ScopeAttr,
    ScrollingAttr,
    SelectedAttr,
    ShapeAttr,
    SpanAttr,
    SrcAttr,
    StandByAttr,
    StartAttr,
    SummaryAttr,
    TabIndexAttr,
    TargetAttr,
    TextAttr,
    TitleAttr,
    TopMarginAttr, // extension
    TranspAttr, // extension
    TypeAttr,
    UseMapAttr,
    ValueAttr,
    ValueTypeAttr,
    VersionAttr,
    WrapAttr, // extension

    ClassAttr,
    IDAttr,

    // events
    OnBlurAttr,
    OnChangeAttr,
    OnClickAttr,
    OnDblClickAttr,
    OnFocusAttr,
    OnKeyDownAttr,
    OnKeyPressAttr,
    OnKeyUpAttr,
    OnLoadAttr,
    OnMouseDownAttr,
    OnMouseMoveAttr,
    OnMouseOutAttr,
    OnMouseOverAttr,
    OnMouseUpAttr,
    OnResetAttr,
    OnSelectAttr,
    OnSubmitAttr,
    OnUnloadAttr,

    // convert to corresponding style property/ies
    BackgroundAttr,
    BGColorAttr,
    CellPaddingAttr,
    CellSpacingAttr,
    ClearAttr,
    ColorAttr,
    FaceAttr,
    HeightAttr,
    HSpaceAttr,
    MarginHeightAttr,
    MarginWidthAttr,
    SizeAttr,
    StyleAttr,
    VAlignAttr,
    VSpaceAttr,
    WidthAttr,

    // link pseudo colors
    ALinkAttr,
    LinkAttr,
    OLinkAttr, // extension
    VLinkAttr
  );

//  THtmlEventAttributeSymbol = OnBlurAttr..OnUnloadAttr;
//  THtmlStyleAttributeSymbol = BackgroundAttr..WidthAttr;
//  THtmlLinkColorAttributeSymbol = ALinkAttr..VLinkAttr;

  TPropertySymbol = (
    UnknownPropSy,

    FontFamily, FontSize, FontStyle, FontWeight,
    TextAlign, TextDecoration,
    LetterSpacing,
    Color,

    BackgroundColor,
    MarginTop, MarginRight, MarginBottom, MarginLeft,
    PaddingTop, PaddingRight, PaddingBottom, PaddingLeft,
    BorderTopWidth, BorderRightWidth, BorderBottomWidth, BorderLeftWidth,
    BorderTopColor, BorderRightColor, BorderBottomColor, BorderLeftColor,
    BorderTopStyle, BorderRightStyle, BorderBottomStyle, BorderLeftStyle,
    psWidth, psHeight, psTop, psBottom, psRight, psLeft,

    Visibility, LineHeight, BackgroundImage, BackgroundPosition,
    BackgroundRepeat, BackgroundAttachment, VerticalAlign, Position, ZIndex,
    ListStyleType, ListStyleImage, psFloat, psClear, TextIndent,
    PageBreakBefore, PageBreakAfter, PageBreakInside, TextTransform,
    WordWrap, FontVariant, BorderCollapse, OverFlow, psDisplay, psEmptyCells,
    //TODO -oBG, 12.03.2011: psWhiteSpace

    // short hands
    MarginX, PaddingX, BorderWidthX, BorderX,
    BorderTX, BorderRX, BorderBX, BorderLX,
    FontX, BackgroundX, ListStyleX, BorderColorX,
    BorderStyleX
  );

  TShortHandSymbol = MarginX..BorderStyleX;

  TStylePropertySymbol = FontFamily..psEmptyCells;
  TPropertyArray = array [TStylePropertySymbol] of Variant;

function TryStrToLinkType(const Str: ThtString; out LinkType: THtmlLinkType): Boolean;
  
function AttributeSymbolToStr(Sy: THtmlAttributeSymbol): ThtString;
function TryStrToAttributeSymbol(const Str: ThtString; out Sy: THtmlAttributeSymbol): Boolean;

function PropertySymbolToStr(Sy: TPropertySymbol): ThtString;
function TryStrToPropertySymbol(const Str: ThtString; out Sy: TPropertySymbol): Boolean;

function TryNameToColor(const Name: ThtString; out Color: TColor): Boolean;
function TryStrToEntity(const Str: ThtString; out Entity: Integer): Boolean;

implementation

//------------------------------------------------------------------------------
// charset codings
//------------------------------------------------------------------------------

type
  PEntity = ^TEntity;
  TEntity = record
    Name: ThtString;
    Value: Integer;
  end;

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
    (Name: 'euro'; Value: 8364) //  euro sign, U+20AC NEW
    );

var
  Entities: ThtStringList;

//-- BG ---------------------------------------------------------- 26.03.2011 --
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

//-- BG ---------------------------------------------------------- 26.03.2011 --
function TryStrToEntity(const Str: ThtString; out Entity: Integer): Boolean;
var
  I: Integer;
begin
  Result := Entities.Find(Str, I);
  if Result then
    Entity := PEntity(Entities.Objects[I]).Value;
end;

//------------------------------------------------------------------------------
// style properties
//------------------------------------------------------------------------------

type
  PPropertyDescription = ^TPropertyDescription;
  TPropertyDescription = record
    Name: ThtString;
    Symbol: TPropertySymbol;
  end;

const
  CPropertyDescriptions: array [1..72] of TPropertyDescription = (
    (Name: 'font-family';              Symbol: FontFamily),
    (Name: 'font-size';                Symbol: FontSize),
    (Name: 'font-style';               Symbol: FontStyle),
    (Name: 'font-weight';              Symbol: FontWeight),
    (Name: 'text-align';               Symbol: TextAlign),
    (Name: 'text-decoration';          Symbol: TextDecoration),
    (Name: 'letter-spacing';           Symbol: LetterSpacing),
    (Name: 'color';                    Symbol: Color),
    (Name: 'background-color';         Symbol: BackgroundColor),
    (Name: 'margin-top';               Symbol: MarginTop),
    (Name: 'margin-right';             Symbol: MarginRight),
    (Name: 'margin-bottom';            Symbol: MarginBottom),
    (Name: 'margin-left';              Symbol: MarginLeft),
    (Name: 'padding-top';              Symbol: PaddingTop),
    (Name: 'padding-right';            Symbol: PaddingRight),
    (Name: 'padding-bottom';           Symbol: PaddingBottom),
    (Name: 'padding-left';             Symbol: PaddingLeft),
    (Name: 'border-top-width';         Symbol: BorderTopWidth),
    (Name: 'border-right-width';       Symbol: BorderRightWidth),
    (Name: 'border-bottom-width';      Symbol: BorderBottomWidth),
    (Name: 'border-left-width';        Symbol: BorderLeftWidth),
    (Name: 'border-top-color';         Symbol: BorderTopColor),
    (Name: 'border-right-color';       Symbol: BorderRightColor),
    (Name: 'border-bottom-color';      Symbol: BorderBottomColor),
    (Name: 'border-left-color';        Symbol: BorderLeftColor),
    (Name: 'border-top-style';         Symbol: BorderTopStyle),
    (Name: 'border-right-style';       Symbol: BorderRightStyle),
    (Name: 'border-bottom-style';      Symbol: BorderBottomStyle),
    (Name: 'border-left-style';        Symbol: BorderLeftStyle),
    (Name: 'width';                    Symbol: psWidth),
    (Name: 'height';                   Symbol: psHeight),
    (Name: 'top';                      Symbol: psTop),
    (Name: 'bottom';                   Symbol: psBottom),
    (Name: 'right';                    Symbol: psRight),
    (Name: 'left';                     Symbol: psLeft),
    (Name: 'visibility';               Symbol: Visibility),
    (Name: 'line-height';              Symbol: LineHeight),
    (Name: 'background-image';         Symbol: BackgroundImage),
    (Name: 'background-position';      Symbol: BackgroundPosition),
    (Name: 'background-repeat';        Symbol: BackgroundRepeat),
    (Name: 'background-attachment';    Symbol: BackgroundAttachment),
    (Name: 'vertical-align';           Symbol: VerticalAlign),
    (Name: 'position';                 Symbol: Position),
    (Name: 'z-index';                  Symbol: ZIndex),
    (Name: 'list-style-type';          Symbol: ListStyleType),
    (Name: 'list-style-image';         Symbol: ListStyleImage),
    (Name: 'float';                    Symbol: psFloat),
    (Name: 'clear';                    Symbol: psClear),
    (Name: 'text-indent';              Symbol: TextIndent),
    (Name: 'page-break-before';        Symbol: PageBreakBefore),
    (Name: 'page-break-after';         Symbol: PageBreakAfter),
    (Name: 'page-break-inside';        Symbol: PageBreakInside),
    (Name: 'text-transform';           Symbol: TextTransform),
    (Name: 'word-wrap';                Symbol: WordWrap),
    (Name: 'font-variant';             Symbol: FontVariant),
    (Name: 'border-collapse';          Symbol: BorderCollapse),
    (Name: 'overflow';                 Symbol: OverFlow),
    (Name: 'display';                  Symbol: psDisplay),
    (Name: 'empty-cells';              Symbol: psEmptyCells),
    //(Name: 'white-space'

    // short hand names
    (Name: 'margin';                   Symbol: MarginX),
    (Name: 'padding';                  Symbol: PaddingX),
    (Name: 'border-width';             Symbol: BorderWidthX),
    (Name: 'border';                   Symbol: BorderX),
    (Name: 'border-top';               Symbol: BorderTX),
    (Name: 'border-right';             Symbol: BorderRX),
    (Name: 'border-bottom';            Symbol: BorderBX),
    (Name: 'border-left';              Symbol: BorderLX),
    (Name: 'font';                     Symbol: FontX),
    (Name: 'background';               Symbol: BackgroundX),
    (Name: 'list-style';               Symbol: ListStyleX),
    (Name: 'border-color';             Symbol: BorderColorX),
    (Name: 'border-style';             Symbol: BorderStyleX)
  );

var
  PropertyDescriptions: ThtStringList;
  PropertyDescriptionsIndex: array [TPropertySymbol] of Integer;

procedure InitProperties;
var
  I: Integer;
  P: PPropertyDescription;
  S: TPropertySymbol;
begin
  // Put the Properties into a sorted StringList for faster access.
  if PropertyDescriptions = nil then
  begin
    PropertyDescriptions := ThtStringList.Create;
    PropertyDescriptions.CaseSensitive := True;
    for I := low(CPropertyDescriptions) to high(CPropertyDescriptions) do
    begin
      P := @CPropertyDescriptions[I];
      PropertyDescriptions.AddObject(P.Name, Pointer(P));
    end;
    PropertyDescriptions.Sort;

    // initialize PropertyDescriptionsIndex and SymbolNames
    for S := low(S) to high(S) do
      PropertyDescriptionsIndex[S] := -1;
    for I := 0 to PropertyDescriptions.Count - 1 do
    begin
      P := PPropertyDescription(PropertyDescriptions.Objects[I]);
      PropertyDescriptionsIndex[P.Symbol] := I;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function TryStrToPropertySymbol(const Str: ThtString; out Sy: TPropertySymbol): Boolean;
var
  I: Integer;
begin
  Result := PropertyDescriptions.Find(Str, I);
  if Result then
    Sy := PPropertyDescription(PropertyDescriptions.Objects[I]).Symbol
  else
    Sy := UnknownPropSy;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function PropertySymbolToStr(Sy: TPropertySymbol): ThtString;
begin
  Result := PPropertyDescription(PropertyDescriptions.Objects[PropertyDescriptionsIndex[Sy]]).Name;
end;

//------------------------------------------------------------------------------
// colors
//------------------------------------------------------------------------------

type
  PColorDescription = ^TColorDescription;
  TColorDescription = record
    Name: ThtString;
    Color: TColor;
  end;

const
  CColorDescriptions: array[1..176] of TColorDescription = (
    (Name: 'transparent';           Color: clNone),
    (Name: 'black';                 Color: clBLACK),
    (Name: 'maroon';                Color: clMAROON),
    (Name: 'green';                 Color: clGREEN),
    (Name: 'olive';                 Color: clOLIVE),
    (Name: 'navy';                  Color: clNAVY),
    (Name: 'purple';                Color: clPURPLE),
    (Name: 'teal';                  Color: clTEAL),
    (Name: 'gray';                  Color: clGRAY),
    (Name: 'silver';                Color: clSILVER),
    (Name: 'red';                   Color: clRED),
    (Name: 'lime';                  Color: clLIME),
    (Name: 'yellow';                Color: clYELLOW),
    (Name: 'blue';                  Color: clBLUE),
    (Name: 'fuchsia';               Color: clFUCHSIA),
    (Name: 'aqua';                  Color: clAQUA),
    (Name: 'white';                 Color: clWHITE),
    (Name: 'aliceblue';             Color: $FFF8F0),
    (Name: 'antiquewhite';          Color: $D7EBFA),
    (Name: 'aquamarine';            Color: $D4FF7F),
    (Name: 'azure';                 Color: $FFFFF0),
    (Name: 'beige';                 Color: $DCF5F5),
    (Name: 'bisque';                Color: $C4E4FF),
    (Name: 'blanchedalmond';        Color: $CDEBFF),
    (Name: 'blueviolet';            Color: $E22B8A),
    (Name: 'brown';                 Color: $2A2AA5),
    (Name: 'burlywood';             Color: $87B8DE),
    (Name: 'cadetblue';             Color: $A09E5F),
    (Name: 'chartreuse';            Color: $00FF7F),
    (Name: 'chocolate';             Color: $1E69D2),
    (Name: 'coral';                 Color: $507FFF),
    (Name: 'cornflowerblue';        Color: $ED9564),
    (Name: 'cornsilk';              Color: $DCF8FF),
    (Name: 'crimson';               Color: $3614DC),
    (Name: 'cyan';                  Color: $FFFF00),
    (Name: 'darkblue';              Color: $8B0000),
    (Name: 'darkcyan';              Color: $8B8B00),
    (Name: 'darkgoldenrod';         Color: $0B86B8),
    (Name: 'darkgray';              Color: $A9A9A9),
    (Name: 'darkgreen';             Color: $006400),
    (Name: 'darkkhaki';             Color: $6BB7BD),
    (Name: 'darkmagenta';           Color: $8B008B),
    (Name: 'darkolivegreen';        Color: $2F6B55),
    (Name: 'darkorange';            Color: $008CFF),
    (Name: 'darkorchid';            Color: $CC3299),
    (Name: 'darkred';               Color: $00008B),
    (Name: 'darksalmon';            Color: $7A96E9),
    (Name: 'darkseagreen';          Color: $8FBC8F),
    (Name: 'darkslateblue';         Color: $8B3D48),
    (Name: 'darkslategray';         Color: $4F4F2F),
    (Name: 'darkturquoise';         Color: $D1CE00),
    (Name: 'darkviolet';            Color: $D30094),
    (Name: 'deeppink';              Color: $9314FF),
    (Name: 'deepskyblue';           Color: $FFBF00),
    (Name: 'dimgray';               Color: $696969),
    (Name: 'dodgerblue';            Color: $FF901E),
    (Name: 'firebrick';             Color: $2222B2),
    (Name: 'floralwhite';           Color: $F0FAFF),
    (Name: 'forestgreen';           Color: $228B22),
    (Name: 'gainsboro';             Color: $DCDCDC),
    (Name: 'ghostwhite';            Color: $FFF8F8),
    (Name: 'gold';                  Color: $00D7FF),
    (Name: 'goldenrod';             Color: $20A5DA),
    (Name: 'greenyellow';           Color: $2FFFAD),
    (Name: 'honeydew';              Color: $F0FFF0),
    (Name: 'hotpink';               Color: $B469FF),
    (Name: 'indianred';             Color: $5C5CCD),
    (Name: 'indigo';                Color: $82004B),
    (Name: 'ivory';                 Color: $F0FFFF),
    (Name: 'khaki';                 Color: $8CE6F0),
    (Name: 'lavender';              Color: $FAE6E6),
    (Name: 'lavenderblush';         Color: $F5F0FF),
    (Name: 'lawngreen';             Color: $00FC7C),
    (Name: 'lemonchiffon';          Color: $CDFAFF),
    (Name: 'lightblue';             Color: $E6D8AD),
    (Name: 'lightcoral';            Color: $8080F0),
    (Name: 'lightcyan';             Color: $FFFFE0),
    (Name: 'lightgoldenrodyellow';  Color: $D2FAFA),
    (Name: 'lightgreen';            Color: $90EE90),
    (Name: 'lightgray';             Color: $D3D3D3),
    (Name: 'lightpink';             Color: $C1B6FF),
    (Name: 'lightsalmon';           Color: $7AA0FF),
    (Name: 'lightseagreen';         Color: $AAB220),
    (Name: 'lightskyblue';          Color: $FACE87),
    (Name: 'lightslategray';        Color: $998877),
    (Name: 'lightsteelblue';        Color: $DEC4B0),
    (Name: 'lightyellow';           Color: $E0FFFF),
    (Name: 'limegreen';             Color: $32CD32),
    (Name: 'linen';                 Color: $E6F0FA),
    (Name: 'magenta';               Color: $FF00FF),
    (Name: 'mediumaquamarine';      Color: $AACD66),
    (Name: 'mediumblue';            Color: $CD0000),
    (Name: 'mediumorchid';          Color: $D355BA),
    (Name: 'mediumpurple';          Color: $DB7093),
    (Name: 'mediumseagreen';        Color: $71B33C),
    (Name: 'mediumslateblue';       Color: $EE687B),
    (Name: 'mediumspringgreen';     Color: $9AFA00),
    (Name: 'mediumturquoise';       Color: $CCD148),
    (Name: 'mediumvioletred';       Color: $8515C7),
    (Name: 'midnightblue';          Color: $701919),
    (Name: 'mintcream';             Color: $FAFFF5),
    (Name: 'mistyrose';             Color: $E1E4FF),
    (Name: 'moccasin';              Color: $B5E4FF),
    (Name: 'navajowhite';           Color: $ADDEFF),
    (Name: 'oldlace';               Color: $E6F5FD),
    (Name: 'olivedrab';             Color: $238E6B),
    (Name: 'orange';                Color: $00A5FF),
    (Name: 'orangered';             Color: $0045FF),
    (Name: 'orchid';                Color: $D670DA),
    (Name: 'palegoldenrod';         Color: $AAE8EE),
    (Name: 'palegreen';             Color: $98FB98),
    (Name: 'paleturquoise';         Color: $EEEEAF),
    (Name: 'palevioletred';         Color: $9370DB),
    (Name: 'papayawhip';            Color: $D5EFFF),
    (Name: 'peachpuff';             Color: $B9DAFF),
    (Name: 'peru';                  Color: $3F85CD),
    (Name: 'pink';                  Color: $CBC0FF),
    (Name: 'plum';                  Color: $DDA0DD),
    (Name: 'powderblue';            Color: $E6E0B0),
    (Name: 'rosybrown';             Color: $8F8FBC),
    (Name: 'royalblue';             Color: $E16941),
    (Name: 'saddlebrown';           Color: $13458B),
    (Name: 'salmon';                Color: $7280FA),
    (Name: 'sandybrown';            Color: $60A4F4),
    (Name: 'seagreen';              Color: $578B2E),
    (Name: 'seashell';              Color: $EEF5FF),
    (Name: 'sienna';                Color: $2D52A0),
    (Name: 'skyblue';               Color: $EBCE87),
    (Name: 'slateblue';             Color: $CD5A6A),
    (Name: 'slategray';             Color: $908070),
    (Name: 'snow';                  Color: $FAFAFF),
    (Name: 'springgreen';           Color: $7FFF00),
    (Name: 'steelblue';             Color: $B48246),
    (Name: 'tan';                   Color: $8CB4D2),
    (Name: 'thistle';               Color: $D8BFD8),
    (Name: 'tomato';                Color: $4763FF),
    (Name: 'turquoise';             Color: $D0E040),
    (Name: 'violet';                Color: $EE82EE),
    (Name: 'wheat';                 Color: $B3DEF5),
    (Name: 'whitesmoke';            Color: $F5F5F5),
    (Name: 'yellowgreen';           Color: $32CD9A),
    (Name: 'grey';                  Color: clgray),
    (Name: 'darkgrey';              Color: $A9A9A9),
    (Name: 'darkslategrey';         Color: $4F4F2F),
    (Name: 'dimgrey';               Color: $696969),
    (Name: 'lightgrey';             Color: $D3D3D3),
    (Name: 'lightslategrey';        Color: $998877),
    (Name: 'slategrey';             Color: $908070),
    (Name: 'background';            Color: clBackground),
    (Name: 'activecaption';         Color: clActiveCaption),
    (Name: 'inactivecaption';       Color: clInactiveCaption),
    (Name: 'menu';                  Color: clMenu),
    (Name: 'window';                Color: clWindow),
    (Name: 'windowframe';           Color: clWindowFrame),
    (Name: 'menutext';              Color: clMenuText),
    (Name: 'windowtext';            Color: clWindowText),
    (Name: 'captiontext';           Color: clCaptionText),
    (Name: 'activeborder';          Color: clActiveBorder),
    (Name: 'inactiveborder';        Color: clInactiveBorder),
    (Name: 'appworkSpace';          Color: clAppWorkSpace),
    (Name: 'highlight';             Color: clHighlight),
    (Name: 'hightlighttext';        Color: clHighlightText),
    (Name: 'buttonface';            Color: clBtnFace),
    (Name: 'buttonshadow';          Color: clBtnShadow),
    (Name: 'graytext';              Color: clGrayText),
    (Name: 'buttontext';            Color: clBtnText),
    (Name: 'inactivecaptiontext';   Color: clInactiveCaptionText),
    (Name: 'buttonhighlight';       Color: clBtnHighlight),
    (Name: 'threeddarkshadow';      Color: cl3DDkShadow),
    (Name: 'threedlightshadow';     Color: clBtnHighlight),
    (Name: 'infotext';              Color: clInfoText),
    (Name: 'infobackground';        Color: clInfoBk),
    (Name: 'scrollbar';             Color: clScrollBar),
    (Name: 'threedface';            Color: clBtnFace),
    (Name: 'threedhighlight';       Color: cl3DLight),
    (Name: 'threedshadow';          Color: clBtnShadow)
  );

var
  ColorDescriptions: ThtStringList;

procedure InitColors;
var
  I: Integer;
  P: PColorDescription;
begin
  // Put the Properties into a sorted StringList for faster access.
  if ColorDescriptions = nil then
  begin
    ColorDescriptions := ThtStringList.Create;
    ColorDescriptions.CaseSensitive := True;
    for I := low(CColorDescriptions) to high(CColorDescriptions) do
    begin
      P := @CColorDescriptions[I];
      ColorDescriptions.AddObject(P.Name, Pointer(P));
    end;
    ColorDescriptions.Sort;
  end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function TryNameToColor(const Name: ThtString; out Color: TColor): Boolean;
var
  I: Integer;
begin
  Result := ColorDescriptions.Find(Name, I);
  if Result then
    Color := PColorDescription(ColorDescriptions.Objects[I]).Color
  else
    Color := clNone;
end;

//------------------------------------------------------------------------------
// html attributes
//------------------------------------------------------------------------------

type
  PAttributeDescription = ^TAttributeDescription;
  TAttributeDescription = record
    Name: ThtString;
    Attr: THtmlAttributeSymbol;
  end;

const
  UnknownAd: TAttributeDescription = (Name: 'unknown'; Attr: UnknownAttr);

  CAttributeDescriptions: array[1..100] of TAttributeDescription = (
    (Name: 'ACTION';            Attr: ActionAttr),
    (Name: 'ACTIVE';            Attr: ActiveAttr),
    (Name: 'ALIGN';             Attr: AlignAttr),
    (Name: 'ALT';               Attr: AltAttr),
    (Name: 'BACKGROUND';        Attr: BackgroundAttr),
    (Name: 'BGCOLOR';           Attr: BGColorAttr),
    (Name: 'BGPROPERTIES';      Attr: BGPropertiesAttr),
    (Name: 'BORDER';            Attr: BorderAttr),
    (Name: 'BORDERCOLOR';       Attr: BorderColorAttr),
    (Name: 'BORDERCOLORDARK';   Attr: BorderColorDarkAttr),
    (Name: 'BORDERCOLORLIGHT';  Attr: BorderColorLightAttr),
    (Name: 'CELLPADDING';       Attr: CellPaddingAttr),
    (Name: 'CELLSPACING';       Attr: CellSpacingAttr),
    (Name: 'CHARSET';           Attr: CharSetAttr),
    (Name: 'CHECKBOX';          Attr: CheckBoxAttr),
    (Name: 'CHECKED';           Attr: CheckedAttr),
    (Name: 'CLASS';             Attr: ClassAttr),
    (Name: 'CLEAR';             Attr: ClearAttr),
    (Name: 'COLOR';             Attr: ColorAttr),
    (Name: 'COLS';              Attr: ColsAttr),
    (Name: 'COLSPAN';           Attr: ColSpanAttr),
    (Name: 'CONTENT';           Attr: ContentAttr),
    (Name: 'COORDS';            Attr: CoordsAttr),
    (Name: 'DISABLED';          Attr: DisabledAttr),
    (Name: 'ENCTYPE';           Attr: EncTypeAttr),
    (Name: 'FACE';              Attr: FaceAttr),
    (Name: 'FOR';               Attr: ForAttr),
    (Name: 'FRAMEBORDER';       Attr: FrameBorderAttr),
    (Name: 'HEIGHT';            Attr: HeightAttr),
    (Name: 'HREF';              Attr: HrefAttr),
    (Name: 'HSPACE';            Attr: HSpaceAttr),
    (Name: 'HTTP-EQUIV';        Attr: HttpEquivAttr),
    (Name: 'ID';                Attr: IDAttr),
    (Name: 'ISMAP';             Attr: IsMapAttr),
    (Name: 'LABEL';             Attr: LabelAttr),
    (Name: 'LANGUAGE';          Attr: LanguageAttr),
    (Name: 'LEFTMARGIN';        Attr: LeftMarginAttr),
    (Name: 'LINK';              Attr: LinkAttr),
    (Name: 'LOOP';              Attr: LoopAttr),
    (Name: 'MARGINHEIGHT';      Attr: MarginHeightAttr),
    (Name: 'MARGINWIDTH';       Attr: MarginWidthAttr),
    (Name: 'MAXLENGTH';         Attr: MaxLengthAttr),
    (Name: 'MEDIA';             Attr: MediaAttr),
    (Name: 'METHOD';            Attr: MethodAttr),
    (Name: 'MULTIPLE';          Attr: MultipleAttr),
    (Name: 'NAME';              Attr: NameAttr),
    (Name: 'NOHREF';            Attr: NoHrefAttr),
    (Name: 'NORESIZE';          Attr: NoResizeAttr),
    (Name: 'NOSHADE';           Attr: NoShadeAttr),
    (Name: 'NOWRAP';            Attr: NoWrapAttr),
    (Name: 'OLINK';             Attr: OLinkAttr),
    (Name: 'ONBLUR';            Attr: OnBlurAttr),
    (Name: 'ONCHANGE';          Attr: OnChangeAttr),
    (Name: 'ONCLICK';           Attr: OnClickAttr),
    (Name: 'ONFOCUS';           Attr: OnFocusAttr),
    (Name: 'ONDBLCLICK';        Attr: OnDblClickAttr),
    (Name: 'ONFOCUS';           Attr: OnFocusAttr),
    (Name: 'ONKEYDOWN';         Attr: OnKeyDownAttr),
    (Name: 'ONKEYPRESS';        Attr: OnKeyPressAttr),
    (Name: 'ONKEYUP';           Attr: OnKeyUpAttr),
    (Name: 'ONLOAD';            Attr: OnLoadAttr),
    (Name: 'ONMOUSEDOWN';       Attr: OnMouseDownAttr),
    (Name: 'ONMOUSEMOVE';       Attr: OnMouseMoveAttr),
    (Name: 'ONMOUSEOUT';        Attr: OnMouseOutAttr),
    (Name: 'ONMOUSEOVER';       Attr: OnMouseOverAttr),
    (Name: 'ONMOUSEUP';         Attr: OnMouseUpAttr),
    (Name: 'ONCHANGE';          Attr: OnResetAttr),
    (Name: 'ONCLICK';           Attr: OnSelectAttr),
    (Name: 'ONFOCUS';           Attr: OnSubmitAttr),
    (Name: 'ONDBLCLICK';        Attr: OnUnloadAttr),
    (Name: 'PLAIN';             Attr: PlainAttr),
    (Name: 'RADIO';             Attr: RadioAttr),
    (Name: 'RATIO';             Attr: RatioAttr),
    (Name: 'READONLY';          Attr: ReadonlyAttr),
    (Name: 'REL';               Attr: RelAttr),
    (Name: 'REV';               Attr: RevAttr),
    (Name: 'ROWS';              Attr: RowsAttr),
    (Name: 'ROWSPAN';           Attr: RowSpanAttr),
    (Name: 'SCROLLING';         Attr: ScrollingAttr),
    (Name: 'SELECTED';          Attr: SelectedAttr),
    (Name: 'SHAPE';             Attr: ShapeAttr),
    (Name: 'SIZE';              Attr: SizeAttr),
    (Name: 'SPAN';              Attr: SpanAttr),
    (Name: 'SRC';               Attr: SrcAttr),
    (Name: 'START';             Attr: StartAttr),
    (Name: 'STYLE';             Attr: StyleAttr),
    (Name: 'TABINDEX';          Attr: TabIndexAttr),
    (Name: 'TARGET';            Attr: TargetAttr),
    (Name: 'TEXT';              Attr: TextAttr),
    (Name: 'TITLE';             Attr: TitleAttr),
    (Name: 'TOPMARGIN';         Attr: TopMarginAttr),
    (Name: 'TRANSP';            Attr: TranspAttr),
    (Name: 'TYPE';              Attr: TypeAttr),
    (Name: 'USEMAP';            Attr: UseMapAttr),
    (Name: 'VALIGN';            Attr: VAlignAttr),
    (Name: 'VALUE';             Attr: ValueAttr),
    (Name: 'VLINK';             Attr: VLinkAttr),
    (Name: 'VSPACE';            Attr: VSpaceAttr),
    (Name: 'WIDTH';             Attr: WidthAttr),
    (Name: 'WRAP';              Attr: WrapAttr)
  );

var
  AttributeDescriptions: ThtStringList;
  AttributeDescriptionsIndex: array [THtmlAttributeSymbol] of PAttributeDescription;

procedure InitAttributes;
var
  I: Integer;
  P: PAttributeDescription;
begin
  // Put the Attributes into a sorted StringList for faster access.
  if AttributeDescriptions = nil then
  begin
    AttributeDescriptionsIndex[UnknownAd.Attr] := @UnknownAd;

    AttributeDescriptions := ThtStringList.Create;
    AttributeDescriptions.CaseSensitive := True;
    for I := low(CAttributeDescriptions) to high(CAttributeDescriptions) do
    begin
      P := @CAttributeDescriptions[I];
      AttributeDescriptions.AddObject(P.Name, Pointer(P));
      AttributeDescriptionsIndex[P.Attr] := P;
    end;
    AttributeDescriptions.Sort;
  end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function TryStrToAttributeSymbol(const Str: ThtString; out Sy: THtmlAttributeSymbol): Boolean;
var
  I: Integer;
begin
  Result := AttributeDescriptions.Find(Str, I);
  if Result then
    Sy := PAttributeDescription(AttributeDescriptions.Objects[I]).Attr
  else
    Sy := UnknownAttr;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function AttributeSymbolToStr(Sy: THtmlAttributeSymbol): ThtString;
begin
  Result := PAttributeDescription(AttributeDescriptionsIndex[Sy]).Name;
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
function TryStrToLinkType(const Str: ThtString; out LinkType: THtmlLinkType): Boolean;
var
  I: THtmlLinkType;
begin
  for I := low(I) to high(I) do
    if CHtmlLinkType[I] = Str then
    begin
      Result := True;
      LinkType := I;
      exit;
    end;
  Result := False;
end;


initialization
  InitAttributes;
  InitColors;
  InitEntities;
  InitProperties;
finalization
  AttributeDescriptions.Free;
  ColorDescriptions.Free;
  Entities.Free;
  PropertyDescriptions.Free;
end.
