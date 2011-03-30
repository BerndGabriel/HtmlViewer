{
Version   12
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

unit HtmlTree;

interface

uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  Contnrs,
  //
  HtmlGlobals,
  HtmlStyles,
  HtmlSymbols;

//------------------------------------------------------------------------------
// THtmlAttribute is class for all attributes of all HTML elements
//------------------------------------------------------------------------------

type
  THtmlAttribute = class
  public
    FSymbol: THtmlAttributeSymbol;
    FName: ThtString;
    FValue: ThtString;
    constructor Create(Symbol: THtmlAttributeSymbol; const Name, Value: ThtString);
    property Symbol: THtmlAttributeSymbol read FSymbol;
    property AsString: ThtString read FValue;
  end;

  THtmlAttributeList = class(TObjectList) {a list of tag attributes,(TAttributes)}
  private
    function GetAttribute(Index: Integer): THtmlAttribute; {$ifdef UseInline} inline; {$endif}
  public
    function Find(Name: ThtString; out Attribute: THtmlAttribute): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    function Find(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    property Items[Index: Integer]: THtmlAttribute read GetAttribute; default;
  end;

//------------------------------------------------------------------------------
// THtmlElement is base class for all elements in the HTML document tree.
//------------------------------------------------------------------------------

type
  THtmlElement = class
  private
    FParent: THtmlElement;
    FChildren: TObjectList;
    FSymbol: THtmlElementSymbol;
    FAttributes: THtmlAttributeList;
    FDocPos: Integer;

    FIds: ThtStringArray;
    FClasses: ThtStringArray;
    function GetChildCount: Integer;
  protected
    function FindAttribute(Name: ThtString; out Attribute: THtmlAttribute): Boolean; overload; virtual;
    function FindAttribute(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean; overload; virtual;
    function GetChild(Index: Integer): THtmlElement;
    function GetPseudos: TPseudos; virtual;
    procedure AddChild(Child: THtmlElement);
    procedure ExtractChild(Child: THtmlElement);
    procedure SetParent(Parent: THtmlElement);
  public
    constructor Create(Parent: THtmlElement; Symbol: THtmlElementSymbol; Attributes: THtmlAttributeList; DocPos: Integer);
    destructor Destroy; override;
    function IndexOf(Child: THtmlElement): Integer; virtual;
    function IsMatching(Selector: TSelector): Boolean;
    property Parent: THtmlElement read FParent;
    property Children[Index: Integer]: THtmlElement read GetChild; default;
    property Count: Integer read GetChildCount;
    property DocPos: Integer read FDocPos;
    property Symbol: THtmlElementSymbol read FSymbol;
  end;

  THtmlElementClass = class of THtmlElement;

  THtmlElementDescription = record
    Name: ThtString;
    Symbol: THtmlElementSymbol;
    Content: THtmlElementSymbols; // allowed content
    EndSym: THtmlElementSymbol;   // NoEndSy == no end symbol
    Clasz: THtmlElementClass;
  end;

const
  // virtual elements use lowercase names for visual effects.
  UnknownEd: THtmlElementDescription = (Name: 'unknown';  Symbol: UnknownSy; Content: []; EndSym: NoEndSy);
  TextEd:    THtmlElementDescription = (Name: 'text';     Symbol: TextSy;    Content: []; EndSym: NoEndSy; Clasz: THtmlElement);
  EofEd:     THtmlElementDescription = (Name: 'eof';      Symbol: EofSy;     Content: []; EndSym: NoEndSy);

function AttributeSymbolToStr(Sy: THtmlAttributeSymbol): ThtString;
function ElementSymbolToElementDescription(Sy: THtmlElementSymbol): THtmlElementDescription;
function ElementSymbolToStr(Sy: THtmlElementSymbol): ThtString;
function TryStrToAttributeSymbol(const Str: ThtString; out Sy: THtmlAttributeSymbol): Boolean;
//function TryStrToElementDescription(const Str: ThtString; out Ed: THtmlElementDescription): Boolean;
function TryStrToElementSymbol(const Str: ThtString; out Sy: THtmlElementSymbol): Boolean;
function TryStrToElementEndSym(const Str: ThtString; out Sy: THtmlElementSymbol): Boolean;

implementation

//------------------------------------------------------------------------------
// html elements
//------------------------------------------------------------------------------

type
  PHtmlElementDescription = ^THtmlElementDescription;

const
  // header repeatable elements

  HeadMiscElements = [ScriptSy, StyleSy, MetaSy, LinkSy, ObjectSy];

  // inline elements

  FontStyleElements = [TTSy, ISy, BSy, USy, SSy, StrikeSy, BigSy, SmallSy];

  PhraseElements = [EmSy, StrongSy, DfnSy, CodeSy, SampSy, KbdSy, VarSy, CiteSy, AbbrSy, AcronymSy];

  SpecialElements = [ASy, ImageSy, AppletSy, ObjectSy, FontSy, BaseFontSy, BRSy, ScriptSy, MapSy,
    QSy, SubSy, SupSy, SpanSy, BdoSy, IFrameSy];

  FormControlElements = [InputSy, SelectSy, TextAreaSy, LabelSy, ButtonSy];

  InlineElements = [TextSy] + FontStyleElements + PhraseElements + SpecialElements + FormControlElements;

  // block elements

  HeadingElements = [H1Sy, H2Sy, H3Sy, H4Sy, H5Sy, H6Sy];

  ListElements = [ULSy, OLSy, DirSy, MenuSy];

  BlockElements = [
    PSy, PreSy, DLSy, DivSy, CenterSy, NoScriptSy, NoFramesSy, BlockQuoteSy, FormSy, IsIndexSy, HRSy,
    TableSy, FieldsetSy, AddressSy] + HeadingElements + ListElements;

  // block or inline
  FlowElements = InlineElements + BlockElements;

  PreExcludedElements = [ImageSy, ObjectSy, AppletSy, BigSy, SmallSy, SubSy, SupSy, FontSy, BaseFontSy];

  ButtonExcludedElements = FormControlElements + [ASy, FormSy, IsIndexSy, FieldsetSy, IFrameSy];

  CElementDescriptions: array [1..98] of THtmlElementDescription = (
    (Name: 'A';           Symbol: ASy;          Content: InlineElements;  EndSym: AEndSy),
    (Name: 'ABBR';        Symbol: AbbrSy;       Content: InlineElements;  EndSym: AbbrEndSy),     // since12
    (Name: 'ACRONYM';     Symbol: AcronymSy;    Content: InlineElements;  EndSym: AcronymEndSy),  // since12
    (Name: 'ADDRESS';     Symbol: AddressSy;    Content: InlineElements + [PSy]; EndSym: AddressEndSy),
    (Name: 'APPLET';      Symbol: AppletSy;     Content: FlowElements + [ParamSy];  EndSym: AppletEndSy),   // since12
    (Name: 'AREA';        Symbol: AreaSy;       EndSym: NoEndSy),
    (Name: 'B';           Symbol: BSy;          Content: InlineElements;  EndSym: BEndSy),
    (Name: 'BASE';        Symbol: BaseSy;       EndSym: NoEndSy),
    (Name: 'BASEFONT';    Symbol: BaseFontSy;   EndSym: NoEndSy),
    (Name: 'BGSOUND';     Symbol: BgSoundSy;    EndSym: NoEndSy),       // extension
    (Name: 'BDO';         Symbol: BdoSy;        Content: InlineElements;  EndSym: BdoEndSy),      // since12
    (Name: 'BIG';         Symbol: BigSy;        Content: InlineElements;  EndSym: BigEndSy),
    (Name: 'BLOCKQUOTE';  Symbol: BlockQuoteSy; Content: FlowElements;  EndSym: BlockQuoteEndSy),
    (Name: 'BODY';        Symbol: BodySy;       Content: FlowElements;  EndSym: BodyEndSy),
    (Name: 'BR';          Symbol: BRSy;         EndSym: NoEndSy),
    (Name: 'BUTTON';      Symbol: ButtonSy;     Content: FlowElements - ButtonExcludedElements;  EndSym: NoEndSy),
    (Name: 'CAPTION';     Symbol: CaptionSy;    Content: InlineElements;  EndSym: CaptionEndSy),
    (Name: 'CENTER';      Symbol: CenterSy;     Content: FlowElements;  EndSym: CenterEndSy),
    (Name: 'CITE';        Symbol: CiteSy;       Content: InlineElements;  EndSym: CiteEndSy),
    (Name: 'CODE';        Symbol: CodeSy;       Content: InlineElements;  EndSym: CodeEndSy),
    (Name: 'COL';         Symbol: ColSy;        EndSym: NoEndSy),
    (Name: 'COLGROUP';    Symbol: ColGroupSy;   Content: [ColSy];  EndSym: ColGroupEndSy),
    (Name: 'DD';          Symbol: DDSy;         Content: FlowElements;  EndSym: DDEndSy),
    (Name: 'DEL';         Symbol: DelSy;        Content: FlowElements;  EndSym: DelEndSy),      // since12
    (Name: 'DFN';         Symbol: DfnSy;        Content: InlineElements;  EndSym: DfnEndSy),      // since12
    (Name: 'DIR';         Symbol: DirSy;        Content: [LiSy];  EndSym: DirEndSy),
    (Name: 'DIV';         Symbol: DivSy;        Content: FlowElements;  EndSym: DivEndSy),
    (Name: 'DL';          Symbol: DLSy;         Content: [DtSy, DdSy]; EndSym: DLEndSy),
    (Name: 'DT';          Symbol: DTSy;         Content: InlineElements;  EndSym: DTEndSy),
    (Name: 'EM';          Symbol: EmSy;         Content: InlineElements;  EndSym: EmEndSy),
    (Name: 'FIELDSET';    Symbol: FieldsetSy;   Content: FlowElements + [TextSy, LegendSy];  EndSym: FieldsetEndSy),
    (Name: 'FONT';        Symbol: FontSy;       Content: InlineElements;  EndSym: FontEndSy),
    (Name: 'FORM';        Symbol: FormSy;       Content: FlowElements - [FormSy];  EndSym: FormEndSy),
    (Name: 'FRAME';       Symbol: FrameSy;      EndSym: NoEndSy),
    (Name: 'FRAMESET';    Symbol: FrameSetSy;   Content: [FrameSetSy, FrameSy, NoFramesSy];  EndSym: FrameSetEndSy),
    (Name: 'H1';          Symbol: H1Sy;         Content: InlineElements;  EndSym: H1EndSy),
    (Name: 'H2';          Symbol: H2Sy;         Content: InlineElements;  EndSym: H2EndSy),
    (Name: 'H3';          Symbol: H3Sy;         Content: InlineElements;  EndSym: H3EndSy),
    (Name: 'H4';          Symbol: H4Sy;         Content: InlineElements;  EndSym: H4EndSy),
    (Name: 'H5';          Symbol: H5Sy;         Content: InlineElements;  EndSym: H5EndSy),
    (Name: 'H6';          Symbol: H6Sy;         Content: InlineElements;  EndSym: H6EndSy),
    (Name: 'HEAD';        Symbol: HeadSy;       Content: HeadMiscElements + [TitleSy, IsIndexSy, BaseSy];  EndSym: HeadEndSy),
    (Name: 'HR';          Symbol: HRSy;         EndSym: NoEndSy),
    (Name: 'HTML';        Symbol: HtmlSy;       Content: [HeadSy, BodySy, FrameSetSy];  EndSym: HtmlEndSy),
    (Name: 'I';           Symbol: ISy;          Content: InlineElements;  EndSym: IEndSy),
    (Name: 'IFRAME';      Symbol: IFrameSy;     Content: FlowElements;  EndSym: IFrameEndSy),     // since12
    (Name: 'IMG';         Symbol: ImageSy;      EndSym: NoEndSy),
    (Name: 'INPUT';       Symbol: InputSy;      EndSym: NoEndSy),
    (Name: 'INS';         Symbol: InsSy;        Content: FlowElements;  EndSym: InsEndSy),      // since12
    (Name: 'KBD';         Symbol: KbdSy;        Content: InlineElements;  EndSym: KbdEndSy),
    (Name: 'LABEL';       Symbol: LabelSy;      Content: InlineElements - [LabelSy];  EndSym: LabelEndSy),
    (Name: 'LEGEND';      Symbol: LegendSy;     Content: InlineElements;  EndSym: LegendEndSy),
    (Name: 'LI';          Symbol: LISy;         Content: FlowElements;  EndSym: LIEndSy),
    (Name: 'LINK';        Symbol: LinkSy;       EndSym: NoEndSy),
    (Name: 'MAP';         Symbol: MapSy;        Content: BlockElements;  EndSym: MapEndSy),
    (Name: 'MENU';        Symbol: MenuSy;       Content: [LiSy];  EndSym: MenuEndSy),
    (Name: 'META';        Symbol: MetaSy;       EndSym: NoEndSy),
    (Name: 'NOBR';        Symbol: NoBrSy;       EndSym: NoBrEndSy),       // extension
    (Name: 'NOFRAMES';    Symbol: NoFramesSy;   Content: FlowElements;  EndSym: NoFramesEndSy),
    (Name: 'NOSCRIPT';    Symbol: NoScriptSy;   Content: FlowElements;  EndSym: NoScriptEndSy), // since12
    (Name: 'OBJECT';      Symbol: ObjectSy;     Content: FlowElements + [ParamSy];  EndSym: ObjectEndSy),
    (Name: 'OL';          Symbol: OLSy;         Content: [LiSy];  EndSym: OLEndSy),
    (Name: 'OPTGROUP';    Symbol: OptGroupSy;   Content: [OptionSy];  EndSym: OptGroupEndSy), // since12
    (Name: 'OPTION';      Symbol: OptionSy;     Content: [TextSy];  EndSym: OptionEndSy),
    (Name: 'P';           Symbol: PSy;          Content: InlineElements;  EndSym: PEndSy),
    (Name: 'PAGE';        Symbol: PageSy;       EndSym: NoEndSy),       // extension
    (Name: 'PANEL';       Symbol: PanelSy;      EndSym: NoEndSy),       // extension
    (Name: 'PARAM';       Symbol: ParamSy;      EndSym: NoEndSy),
    (Name: 'PRE';         Symbol: PreSy;        Content: InlineElements - PreExcludedElements;  EndSym: PreEndSy),
    (Name: 'Q';           Symbol: QSy;          Content: InlineElements;  EndSym: QEndSy), // since12
    (Name: 'READONLY';    Symbol: ReadonlySy;   EndSym: NoEndSy),       // extension
    (Name: 'S';           Symbol: SSy;          Content: InlineElements;  EndSym: SEndSy),
    (Name: 'SAMP';        Symbol: SampSy;       Content: InlineElements;  EndSym: SampEndSy),
    (Name: 'SCRIPT';      Symbol: ScriptSy;     EndSym: ScriptEndSy),
    (Name: 'SELECT';      Symbol: SelectSy;     Content: [OptGroupSy, OptionSy];  EndSym: SelectEndSy),
    (Name: 'SELECTED';    Symbol: SelectedSy;   EndSym: NoEndSy),       // extension
    (Name: 'SMALL';       Symbol: SmallSy;      Content: InlineElements;  EndSym: SmallEndSy),
    (Name: 'SPAN';        Symbol: SpanSy;       Content: InlineElements;  EndSym: SpanEndSy),
    (Name: 'STRIKE';      Symbol: StrikeSy;     Content: InlineElements;  EndSym: StrikeEndSy),
    (Name: 'STRONG';      Symbol: StrongSy;     Content: InlineElements;  EndSym: StrongEndSy),
    (Name: 'STYLE';       Symbol: StyleSy;      EndSym: StyleEndSy),
    (Name: 'SUB';         Symbol: SubSy;        Content: InlineElements;  EndSym: SubEndSy),
    (Name: 'SUP';         Symbol: SupSy;        Content: InlineElements;  EndSym: SupEndSy),
    (Name: 'TABLE';       Symbol: TableSy;      Content: [CaptionSy, ColSy, ColGroupSy, TrSy, THeadSy, TBodySy, TFootSy];  EndSym: TableEndSy),
    (Name: 'TBODY';       Symbol: TBodySy;      Content: [TrSy];  EndSym: TBodyEndSy),
    (Name: 'TD';          Symbol: TDSy;         Content: FlowElements;  EndSym: TDEndSy),
    (Name: 'TEXTAREA';    Symbol: TextAreaSy;   Content: [TextSy];  EndSym: TextAreaEndSy),
    (Name: 'TFOOT';       Symbol: TFootSy;      Content: [TrSy];  EndSym: TFootEndSy),
    (Name: 'TH';          Symbol: THSy;         Content: FlowElements;  EndSym: THEndSy),
    (Name: 'THEAD';       Symbol: THeadSy;      Content: [TrSy];  EndSym: THeadEndSy),
    (Name: 'TITLE';       Symbol: TitleSy;      Content: [TextSy];  EndSym: TitleEndSy),
    (Name: 'TR';          Symbol: TRSy;         Content: [ThSy, TdSy];  EndSym: TREndSy),
    (Name: 'TT';          Symbol: TTSy;         Content: InlineElements;  EndSym: TTEndSy),
    (Name: 'U';           Symbol: USy;          Content: InlineElements;  EndSym: UEndSy),
    (Name: 'UL';          Symbol: ULSy;         Content: [LiSy];  EndSym: ULEndSy),
    (Name: 'VAR';         Symbol: VarSy;        Content: InlineElements;  EndSym: VarEndSy),
    (Name: 'WBR';         Symbol: WbrSy;        EndSym: NoEndSy),     // extension
    (Name: 'WRAP';        Symbol: WrapSy;       EndSym: NoEndSy)      // extension
    );
var
  ElementDescriptions: ThtStringList;
  ElementDescriptionsIndex: array [THtmlElementSymbol] of PHtmlElementDescription;

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure InitElementDescriptions;

  procedure SetIndex(const Ed: THtmlElementDescription);
  begin
    ElementDescriptionsIndex[Ed.Symbol] := @Ed;
    if Ed.EndSym <> NoEndSy then
      ElementDescriptionsIndex[Ed.EndSym] := @Ed;
  end;

var
  I: Integer;
begin
  // Put the element descriptions into a sorted StringList for faster access.
  if ElementDescriptions = nil then
  begin
    SetIndex(UnknownEd);
    SetIndex(TextEd);
    SetIndex(EofEd);
    ElementDescriptions := ThtStringList.Create;
    ElementDescriptions.CaseSensitive := True;
    for I := low(CElementDescriptions) to high(CElementDescriptions) do
    begin
      if CElementDescriptions[I].Clasz = nil then
        CElementDescriptions[I].Clasz := THtmlElement;
      ElementDescriptions.AddObject(CElementDescriptions[I].Name, @CElementDescriptions[I]);
      SetIndex(CElementDescriptions[I]);
    end;
    ElementDescriptions.Sort;
  end;
end;

////-- BG ---------------------------------------------------------- 29.03.2011 --
//function TryStrToElementDescription(const Str: ThtString; out Ed: THtmlElementDescription): Boolean;
//var
//  I: Integer;
//begin
//  Result := ElementDescriptions.Find(Str, I);
//  if Result then
//    Ed := PHtmlElementDescription(ElementDescriptions.Objects[I])^
//  else
//  begin
//    Ed := UnknownEd;
//    Ed.Name := Str;
//  end;
//end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
function TryStrToElementEndSym(const Str: ThtString; out Sy: THtmlElementSymbol): Boolean;
var
  I: Integer;
begin
  Result := ElementDescriptions.Find(Str, I);
  if Result then
    Sy := PHtmlElementDescription(ElementDescriptions.Objects[I]).EndSym
  else
    Sy := UnknownSy;
end;


//-- BG ---------------------------------------------------------- 29.03.2011 --
function TryStrToElementSymbol(const Str: ThtString; out Sy: THtmlElementSymbol): Boolean;
var
  I: Integer;
begin
  Result := ElementDescriptions.Find(Str, I);
  if Result then
    Sy := PHtmlElementDescription(ElementDescriptions.Objects[I]).Symbol
  else
    Sy := UnknownSy;
end;

//-- BG ---------------------------------------------------------- 29.03.2011 --
function ElementSymbolToElementDescription(Sy: THtmlElementSymbol): THtmlElementDescription;
var
  P: PHtmlElementDescription;
begin
  P := ElementDescriptionsIndex[Sy];
  if P <> nil then
    Result := P^
  else
    Result := UnknownEd;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function ElementSymbolToStr(Sy: THtmlElementSymbol): ThtString;
begin
  if ElementDescriptionsIndex[Sy] <> nil then
    Result := ElementDescriptionsIndex[Sy].Name
  else
    Result := '';
end;

//------------------------------------------------------------------------------
// html attributes
//------------------------------------------------------------------------------

type
  PAttributeDescription = ^TAttributeDescription;
  TAttributeDescription = record
    Name: ThtString;
    Symbol: THtmlAttributeSymbol;
  end;

const
  CAttributeDescriptions: array[1..84] of TAttributeDescription = (
    (Name: 'ACTION';            Symbol: ActionSy),
    (Name: 'ACTIVE';            Symbol: ActiveSy),
    (Name: 'ALIGN';             Symbol: AlignSy),
    (Name: 'ALT';               Symbol: AltSy),
    (Name: 'BACKGROUND';        Symbol: BackgroundSy),
    (Name: 'BGCOLOR';           Symbol: BGColorSy),
    (Name: 'BGPROPERTIES';      Symbol: BGPropertiesSy),
    (Name: 'BORDER';            Symbol: BorderSy),
    (Name: 'BORDERCOLOR';       Symbol: BorderColorSy),
    (Name: 'BORDERCOLORDARK';   Symbol: BorderColorDarkSy),
    (Name: 'BORDERCOLORLIGHT';  Symbol: BorderColorLightSy),
    (Name: 'CELLPADDING';       Symbol: CellPaddingSy),
    (Name: 'CELLSPACING';       Symbol: CellSpacingSy),
    (Name: 'CHARSET';           Symbol: CharSetSy),
    (Name: 'CHECKBOX';          Symbol: CheckBoxSy),
    (Name: 'CHECKED';           Symbol: CheckedSy),
    (Name: 'CLASS';             Symbol: ClassSy),
    (Name: 'CLEAR';             Symbol: ClearSy),
    (Name: 'COLOR';             Symbol: ColorSy),
    (Name: 'COLS';              Symbol: ColsSy),
    (Name: 'COLSPAN';           Symbol: ColSpanSy),
    (Name: 'CONTENT';           Symbol: ContentSy),
    (Name: 'COORDS';            Symbol: CoordsSy),
    (Name: 'DISABLED';          Symbol: DisabledSy),
    (Name: 'ENCTYPE';           Symbol: EncTypeSy),
    (Name: 'FACE';              Symbol: FaceSy),
    (Name: 'FRAMEBORDER';       Symbol: FrameBorderSy),
    (Name: 'HEIGHT';            Symbol: HeightSy),
    (Name: 'HREF';              Symbol: HrefSy),
    (Name: 'HSPACE';            Symbol: HSpaceSy),
    (Name: 'HTTP-EQUIV';        Symbol: HttpEqSy),
    (Name: 'ID';                Symbol: IDSy),
    (Name: 'ISMAP';             Symbol: IsMapSy),
    (Name: 'LABEL';             Symbol: LabelAttrSy),
    (Name: 'LANGUAGE';          Symbol: LanguageSy),
    (Name: 'LEFTMARGIN';        Symbol: LeftMarginSy),
    (Name: 'LINK';              Symbol: LinkAttrSy),
    (Name: 'LOOP';              Symbol: LoopSy),
    (Name: 'MARGINHEIGHT';      Symbol: MarginHeightSy),
    (Name: 'MARGINWIDTH';       Symbol: MarginWidthSy),
    (Name: 'MAXLENGTH';         Symbol: MaxLengthSy),
    (Name: 'MEDIA';             Symbol: MediaSy),
    (Name: 'METHOD';            Symbol: MethodSy),
    (Name: 'MULTIPLE';          Symbol: MultipleSy),
    (Name: 'NAME';              Symbol: NameSy),
    (Name: 'NOHREF';            Symbol: NoHrefSy),
    (Name: 'NORESIZE';          Symbol: NoResizeSy),
    (Name: 'NOSHADE';           Symbol: NoShadeSy),
    (Name: 'NOWRAP';            Symbol: NoWrapSy),
    (Name: 'OLINK';             Symbol: OLinkSy),
    (Name: 'ONBLUR';            Symbol: OnBlurSy),
    (Name: 'ONCHANGE';          Symbol: OnChangeSy),
    (Name: 'ONCLICK';           Symbol: OnClickSy),
    (Name: 'ONFOCUS';           Symbol: OnFocusSy),
    (Name: 'PLAIN';             Symbol: PlainSy),
    (Name: 'RADIO';             Symbol: RadioSy),
    (Name: 'RATIO';             Symbol: RatioSy),
    (Name: 'READONLY';          Symbol: ReadonlyAttrSy),
    (Name: 'REL';               Symbol: RelSy),
    (Name: 'REV';               Symbol: RevSy),
    (Name: 'ROWS';              Symbol: RowsSy),
    (Name: 'ROWSPAN';           Symbol: RowSpanSy),
    (Name: 'SCROLLING';         Symbol: ScrollingSy),
    (Name: 'SELECTED';          Symbol: SelectedAttrSy),
    (Name: 'SHAPE';             Symbol: ShapeSy),
    (Name: 'SIZE';              Symbol: SizeSy),
    (Name: 'SPAN';              Symbol: SpanAttrSy),
    (Name: 'SRC';               Symbol: SrcSy),
    (Name: 'START';             Symbol: StartSy),
    (Name: 'STYLE';             Symbol: StyleAttrSy),
    (Name: 'TABINDEX';          Symbol: TabIndexSy),
    (Name: 'TARGET';            Symbol: TargetSy),
    (Name: 'TEXT';              Symbol: TextAttrSy),
    (Name: 'TITLE';             Symbol: TitleAttrSy),
    (Name: 'TOPMARGIN';         Symbol: TopMarginSy),
    (Name: 'TRANSP';            Symbol: TranspSy),
    (Name: 'TYPE';              Symbol: TypeSy),
    (Name: 'USEMAP';            Symbol: UseMapSy),
    (Name: 'VALIGN';            Symbol: VAlignSy),
    (Name: 'VALUE';             Symbol: ValueSy),
    (Name: 'VLINK';             Symbol: VLinkSy),
    (Name: 'VSPACE';            Symbol: VSpaceSy),
    (Name: 'WIDTH';             Symbol: WidthSy),
    (Name: 'WRAP';              Symbol: WrapAttrSy)
  );

var
  AttributeDescriptions: ThtStringList;
  AttributeDescriptionsIndex: array [THtmlAttributeSymbol] of Integer;

procedure InitAttributes;
var
  I: Integer;
  P: PAttributeDescription;
  S: THtmlAttributeSymbol;
begin
  // Put the Attributes into a sorted StringList for faster access.
  if AttributeDescriptions = nil then
  begin
    AttributeDescriptions := ThtStringList.Create;
    AttributeDescriptions.CaseSensitive := True;
    for I := low(CAttributeDescriptions) to high(CAttributeDescriptions) do
    begin
      P := @CAttributeDescriptions[I];
      AttributeDescriptions.AddObject(P.Name, Pointer(P));
    end;
    AttributeDescriptions.Sort;

    // initialize AttributeDescriptionsIndex and SymbolNames
    for S := low(S) to high(S) do
      AttributeDescriptionsIndex[S] := -1;
    for I := 0 to AttributeDescriptions.Count - 1 do
    begin
      P := PAttributeDescription(AttributeDescriptions.Objects[I]);
      AttributeDescriptionsIndex[P.Symbol] := I;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function TryStrToAttributeSymbol(const Str: ThtString; out Sy: THtmlAttributeSymbol): Boolean;
var
  I: Integer;
begin
  Result := AttributeDescriptions.Find(Str, I);
  if Result then
    Sy := PAttributeDescription(AttributeDescriptions.Objects[I]).Symbol
  else
    Sy := UnknownAttrSy;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
function AttributeSymbolToStr(Sy: THtmlAttributeSymbol): ThtString;
begin
  Result := PAttributeDescription(AttributeDescriptions.Objects[AttributeDescriptionsIndex[Sy]]).Name;
end;



{ THtmlAttribute }

//-- BG ---------------------------------------------------------- 26.03.2011 --
constructor THtmlAttribute.Create(Symbol: THtmlAttributeSymbol; const Name, Value: ThtString);
begin
  inherited Create;
  FSymbol := Symbol;
  FName := Name;
  FValue := Value;
end;

{ THtmlAttributeList }

//-- BG ---------------------------------------------------------- 26.03.2011 --
function THtmlAttributeList.Find(Name: ThtString; out Attribute: THtmlAttribute): Boolean;
var
  Symbol: THtmlAttributeSymbol;
begin
  if TryStrToAttributeSymbol(Name, Symbol) then
    Result := Find(Symbol, Attribute)
  else
  begin
    Attribute := nil;
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function THtmlAttributeList.Find(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Attribute := GetAttribute(I);
    Result := Attribute.Symbol = Symbol;
    if Result then
      exit;
  end;
  Attribute := nil;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
function THtmlAttributeList.GetAttribute(Index: Integer): THtmlAttribute;
begin
  Result := Get(Index);
end;

{ THtmlElement }

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure THtmlElement.AddChild(Child: THtmlElement);
begin
  if FChildren = nil then
    FChildren := TObjectList.Create;
  FChildren.Add(Child);
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
constructor THtmlElement.Create(Parent: THtmlElement; Symbol: THtmlElementSymbol; Attributes: THtmlAttributeList; DocPos: Integer);
begin
  inherited Create;
  FSymbol := Symbol;
  FAttributes := Attributes;
  FDocPos := DocPos;
  SetParent(Parent);
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
destructor THtmlElement.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure THtmlElement.ExtractChild(Child: THtmlElement);
begin
  if FChildren <> nil then
    FChildren.Extract(Child);
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.FindAttribute(Name: ThtString; out Attribute: THtmlAttribute): Boolean;
begin
  if FAttributes <> nil then
    Result := FAttributes.Find(Name, Attribute)
  else
  begin
    Attribute := nil;
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.FindAttribute(Symbol: THtmlAttributeSymbol; out Attribute: THtmlAttribute): Boolean;
begin
  if FAttributes <> nil then
    Result := FAttributes.Find(Symbol, Attribute)
  else
  begin
    Attribute := nil;
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlElement.GetChild(Index: Integer): THtmlElement;
begin
  Result := THtmlElement(FChildren[Index]);
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
function THtmlElement.GetChildCount: Integer;
begin
  if FChildren <> nil then
    Result := FChildren.Count
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.GetPseudos: TPseudos;
begin
  Result := [];
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlElement.IndexOf(Child: THtmlElement): Integer;
begin
  if FChildren <> nil then
    Result := FChildren.IndexOf(Child)
  else
    Result := -1;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlElement.IsMatching(Selector: TSelector): Boolean;

  function IsMatchingSimple: Boolean;

    function IncludesStringArray(S, F: ThtStringArray): Boolean;
    var
      I: Integer;
    begin
      Result := Length(S) <= Length(F);
      if not Result then
        exit;
      for I := Low(S) to High(S) do
        if IndexOfString(F, S[I]) < 0 then
          exit;
      Result := True;
    end;

  var
    Index: Integer;
    Attribute: THtmlAttribute;
    Match: TAttributeMatch;
    S: THtmlElementSymbol;
  begin
    Result := False;

    // http://www.w3.org/TR/2010/WD-CSS2-20101207/selector.html
    // If all conditions in the selector are true for a certain element, the selector matches the element.

    if Selector.Pseudos <> [] then
      if not (Selector.Pseudos >= GetPseudos) then
        exit;

    // a loop about tags? there is one or none tag in the selector.
    for Index := Low(Selector.Tags) to High(Selector.Tags) do
      if TryStrToElementSymbol(Selector.Tags[Index], S) then
        if S <> FSymbol then
          exit;

    // a loop about ids? CSS 2.1 allows more than 1 ID, but most browsers do not support them.
    if not IncludesStringArray(Selector.Ids, FIds) then
      exit;

    if not IncludesStringArray(Selector.Classes, FClasses) then
      exit;

    for Index := 0 to Selector.AttributeMatchesCount - 1 do
    begin
      Match := Selector.AttributeMatches[Index];
      if not FindAttribute(Match.Name, Attribute) then
        exit;
      case Match.Oper of
        //no more checks here. Attribute it set! amoSet: ;       // [name] : matches, if attr is set and has any value.

        amoEquals:     // [name=value] : matches, if attr equals value.
          if htCompareString(Match.Value, Attribute.AsString) <> 0 then
            break;

        amoContains:   // [name~=value] : matches, if attr is a white space separated list of values and value is one of these values.
          if PosX(Match.Value + ' ', Attribute.AsString + ' ', 1) = 0 then
            break;

        amoStartsWith: // [name|=value] : matches, if attr equals value or starts with value immediately followed by a hyphen.
          if PosX(Match.Value + '-', Attribute.AsString + '-', 1) <> 1 then
            break;
        end;
      end;

    Result := True;
  end;

  function IsChild(Selector: TSelector): Boolean;
  var
    P: THtmlElement;
  begin
    P := Parent;
    Result := (P <> nil) and P.IsMatching(Selector);
  end;

  function IsDescendant(Selector: TSelector): Boolean;
  var
    Node: THtmlElement;
  begin
    Result := False;
    Node := Parent;
    while Node <> nil do
    begin
      Result := Node.IsMatching(Selector);
      if Result then
        break;
      Node := Node.Parent;
    end;
  end;

  function IsFollower(Selector: TSelector): Boolean;
  var
    P: THtmlElement;
    I: Integer;
  begin
    P := Parent;
    Result := P <> nil;
    if Result then
    begin
      I := P.IndexOf(Self);
      if I > 0 then
        Result := P[I - 1].IsMatching(Selector);
    end;
  end;

begin
  Result := IsMatchingSimple;
  if Result then
    if Selector is TCombinedSelector then
      case TCombinedSelector(Selector).Combinator of
        scChild:      Result := IsChild(TCombinedSelector(Selector).LeftHand);
        scDescendant: Result := IsDescendant(TCombinedSelector(Selector).LeftHand);
        scFollower:   Result := IsFollower(TCombinedSelector(Selector).LeftHand);
      end;
end;

//-- BG ---------------------------------------------------------- 26.03.2011 --
procedure THtmlElement.SetParent(Parent: THtmlElement);
begin
  if FParent <> Parent then
  begin
    if FParent <> nil then
      FParent.ExtractChild(Self);
    FParent := Parent;
    if FParent <> nil then
      FParent.AddChild(Self);
  end;
end;


initialization
  InitAttributes;
  InitElementDescriptions;
finalization
  AttributeDescriptions.Free;
  ElementDescriptions.Free;
end.
