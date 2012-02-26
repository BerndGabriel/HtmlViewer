{
Version   11.5
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
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

unit HTMLUn2;

interface
uses
{$ifdef LCL}
  LclIntf, IntfGraphics, FpImage, LclType, LResources, LMessages, HtmlMisc,
{$else}
  Windows,
{$endif}
  SysUtils, Contnrs, Classes, Graphics, ClipBrd, Controls, ExtCtrls, Messages, Variants, Types,
{$IFNDEF NoGDIPlus}
  GDIPL2A,
{$ENDIF}
{$ifdef METAFILEMISSING}
  MetaFilePrinter,
{$endif}
  HtmlBuffer,
  HtmlGif2,
  HtmlGlobals,
  HtmlImages,
  StyleTypes,
  StyleUn,
  UrlSubs;

const
  VersionNo = '11.5';
  MaxHScroll = 100000; {max horizontal display in pixels}
  Tokenleng = 300;
  TopLim = -200; {drawing limits}
  BotLim = 5000;
  FmCtl = WideChar(#2);
  ImgPan = WideChar(#4);
  BrkCh = WideChar(#8);

type
  THtQuirksMode = (qmDetect, qmStandards, qmQuirks);
  // BG, 26.12.2011:
  TWidthType = (
    wtNone,
    wtAbsolute,
    wtPercent,
    wtRelative);

  // BG, 26.12.2011:
  TSpecWidth = record
    Value: Integer;
    VType: TWidthType; // treat wtNone like "0*" (Value = 0.0, CType = wtRelative)
  end;

  //BG, 09.09.2009: renamed TGif and TPng to TrGif and TrPng
  //  TGif interfered with same named class.
  JustifyType = (NoJustify, Left, Centered, Right, FullJustify);
  TRowType = (THead, TBody, TFoot);

  Symb = (
    HtmlSy, TitleSy, BodySy, HeadSy, PSy, PEndSy, BSy, BEndSy, ISy, IEndSy,
    HtmlEndSy, TitleEndSy, BodyEndSy, HeadEndSy, BRSy, HeadingSy, HeadingEndSy,
    EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, HRSy,
    CiteSy, VarSy, CiteEndSy, VarEndSy, BaseSy,
       {Keep order}
    TTSy, CodeSy, KbdSy, SampSy, TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
       {end order}
    OLSy, OLEndSy, LISy, LIEndSy, ULSy, ULEndSy, DirSy, DirEndSy, MenuSy, MenuEndSy,
    DLSy, DLEndSy, DDSy, DDEndSy, DTSy, DTEndSy, AddressSy, AddressEndSy,
    BlockQuoteSy, BlockQuoteEndSy, PreSy, PreEndSy, ImageSy, Centersy, CenterEndSy,
    OtherAttribute, ASy, AEndSy, HrefSy, NameSy, SrcSy, AltSy, AlignSy,
    OtherChar, OtherSy, CommandSy, TextSy, EofSy, LinkSy, BGColorSy,
    BackgroundSy, TableSy, TableEndSy, TDSy, TDEndSy, TRSy, TREndSy, THSy, THEndSy,
    ColSpanSy, RowSpanSy, BorderSy, CellPaddingSy, CellSpacingSy, VAlignSy,
    WidthSy, CaptionSy, CaptionEndSy, StartSy, ButtonSy, InputSy, ValueSy,
    TypeSy, CheckBoxSy, RadioSy, FieldsetSy, FieldsetEndSy, LegendSy, LegendEndSy,
    FormSy, FormEndSy, MethodSy, ActionSy,
    CheckedSy, SizeSy, MaxLengthSy, TextAreaSy, TextAreaEndSy, ColsSy,
    RowsSy, SelectSy, SelectEndSy, OptionSy, OptionEndSy, SelectedSy,
    MultipleSy, FontSy, FontEndSy, ColorSy, FaceSy, BaseFontSy,
    TranspSy, SubSy, SubEndSy, SupSy, SupEndSy, ClearSy, IsMapSy,
    BigSy, BigEndSy, SmallSy, SmallEndSy, BorderColorSy, MapSy, MapEndSy,
    AreaSy, ShapeSy, CoordsSy, NoHrefSy, UseMapSy, HeightSy, PlainSy,
    FrameSetSy, FrameSetEndSy, FrameSy, TargetSy, NoFramesSy, NoFramesEndSy,
    NoResizeSy, ScrollingSy, PageSy, HSpaceSy, VSpaceSy, ScriptSy, ScriptEndSy,
    LanguageSy, DivSy, DivEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy,
    FrameBorderSy, MarginWidthSy, MarginHeightSy, BgSoundSy, LoopSy,
    OnClickSy, WrapSy, NoShadeSy, MetaSy, HttpEqSy, ContentSy, EncTypeSy,
    VLinkSy, OLinkSy, ActiveSy, PanelSy, NoBrSy, NoBrEndSy, WbrSy,
    ClassSy, IDSy, StyleSy, StyleEndSy, SpanSy, SpanEndSy, liAloneSy,
    RelSy, RevSy, NoWrapSy, BorderColorLightSy, BorderColorDarkSy,
    CharSetSy, RatioSy, OnFocusSy, OnBlurSy, OnChangeSy, ColSy, ColGroupSy,
    ColGroupEndSy, TabIndexSy, BGPropertiesSy, DisabledSy,
    TopMarginSy, LeftMarginSy, LabelSy, LabelEndSy, THeadSy, TBodySy, TFootSy,
    THeadEndSy, TBodyEndSy, TFootEndSy, ObjectSy, ObjectEndSy, ParamSy,
    ReadonlySy, EolSy, MediaSy, IFrameSy, IFrameEndSy,
    {HTML5 elements}
    HeaderSy, HeaderEndSy,
    SectionSy, SectionEndSy,
    NavSy, NavEndSy,
    ArticleSy, ArticleEndSy,
    AsideSy, AsideEndSy,
    FooterSy, FooterEndSy,
    HGroupSy, HGroupEndSy);

//------------------------------------------------------------------------------

  { Like TList but frees it's items. Use only descendents of TObject! }
  //BG, 03.03.2011: what about TObjectList?
  TFreeList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

//------------------------------------------------------------------------------
// tag attributes
//------------------------------------------------------------------------------

  TAttribute = class(TObject) {holds a tag attribute}
  public
    Which: Symb; {symbol of attribute such as HrefSy}
    WhichName: ThtString;
    Value: Integer; {numeric value if appropriate}
    DblValue: Double; {numeric value if appropriate}
    xPercent: boolean; {if value is in percent}
    Name: ThtString; {ThtString (mixed case), value after '=' sign}
    CodePage: Integer;
    constructor Create(ASym: Symb; const AValue: Double; const NameStr, ValueStr: ThtString; ACodePage: Integer);
    property AsString: ThtString read Name;
    property AsInteger: Integer read Value;
    property AsDouble: Double read DblValue;
  end;

  TAttributeList = class(TFreeList) {a list of tag attributes,(TAttributes)}
  private
    Prop: TProperties;
    SaveID: ThtString;
    function GetClass: ThtString;
    function GetID: ThtString;
    function GetTitle: ThtString;
    function GetStyle: TProperties;
    function GetAttribute(Index: Integer): TAttribute; {$ifdef UseInline} inline; {$endif}
  public
    destructor Destroy; override;
    procedure Clear; override;
    function Find(Sy: Symb; var T: TAttribute): boolean; overload; {$ifdef UseInline} inline; {$endif}
    function Find(SyName: ThtString; var T: TAttribute): boolean; overload; {$ifdef UseInline} inline; {$endif}
    function CreateStringList: ThtStringList;
    property TheClass: ThtString read GetClass;
    property TheID: ThtString read GetID;
    property TheTitle: ThtString read GetTitle;
    property TheStyle: TProperties read GetStyle;
    property Items[Index: Integer]: TAttribute read GetAttribute; default;
  end;

//------------------------------------------------------------------------------
// copy to clipboard support
//------------------------------------------------------------------------------

  SelTextCount = class(TObject)
  private
    Buffer: PWideChar;
    BufferLeng: Integer;
    Leng: Integer;
  public
    procedure AddText(P: PWideChar; Size: Integer); virtual;
    procedure AddTextCR(P: PWideChar; Size: Integer); {$ifdef UseInline} inline; {$endif}
    function Terminate: Integer; virtual;
  end;

  SelTextBuf = class(SelTextCount)
  public
    constructor Create(ABuffer: PWideChar; Size: Integer);
    procedure AddText(P: PWideChar; Size: Integer); override;
    function Terminate: Integer; override;
  end;

  ClipBuffer = class(SelTextBuf)
  private
    procedure CopyToClipboard;
  public
    constructor Create(Leng: Integer);
    destructor Destroy; override;
    function Terminate: Integer; override;
  end;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

  {holds start and end point of URL text}
  TutText = record //BG, 03.03.2011: changed to record. no need to use a class
    Start: Integer;
    Last: Integer;
  end;

  TUrlTarget = class(TObject)
  public
    URL: ThtString;
    Target: ThtString;
    ID: Integer;
    Attr: ThtString;
    utText: TutText;
    TabIndex: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const AnUrl, ATarget: ThtString; L: TAttributeList; AStart: Integer); overload;
    procedure Assign(const UT: TUrlTarget); overload;
    procedure Clear;
    procedure SetLast(List: TList {of TFontObjBase}; ALast: Integer);
    property Start: Integer read utText.Start;
    property Last: Integer read utText.Last;
  end;

  // BG, 31.12.2011:
  TMapArea = class(TObject)
  private
    FHRef: ThtString;
    FRegion: THandle;
    FTarget: ThtString;
    FTitle: ThtString;
  public
    function PtInRegion(X, Y: Integer): Boolean;
    property HRef: ThtString read FHRef;
    property Target: ThtString read FTarget;
    property Title: ThtString read FTitle;
  end;

  // BG, 31.12.2011: 
  TMapAreaList = class(TObjectList)
  private
    function getArea(Index: Integer): TMapArea;
  public
    property Items[Index: Integer]: TMapArea read getArea; default;
  end;

  TMapItem = class(TObject) {holds a client map info}
  private
    FAreas: TMapAreaList;
  public
    MapName: ThtString;
    constructor Create;
    destructor Destroy; override;
    function GetURL(X, Y: Integer; var URLTarg: TURLTarget; var ATitle: ThtString): boolean;
    procedure AddArea(Attrib: TAttributeList);
  end;

  TFontObjBase = class(TObject) {font information}
  public
    UrlTarget: TUrlTarget;
  end;

//------------------------------------------------------------------------------
// indentation manager
//------------------------------------------------------------------------------

  IndentRec = class(TObject)
    X: Integer;   // left or right indentation relative to LfEdge.
    YT: Integer;  // top Y inclusive coordinate for this record relative to document top.
    YB: Integer;  // bottom Y exclusive coordinate for this record relative to document top.
    ID: TObject;  // block level indicator for this record, 0 for not applicable
  end;

  TIndentManager = class(TObject)
  private
    function LeftEdge(Y: Integer): Integer;
    function RightEdge(Y: Integer): Integer;
  public
    LfEdge: Integer;    // left edge of the block content area.
                        // TCell.DoLogic calculates with LfEdge = 0.
                        // TCell.Draw then may shift the block by setting LfEdge to X.
    Width: Integer;     // width of the block content area.
    ClipWidth: Integer; // clip width ???
    L: TFreeList;       // list of left side indentations of type IndentRec.
    R: TFreeList;       // list of right side indentations of type IndentRec.
    CurrentID: TObject; // the current block level (a TBlock pointer)
  public
    constructor Create;
    destructor Destroy; override;
    function AddLeft(YT, YB, W: Integer): IndentRec;
    function AddRight(YT, YB, W: Integer): IndentRec;
    function AlignLeft(var Y: Integer; W: Integer; SpW: Integer = 0; SpH: Integer = 0): Integer;
    function AlignRight(var Y: Integer; W: Integer; SpW: Integer = 0; SpH: Integer = 0): Integer;
    function GetNextWiderY(Y: Integer): Integer;
    function ImageBottom: Integer;
    function LeftIndent(Y: Integer): Integer;
    function RightSide(Y: Integer): Integer;
    function SetLeftIndent(XLeft, Y: Integer): Integer;
    function SetRightIndent(XRight, Y: Integer): Integer;
    procedure FreeLeftIndentRec(I: Integer);
    procedure FreeRightIndentRec(I: Integer);
    procedure GetClearY(var CL, CR: Integer);
    procedure Init(Lf, Wd: Integer);
    procedure Reset(Lf: Integer);
  end;

//------------------------------------------------------------------------------
// parser
//------------------------------------------------------------------------------

  IndexArray = array[1..TokenLeng] of Integer;
  PIndexArray = ^IndexArray;
  ChrArray = array[1..TokenLeng] of WideChar;
  PChrArray = ^ChrArray;

  {Simplified variant of TokenObj, to temporarily keep a ThtString of ANSI
   characters along with their original indices.}
  TCharCollection = class
  private
    FChars: ThtString;
    FIndices: PIndexArray;
    FCurrentIndex: Integer;
    function GetSize: Integer;
    function GetAsString: ThtString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(C: ThtChar; Index: Integer); overload;
    procedure Add(const S: ThtString; Index: Integer); overload;
    procedure Clear;
//    procedure Concat(const T: TCharCollection);

    property AsString: ThtString read GetAsString;
    property Indices: PIndexArray read FIndices;
    property Size: Integer read GetSize;
  end;

  TokenObj = class
  private
    St: UnicodeString;
    StringOK: boolean;
    FCapacity: Integer;
    FCount: Integer;
    function GetString: UnicodeString;
    procedure SetCapacity(NewCapacity: Integer);
  public
    C: PChrArray;
    I: PIndexArray;
    constructor Create;
    destructor Destroy; override;
    procedure AddUnicodeChar(Ch: WideChar; Ind: Integer);
    procedure AddString(S: TCharCollection);
//    procedure Concat(T: TokenObj);
    procedure Clear;
//    procedure Remove(N: Integer);
//    procedure Replace(N: Integer; Ch: WideChar);

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property S: UnicodeString read GetString;
  end;

//------------------------------------------------------------------------------
// TIDObject is base class for all objects the HtmlViewer can scroll to.
//------------------------------------------------------------------------------
// If they have an ID, the parser puts them into the HtmlViewer's IDNameList,
// a TIDObjectList, where they can be obtained from by ID.
// Their Y coordinates can be retrieved and HtmlViewer can scroll to them.
//
// Most decendents are objects representing an HTML tag, except TChPosObj.
//------------------------------------------------------------------------------

  TIDObject = class(TObject)
  protected
    function GetYPosition: Integer; virtual; abstract;
    function FreeMe: Boolean; virtual; // some objects the TIDObjectsList owns, some others not.
  public
    property YPosition: Integer read GetYPosition;
  end;

  //BG, 04.03.2011: TIDNameList renamed to TIDObjectList and used TObject changed to TIDObject.
  TIDObjectList = class(ThtStringList)
  private
    function GetObject(Index: Integer): TIDObject; reintroduce;
  public
    constructor Create;
    destructor Destroy; override;
    function AddObject(const S: ThtString; AObject: TIDObject): Integer; reintroduce;
    procedure Clear; override;
    property Objects[Index: Integer]: TIDObject read GetObject; default;
  end;

  htColorArray = packed array[0..3] of TColor;
  htBorderStyleArray = packed array[0..3] of BorderStyleType;

//BG, 11.09.2010: moved to this unit to reduce circular dependencies:

  guResultType = set of (guUrl, guControl, guTitle);

//------------------------------------------------------------------------------
// ThvPanel is base class for panels held in TPanelObj
//------------------------------------------------------------------------------

  ThvPanel = class(TPanel)
  public
    FVisible: boolean;
    procedure SetVisible(Value: boolean);
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  TPanelCreateEvent = procedure(Sender: TObject; const AName, AType, SRC: ThtString; Panel: ThvPanel) of object;
  TPanelDestroyEvent = procedure(Sender: TObject; Panel: ThvPanel) of object;
  TPanelPrintEvent = procedure(Sender: TObject; Panel: ThvPanel; const Bitmap: TBitmap) of object;
  TObjectTagEvent = procedure(Sender: TObject; Panel: ThvPanel; const Attributes, Params: ThtStringList; var WantPanel: boolean) of object;
  TObjectClickEvent = procedure(Sender, Obj: TObject; const OnClick: ThtString) of object;
  ThtObjectEvent = procedure(Sender, Obj: TObject; const Attribute: ThtString) of object;

//------------------------------------------------------------------------------
// ThtControlBase is base class for TViewerBase and TFrameBase
//------------------------------------------------------------------------------

  ThtControlBase = class(TWinControl);

//------------------------------------------------------------------------------
// TViewerBase is base class for THtmlViewer and TFrameViewer
//------------------------------------------------------------------------------

  TGetStreamEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TMemoryStream) of object;

  THotSpotTargetClickEvent = procedure(Sender: TObject; const Target, URL: ThtString; var Handled: boolean) of object;
  THotSpotTargetEvent = procedure(Sender: TObject; const Target, URL: ThtString) of object;
  ThtProgressEvent = procedure(Sender: TObject; Stage: TProgressStage; PercentDone: Integer) of object;
  TImageClickEvent = procedure(Sender, Obj: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TImageOverEvent = procedure(Sender, Obj: TObject; Shift: TShiftState; X, Y: Integer) of object;
  TIncludeType = procedure(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer) of object;
  TLinkType = procedure(Sender: TObject; const Rel, Rev, Href: ThtString) of object;
  TMetaType = procedure(Sender: TObject; const HttpEq, Name, Content: ThtString) of object;
  TPagePrinted = procedure(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean) of object;
  TParseEvent = procedure(Sender: TObject; var Source: TBuffer) of object;
  TProcessingEvent = procedure(Sender: TObject; ProcessingOn: Boolean) of object;
  TScriptEvent = procedure(Sender: TObject; const Name, ContentType, Src, Script: ThtString) of object;
  TSoundType = procedure(Sender: TObject; const SRC: ThtString; Loop: Integer; Terminate: boolean) of object;
  THTMLViewPrinted = TNotifyEvent;
  THTMLViewPrinting = procedure(Sender: TObject; var StopPrinting: Boolean) of object;
  TLinkDrawnEvent = procedure(Sender: TObject; Page: Integer; const Url, Target: ThtString; ARect: TRect) of object;
  TFileBrowseEvent = procedure(Sender, Obj: TObject; var S: ThtString) of object;
  TGetBitmapEvent = procedure(Sender: TObject; const SRC: ThtString; var Bitmap: TBitmap; var Color: TColor) of object;
  TGetImageEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TStream) of object;
  TGottenImageEvent = TGetImageEvent;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList) of object;

  TViewerBase = class(ThtControlBase)
  private
    FBackGround, FHotSpotColor, FVisitedColor, FOverColor: TColor;
    FCharset: TFontCharset;
    FCodePage: Integer;
    FFontColor: TColor;
    FFontName, FPreFontName: TFontName;
    FFontSize: Integer;
    FHistoryMaxCount, FImageCacheCount, FVisitedMaxCount: Integer;
    FMarginWidth, FMarginHeight: Integer;
    FNoSelect: Boolean;
    FPrintMarginLeft, FPrintMarginRight, FPrintMarginTop, FPrintMarginBottom: Double;
    FPrintMaxHPages: Integer;
    FPrintScale: Double;
    FServerRoot: ThtString;
    //
    FOnBitmapRequest: TGetBitmapEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnHistoryChange: TNotifyEvent;
    FOnHotSpotTargetClick: THotSpotTargetClickEvent;
    FOnHotSpotTargetCovered: THotSpotTargetEvent;
    FOnImageClick: TImageClickEvent;
    FOnImageOver: TImageOverEvent;
    FOnImageRequest: TGetImageEvent;
    FOnImageRequested: TGottenImageEvent;
    FOnInclude: TIncludeType;
    FOnLink: TLinkType;
    FOnMeta: TMetaType;
    FOnMouseDouble: TMouseEvent;
    FOnObjectBlur: ThtObjectEvent;
    FOnObjectChange: ThtObjectEvent;
    FOnObjectClick: TObjectClickEvent;
    FOnObjectFocus: ThtObjectEvent;
    FOnObjectTag: TObjectTagEvent;
    FOnPanelCreate: TPanelCreateEvent;
    FOnPanelDestroy: TPanelDestroyEvent;
    FOnPanelPrint: TPanelPrintEvent;
    FOnParseBegin: TParseEvent;
    FOnParseEnd: TNotifyEvent;
    FOnPrinted: THTMLViewPrinted;
    FOnPrintHeader, FOnPrintFooter: TPagePrinted;
    FOnPrinting: THTMLViewPrinting;
    FOnProcessing: TProcessingEvent;
    FOnProgress: ThtProgressEvent;
    FOnScript: TScriptEvent;
    FOnSoundRequest: TSoundType;
    FQuirksMode : THtQuirksMode;
  protected
    procedure SetQuirksMode(const AValue: THtQuirksMode); virtual;
    procedure SetActiveColor(const Value: TColor); virtual;
    procedure SetCharset(const Value: TFontCharset); virtual;
    procedure SetCodePage(const Value: Integer); virtual;
    procedure SetDefBackground(const Value: TColor); virtual;
    procedure SetFontColor(const Value: TColor); virtual;
    procedure SetFontName(const Value: TFontName); virtual;
    procedure SetFontSize(const Value: Integer); virtual;
    procedure SetHistoryMaxCount(const Value: Integer); virtual;
    procedure SetHotSpotColor(const Value: TColor); virtual;
    procedure SetImageCacheCount(const Value: Integer); virtual;
    procedure SetMarginHeight(const Value: Integer); virtual;
    procedure SetMarginWidth(const Value: Integer); virtual;
    procedure SetNoSelect(const Value: Boolean); virtual;
    procedure SetOnBitmapRequest(const Value: TGetBitmapEvent); virtual;
    procedure SetOnDragDrop(const Value: TDragDropEvent); virtual;
    procedure SetOnDragOver(const Value: TDragOverEvent); virtual;
    procedure SetOnHistoryChange(const Value: TNotifyEvent); virtual;
    procedure SetOnHotSpotTargetClick(const Value: THotSpotTargetClickEvent); virtual;
    procedure SetOnHotSpotTargetCovered(const Value: THotSpotTargetEvent); virtual;
    procedure SetOnImageClick(const Value: TImageClickEvent); virtual;
    procedure SetOnImageOver(const Value: TImageOverEvent); virtual;
    procedure SetOnImageRequest(const Value: TGetImageEvent); virtual;
    procedure SetOnImageRequested(const Value: TGottenImageEvent); virtual;
    procedure SetOnInclude(const Handler: TIncludeType); virtual;
    procedure SetOnLink(const Handler: TLinkType); virtual;
    procedure SetOnMeta(const Value: TMetaType); virtual;
    procedure SetOnMouseDouble(const Value: TMouseEvent); virtual;
    procedure SetOnObjectBlur(const Value: ThtObjectEvent); virtual;
    procedure SetOnObjectChange(const Value: ThtObjectEvent); virtual;
    procedure SetOnObjectClick(const Value: TObjectClickEvent); virtual;
    procedure SetOnObjectFocus(const Value: ThtObjectEvent); virtual;
    procedure SetOnObjectTag(const Value: TObjectTagEvent); virtual;
    procedure SetOnPanelCreate(const Value: TPanelCreateEvent); virtual;
    procedure SetOnPanelDestroy(const Value: TPanelDestroyEvent); virtual;
    procedure SetOnPanelPrint(const Value: TPanelPrintEvent); virtual;
    procedure SetOnParseBegin(const Value: TParseEvent); virtual;
    procedure SetOnParseEnd(const Value: TNotifyEvent); virtual;
    procedure SetOnPrinted(const Value: THTMLViewPrinted); virtual;
    procedure SetOnPrintFooter(const Value: TPagePrinted); virtual;
    procedure SetOnPrintHeader(const Value: TPagePrinted); virtual;
    procedure SetOnPrinting(const Value: THTMLViewPrinting); virtual;
    procedure SetOnProcessing(const Value: TProcessingEvent); virtual;
    procedure SetOnProgress(const Value: ThtProgressEvent); virtual;
    procedure SetOnScript(const Handler: TScriptEvent); virtual;
    procedure SetOnSoundRequest(const Handler: TSoundType); virtual;
    procedure SetPreFontName(const Value: TFontName); virtual;
    procedure SetPrintMarginBottom(const Value: Double); virtual;
    procedure SetPrintMarginLeft(const Value: Double); virtual;
    procedure SetPrintMarginRight(const Value: Double); virtual;
    procedure SetPrintMarginTop(const Value: Double); virtual;
    procedure SetPrintMaxHPages(const Value: Integer); virtual;
    procedure SetPrintScale(const Value: Double); virtual;
    procedure SetServerRoot(const Value: ThtString); virtual;
    procedure SetVisitedColor(const Value: TColor); virtual;
    procedure SetVisitedMaxCount(const Value: Integer); virtual;
    procedure ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ViewerDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateCopy(Owner: TComponent; Source: TViewerBase); virtual;
    // Load(Url): Url might be an absolute Url or an absolute PathName or a relative Url/PathName.
    procedure Load(const Url: ThtString); virtual; abstract;
    property QuirksMode : THtQuirksMode read FQuirksMode write SetQuirksMode;
    property CodePage: Integer read FCodePage write SetCodePage;
    property CharSet: TFontCharset read FCharSet write SetCharset;
    property DefBackground: TColor read FBackground write SetDefBackground default clBtnFace;
    property DefFontColor: TColor read FFontColor write SetFontColor default clBtnText;
    property DefFontName: TFontName read FFontName write SetFontName;
    property DefFontSize: Integer read FFontSize write SetFontSize default 12;
    property DefHotSpotColor: TColor read FHotSpotColor write SetHotSpotColor default clBlue;
    property DefOverLinkColor: TColor read FOverColor write SetActiveColor default clBlue;
    property DefPreFontName: TFontName read FPreFontName write SetPreFontName;
    property DefVisitedLinkColor: TColor read FVisitedColor write SetVisitedColor default clPurple;
    property HistoryMaxCount: Integer read FHistoryMaxCount write SetHistoryMaxCount;
    property ImageCacheCount: Integer read FImageCacheCount write SetImageCacheCount default 5;
    property MarginHeight: Integer read FMarginHeight write SetMarginHeight default 5;
    property MarginWidth: Integer read FMarginWidth write SetMarginWidth default 10;
    property NoSelect: Boolean read FNoSelect write SetNoSelect;
    property PrintMarginBottom: Double read FPrintMarginBottom write SetPrintMarginBottom;
    property PrintMarginLeft: Double read FPrintMarginLeft write SetPrintMarginLeft;
    property PrintMarginRight: Double read FPrintMarginRight write SetPrintMarginRight;
    property PrintMarginTop: Double read FPrintMarginTop write SetPrintMarginTop;
    property PrintMaxHPages: Integer read FPrintMaxHPages write SetPrintMaxHPages default 2;
    property PrintScale: Double read FPrintScale write SetPrintScale;
    property ServerRoot: ThtString read FServerRoot write SetServerRoot;
    property VisitedMaxCount: Integer read FVisitedMaxCount write SetVisitedMaxCount default 50;
    //
    property OnBitmapRequest: TGetBitmapEvent read FOnBitmapRequest write SetOnBitmapRequest;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write SetOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write SetOnDragOver;
    property OnHistoryChange: TNotifyEvent read FOnHistoryChange write SetOnHistoryChange;
    property OnHotSpotTargetClick: THotSpotTargetClickEvent read FOnHotSpotTargetClick write SetOnHotSpotTargetClick;
    property OnHotSpotTargetCovered: THotSpotTargetEvent read FOnHotSpotTargetCovered write SetOnHotSpotTargetCovered;
    property OnImageClick: TImageClickEvent read FOnImageClick write SetOnImageClick;
    property OnImageOver: TImageOverEvent read FOnImageOver write SetOnImageOver;
    property OnImageRequest: TGetImageEvent read FOnImageRequest write SetOnImageRequest;
    property OnImageRequested: TGottenImageEvent read FOnImageRequested write SetOnImageRequested;
    property OnInclude: TIncludeType read FOnInclude write SetOnInclude;
    property OnLink: TLinkType read FOnLink write SetOnLink;
    property OnMeta: TMetaType read FOnMeta write SetOnMeta;
    property OnMouseDouble: TMouseEvent read FOnMouseDouble write SetOnMouseDouble;
    property OnObjectBlur: ThtObjectEvent read FOnObjectBlur write SetOnObjectBlur;
    property OnObjectChange: ThtObjectEvent read FOnObjectChange write SetOnObjectChange;
    property OnObjectClick: TObjectClickEvent read FOnObjectClick write SetOnObjectClick;
    property OnObjectFocus: ThtObjectEvent read FOnObjectFocus write SetOnObjectFocus;
    property OnObjectTag: TObjectTagEvent read FOnObjectTag write SetOnObjectTag;
    property OnPanelCreate: TPanelCreateEvent read FOnPanelCreate write SetOnPanelCreate;
    property OnPanelDestroy: TPanelDestroyEvent read FOnPanelDestroy write SetOnPanelDestroy;
    property OnPanelPrint: TPanelPrintEvent read FOnPanelPrint write SetOnPanelPrint;
    property OnParseBegin: TParseEvent read FOnParseBegin write SetOnParseBegin;
    property OnParseEnd: TNotifyEvent read FOnParseEnd write SetOnParseEnd;
    property OnPrinted: THTMLViewPrinted read FOnPrinted write SetOnPrinted;
    property OnPrintFooter: TPagePrinted read FOnPrintFooter write SetOnPrintFooter;
    property OnPrintHeader: TPagePrinted read FOnPrintHeader write SetOnPrintHeader;
    property OnPrinting: THTMLViewPrinting read FOnPrinting write SetOnPrinting;
    property OnProcessing: TProcessingEvent read FOnProcessing write SetOnProcessing;
    property OnProgress: ThtProgressEvent read FOnProgress write SetOnProgress;
    property OnScript: TScriptEvent read FOnScript write SetOnScript;
    property OnSoundRequest: TSoundType read FOnSoundRequest write SetOnSoundRequest;
  end;

  TViewerBaseClass = class of TViewerBase;

  TablePartType = (Normal, DoHead, DoBody1, DoBody2, DoBody3, DoFoot);
  TTablePartRec = class
    TablePart: TablePartType;
    PartStart: Integer;
    PartHeight: Integer;
    FootHeight: Integer;
  end;

  THtmlViewerBase = class(TViewerBase)
  protected
    // set to determine if child objects should be in "quirks" mode
    //This must be protected because it's set directly in a descendant
    FUseQuirksMode : Boolean;
  public
    TablePartRec: TTablePartRec;
    function HtmlExpandFilename(const Filename: ThtString): ThtString; virtual; abstract;
    function ShowFocusRect: Boolean; virtual; abstract;
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure htProgress(Percent: Integer); virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    // set to determine if child objects should be in "quirks" mode
    property UseQuirksMode : Boolean read FUseQuirksMode;
  end;

  TFrameViewerBase = class(TViewerBase)
  private
    procedure wmerase(var msg: TMessage); message WM_ERASEBKGND;
  public
    function CreateSubFrameSet(FrameSet: TObject): TObject; virtual; abstract;
    procedure AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: ThtString); virtual; abstract;
    procedure DoAttributes(FrameSet: TObject; Attr: TAttributeList); virtual; abstract;
  end;

//------------------------------------------------------------------------------
// string methods
//------------------------------------------------------------------------------

function StrLenW(Str: PWideChar): Cardinal;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function WidePos(SubStr, S: UnicodeString): Integer;
//function WideTrim(const S: UnicodeString): UnicodeString; deprecated; // use HtmlGlobals.htTrim() instead
//function WideUpperCase1(const S: UnicodeString): UnicodeString; {$ifdef UNICODE} inline; {$endif} deprecated; // use HtmlGlobals.htUppercase() instead
//function WideLowerCase1(const S: UnicodeString): UnicodeString; {$ifdef UNICODE} inline; {$endif}deprecated; // use HtmlGlobals.htLowercase() instead
function WideSameText1(const S1, S2: UnicodeString): boolean; {$ifdef UseInline} inline; {$endif}
function WideSameStr1(const S1, S2: UnicodeString): boolean;  {$ifdef UseInline} inline; {$endif}

function WideStringToMultibyte(CodePage: Integer; W: UnicodeString): AnsiString;

function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; out Extent: TSize): Integer;
function GetXExtent(DC: HDC; P: PWideChar; N: Integer): Integer;
procedure WrapTextW(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; S: UnicodeString);

//------------------------------------------------------------------------------
// misc. methods
//------------------------------------------------------------------------------

// BG, 26.12.2011: new type TSpecWidth
function SpecWidth(Value: Integer; VType: TWidthType): TSpecWidth;
function ToSpecWidth(AsInteger: Integer; AsString: string): TSpecWidth;

//------------------------------------------------------------------------------
// canvas methods
//------------------------------------------------------------------------------

function CalcClipRect(Canvas: TCanvas; const Rect: TRect; Printing: boolean): TRect;
procedure GetClippingRgn(Canvas: TCanvas; const ARect: TRect; Printing: boolean; var Rgn, SaveRgn: HRgn);

procedure FillRectWhite(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
procedure DrawFormControlRect(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Raised, PrintMonoBlack, Disabled: boolean; Color: TColor);

procedure DrawBorder(Canvas: TCanvas; ORect, IRect: TRect; const C: htColorArray;
  const S: htBorderStyleArray; BGround: TColor; Print: boolean);

implementation

uses
  Forms, Math,
  {$ifndef FPC_TODO}jpeg, {$endif}
  {$IFDEF UNICODE} PngImage, {$ENDIF}
  DitherUnit, StylePars;

type
  EGDIPlus = class(Exception);

{$ifdef UseASMx86}

function StrLenW(Str: PWideChar): Cardinal;
// returns number of characters in a ThtString excluding the null terminator
asm
       MOV     EDX, EDI
       MOV     EDI, EAX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       MOV     EAX, 0FFFFFFFEH
       SUB     EAX, ECX
       MOV     EDI, EDX
end;

function StrPosW(Str, SubStr: PWideChar): PWideChar;
// returns a pointer to the first occurance of SubStr in Str
asm
       PUSH    EDI
       PUSH    ESI
       PUSH    EBX
       OR      EAX, EAX
       JZ      @@2
       OR      EDX, EDX
       JZ      @@2
       MOV     EBX, EAX
       MOV     EDI, EDX
       XOR     AX, AX
       MOV     ECX, 0FFFFFFFFH
       REPNE   SCASW
       NOT     ECX
       DEC     ECX
       JZ      @@2
       MOV     ESI, ECX
       MOV     EDI, EBX
       MOV     ECX, 0FFFFFFFFH
       REPNE   SCASW
       NOT     ECX
       SUB     ECX, ESI
       JBE     @@2
       MOV     EDI, EBX
       LEA     EBX, [ESI - 1]
@@1:
       MOV     ESI, EDX
       LODSW
       REPNE   SCASW
       JNE     @@2
       MOV     EAX, ECX
       PUSH    EDI
       MOV     ECX, EBX
       REPE    CMPSW
       POP     EDI
       MOV     ECX, EAX
       JNE     @@1
       LEA     EAX, [EDI - 2]
       JMP     @@3

@@2:
       XOR     EAX, EAX
@@3:
       POP     EBX
       POP     ESI
       POP     EDI
end;

function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
asm
        PUSH    EDI
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        STD
        DEC     EDI
        DEC     EDI
        MOV     AX,Chr
        REPNE   SCASW
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        INC     EAX
        INC     EAX
@@1:    CLD
        POP     EDI
end;

function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar; assembler;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI,Str
        MOV     ECX,$FFFFFFFF
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        POP     EDI
        MOV     AX,Chr
        REPNE   SCASW
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        DEC     EAX
        DEC     EAX
@@1:    POP     EDI
end;

{$else}

// Pascal-ized equivalents of assembler functions.
function StrLenW(Str: PWideChar): Cardinal;
begin
  Result := 0;
  if Str <> nil then
    while Str[Result] <> #0 do
      Inc(Result);
end;

function StrPosW(Str, SubStr: PWideChar): PWideChar;
var
  StrPos    : PWideChar;
  SubstrPos : PWideChar;
begin
  if SubStr^ = #0 then  // Make sure substring not null string
  begin
    Result := nil;
    Exit;
  end;
  Result := Str;
  while Result^ <> #0 do  // Until reach end of string
  begin
    StrPos := Result;
    SubstrPos := SubStr;
    while SubstrPos^ <> #0 do  // Until reach end of substring
    begin
      if StrPos^ <> SubstrPos^ then  // No point in continuing?
        Break;
      StrPos := StrPos + 1;
      SubstrPos := SubstrPos + 1;
    end;
    if SubstrPos^ = #0 then  // Break because reached end of substring?
      Exit;
    Result := Result + 1;
  end;
  Result := nil;
end;

function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := StrScanW(Str, #0);
  if Chr = #0 then  // Null-terminating char considered part of string.
    Exit;
  while Result <> Str do
  begin
    Result := Result - 1;
    if Result^ = Chr then
      Exit;
  end;
  Result := nil;
end;

function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> #0 do
  begin
    if Result^ = Chr then
      Exit;
    Result := Result + 1;
  end;
  if Chr = #0 then
    Exit;  // Null-terminating char considered part of string. See call
           // searching for #0 to find end of string.
  Result := nil;
end;

{$endif}


{----------------FitText}

function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; out Extent: TSize): Integer;
{return count <= Max which fits in Width.  Return X, the extent of chars that fit}
type
  Integers = array[1..1] of Integer;
var
  Ints: ^Integers;
  L, H, I: Integer;
begin
  Extent.cx := 0;
  Extent.cy := 0;
  Result := 0;
  if (Width <= 0) or (Max = 0) then
    Exit;

  if not IsWin32Platform then
  begin
    GetMem(Ints, Sizeof(Integer) * Max);
    try
      if GetTextExtentExPointW(DC, S, Max, Width, @Result, @Ints^, Extent) then
        if Result > 0 then
          Extent.cx := Ints^[Result]
        else
          Extent.cx := 0;
    finally
      FreeMem(Ints);
    end;
  end
  else {GetTextExtentExPointW not available in win98, 95}
  begin {optimize this by looking for Max to fit first -- it usually does}
    L := 0;
    H := Max;
    I := H;
    while L <= H do
    begin
      GetTextExtentPoint32W(DC, S, I, Extent);
      if Extent.cx < Width then
        L := I + 1
      else
        H := I - 1;
      if Extent.cx = Width then
        Break;
      I := (L + H) shr 1;
    end;
    Result := I;
  end;
end;

{----------------WidePos}

function WidePos(SubStr, S: UnicodeString): Integer;
// Unicode equivalent for Pos() function.
var
  P: PWideChar;
begin
  P := StrPosW(PWideChar(S), PWideChar(SubStr));
  if P = nil then
    Result := 0
  else
    Result := P - PWideChar(S) + 1;
end;

//{----------------WideUpperCase1}
//
//{$ifdef UNICODE}
//
//function WideUpperCase1(const S: UnicodeString): UnicodeString;
//begin
//  Result := WideUpperCase(S);
//end;
//
//function WideLowerCase1(const S: UnicodeString): UnicodeString;
//begin
//  Result := WideLowerCase(S);
//end;
//
//{$else}
//
//function WideUpperCase1(const S: UnicodeString): UnicodeString;
//var
//  Len, NewLen: Integer;
//  Tmp: string;
//begin
//  Len := Length(S);
//  if not IsWin32Platform then
//  begin
//    SetString(Result, PWideChar(S), Len);
//    if Len > 0 then
//      CharUpperBuffW(Pointer(Result), Len);
//  end
//  else
//  begin {win95,98,ME}
//    SetLength(Tmp, 2 * Len);
//    NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Len, PChar(Tmp), 2 * Len, nil, nil);
//    SetLength(Tmp, NewLen);
//    Tmp := AnsiUppercase(Tmp);
//    SetLength(Result, Len);
//    MultibyteToWideChar(CP_ACP, 0, PChar(Tmp), NewLen, PWideChar(Result), Len);
//  end;
//end;
//
//function WideLowerCase1(const S: UnicodeString): UnicodeString;
//var
//  Len, NewLen: Integer;
//  Tmp: string;
//begin
//  Len := Length(S);
//  if not IsWin32Platform then
//  begin
//    SetString(Result, PWideChar(S), Len);
//    if Len > 0 then
//      CharLowerBuffW(Pointer(Result), Len);
//  end
//  else
//  begin {win95,98,ME}
//    SetLength(Tmp, 2 * Len);
//    NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Len, PChar(Tmp), 2 * Len, nil, nil);
//    SetLength(Tmp, NewLen);
//    Tmp := AnsiLowercase(Tmp);
//    SetLength(Result, Len);
//    MultibyteToWideChar(CP_ACP, 0, PChar(Tmp), NewLen, PWideChar(Result), Len);
//  end;
//end;
//
//{$endif}

function WideSameText1(const S1, S2: UnicodeString): boolean;
begin
  Result := htUpperCase(S1) = htUpperCase(S2);
end;

function WideSameStr1(const S1, S2: UnicodeString): boolean;
begin
  Result := S1 = S2;
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
function ScaleRect(const Rect: TRect; ScaleX, ScaleY: Double): TRect;
begin
  Result.Left := Round(Rect.Left * ScaleX);
  Result.Right := Round(Rect.Right * ScaleX);
  Result.Top := Round(Rect.Top * ScaleY);
  Result.Bottom := Round(Rect.Bottom * ScaleY);
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
function CalcClipRect(Canvas: TCanvas; const Rect: TRect; Printing: boolean): TRect;
var
  Point: TPoint;
  SizeV, SizeW: TSize;
begin
  GetWindowOrgEx(Canvas.Handle, Point); {when scrolling or animated Gifs, canvas may not start at X=0, Y=0}
  Result := Rect;
  OffsetRect(Result, -Point.X, -Point.Y);
  if Printing then
  begin
    GetViewportExtEx(Canvas.Handle, SizeV);
    GetWindowExtEx(Canvas.Handle, SizeW);
    Result := ScaleRect(Result, SizeV.cx / SizeW.cx, SizeV.cy / SizeW.cy);
  end;
end;

procedure GetClippingRgn(Canvas: TCanvas; const ARect: TRect; Printing: boolean; var Rgn, SaveRgn: HRgn);
var
  Point: TPoint;
  SizeV, SizeW: TSize;
  HF, VF: double;
  Rslt: Integer;
begin
{find a clipregion to prevent overflow.  First check to see if there is
 already a clip region.  Return the old region, SaveRgn, (or 0) so it can be
 restored later.}
  SaveRgn := CreateRectRgn(0, 0, 1, 1);
  Rslt := GetClipRgn(Canvas.Handle, SaveRgn); {Rslt = 1 for existing region, 0 for none}
  if Rslt = 0 then
  begin
    DeleteObject(SaveRgn);
    SaveRgn := 0;
  end;
{Form the region}
  GetWindowOrgEx(Canvas.Handle, Point); {when scrolling or animated Gifs, canvas may not start at X=0, Y=0}
  with ARect do
    if not Printing then
      Rgn := CreateRectRgn(Left - Point.X, Top - Point.Y, Right - Point.X, Bottom - Point.Y)
    else
    begin
      GetViewportExtEx(Canvas.Handle, SizeV);
      GetWindowExtEx(Canvas.Handle, SizeW);
      HF := (SizeV.cx / SizeW.cx); {Horizontal adjustment factor}
      VF := (SizeV.cy / SizeW.cy); {Vertical adjustment factor}
      Rgn := CreateRectRgn(Round(HF * (Left - Point.X)), Round(VF * (Top - Point.Y)), Round(HF * (Right - Point.X)), Round(VF * (Bottom - Point.Y)));
    end;
  if Rslt = 1 then {if there was a region, use the intersection with this region}
    CombineRgn(Rgn, Rgn, SaveRgn, Rgn_And);
end;

function WideTrim(const S: UnicodeString): UnicodeString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

procedure WrapTextW(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; S: UnicodeString);
{Wraps text in a clipping rectangle. Font must be set on entry}
var
  ARect: TRect;
  TAlign: Integer;
begin
  TAlign := SetTextAlign(Canvas.Handle, TA_Top or TA_Left);
  ARect := Rect(X1, Y1, X2, Y2);
  DrawTextW(Canvas.Handle, PWideChar(S), Length(S), ARect, DT_Wordbreak);
  SetTextAlign(Canvas.Handle, TAlign);
end;

function GetXExtent(DC: HDC; P: PWideChar; N: Integer): Integer;
var
  ExtS: TSize;
  Dummy: Integer;

begin
  if not IsWin32Platform then
    GetTextExtentExPointW(DC, P, N, 0, @Dummy, nil, ExtS)
  else
    GetTextExtentPoint32W(DC, P, N, ExtS); {win95, 98 ME}
  Result := ExtS.cx;
end;

procedure FillRectWhite(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
var
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
begin
  with Canvas do
  begin
    OldBrushStyle := Brush.Style; {save style first}
    OldBrushColor := Brush.Color;
    Brush.Color := Color;
    Brush.Style := bsSolid;
    FillRect(Rect(X1, Y1, X2, Y2));
    Brush.Color := OldBrushColor;
    Brush.Style := OldBrushStyle; {style after color as color changes style}
  end;
end;

procedure DrawFormControlRect(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Raised, PrintMonoBlack, Disabled: boolean; Color: TColor);
{Draws lowered rectangles for form control printing}
var
  OldStyle: TPenStyle;
  OldWid: Integer;
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
  MonoBlack: boolean;
begin
  with Canvas do
  begin
    MonoBlack := PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
      (GetDeviceCaps(Handle, PLANES) = 1);
    Dec(X2); Dec(Y2);
    OldWid := Pen.Width;
    OldStyle := Pen.Style;
    OldBrushStyle := Brush.Style; {save style first}
    OldBrushColor := Brush.Color;
    if not MonoBlack and Disabled then
      Brush.Color := clBtnFace
    else
      Brush.Color := color;
    Brush.Style := bsSolid;
    FillRect(Rect(X1, Y1, X2, Y2));
    Brush.Color := OldBrushColor;
    Brush.Style := OldBrushStyle; {style after color as color changes style}

    Pen.Style := psInsideFrame;
    if MonoBlack then
    begin
      Pen.Width := 1;
      Pen.Color := clBlack;
    end
    else
    begin
      Pen.Width := 2;
      if Raised then
        Pen.Color := clSilver
      else
        Pen.Color := clBtnShadow;
    end;
    MoveTo(X1, Y2);
    LineTo(X1, Y1);
    LineTo(X2, Y1);
    if not MonoBlack then
      if Raised then
        Pen.Color := clBtnShadow
      else
        Pen.Color := clSilver;
    LineTo(X2, Y2);
    LineTo(X1, Y2);
    Pen.Style := OldStyle;
    Pen.Width := OldWid;
  end;
end;

{$IFDEF Ver90}

procedure Assert(B: boolean; const S: ThtString);
begin {dummy Assert for Delphi 2}
end;
{$ENDIF}

//-- BG ---------------------------------------------------------- 26.12.2011 --
function SpecWidth(Value: Integer; VType: TWidthType): TSpecWidth;
begin
  Result.Value := Value;
  Result.VType := VType;
end;

//-- BG ---------------------------------------------------------- 26.12.2011 --
function ToSpecWidth(AsInteger: Integer; AsString: string): TSpecWidth;
// Return a TSpecWidth prepared with values given in AsDouble *and* AsString.
// AsString is used to evaluate the type while AsDouble is used to evaluate the value.
// BG, 26.12.2011: Currently percentage is still converted to permille as done before Value became type Double.
begin
  if Pos('%', AsString) > 0 then
  begin
    Result.Value := Min(100, AsInteger) * 10;
    Result.VType := wtPercent;
  end
  else if Pos('*', AsString) > 0 then // this is not specified for <td>, <th>. Only <col> and <colgroup> support it officially.
  begin
    Result.Value := AsInteger;
    Result.VType := wtRelative;
  end
  else
  begin
    Result.Value := AsInteger;
    Result.VType := wtAbsolute;
  end;
end;


procedure TFreeList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

{ TAttribute }

constructor TAttribute.Create(ASym: Symb; const AValue: Double; const NameStr, ValueStr: ThtString; ACodePage: Integer);
begin
  inherited Create;
  Which := ASym;
  DblValue := AValue;
  Value := Trunc(AValue);
  WhichName := NameStr;
  Name := ValueStr;
  CodePage := ACodePage;
end;

{----------------TAttributeList}

procedure TAttributeList.Clear;
begin
  inherited Clear;
  SaveID := '';
end;

function TAttributeList.CreateStringList: ThtStringList;
var
  I: Integer;
begin
  Result := ThtStringList.Create;
  for I := 0 to Count - 1 do
    with Items[I] do
      Result.Add(WhichName + '=' + Name);
end;

destructor TAttributeList.Destroy;
begin
  Prop.Free;
  inherited;
end;

function TAttributeList.Find(Sy: Symb; var T: TAttribute): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Which = Sy then
    begin
      Result := True;
      T := Items[I];
      Exit;
    end;
  Result := False;
end;

function TAttributeList.Find(SyName: ThtString; var T: TAttribute): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].WhichName = SyName then
    begin
      Result := True;
      T := Items[I];
      Exit;
    end;
  Result := False;
end;

function TAttributeList.GetAttribute(Index: Integer): TAttribute;
begin
  Result := Get(Index);
end;

function TAttributeList.GetClass: ThtString;
var
  T: TAttribute;
  S: ThtString;
  I: Integer;
begin
  Result := '';
  if Find(ClassSy, T) then
  begin
    S := Lowercase(Trim(T.Name));
    I := Pos(' ', S);
    if I <= 0 then {a single class name}
      Result := S
    else
    begin {multiple class names.  Format as "class1.class2.class3"}
      repeat
        Result := Result + '.' + System.Copy(S, 1, I - 1);
        System.Delete(S, 1, I);
        S := Trim(S);
        I := Pos(' ', S);
      until I <= 0;
      Result := Result + '.' + S;
      Result := SortContextualItems(Result); {put in standard multiple order}
      System.Delete(Result, 1, 1); {remove initial '.'}
    end;
  end;
end;

function TAttributeList.GetID: ThtString;
var
  T: TAttribute;
begin
  Result := SaveID;
  if (Result = '') and Find(IDSy, T) then
  begin
    Result := Lowercase(T.Name);
    SaveID := Result;
  end;
end;

function TAttributeList.GetTitle: ThtString;
var
  T: TAttribute;
begin
  if Find(TitleSy, T) then
    Result := T.Name
  else
    Result := '';
end;

function TAttributeList.GetStyle: TProperties;
var
  T: TAttribute;
begin
  if Find(StyleSy, T) then
  begin
    Prop.Free;
    Prop := TProperties.Create(False);
    Result := Prop;
    ParsePropertyStr(T.Name, Result);
  end
  else
    Result := nil;
end;

{----------------TUrlTarget.Create}

constructor TUrlTarget.Create;
begin
  inherited Create;
  //utText := TutText.Create;
  utText.Start := -1;
  utText.Last := -1;
end;

destructor TUrlTarget.Destroy;
begin
  //FreeAndNil(utText);
  inherited Destroy;
end;

var
  Sequence: Integer = 10;

procedure TUrlTarget.Assign(const AnUrl, ATarget: ThtString; L: TAttributeList; AStart: Integer);
var
  SL: ThtStringList;
begin
  Url := AnUrl;
  Target := ATarget;
  ID := Sequence;
  Inc(Sequence);
  utText.Start := AStart;
  SL := L.CreateStringList;
  try
    Attr := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TUrlTarget.Assign(const UT: TUrlTarget); 
begin
  Url := UT.Url;
  Target := UT.Target;
  ID := UT.ID;
  TabIndex := UT.TabIndex;
  Attr := UT.Attr;
  utText.Start := UT.utText.Start;
  utText.Last := UT.utText.Last;
end;

procedure TUrlTarget.Clear;
begin
  Url := '';
  Target := '';
  ID := 0;
  TabIndex := 0;
  Attr := '';
  utText.Start := -1;
  utText.Last := -1;
end;

procedure TUrlTarget.SetLast(List: TList; ALast: Integer);
var
  I: Integer;
begin
  utText.Last := ALast;
  if (List.Count > 0) then
    for I := List.Count - 1 downto 0 do
      if (ID = TFontObjBase(List[I]).UrlTarget.ID) then
        TFontObjBase(List[I]).UrlTarget.utText.Last := ALast
      else
        Break;
end;

{----------------SelTextCount}

procedure SelTextCount.AddText(P: PWideChar; Size: Integer);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
    case P[I] of
      {ImgPan and FmCtl used to mark images, form controls}
      FmCtl, ImgPan:;
    else
      Inc(Leng);
    end;
end;

procedure SelTextCount.AddTextCR(P: PWideChar; Size: Integer);
begin
  AddText(P, Size);
  AddText(#13#10, 2);
end;

function SelTextCount.Terminate: Integer;
begin
  Result := Leng;
end;

{----------------SelTextBuf.Create}

constructor SelTextBuf.Create(ABuffer: PWideChar; Size: Integer);
begin
  inherited Create;
  Buffer := ABuffer;
  BufferLeng := Size;
end;

procedure SelTextBuf.AddText(P: PWideChar; Size: Integer);
var
  SizeM1: Integer;
  I: Integer;
begin
  SizeM1 := BufferLeng - 1;
  for I := 0 to Size - 1 do
    case P[I] of
      {ImgPan and FmCtl used to mark images, form controls}
      FmCtl, ImgPan, BrkCh:;
    else
      if Leng < SizeM1 then
      begin
        Buffer[Leng] := P[I];
        Inc(Leng);
      end;
    end;
end;

function SelTextBuf.Terminate: Integer;
begin
  Buffer[Leng] := #0;
  Result := Leng + 1;
end;

{----------------ClipBuffer.Create}

constructor ClipBuffer.Create(Leng: Integer);
begin
  inherited Create(nil, 0);
  BufferLeng := Leng;
  Getmem(Buffer, BufferLeng * 2);
end;

destructor ClipBuffer.Destroy;
begin
  if Assigned(Buffer) then
    FreeMem(Buffer);
  inherited Destroy;
end;

procedure ClipBuffer.CopyToClipboard;
{Unicode clipboard routine courtesy Mike Lischke}
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * BufferLeng);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(Buffer^, DataPtr^, 2 * BufferLeng);
{$ifndef FPC_TODO}
      Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
{$endif}
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
    raise;
  end;
end;

function ClipBuffer.Terminate: Integer;
begin
  Buffer[Leng] := #0;
  Result := Leng + 1;
  if IsWin32Platform then
    Clipboard.AsText := Buffer
  else
    CopyToClipboard;
end;

{ TMapArea }

//-- BG ---------------------------------------------------------- 31.12.2011 --
function TMapArea.PtInRegion(X, Y: Integer): Boolean;
begin
  Result := Windows.PtInRegion(FRegion, X, Y);
end;

{ TMapAreaList }

//-- BG ---------------------------------------------------------- 31.12.2011 --
function TMapAreaList.getArea(Index: Integer): TMapArea;
begin
  Result := get(Index);
end;

{ TMapItem }

constructor TMapItem.Create;
begin
  inherited Create;
  FAreas := TMapAreaList.Create;
end;

destructor TMapItem.Destroy;
begin
  FAreas.Free;
  inherited Destroy;
end;

function TMapItem.GetURL(X, Y: Integer; var URLTarg: TUrlTarget; var ATitle: ThtString): boolean;
var
  I: Integer;
  Area: TMapArea;
begin
  Result := False;
  for I := 0 to FAreas.Count - 1 do
  begin
    Area := FAreas[I];
    if Area.PtInRegion(X, Y) then
    begin
      if Area.HRef <> '' then {could be NoHRef}
      begin
        URLTarg := TUrlTarget.Create;
        URLTarg.URL := Area.HRef;
        URLTarg.Target := Area.Target;
        ATitle := Area.Title;
        Result := True;
      end;
      Exit;
    end;
  end;
end;

procedure TMapItem.AddArea(Attrib: TAttributeList);
const
  MAXCNT = 300;

  function GetSubStr(var S: ThtString): ThtString;
  var
    J, K: Integer;
  begin
    J := Pos(',', S);
    K := Pos(' ', S); {for non comma situations (bad syntax)}
    if (J > 0) and ((K = 0) or (K > J)) then
    begin
      Result := copy(S, 1, J - 1);
      Delete(S, 1, J);
    end
    else if K > 0 then
    begin
      Result := copy(S, 1, K - 1);
      Delete(S, 1, K);
    end
    else
    begin
      Result := Trim(S);
      S := '';
    end;
    while (Length(S) > 0) and ((S[1] = ',') or (S[1] = ' ')) do
      Delete(S, 1, 1);
  end;

var
  S, S1: ThtString;
  I, Cnt, Rad: Integer;
  Nm: ThtString;
  Coords: array[0..MAXCNT] of Integer;
  Rect: TRect absolute Coords;
  Shape: (shRect, shCircle, shPoly, shDefault);
  Area: TMapArea;
begin
  if FAreas.Count >= 1000 then
    Exit;
  Area := TMapArea.Create;
  try
    Shape := shRect;
    Cnt := 0;
    for I := 0 to Attrib.Count - 1 do
      with Attrib[I] do
        case Which of
          HRefSy:
            Area.FHRef := Name;

          TargetSy:
            Area.FTarget := Name;

          TitleSy:
            Area.FTitle := Name;

          NoHrefSy:
            Area.FHRef := '';

          CoordsSy:
            begin
              Cnt := 0;
              S := Trim(Name);
              S1 := GetSubStr(S);
              while (S1 <> '') and (Cnt <= MAXCNT) do
              begin
                Coords[Cnt] := StrToIntDef(S1, 0);
                S1 := GetSubStr(S);
                Inc(Cnt);
              end;
            end;

          ShapeSy:
            begin
              Nm := copy(Lowercase(Name), 1, 4);
              if (Nm = 'circ') or (Nm = 'circle') then
                Shape := shCircle
              else if (Nm = 'poly') or (Nm = 'polygon') then
                Shape := shPoly
              else if (Nm = 'rect') or (Nm = 'rectangle') then
                Shape := shRect;
            end;
        end;

    case Shape of
      shRect:
        begin
          if Cnt < 4 then
            Exit;
          Inc(Coords[2]);
          Inc(Coords[3]);
          Area.FRegion := CreateRectRgnIndirect(Rect);
        end;

      shCircle:
        begin
          if Cnt < 3 then
            Exit;
          Rad := Coords[2];
          Dec(Coords[0], Rad);
          Dec(Coords[1], Rad);
          Coords[2] := Coords[0] + 2 * Rad + 1;
          Coords[3] := Coords[1] + 2 * Rad + 1;
          Area.FRegion := CreateEllipticRgnIndirect(Rect);
        end;

      shPoly:
        begin
          if Cnt < 6 then
            Exit;
{$ifdef LCL}
          Area.FRegion := CreatePolygonRgn(PPoint(@Coords[0]), Cnt div 2, Winding);
{$else}
          Area.FRegion := CreatePolygonRgn(Coords, Cnt div 2, Winding);
{$endif}
        end;
    end;
    if Area.FRegion <> 0 then
    begin
      FAreas.Add(Area);
      Area := nil;
    end;
  finally
    Area.Free;
  end;
end;


{ TIndentManager }

constructor TIndentManager.Create;
begin
  inherited Create;
  R := TFreeList.Create;
  L := TFreeList.Create;
end;

destructor TIndentManager.Destroy;
begin
  R.Free;
  L.Free;
  inherited Destroy;
end;

////------------------------------------------------------------------------------
//function TIndentManager.AddImage(Y: Integer; Img: TFloatingObj): IndentRec;
//{Given a new floating image, update the edge information.  Fills  Img.Indent,
// the distance from the left edge to the upper left corner of the image}
//var
//  IH, IW: Integer;
//begin
//  Result := nil;
//  if Assigned(Img) then
//  begin
//    IW := Img.HSpaceL + Img.ImageWidth  + Img.HSpaceR;
//    IH := Img.VSpaceT + Img.ImageHeight + Img.VSpaceB;
//    case Img.Floating of
//      ALeft:
//      begin
//        Result := AddLeft(Y, Y + IH, IW);
//        Img.Indent := Result.X - IW + Img.HSpaceL;
//      end;
//
//      ARight:
//      begin
//        Result := AddRight(Y, Y + IH, IW);
//        Img.Indent := Result.X + Img.HSpaceL;
//      end;
//    end;
//  end;
//end;

//-- BG ---------------------------------------------------------- 05.02.2011 --
function TIndentManager.AddLeft(YT, YB, W: Integer): IndentRec;
// For a floating block, update the left edge information.
begin
  Result := IndentRec.Create;
  Result.YT := YT;
  Result.YB := YB;
  Result.X := LeftEdge(YT) + W;
  L.Add(Result);
end;

//-- BG ---------------------------------------------------------- 05.02.2011 --
function TIndentManager.AddRight(YT, YB, W: Integer): IndentRec;
// For a floating block, update the right edge information.
begin
  Result := IndentRec.Create;
  Result.YT := YT;
  Result.YB := YB;
  Result.X := RightEdge(YT) - W;
  R.Add(Result);
end;

{----------------TIndentManager.Reset}

//-- BG ---------------------------------------------------------- 23.02.2011 --
procedure TIndentManager.Init(Lf, Wd: Integer);
begin
  LfEdge := Lf;
  Width  := Wd;
  R.Clear;
  L.Clear;
  CurrentID := nil;
end;

procedure TIndentManager.Reset(Lf: Integer);
begin
  LfEdge := Lf;
  CurrentID := nil;
end;

const
  BigY = 9999999;

//-- BG ---------------------------------------------------------- 23.02.2011 --
function TIndentManager.LeftEdge(Y: Integer): Integer;
// Returns the right most left indentation at Y relative to LfEdge.
// If there are no left indentations at Y, returns 0.
var
  I: Integer;
  IR: IndentRec;
  MinX: Integer;
begin
  Result := -MaxInt;
  MinX := 0;
  for I := 0 to L.Count - 1 do
  begin
    IR := L.Items[I];
    if (Y >= IR.YT) and (Y < IR.YB) and (Result < IR.X) then
      if (IR.ID = nil) or (IR.ID = CurrentID) then
        Result := IR.X;
    if IR.ID = CurrentID then
      MinX := IR.X;
  end;
  if Result = -MaxInt then
    Result := MinX;
end;

//-- BG ---------------------------------------------------------- 23.02.2011 --
function TIndentManager.LeftIndent(Y: Integer): Integer;
// Returns the right most left indentation at Y relative to block.
// If there are no left indentations at Y, returns LfEdge.
begin
  Result := LeftEdge(Y) + LfEdge;
end;

//-- BG ---------------------------------------------------------- 23.02.2011 --
function TIndentManager.RightEdge(Y: Integer): Integer;
// Returns the left most right indentation at Y relative LfEdge.
// If there are no indentations at Y, returns Width.
var
  I: Integer;
  IR: IndentRec;
  MinX: Integer;
begin
  Result := MaxInt;
  for I := 0 to R.Count - 1 do
  begin
    IR := R.Items[I];
    if (Y >= IR.YT) and (Y < IR.YB) and (Result > IR.X) then
      if (IR.ID = nil) or (IR.ID = CurrentID) then
        Result := IR.X;
  end;
  if Result = MaxInt then
  begin
    //BG, 01.03.2011: Issue 77: Error of the elements
    MinX := 0;
    for I := L.Count - 1 downto 0 do
    begin
      IR := L.Items[I];
      if IR.ID = CurrentID then
      begin
        MinX := IR.X;
        break;
      end;
    end;
    Result := Width + MinX;
  end;
end;

//-- BG ---------------------------------------------------------- 23.02.2011 --
function TIndentManager.RightSide(Y: Integer): Integer;
// Returns the left most right indentation at Y relative to block.
// If there are no indentations at Y, returns Width + LfEdge.
begin
  Result := RightEdge(Y) + LfEdge;
end;

function TIndentManager.ImageBottom: Integer;
// Returns the bottom of the last floating image.
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to L.Count - 1 do
    with IndentRec(L.Items[I]) do
      if (ID = nil) and (YB > Result) then
        Result := YB;
  for I := 0 to R.Count - 1 do
    with IndentRec(R.Items[I]) do
      if (ID = nil) and (YB > Result) then
        Result := YB;
end;

procedure TIndentManager.GetClearY(var CL, CR: Integer);
{returns the left and right Y values which will clear image margins}
var
  I: Integer;
begin
  CL := -1;
  for I := 0 to L.Count - 1 do
    with IndentRec(L.Items[I]) do
      if (ID = nil) and (YB > CL) then
        CL := YB;
  CR := -1;
  for I := 0 to R.Count - 1 do
    with IndentRec(R.Items[I]) do
      if (ID = nil) and (YB > CR) then
        CR := YB;
  Inc(CL);
  Inc(CR);
end;

////-- BG ---------------------------------------------------------- 08.06.2008 --
//function TIndentManager.GetNextLeftXY(var Y: Integer; X, ThisWidth, MaxWidth, MinIndex: Integer): Integer;
//var
//  Index: Integer;
//  Indent: IndentRec;
//  DummyCR: Integer;
//begin
//  if X < 0 then
//  begin
//    dec(X, Auto);
//    inc(MaxWidth, 2 * Auto);
//  end;
//
//  Index := L.Count - 1;
//  if Index >= MinIndex then
//  begin
//    Indent := IndentRec(L[Index]);
//    if Y < Indent.YB then
//    begin
//      // set y to previous used y == y of current line
//      Y := Max(Y, Indent.YT);
//      Result := Max(X, Indent.X);
//    end
//    else
//      Result := X;
//  end
//  else
//    Result := Max(X, LeftIndent(Y));
//
//  if Result + ThisWidth > MaxWidth + X then
//  begin
//    Result := X;
//    GetClearY(Y, DummyCR);
//  end;
//end;

//-- BG ---------------------------------------------------------- 06.02.2011 --
function TIndentManager.AlignLeft(var Y: Integer; W, SpW, SpH: Integer): Integer;
// Returns the aligned Y position of a block of width W starting at Y.
// Result is > Y, if at Y is not enough width for the block and optional additional space Sp
// Additional space e.g. for a textRec between aligned images.
var
  I, CL, CR, LX, RX, XL, XR, YY, MinX: Integer;
begin
  Result := LeftEdge(Y);
  if Result + W + SpW > RightEdge(Y) then
  begin
    // too wide, must find a wider place below:
    if (SpH > 0) and (Result + W <= RightEdge(Y + SpH)) then
    begin
      // fits into area below space Sp
      Inc(Y, SpH);
    end
    else
    begin
      // too wide, must find a wider place below:
      YY := Y;
      MinX := 0;

      CL := Y;
      XL := Result; // valium for the compiler
      for I := L.Count - 1 downto 0 do
        with IndentRec(L.Items[I]) do
        begin
          if ID = CurrentID then
          begin
            MinX := X;
            break;
          end;
          if (ID = nil) and (YB > Y) and ((YB < CL) or (CL = Y)) then
          begin
            if X = LeftEdge(YB - 1) then
            begin
              // This is the right most left indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CL := YB;
                XL := LX;
              end;
            end;
          end;
        end;

      CR := Y;
      XR := Result; // valium for the compiler
      for I := R.Count - 1 downto 0 do
        with IndentRec(R.Items[I]) do
        begin
          if ID = CurrentID then
            break;
          if (ID = nil) and (YB > Y) and ((YB < CR) or (CR = Y)) then
          begin
            if X = RightEdge(YB - 1) then
            begin
              // This is the left most right indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CR := YB;
                XR := LX;
              end;
            end;
          end;
        end;

      if CL = Y then
      begin
        if CR = Y then
        begin
          // no better place found, just append at the end.
          Y := YY;
          Result := MinX;
        end
        else
        begin
          Y := CR;
          Result := XR;
        end
      end
      else if CR = Y then
      begin
        Y := CL;
        Result := XL;
      end
      else if CL < CR then
      begin
        Y := CL;
        Result := XL;
      end
      else
      begin
        Y := CR;
        Result := XR;
      end;
    end;
  end;
  Inc(Result, LfEdge);
end;

function TIndentManager.AlignRight(var Y: Integer; W, SpW, SpH: Integer): Integer;
var
  I, CL, CR, LX, RX, XL, XR, YY, MaxX: Integer;
begin
  Result := RightEdge(Y) - W;
  if Result < LeftEdge(Y) + SpW then
  begin
    // too wide, must find a wider place below:
    if (SpH > 0) and (Result >= LeftEdge(Y + SpH)) then
    begin
      // fits into area below space Sp
      Inc(Y, SpH);
    end
    else
    begin
      YY := Y;
      MaxX := Width - W;

      CL := Y;
      XL := Result; // valium for the compiler
      for I := L.Count - 1 downto 0 do
        with IndentRec(L.Items[I]) do
        begin
          if ID = CurrentID then
            break;
          if (ID = nil) and (YB > Y) and ((YB < CL) or (CL = Y)) then
          begin
            if X = LeftEdge(YB - 1) then
            begin
              // This is the right most left indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CL := YB;
                XL := RX;
              end;
            end;
          end;
        end;

      CR := Y;
      XR := Result; // valium for the compiler
      for I := R.Count - 1 downto 0 do
        with IndentRec(R.Items[I]) do
        begin
          if ID = CurrentID then
          begin
            MaxX := X - W;
            break;
          end;
          if (ID = nil) and (YB > Y) and ((YB < CR) or (CR = Y)) then
          begin
            if X = RightEdge(YB - 1) then
            begin
              // This is the left most right indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CR := YB;
                XR := RX;
              end;
            end;
          end;
        end;

      if CL = Y then
      begin
        if CR = Y then
        begin
          // no better place found, just append at the end.
          Y := YY;
          Result := MaxX;
        end
        else
        begin
          Y := CR;
          Result := XR;
        end
      end
      else if CR = Y then
      begin
        Y := CL;
        Result := XL;
      end
      else if CL < CR then
      begin
        Y := CL;
        Result := XL;
      end
      else
      begin
        Y := CR;
        Result := XR;
      end;
    end;
  end;
  Inc(Result, LfEdge);
end;

function TIndentManager.GetNextWiderY(Y: Integer): Integer;
{returns the next Y value which offers a wider space or Y if none}
var
  I, CL, CR: Integer;
begin
  CL := Y;
  for I := 0 to L.Count - 1 do
    with IndentRec(L.Items[I]) do
      if not Assigned(ID) and (YB > Y) and ((YB < CL) or (CL = Y)) then
        CL := YB;
  CR := Y;
  for I := 0 to R.Count - 1 do
    with IndentRec(R.Items[I]) do
      if not Assigned(ID) and (YB > Y) and ((YB < CR) or (CR = Y)) then
        CR := YB;
  if CL = Y then
    Result := CR
  else if CR = Y then
    Result := CL
  else
    Result := Min(CL, CR);
end;

function TIndentManager.SetLeftIndent(XLeft, Y: Integer): Integer;
var
  IR: IndentRec;
begin
  IR := IndentRec.Create;
  with IR do
  begin
    YT := Y;
    YB := BigY;
    X := XLeft;
    ID := CurrentID;
  end;
  Result := L.Add(IR);
end;

function TIndentManager.SetRightIndent(XRight, Y: Integer): Integer;
var
  IR: IndentRec;
begin
  IR := IndentRec.Create;
  with IR do
  begin
    YT := Y;
    YB := BigY;
    X := XRight;
    ID := CurrentID;
  end;
  Result := R.Add(IR);
end;

procedure TIndentManager.FreeLeftIndentRec(I: Integer);
begin
  L.Delete(I);
end;

procedure TIndentManager.FreeRightIndentRec(I: Integer);
begin
  R.Delete(I);
end;

function CopyPalette(Source: hPalette): hPalette;
var
  LP: ^TLogPalette;
  NumEntries: Integer;
begin
  Result := 0;
  if ColorBits > 8 then
    Exit;
  GetMem(LP, Sizeof(TLogPalette) + 256 * Sizeof(TPaletteEntry));
  try
    with LP^ do
    begin
      palVersion := $300;
      palNumEntries := 256;
      NumEntries := GetPaletteEntries(Source, 0, 256, palPalEntry);
      if NumEntries > 0 then
      begin
        palNumEntries := NumEntries;
        Result := CreatePalette(LP^);
      end;
    end;
  finally
    FreeMem(LP, Sizeof(TLogPalette) + 256 * Sizeof(TPaletteEntry));
  end;
end;

//function InSet(W: WideChar; S: SetOfChar): boolean;
//begin
//  if Ord(W) > 255 then
//    Result := False
//  else
//    Result := ThtChar(W) in S;
//end;

{----------------TCharCollection.GetAsString:}

function TCharCollection.GetAsString: ThtString;
begin
  Result := Copy(FChars, 1, FCurrentIndex);
end;

function TCharCollection.GetSize: Integer;

begin
  Result := FCurrentIndex;
end;

constructor TCharCollection.Create;
begin
  inherited;
  SetLength(FChars, TokenLeng);
  GetMem(FIndices, TokenLeng * Sizeof(Integer));
  FCurrentIndex := 0;
end;

destructor TCharCollection.Destroy;
begin
  FreeMem(FIndices);
  inherited;
end;

procedure TCharCollection.Add(C: ThtChar; Index: Integer);
begin
  if FCurrentIndex = Length(FChars) then
  begin
    SetLength(FChars, FCurrentIndex + 50);
    ReallocMem(FIndices, (FCurrentIndex + 50) * Sizeof(Integer));
  end;
  Inc(FCurrentIndex);
  FIndices^[FCurrentIndex] := Index;
  FChars[FCurrentIndex] := C;
end;

procedure TCharCollection.Add(const S: ThtString; Index: Integer);
var
  K: Integer;
begin
  K := FCurrentIndex + Length(S);
  if K >= Length(FChars) then
  begin
    SetLength(FChars, K + 50);
    ReallocMem(FIndices, (K + 50) * Sizeof(Integer));
  end;
  Move(PhtChar(S)^, FChars[FCurrentIndex + 1], Length(S) * SizeOf(ThtChar));
  while FCurrentIndex < K do
  begin
    Inc(FCurrentIndex);
    FIndices^[FCurrentIndex] := Index;
  end;
end;

procedure TCharCollection.Clear;
begin
  FCurrentIndex := 0;
  SetLength(FChars, 0);
end;

//procedure TCharCollection.Concat(T: TCharCollection);
//var
//  K: Integer;
//begin
//  K := FCurrentIndex + T.FCurrentIndex;
//  if K >= Length(FChars) then
//  begin
//    SetLength(FChars, K + 50);
//    ReallocMem(FIndices, (K + 50) * Sizeof(Integer));
//  end;
//  Move(PhtChar(T.FChars)^, FChars[FCurrentIndex + 1], T.FCurrentIndex * SizeOf(ThtChar)); //@@@ Tiburon: todo test
//  Move(T.FIndices^[1], FIndices^[FCurrentIndex + 1], T.FCurrentIndex * Sizeof(Integer));
//  FCurrentIndex := K;
//end;

{ TokenObj }

constructor TokenObj.Create;
begin
  inherited;
  GetMem(C, TokenLeng * Sizeof(WideChar));
  GetMem(I, TokenLeng * Sizeof(Integer));
  FCapacity := TokenLeng;
  FCount := 0;
  St := '';
  StringOK := True;
end;

destructor TokenObj.Destroy;
begin
  FreeMem(I);
  FreeMem(C);
  inherited;
end;

procedure TokenObj.AddUnicodeChar(Ch: WideChar; Ind: Integer);
{Ch must be Unicode in this method}
begin
  if Count >= Capacity then
    SetCapacity(Capacity + 50);
  Inc(FCount);
  C^[Count] := Ch;
  I^[Count] := Ind;
  StringOK := False;
end;

procedure TokenObj.Clear;
begin
  FCount := 0;
  St := '';
  StringOK := True;
end;

function WideStringToMultibyte(CodePage: Integer; W: UnicodeString): Ansistring;
var
  NewLen, Len: Integer;
begin
  if CodePage = CP_UTF8 then {UTF-8 encoded ThtString.}
    Result := UTF8Encode(W)
  else
  begin
    Len := Length(W);
    SetLength(Result, 3 * Len);
    NewLen := WideCharToMultiByte(CodePage, 0, PWideChar(W), Len, PAnsiChar(Result), 3 * Len, nil, nil);
    if NewLen = 0 then
    { Invalid code page. Try default.}
      NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(W), Len, PAnsiChar(Result), 3 * Len, nil, nil);
    SetLength(Result, NewLen);
  end;
end;

//function ByteNum(CodePage: Integer; P: PAnsiChar): Integer;
//var
//  P1: PAnsiChar;
//begin
//  if CodePage <> CP_UTF8 then
//  begin
//    P1 := {$ifdef LCL} CharNextEx {$else} CharNextExA {$endif} (CodePage, P, 0);
//    if Assigned(P1) then
//      Result := P1 - P
//    else
//      Result := 0;
//  end
//  else
//    case ord(P^) of {UTF-8}
//      0: Result := 0;
//      1..127: Result := 1;
//      192..223: Result := 2;
//      224..239: Result := 3;
//      240..247: Result := 4;
//    else
//      Result := 1; {error}
//    end;
//end;

procedure TokenObj.AddString(S: TCharCollection);
var
  K: Integer;
begin
  K := Count + S.FCurrentIndex;
  if K >= Capacity then
    SetCapacity(K + 50);
  Move(S.FChars[1], C^[Count + 1], S.FCurrentIndex * Sizeof(WideChar));
  Move(S.FIndices[1], I^[Count + 1], S.FCurrentIndex * Sizeof(Integer));
  FCount := K;
  StringOK := False;
end;

//procedure TokenObj.Concat(T: TokenObj);
//var
//  K: Integer;
//begin
//  K := Count + T.Count;
//  if K >= Capacity then
//    SetCapacity(K + 50);
//  Move(T.C^, C^[Count + 1], T.Count * Sizeof(WideChar));
//  Move(T.I^, I^[Count + 1], T.Count * Sizeof(Integer));
//  FCount := K;
//  StringOK := False;
//end;
//
//procedure TokenObj.Remove(N: Integer);
//begin {remove a single character}
//  if N <= Count then
//  begin
//    Move(C^[N + 1], C^[N], (Count - N) * Sizeof(WideChar));
//    Move(I^[N + 1], I^[N], (Count - N) * Sizeof(Integer));
//    if StringOK then
//      Delete(St, N, 1);
//    Dec(FCount);
//  end;
//end;
//
//procedure TokenObj.Replace(N: Integer; Ch: WideChar);
//begin {replace a single character}
//  if N <= Count then
//  begin
//    C^[N] := Ch;
//    if StringOK then
//      St[N] := Ch;
//  end;
//end;

//-- BG ---------------------------------------------------------- 20.01.2011 --
procedure TokenObj.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(C, NewCapacity * Sizeof(WideChar));
    ReallocMem(I, NewCapacity * Sizeof(Integer));
    FCapacity := NewCapacity;
    if NewCapacity < Count then
    begin
      FCount := NewCapacity;
      if StringOK then
        St := Copy(St, 1, Count);
    end;
  end;
end;

function TokenObj.GetString: UnicodeString;
begin
  if not StringOK then
  begin
    SetLength(St, Count);
    Move(C^, St[1], SizeOf(WideChar) * Count);
    StringOK := True;
  end;
  Result := St;
end;

{----------------TIDObjectList}

function TIDObjectList.AddObject(const S: ThtString; AObject: TIDObject): Integer;
var
  I: Integer;
begin
  if Find(S, I) then
  begin
    try
      if Objects[I].FreeMe then
        Objects[I].Free;
    except
    end;
    Delete(I);
  end;
  Result := inherited AddObject(S, AObject);
end;

procedure TIDObjectList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  try
    if Objects[I].FreeMe then
      Objects[I].Free;
  except
  end;
  inherited Clear;
end;

constructor TIDObjectList.Create;
begin
  inherited Create;
  Sorted := True;
end;

destructor TIDObjectList.Destroy;
begin
  Clear;
  inherited
end;

//-- BG ---------------------------------------------------------- 04.03.2011 --
function TIDObjectList.GetObject(Index: Integer): TIDObject;
begin
  Result := TIDObject(inherited GetObject(Index));
end;

type
  BorderPointArray = array[0..3] of TPoint;

//function Points(P0, P1, P2, P3: TPoint): BorderPointArray;
//begin
//  Result[0] := P0;
//  Result[1] := P1;
//  Result[2] := P2;
//  Result[3] := P3;
//end;

procedure DrawOnePolygon(Canvas: TCanvas; P: BorderPointArray; Color: TColor;
  Side: byte; Printing: boolean);
{Here we draw a 4 sided polygon (by filling a region).  This represents one
 side (or part of a side) of a border.
 For single pixel thickness, drawing is done by lines for better printing}
//BG, 22.08.2010: in print preview results are better without the single pixel exception.
//type
//  SideArray = array[0..3, 1..4] of Integer;
//const
//  AD: SideArray = ((0, 1, 0, 3),
//    (0, 1, 1, 1),
//    (2, 0, 2, 1),
//    (1, 3, 3, 3));
//  AP: SideArray = ((0, 1, 0, 3),
//    (0, 1, 2, 1),
//    (2, 0, 2, 2),
//    (1, 3, 3, 3));
var
  R: HRgn;
//  OldWidth: Integer;
//  OldStyle: TPenStyle;
//  OldColor: TColor;
//  Thickness: Integer;
//  P1, P2: TPoint;
//  I: SideArray;
begin
//  if Side in [0, 2] then
//    Thickness := Abs(P[2].X - P[1].X)
//  else
//    Thickness := Abs(P[1].Y - P[2].Y);
//  if Thickness = 1 then
//  begin
//    with Canvas do
//    begin
//      OldColor := Pen.Color;
//      OldStyle := Pen.Style;
//      OldWidth := Pen.Width;
//      Pen.Color := Color;
//      Pen.Style := psSolid;
//      Pen.Width := 1;
//      if Printing then
//        I := AP
//      else
//        I := AD;
//      P1 := Point(P[I[Side, 1]].X, P[I[Side, 2]].Y);
//      P2 := Point(P[I[Side, 3]].X, P[I[Side, 4]].Y);
//      MoveTo(P1.X, P1.Y);
//      LineTo(P2.X, P2.Y);
//      Pen.Width := OldWidth;
//      Pen.Style := OldStyle;
//      Pen.Color := OldColor;
//    end;
//  end
//  else
  begin
    R := CreatePolygonRgn(P, 4, Alternate);
    try
      with Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := Color;
        FillRgn(Handle, R, Brush.Handle);
      end;
    finally
      DeleteObject(R);
    end;
  end;
end;

{----------------DrawBorder}

procedure DrawBorder(Canvas: TCanvas; ORect, IRect: TRect; const C: htColorArray;
  const S: htBorderStyleArray; BGround: TColor; Print: boolean);
{Draw the 4 sides of a border.  The sides may be of different styles or colors.
 The side indices, 0,1,2,3, represent left, top, right, bottom.
 ORect is the outside rectangle of the border, IRect the inside Rectangle.
 BGround is the background color used for the bssDouble style}
var
  PO, PI, PM, P1, P2, Bnd: BorderPointArray;
  I: Integer;
  Color: TColor;
  MRect: TRect;
  lb: TLogBrush;
  Pn, OldPn: HPen;
  W, D: array[0..3] of Integer;
  InPath: boolean;
  PenType, Start: Integer;
  StyleSet: set of BorderStyleType;
  OuterRegion, InnerRegion: THandle;
  Brush: TBrush;

begin
{Limit the borders to somewhat more than the screen size}

  ORect.Bottom := Min(ORect.Bottom, BotLim);
  ORect.Top := Max(ORect.Top, TopLim);
  IRect.Bottom := Min(IRect.Bottom, BotLim);
  IRect.Top := Max(IRect.Top, TopLim);

{Widths are needed for Dashed, Dotted, and Double}
  W[0] := IRect.Left - Orect.Left;
  W[1] := IRect.Top - Orect.Top;
  W[2] := ORect.Right - IRect.Right;
  W[3] := ORect.Bottom - IRect.Bottom;
  if (W[0] = 0) and (W[1] = 0) and (W[2] = 0) and (W[3] = 0) then
    exit;

{Find out what style types are represented in this border}
  StyleSet := [];
  for I := 0 to 3 do
    Include(StyleSet, S[I]);

{find the outside and inside corner points for the border segments}
  with ORect do
  begin
    PO[0] := Point(Left, Bottom);
    PO[1] := TopLeft;
    PO[2] := Point(Right, Top);
    PO[3] := BottomRight;
  end;
  with IRect do
  begin
    PI[0] := Point(Left, Bottom);
    PI[1] := TopLeft;
    PI[2] := Point(Right, Top);
    PI[3] := BottomRight;
  end;

{Points midway between the outer and inner rectangle are needed for
 ridge, groove, dashed, dotted styles}
  if [bssRidge, bssGroove, bssDotted, bssDashed] * StyleSet <> [] then
  begin
    MRect := Rect((ORect.Left + IRect.Left) div 2, (ORect.Top + IRect.Top) div 2,
      (ORect.Right + IRect.Right) div 2, (ORect.Bottom + IRect.Bottom) div 2);
    with MRect do
    begin
      PM[0] := Point(Left, Bottom);
      PM[1] := TopLeft;
      PM[2] := Point(Right, Top);
      PM[3] := BottomRight;
    end;
  end;

{the Double style needs the space between inner and outer rectangles divided
 into three parts}
  if bssDouble in StyleSet then
  begin
    for I := 0 to 3 do
    begin
      D[I] := W[I] div 3;
      if W[I] mod 3 = 2 then
        Inc(D[I]);
    end;

    with ORect do
      MRect := Rect(Left + D[0], Top + D[1], Right - D[2], Bottom - D[3]);

    with MRect do
    begin
      P1[0] := Point(Left, Bottom);
      P1[1] := TopLeft;
      P1[2] := Point(Right, Top);
      P1[3] := BottomRight;
    end;

    with IRect do
      MRect := Rect(Left - D[0], Top - D[1], Right + D[2], Bottom + D[3]);

    with MRect do
    begin
      P2[0] := Point(Left, Bottom);
      P2[1] := TopLeft;
      P2[2] := Point(Right, Top);
      P2[3] := BottomRight;
    end;
  end;

{double, dotted, dashed styles need a background fill}
  if (BGround <> clNone) and ([bssDouble, bssDotted, bssDashed] * StyleSet <> []) then
  begin
    with ORect do
      OuterRegion := CreateRectRgn(Left, Top, Right, Bottom);
    with IRect do
      InnerRegion := CreateRectRgn(Left, Top, Right, Bottom);
    CombineRgn(OuterRegion, OuterRegion, InnerRegion, RGN_DIFF);
    Brush := TBrush.Create;
    try
      Brush.Color := BGround or PalRelative;
      Brush.Style := bsSolid;
      FillRgn(Canvas.Handle, OuterRegion, Brush.Handle);
    finally
      Brush.Free;
      DeleteObject(OuterRegion);
      DeleteObject(InnerRegion);
    end;
  end;

  InPath := False;
  Pn := 0;
  OldPn := 0;
  Start := 0;

  try
    for I := 0 to 3 do
      if S[I] in [bssSolid, bssInset, bssOutset] then
      begin
        Bnd[0] := PO[I];
        Bnd[1] := PO[(I + 1) mod 4];
        Bnd[2] := PI[(I + 1) mod 4];
        Bnd[3] := PI[I];
        Color := C[I] or PalRelative;
        case S[I] of
          bssSolid:
            DrawOnePolygon(Canvas, Bnd, Color, I, Print);
          bssInset:
            begin
              if I in [0, 1] then
                Color := Darker(C[I]) or PalRelative
              else
                Color := Lighter(C[I]) or PalRelative;
              DrawOnePolygon(Canvas, Bnd, Color, I, Print);
            end;
          bssOutset:
            begin
              if (I in [2, 3]) then
                Color := Darker(C[I]) or PalRelative
              else
                Color := Lighter(C[I]) or PalRelative;
              DrawOnePolygon(Canvas, Bnd, Color, I, Print);
            end;
        end;
      end
      else if S[I] in [bssRidge, bssGroove] then
      begin {ridge or groove}
        Bnd[0] := PO[I];
        Bnd[1] := PO[(I + 1) mod 4];
        Bnd[2] := PM[(I + 1) mod 4];
        Bnd[3] := PM[I];
        case S[I] of
          bssGroove:
            begin
              if I in [0, 1] then
                Color := Darker(C[I]) or PalRelative
              else
                Color := Lighter(C[I]) or PalRelative;
              DrawOnePolygon(Canvas, Bnd, Color, I, Print);
            end;
          bssRidge:
            begin
              if (I in [2, 3]) then
                Color := Darker(C[I]) or PalRelative
              else
                Color := Lighter(C[I]) or PalRelative;
              DrawOnePolygon(Canvas, Bnd, Color, I, Print);
            end;
        end;
        Bnd[0] := PM[I];
        Bnd[1] := PM[(I + 1) mod 4];
        Bnd[2] := PI[(I + 1) mod 4];
        Bnd[3] := PI[I];
        case S[I] of
          bssRidge:
            begin
              if I in [0, 1] then
                Color := Darker(C[I]) or PalRelative
              else
                Color := Lighter(C[I]) or PalRelative;
              DrawOnePolygon(Canvas, Bnd, Color, I, Print);
            end;
          bssGroove:
            begin
              if (I in [2, 3]) then
                Color := Darker(C[I]) or PalRelative
              else
                Color := Lighter(C[I]) or PalRelative;
              DrawOnePolygon(Canvas, Bnd, Color, I, Print);
            end;
        end;
      end
      else if S[I] = bssDouble then
      begin
        Color := C[I] or PalRelative;
        Bnd[0] := PO[I];
        Bnd[1] := PO[(I + 1) mod 4];
        Bnd[2] := P1[(I + 1) mod 4];
        Bnd[3] := P1[I];
        DrawOnePolygon(Canvas, Bnd, Color, I, Print);
        Bnd[0] := P2[I];
        Bnd[1] := P2[(I + 1) mod 4];
        Bnd[2] := PI[(I + 1) mod 4];
        Bnd[3] := PI[I];
        DrawOnePolygon(Canvas, Bnd, Color, I, Print);
      end
      else if S[I] in [bssDashed, bssDotted] then
      begin
        if not InPath then
        begin
          lb.lbStyle := BS_SOLID;
          lb.lbColor := C[I] or PalRelative;
          lb.lbHatch := 0;
          if S[I] = bssDotted then
            PenType := PS_Dot or ps_EndCap_Round
          else
            PenType := PS_Dash or ps_EndCap_Square;
          Pn := ExtCreatePen(PS_GEOMETRIC or PenType or ps_Join_Miter, W[I], lb, 0, nil);
          OldPn := SelectObject(Canvas.Handle, Pn);
          BeginPath(Canvas.Handle);
          movetoEx(Canvas.Handle, PM[I].x, PM[I].y, nil);
          Start := I;
          InPath := True;
        end;
        LineTo(Canvas.Handle, PM[(I + 1) mod 4].x, PM[(I + 1) mod 4].y);
        if (I = 3) or (S[I + 1] <> S[I]) or (C[I + 1] <> C[I]) or (W[I + 1] <> W[I]) then
        begin
          if (I = 3) and (Start = 0) then
            CloseFigure(Canvas.Handle); {it's a closed path}
          EndPath(Canvas.Handle);
          StrokePath(Canvas.Handle);
          SelectObject(Canvas.Handle, OldPn);
          DeleteObject(Pn);
          Pn := 0;
          InPath := False;
        end;
      end;
  finally
    if Pn <> 0 then
    begin
      SelectObject(Canvas.Handle, OldPn);
      DeleteObject(Pn);
    end;
  end;
end;

{ TViewerBase }

//-- BG ---------------------------------------------------------- 24.11.2011 --
constructor TViewerBase.Create(AOwner: TComponent);
begin
  inherited;
  PrintMarginLeft := 2.0;
  PrintMarginRight := 2.0;
  PrintMarginTop := 2.0;
  PrintMarginBottom := 2.0;
  PrintMaxHPages := 2;
  PrintScale := 1.0;
  Charset := DEFAULT_CHARSET;
  MarginHeight := 5;
  MarginWidth := 10;
  DefBackground := clBtnFace;
  DefFontColor := clBtnText;
  DefHotSpotColor := clBlue;
  DefOverLinkColor := clBlue;
  DefVisitedLinkColor := clPurple;
  VisitedMaxCount := 50;
  DefFontSize := 12;
  DefFontName := 'Times New Roman';
  DefPreFontName := 'Courier New';
  ImageCacheCount := 5;
  FQuirksMode := qmDetect;
end;

//-- BG ---------------------------------------------------------- 16.11.2011 --
constructor TViewerBase.CreateCopy(Owner: TComponent; Source: TViewerBase);
begin
  Create(Owner);

  Charset := Source.Charset;
  CodePage := Source.CodePage;
  DefBackGround := Source.DefBackGround;
  DefFontColor := Source.DefFontColor;
  DefFontName := Source.DefFontName;
  DefFontSize := Source.DefFontSize;
  DefHotSpotColor := Source.DefHotSpotColor;
  DefOverLinkColor := Source.DefOverLinkColor;
  DefPreFontName := Source.DefPreFontName;
  DefVisitedLinkColor := Source.DefVisitedLinkColor;
  NoSelect := Source.NoSelect;
  ServerRoot := Source.ServerRoot;

  MarginHeight := Source.MarginHeight;
  MarginWidth := Source.MarginWidth;
  PrintMarginBottom := Source.PrintMarginBottom;
  PrintMarginLeft := Source.PrintMarginLeft;
  PrintMarginRight := Source.PrintMarginRight;
  PrintMarginTop := Source.PrintMarginTop;
  PrintMaxHPages := Source.PrintMaxHPages;
  PrintScale := Source.PrintScale;
  FQuirksMode := Source.QuirksMode;
  HistoryMaxCount := Source.HistoryMaxCount;
  ImageCacheCount := Source.ImageCacheCount;
  VisitedMaxCount := Source.VisitedMaxCount;

  OnBitmapRequest := Source.OnBitmapRequest;
  OnDragDrop := Source.OnDragDrop;
  OnDragOver := Source.OnDragOver;
  OnHistoryChange := Source.OnHistoryChange;
  OnHotSpotTargetClick := Source.OnHotSpotTargetClick;
  OnHotSpotTargetCovered := Source.OnHotSpotTargetCovered;
  OnImageClick := Source.OnImageClick;
  OnImageOver := Source.OnImageOver;
  OnImageRequest := Source.OnImageRequest;
  OnImageRequested := Source.OnImageRequested;
  OnInclude := Source.OnInclude;
  OnLink := Source.OnLink;
  OnMeta := Source.OnMeta;
  OnMouseDouble := Source.OnMouseDouble;
  OnObjectBlur := Source.OnObjectBlur;
  OnObjectChange := Source.OnObjectChange;
  OnObjectClick := Source.OnObjectClick;
  OnObjectFocus := Source.OnObjectFocus;
  OnObjectTag := Source.OnObjectTag;
  OnObjectTag := Source.OnObjectTag;
  OnPanelCreate := Source.OnPanelCreate;
  OnPanelDestroy := Source.OnPanelDestroy;
  OnPanelPrint := Source.OnPanelPrint;
  OnParseBegin := Source.OnParseBegin;
  OnParseEnd := Source.OnParseEnd;
  OnPrinted := Source.OnPrinted;
  OnPrintFooter := Source.OnPrintFooter;
  OnPrintHeader := Source.OnPrintHeader;
  OnPrinting := Source.OnPrinting;
  OnProcessing := Source.OnProcessing;
  OnProgress := Source.OnProgress;
  OnScript := Source.OnScript;
  OnSoundRequest := Source.OnSoundRequest;

  Cursor := Cursor;
  OnKeyDown := OnKeyDown;
  OnKeyPress := OnKeyPress;
  OnKeyUp := OnKeyUp;
  OnMouseDown := OnMouseDown;
  OnMouseMove := OnMouseMove;
  OnMouseUp := OnMouseUp;
end;

procedure TViewerBase.SetActiveColor(const Value: TColor);
begin
  FOverColor := Value;
end;

procedure TViewerBase.SetCharset(const Value: TFontCharset);
begin
  FCharSet := Value;
end;

procedure TViewerBase.SetCodePage(const Value: Integer);
begin
  FCodePage := Value;
end;

procedure TViewerBase.SetDefBackground(const Value: TColor);
begin
  FBackground := Value;
end;

procedure TViewerBase.SetOnBitmapRequest(const Value: TGetBitmapEvent);
begin
  FOnBitmapRequest := Value;
end;

procedure TViewerBase.SetOnDragDrop(const Value: TDragDropEvent);
begin
  FOnDragDrop := Value;
end;

procedure TViewerBase.SetOnDragOver(const Value: TDragOverEvent);
begin
  FOnDragOver := Value;
end;

procedure TViewerBase.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
end;

procedure TViewerBase.SetFontName(const Value: TFontName);
begin
  FFontName := Value;
end;

procedure TViewerBase.SetFontSize(const Value: Integer);
begin
  FFontSize := Value;
end;

procedure TViewerBase.SetHistoryMaxCount(const Value: Integer);
begin
  FHistoryMaxCount := Value;
end;

procedure TViewerBase.SetHotSpotColor(const Value: TColor);
begin
  FHotSpotColor := Value;
end;

procedure TViewerBase.SetImageCacheCount(const Value: Integer);
begin
  FImageCacheCount := Value;
end;

procedure TViewerBase.SetMarginHeight(const Value: Integer);
begin
  FMarginHeight := Value;
end;

procedure TViewerBase.SetMarginWidth(const Value: Integer);
begin
  FMarginWidth := Value;
end;

procedure TViewerBase.SetNoSelect(const Value: Boolean);
begin
  FNoSelect := Value;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnHistoryChange(const Value: TNotifyEvent);
begin
  FOnHistoryChange := Value;
end;

procedure TViewerBase.SetOnHotSpotTargetClick(const Value: THotSpotTargetClickEvent);
begin
  FOnHotSpotTargetClick := Value;
end;

procedure TViewerBase.SetOnHotSpotTargetCovered(const Value: THotSpotTargetEvent);
begin
  FOnHotSpotTargetCovered := Value;
end;

procedure TViewerBase.SetOnImageClick(const Value: TImageClickEvent);
begin
  FOnImageClick := Value;
end;

procedure TViewerBase.SetOnImageOver(const Value: TImageOverEvent);
begin
  FOnImageOver := Value;
end;

procedure TViewerBase.SetOnImageRequest(const Value: TGetImageEvent);
begin
  FOnImageRequest := Value;
end;

procedure TViewerBase.SetOnImageRequested(const Value: TGottenImageEvent);
begin
  FOnImageRequested := Value;
end;

procedure TViewerBase.SetOnInclude(const Handler: TIncludeType);
begin
  FOnInclude := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnLink(const Handler: TLinkType);
begin
  FOnLink := Handler;
end;

procedure TViewerBase.SetOnMeta(const Value: TMetaType);
begin
  FOnMeta := Value;
end;

procedure TViewerBase.SetOnMouseDouble(const Value: TMouseEvent);
begin
  FOnMouseDouble := Value;
end;

procedure TViewerBase.SetOnObjectBlur(const Value: ThtObjectEvent);
begin
  FOnObjectBlur := Value;
end;

procedure TViewerBase.SetOnObjectChange(const Value: ThtObjectEvent);
begin
  FOnObjectChange := Value;
end;

procedure TViewerBase.SetOnObjectClick(const Value: TObjectClickEvent);
begin
  FOnObjectClick := Value;
end;

procedure TViewerBase.SetOnObjectFocus(const Value: ThtObjectEvent);
begin
  FOnObjectFocus := Value;
end;

procedure TViewerBase.SetOnObjectTag(const Value: TObjectTagEvent);
begin
  FOnObjectTag := Value;
end;

procedure TViewerBase.SetOnPanelCreate(const Value: TPanelCreateEvent);
begin
  FOnPanelCreate := Value;
end;

procedure TViewerBase.SetOnPanelDestroy(const Value: TPanelDestroyEvent);
begin
  FOnPanelDestroy := Value;
end;

procedure TViewerBase.SetOnPanelPrint(const Value: TPanelPrintEvent);
begin
  FOnPanelPrint := Value;
end;

procedure TViewerBase.SetOnParseBegin(const Value: TParseEvent);
begin
  FOnParseBegin := Value;
end;

procedure TViewerBase.SetOnParseEnd(const Value: TNotifyEvent);
begin
  FOnParseEnd := Value;
end;

procedure TViewerBase.SetOnPrinted(const Value: THTMLViewPrinted);
begin
  FOnPrinted := Value;
end;

procedure TViewerBase.SetOnPrintFooter(const Value: TPagePrinted);
begin
  FOnPrintFooter := Value;
end;

procedure TViewerBase.SetOnPrintHeader(const Value: TPagePrinted);
begin
  FOnPrintHeader := Value;
end;

procedure TViewerBase.SetOnPrinting(const Value: THTMLViewPrinting);
begin
  FOnPrinting := Value;
end;

procedure TViewerBase.SetOnProcessing(const Value: TProcessingEvent);
begin
  FOnProcessing := Value;
end;

procedure TViewerBase.SetOnProgress(const Value: ThtProgressEvent);
begin
  FOnProgress := Value;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnScript(const Handler: TScriptEvent);
begin
  FOnScript := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnSoundRequest(const Handler: TSoundType);
begin
  FOnSoundRequest := Handler;
end;

procedure TViewerBase.SetPreFontName(const Value: TFontName);
begin
  FPreFontName := Value;
end;

procedure TViewerBase.SetPrintMarginBottom(const Value: Double);
begin
  FPrintMarginBottom := Value;
end;

procedure TViewerBase.SetPrintMarginLeft(const Value: Double);
begin
  FPrintMarginLeft := Value;
end;

procedure TViewerBase.SetPrintMarginRight(const Value: Double);
begin
  FPrintMarginRight := Value;
end;

procedure TViewerBase.SetPrintMarginTop(const Value: Double);
begin
  FPrintMarginTop := Value;
end;

procedure TViewerBase.SetPrintMaxHPages(const Value: Integer);
begin
  FPrintMaxHPages := Value;
end;

procedure TViewerBase.SetPrintScale(const Value: Double);
begin
  FPrintScale := Value;
end;

procedure TViewerBase.SetQuirksMode(const AValue: THtQuirksMode);
begin
  FQuirksMode := AValue;
end;

procedure TViewerBase.SetServerRoot(const Value: ThtString);
begin
  FServerRoot := ExcludeTrailingPathDelimiter(Trim(Value));
end;

procedure TViewerBase.SetVisitedColor(const Value: TColor);
begin
  FVisitedColor := Value;
end;

procedure TViewerBase.SetVisitedMaxCount(const Value: Integer);
begin
  FVisitedMaxCount := Value;
end;

procedure TViewerBase.ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y);
end;

procedure TViewerBase.ViewerDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

{ THtmlViewerBase }

//-- BG ---------------------------------------------------------- 12.09.2010 --
procedure THtmlViewerBase.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // just to make it public.
  inherited;
end;

{ TFrameViewerBase }

procedure TFrameViewerBase.wmerase(var msg: TMessage);
begin
  msg.result := 1;
end;

{ TIDObject }

//-- BG ---------------------------------------------------------- 06.03.2011 --
function TIDObject.FreeMe: Boolean;
begin
  Result := False;
end;

{ ThvPanel }

procedure ThvPanel.SetVisible(Value: boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    if FVisible then
      Show
    else
      Hide;
  end;
end;

end.

