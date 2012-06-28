{
Version   11.3
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2012 by HtmlViewer Team

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
  SysUtils, Contnrs, Classes, Graphics, ClipBrd, Controls, Messages, Variants, Types,
{$IFNDEF NoGDIPlus}
  GDIPL2A,
{$ENDIF}
{$ifdef METAFILEMISSING}
  MetaFilePrinter,
{$endif}
  StyleUn, HtmlGlobals, HtmlBuffer, HtmlGif2;

const
  VersionNo = '11.3';
  MaxHScroll = 6000; {max horizontal display in pixels}
  HandCursor = crHandPoint; //10101;
  OldThickIBeamCursor = 2;
  UpDownCursor = 10103;
  UpOnlyCursor = 10104;
  DownOnlyCursor = 10105;
  Tokenleng = 300;
  TopLim = -200; {drawing limits}
  BotLim = 5000;
  FmCtl = #2;
  ImgPan = #4;
  BrkCh = #8;

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
    VType: TWidthType;
  end;

  //BG, 09.09.2009: renamed TGif and TPng to TrGif and TrPng
  //  TGif interfered with same named class.
  Transparency = (NotTransp, LLCorner, TrGif, TrPng);
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
    ReadonlySy, EolSy, MediaSy,
	
    {HTML5 elements}
    HeaderSy, HeaderEndSy,
    SectionSy, SectionEndSy,
    NavSy, NavEndSy,
    ArticleSy, ArticleEndSy,
    AsideSy, AsideEndSy,
    FooterSy, FooterEndSy,
    HGroupSy, HGroupEndSy,
    MarkSy, MarkEndSy);

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
    function Find(Sy: Symb; out T: TAttribute): boolean; {$ifdef UseInline} inline; {$endif}
    function CreateStringList: ThtStringList;
    property TheClass: ThtString read GetClass;
    property TheID: ThtString read GetID;
    property TheTitle: ThtString read GetTitle;
    property TheStyle: TProperties read GetStyle;
    property Items[Index: Integer]: TAttribute read GetAttribute; default;
  end;

//------------------------------------------------------------------------------
// image cache
//------------------------------------------------------------------------------

  TgpObject = TObject;

  TBitmapItem = class(TObject)
  public
    AccessCount: Integer;
    UsageCount: Integer; {how many in use}
    Transp: Transparency; {identifies what the mask is for}
    MImage: TgpObject; {main image, bitmap or animated GIF}
    Mask: TBitmap; {its mask}
    constructor Create(AImage: TgpObject; AMask: TBitmap; Tr: Transparency);
    destructor Destroy; override;
  end;

  TStringBitmapList = class(ThtStringList)
  {a list of bitmap filenames and TBitmapItems}
  private
    FMaxCache: Integer;
    function GetObject(Index: Integer): TBitmapItem; reintroduce;
  public
    constructor Create;
    destructor Destroy; override;
    function AddObject(const S: ThtString; AObject: TBitmapItem): Integer; reintroduce;
    function GetImage(I: Integer): TgpObject; {$ifdef UseInline} inline; {$endif}
    procedure BumpAndCheck;
    procedure Clear; override;
    procedure DecUsage(const S: ThtString);
    procedure IncUsage(const S: ThtString);
    procedure PurgeCache;
    procedure SetCacheCount(N: Integer);
    property MaxCache: Integer read FMaxCache;
    property Objects[Index: Integer]: TBitmapItem read GetObject;
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
    function PtInArea(X, Y: Integer): Boolean;
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
    function GetURL(X, Y: Integer; out URLTarg: TURLTarget; out ATitle: ThtString): boolean;
    procedure AddArea(Attrib: TAttributeList);
  end;

  TFontObjBase = class(TObject) {font information}
  public
    UrlTarget: TUrlTarget;
  end;

//------------------------------------------------------------------------------
// device independent bitmap wrapper
//------------------------------------------------------------------------------

  TDib = class(TObject)
  private
    Info: PBitmapInfoHeader;
    InfoSize: Integer;
    Image: Pointer;
    ImageSize: Integer;
    FHandle: THandle;
    procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP);
    procedure GetDIBX(DC: HDC; Bitmap: HBITMAP; Palette: HPALETTE);
    procedure Allocate(Size: Integer);
    procedure DeAllocate;
  public
    constructor CreateDIB(DC: HDC; Bitmap: TBitmap);
    destructor Destroy; override;
    function CreateDIBmp: hBitmap;
    procedure DrawDIB(DC: HDC; X, Y, W, H: Integer; ROP: DWord);
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
    procedure GetClearY(out CL, CR: Integer);
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

  { TCharCollection }

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
    procedure Add(C: AnsiChar; Index: Integer); overload;
    procedure Add(C: WideChar; Index: Integer); overload;
    procedure Add(const S: ThtString; Index: Integer); overload;
    procedure Clear;
    procedure Concat(T: TCharCollection);

    property AsString: ThtString read GetAsString;
    property Indices: PIndexArray read FIndices;
    property Size: Integer read GetSize;
  end;

  TokenObj = class
  private
    St: WideString;
    StringOK: boolean;
    FCapacity: Integer;
    FCount: Integer;
    function GetString: WideString;
    procedure SetCapacity(NewCapacity: Integer);
  public
    C: PChrArray;
    I: PIndexArray;
    constructor Create;
    destructor Destroy; override;
    procedure AddUnicodeChar(Ch: WideChar; Ind: Integer);
    procedure AddString(S: TCharCollection);
    procedure Concat(T: TokenObj);
    procedure Clear;
    procedure Remove(N: Integer);
    procedure Replace(N: Integer; Ch: WideChar);

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property S: WideString read GetString;
  end;

{$IFNDEF NoMetafile}

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

  ThtMetaFile = class(TMetaFile)
  private
    FBitmap, FMask: TBitmap;
    FWhiteBGBitmap: TBitmap;
    function GetBitmap: TBitmap;
    function GetMask: TBitmap;
    procedure Construct;
    function GetWhiteBGBitmap: TBitmap;
  public
    destructor Destroy; override;
    property Bitmap: TBitmap read GetBitmap;
    property Mask: TBitmap read GetMask;
    property WhiteBGBitmap: TBitmap read GetWhiteBGBitmap;
  end;

{$ENDIF}

//------------------------------------------------------------------------------
// TIDObject is base class for all tag objects.
//------------------------------------------------------------------------------
// If they have an ID, the parser puts them into the HtmlViewer's IDNameList,
// a TIDObjectList, where they can be obtained from by ID.
// Their Y coordinates can be retrieved and HtmlViewer can scroll to them.
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

  TImageType = (NoImage, Bmp, Gif, {Gif89,} Png, Jpg);

  htColorArray = packed array[0..3] of TColor;
  htBorderStyleArray = packed array[0..3] of BorderStyleType;

//BG, 11.09.2010: moved to this unit to reduce circular dependencies:

  guResultType = set of (guUrl, guControl, guTitle);

//------------------------------------------------------------------------------
// TViewerBase is base class for both THtmlViewer and TFrameViewer
//------------------------------------------------------------------------------

  TGetStreamEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TMemoryStream) of object;
  TIncludeType = procedure(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer) of object;
  TLinkType = procedure(Sender: TObject; const Rel, Rev, Href: ThtString) of object;
  TMetaType = procedure(Sender: TObject; const HttpEq, Name, Content: ThtString) of object;
  TScriptEvent = procedure(Sender: TObject; const Name, ContentType, Src, Script: ThtString) of object;
  TSoundType = procedure(Sender: TObject; const SRC: ThtString; Loop: Integer; Terminate: boolean) of object;

  TViewerBase = class(TWinControl)
  private
    FOnInclude: TIncludeType;
    FOnLink: TLinkType;
    FOnScript: TScriptEvent;
    FOnSoundRequest: TSoundType;
  protected
    // set to determine if child objects should be in "quirks" mode
    //This must be protected because it's set directly in a descendant
    FUseQuirksMode : Boolean;
    FQuirksMode : THtQuirksMode;
    procedure SetOnInclude(Handler: TIncludeType); virtual;
    procedure SetOnLink(Handler: TLinkType); virtual;
    procedure SetOnScript(Handler: TScriptEvent); virtual;
    procedure SetOnSoundRequest(Handler: TSoundType); virtual;
    procedure SetQuirksMode(const AValue: THtQuirksMode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property QuirksMode : THtQuirksMode read FQuirksMode write SetQuirksMode;

    property OnInclude: TIncludeType read FOnInclude write SetOnInclude;
    property OnLink: TLinkType read FOnLink write SetOnLink;
    property OnScript: TScriptEvent read FOnScript write SetOnScript;
    property OnSoundRequest: TSoundType read FOnSoundRequest write SetOnSoundRequest;
    property UseQuirksMode : Boolean read FUseQuirksMode;
  end;

  TablePartType = (Normal, DoHead, DoBody1, DoBody2, DoBody3, DoFoot);
  TTablePartRec = class
    TablePart: TablePartType;
    PartStart: Integer;
    PartHeight: Integer;
    FootHeight: Integer;
  end;

  THtmlViewerBase = class(TViewerBase)
  public
    TablePartRec: TTablePartRec;
    function HtmlExpandFilename(const Filename: ThtString): ThtString; virtual; abstract;
    function ShowFocusRect: Boolean; virtual; abstract;
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure htProgress(Percent: Integer); virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
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

var
  DefBitMap, ErrorBitMap, ErrorBitmapMask: TBitMap;
  WaitStream: TMemoryStream;
  ErrorStream: TMemoryStream;

//------------------------------------------------------------------------------
// string methods
//------------------------------------------------------------------------------

function StrLenW(Str: PWideChar): Cardinal;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function WidePos(SubStr, S: WideString): Integer;
function WideTrim(const S: WideString): WideString;
function WideUpperCase1(const S: WideString): WideString; {$ifdef UNICODE} inline; {$endif}
function WideLowerCase1(const S: WideString): WideString; {$ifdef UNICODE} inline; {$endif}
function WideSameText1(const S1, S2: WideString): boolean; {$ifdef UseInline} inline; {$endif}
function WideSameStr1(const S1, S2: WideString): boolean;  {$ifdef UseInline} inline; {$endif}

function WideStringToMultibyte(CodePage: Integer; W: WideString): AnsiString;

function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; out Extent: TSize): Integer;
function GetXExtent(DC: HDC; P: PWideChar; N: Integer): Integer;
procedure WrapTextW(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; S: WideString);

//------------------------------------------------------------------------------
// image methods
//------------------------------------------------------------------------------

function LoadImageFromFile(const FName: ThtString; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
function LoadImageFromStream(Stream: TStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
function KindOfImage(Stream: TStream): TImageType;

function GetImageHeight(Image: TGpObject): Integer;
function GetImageWidth(Image: TGpObject): Integer;
function EnlargeImage(Image: TGpObject; W, H: Integer): TBitmap;

function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
procedure FinishTransparentBitmap(ahdc: HDC; InImage, Mask: TBitmap; xStart, yStart, W, H: Integer);
procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: Integer; Bitmap, Mask: TBitmap; YI, HI: Integer);

{$IFNDEF NoGDIPlus}
procedure DrawGpImage(Handle: THandle; Image: TGPImage; DestX, DestY: Integer); overload;
procedure DrawGpImage(Handle: THandle; Image: TGpImage; DestX, DestY,
  SrcX, SrcY, SrcW, SrcH: Integer); overload;
procedure StretchDrawGpImage(Handle: THandle; Image: TGpImage; DestX, DestY, DestW, DestH: Integer);
procedure PrintGpImageDirect(Handle: THandle; Image: TGpImage; DestX, DestY: Integer;
  ScaleX, ScaleY: single);
procedure StretchPrintGpImageDirect(Handle: THandle; Image: TGpImage;
  DestX, DestY, DestW, DestH: Integer;
  ScaleX, ScaleY: single);
procedure StretchPrintGpImageOnColor(Canvas: TCanvas; Image: TGpImage;
  DestX, DestY, DestW, DestH: Integer; Color: TColor = clWhite);
{$ENDIF NoGDIPlus}

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

function WidePos(SubStr, S: WideString): Integer;
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

{----------------WideUpperCase1}

{$ifdef UNICODE}

function WideUpperCase1(const S: WideString): WideString;
begin
  Result := WideUpperCase(S);
end;

function WideLowerCase1(const S: WideString): WideString;
begin
  Result := WideLowerCase(S);
end;

{$else}

function WideUpperCase1(const S: WideString): WideString;
var
  Len, NewLen: Integer;
  Tmp: string;
begin
  Len := Length(S);
  if not IsWin32Platform then
  begin
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then
      CharUpperBuffW(Pointer(Result), Len);
  end
  else
  begin {win95,98,ME}
    SetLength(Tmp, 2 * Len);
    NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Len, PChar(Tmp), 2 * Len, nil, nil);
    SetLength(Tmp, NewLen);
    Tmp := AnsiUppercase(Tmp);
    SetLength(Result, Len);
    MultibyteToWideChar(CP_ACP, 0, PChar(Tmp), NewLen, PWideChar(Result), Len);
  end;
end;

function WideLowerCase1(const S: WideString): WideString;
var
  Len, NewLen: Integer;
  Tmp: string;
begin
  Len := Length(S);
  if not IsWin32Platform then
  begin
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then
      CharLowerBuffW(Pointer(Result), Len);
  end
  else
  begin {win95,98,ME}
    SetLength(Tmp, 2 * Len);
    NewLen := WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Len, PChar(Tmp), 2 * Len, nil, nil);
    SetLength(Tmp, NewLen);
    Tmp := AnsiLowercase(Tmp);
    SetLength(Result, Len);
    MultibyteToWideChar(CP_ACP, 0, PChar(Tmp), NewLen, PWideChar(Result), Len);
  end;
end;

{$endif}

function WideSameText1(const S1, S2: WideString): boolean;
begin
  Result := WideUpperCase1(S1) = WideUpperCase1(S2);
end;

function WideSameStr1(const S1, S2: WideString): boolean;
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
  SelectClipRgn(Canvas.Handle, Rgn);
end;

function WideTrim(const S: WideString): WideString;
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

procedure WrapTextW(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; S: WideString);
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
      Brush.Color := ThemedColor(clBtnFace)
    else
      Brush.Color := ThemedColor(color);
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
        Pen.Color := ThemedColor(clBtnShadow);
    end;
    MoveTo(X1, Y2);
    LineTo(X1, Y1);
    LineTo(X2, Y1);
    if not MonoBlack then
      if Raised then
        Pen.Color := ThemedColor(clBtnShadow)
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
// Return a TSpecWidth prepared with values given in AsInteger *and* AsString.
// AsString is used to evaluate the type while AsInteger is used to evaluate the value.
// BG, 26.12.2011: Currently percentage is still converted to permille as done before Value became type Integer.
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

constructor TBitmapItem.Create(AImage: TgpObject; AMask: TBitmap; Tr: Transparency);
begin
  inherited Create;
  MImage := AImage;
  Mask := AMask;
  AccessCount := 0;
  Transp := Tr;
end;

destructor TBitmapItem.Destroy;
begin
  Assert(UsageCount = 0, 'Freeing Image still in use');
  MImage.Free;
  Mask.Free;
  inherited Destroy;
end;

constructor TStringBitmapList.Create;
begin
  inherited Create;
  FMaxCache := 4;
  {$IFNDEF NoGDIPlus}
  CheckInitGDIPlus;
  {$ENDIF NoGDIPlus}
end;

destructor TStringBitmapList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  {$IFNDEF NoGDIPlus}
  CheckExitGDIPlus;
  {$ENDIF NoGDIPlus}
  inherited Destroy;
end;

function TStringBitmapList.AddObject(const S: ThtString; AObject: TBitmapItem): Integer;
begin
  Result := inherited AddObject(S, AObject);
  Inc(AObject.UsageCount);
end;

procedure TStringBitmapList.DecUsage(const S: ThtString);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    with Objects[I] do
    begin
      Dec(UsageCount);
      Assert(UsageCount >= 0, 'Cache image usage count < 0');
    end;
end;

procedure TStringBitmapList.IncUsage(const S: ThtString);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    Inc(Objects[I].UsageCount);
end;

procedure TStringBitmapList.SetCacheCount(N: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with Objects[I] do
    begin
      if (AccessCount > N) and (UsageCount <= 0) then
      begin
        Delete(I);
        Free;
      end;
    end;
  FMaxCache := N;
end;

function TStringBitmapList.GetImage(I: Integer): TgpObject;
begin
  with Objects[I] do
  begin
    Result := MImage;
    AccessCount := 0;
    Inc(UsageCount);
  end;
end;

//-- BG ---------------------------------------------------------- 06.03.2011 --
function TStringBitmapList.GetObject(Index: Integer): TBitmapItem;
begin
  Result := TBitmapItem(inherited GetObject(Index));
end;

procedure TStringBitmapList.BumpAndCheck;
var
  I: Integer;
  Tmp: TBitmapItem;
begin
  for I := Count - 1 downto 0 do
  begin
    Tmp := Objects[I];
    Inc(Tmp.AccessCount);
    if (Tmp.AccessCount > FMaxCache) and (Tmp.UsageCount <= 0) then
    begin
      Delete(I);
      Tmp.Free; {the TBitmapItem}
    end;
  end;
end;

procedure TStringBitmapList.PurgeCache;
var
  I: Integer;
  Tmp: TBitmapItem;
begin
  for I := Count - 1 downto 0 do
  begin
    Tmp := Objects[I];
    if (Tmp.UsageCount <= 0) then
    begin
      Delete(I);
      Tmp.Free; {the TBitmapItem}
    end;
  end;
end;

procedure TStringBitmapList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
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

function TAttributeList.Find(Sy: Symb; out T: TAttribute): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].which = Sy then
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
    Prop := TProperties.Create;
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
{$ifdef LCL}
begin
  Clipboard.AddFormat(CF_UNICODETEXT, Buffer[0], BufferLeng * sizeof(WIDECHAR));
end;
{$else}
var
  Len: Integer;
  Mem: HGLOBAL;
  Wuf: PWideChar;
begin
  Len := BufferLeng;
  Mem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, (Len + 1) * SizeOf(ThtChar));
  try
    Wuf := GlobalLock(Mem);
    try
      Move(Buffer[0], Wuf[0], Len * SizeOf(ThtChar));
      Wuf[Len] := #0;
      // BG, 28.06.2012: use API method. The vcl clipboard does not support multiple formats.
      SetClipboardData(CF_UNICODETEXT, Mem);
    finally
      GlobalUnlock(Mem);
    end;
  except
    GlobalFree(Mem);
  end;
end;
{$endif}

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
function TMapArea.PtInArea(X, Y: Integer): Boolean;
begin
  Result := PtInRegion(FRegion, X, Y);
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

function TMapItem.GetURL(X, Y: Integer; out URLTarg: TUrlTarget; out ATitle: ThtString): boolean;
var
  I: Integer;
  Area: TMapArea;
begin
  Result := False;
  URLTarg := nil;
  for I := 0 to FAreas.Count - 1 do
  begin
    Area := FAreas[I];
    if Area.PtInArea(X, Y) then
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


function KindOfImage(Stream: TStream): TImageType;
var
  Pos: Int64;
  Magic: DWord;
  WMagic: Word absolute Magic;
//  BMagic: Byte absolute Magic;
begin
  Pos := Stream.Position;
  Stream.Position := 0;
  try
    Stream.Read(Magic, sizeof(Magic));
    if Magic = $38464947 then
    begin
//      Stream.Read(BMagic, sizeof(BMagic));
//      if BMagic = Ord('9') then
//        Result := Gif89
//      else
        Result := Gif;
    end
    else if Magic = $474E5089 then
      Result := Png
    else
      case WMagic of
        $4D42: Result := Bmp;
        $D8FF: Result := Jpg;
      else
        Result := NoImage;
      end;
  finally
    Stream.Position := Pos;
  end;
end;

function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
begin
  try
    if ColorValid then
      Image.TransparentColor := AColor; {color has already been selected}
  {else the transparent color is the lower left pixel of the bitmap}

    Image.Transparent := True;

    Result := TBitmap.Create;
    try
      Result.Handle := Image.ReleaseMaskHandle;
      Image.Transparent := False;
    except
      FreeAndNil(Result);
    end;
  except
    Result := nil;
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function LoadImageFromStream(Stream: TStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
// extracted from ThtDocument.GetTheImage(), ThtDocument.InsertImage(), and ThtDocument.ReplaceImage()

  function ConvertImage(Bitmap: TBitmap): TBitmap;
  {convert bitmap into a form for BitBlt later}

    function DIBConvert: TBitmap;
    var
      DC: HDC;
      DIB: TDib;
      OldBmp: HBitmap;
      OldPal: HPalette;
      Hnd: HBitmap;
    begin
      DC := CreateCompatibleDC(0);
      OldBmp := SelectObject(DC, Bitmap.Handle);
      OldPal := SelectPalette(DC, ThePalette, False);
      RealizePalette(DC);
      DIB := TDib.CreateDIB(DC, Bitmap);
      Hnd := DIB.CreateDIBmp;
      DIB.Free;
      SelectPalette(DC, OldPal, False);
      SelectObject(DC, OldBmp);
      DeleteDC(DC);
      Bitmap.Free;
      Result := TBitmap.Create;
      Result.Handle := Hnd;
      if (ColorBits = 8) and (Result.Palette = 0) then
        Result.Palette := CopyPalette(ThePalette);
    end;

  begin
    if ColorBits > 8 then
    begin
      if Bitmap.PixelFormat <= pf8bit then
        Result := DIBConvert
      else
        Result := Bitmap;
    end
    else if Bitmap.HandleType = bmDIB then
    begin
      Result := GetBitmap(Bitmap);
      Bitmap.Free;
    end
    else
      Result := DIBConvert;
  end;

  function LoadGifFromStream(Stream: TStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
  var
    GifImage: TGifImage;
    Bitmap: TBitmap;
  begin
    GifImage := CreateAGifFromStream(Stream);
    if GifImage.IsAnimated then
      Result := GifImage
    else
    begin
      Bitmap := TBitmap.Create;
      try
        Bitmap.Assign(GifImage.MaskedBitmap);
        if GifImage.IsTransparent then
        begin
          AMask := TBitmap.Create;
          AMask.Assign(GifImage.Mask);
          Transparent := TrGif;
        end
        else if Transparent = LLCorner then
          AMask := GetImageMask(Bitmap, False, 0);
        GifImage.Free;
        Result := Bitmap;
      except
        Bitmap.Free;
        raise;
      end;
    end;
  end;

  function LoadPngFromStream(Stream: TStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
{$ifdef LCL}
  var
    PngImage: TPortableNetworkGraphic;
    Bitmap: TBitmap;
{$endif}
  begin
{$IFNDEF NoGDIPlus}
    if GDIPlusActive then
    begin
      Result := TgpImage.Create(Stream);
      Transparent := NotTransp;
      exit;
    end;
{$ENDIF !NoGDIPlus}
{$ifdef LCL}
    PngImage := TPortableNetworkGraphic.Create;
    try
      Transparent := TrPng;
      PngImage.LoadFromStream(Stream);
      if ColorBits <= 8 then
        PngImage.PixelFormat := pf8bit
      else
        PngImage.PixelFormat := pf24bit;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Assign(PngImage);
        PngImage.Mask(clDefault);
        if PngImage.MaskHandleAllocated then
        begin
          AMask := TBitmap.Create;
          AMask.LoadFromBitmapHandles(PngImage.MaskHandle, 0);
        end;
        Result := ConvertImage(Bitmap);
      except
        Bitmap.Free;
        raise;
      end;
    finally
      PngImage.Free;
    end;
{$else}
    Result := nil;
{$endif}
  end;

  function LoadJpgFromStream(Stream: TStream; var Transparent: Transparency; var AMask: TBitmap): TBitmap;
  var
    jpImage: TJpegImage;
  begin
    Transparent := NotTransp;
    jpImage := TJpegImage.Create;
    try
      jpImage.LoadFromStream(Stream);
      if ColorBits <= 8 then
      begin
        jpImage.PixelFormat := {$ifdef LCL} pf8bit {$else} jf8bit {$endif};
        if not jpImage.GrayScale and (ColorBits = 8) then
          jpImage.Palette := CopyPalette(ThePalette);
      end
      else
        jpImage.PixelFormat := {$ifdef LCL} pf24bit {$else} jf24bit {$endif};
      Result := TBitmap.Create;
      try
        Result.Assign(jpImage);
        Result := ConvertImage(Result);
      except
        Result.Free;
        raise;
      end;
    finally
      jpImage.Free;
    end;
  end;

  function LoadBmpFromStream(Stream: TStream; Transparent: Transparency; var AMask: TBitmap): TBitmap;
  begin
    Result := TBitmap.Create;
    try
      Result.LoadFromStream(Stream);
      Result := ConvertImage(Result);
      if Transparent = LLCorner then
        AMask := GetImageMask(Result, False, 0);
    except
      Result.Free;
      raise;
    end;
  end;

  function LoadMetaFileFromStream(Stream: TStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
  begin
{$IFDEF NoMetafile}
    Result := nil;
{$ELSE}
    Result := ThtMetafile.Create;
    try
      AMask := nil;
      Transparent := NotTransp;
      ThtMetaFile(Result).LoadFromStream(Stream);
    except
      FreeAndNil(Result);
    end;
{$ENDIF}
  end;

begin
  if (Stream = nil) or (Stream.Size < SizeOf(DWord)) then
  begin
    Result := nil;
    exit;
  end;

  Stream.Position := 0;
  case KindOfImage(Stream) of
    Gif: Result := LoadGifFromStream(Stream, Transparent, AMask);
    Png: Result := LoadPngFromStream(Stream, Transparent, AMask);
    Jpg: Result := LoadJpgFromStream(Stream, Transparent, AMask);
    Bmp: Result := LoadBmpFromStream(Stream, Transparent, AMask);
  else
    Result := LoadMetaFileFromStream(Stream, Transparent, AMask);
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function LoadImageFromFile(const FName: ThtString; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
// extracted from ThtDocument.GetTheImage() and redesigned.
// Now the image file is loaded once only (was: 2 to 3 times) and GetImageAndMaskFromFile() is obsolete.
var
  Stream: TStream;
begin {look for the image file}
  Result := nil;
  if FileExists(FName) then
  begin
    Stream := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadImageFromStream(Stream, Transparent, AMask);
    finally
      Stream.Free;
    end;
  end;
end;

{----------------FinishTransparentBitmap }

procedure FinishTransparentBitmap(ahdc: HDC; InImage, Mask: TBitmap; xStart, yStart, W, H: Integer);
var
  bmAndBack,
    bmSave,
    bmBackOld,
    bmObjectOld: HBitmap;
  hdcInvMask,
    hdcMask,
    hdcImage: HDC;
  DestSize, SrcSize: TPoint;
  OldBack, OldFore: TColor;
  BM: {$ifdef LCL} LclType.Bitmap {$else} Windows.TBitmap {$endif};
  Image: TBitmap;

begin
  Image := TBitmap.Create; {protect original image}
  try
    Image.Assign(InImage);

    hdcImage := CreateCompatibleDC(ahdc);
    SelectObject(hdcImage, Image.Handle); { select the bitmap }

  { convert bitmap dimensions from device to logical points}
    SrcSize.x := Image.Width;
    SrcSize.y := Image.Height;
    DPtoLP(hdcImage, SrcSize, 1);

    DestSize.x := W;
    DestSize.y := H;
    DPtoLP(hdcImage, DestSize, 1);

  { create a bitmap for each DC}
  { monochrome DC}
    bmAndBack := CreateBitmap(SrcSize.x, SrcSize.y, 1, 1, nil);

    bmSave := CreateCompatibleBitmap(ahdc, DestSize.x, DestSize.y);
    GetObject(bmSave, SizeOf(BM), @BM);
    if (BM.bmBitsPixel > 1) or (BM.bmPlanes > 1) then
    begin
    { create some DCs to hold temporary data}
      hdcInvMask := CreateCompatibleDC(ahdc);
      hdcMask := CreateCompatibleDC(ahdc);

    { each DC must select a bitmap object to store pixel data}
      bmBackOld := SelectObject(hdcInvMask, bmAndBack);

    { set proper mapping mode}
      SetMapMode(hdcImage, GetMapMode(ahdc));

      bmObjectOld := SelectObject(hdcMask, Mask.Handle);

    { create the inverse of the object mask}
      BitBlt(hdcInvMask, 0, 0, SrcSize.x, SrcSize.y, hdcMask, 0, 0, NOTSRCCOPY);

    {set the background color of the source DC to the color contained in the
     parts of the bitmap that should be transparent, the foreground to the parts that
     will show}
      OldBack := SetBkColor(ahDC, clWhite);
      OldFore := SetTextColor(ahDC, clBlack);

    { Punch out a black hole in the background where the image will go}
      SetStretchBltMode(ahDC, WhiteOnBlack);
      StretchBlt(ahDC, XStart, YStart, DestSize.x, DestSize.y, hdcMask, 0, 0, SrcSize.x, SrcSize.y, SRCAND);

    { mask out the transparent colored pixels on the bitmap}
      BitBlt(hdcImage, 0, 0, SrcSize.x, SrcSize.y, hdcInvMask, 0, 0, SRCAND);

    { XOR the bitmap with the background on the destination DC}
{$IFDEF HalfToneStretching}
      SetStretchBltMode(ahDC, HALFTONE);
{$ELSE}
      SetStretchBltMode(ahDC, COLORONCOLOR);
{$ENDIF}
      StretchBlt(ahDC, XStart, YStart, W, H, hdcImage, 0, 0, Image.Width, Image.Height, SRCPAINT);

      SetBkColor(ahDC, OldBack);
      SetTextColor(ahDC, OldFore);

    { delete the memory bitmaps}
      DeleteObject(SelectObject(hdcInvMask, bmBackOld));
      SelectObject(hdcMask, bmObjectOld);

    { delete the memory DCs}
      DeleteDC(hdcInvMask);
      DeleteDC(hdcMask);
    end
    else
    begin
      DeleteObject(bmAndBack);
    end;
    DeleteObject(bmSave);
    DeleteDC(hdcImage);
  finally
    Image.Free;
  end;
end;

{----------------TDib.CreateDIB}

constructor TDib.CreateDIB(DC: HDC; Bitmap: TBitmap);
{given a TBitmap, construct a device independent bitmap}
var
  ImgSize: DWord;
begin
  InitializeBitmapInfoHeader(Bitmap.Handle);
  ImgSize := Info^.biSizeImage;
  Allocate(ImgSize);
  try
    GetDIBX(DC, Bitmap.Handle, Bitmap.Palette);
  except
    DeAllocate;
    raise;
  end;
end;

destructor TDib.Destroy;
begin
  DeAllocate;
  inherited Destroy;
end;

procedure TDib.Allocate(Size: Integer);
begin
  ImageSize := Size;
  if Size < $FF00 then
    GetMem(Image, Size)
  else
  begin
    FHandle := GlobalAlloc(HeapAllocFlags, Size);
    if FHandle = 0 then
      ABort;
    Image := GlobalLock(FHandle);
  end;
end;

procedure TDib.DeAllocate;
begin
  if ImageSize > 0 then
  begin
    if ImageSize < $FF00 then
      Freemem(Image, ImageSize)
    else
    begin
      GlobalUnlock(FHandle);
      GlobalFree(FHandle);
    end;
    ImageSize := 0;
  end;
  if InfoSize > 0 then
  begin
    FreeMem(Info, InfoSize);
    InfoSize := 0;
  end;
end;

procedure TDib.InitializeBitmapInfoHeader(Bitmap: HBITMAP);
var
  BM: {$ifdef LCL} LclType.Bitmap {$else} Windows.TBitmap {$endif};
  BitCount: Integer;

  function WidthBytes(I: Integer): Integer;
  begin
    Result := ((I + 31) div 32) * 4;
  end;

begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  BitCount := BM.bmBitsPixel * BM.bmPlanes;
  if BitCount > 8 then
    InfoSize := SizeOf(TBitmapInfoHeader)
  else
    InfoSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl BitCount);
  GetMem(Info, InfoSize);

  with Info^ do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := BM.bmWidth;
    biHeight := BM.bmHeight;
    biBitCount := BM.bmBitsPixel * BM.bmPlanes;
    biPlanes := 1;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
    biCompression := BI_RGB;
    if biBitCount in [16, 32] then
      biBitCount := 24;
    biSizeImage := WidthBytes(biWidth * biBitCount) * biHeight;
  end;
end;

procedure TDib.GetDIBX(DC: HDC; Bitmap: HBITMAP; Palette: HPALETTE);
var
  OldPal: HPALETTE;
  Rslt: Integer;
  bmInfo: PBitmapInfo;
begin
  OldPal := 0;
  if Palette <> 0 then
  begin
    OldPal := SelectPalette(DC, Palette, False);
    RealizePalette(DC);
  end;
  bmInfo := PBitmapInfo(Info);
  Rslt := GetDIBits(DC, Bitmap, 0, Info^.biHeight, Image, bmInfo^, DIB_RGB_COLORS);
  if OldPal <> 0 then
    SelectPalette(DC, OldPal, False);
  if Rslt = 0 then
  begin
    OutofMemoryError;
  end;
end;

procedure TDib.DrawDIB(DC: HDC; X, Y, W, H: Integer; ROP: DWord);
var
  bmInfo: PBitmapInfo;
begin
  bmInfo := PBitmapInfo(Info);
  with Info^ do
    StretchDIBits(DC, X, Y, W, H, 0, 0, biWidth, biHeight, Image,
      bmInfo^, DIB_RGB_COLORS, ROP);
end;

function TDib.CreateDIBmp: hBitmap;
var
  bmInfo: PBitmapInfo;
  DC: HDC;
  OldPal: HPalette;
begin
  bmInfo := PBitmapInfo(Info);
  DC := GetDC(0);
  OldPal := SelectPalette(DC, ThePalette, False);
  RealizePalette(DC);
  try
    Result := CreateDIBitmap(DC, bmInfo^.bmiHeader, CBM_INIT, Image,
      bmInfo^, DIB_RGB_COLORS);
  finally
    SelectPalette(DC, OldPal, False);
    ReleaseDC(0, DC);
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

procedure TIndentManager.GetClearY(out CL, CR: Integer);
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

{$IFNDEF NoMetafile}

procedure ThtMetaFile.Construct;
var
  Tmp: TBitmap;
  pe: TPaletteEntry;
  Color: TColor;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := Width;
      FBitmap.Height := Height;
      PatBlt(FBitmap.Canvas.Handle, 0, 0, Width, Height, Blackness);
      FBitmap.Canvas.Draw(0, 0, Self);

      Tmp := TBitmap.Create;
      try
        Tmp.Width := Width;
        Tmp.Height := Height;
        Tmp.PixelFormat := pf8Bit;
      {pick an odd color from the palette to represent the background color,
       one not likely in the metafile}
        GetPaletteEntries(Tmp.Palette, 115, 1, pe);
        Color := pe.peBlue shl 16 or pe.peGreen shl 8 or pe.peRed;
        Tmp.Canvas.Brush.Color := Color;
        Tmp.Canvas.FillRect(Rect(0, 0, Width, Height));
        Tmp.Canvas.Draw(0, 0, Self);

        FMask := GetImageMask(Tmp, False, Color);
      finally
        Tmp.Free;
      end;
    except
      FreeAndNil(FBitmap);
    end;
  end;
end;

function ThtMetaFile.GetBitmap: TBitmap;
begin
  Construct;
  Result := FBitmap;
end;

function ThtMetaFile.GetMask: TBitmap;
begin
  Construct;
  Result := FMask;
end;

function ThtMetaFile.GetWhiteBGBitmap: TBitmap;
begin
  if not Assigned(FWhiteBGBitmap) then
  begin
    FWhiteBGBitmap := TBitmap.Create;
    try
      FWhiteBGBitmap.Width := Width;
      FWhiteBGBitmap.Height := Height;
      PatBlt(FWhiteBGBitmap.Canvas.Handle, 0, 0, Width, Height, Whiteness);
      FWhiteBGBitmap.Canvas.Draw(0, 0, Self);
    except
      FreeAndNil(FWhiteBGBitmap);
    end;
  end;
  Result := FWhiteBGBitmap;
end;

destructor ThtMetaFile.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FMask);
  FreeAndNil(FWhiteBGBitmap);
  inherited;
end;
{$ENDIF}

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

procedure TCharCollection.Add(C: AnsiChar; Index: Integer);
begin
  Add(WideChar(C), Index);
end;

procedure TCharCollection.Add(C: WideChar; Index: Integer);
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
  FChars := '';
end;

procedure TCharCollection.Concat(T: TCharCollection);
var
  K: Integer;
begin
  K := FCurrentIndex + T.FCurrentIndex;
  if K >= Length(FChars) then
  begin
    SetLength(FChars, K + 50);
    ReallocMem(FIndices, (K + 50) * Sizeof(Integer));
  end;
  Move(PhtChar(T.FChars)^, FChars[FCurrentIndex + 1], T.FCurrentIndex * SizeOf(ThtChar)); //@@@ Tiburon: todo test
  Move(T.FIndices^[1], FIndices^[FCurrentIndex + 1], T.FCurrentIndex * Sizeof(Integer));
  FCurrentIndex := K;
end;

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

function WideStringToMultibyte(CodePage: Integer; W: WideString): Ansistring;
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

function ByteNum(CodePage: Integer; P: PAnsiChar): Integer;
var
  P1: PAnsiChar;
begin
  if CodePage <> CP_UTF8 then
  begin
    P1 := {$ifdef LCL} CharNextEx {$else} CharNextExA {$endif} (CodePage, P, 0);
    if Assigned(P1) then
      Result := P1 - P
    else
      Result := 0;
  end
  else
    case ord(P^) of {UTF-8}
      0: Result := 0;
      1..127: Result := 1;
      192..223: Result := 2;
      224..239: Result := 3;
      240..247: Result := 4;
    else
      Result := 1; {error}
    end;
end;

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

procedure TokenObj.Concat(T: TokenObj);
var
  K: Integer;
begin
  K := Count + T.Count;
  if K >= Capacity then
    SetCapacity(K + 50);
  Move(T.C^, C^[Count + 1], T.Count * Sizeof(WideChar));
  Move(T.I^, I^[Count + 1], T.Count * Sizeof(Integer));
  FCount := K;
  StringOK := False;
end;

procedure TokenObj.Remove(N: Integer);
begin {remove a single character}
  if N <= Count then
  begin
    Move(C^[N + 1], C^[N], (Count - N) * Sizeof(WideChar));
    Move(I^[N + 1], I^[N], (Count - N) * Sizeof(Integer));
    if StringOK then
      Delete(St, N, 1);
    Dec(FCount);
  end;
end;

procedure TokenObj.Replace(N: Integer; Ch: WideChar);
begin {replace a single character}
  if N <= Count then
  begin
    C^[N] := Ch;
    if StringOK then
      St[N] := Ch;
  end;
end;

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

function TokenObj.GetString: WideString;
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

{----------------BitmapToRegion}

function BitmapToRegion(ABmp: TBitmap; XForm: PXForm; TransparentColor: TColor): HRGN;
{Find a Region corresponding to the non-transparent area of a bitmap.

 Thanks to Felipe Machado.  See http://www.delphi3000.com/
 Minor modifications made.}
const
  AllocUnit = 100;
type
  PRectArray = ^TRectArray;
  TRectArray = array[0..(MaxInt div SizeOf(TRect)) - 1] of TRect;
var
  pr: PRectArray; // used to access the rects array of RgnData by index
  h: HRGN; // Handles to regions
  RgnData: PRgnData; // Pointer to structure RGNDATA used to create regions
  lr, lg, lb: Byte; // values for lowest and hightest trans. colors
  x, y, x0: Integer; // coordinates of current rect of visible pixels
  maxRects: Cardinal; // Number of rects to realloc memory by chunks of AllocUnit
{$ifdef LCL}
  bmp: TLazIntfImage;
  b: TFpColor;
{$else}
  b: PByteArray; // used to easy the task of testing the byte pixels (R,G,B)
  ScanLinePtr: Pointer; // Pointer to current ScanLine being scanned
  ScanLineInc: Integer; // Offset to next bitmap scanline (can be negative)
  bmp: TBitmap;
{$endif}
begin
  Result := 0;
  lr := GetRValue(TransparentColor);
  lg := GetGValue(TransparentColor);
  lb := GetBValue(TransparentColor);
  { ensures that the pixel format is 32-bits per pixel }
{$ifdef LCL}
  bmp := TLazIntfImage.Create(0,0);
  try
    bmp.Assign(ABmp);
    { alloc initial region data }
    maxRects := AllocUnit;
    GetMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects));
    FillChar(RgnData^, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), 0);
    try
      with RgnData^.rdh do
      begin
        dwSize := SizeOf(TRgnDataHeader);
        iType := RDH_RECTANGLES;
        nCount := 0;
        nRgnSize := 0;
        SetRect(rcBound, MAXLONG, MAXLONG, 0, 0);
      end;
      { scan each bitmap row - the orientation doesn't matter (Bottom-up or not) }
      for y := 0 to bmp.Height - 1 do
      begin
        x := 0;
        while x < bmp.Width do
        begin
          x0 := x;
          while x < bmp.Width do
          begin
            b := bmp[x,y];
            if (b.red = lr) and (b.green = lg) and (b.blue = lb) then
              Break; // pixel is transparent
            Inc(x);
          end;
          { test to see if we have a non-transparent area in the image }
          if x > x0 then
          begin
            { increase RgnData by AllocUnit rects if we exceeds maxRects }
            if RgnData^.rdh.nCount >= maxRects then
            begin
              Inc(maxRects, AllocUnit);
              ReallocMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
              pr := @RgnData^.Buffer;
              FillChar(pr^[maxRects - AllocUnit], AllocUnit * SizeOf(TRect), 0);
            end;
            { Add the rect (x0, y)-(x, y+1) as a new visible area in the region }
            pr := @RgnData^.Buffer; // Buffer is an array of rects
            with RgnData^.rdh do
            begin
              SetRect(pr[nCount], x0, y, x, y + 1);
              { adjust the bound rectangle of the region if we are "out-of-bounds" }
              if x0 < rcBound.Left then
                rcBound.Left := x0;
              if y < rcBound.Top then
                rcBound.Top := y;
              if x > rcBound.Right then
                rcBound.Right := x;
              if y + 1 > rcBound.Bottom then
                rcBound.Bottom := y + 1;
              Inc(nCount);
            end;
          end; // if x > x0
          { Need to create the region by muliple calls to ExtCreateRegion, 'cause }
          { it will fail on Windows 98 if the number of rectangles is too large   }
          if RgnData^.rdh.nCount = 2000 then
          begin
            h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), RgnData^);
            if Result > 0 then
            begin // Expand the current region
              CombineRgn(Result, Result, h, RGN_OR);
              DeleteObject(h);
            end
            else // First region, assign it to Result
              Result := h;
            RgnData^.rdh.nCount := 0;
            SetRect(RgnData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
          end;
          Inc(x);
        end; // scan every sample byte of the image
      end;
      { need to call ExCreateRegion one more time because we could have left    }
      { a RgnData with less than 2000 rects, so it wasn't yet created/combined  }
      if RgnData^.rdh.nCount > 0 then {LDB  0 Count causes exception and abort in Win98}
        h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects), RgnData^)
      else
        h := 0;
      if Result > 0 then
      begin
        CombineRgn(Result, Result, h, RGN_OR);
        DeleteObject(h);
      end
      else
        Result := h;
    finally
      FreeMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
    end;
  finally
    bmp.Free;
  end;
{$else}
  bmp := TBitmap.Create;
  try
    bmp.Assign(ABmp);
    bmp.PixelFormat := pf32bit;
    { alloc initial region data }
    maxRects := AllocUnit;
    GetMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects));
    FillChar(RgnData^, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), 0);
    try
      with RgnData^.rdh do
      begin
        dwSize := SizeOf(TRgnDataHeader);
        iType := RDH_RECTANGLES;
        nCount := 0;
        nRgnSize := 0;
        SetRect(rcBound, MAXLONG, MAXLONG, 0, 0);
      end;
      { scan each bitmap row - the orientation doesn't matter (Bottom-up or not) }
      ScanLinePtr := bmp.ScanLine[0];
      if bmp.Height > 1 then
        ScanLineInc := PtrSub(bmp.ScanLine[1], ScanLinePtr)
      else
        ScanLineInc := 0;
      for y := 0 to bmp.Height - 1 do
      begin
        x := 0;
        while x < bmp.Width do
        begin
          x0 := x;
          while x < bmp.Width do
          begin
            b := @PByteArray(ScanLinePtr)[x * SizeOf(TRGBQuad)];
            // BGR-RGB: Windows 32bpp BMPs are made of BGRa quads (not RGBa)
            if (b[2] = lr) and (b[1] = lg) and (b[0] = lb) then
              Break; // pixel is transparent
            Inc(x);
          end;
          { test to see if we have a non-transparent area in the image }
          if x > x0 then
          begin
            { increase RgnData by AllocUnit rects if we exceeds maxRects }
            if RgnData^.rdh.nCount >= maxRects then
            begin
              Inc(maxRects, AllocUnit);
              ReallocMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
              pr := @RgnData^.Buffer;
              FillChar(pr^[maxRects - AllocUnit], AllocUnit * SizeOf(TRect), 0);
            end;
            { Add the rect (x0, y)-(x, y+1) as a new visible area in the region }
            pr := @RgnData^.Buffer; // Buffer is an array of rects
            with RgnData^.rdh do
            begin
              SetRect(pr[nCount], x0, y, x, y + 1);
              { adjust the bound rectangle of the region if we are "out-of-bounds" }
              if x0 < rcBound.Left then
                rcBound.Left := x0;
              if y < rcBound.Top then
                rcBound.Top := y;
              if x > rcBound.Right then
                rcBound.Right := x;
              if y + 1 > rcBound.Bottom then
                rcBound.Bottom := y + 1;
              Inc(nCount);
            end;
          end; // if x > x0
          { Need to create the region by muliple calls to ExtCreateRegion, 'cause }
          { it will fail on Windows 98 if the number of rectangles is too large   }
          if RgnData^.rdh.nCount = 2000 then
          begin
            h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), RgnData^);
            if Result > 0 then
            begin // Expand the current region
              CombineRgn(Result, Result, h, RGN_OR);
              DeleteObject(h);
            end
            else // First region, assign it to Result
              Result := h;
            RgnData^.rdh.nCount := 0;
            SetRect(RgnData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
          end;
          Inc(x);
        end; // scan every sample byte of the image
        PtrInc(ScanLinePtr, ScanLineInc);
      end;
      { need to call ExCreateRegion one more time because we could have left    }
      { a RgnData with less than 2000 rects, so it wasn't yet created/combined  }
      if RgnData^.rdh.nCount > 0 then {LDB  0 Count causes exception and abort in Win98}
        h := ExtCreateRegion(XForm, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects), RgnData^)
      else
        h := 0;
      if Result > 0 then
      begin
        CombineRgn(Result, Result, h, RGN_OR);
        DeleteObject(h);
      end
      else
        Result := h;
    finally
      FreeMem(RgnData, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * MaxRects));
    end;
  finally
    bmp.Free;
  end;
{$endif}
end;

{----------------EnlargeImage}

function EnlargeImage(Image: TGpObject; W, H: Integer): TBitmap;
{enlarge 1 pixel images for tiling.  Returns a TBitmap regardless of Image type}
var
  NewBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  {$IFNDEF NoGDIPlus}
  if Image is TGpImage then
    NewBitmap := TGpImage(Image).GetBitmap
  else {$ENDIF !NoGDIPlus}
    NewBitmap := Image as TBitmap;
  Result.Assign(NewBitmap);
  if NewBitmap.Width = 1 then
    Result.Width := Min(100, W)
  else
    Result.Width := NewBitmap.Width;
  if NewBitmap.Height = 1 then
    Result.Height := Min(100, H)
  else
    Result.Height := NewBitmap.Height;
  Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), NewBitmap);
  {$IFNDEF NoGDIPlus}
  if Image is TGpImage then
    NewBitmap.Free;
  {$ENDIF NoGDIPlus}
end;

{----------------PrintBitmap}

type
  AllocRec = class(TObject)
    Ptr: Pointer;
    ASize: Integer;
    AHandle: THandle;
  end;

procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
{Y relative to top of display here}
{$ifdef LCL}
begin
  Canvas.StretchDraw(Rect(X, Y, X + W, Y + H), Bitmap);
{$else}
  function Allocate(Size: Integer): AllocRec;
  begin
    Result := AllocRec.Create;
    with Result do
    begin
      ASize := Size;
      if Size < $FF00 then
        GetMem(Ptr, Size)
      else
      begin
        AHandle := GlobalAlloc(HeapAllocFlags, Size);
        if AHandle = 0 then
          ABort;
        Ptr := GlobalLock(AHandle);
      end;
    end;
  end;

  procedure DeAllocate(AR: AllocRec);
  begin
    with AR do
      if ASize < $FF00 then
        Freemem(Ptr, ASize)
      else
      begin
        GlobalUnlock(AHandle);
        GlobalFree(AHandle);
      end;
    AR.Free;
  end;

var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: AllocRec;
  ImageSize: DWord;
  InfoSize: DWord;
begin
  if (Bitmap = nil) or (Bitmap.Handle = 0) then
    Exit;
  DC := Canvas.Handle;
  try
    GetDIBSizes(Bitmap.Handle, InfoSize, ImageSize);
    GetMem(Info, InfoSize);
    try
      Image := Allocate(ImageSize);
      OldPal := SelectPalette(DC, ThePalette, False);
      try
        GetDIB(Bitmap.Handle, ThePalette, Info^, Image.Ptr^);
        RealizePalette(DC);
        with Info^.bmiHeader do
          StretchDIBits(DC, X, Y, W, H, 0, 0, biWidth, biHeight, Image.Ptr, Info^, DIB_RGB_COLORS, SRCCOPY);
      finally
        DeAllocate(Image);
        SelectPalette(DC, OldPal, False);
      end;
    finally
      FreeMem(Info, InfoSize);
    end;
  except
  end;
{$endif}
end;

{----------------PrintTransparentBitmap3}

procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: Integer;
  Bitmap, Mask: TBitmap; YI, HI: Integer);
{Y relative to top of display here}
{This routine prints transparently on complex background by printing through a clip region}
{X, Y are point where upper left corner will be printed.
 NewW, NewH are the Width and Height of the output (possibly stretched)
 Vertically only a portion of the Bitmap, Mask may be printed starting at
   Y=YI in the bitmap and a height of HI
}
var
  DC: HDC;
  hRgn, OldRgn: THandle;
  Rslt: Integer;
  XForm: TXForm;
  SizeV, SizeW: TSize;
  HF, VF: double;
  ABitmap, AMask: TBitmap;
  BitmapCopy: boolean;
  Origin: TPoint; //BG, 29.08.2009: window origin for correct mask translation
begin
{the following converts the black masked area in the image to white.  This may look
 better in WPTools which currently doesn't handle the masking}
  if (Bitmap.Handle = 0) or (HI <= 0) or (Bitmap.Width <= 0) then
    Exit;
  BitmapCopy := Bitmap.Height <> HI;
  try
    if BitmapCopy then
    begin
      ABitmap := TBitmap.Create;
      AMask := TBitmap.Create;
    end
    else
    begin
      ABitmap := Bitmap;
      AMask := Mask;
    end;
    try
      if BitmapCopy then
      begin
        Abitmap.Assign(Bitmap);
        ABitmap.Height := HI;
        BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, Bitmap.Canvas.Handle, 0, YI, SrcCopy);
        AMask.Assign(Mask);
        AMask.Height := HI;
        BitBlt(AMask.Canvas.Handle, 0, 0, AMask.Width, HI, Mask.Canvas.Handle, 0, YI, SrcCopy);
      end;

      SetBkColor(ABitmap.Canvas.Handle, clWhite);
      SetTextColor(ABitmap.Canvas.Handle, clBlack);
      BitBlt(ABitmap.Canvas.Handle, 0, 0, Bitmap.Width, HI, AMask.Canvas.Handle, 0, 0, SRCPAINT);

      DC := Canvas.Handle;
    {calculate a transform for the clip region as it may be a different size than
     the mask and needs to be positioned on the canvas.}
      GetViewportExtEx(DC, SizeV);
      GetWindowExtEx(DC, SizeW);
      GetWindowOrgEx(DC, Origin); //BG, 29.08.2009: get origin for correct mask translation

      HF := (SizeV.cx / SizeW.cx); {Horizontal adjustment factor}
      VF := (SizeV.cy / SizeW.cy); {Vertical adjustment factor}

      XForm.eM11 := HF * (NewW / Bitmap.Width);
      XForm.eM12 := 0;
      XForm.eM21 := 0;
      XForm.eM22 := VF * (NewH / HI);
      XForm.edx := HF * (X - Origin.X); //BG, 29.08.2009: subtract origin
      XForm.edy := VF * Y;

    {Find the region for the white area of the Mask}
      hRgn := BitmapToRegion(AMask, @XForm, $FFFFFF);
      if hRgn <> 0 then {else nothing to output--this would be unusual}
      begin
        OldRgn := CreateRectRgn(0, 0, 1, 1); {a valid region is needed for the next call}
        Rslt := GetClipRgn(DC, OldRgn); {save the Old clip region}
        try
          if Rslt = 1 then
            CombineRgn(hRgn, hRgn, OldRgn, RGN_AND);
          SelectClipRgn(DC, hRgn);
          PrintBitmap(Canvas, X, Y, NewW, NewH, ABitmap);
        finally
          if Rslt = 1 then
            SelectClipRgn(DC, OldRgn)
          else
            SelectClipRgn(DC, 0);
          DeleteObject(hRgn);
          DeleteObject(OldRgn);
        end;
      end;
    finally
      if BitmapCopy then
      begin
        ABitmap.Free;
        AMask.Free;
      end;
    end;
  except
  end;
end;

type
  BorderPointArray = array[0..3] of TPoint;

{$IFNDEF NoGDIPlus}

procedure DrawGpImage(Handle: THandle; Image: TGpImage; DestX, DestY: Integer);
{Draws the entire image as specified at the point specified}
var
  g: TGpGraphics;
begin
  g := TGPGraphics.Create(Handle);
  try
    g.DrawImage(Image, DestX, DestY, Image.Width, Image.Height);
  except
  end;
  g.Free;
end;

procedure DrawGpImage(Handle: THandle; Image: TGpImage; DestX, DestY,
  SrcX, SrcY, SrcW, SrcH: Integer);
{Draw a portion of the image at DestX, DestY.  No stretching}
var
  g: TGpGraphics;
begin
  g := TGPGraphics.Create(Handle);
  try
    g.DrawImage(Image, DestX, DestY, SrcX, SrcY, SrcW, SrcH);
  except
  end;
  g.Free;
end;

procedure StretchDrawGpImage(Handle: THandle; Image: TGpImage; DestX, DestY,
  DestW, DestH: Integer);
{Draws the entire image in the rectangle specified}
var
  g: TGpGraphics;
begin
  g := TGPGraphics.Create(Handle);
  try
    g.DrawImage(Image, DestX, DestY, DestW, DestH);
  except
  end;
  g.Free;
end;

procedure StretchPrintGpImageDirect(Handle: THandle; Image: TGpImage;
  DestX, DestY, DestW, DestH: Integer;
  ScaleX, ScaleY: single);
{Prints the entire image at the point specified with the height and width specified}
var
  g: TGpGraphics;
begin
  g := TGPGraphics.Create(Handle);
  try
    g.ScaleTransform(ScaleX, ScaleY);
    g.DrawImage(Image, DestX, DestY, DestW, DestH);
  except
  end;
  g.Free;
end;

procedure StretchPrintGpImageOnColor(Canvas: TCanvas; Image: TGpImage;
  DestX, DestY, DestW, DestH: Integer; Color: TColor = clWhite);
var
  g: TGpGraphics;
  bg: TBitmap;
begin {Draw image on white background first, then print}
  bg := TBitmap.Create;
  bg.Width := TGPImage(Image).Width;
  bg.Height := TGPImage(Image).Height;
  bg.Canvas.Brush.Color := Color;
  bg.Canvas.FillRect(Rect(0, 0, bg.Width, bg.Height));
  g := TGPGraphics.Create(bg.Canvas.Handle);
  g.DrawImage(TGPImage(Image), 0, 0, bg.Width, bg.Height);
  g.Free;
  Canvas.StretchDraw(Rect(DestX, DestY, DestX + DestW, DestY + DestH), bg);
  bg.Free;
end;

procedure PrintGpImageDirect(Handle: THandle; Image: TGpImage; DestX, DestY: Integer;
  ScaleX, ScaleY: single);
{Prints the entire image as specified at the point specified}
var
  g: TGpGraphics;
begin
  g := TGPGraphics.Create(Handle);
  try
    g.ScaleTransform(ScaleX, ScaleY);
    g.DrawImage(Image, DestX, DestY, Image.Width, Image.Height);
  except
  end;
  g.Free;
end;

{$ENDIF NoGDIPlus}

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
        Brush.Color := ThemedColor(Color);
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
      Brush.Color := ThemedColor(BGround) or PalRelative;
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
          lb.lbColor := ThemedColor(C[I]) or PalRelative;
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

{ TgpObject }

function GetImageHeight(Image: TGpObject): Integer;
begin
  if Image is TBitmap then
    Result := TBitmap(Image).Height
  else {$IFNDEF NoGDIPlus}
  if Image is TGpImage then
    Result := TGpImage(Image).Height
  else {$ENDIF !NoGDIPlus}
  if Image is TGifImage then
    Result := TGifImage(Image).Height
{$IFNDEF NoMetafile}
  else if Image is ThtMetaFile then
    Result := ThtMetaFile(Image).Height
{$ENDIF}
  else
    raise(EGDIPlus.Create('Not a TBitmap, TGifImage, TMetafile, or TGpImage'));
end;

function GetImageWidth(Image: TGpObject): Integer;
begin
  if Image is TBitmap then
    Result := TBitmap(Image).Width
  else {$IFNDEF NoGDIPlus}
  if Image is TGpImage then
    Result := TGpImage(Image).Width
  else {$ENDIF !NoGDIPlus}
  if Image is TGifImage then
    Result := TGifImage(Image).Width
{$IFNDEF NoMetafile}
  else if Image is ThtMetaFile then
    Result := ThtMetaFile(Image).Width
{$ENDIF}
  else
    raise(EGDIPlus.Create('Not a TBitmap, TGifImage, TMetafile, or TGpImage'));
end;

{ TViewerBase }

//-- BG ---------------------------------------------------------- 05.01.2010 --
constructor TViewerBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuirksMode := qmDetect;
end;

procedure TViewerBase.SetOnInclude(Handler: TIncludeType);
begin
  FOnInclude := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnLink(Handler: TLinkType);
begin
  FOnLink := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnScript(Handler: TScriptEvent);
begin
  FOnScript := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TViewerBase.SetOnSoundRequest(Handler: TSoundType);
begin
  FOnSoundRequest := Handler;
end;

procedure TViewerBase.SetQuirksMode(const AValue: THtQuirksMode);
begin
  FQuirksMode := AValue;
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

{$ifdef LCL}
const
  DefaultBitmap = 'DefaultBitmap';
  ErrBitmap = 'ErrBitmap';
  ErrBitmapMask = 'ErrBitmapMask';
//  Hand_Cursor = 'Hand_Cursor';

procedure htLoadBitmap(var Bitmap: TBitmap; const Resource: String);
begin
  Bitmap.LoadFromLazarusResource(Resource);
end;

function htLoadCursor(const CursorName: String): HICON;
begin
  Result := LoadCursorFromLazarusResource(CursorName);
end;

{$else}
const
  DefaultBitmap = 1002;
  ErrBitmap = 1001;
  ErrBitmapMask = 1005;

procedure htLoadBitmap(var Bitmap: TBitmap; Resource: Integer);
begin
  BitMap.Handle := LoadBitmap(HInstance, MakeIntResource(Resource));
end;

function htLoadCursor(const CursorName: PChar): HICON;
begin
  Result := LoadCursor(HInstance, CursorName);
end;

{$endif}

{ TIDObject }

//-- BG ---------------------------------------------------------- 06.03.2011 --
function TIDObject.FreeMe: Boolean;
begin
  Result := False;
end;

initialization
  DefBitMap := TBitmap.Create;
  ErrorBitMap := TBitmap.Create;
  ErrorBitMapMask := TBitmap.Create;
{$ifdef LCL}
  {$I htmlun2.lrs}
{$else}
  {$R Html32.res}
{$endif}
  htLoadBitmap(DefBitMap, DefaultBitmap);
  htLoadBitmap(ErrorBitMap, ErrBitmap);
  htLoadBitmap(ErrorBitMapMask, ErrBitmapMask);
  Screen.Cursors[UpDownCursor] := htLoadCursor('UPDOWNCURSOR');
  Screen.Cursors[UpOnlyCursor] := htLoadCursor('UPONLYCURSOR');
  Screen.Cursors[DownOnlyCursor] := htLoadCursor('DOWNONLYCURSOR');
  WaitStream := TMemoryStream.Create;
  ErrorStream := TMemoryStream.Create;

finalization
  DefBitMap.Free;
  ErrorBitMap.Free;
  ErrorBitMapMask.Free;
  WaitStream.Free;
  ErrorStream.Free;
end.

