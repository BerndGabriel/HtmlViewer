{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2023 by HtmlViewer Team

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
{$ifdef UseInline}
  Math,
{$endif}
{$ifdef LCL}
  LclIntf, IntfGraphics, FpImage, LclType, LResources, LMessages, HtmlMisc,
{$else}
  Windows,
{$endif}
  SysUtils, Contnrs, Classes, Graphics, ClipBrd, Controls, ExtCtrls, Messages,
  Variants, Types, ComCtrls, Forms,
{$ifdef Compiler20_Plus}
  CommCtrl,
{$endif}
{$ifndef NoGDIPlus}
  GDIPL2A,
{$endif}
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
{$ifdef METAFILEMISSING}
  MetaFilePrinter,
{$endif}
  HtmlBuffer,
  HTMLGif2,
  HtmlGlobals,
  HtmlImages,
  StyleTypes,
  HtmlSymb;

const
  VersionNo = '11.10';
  MaxHScroll = 100000; {max horizontal display in pixels}
  Tokenleng = 300;
  TopLim = -200; {drawing limits}
  BotLim = 5000;
  FmCtl = WideChar(#2);
  ImgPan = WideChar(#4);
  BrkCh = WideChar(#8);
  htDefFontName = 'Serif';
  htDefPreFontName = 'Monospace';


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

  ThtJustify = (NoJustify, Left, Centered, Right, FullJustify);
  TRowType = (THead, TBody, TFoot);

//------------------------------------------------------------------------------
// tag attributes
//------------------------------------------------------------------------------

  TAttribute = class {holds a tag attribute}
  public
    Which: TAttrSymb; {symbol of attribute such as HrefSy}
    WhichName: ThtString;
    Value: Integer; {numeric value if appropriate}
    DblValue: Double; {numeric value if appropriate}
    Name: ThtString; {ThtString (mixed case), value after '=' sign}
    CodePage: Integer;
    constructor Create(ASym: TAttrSymb; const AValue: Double; const NameStr, ValueStr: ThtString; ACodePage: Integer);
    constructor CreateCopy(ASource: TAttribute);
    property AsString: ThtString read Name;
    property AsInteger: Integer read Value;
    property AsDouble: Double read DblValue;
  end;

{$ifdef UseGenerics}
  TAttributeList = class(TObjectList<TAttribute>) {a list of tag attributes}
{$else}
  TAttributeList = class(TObjectList) {a list of tag attributes,(TAttributes)}
{$endif}
  private
    SaveID: ThtString;
    function GetClass: ThtString;
    function GetID: ThtString;
    function GetTitle: ThtString;
{$ifdef UseGenerics}
{$else}
    function GetAttribute(Index: Integer): TAttribute; {$ifdef UseInline} inline; {$endif}
{$endif}
  public
    constructor CreateCopy(ASource: TAttributeList);
    function Clone: TAttributeList; virtual;
{$ifdef UseGenerics}
    procedure Clear; virtual;
{$else}
    procedure Clear; override;
{$endif}
    function Find(const Name: ThtString; var T: TAttribute): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    function Find(Sy: TAttrSymb; var T: TAttribute): Boolean; overload; {$ifdef UseInline} inline; {$endif}
    function CreateStringList: ThtStringList;
    property TheClass: ThtString read GetClass;
    property TheID: ThtString read GetID;
    property TheTitle: ThtString read GetTitle;
{$ifdef UseGenerics}
{$else}
    property Items[Index: Integer]: TAttribute read GetAttribute; default;
{$endif}
  end;

//------------------------------------------------------------------------------
// copy to clipboard support
//------------------------------------------------------------------------------

  TSelTextCount = class
  private
    Buffer: PWideChar;
    BufferLeng: Integer;
    Leng: Integer;
  public
    procedure AddText(P: PWideChar; Size: Integer); virtual;
    procedure AddTextCR(P: PWideChar; Size: Integer); {$ifdef UseInline} inline; {$endif}
    function Terminate: Integer; virtual;
  end;

  TSelTextBuf = class(TSelTextCount)
  public
    constructor Create(ABuffer: PWideChar; Size: Integer);
    procedure AddText(P: PWideChar; Size: Integer); override;
    function Terminate: Integer; override;
  end;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

  TFontObjBaseList = class;

  {holds start and end point of URL text}
  TutText = record //BG, 03.03.2011: changed to record. no need to use a class
    Start: Integer;
    Last: Integer;
  end;

  TUrlTarget = class
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
    procedure SetLast(List: TFontObjBaseList; ALast: Integer);
    property Start: Integer read utText.Start;
    property Last: Integer read utText.Last;
  end;

  // BG, 31.12.2011:
  TMapArea = class
  private
    FHRef: ThtString;
    FRegion: HRGN;
    FTarget: ThtString;
    FTitle: ThtString;
  public
    destructor Destroy; override;
    function PtInArea(X, Y: Integer): Boolean;
    property HRef: ThtString read FHRef;
    property Target: ThtString read FTarget;
    property Title: ThtString read FTitle;
  end;

  // BG, 31.12.2011:
{$ifdef UseGenerics}
  TMapAreaList = class(TObjectList<TMapArea>);
{$else}
  TMapAreaList = class(TObjectList)
  private
    function GetArea(Index: Integer): TMapArea; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TMapArea read GetArea; default;
  end;
{$endif}

  TMapItem = class {holds a client map info}
  private
    FAreas: TMapAreaList;
  public
    MapName: ThtString;
    constructor Create;
    destructor Destroy; override;
    function GetURL(X, Y: Integer; var URLTarg: TURLTarget; var ATitle: ThtString): boolean;
    procedure AddArea(Attrib: TAttributeList);
  end;

{$ifdef UseGenerics}
  ThtMap = class(TObjectList<TMapItem>);
{$else}
  ThtMap = class(TObjectList)
  private
    function Get(Index: Integer): TMapItem; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TMapItem read Get; default;
  end;
{$endif}

  TFontObjBase = class {font information}
  public
    UrlTarget: TUrlTarget;
  end;

{$ifdef UseGenerics}
  TFontObjBaseList = class(TObjectList<TFontObjBase>);
{$else}
  TFontObjBaseList = class(TObjectList)
  private
    function GetBase(Index: Integer): TFontObjBase; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TFontObjBase read GetBase; default;
  end;
{$endif}

//------------------------------------------------------------------------------
// indentation manager
//------------------------------------------------------------------------------

  TIndentRec = class
  public
    X: Integer;   // left or right indentation relative to LfEdge.
    YT: Integer;  // top Y inclusive coordinate for this record relative to document top.
    YB: Integer;  // bottom Y exclusive coordinate for this record relative to document top.
    ID: TObject;  // block level indicator for this record, 0 for not applicable
  end;

  TIndentRecList = class(TObjectList)
  private
    function Get(Index: Integer): TIndentRec; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TIndentRec read Get; default;
  end;

  TIndentManager = class
  private
    function LeftEdge(Y: Integer): Integer;
    function RightEdge(Y: Integer): Integer;
  public
    LfEdge: Integer;    // left edge of the block content area.
                        // TCell.DoLogic calculates with LfEdge = 0.
                        // TCell.Draw then may shift the block by setting LfEdge to X.
    Width: Integer;     // width of the block content area.
    ClipWidth: Integer; // clip width ???
    L: TIndentRecList;  // list of left side indentations of type IndentRec.
    R: TIndentRecList;  // list of right side indentations of type IndentRec.
    CurrentID: TObject; // the current block level (a TBlock pointer)
    LTopMin: Integer;
    RTopMin: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddLeft(YT, YB, W: Integer): TIndentRec;
    function AddRight(YT, YB, W: Integer): TIndentRec;
    function AlignLeft(var Y: Integer; W: Integer): Integer;
    function AlignRight(var Y: Integer; W: Integer): Integer;
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
    // AdjustY() is called after an inline row has been produced. If floating objects have been moved
    // down before the actual height of the entire row was computed, their Y coordinates aren't too
    // small now. AdjustY() moves them down below given Y + Height.
    procedure AdjustY(FirstLeftIndex, FirstRightIndex, Y, Height: Integer);
  end;

//------------------------------------------------------------------------------
// parser
//------------------------------------------------------------------------------

  {Simplified variant of TokenObj, to temporarily keep a ThtString of unicode
   characters along with their original indices.}

  { TCharCollection }

  TCharCollection = class
  private
    FChars: ThtString;
    FIndices: array of Integer;
    FCurrentIndex: Integer;
    function GetSize: Integer;
    function GetAsString: ThtString;
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
  public
    constructor Create;
    procedure Add(C: ThtChar; Index: Integer); overload;
    procedure Add(const S: ThtString; Index: Integer); overload;
    procedure Clear;
//    procedure Concat(T: TCharCollection);

    property AsString: ThtString read GetAsString;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Size: Integer read GetSize;
  end;

  { TokenObj }

  TTokenObj = class
  private
    St: UnicodeString;
    StringOK: boolean;
    FCount: Integer;
    function GetCapacity: Integer;
    function GetString: UnicodeString;
    procedure SetCapacity(NewCapacity: Integer);
  public
    C: array of ThtChar;
    I: array of Integer;
    constructor Create;
    procedure AddUnicodeChar(Ch: WideChar; Ind: Integer);
    procedure AddString(S: TCharCollection);
    procedure Clear;
//    procedure Concat(T: TokenObj);
//    procedure Remove(N: Integer);
//    procedure Replace(N: Integer; Ch: ThtChar);

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
    property S: UnicodeString read GetString;
  end;

//------------------------------------------------------------------------------
// TIDObject is base class for all tag objects.
//------------------------------------------------------------------------------
// If they have an ID, the parser puts them into the HtmlViewer's IDNameList,
// a TIDObjectList, where they can be obtained from by ID.
// Their Y coordinates can be retrieved and HtmlViewer can scroll to them.
//
// Most descendants are objects representing an HTML tag, except TChPosObj.
//------------------------------------------------------------------------------

  TIDObject = class
  private
    function GetId(): ThtString; //>-- DZ
  protected
    FHtmlId: ThtString; //>-- DZ real ID from HTML if any
    FGlobalId: ThtString; //>-- DZ global unique ID

    function GetYPosition: Integer; virtual; abstract;
    function FreeMe: Boolean; virtual; // some objects the TIDObjectsList owns, some others not.
  public
    constructor Create(const AHtmlID: ThtString);
    procedure AfterConstruction(); override;//>-- DZ

    property YPosition: Integer read GetYPosition;
    property Id: ThtString read GetId; //>-- DZ if FhtmlId then FglobalId will be returned as result
    property HtmlId: ThtString read FHtmlId; //>-- DZ
    property GlobalId: ThtString read FGlobalId; //>-- DZ
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

  ThtColorArray = packed array[0..3] of TColor;
  ThtBorderStyleArray = packed array[0..3] of ThtBorderStyle;

//BG, 11.09.2010: moved to this unit to reduce circular dependencies:

  ThtguResultType = set of (guUrl, guControl, guTitle);

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

{$ifndef Compiler20_Plus}
const
  PBS_MARQUEE  = $08;
  PBM_SETBARCOLOR = WM_USER+9;
  PBM_SETMARQUEE = WM_USER+10;
  PBM_GETBARCOLOR = WM_USER+14;

type
  TProgressBarStyle = (pbstNormal, pbstMarquee);

  ThvProgressBar = class(TProgressBar)
  private
    FStyle: TProgressBarStyle;
    procedure SetStyle(Value: TProgressBarStyle);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Style: TProgressBarStyle read FStyle write SetStyle;
  end;
{$else}
  ThvProgressBar = TProgressBar;
{$endif}

  // While the meter is not a progressbar, it is the closest control to what is required.
  // There are a few downsides to the Windows ProgressBar:
  //  - it animates to the value instead of jumping there. This is fixed by overriding the Position property.
  //  - Under Theming the progress bar animates a glowing swish that cannot be turned off.
  // This class also allows the user to define a color for when the bar is < low and one for > high.
  // This matches other browsers implementations
  ThvMeter = class(ThvProgressBar)
  private
    FLow: double;
    FHigh: double;
    FOptimum: double;  // This is only for reference and has not affect on the color
    FLowColor: TColor;
    FHighColor: TColor;

    procedure SetPosition(Value: Integer);
    function GetPosition: Integer;
    procedure UpdateColor;
  protected
    procedure SetLow(const value: double); virtual;
    procedure SetHigh(const value: double); virtual;
    procedure SetOptimum(const value: double); virtual;
    procedure SetLowColor(const value: TColor); virtual;
    procedure SetHighColor(const value: TColor); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Position: Integer read GetPosition write SetPosition default 0;

    property Low: double read FLow write SetLow;
    property High: double read FHigh write SetHigh;
    property Optimum: double read FOptimum write SetOptimum;

    property LowColor: TColor read FLowColor write SetLowColor;
    property HighColor: TColor read FHighColor write SetHighColor;
  end;



//------------------------------------------------------------------------------
// ThtControlBase is base class for TViewerBase and TFrameBase
//------------------------------------------------------------------------------

  ThtControlBase = class(TWinControl);

//------------------------------------------------------------------------------
// TViewerBase is base class for THtmlViewer and TFrameViewer
//------------------------------------------------------------------------------

  TGetStreamEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TStream) of object;
  TGottenStreamEvent = TGetStreamEvent;
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
  TGottenBitmapEvent = TGetBitmapEvent;
  TGetImageEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TStream) of object;
  TGottenImageEvent = TGetImageEvent;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList) of object;

  TViewerBase = class(ThtControlBase)
  private
    FBackGround, FHotSpotColor, FVisitedColor, FOverColor: TColor;
    FCharset: TFontCharset;
    FCodePage: TBuffCodePage;
    FFontColor: TColor;
    FFontName, FPreFontName: TFontName;
    FFontSize: Integer;
    FHistoryMaxCount, FImageCacheCount, FVisitedMaxCount: Integer;
    FLoadCursor: TCursor;
    FMarginWidth, FMarginHeight: Integer;
    FNoSelect: Boolean;
    FPrintMarginLeft, FPrintMarginRight, FPrintMarginTop, FPrintMarginBottom: Double;
    FPrintMaxHPages: Integer;
    FPrintScale: Double;
    FServerRoot: ThtString;
    //
    FOnBitmapRequest: TGetBitmapEvent;
    FOnBitmapRequested: TGottenBitmapEvent;
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
    function StoreFontName: Boolean;
    function StorePreFontName: Boolean;
    function GetPPI: Integer;
{$ifndef Compiler31_Plus}
    function FormPixelsPerInch: Integer;
{$endif}
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
  protected
    FInCMParentColorChanged: Integer;
{$ifndef Compiler31_Plus}
    FCurrentPPI: Integer;
    procedure SetPPI(Value: Integer); virtual;
    procedure SetParent(NewParent: TWinControl); override;
{$endif}
    procedure StyleChanged; virtual; abstract;
    procedure ScaleChanged; virtual; abstract;
    procedure ChangeScale(M, D: Integer{$ifdef Compiler31_Plus}; isDpiChange: Boolean{$endif}); override;
{$ifdef LCL}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
{$endif}
    {$ifdef has_StyleElements}
    procedure UpdateStyleElements; override;
    {$endif}
    function GetUseQuirksMode: Boolean; virtual; abstract;
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
    procedure SetLoadCursor(const Value: TCursor); virtual;
    procedure SetMarginHeight(const Value: Integer); virtual;
    procedure SetMarginWidth(const Value: Integer); virtual;
    procedure SetNoSelect(const Value: Boolean); virtual;
    procedure SetOnBitmapRequest(const Value: TGetBitmapEvent); virtual;
    procedure SetOnBitmapRequested(const Value: TGottenBitmapEvent); virtual;
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
    // HtmlExpandFilename(Filename, CurrentFilename): Try to get the absolute pathname of the given filename in the local filesystem
    function HtmlExpandFilename(const Filename: ThtString; const CurrentFilename: ThtString = ''): ThtString; virtual; abstract;
{$ifndef Compiler31_Plus}
    function ParentForm: TCustomForm;
{$endif}
    function ThemedColor(AColor: TColor; AStyleElement: ThtStyleElement): TColor;
    function ThemedColorToRGB(AColor: TColor; AStyleElement: ThtStyleElement): TColor;

    property QuirksMode : THtQuirksMode read FQuirksMode write SetQuirksMode default qmStandards;
    // set to determine if child objects should be in "quirks" mode
    property UseQuirksMode: Boolean read GetUseQuirksMode;
    property CodePage: Integer read FCodePage write SetCodePage default 0;
    property CharSet: TFontCharset read FCharSet write SetCharset default DEFAULT_CHARSET;
    property DefBackground: TColor read FBackGround write SetDefBackground default clBtnFace;
    property DefFontColor: TColor read FFontColor write SetFontColor default clBtnText;
    property DefFontName: TFontName read FFontName write SetFontName stored StoreFontName;
    property DefFontSize: Integer read FFontSize write SetFontSize default 12;
    property DefHotSpotColor: TColor read FHotSpotColor write SetHotSpotColor default clBlue;
    property DefOverLinkColor: TColor read FOverColor write SetActiveColor default clBlue;
    property DefPreFontName: TFontName read FPreFontName write SetPreFontName stored StorePreFontName;
    property DefVisitedLinkColor: TColor read FVisitedColor write SetVisitedColor default clPurple;
    property HistoryMaxCount: Integer read FHistoryMaxCount write SetHistoryMaxCount;
    property ImageCacheCount: Integer read FImageCacheCount write SetImageCacheCount default 5;
    property LoadCursor: TCursor read FLoadCursor write SetLoadCursor default crHourGlass;
    property MarginHeight: Integer read FMarginHeight write SetMarginHeight default 5;
    property MarginWidth: Integer read FMarginWidth write SetMarginWidth default 10;
    property NoSelect: Boolean read FNoSelect write SetNoSelect;
    property PixelsPerInch: Integer read GetPPI;
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
    property OnBitmapRequested: TGottenBitmapEvent read FOnBitmapRequested write SetOnBitmapRequested;
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
  published
    {$ifdef has_StyleElements}
    property StyleElements;
    {$endif}
  end;

  TViewerBaseClass = class of TViewerBase;

  TTablePartType = (Normal, DoHead, DoBody1, DoBody2, DoBody3, DoFoot);
  TTablePartRec = class
  public
    TablePart: TTablePartType;
    PartStart: Integer;
    PartHeight: Integer;
    FootHeight: Integer;
  end;

  THtmlViewerBase = class(TViewerBase)
  public
    TablePartRec: TTablePartRec;
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

//function WideStringToMultibyte(CodePage: Integer; W: UnicodeString): AnsiString;

function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; out Extent: TSize): Integer;
function GetTextExtent(DC: HDC; P: PWideChar; N: Integer): TSize;
procedure WrapTextW(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; S: UnicodeString);

//------------------------------------------------------------------------------
// misc. methods
//------------------------------------------------------------------------------

// BG, 26.12.2011: new type TSpecWidth
function SpecWidth(Value: Integer; VType: TWidthType): TSpecWidth;
function ToSpecWidth(AsInteger: Integer; AsString: ThtString): TSpecWidth;

//------------------------------------------------------------------------------
// canvas methods
//------------------------------------------------------------------------------

function CalcClipRect(Canvas: TCanvas; const Rect: TRect; Printing: boolean): TRect;
procedure GetClippingRgn(Canvas: TCanvas; const ARect: TRect; Printing: boolean; var Rgn, SaveRgn: HRgn);

procedure FillRectWhite(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
procedure DrawFormControlRect(Canvas: TCanvas; X1, Y1, X2, Y2: Integer;
  Raised, PrintMonoBlack, Disabled: Boolean; Color: TColor; ThemedColor: ThtThemedColor);
procedure DrawBorder(Canvas: TCanvas; ORect, IRect: TRect;
  const C: ThtColorArray; const S: ThtBorderStyleArray;
  BGround: TColor; Print: Boolean; ThemedColor: ThtThemedColor);

type
  THtBorderPointArray = array[0..3] of TPoint;

implementation

uses
  {$ifndef UseInline} Math, {$endif}
  {$ifdef UseVCLStyles}
  Vcl.Themes,
  {$endif}
  {$ifdef  HasSystemUITypes}
  System.UITypes,
  {$endif}
  {$ifndef FPC_TODO}jpeg, {$endif}
  {$IFDEF UNICODE} PngImage, {$ENDIF}
  DitherUnit, StylePars;

type
  EGDIPlus = class(Exception);

{$ifdef UseGlobalObjectId}
var
  GlobalObjectIdPrefix: ThtString;
  GlobalObjectIdCount: Cardinal; //>-- DZ, counter for TIDObject.FGlobalId
{$endif}

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
 {$ifdef UseInline} inline; {$endif}
begin
  Result := 0;
  if Str <> nil then
    while Str[Result] <> #0 do
      Inc(Result);
end;

function StrPosW(Str, SubStr: PWideChar): PWideChar;
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
{return count <= Max which fits in Width.  Return X, the extent of chars that fit}
var
  Ints: array of Integer;
  L, H, I: Integer;
begin
  Extent.cx := 0;
  Extent.cy := 0;
  Result := 0;
  if (Width <= 0) or (Max = 0) then
    Exit;

  if not IsWin32Platform then
  begin
    if Width < Max then
    begin
      // speed up calculating long S, which obviously cannot fit into Width as
      // characters usually are wider than 1 pixel.
      SetLength(Ints, Width);
      if GetTextExtentExPointW(DC, S, Width, Width, @Result, @Ints[0], Extent) then
        if Result < Width then
          Exit;

        // Oh, Result = Width. Better continue and try the complete string:
    end;

    SetLength(Ints, Max);
    GetTextExtentExPointW(DC, S, Max, Width, @Result, @Ints[0], Extent);
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
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
begin
  Result := htUpperCase(S1) = htUpperCase(S2);
end;

function WideSameStr1(const S1, S2: UnicodeString): boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := S1 = S2;
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
function ScaleRect(const Rect: TRect; ScaleX, ScaleY: Double): TRect;
 {$ifdef UseInline} inline; {$endif}
begin
  Result.Left := Round(Rect.Left * ScaleX);
  Result.Right := Round(Rect.Right * ScaleX);
  Result.Top := Round(Rect.Top * ScaleY);
  Result.Bottom := Round(Rect.Bottom * ScaleY);
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
function CalcClipRect(Canvas: TCanvas; const Rect: TRect; Printing: boolean): TRect;
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
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
 {$ifdef UseInline} inline; {$endif}
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

function GetTextExtent(DC: HDC; P: PWideChar; N: Integer): TSize;
 {$ifdef UseInline} inline; {$endif}
begin
    GetTextExtentPoint32W(DC, P, N, Result); {win95, 98 ME}
end;

procedure FillRectWhite(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
 {$ifdef UseInline} inline; {$endif}
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

procedure DrawFormControlRect(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Raised, PrintMonoBlack, Disabled: Boolean; Color: TColor; ThemedColor: ThtThemedColor);
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
    if not MonoBlack then
    begin
      if Disabled then
        Brush.Color := ThemedColor(clBtnFace, htseClient)
      else
        Brush.Color := ThemedColor(Color, htseClient);
    end
    else
      Brush.Color := clBlack;//color;
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
        Pen.Color := ThemedColor(clBtnHighlight, htseClient)
      else
        Pen.Color := ThemedColor(clBtnShadow, htseClient);
    end;
    MoveTo(X1, Y2);
    LineTo(X1, Y1);
    LineTo(X2, Y1);
    if not MonoBlack then
    begin
      if Raised then
        Pen.Color := ThemedColor(clBtnShadow, htseClient)
      else
        Pen.Color := ThemedColor(clBtnHighlight, htseClient);
    end
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
 {$ifdef UseInline} inline; {$endif}
begin
  Result.Value := Value;
  Result.VType := VType;
end;

//-- BG ---------------------------------------------------------- 26.12.2011 --
function ToSpecWidth(AsInteger: Integer; AsString: ThtString): TSpecWidth;
// Return a TSpecWidth prepared with values given in AsInteger *and* AsString.
// AsString is used to evaluate the type while AsInteger is used to evaluate the value.
// BG, 26.12.2011: Currently percentage is still converted to permille as done before Value became type Integer.
 {$ifdef UseInline} inline; {$endif}
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

{ TAttribute }

constructor TAttribute.Create(ASym: TAttrSymb; const AValue: Double; const NameStr, ValueStr: ThtString; ACodePage: Integer);
begin
  inherited Create;
  Which := ASym;
  DblValue := AValue;
  Value := Trunc(AValue);
  WhichName := NameStr;
  Name := ValueStr;
  CodePage := ACodePage;
end;

//-- BG ---------------------------------------------------------- 27.01.2013 --
constructor TAttribute.CreateCopy(ASource: TAttribute);
begin
  inherited Create;
  Which := ASource.Which;
  WhichName := ASource.WhichName;
  Value := ASource.Value;
  DblValue := ASource.DblValue;
  Name := ASource.Name;
  CodePage := ASource.CodePage;
end;

{----------------TAttributeList}

procedure TAttributeList.Clear;
begin
  inherited Clear;
  SaveID := '';
end;

//-- BG ---------------------------------------------------------- 21.10.2016 --
function TAttributeList.Clone: TAttributeList;
begin
  Result := TAttributeList.CreateCopy(Self);
end;

//-- BG ---------------------------------------------------------- 27.01.2013 --
constructor TAttributeList.CreateCopy(ASource: TAttributeList);
var
  I: Integer;
begin
  inherited Create;
  if ASource <> nil then
    for I := 0 to ASource.Count - 1 do
      Add(TAttribute.CreateCopy(ASource[I]));
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

function TAttributeList.Find(const Name: ThtString; var T: TAttribute): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].WhichName = Name then
    begin
      Result := True;
      T := Items[I];
      Exit;
    end;
  Result := False;
end;

function TAttributeList.Find(Sy: TAttrSymb; var T: TAttribute): Boolean;
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

{$ifdef UseGenerics}
{$else}
function TAttributeList.GetAttribute(Index: Integer): TAttribute;
begin
  Result := Get(Index);
end;
{$endif}

function TAttributeList.GetClass: ThtString;
var
  T: TAttribute;
  S: ThtString;
  I: Integer;
begin
  Result := '';
  T := nil;
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
  T := nil;
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
  T := nil;
  if Find(TitleSy, T) then
    Result := T.Name
  else
    Result := '';
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

procedure TUrlTarget.SetLast(List: TFontObjBaseList; ALast: Integer);
var
  I: Integer;
begin
  utText.Last := ALast;
  if List.Count > 0 then
    for I := List.Count - 1 downto 0 do
      if ID = List[I].UrlTarget.ID then
        List[I].UrlTarget.utText.Last := ALast
      else
        Break;
end;

{----------------TSelTextCount}

procedure TSelTextCount.AddText(P: PWideChar; Size: Integer);
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

procedure TSelTextCount.AddTextCR(P: PWideChar; Size: Integer);
begin
  AddText(P, Size);
  AddText(#13#10, 2);
end;

function TSelTextCount.Terminate: Integer;
begin
  Result := Leng;
end;

{----------------SelTextBuf.Create}

constructor TSelTextBuf.Create(ABuffer: PWideChar; Size: Integer);
begin
  inherited Create;
  Buffer := ABuffer;
  BufferLeng := Size;
end;

procedure TSelTextBuf.AddText(P: PWideChar; Size: Integer);
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

function TSelTextBuf.Terminate: Integer;
begin
  Buffer[Leng] := #0;
  Result := Leng + 1;
end;

{ TMapArea }

//-- BG ---------------------------------------------------------- 25.06.2014 --
destructor TMapArea.Destroy;
begin
  if FRegion <> 0 then
    DeleteObject(FRegion);
  inherited;
end;

//-- BG ---------------------------------------------------------- 31.12.2011 --
function TMapArea.PtInArea(X, Y: Integer): Boolean;
begin
  Result := PtInRegion(FRegion, X, Y);
end;

{$ifdef UseGenerics}
{$else}

{ TMapAreaList }

//-- BG ---------------------------------------------------------- 31.12.2011 --
function TMapAreaList.GetArea(Index: Integer): TMapArea;
begin
  Result := Get(Index);
end;

{$endif}

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
//  if FAreas.Count >= 1000 then
//    Exit;
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
                Coords[Cnt] := StrToIntDef( htStringToString(S1), 0);
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
  R := TIndentRecList.Create;
  L := TIndentRecList.Create;
end;

destructor TIndentManager.Destroy;
begin
  R.Free;
  L.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 05.02.2011 --
function TIndentManager.AddLeft(YT, YB, W: Integer): TIndentRec;
// For a floating block, update the left edge information.
begin
  Result := TIndentRec.Create;
  Result.YT := YT;
  Result.YB := YB;
  Result.X := LeftEdge(YT) + W;
  L.Add(Result);
  LTopMin := YT;
end;

//-- BG ---------------------------------------------------------- 05.02.2011 --
function TIndentManager.AddRight(YT, YB, W: Integer): TIndentRec;
// For a floating block, update the right edge information.
begin
  Result := TIndentRec.Create;
  Result.YT := YT;
  Result.YB := YB;
  Result.X := RightEdge(YT) - W;
  R.Add(Result);
  RTopMin := YT;
end;

//-- BG ---------------------------------------------------------- 12.08.2013 --
procedure TIndentManager.AdjustY(FirstLeftIndex, FirstRightIndex, Y, Height: Integer);
var
  I, D: Integer;
  IR: TIndentRec;
begin
  D := 0;
  for I := FirstLeftIndex to L.Count - 1 do
  begin
    IR := L[I];
    if IR.YT > Y then
    begin
      if IR.YT < Y + Height then
        D := Y + Height - IR.YT;
      Inc(IR.YT, D);
      Inc(IR.YB, D);
    end;
  end;

  D := 0;
  for I := FirstRightIndex to R.Count - 1 do
  begin
    IR := R[I];
    if IR.YT > Y then
    begin
      if IR.YT < Y + Height then
        D := Y + Height - IR.YT;
      Inc(IR.YT, D);
      Inc(IR.YB, D);
    end;
  end;
end;

{----------------TIndentManager.Reset}

//-- BG ---------------------------------------------------------- 23.02.2011 --
procedure TIndentManager.Init(Lf, Wd: Integer);
begin
  LfEdge := Lf;
  Width  := Wd;
  R.Clear;
  L.Clear;
  LTopMin := 0;
  RTopMin := 0;
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
  IR: TIndentRec;
  MinX: Integer;
begin
  Result := -MaxInt;
  MinX := 0;
  for I := 0 to L.Count - 1 do
  begin
    IR := L[I];
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
  IR: TIndentRec;
  MinX: Integer;
begin
  Result := MaxInt;
  for I := 0 to R.Count - 1 do
  begin
    IR := R[I];
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
      IR := L[I];
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
    with L[I] do
      if (ID = nil) and (YB > Result) then
        Result := YB;
  for I := 0 to R.Count - 1 do
    with R[I] do
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
    with L[I] do
      if (ID = nil) and (YB > CL) then
        CL := YB;
  CR := -1;
  for I := 0 to R.Count - 1 do
    with R[I] do
      if (ID = nil) and (YB > CR) then
        CR := YB;
  Inc(CL);
  Inc(CR);
end;

//-- BG ---------------------------------------------------------- 06.02.2011 --
function TIndentManager.AlignLeft(var Y: Integer; W: Integer): Integer;
// Find an available area to the left at or below Y with a width of W pixels.
//
// Returns the absolute X position of the found area.
// On return Y may be adjusted to a larger value if at original Y there is not
// enough width W.
var
  I, CL, CR, LX, RX, XL, XR, YY, MinX: Integer;
begin
  Y := Max(Y, LTopMin);
  Result := LeftEdge(Y);
  if Result + W > RightEdge(Y) then
  begin
    // too wide, must find a wider place below:
    YY := Y;
    MinX := 0;

    CL := Y;
    XL := Result; // valium for the compiler
    for I := L.Count - 1 downto 0 do
      with L[I] do
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
      with R[I] do
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
  Inc(Result, LfEdge);
end;

function TIndentManager.AlignRight(var Y: Integer; W: Integer): Integer;
// Find an available area to the right at or below Y with a width of W pixels.
//
// Returns the absolute X position of the found area.
// On return Y may be adjusted to a larger value if at original Y there is not
// enough width W.
var
  I, CL, CR, LX, RX, XL, XR, YY, MaxX: Integer;
begin
  Y := Max(Y, RTopMin);
  Result := RightEdge(Y) - W;
  if Result < LeftEdge(Y) then
  begin
    // too wide, must find a wider place below:
    YY := Y;
    MaxX := Width - W;

    CL := Y;
    XL := Result; // valium for the compiler
    for I := L.Count - 1 downto 0 do
      with L[I] do
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
      with R[I] do
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
  Inc(Result, LfEdge);
end;

function TIndentManager.GetNextWiderY(Y: Integer): Integer;
{returns the next Y value which offers a wider space or Y if none}
var
  I, CL, CR: Integer;
begin
  CL := Y;
  for I := 0 to L.Count - 1 do
    with L[I] do
      if not Assigned(ID) and (YB > Y) and ((YB < CL) or (CL = Y)) then
        CL := YB;
  CR := Y;
  for I := 0 to R.Count - 1 do
    with R[I] do
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
  IR: TIndentRec;
begin
  IR := TIndentRec.Create;
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
  IR: TIndentRec;
begin
  IR := TIndentRec.Create;
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
 {$ifdef UseInline} inline; {$endif}
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

function TCharCollection.GetCapacity: Integer;
begin
  Result := Length(FChars);
end;

function TCharCollection.GetSize: Integer;
begin
  Result := FCurrentIndex;
end;

constructor TCharCollection.Create;
begin
  inherited;
  FCurrentIndex := 0;
  Capacity := TokenLeng;
end;

procedure TCharCollection.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> Capacity then
  begin
    SetLength(FChars, NewCapacity);
    SetLength(FIndices, NewCapacity + 1);
  end;
end;

procedure TCharCollection.Add(C: ThtChar; Index: Integer);
begin
  Inc(FCurrentIndex);
  if Capacity <= FCurrentIndex then
    Capacity := Capacity + 50;
  FIndices[FCurrentIndex] := Index;
  FChars[FCurrentIndex] := C;
end;

procedure TCharCollection.Add(const S: ThtString; Index: Integer);
var
  K, L: Integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    K := FCurrentIndex + L;
    if Capacity <= K then
      Capacity := K + 50;
    Move(S[1], FChars[FCurrentIndex + 1], L * SizeOf(ThtChar));
    while FCurrentIndex < K do
    begin
      Inc(FCurrentIndex);
      FIndices[FCurrentIndex] := Index;
    end;
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
//  if Capacity <= K then
//     Capacity := K + 50;
//  Move(T.FChars[1],     FChars[FCurrentIndex + 1], T.FCurrentIndex * SizeOf(ThtChar)); //@@@ Tiburon: todo test
//  Move(T.FIndices[1], FIndices[FCurrentIndex + 1], T.FCurrentIndex * Sizeof(Integer));
//  FCurrentIndex := K;
//end;

{ TokenObj }

constructor TTokenObj.Create;
begin
  inherited;
  Capacity := TokenLeng;
  FCount := 0;
  St := '';
  StringOK := True;
end;

procedure TTokenObj.AddUnicodeChar(Ch: WideChar; Ind: Integer);
{Ch must be Unicode in this method}
begin
  if Capacity <= Count then
    Capacity := Capacity + 50;
  Inc(FCount);
  C[Count] := Ch;
  I[Count] := Ind;
  StringOK := False;
end;

procedure TTokenObj.Clear;
begin
  FCount := 0;
  St := '';
  StringOK := True;
end;

procedure TTokenObj.AddString(S: TCharCollection);
var
  K: Integer;
begin
  K := Count + S.FCurrentIndex;
  if Capacity <= K then
    Capacity := K + 50;
  Move(S.FChars[1],   C[Count + 1], S.FCurrentIndex * Sizeof(ThtChar));
  Move(S.FIndices[1], I[Count + 1], S.FCurrentIndex * Sizeof(Integer));
  FCount := K;
  StringOK := False;
end;

//procedure TokenObj.Concat(T: TokenObj);
//var
//  K: Integer;
//begin
//  K := Count + T.Count;
//  if Capacity <= K then
//    Capacity := K + 50;
//  Move(T.C[1], C[Count + 1], T.Count * Sizeof(ThtChar));
//  Move(T.I[1], I[Count + 1], T.Count * Sizeof(Integer));
//  FCount := K;
//  StringOK := False;
//end;
//
//procedure TokenObj.Remove(N: Integer);
//begin {remove a single character}
//  if N <= Count then
//  begin
//    if N < Count then
//    begin
//      Move(C[N + 1], C[N], (Count - N) * Sizeof(ThtChar));
//      Move(I[N + 1], I[N], (Count - N) * Sizeof(Integer));
//    end;
//    if StringOK then
//      Delete(St, N, 1);
//    Dec(FCount);
//  end;
//end;
//
//procedure TokenObj.Replace(N: Integer; Ch: ThtChar);
//begin {replace a single character}
//  if N <= Count then
//  begin
//    C[N] := Ch;
//    if StringOK then
//      St[N] := Ch;
//  end;
//end;

function TTokenObj.GetCapacity: Integer;
begin
  Result := Length(C) - 1;
end;

//-- BG ---------------------------------------------------------- 20.01.2011 --
procedure TTokenObj.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> Capacity then
  begin
    SetLength(C, NewCapacity + 1);
    SetLength(I, NewCapacity + 1);
    if NewCapacity < Count then
    begin
      FCount := NewCapacity;
      if StringOK then
        St := Copy(St, 1, Count);
    end;
  end;
end;

function TTokenObj.GetString: UnicodeString;
begin
  if not StringOK then
  begin
    SetLength(St, Count);
    if Count > 0 then
       Move(C[1], St[1], SizeOf(WideChar) * Count);
    StringOK := True;
  end;
  Result := St;
end;

{----------------TIDObjectList}

function TIDObjectList.AddObject(const S: ThtString; AObject: TIDObject): Integer;
var
  I: Integer;
  O: TIdObject;
begin
  I := -1;
  if Find(S, I) then
  begin
    try
      O := Objects[I];
      if O.FreeMe then
        O.Free;
    except
    end;
    Delete(I);
  end;
  Result := inherited AddObject(S, AObject);
end;

procedure TIDObjectList.Clear;
var
  I: Integer;
  O: TIdObject;
begin
  for I := 0 to Count - 1 do
  try
    O := Objects[I];
    if O.FreeMe then
      O.Free;
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



//function Points(P0, P1, P2, P3: TPoint): THtBorderPointArray;
//begin
//  Result[0] := P0;
//  Result[1] := P1;
//  Result[2] := P2;
//  Result[3] := P3;
//end;

// BG, 17.04.2013: Color of DrawOnePolygon() must be a real RGB or palette value. Themed or system colors are not supported!
procedure DrawOnePolygon(Canvas: TCanvas; P: THtBorderPointArray; Color: TColor; Side: byte; Printing: Boolean);
 {$ifdef UseInline} inline; {$endif}
{Here we draw a 4 sided polygon (by filling a region).  This represents one
 side (or part of a side) of a border.
 For single pixel thickness, drawing is done by lines for better printing}
//BG, 22.08.2010: in print preview results are better without the single pixel exception.
{$ifdef BorderSinglePixelException}
type
  SideArray = array[0..3, 1..4] of Integer;
const
  AD: SideArray = ((0, 1, 0, 3),
    (0, 1, 1, 1),
    (2, 0, 2, 1),
    (1, 3, 3, 3));
  AP: SideArray = ((0, 1, 0, 3),
    (0, 1, 2, 1),
    (2, 0, 2, 2),
    (1, 3, 3, 3));
{$endif}
var
  R: HRgn;
{$ifdef BorderSinglePixelException}
  OldWidth: Integer;
  OldStyle: TPenStyle;
  OldColor: TColor;
  Thickness: Integer;
  P1, P2: TPoint;
  I: SideArray;
{$endif}
begin
{$ifdef BorderSinglePixelException}
  if Side in [0, 2] then
    Thickness := Abs(P[2].X - P[1].X)
  else
    Thickness := Abs(P[1].Y - P[2].Y);
  if Thickness = 1 then
  begin
    with Canvas do
    begin
      OldColor := Pen.Color;
      OldStyle := Pen.Style;
      OldWidth := Pen.Width;
      Pen.Color := Color;
      Pen.Style := psSolid;
      Pen.Width := 1;
      if Printing then
        I := AP
      else
        I := AD;
      P1 := Point(P[I[Side, 1]].X, P[I[Side, 2]].Y);
      P2 := Point(P[I[Side, 3]].X, P[I[Side, 4]].Y);
      MoveTo(P1.X, P1.Y);
      LineTo(P2.X, P2.Y);
      Pen.Width := OldWidth;
      Pen.Style := OldStyle;
      Pen.Color := OldColor;
    end;
  end
  else
{$endif}
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
procedure DrawBorder(Canvas: TCanvas; ORect, IRect: TRect;
  const C: ThtColorArray; const S: ThtBorderStyleArray;
  BGround: TColor; Print: Boolean; ThemedColor: ThtThemedColor);
{Draw the 4 sides of a border.  The sides may be of different styles or colors.
 The side indices, 0,1,2,3, represent left, top, right, bottom.
 ORect is the outside rectangle of the border, IRect the inside Rectangle.
 BGround is the background color used for the bssDouble style}
var
  PO, PI, PM, P1, P2, Bnd: THtBorderPointArray;
  I: Integer;
  Cl, Color: TColor;
  MRect: TRect;
  lb: TLogBrush;
  Pn, OldPn: HPen;
  W, D: array[0..3] of Integer;
  InPath: boolean;
  PenType, Start: Integer;
  StyleSet: set of ThtBorderStyle;
  OuterRegion, InnerRegion: HRGN;
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
      Brush.Color := ThemedColor(BGround, htseClient) or PalRelative;
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
    begin
      Color := ThemedColor(C[I], htseClient);
      case S[I] of
        bssSolid, bssInset, bssOutset:
        begin
          Bnd[0] := PO[I];
          Bnd[1] := PO[(I + 1) mod 4];
          Bnd[2] := PI[(I + 1) mod 4];
          Bnd[3] := PI[I];
          case S[I] of
            bssInset:
              if I in [0, 1] then
                Color := Darker(Color)
              else
                Color := Lighter(Color);

            bssOutset:
              if I in [2, 3] then
                Color := Darker(Color)
              else
                Color := Lighter(Color);
          end;
          DrawOnePolygon(Canvas, Bnd, Color or PalRelative, I, Print);
        end;

        bssRidge, bssGroove:
        begin {ridge or groove}
          Cl := Color;
          Bnd[0] := PO[I];
          Bnd[1] := PO[(I + 1) mod 4];
          Bnd[2] := PM[(I + 1) mod 4];
          Bnd[3] := PM[I];
          case S[I] of
            bssGroove:
              if I in [0, 1] then
                Color := Darker(Color)
              else
                Color := Lighter(Color);

            bssRidge:
              if I in [2, 3] then
                Color := Darker(Color)
              else
                Color := Lighter(Color);
          end;
          DrawOnePolygon(Canvas, Bnd, Color or PalRelative, I, Print);

          Color := Cl;
          Bnd[0] := PM[I];
          Bnd[1] := PM[(I + 1) mod 4];
          Bnd[2] := PI[(I + 1) mod 4];
          Bnd[3] := PI[I];
          case S[I] of
            bssRidge:
              if I in [0, 1] then
                Color := Darker(Color)
              else
                Color := Lighter(Color);

            bssGroove:
              if (I in [2, 3]) then
                Color := Darker(Color)
              else
                Color := Lighter(Color);
          end;
          DrawOnePolygon(Canvas, Bnd, Color or PalRelative, I, Print);
        end;

        bssDouble:
        begin
          Color := Color or PalRelative;

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
        end;

        bssDashed, bssDotted:
        begin
          if not InPath then
          begin
            lb.lbStyle := BS_SOLID;
            lb.lbColor := Color or PalRelative;
            lb.lbHatch := 0;
            if S[I] = bssDotted then
              PenType := PS_Dot or ps_EndCap_Round
            else
              PenType := PS_Dash or ps_EndCap_Square;
            Pn := ExtCreatePen(PS_GEOMETRIC or PenType or ps_Join_Miter, W[I], lb, 0, nil);
            OldPn := SelectObject(Canvas.Handle, Pn);
            BeginPath(Canvas.Handle);
            MoveToEx(Canvas.Handle, PM[I].x, PM[I].y, nil);
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
{$ifndef Compiler31_Plus}
  FCurrentPPI := FormPixelsPerInch;
{$endif}
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
  DefFontName := htDefFontName;
  DefPreFontName := htDefPreFontName;
  ImageCacheCount := 5;
  QuirksMode := qmStandards;
  LoadCursor := crHourGlass;
{$ifdef HasGestures}
  Touch.InteractiveGestureOptions := [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia];
  Touch.InteractiveGestures := [igPan];
{$endif}
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
  LoadCursor := Source.LoadCursor;
  NoSelect := Source.NoSelect;
  ServerRoot := Source.ServerRoot;
  ParentColor := Source.ParentColor;
  ParentFont := Source.ParentFont;

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

  Cursor := Source.Cursor;
  ParentColor := Source.ParentColor;
  ParentFont := Source.ParentFont;
  ParentShowHint := Source.ParentShowHint;
  OnKeyDown := Source.OnKeyDown;
  OnKeyPress := Source.OnKeyPress;
  OnKeyUp := Source.OnKeyUp;
  OnMouseDown := Source.OnMouseDown;
  OnMouseMove := Source.OnMouseMove;
  OnMouseUp := Source.OnMouseUp;

{$ifdef HasGestures}
  Touch := Source.Touch;
  OnGesture := Source.OnGesture;
{$endif}
end;

//-- BG ---------------------------------------------------------- 08.01.2023 --
function TViewerBase.ThemedColor(AColor: TColor; AStyleElement: ThtStyleElement): TColor;
begin
  Result := AColor;
{$ifdef LCL}
  if Result = clDefault then
    case AStyleElement of
      htseClient: Result := GetDefaultColor(dctBrush);
      htseFont  : Result := GetDefaultColor(dctFont);
    end;
{$endif}

{$ifdef UseVCLStyles}
{$ifdef has_StyleElements}
  case AStyleElement of
    htseFont:
      if (seFont in StyleElements) and TStyleManager.IsCustomStyleActive then
        Result := StyleServices.GetSystemColor(Result);
    htseClient:
      if (seClient in StyleElements) and TStyleManager.IsCustomStyleActive then
        Result := StyleServices.GetSystemColor(Result);
    htseBorder:
      if (seBorder in StyleElements) and TStyleManager.IsCustomStyleActive then
        Result := StyleServices.GetSystemColor(Result);
  end;
{$else}
    if TStyleManager.IsCustomStyleActive then
      Result := StyleServices.GetSystemColor(Result);
{$endif}
{$endif}
end;

//-- BG ---------------------------------------------------------- 08.01.2023 --
function TViewerBase.ThemedColorToRGB(AColor: TColor; AStyleElement: ThtStyleElement): TColor;
begin
  Result := ColorToRGB( ThemedColor( AColor, AStyleElement ));
end;

//-- BG ---------------------------------------------------------- 06.12.2022 --
procedure TViewerBase.CMParentColorChanged(var Message: TMessage);
var
  OldColor: TColor;
  LName: string;
  LClassName: string;
begin
  if csLoading in ComponentState then Exit;
  LName := Name;
  lClassName := ClassName;

  OldColor := ThemedColorToRGB(Color, htseClient);
  Inc(FInCMParentColorChanged); // in FPC inherited produces recursive calls to CMParentColorChanged when ParentColor changed to tr
  if FInCMParentColorChanged = 1 then
  begin
    inherited;
    if not ParentColor then
      Color := FBackGround;
    if OldColor <> ThemedColorToRGB(Color, htseClient) then
      StyleChanged;
  end;
  Dec(FInCMParentColorChanged);
end;

procedure TViewerBase.CMParentFontChanged(var Message: TMessage);
begin
  if csLoading in ComponentState then Exit;
  inherited;
  StyleChanged;
end;

//-- BG ------------------------------------------------------- 24.07.2022 --
procedure TViewerBase.ChangeScale(M, D: Integer{$ifdef Compiler31_Plus}; isDpiChange: Boolean{$endif});
begin
  inherited;
  if M <> D then
    ScaleChanged;
end;

{$ifndef Compiler31_Plus}

//-- BG ------------------------------------------------------- 03.10.2022 --
function TViewerBase.ParentForm: TCustomForm;
begin
  Result := GetParentForm(Self);
end;

//-- BG ------------------------------------------------------- 29.09.2022 --
function TViewerBase.FormPixelsPerInch: Integer;
var
  P: TCustomForm;
begin
  P := ParentForm;

  if P <> nil then
    Result := {$ifndef LCL}ThtCustomForm{$endif}(P).PixelsPerInch
  else
    Result := Screen.PixelsPerInch;
end;

//-- BG ------------------------------------------------------- 03.10.2022 --
procedure TViewerBase.SetPPI(Value: Integer);
begin
  FCurrentPPI := Value;
end;

//-- BG ------------------------------------------------------- 29.09.2022 --
procedure TViewerBase.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if not (csDestroying in ComponentState) then
    SetPPI( FormPixelsPerInch );
end;
{$endif}

{$ifdef LCL}
//-- BG ------------------------------------------------------- 29.09.2022 --
procedure TViewerBase.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
var
  NewPPI: Integer;
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  NewPPI := Round(FCurrentPPI * AYProportion);
  if FCurrentPPI <> NewPPI then
  begin
    FCurrentPPI := NewPPI;
    ScaleChanged;
  end;
end;
{$endif}

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
  if FBackGround <> Value then
  begin
    FBackGround := Value;
    if not ParentColor then
    begin
      Color := FBackGround;
      StyleChanged;
    end;
  end;
end;

procedure TViewerBase.SetOnBitmapRequest(const Value: TGetBitmapEvent);
begin
  FOnBitmapRequest := Value;
end;

procedure TViewerBase.SetOnBitmapRequested(const Value: TGottenBitmapEvent);
begin
  FOnBitmapRequested := Value;
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

procedure TViewerBase.SetLoadCursor(const Value: TCursor);
begin
  FLoadCursor := Value;
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

function TViewerBase.StoreFontName: Boolean;
begin
  Result := FFontName <> htDefFontName;
end;

//-- BG ---------------------------------------------------------- 29.09.2022 --
function TViewerBase.GetPPI: Integer;
begin
  Result := FCurrentPPI;
end;

function TViewerBase.StorePreFontName: Boolean;
begin
  Result := FPreFontName <> htDefPreFontName;
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

{$ifdef has_StyleElements}
procedure TViewerBase.UpdateStyleElements;
begin
  inherited UpdateStyleElements;
end;
{$endif}

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

//>-- DZ 18.09.2011
constructor TIDObject.Create(const AHtmlId: ThtString);
begin
  inherited Create;
  FHtmlID:= Trim(AHtmlId);
end;

procedure TIDObject.AfterConstruction();
begin
  inherited AfterConstruction;
{$ifdef UseGlobalObjectId}
  Inc(GlobalObjectIdCount);
  FGlobalId := GlobalObjectIdPrefix + IntToStr(GlobalObjectIdCount);
{$endif}
end;

function TIDObject.GetId(): ThtString;
begin
  Result := FHtmlId;
{$ifdef UseGlobalObjectId}
  if Length(Result) = 0 then
    Result := FGlobalId;
{$endif}
end;
//<-- DZ

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

{ ThvProgressBar }

{$ifndef Compiler20_Plus}
procedure ThvProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if FStyle = pbstMarquee then
    Params.Style := Params.Style or PBS_MARQUEE;
end;

procedure ThvProgressBar.SetStyle(Value: TProgressBarStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;

    // need to recreate so we set the Params.Style before sending the message
{$ifdef LCL}
    RecreateWnd(Self);
{$else}
    RecreateWnd;
{$endif}

    case Value of
      pbstNormal:  SendMessage(Self.Handle, PBM_SETMARQUEE, 0,   0); // switch off marquee animation
      pbstMarquee: SendMessage(Self.Handle, PBM_SETMARQUEE, 1, 100); // switch on marquee animation
    end;
  end;
end;
{$endif}

{ ThvMeter }

constructor ThvMeter.Create(AOwner: TComponent);
begin
  inherited;

  FLowColor := clRed;      // default color
  FHighColor := clYellow;  // default color
  FLow := MinDouble;       // wont show unless set
  FHigh := MaxDouble;      // wont show unless set
  FOptimum := 50;
end;

// GetPosition and SetPosition are called from an overridden Position
// property which allows us to set the position in a way which stops animation
function ThvMeter.GetPosition: Integer;
begin
  result := inherited Position;
end;

procedure ThvMeter.SetPosition(Value: Integer);
begin
  if Value = inherited Position then
    Exit ;

  // In Vista +, the progress bar only animates on incrementing the position.
  // Therefore, we set the value 1 greater and subtract 1, therefore "jumping" to the position.
  // And Therefore, making it look more like a meter, than a progressbar
  if value < Max then
  begin
    inherited Position := value + 1;
    inherited Position := value;
  end
  else
  begin
    Max := Max + 1;
    inherited Position := Max;
    inherited Position := value;
    Max := Max - 1;
  end;

  UpdateColor;
end;

procedure ThvMeter.UpdateColor;
var
  position: Integer;
begin
  position := GetPosition;

  // This only works if theming is switched off as Windows does not
  // change the color. The best you could do is change the state.
  // Also, if you switch the values and go to low/high then back to "normal/original" range, it does not switch the color back.
  // I do not know how to make it return to the normal color, so chose blue
  if position < FLow then
    SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FLowColor))
  else if position > FHigh then
    SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FHighColor))
  else
    SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(clBlue));
end;

procedure ThvMeter.SetLow(const value: double);
begin
  if value = FLow then
    Exit;

  if (value > Min) and (value < FHigh) and (value < Max) then
    FLow := value;

  UpdateColor;
end;
procedure ThvMeter.SetHigh(const value: double);
begin
  if value = FHigh then
    Exit;

  if (value < Max) and (value > FLow) and (value > Min) then
    FHigh := value;

  UpdateColor;
end;
procedure ThvMeter.SetOptimum(const value: double);
begin
  if value = FOptimum then
    FOptimum := value;

  // no need to update color as Optimum value has no affect on color  
  //UpdateColor;
end;

procedure ThvMeter.SetLowColor(const value: TColor);
begin
  if FLowColor = value then
    Exit;

  FLowColor := value;

  UpdateColor;
end;
procedure ThvMeter.SetHighColor(const value: TColor);
begin
  if FHighColor = value then
    Exit;

  FHighColor := value;

  UpdateColor;
end;


{ TIndentRecList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TIndentRecList.Get(Index: Integer): TIndentRec;
begin
  Result := inherited Get(Index);
end;

{$ifdef UseGenerics}
{$else}

{ ThtMap }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function ThtMap.Get(Index: Integer): TMapItem;
begin
  Result := inherited Get(Index);
end;

{ TFontObjBaseList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TFontObjBaseList.GetBase(Index: Integer): TFontObjBase;
begin
  Result := inherited Get(Index);
end;

{$endif}

initialization
{$ifdef UseGlobalObjectId}
  GlobalObjectIdPrefix := 'GOID-';
  GlobalObjectIdCount := 0; //>-- DZ
{$endif}
end.

