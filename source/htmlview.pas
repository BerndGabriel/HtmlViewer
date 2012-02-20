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

********************************************************************************}

{$I htmlcons.inc}

unit HtmlView;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, LMessages, types, FPimage, HtmlMisc,
{$else}
  Windows,
{$endif}
  Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Contnrs, SysUtils,
{$ifndef NoMetafile}
  MetaFilePrinter, vwPrint,
{$endif}
  URLSubs,
  HtmlGlobals,
  HtmlBuffer,
  HtmlImages,
  HTMLUn2,
  ReadHTML,
  HTMLSubs,
  StyleTypes,
  StyleUn;

const
  wm_FormSubmit = wm_User + 100;
  wm_MouseScroll = wm_User + 102;
  wm_UrlAction = wm_User + 103;

type
  EhtException = class(Exception);
  EhtLoadError = class(EhtException);
  EExcessiveSizeError = class(EhtException);
  EIllegalArgument = class(EhtException);

  THtmlViewer = class;

  THTMLBorderStyle = (htFocused, htNone, htSingle);
  TRightClickParameters = class(TObject)
    URL, Target: ThtString;
    Image: TImageObj;
    ImageX, ImageY: Integer;
    ClickWord: UnicodeString;
  end;
  TCreateIFrameControlEvent = function(Sender: TObject; Owner: TComponent): TViewerBase of object;
  TFilenameExpanded = procedure(Sender: TObject; var Filename: ThtString) of object;
  THotSpotClickEvent = procedure(Sender: TObject; const SRC: ThtString; var Handled: Boolean) of object;
  THotSpotEvent = procedure(Sender: TObject; const SRC: ThtString) of object;
  ThtmlPagePrinted = procedure(Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL, XR: Integer; var StopPrinting: Boolean) of object;
  TMetaRefreshType = procedure(Sender: TObject; Delay: Integer; const URL: ThtString) of object;
  TRightClickEvent = procedure(Sender: TObject; Parameters: TRightClickParameters) of object;

  THtmlViewerOption = (
    htOverLinksActive, htNoLinkUnderline, htPrintTableBackground,
    htPrintBackground, htPrintMonochromeBlack, htShowDummyCaret,
    htShowVScroll, htNoWheelMouse, htNoLinkHilite,
    htNoFocusRect //MK20091107
    );
  THtmlViewerOptions = set of THtmlViewerOption;

  ThtScrollInfo = record
    BWidth: Integer;   // single border width of paintpanel
    BWidth2: Integer;  // double border width of paintpanel
    HWidth: Integer;   // width of paintpanel
    VHeight: Integer;  // height of paintpanel
    HBar: Boolean;     // show horizontal scrollbar
    VBar: Boolean;     // show vertical scrollbar
  end;

  TPaintPanel = class(TCustomPanel)
  private
    FViewer: THtmlViewer;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LButtonDblClk;
  public
    constructor CreateIt(AOwner: TComponent; Viewer: THtmlViewer);
    procedure Paint; override;
    property ParentViewer: THtmlViewer read FViewer;
  end;

  THtmlFileType = (HTMLType, TextType, ImgType, OtherType);

{$ifdef NoMetafile}
  //From MetaFilePrinter.pas
  TPageEvent = procedure(Sender: TObject; NumPage: Integer; var StopPrinting: Boolean) of object;
{$endif}

  THtmlViewerStateBit = (
    vsBGFixed,
    vsDontDraw,
    vsCreating,
    vsProcessing,
    vsLocalImageCache,
    vsHotSpotAction,
    vsMouseScrolling,
    vsLeftButtonDown,
    vsMiddleScrollOn,
    vsHiliting);
  THtmlViewerState = set of THtmlViewerStateBit;

  THistoryItem = class(TObject)
  private
    FUrl: ThtString;
    FTitle: ThtString;
    FPosition: Integer;
    FFileType: THtmlFileType;
  public
    property FileType: THtmlFileType read FFileType write FFileType;
    property Position: Integer read FPosition write FPosition;
    property Title: ThtString read FTitle write FTitle;
    property Url: ThtString read FUrl write FUrl;
  end;

  THistory = class(TObject)
  private
    FHistory: TObjectList;
    FIndex: Integer;
    function GetItem(Index: Integer): THistoryItem;
    function GetCount: Integer;
    function GetLowestIndexOfUrl(const Url: ThtString; Start: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Item: THistoryItem);
    property Index: Integer read FIndex write FIndex;
    property Items[Index: Integer]: THistoryItem read GetItem; default;
    property Count: Integer read GetCount;
  end;

  ThvHistoryItem = class(THistoryItem)
  private
    FFormData: TFreeList;
  public
    destructor Destroy; override;
    property FormData: TFreeList read FFormData write FFormData;
  end;

  ThvHistory = class(THistory)
  private
    function GetItem(Index: Integer): ThvHistoryItem;
  public
    property Items[Index: Integer]: ThvHistoryItem read GetItem; default;
  end;

  ThtPrintPreviewMode = (
    ppAuto,         // THtmlViewer.Print() selects ppPreview, if Prn is a TMetaFilePrinter or selects ppMultiPrint else.
    ppNoOutput,     // THtmlViewer.Print() detects the number of total pages only, but produces no output.
    ppPreview,      // THtmlViewer.Print() produces a print preview. This requires that Prn is a TMetaFilePrinter.
    ppSinglePrint,  // THtmlViewer.Print() produces a single print job. Ends the job or cancels it, if no output has been produced.
    ppMultiPrint);  // THtmlViewer.Print() produces the print, but does not end or cancel the job.

  THtmlViewer = class(THtmlViewerBase)
  private
    // child components
    FBorderPanel: TPanel;
    FPaintPanel: TPaintPanel;
    FHTMLTimer: TTimer;
    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;
    sbWidth: Integer; // cached scroll bar width.

    // constructed stuff
    FFrameOwner: TComponent;
    FVisited: ThtStringList; {visited URLs}

    // stuff copied in CreateCopy
    FBase, FBaseEx, FBaseTarget: ThtString;
    FBorderStyle: THTMLBorderStyle;
    FScrollBars: TScrollStyle;
    FOptions: THtmlViewerOptions;

    // events (also copied in CreateCopy)
    FOnCreateIFrameControl: TCreateIFrameControlEvent;
    FOnFilenameExpanded: TFilenameExpanded; //BG, 19.09.2010: Issue 7: Slow UNC Lookups for Images
    FOnFormSubmit: TFormSubmitEvent;
    FOnHotSpotClick: THotSpotClickEvent;
    FOnHotSpotCovered: THotSpotEvent;
    FOnhtStreamRequest: TGetStreamEvent;
    FOnLinkDrawn: TLinkDrawnEvent;
    FOnMetaRefresh: TMetaRefreshType;
    FOnPageEvent: TPageEvent;
    FOnPrintHTMLHeader, FOnPrintHTMLFooter: ThtmlPagePrinted;
    //FOnPrinting: THTMLViewPrinting;
    FOnRightClick: TRightClickEvent;

    // status info
    FViewerState: THtmlViewerState;
    FHistory: ThvHistory;

    // document related stuff
    FSectionList: ThtDocument;
    FCurrentFile: ThtString;
    FCurrentFileType: ThtmlFileType;
    FDocument: TBuffer;
    FRefreshDelay: Integer;
    FRefreshURL: ThtString;
    FTitle: ThtString;
    // document related status stuff
    FCaretPos: Integer;
    FLinkAttributes: ThtStringList;
    FLinkStart: TPoint;
    FLinkText: UnicodeString;
    FMaxVertical: Integer;
    FNameList: ThtStringList;
    FScrollWidth: Integer;
    // BG, 10.08.2011 speed up inserting images
    FInsertedImages: TStrings;
    FImagesInserted: TTimer;
    FImagesReformat: Boolean;

    // print/preview status
    FPage: Integer;
    FPrintedSize: TPoint;
    FWidthRatio: Double;

    // mouse move status
    FTarget: ThtString;
    FURL: ThtString;
    FTitleAttr: ThtString;
    MiddleY: Integer;
    NoJump: Boolean;
    Sel1: Integer; // text selection status

    // set in LoadStream and read in doImage to get the image into the document.
    FImageStream: TStream;

    // set in SubmitForm and read in WMFormSubmit
    FAction, FFormTarget, FEncType, FMethod: ThtString;
    FStringList: ThtStringList;

{$ifndef NoMetafile}
    vwP: TvwPrinter;
{$endif}
    function CreateHeaderFooter: THtmlViewer;
    function GetBaseTarget: ThtString;
    function GetCursor: TCursor;
    function GetDocumentSource: ThtString;
    function GetFormControlList: TFormControlObjList;
    function GetFormData: TFreeList;
    function GetHScrollBarRange: Integer;
    function GetHScrollPos: Integer;
    function GetIDControl(const ID: ThtString): TIDObject;
    function GetIDDisplay(const ID: ThtString): TPropDisplay;
    function GetLinkList: TList;
    function GetNameList: ThtStringList;
    function GetOnExpandName: TExpandNameEvent;
    function GetOnFileBrowse: TFileBrowseEvent;
    function GetOurPalette: HPalette;
    function GetPosition: Integer;
    function GetScrollBarRange: Integer;
    function GetScrollInfo(DocWidth, DocHeight: Integer): ThtScrollInfo;
    function GetScrollPos: Integer;
    function GetSelLength: Integer;
    function GetSelText: UnicodeString;
    function GetViewImages: Boolean;
    function GetWordAtCursor(X, Y: Integer; var St, En: Integer; var AWord: UnicodeString): Boolean;
    procedure BackgroundChange(Sender: TObject);
    procedure DoHilite(X, Y: Integer); virtual;
    procedure DoImage(Sender: TObject; const SRC: ThtString; var Stream: TStream);
    procedure DoLogic;
    procedure DoScrollBars;
    procedure DrawBorder;
    procedure FormControlEnterEvent(Sender: TObject);
    procedure HandleMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
    procedure HTMLTimerTimer(Sender: TObject);
    procedure ImagesInsertedTimer(Sender: TObject);
    procedure InitLoad;
    procedure Layout;
    procedure Parsed(const Title, Base, BaseTarget: ThtString);
    procedure ParseHtml;
    procedure ParseText;
    procedure ScrollHorz(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollTo(Y: Integer);
    procedure ScrollVert(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure SetBase(Value: ThtString);
    procedure SetBorderStyle(Value: THTMLBorderStyle);
    procedure SetCaretPos(Value: Integer);
    procedure SetFormData(T: TFreeList);
    procedure SetHistoryIndex(Value: Integer);
    procedure SetHScrollPos(Value: Integer);
    procedure SetIDDisplay(const ID: ThtString; Value: TPropDisplay);
    procedure SetOnExpandName(Handler: TExpandNameEvent);
    procedure SetOnFileBrowse(Handler: TFileBrowseEvent);
    procedure SetOnFormSubmit(Handler: TFormSubmitEvent);
    procedure SetOptions(Value: THtmlViewerOptions);
    procedure SetOurPalette(Value: HPalette);
    procedure SetPosition(Value: Integer);
    procedure SetProcessing(Value: Boolean);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetScrollPos(Value: Integer);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetViewerStateBit(Index: THtmlViewerStateBit; Value: Boolean);
    procedure SetViewImages(Value: Boolean);
    procedure SubmitForm(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList);
    procedure UpdateImageCache;
    procedure WMFormSubmit(var Message: TMessage); message WM_FormSubmit;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMMouseScroll(var Message: TMessage); message WM_MouseScroll;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMUrlAction(var Message: TMessage); message WM_UrlAction;
    function GetHistoryIndex: Integer;
    property HTMLTimer: TTimer read FHTMLTimer;
    property BorderPanel: TPanel read FBorderPanel;
    property PaintPanel: TPaintPanel read FPaintPanel;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetPalette: HPALETTE; override;
    function HotSpotClickHandled: Boolean; dynamic;
    function IsProcessing: Boolean;
    procedure DoBackground1(ACanvas: TCanvas; ATop, AWidth, AHeight, FullHeight: Integer);
    procedure DoBackground2(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: Integer; AColor: TColor);
    procedure HTMLMouseDblClk(Message: TWMMouse);
    procedure HTMLMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint);
    procedure HTMLPaint(ACanvas: TCanvas; const ARect: TRect);
    procedure LoadDocument(Document: TBuffer; const DocName: ThtString; DocType: THtmlFileType);
    procedure LoadFile(const FileName: ThtString; ft: ThtmlFileType); virtual;
    procedure LoadString(const Source, Reference: ThtString; ft: ThtmlFileType);
    procedure LoadStream(const URL: ThtString; AStream: TStream; ft: ThtmlFileType);
    procedure PaintWindow(DC: HDC); override;
    procedure SetActiveColor(const Value: TColor); override;
    procedure SetCharset(const Value: TFontCharset); override;
    procedure SetCursor(Value: TCursor); {$ifdef LCL} override; {$endif LCL}
    procedure SetDefBackground(const Value: TColor); override;
    procedure SetHistoryMaxCount(const Value: Integer); override;
    procedure SetHotSpotColor(const Value: TColor); override;
    procedure SetImageCacheCount(const Value: Integer); override;
    procedure SetNoSelect(const Value: Boolean); override;
    procedure SetOnBitmapRequest(const Handler: TGetBitmapEvent); override;
    procedure SetOnDragDrop(const Value: TDragDropEvent); override;
    procedure SetOnDragOver(const Value: TDragOverEvent); override;
    procedure SetOnImageRequest(const Handler: TGetImageEvent); override;
    procedure SetOnImageRequested(const Handler: TGottenImageEvent); override;
    procedure SetOnObjectBlur(const Handler: ThtObjectEvent); override;
    procedure SetOnObjectChange(const Handler: ThtObjectEvent); override;
    procedure SetOnObjectClick(const Handler: TObjectClickEvent); override;
    procedure SetOnObjectFocus(const Handler: ThtObjectEvent); override;
    procedure SetOnPanelCreate(const Handler: TPanelCreateEvent); override;
    procedure SetOnPanelDestroy(const Handler: TPanelDestroyEvent); override;
    procedure SetOnPanelPrint(const Handler: TPanelPrintEvent); override;
    procedure SetOnScript(const Handler: TScriptEvent); override;
    procedure SetPreFontName(const Value: TFontName); override;
    procedure SetPrintScale(const Value: Double); override;
    procedure SetVisitedColor(const Value: TColor); override;
    procedure SetVisitedMaxCount(const Value: Integer); override;
    property ScrollWidth: Integer read FScrollWidth;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateCopy(Owner: TComponent; Source: TViewerBase); override;
    destructor Destroy; override;
    function CreateIFrameControl: TViewerBase;
    function DisplayPosToXy(DisplayPos: Integer; var X, Y: Integer): Boolean;
    function Find(const S: UnicodeString; MatchCase: Boolean): Boolean;
    function FindDisplayPos(SourcePos: Integer; Prev: Boolean): Integer;
    function FindEx(const S: UnicodeString; MatchCase, Reverse: Boolean): Boolean;
    function FindSourcePos(DisplayPos: Integer): Integer;
    function FullDisplaySize(FormatWidth: Integer): TSize;
    function GetCharAtPos(Pos: Integer; var Ch: WideChar; var Font: TFont): Boolean;
    function GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
    function GetTextByIndices(AStart, ALast: Integer): UnicodeString;
    function GetURL(X, Y: Integer; var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType;
    function HtmlExpandFilename(const Filename: ThtString): ThtString; override;
    function InsertImage(const Src: ThtString; Stream: TStream): Boolean;
    function MakeBitmap(YTop, FormatWidth, Width, Height: Integer): TBitmap;
{$ifndef NoMetafile}
    function MakeMetaFile(YTop, FormatWidth, Width, Height: Integer): TMetaFile;
    function MakePagedMetaFiles(Width, Height: Integer): TList;
    procedure NumPrinterPages(MFPrinter: TMetaFilePrinter; out Width, Height: Integer); overload;
    procedure OpenPrint;
    procedure ClosePrint;
    procedure AbortPrint;
    procedure Print(FromPage: Integer = 1; ToPage: Integer = MaxInt; Prn: TvwPrinter = nil); overload;
    function PrintPreview(Prn: TMetaFilePrinter; NoOutput: Boolean = False; FromPage: Integer = 1; ToPage: Integer = MaxInt): Integer; virtual;
    // print or preview:
    function Print(Prn: ThtPrinter; FromPage: Integer; ToPage: Integer; Mode: ThtPrintPreviewMode = ppAuto): Integer; overload;
{$endif}
    function NumPrinterPages(out WidthRatio: Double): Integer; overload;
    function NumPrinterPages: Integer; overload;
    function PositionTo(Dest: ThtString): Boolean;
    function PtInObject(X, Y: Integer; var Obj: TObject): Boolean; {X, Y, are client coord}
    function ShowFocusRect: Boolean; override;
    function XYToDisplayPos(X, Y: Integer): Integer;
    procedure AddVisitedLink(const S: ThtString);
    procedure BumpHistory(const OldFileName, OldTitle: ThtString; OldPos: Integer; OldFormData: TFreeList; OldDocType: ThtmlFileType);
    procedure CheckVisitedLinks;
    procedure Clear; virtual;
    procedure ClearHistory;
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure CopyToClipboard;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Draw(Canvas: TCanvas; YTop, FormatWidth, Width, Height: Integer);
    procedure htProgress(Percent: Integer); override;
    procedure htProgressEnd;
    procedure htProgressInit;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Load(const Url: ThtString); override;
    //
    procedure LoadFromDocument(Document: TBuffer; const Reference: ThtString; DocType: THtmlFileType = HtmlType);
    procedure LoadFromFile(const FileName: ThtString; DocType: THtmlFileType = HtmlType);
    procedure LoadFromStream(const AStream: TStream; const Reference: ThtString = ''; DocType: THtmlFileType = HtmlType);
    procedure LoadFromString(const S: ThtString; const Reference: ThtString = ''; DocType: THtmlFileType = HtmlType);
    // due to reduction of code duplications the following methods became obsolete:
    procedure LoadFromBuffer(Buffer: PChar; BufLenTChars: Integer; const Reference: ThtString = ''); deprecated; // use LoadFromString() instead
    procedure LoadImageFile(const FileName: ThtString); deprecated; // use LoadFromFile() instead.
    procedure LoadStrings(const Strings: ThtStrings; const Reference: ThtString = ''); deprecated; // use LoadFromString() instead.
    procedure LoadTextFile(const FileName: ThtString); deprecated; // use LoadFromFile() instead.
    procedure LoadTextFromString(const S: ThtString); deprecated; // use LoadFromString() instead.
    procedure LoadTextStrings(Strings: ThtStrings); deprecated; // use LoadFromString() instead.
    //
    procedure Reformat;
    procedure Reload;
    procedure Repaint; override;
    procedure ReplaceImage(const NameID: ThtString; NewImage: TStream);
    procedure SelectAll;
    procedure SetImageCache(ImageCache: ThtImageCache);
    procedure TriggerUrlAction;
    procedure UrlAction;
    property Base: ThtString read FBase write SetBase;
    property BaseEx: ThtString read FBaseEx write FBaseEx;
    property BaseTarget: ThtString read GetBaseTarget;
    property CaretPos: Integer read FCaretPos write SetCaretPos;
    property CurrentFile: ThtString read FCurrentFile;
    property DocumentSource: ThtString read GetDocumentSource;
    property DocumentTitle: ThtString read FTitle;
    property FormControlList: TFormControlObjList read GetFormControlList;
    property FormData: TFreeList read GetFormData write SetFormData;
    property History: ThvHistory read FHistory;
    property HistoryIndex: Integer read GetHistoryIndex write SetHistoryIndex;
    property HScrollBarPosition: Integer read GetHScrollPos write SetHScrollPos;
    property HScrollBarRange: Integer read GetHScrollBarRange;
    property IDControl[const ID: ThtString]: TIDObject read GetIDControl;
    property IDDisplay[const ID: ThtString]: TPropDisplay read GetIDDisplay write SetIDDisplay;
    property LinkAttributes: ThtStringList read FLinkAttributes;
    property LinkList: TList read GetLinkList;
    property LinkStart: TPoint read FLinkStart;
    property LinkText: UnicodeString read FLinkText write FLinkText;
    property MaxVertical: Integer read FMaxVertical;
    property NameList: ThtStringList read GetNameList;
    property OnCreateIFrameControl: TCreateIFrameControlEvent read FOnCreateIFrameControl write FOnCreateIFrameControl;
    property OnExpandName: TExpandNameEvent read GetOnExpandName write SetOnExpandName;
    property OnLinkDrawn: TLinkDrawnEvent read FOnLinkDrawn write FOnLinkDrawn;
    property OnPageEvent: TPageEvent read FOnPageEvent write FOnPageEvent;
    property Palette: HPalette read GetOurPalette write SetOurPalette;
    property Position: Integer read GetPosition write SetPosition;
    //property Processing: Boolean index vsProcessing read GetViewerStateBit; //BG, 25.11.2010: C++Builder fails handling enumeration indexed properties
    property SectionList: ThtDocument read FSectionList;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read FCaretPos write SetSelStart;
    property SelText: UnicodeString read GetSelText;
    property Target: ThtString read FTarget write FTarget;
    property TitleAttr: ThtString read FTitleAttr;
    //property TitleHistory: ThtStrings read FTitleHistory;
    property URL: ThtString read FURL write FURL;
    property ViewerState: THtmlViewerState read FViewerState;
    property VScrollBarPosition: Integer read GetScrollPos write SetScrollPos;
    property VScrollBarRange: Integer read GetScrollBarRange;
    property FrameOwner: TComponent read FFrameOwner;
    property HScrollBar: TScrollBar read FHScrollBar;
    property Visited: ThtStringList read FVisited; {visited URLs}
    property VScrollBar: TScrollBar read FVScrollBar;
  published
    property BorderStyle: THTMLBorderStyle read FBorderStyle write SetBorderStyle;
    property CharSet;
    property DefBackground;
    property DefFontColor;
    property DefFontName;
    property DefFontSize;
    property DefHotSpotColor;
    property DefOverLinkColor;
    property DefPreFontName;
    property DefVisitedLinkColor;
    property HistoryMaxCount;
    property HtOptions: THtmlViewerOptions read FOptions write SetOptions default [htPrintTableBackground, htPrintMonochromeBlack];
    property ImageCacheCount;
    property MarginHeight;
    property MarginWidth;
    property NoSelect;
    property PrintMarginBottom;
    property PrintMarginLeft;
    property PrintMarginRight;
    property PrintMarginTop;
    property PrintMaxHPages;
    property PrintScale;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property ServerRoot;
    property ViewImages: Boolean read GetViewImages write SetViewImages default True;
    property VisitedMaxCount;
    //
    property OnBitmapRequest;
    property OnDragDrop;
    property OnDragOver;
    property OnFileBrowse: TFileBrowseEvent read GetOnFileBrowse write SetOnFileBrowse;
    property OnFilenameExpanded: TFilenameExpanded read FOnFilenameExpanded write FOnFilenameExpanded;
    property OnFormSubmit: TFormSubmitEvent read FOnFormSubmit write SetOnFormSubmit;
    property OnHistoryChange;
    property OnHotSpotClick: THotSpotClickEvent read FOnHotSpotClick write FOnHotSpotClick;
    property OnHotSpotCovered: THotSpotEvent read FOnHotSpotCovered write FOnHotSpotCovered;
    property OnhtStreamRequest: TGetStreamEvent read FOnhtStreamRequest write FOnhtStreamRequest;
    property OnImageClick;
    property OnImageOver;
    property OnImageRequest;
    property OnImageRequested;
    property OnInclude;
    property OnLink;
    property OnMeta;
    property OnMetaRefresh: TMetaRefreshType read FOnMetaRefresh write FOnMetaRefresh;
    property OnMouseDouble;
    property OnObjectBlur;
    property OnObjectChange;
    property OnObjectClick;
    property OnObjectFocus;
    property OnObjectTag;
    property OnPanelCreate;
    property OnPanelDestroy;
    property OnPanelPrint;
    property OnParseBegin;
    property OnParseEnd;
    property OnPrinted;
    property OnPrintFooter;
    property OnPrintHeader;
    property OnPrintHTMLFooter: ThtmlPagePrinted read FOnPrintHTMLFooter write FOnPrintHTMLFooter;
    property OnPrintHTMLHeader: ThtmlPagePrinted read FOnPrintHTMLHeader write FOnPrintHTMLHeader;
    property OnPrinting;
    property OnProcessing;
    property OnProgress;
    property OnRightClick: TRightClickEvent read FOnRightClick write FOnRightClick;
    property OnScript;
    property OnSoundRequest;
    //
    property Align;
    property Anchors;
{$ifdef LCL}
    property Cursor default crIBeam;
{$else}
    property Cursor: TCursor read GetCursor write SetCursor default crIBeam;
{$endif}
    property Enabled;
    property Height default 150;
    property Name;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Visible;
    property Width default 150;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseWheel;
  end;
  THtmlViewerClass = class of THtmlViewer;

function IsHtmlExt(const Ext: ThtString): Boolean;
function IsImageExt(const Ext: ThtString): Boolean;
function IsTextExt(const Ext: ThtString): Boolean;
function GetFileType(const S: ThtString): THtmlFileType;

implementation

uses
  Math, Clipbrd, Forms, Printers, {$IFDEF UNICODE}AnsiStrings, {$ENDIF}
  HTMLGif2 {$IFNDEF NoGDIPlus}, GDIPL2A{$ENDIF};

const
  ScrollGap = 20;

type
  PositionObj = class(TObject)
    Pos: Integer;
    FileType: ThtmlFileType;
    FormData: TFreeList;
    destructor Destroy; override;
  end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
function IsHtmlExt(const Ext: ThtString): Boolean;
begin
  Result := (Ext = '.html') or (Ext = '.css') or (Ext = '.htm') or (Ext = '.php') or (Ext = '.xhtml') or (Ext = '.xht');
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
function IsImageExt(const Ext: ThtString): Boolean;
begin
  Result := (Ext = '.gif') or (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.bmp');
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
function IsTextExt(const Ext: ThtString): Boolean;
begin
  Result := (Ext = '.txt');
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
function GetFileType(const S: ThtString): THtmlFileType;
var
  Ext: ThtString;
begin
  Ext := LowerCase(ExtractFileExt(S));
  if IsHtmlExt(Ext) then
    Result := HTMLType
  else if IsImageExt(Ext) then
    Result := ImgType
  else if IsTextExt(Ext) then
    Result := TextType
  else
    Result := OtherType;
end;

constructor THtmlViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrameOwner := AOwner;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
  Include(FViewerState, vsCreating);
  Height := 150;
  Width := 150;
  SetCursor(crIBeam);

  FBorderPanel := TPanel.Create(Self);
  FBorderPanel.BevelInner := bvNone;
  FBorderPanel.BevelOuter := bvNone;
  FBorderPanel.Align := alClient;
{$ifndef LCL}
  FBorderPanel.Ctl3D := False;
  FBorderPanel.ParentCtl3D := False;
{$ifdef delphi7_plus}
  FBorderPanel.ParentBackground := False;
{$endif}
{$endif}
  FBorderPanel.Parent := Self;

  FPaintPanel := TPaintPanel.CreateIt(Self, Self);
  FPaintPanel.ParentFont := False;
  FPaintPanel.Parent := self;
  FPaintPanel.BevelOuter := bvNone;
  FPaintPanel.BevelInner := bvNone;
{$ifndef LCL}
  FPaintPanel.Ctl3D := False;
{$endif}
  //FPaintPanel.OnPaint := HTMLPaint;
  FPaintPanel.OnMouseDown := HTMLMouseDown;
  FPaintPanel.OnMouseMove := HTMLMouseMove;
  FPaintPanel.OnMouseUp := HTMLMouseUp;

  FVScrollBar := TScrollBar.Create(Self);
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.SmallChange := 16;
  FVScrollBar.OnScroll := ScrollVert;
  FVScrollBar.Visible := False;
  FVScrollBar.TabStop := False;
  sbWidth := FVScrollBar.Width;
  FVScrollBar.Parent := Self;

  FHScrollBar := TScrollBar.Create(Self);
  FHScrollBar.Kind := sbHorizontal;
  FHScrollBar.SmallChange := 15;
  FHScrollBar.OnScroll := ScrollHorz;
  FHScrollBar.Visible := False;
  FHScrollBar.TabStop := False;
  FHScrollBar.Parent := Self;

  FScrollBars := ssBoth;

  FSectionList := ThtDocument.Create(Self, PaintPanel);
  FSectionList.ControlEnterEvent := FormControlEnterEvent;
  FSectionList.OnBackgroundChange := BackgroundChange;
  FSectionList.ShowImages := True;
  FNameList := FSectionList.IDNameList;

  SetOptions([htPrintTableBackground, htPrintMonochromeBlack]);

  FHistory := ThvHistory.Create;
  FVisited := ThtStringList.Create;

  FHTMLTimer := TTimer.Create(Self);
  FHTMLTimer.Enabled := False;
  FHTMLTimer.Interval := 200;
  FHTMLTimer.OnTimer := HTMLTimerTimer;
  FLinkAttributes := ThtStringList.Create;

  FInsertedImages := TStringList.Create;
  FImagesInserted := TTimer.Create(Self);
  FImagesInserted.Enabled := False;
  FImagesInserted.Interval := 100;
  FImagesInserted.OnTimer := ImagesInsertedTimer;
{$ifdef LCL}
  // BG, 24.10.2010: there is no initial WMSize message, thus size child components now:
  DoScrollBars;
{$endif}
  Exclude(FViewerState, vsCreating);
end;

//-- BG ---------------------------------------------------------- 23.11.2011 --
constructor THtmlViewer.CreateCopy(Owner: TComponent; Source: TViewerBase);
var
  Viewer: THtmlViewer absolute Source;
begin
  inherited CreateCopy(Owner, Viewer);
  if Source is THtmlViewer then
  begin
    Self.QuirksMode := Viewer.QuirksMode;
    FBase := Viewer.FBase;
    FBaseEx := Viewer.FBaseEx;
    FBaseTarget := Viewer.FBaseTarget;
    FBorderStyle := Viewer.FBorderStyle;
    FScrollBars := Viewer.FScrollBars;
    HtOptions := Viewer.HtOptions;

    OnCreateIFrameControl := Viewer.OnCreateIFrameControl;
    OnFilenameExpanded := Viewer.OnFilenameExpanded; //BG, 19.09.2010 := Viewer.Issue 7 := Viewer.Slow UNC Lookups for Images
    OnFormSubmit := Viewer.OnFormSubmit;
    OnHistoryChange := Viewer.OnHistoryChange;
    OnHotSpotClick := Viewer.OnHotSpotClick;
    OnHotSpotCovered := Viewer.OnHotSpotCovered;
    OnhtStreamRequest := Viewer.OnhtStreamRequest;
    OnLinkDrawn := Viewer.OnLinkDrawn;
    OnMetaRefresh := Viewer.OnMetaRefresh;
    OnPageEvent := Viewer.OnPageEvent;
    OnPrintHTMLFooter := Viewer.OnPrintHTMLFooter;
    OnPrintHTMLHeader := Viewer.OnPrintHTMLHeader;
    OnRightClick := Viewer.OnRightClick;
  end;
end;

destructor THtmlViewer.Destroy;
begin
{$ifndef NoMetaFile}
  AbortPrint;
{$endif}
  FImagesInserted.Free;
  FInsertedImages.Free;
  FHTMLTimer.Free;
  Exclude(FViewerState, vsMiddleScrollOn);
  if vsLocalImageCache in FViewerState then
  begin
    FSectionList.Clear;
    FSectionList.ImageCache.Free;
  end;
  FSectionList.Free;
  FHistory.Free;
  Visited.Free;
  FLinkAttributes.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure THtmlViewer.Parsed(const Title, Base, BaseTarget: ThtString);
begin
  FTitle := Title;
  if Base <> '' then
    FBase := Base
  else
    FBase := BaseEx;
  FBaseTarget := BaseTarget;
  if Assigned(OnParseEnd) then
    OnParseEnd(Self);
  try
    Include(FViewerState, vsDontDraw);
    {Load the background bitmap if any and if ViewImages set}
    FSectionList.GetBackgroundBitmap;
    DoLogic;
  finally
    Exclude(FViewerState, vsDontDraw);
  end;
end;

//-- BG ---------------------------------------------------------- 13.03.2011 --
procedure THtmlViewer.ParseHtml;
var
  Parser: THtmlParser;
begin
  Parser := THtmlParser.Create(FDocument);
  try
    Parser.ParseHtml(FSectionList, OnInclude, OnSoundRequest, HandleMeta, OnLink);
    Parsed(Parser.Title, Parser.Base, Parser.BaseTarget);
  finally
    Parser.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 13.03.2011 --
procedure THtmlViewer.ParseText;
var
  Parser: THtmlParser;
begin
  Parser := THtmlParser.Create(FDocument);
  try
    Parser.ParseText(FSectionList);
    Parsed(Parser.Title, Parser.Base, Parser.BaseTarget);
  finally
    Parser.Free;
  end;
end;

procedure THtmlViewer.LoadFile(const FileName: ThtString; ft: ThtmlFileType);
var
  Dest, Name: ThtString;
  Stream: TStream;
begin
  IOResult; {eat up any pending errors}
  SplitDest(FileName, Name, Dest);
  if Name <> '' then
    Name := ExpandFileName(Name);
  FCurrentFile := Name; //BG, 03.04.2011: issue 83: Failure to set FCurrentFile
  if not FileExists(Name) then
    raise EhtLoadError.CreateFmt('Can''t locate ''%s''.', [Name]);
  Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
  try
    LoadStream(Name + Dest, Stream, ft);
  finally
    Stream.Free;
  end;
end;

procedure THtmlViewer.LoadFromFile(const FileName: ThtString; DocType: ThtmlFileType);
var
  OldFile, OldTitle: ThtString;
  OldPos: Integer;
  OldType: ThtmlFileType;
  OldFormData: TFreeList;
begin
  if IsProcessing then
    Exit;
  if Filename <> '' then
  begin
    OldFile := FCurrentFile;
    OldTitle := FTitle;
    OldPos := Position;
    OldType := FCurrentFileType;
    OldFormData := GetFormData;
    try
      LoadFile(FileName, DocType);
      if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
        BumpHistory(OldFile, OldTitle, OldPos, OldFormData, OldType)
      else
        OldFormData.Free;
    except
      OldFormData.Free;
      raise;
    end;
  end;
end;

{----------------THtmlViewer.LoadTextFile}

procedure THtmlViewer.LoadTextFile(const FileName: ThtString);
begin
  LoadFromFile(FileName, TextType);
end;

{----------------THtmlViewer.LoadImageFile}

procedure THtmlViewer.LoadImageFile(const FileName: ThtString);
begin
  LoadFromFile(FileName, ImgType);
end;

{----------------THtmlViewer.LoadStrings}

procedure THtmlViewer.LoadStrings(const Strings: ThtStrings; const Reference: ThtString);
begin
  LoadString(Strings.Text, Reference, HTMLType);
end;

{----------------THtmlViewer.LoadTextStrings}

procedure THtmlViewer.LoadTextStrings(Strings: ThtStrings);
begin
  LoadString(Strings.Text, '', TextType);
end;

{----------------THtmlViewer.LoadFromBuffer}

procedure THtmlViewer.LoadFromBuffer(Buffer: PChar; BufLenTChars: Integer; const Reference: ThtString);
var
  S: ThtString;
begin
  SetString(S, Buffer, BufLenTChars); // Yunqa.de: SetString() is faster than SetLength().
  LoadFromString(S, Reference, HtmlType);
end;

{----------------THtmlViewer.LoadTextFromString}

// deprecated
procedure THtmlViewer.LoadTextFromString(const S: ThtString);
begin
  LoadString(S, '', TextType);
end;

//-- BG ---------------------------------------------------------- 27.12.2010 --
procedure THtmlViewer.LoadFromDocument(Document: TBuffer; const Reference: ThtString; DocType: THtmlFileType);
begin
  LoadDocument(Document, Reference, DocType);
end;

{----------------THtmlViewer.LoadFromString}
procedure THtmlViewer.LoadFromString(const S: ThtString; const Reference: ThtString; DocType: THtmlFileType);
begin
  LoadString(S, Reference, DocType);
end;

{----------------THtmlViewer.LoadString}

//-- BG ---------------------------------------------------------- 24.11.2011 --
procedure THtmlViewer.Load(const Url: ThtString);
begin
  inherited;
  LoadFromFile(Url);
end;

//-- BG ---------------------------------------------------------- 01.01.2012 --
procedure THtmlViewer.LoadDocument(Document: TBuffer; const DocName: ThtString; DocType: THtmlFileType);
var
  Dest, Name, OldFile: ThtString;
  OldCursor: TCursor;
begin
  if IsProcessing then
    Exit;

  SplitDest(DocName, Name, Dest);
  case DocType of
    ImgType:
      raise EhtException.Create('LoadDocument with DocType = ''ImgType'' not supported.');
  else
    if Document = nil then
      raise EhtException.Create('LoadDocument requires document to load. Parameter ''Document'' must not be nil.');
  end;

  SetProcessing(True);
  try
    OldFile := FCurrentFile;
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    Include(FViewerState, vsDontDraw);
    try
      FRefreshDelay := 0;
      FSectionList.ProgressStart := 75;
      htProgressInit;
      try
        //handle quirks mode settings
        if (DocType = HTMLType) then begin
          case QuirksMode of
            qmDetect :
              begin
                with THtmlParser.Create(Document) do
                try
                  FUseQuirksMode := ShouldUseQuirksMode;
                finally
                  Free;
                end;
              end;
            qmStandards :
              begin
                FUseQuirksMode := False;
              end;
            qmQuirks :
              begin
                FUseQuirksMode := True;
              end;
          end;
        end else begin
          FUseQuirksMode := False;
        end;
        Self.FSectionList.UseQuirksMode := FUseQuirksMode;
        // terminate old document
        InitLoad;
        CaretPos := 0;
        Sel1 := -1;
        if Assigned(OnSoundRequest) then
          OnSoundRequest(Self, '', 0, True);
        FreeAndNil(FDocument);

        // load new document
        FDocument := Document;
        FCurrentFile := ExpandFileName(Name);
        FCurrentFileType := DocType;
        if Assigned(OnParseBegin) then
          OnParseBegin(Self, FDocument);

        case DocType of
          HTMLType:
            ParseHtml;
        else
          ParseText;
        end;
        CheckVisitedLinks;
        if not PositionTo(Dest) and ((FCurrentFile = '') or (FCurrentFile <> OldFile)) then
        begin
          {if neither destination specified nor same file, move to top}
          ScrollTo(0);
          HScrollBar.Position := 0;
        end;
      finally
        PaintPanel.Invalidate;
        htProgressEnd;
      end;
    finally
      Exclude(FViewerState, vsDontDraw);
      Screen.Cursor := OldCursor;
    end;
  finally
    SetProcessing(False);
  end;
  if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
    FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------THtmlViewer.LoadString}

procedure THtmlViewer.LoadString(const Source, Reference: ThtString; ft: ThtmlFileType);
begin
  if IsProcessing then
    Exit;
  LoadDocument(TBuffer.Create(Source, Reference), Reference, ft);
end;

{----------------THtmlViewer.LoadFromStream}

procedure THtmlViewer.LoadFromStream(const AStream: TStream; const Reference: ThtString; DocType: ThtmlFileType);
begin
  LoadStream(Reference, AStream, DocType);
end;

procedure THtmlViewer.DoImage(Sender: TObject; const SRC: ThtString; var Stream: TStream);
begin
  Stream := FImageStream;
end;

{----------------THtmlViewer.LoadStream}

procedure THtmlViewer.LoadStream(const URL: ThtString; AStream: TStream; ft: ThtmlFileType);
var
  SaveOnImageRequest: TGetImageEvent;
begin
  if IsProcessing or not Assigned(AStream) then
    Exit;
  AStream.Position := 0;
  if ft in [HTMLType, TextType] then
    LoadDocument(TBuffer.Create(AStream, URL), URL, ft)
  else
  begin
    SaveOnImageRequest := OnImageRequest;
    try
      FImageStream := AStream;
      OnImageRequest := DoImage;
      LoadDocument(TBuffer.Create('<img src="' + URL + '">'), URL, HTMLType)
    finally
      FImageStream := nil;
      OnImageRequest := SaveOnImageRequest;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 20.02.2011 --
function THtmlViewer.GetScrollInfo(DocWidth, DocHeight: Integer): ThtScrollInfo;
begin
  with Result do
  begin
    if (FBorderStyle = htNone) and not (csDesigning in ComponentState) then
    begin
      BWidth2 := 0;
      BWidth  := 0;
    end
    else
    begin
      BWidth2 := BorderPanel.Width - BorderPanel.ClientWidth;
      BWidth  := BWidth2 div 2;
    end;
    HWidth  := Width  - BWidth2;
    VHeight := Height - BWidth2;

    case FScrollBars of
      ssVertical:
      begin
        VBar := (htShowVScroll in htOptions) or (DocHeight > VHeight);
        if VBar then
          Dec(HWidth, sbWidth);
        HBar := False;
      end;

      ssHorizontal:
      begin
        VBar := False;
        HBar := DocWidth > HWidth;
        if HBar then
          Dec(VHeight, sbWidth);
      end;

      ssBoth:
      begin
        VBar := (htShowVScroll in htOptions) or (DocHeight > VHeight);
        if VBar then
          Dec(HWidth, sbWidth);
        HBar := DocWidth > HWidth;
        if HBar then
        begin
          Dec(VHeight, sbWidth);
          if not VBar and (DocHeight > VHeight) then
          begin
            VBar := True;
            Dec(HWidth, sbWidth);
          end;
        end;
      end;
    else
      HBar := False;
      VBar := False;
    end;
  end;
end;

{----------------THtmlViewer.DoScrollBars}

procedure THtmlViewer.DoScrollBars;

  procedure SetPageSizeAndMax(SB: TScrollBar; PageSize, Max: Integer);
  begin
    //BG, 19.02.2011: you can set neither a pagesize > max nor a max < pagesize
    // thus set in different order for growing and shrinking:
    if PageSize < SB.Max then
    begin
      SB.PageSize := PageSize;
      SB.Max := Max;
    end
    else
    begin
      SB.Max := Max;
      SB.PageSize := PageSize;
    end;
  end;

var
  ScrollInfo: ThtScrollInfo;
begin
  FScrollWidth := Min(ScrollWidth, MaxHScroll);
  ScrollInfo := GetScrollInfo(FScrollWidth, FMaxVertical);
  with ScrollInfo do
  begin
    BorderPanel.Visible := False;
    if BWidth > 0 then
      BorderPanel.Visible := True;

    PaintPanel.Left := BWidth;
    PaintPanel.Top := BWidth;
    PaintPanel.Width := HWidth;
    PaintPanel.Height := VHeight;

    HScrollBar.Visible := HBar;
    if HBar then
    begin
      HScrollBar.SetBounds(PaintPanel.Left, PaintPanel.Top + PaintPanel.Height, PaintPanel.Width, sbWidth);
      HScrollBar.LargeChange := Max(16, HWidth - HScrollBar.SmallChange);
      HScrollBar.Enabled := HWidth < ScrollWidth;
      if HScrollBar.Enabled then
        SetPageSizeAndMax(HScrollBar, HWidth, ScrollWidth);
    end;

    VScrollBar.Visible := VBar;
    if VBar then
    begin
      VScrollBar.SetBounds(PaintPanel.Left + PaintPanel.Width, PaintPanel.Top, sbWidth, PaintPanel.Height);
      VScrollBar.LargeChange := Max(16, VHeight - VScrollBar.SmallChange);
      VScrollBar.Enabled := VHeight < FMaxVertical;
      if VScrollBar.Enabled then
        SetPageSizeAndMax(VScrollBar, VHeight, FMaxVertical);
    end;
  end;
end;

{----------------THtmlViewer.DoLogic}

//BG, 20.02.2011: collecting some statistics to find the fastest way to try DoLogic:
// If I show a lot of large documents, trying with scrollbar first is faster as the first try
// will be already the last one, but lots of small documents might prefer checking without
// scrollbar first.
var
  VScrollBalance: Integer;
  LogicDonePos: array [0..3] of integer;
  LogicDoneNeg: array [0..3] of integer;

procedure THtmlViewer.DoLogic;

  procedure SectionListDoLogic(Width, Height: Integer);
  var
    Curs: Integer;
  begin
    Curs := 0;
    FScrollWidth := 0;
    FMaxVertical := FSectionList.DoLogic(PaintPanel.Canvas, 0, Width, Height, 0, FScrollWidth, Curs);
  end;

var
  SI, SI2, SI3: ThtScrollInfo;
begin
  HandleNeeded;
  Include(FViewerState, vsDontDraw);
  try
    SI := GetScrollInfo(0, 0);
    case FScrollBars of
      ssBoth,
      ssVertical:
      begin
        // if lots of long documents are shown, it is most likely that a first DoLogic()
        // with scrollbar will find the necessity of a vertical scrollbar again:
        if VScrollBalance >= 0 then
        begin
          Inc(LogicDonePos[0]);
          SectionListDoLogic(SI.HWidth - sbWidth, SI.VHeight);
          if not (htShowVScroll in FOptions) and (FMaxVertical > 0) then
          begin
            Inc(LogicDonePos[1]);
            SI2 := GetScrollInfo(ScrollWidth, FMaxVertical);
            SI.VBar := SI2.VBar;
            if not SI2.VBar or (FMaxVertical <= 2 * SI.VHeight) then
            begin
              // scrollbar is not needed or maybe becomes obsolete on a wider panel:
              // Try whether it is not needed on a wider paint panel:
              Inc(LogicDonePos[2]);
              SectionListDoLogic(SI.HWidth, SI.VHeight);
              SI3 := GetScrollInfo(ScrollWidth, FMaxVertical);
              SI.VBar := SI3.VBar;
              if SI.VBar then
              begin
                // vertical scrollbar still needed:
                Inc(LogicDonePos[3]);
                SectionListDoLogic(SI.HWidth - sbWidth, SI.VHeight);
              end;
            end;
          end;
        end
        else
        begin
          Inc(LogicDoneNeg[0]);
          if htShowVScroll in FOptions then
            // has a vertical scrollbar at all
            SectionListDoLogic(SI.HWidth - sbWidth, SI.VHeight)
          else
          begin
            // see if there is a vertical scrollbar with full width
            Inc(LogicDoneNeg[1]);
            SectionListDoLogic(SI.HWidth, SI.VHeight);
            SI2 := GetScrollInfo(ScrollWidth, FMaxVertical);
            SI.VBar := SI2.VBar;
            if SI.VBar then
            begin
              // yes, there is vertical scrollbar, allow for it
              Inc(LogicDoneNeg[2]);
              SectionListDoLogic(SI.HWidth - sbWidth, SI.VHeight);
              SI3 := GetScrollInfo(ScrollWidth, FMaxVertical);
              SI.VBar := SI3.VBar;
              if not SI.VBar then
              begin
                Inc(LogicDoneNeg[3]);
                SectionListDoLogic(SI.HWidth, SI.VHeight);
              end;
            end;
          end;
        end;
        if (FMaxVertical > 0) and not (htShowVScroll in FOptions) then
          if SI.VBar then
            Inc(VScrollBalance)
          else
            Dec(VScrollBalance);
      end;

    else
      {there is no vertical scrollbar}
      SectionListDoLogic(SI.HWidth, SI.VHeight);
    end;

    DoScrollbars;
  finally
    Exclude(FViewerState, vsDontDraw);
  end;
end;

procedure THtmlViewer.WMSize(var Message: TWMSize);
begin
  inherited;
  if vsCreating in FViewerState then
    Exit;
  if IsProcessing then
    DoScrollBars
  else
    Layout;
  if FMaxVertical < PaintPanel.Height then
    Position := 0
  else
    ScrollTo(VScrollBar.Position); {keep aligned to limits}
  with HScrollBar do
    Position := Math.Min(Position, Max - PaintPanel.Width);
end;

procedure THtmlViewer.ScrollHorz(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
{only the horizontal scrollbar comes here}
begin
  ScrollPos := Min(ScrollPos, HScrollBar.Max - PaintPanel.Width);
  PaintPanel.Invalidate;
end;

procedure THtmlViewer.ScrollTo(Y: Integer);
begin
  Y := Min(Y, FMaxVertical - PaintPanel.Height);
  Y := Max(Y, 0);
  VScrollBar.Position := Y;
  FSectionList.SetYOffset(Y);
  Invalidate;
end;

//-- BG ---------------------------------------------------------- 19.02.2011 --
// thanks to Alex at grundis.de
procedure THtmlViewer.ScrollVert(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  // BG, 18.12.2011: Added: reduce scroll position to avoid scrolling beyond end of document:
  ScrollPos := Min(ScrollPos, VScrollBar.Max - PaintPanel.Height);
  FSectionList.SetYOffset(ScrollPos);
  if vsBGFixed in FViewerState then
    PaintPanel.Invalidate
  else
  begin {scroll background into opposite direction to keep it in a fixed position in the viewport.}
    ScrollWindow(PaintPanel.Handle, 0, VScrollBar.Position - ScrollPos, nil, nil);
    PaintPanel.Update;
  end;
end;

procedure THtmlViewer.Layout;
var
  OldPos: Integer;
begin
  if IsProcessing then
    Exit;
  SetProcessing(True);
  try
    OldPos := Position;
    FSectionList.ProgressStart := 0;
    htProgressInit;
    DoLogic;
    Position := OldPos; {return to old position after width change}
  finally
    htProgressEnd;
    SetProcessing(False);
  end;
end;

function THtmlViewer.HotSpotClickHandled: Boolean;
begin
  Result := False;
  if Assigned(FOnHotSpotClick) then
    FOnHotSpotClick(Self, URL, Result);
end;

procedure THtmlViewer.TriggerUrlAction;
begin
  PostMessage(Handle, wm_UrlAction, 0, 0);
end;

procedure THtmlViewer.WMUrlAction(var Message: TMessage);
begin
  UrlAction;
end;

procedure THtmlViewer.URLAction;
var
  S, Dest: ThtString;
  OldPos: Integer;
  ft: THtmlFileType;
begin
  if not HotSpotClickHandled then
  begin
    OldPos := Position;
    SplitDest(Url, S, Dest);
    if S = '' then
    begin
      {no filename with this one}
      if PositionTo(Dest) then
        BumpHistory(FCurrentFile, FTitle, OldPos, nil, FCurrentFileType);
    end
    else
    begin
      S := HTMLExpandFileName(S);
      ft := GetFileType(S);
      case ft of

        HTMLType:
          if S <> FCurrentFile then
          begin
            LoadFromFile(S + Dest);
            AddVisitedLink(S + Dest);
          end
          else if PositionTo(Dest) then {file already loaded, change position}
            BumpHistory(FCurrentFile, FTitle, OldPos, nil, HTMLType);

        ImgType,
        TextType:
          LoadFromFile(S + Dest, ft);
      end;
    end;
    {Note: Self may not be valid here}
  end;
end;

{----------------THtmlViewer.AddVisitedLink}

procedure THtmlViewer.AddVisitedLink(const S: ThtString);
var
  I, J: Integer;
  S1, UrlTmp: ThtString;
begin
  if Assigned(FrameOwner) or (VisitedMaxCount = 0) then
    Exit; {TFrameViewer will take care of visited links}
  I := Visited.IndexOf(S);
  if I = 0 then
    Exit;
  if I < 0 then
  begin
    for J := 0 to SectionList.LinkList.Count - 1 do
      with TFontObj(SectionList.LinkList[J]) do
      begin
        UrlTmp := Url;
        if Length(UrlTmp) > 0 then
        begin
          if Url[1] = '#' then
            S1 := FCurrentFile + UrlTmp
          else
            S1 := HTMLExpandFilename(UrlTmp);
          // Yunqa.de: Except for the domain, URLs might be case sensitive!
          if S = S1 then
            Visited := True;
        end;
      end;
  end
  else
    Visited.Delete(I); {thus moving it to the top}
  Visited.Insert(0, S);
  for I := Visited.Count - 1 downto VisitedMaxCount do
    Visited.Delete(I);
end;

//-- BG ---------------------------------------------------------- 09.08.2011 --
function THtmlViewer.IsProcessing: Boolean;
begin
  Result := vsProcessing in FViewerState;
//  if Result then
//    assert(False, 'Viewer processing. Data may get lost!');
end;

{----------------THtmlViewer.CheckVisitedLinks}

procedure THtmlViewer.CheckVisitedLinks;
var
  I, J: Integer;
  S, S1: ThtString;
begin
  if VisitedMaxCount = 0 then
    Exit;
  for I := 0 to Visited.Count - 1 do
  begin
    S := Visited[I];
    for J := 0 to SectionList.LinkList.Count - 1 do
      with TFontObj(SectionList.LinkList[J]) do
      begin
        if (Url <> '') and (Url[1] = '#') then
          S1 := FCurrentFile + Url
        else
          S1 := HTMLExpandFilename(Url);
        if CompareText(S, S1) = 0 then
          Visited := True;
      end;
  end;
end;

{----------------THtmlViewer.HTMLMouseDown}

procedure THtmlViewer.HTMLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  XR, CaretHt: Integer;
  YR: Integer;
  InText: Boolean;
  Dummy: TUrlTarget;
  DummyFC: TIDObject {TImageFormControlObj};
  DummyTitle: ThtString;
begin
  inherited MouseDown(Button, Shift, X, Y);

  SetFocus;
  Exclude(FViewerState, vsHotSpotAction);
  if vsMiddleScrollOn in FViewerState then
  begin
    Exclude(FViewerState, vsMiddleScrollOn);
    Exclude(FViewerState, vsMouseScrolling);
    PaintPanel.Cursor := Cursor;
  end
  else if (Button = mbMiddle) and not (htNoWheelMouse in htOptions) then {comment this out to disable mouse middle button scrolling}
  begin
    Include(FViewerState, vsMiddleScrollOn);
    MiddleY := Y;
    PaintPanel.Cursor := UpDownCursor;
  end
  else if (Button = mbLeft) then
  begin
    Include(FViewerState, vsLeftButtonDown);
    if not (htNoLinkHilite in FOptions) or not (guUrl in GetURL(X, Y, Dummy, DummyFC, DummyTitle)) then
      Include(FViewerState, vsHiLiting);
    with FSectionList do
    begin
      Sel1 := FindCursor(PaintPanel.Canvas, X, Y + YOff, XR, YR, CaretHt, InText);
      if Sel1 > -1 then
      begin
        if (SelB <> SelE) or (ssShift in Shift) then
          InvalidateRect(PaintPanel.Handle, nil, True);
        if (ssShift in Shift) then
          if Sel1 < CaretPos then
          begin
            SelE := CaretPos;
            SelB := Sel1;
          end
          else
          begin
            SelB := CaretPos;
            SelE := Sel1;
          end
        else
        begin
          SelB := Sel1;
          SelE := Sel1;
          CaretPos := Sel1;
        end;
      end;
      LButtonDown(True); {signal to ThtDocument}
    end;
  end;
end;

procedure THtmlViewer.HTMLTimerTimer(Sender: TObject);
var
  Pt: TPoint;
begin
  if GetCursorPos(Pt) and (WindowFromPoint(Pt) <> PaintPanel.Handle) then
  begin
    SectionList.CancelActives;
    HTMLTimer.Enabled := False;
    if FURL <> '' then
    begin
      FURL := '';
      FTarget := '';
      if Assigned(FOnHotSpotCovered) then
        FOnHotSpotCovered(Self, '');
    end;
  end;
end;

function THtmlViewer.PtInObject(X, Y: Integer; var Obj: TObject): Boolean; {X, Y, are client coord} {css}
var
  IX, IY: Integer;
begin
  Result := PtInRect(ClientRect, Point(X, Y)) and
    FSectionList.PtInObject(X, Y + FSectionList.YOff, Obj, IX, IY);
end;

procedure THtmlViewer.ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Dummy: TUrlTarget;
  DummyFC: TIDObject {TImageFormControlObj};
  FormControlObj: TFormControlObj absolute Sender;
begin
  if Sender is TFormControlObj then
    //with TFormControlObj(Sender) do
    begin
      FTitleAttr := FormControlObj.Title;
      if FTitleAttr = '' then
      begin
        Dummy := nil;
        GetURL(X + FormControlObj.Left, Y + FormControlObj.Top, Dummy, DummyFC, FTitleAttr);
        if Assigned(Dummy) then
          Dummy.Free;
      end;
      inherited MouseMove(Shift, X, Y);
    end;
end;

function THtmlViewer.GetTextByIndices(AStart, ALast: Integer): UnicodeString;
var
  SaveSelB: Integer;
  SaveSelE: Integer;
begin
  if (AStart >= 0) and (ALast >= 0) and (ALast > AStart) then
    with FSectionList do
    begin
      SaveSelB := SelB;
      SaveSelE := SelE;
      SelB := Self.FindDisplayPos(AStart, False);
      SelE := Self.FindDisplayPos(ALast, False);
      Result := GetSelText;
      DisplayPosToXY(SelB, FLinkStart.X, FLinkStart.Y);
      Dec(FLinkStart.Y, VScrollBar.Position);
      SelB := SaveSelB;
      SelE := SaveSelE;
    end
  else
    Result := '';
end;

{----------------THtmlViewer.HTMLMouseMove}

procedure THtmlViewer.HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  UrlTarget: TUrlTarget;
  Url, Target: ThtString;
  FormControl: TIDObject {TImageFormControlObj};
  Obj: TObject;
  IX, IY: Integer;
  XR, CaretHt: Integer;
  YR: Integer;
  InText: Boolean;
  NextCursor: TCursor;
  guResult: guResultType;
begin
  inherited MouseMove(Shift, X, Y);

  if vsMiddleScrollOn in FViewerState then
  begin
    if not (vsMouseScrolling in FViewerState) and (Abs(Y - MiddleY) > ScrollGap) then
    begin
      Include(FViewerState, vsMouseScrolling);
      PostMessage(Handle, wm_MouseScroll, 0, 0);
    end;
    Exit;
  end;

  UrlTarget := nil;
  URL := '';
  NextCursor := crArrow;
  FTitleAttr := '';
  guResult := GetURL(X, Y, UrlTarget, FormControl, FTitleAttr);
  if guUrl in guResult then
  begin
    NextCursor := HandCursor;
    Url := UrlTarget.Url;
    Target := UrlTarget.Target;
    FLinkAttributes.Text := UrlTarget.Attr;
    FLinkText := GetTextByIndices(UrlTarget.Start, UrlTarget.Last);
    UrlTarget.Free;
  end;
  if guControl in guResult then
    NextCursor := HandCursor;
  if (Assigned(OnImageClick) or Assigned(OnImageOver)) and
    FSectionList.PtInObject(X, Y + FSectionList.YOff, Obj, IX, IY) then
  begin
    if NextCursor <> HandCursor then {in case it's also a Link}
      NextCursor := crArrow;
    if Assigned(OnImageOver) then
      OnImageOver(Self, Obj, Shift, IX, IY);
  end
  else if (FSectionList.FindCursor(PaintPanel.Canvas, X, Y + FSectionList.YOff, XR, YR, CaretHt, InText) >= 0)
    and InText and (NextCursor <> HandCursor) then
    NextCursor := Cursor;

  PaintPanel.Cursor := NextCursor;

  if ((NextCursor = HandCursor) or (SectionList.ActiveImage <> nil)) then
    HTMLTimer.Enabled := True
  else
    HTMLTimer.Enabled := False;

  if (URL <> FURL) or (Target <> FTarget) then
  begin
    FURL := URL;
    FTarget := Target;
    if Assigned(FOnHotSpotCovered) then
      FOnHotSpotCovered(Self, URL);
  end;
  if (ssLeft in Shift) and not (vsMouseScrolling in FViewerState) and ((Y <= 0) or (Y >= Self.Height)) then
  begin
    Include(FViewerState, vsMouseScrolling);
    PostMessage(Handle, wm_MouseScroll, 0, 0);
  end;
  if (ssLeft in Shift) and not NoSelect then
    DoHilite(X, Y);
end;

procedure THtmlViewer.HTMLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  UrlTarget: TUrlTarget;
  FormControl: TIDObject {TImageFormControlObj};
  Obj: TObject;
  IX, IY: Integer;
  InImage, TmpLeft: Boolean;
  Parameters: TRightClickParameters;
  AWord: UnicodeString;
  St, En: Integer;
  guResult: guResultType;
  I, ThisID: Integer;
  ParentForm: TCustomForm;
begin
  if vsMiddleScrollOn in FViewerState then
  begin
  {cancel unless it's middle button and has moved}
    if (Button <> mbMiddle) or (Y <> MiddleY) then
    begin
      Exclude(FViewerState, vsMiddleScrollOn);
      PaintPanel.Cursor := Cursor;
    end;
    Exit;
  end;

  inherited MouseUp(Button, Shift, X, Y);

  if Assigned(OnImageClick) or Assigned(OnRightClick) then
  begin
    InImage := FSectionList.PtInObject(X, Y + FSectionList.YOff, Obj, IX, IY);
    if Assigned(OnImageClick) and InImage then
      OnImageClick(Self, Obj, Button, Shift, IX, IY);
    if (Button = mbRight) and Assigned(OnRightClick) then
    begin
      Parameters := TRightClickParameters.Create;
      try
        if InImage then
        begin
          Parameters.Image := Obj as TImageObj;
          Parameters.ImageX := IX;
          Parameters.ImageY := IY;
        end;
        if guUrl in GetURL(X, Y, UrlTarget, FormControl, FTitleAttr) then
        begin
          Parameters.URL := UrlTarget.Url;
          Parameters.Target := UrlTarget.Target;
          UrlTarget.Free;
        end;
        if GetWordAtCursor(X, Y, St, En, AWord) then
          Parameters.ClickWord := AWord;
        HTMLTimer.Enabled := False;
        OnRightClick(Self, Parameters);
      finally
        HTMLTimer.Enabled := True;
        Parameters.Free;
      end;
    end;
  end;

  if (Button = mbLeft) and not (ssShift in Shift) then
  begin
    Exclude(FViewerState, vsMouseScrolling);
    DoHilite(X, Y);
    Exclude(FViewerState, vsHiliting);
    FSectionList.LButtonDown(False);
    TmpLeft := vsLeftButtonDown in FViewerState;
    Exclude(FViewerState, vsLeftButtonDown);
    if TmpLeft and (FSectionList.SelE <= FSectionList.SelB) then
    begin
      guResult := GetURL(X, Y, UrlTarget, FormControl, FTitleAttr);
      if guControl in guResult then
        TImageFormControlObj(FormControl).ImageClick(nil)
      else if guUrl in guResult then
      begin
        FURL := UrlTarget.Url;
        FTarget := UrlTarget.Target;
        FLinkAttributes.Text := UrlTarget.Attr;
        FLinkText := GetTextByIndices(UrlTarget.Start, UrlTarget.Last);
        ThisID := UrlTarget.ID;
        for I := 0 to LinkList.Count - 1 do
          with TFontObj(LinkList.Items[I]) do
            if (ThisID = UrlTarget.ID) and Assigned(TabControl) then
            begin
              ParentForm := GetParentForm(TabControl);
              if Assigned(ParentForm) and TabControl.CanFocus then
              begin
                NoJump := True; {keep doc from jumping position on mouse click}
                try
                  ParentForm.ActiveControl := TabControl;
                finally
                  NoJump := False;
                end;
              end;
              break;
            end;
        UrlTarget.Free;
        Include(FViewerState, vsHotSpotAction); {prevent Double click action}
        URLAction;
      {Note:  Self pointer may not be valid after URLAction call (TFrameViewer, HistoryMaxCount=0)}
      end;
    end;
  end;
end;

{----------------THtmlViewer.HTMLMouseWheel}

procedure THtmlViewer.HTMLMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint);
var
  Lines: Integer;
begin
  Lines := Mouse.WheelScrollLines;
  if Lines > 0 then
    if WheelDelta > 0 then
      VScrollBarPosition := VScrollBarPosition - (Lines * 16)
    else
      VScrollBarPosition := VScrollBarPosition + (Lines * 16)
  else
    VScrollBarPosition := VScrollBarPosition - WheelDelta div 2;
end;

function THtmlViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  result := inherited DoMouseWheel(shift, wheelDelta, mousePos);
  if not result and not (htNoWheelMouse in htOptions) then
  begin
    HTMLMouseWheel(Self, Shift, WheelDelta, MousePos);
    Result := True;
  end;
end;

 {----------------THtmlViewer.XYToDisplayPos}

function THtmlViewer.XYToDisplayPos(X, Y: Integer): Integer;
var
  InText: Boolean;
  XR, YR, CaretHt: Integer;
begin
  with SectionList do
    Result := FindCursor(PaintPanel.Canvas, X, Y + YOff, XR, YR, CaretHt, InText);
  if not InText then
    Result := -1;
end;

function THtmlViewer.GetBaseTarget: ThtString;
begin

end;

{----------------THtmlViewer.GetCharAtPos}

function THtmlViewer.GetCharAtPos(Pos: Integer; var Ch: WideChar;
  var Font: TFont): Boolean;
var
  Obj: TObject;
  FO: TFontObj;
  Index: Integer;
begin
  Result := FSectionList.GetChAtPos(Pos, Ch, Obj);
  if Result and (Obj is TSection) then
    with TSection(Obj) do
    begin
      FO := Fonts.GetFontObjAt(Pos - StartCurs, Index);
      Font := FO.TheFont;
    end;
end;

{----------------THtmlViewer.GetWordAtCursor}

function THtmlViewer.GetWordAtCursor(X, Y: Integer; var St, En: Integer; var AWord: UnicodeString): Boolean;
var
  XR, X1, CaretHt: Integer;
  YR, Y1: Integer;
  Obj: TObject;
  Ch: WideChar;
  InText: Boolean;
  Tmp: UnicodeString;

  function AlphaNum(Ch: WideChar): Boolean;
  begin
    case Ch of
      WideChar('a')..WideChar('z'),
      WideChar('A')..WideChar('Z'),
      WideChar('0')..WideChar('9'):
        Result := True;
    else
      Result := Ch >= #192;
    end;
  end;

  function GetCh(Pos: Integer): WideChar;
  var
    Ch: WideChar;
    Obj1: TObject;
  begin
    Result := ' ';
    if not FSectionList.GetChAtPos(Pos, Ch, Obj1) or (Obj1 <> Obj) then
      Exit;
    Result := Ch;
  end;

begin
  Result := False;
  AWord := '';
  with FSectionList do
  begin
    InText := False;
    CaretPos := FindCursor(PaintPanel.Canvas, X,
      Y + YOff, XR, YR, CaretHt, InText);
    CursorToXy(PaintPanel.Canvas, CaretPos, X1, Y1);
    if InText then {else cursor is past end of row}
    begin
      en := CaretPos;
      st := en - 1;
      if GetChAtPos(en, Ch, Obj) and AlphaNum(Ch) then
      begin
        AWord := Ch;
        Result := True;
        Inc(en);
        Ch := GetCh(en);
        while AlphaNum(Ch) do
        begin
          Tmp := Ch; {Delphi 3 needs this nonsense}
          AWord := AWord + Tmp;
          Inc(en);
          Ch := GetCh(en);
        end;
        if St >= 0 then
        begin
          Ch := GetCh(st);
          while (st >= 0) and AlphaNum(Ch) do
          begin
            System.Insert(Ch, AWord, 1);
            Dec(st);
            if St >= 0 then
              Ch := GetCh(St);
          end;
        end;
      end;
    end;
  end;
end;

{----------------THtmlViewer.HTMLMouseDblClk}

procedure THtmlViewer.HTMLMouseDblClk(Message: TWMMouse);
var
  st, en: Integer;
  AWord: UnicodeString;
begin
  FSectionList.LButtonDown(True);
  if FViewerState * [vsProcessing, vsHotSpotAction] <> [] then
    Exit;
  if not NoSelect and GetWordAtCursor(Message.XPos, Message.YPos, St, En, AWord) then
  begin
    FSectionList.SelB := st + 1;
    FSectionList.SelE := en;
    FCaretPos := st + 1;
    InvalidateRect(PaintPanel.Handle, nil, True);
  end;
  if Assigned(OnMouseDouble) then
    with Message do
      OnMouseDouble(Self, mbLeft, KeysToShiftState(Keys), XPos, YPos);
end;

procedure THtmlViewer.DoHilite(X, Y: Integer);
var
  Curs, YR, YWin: Integer;
  XR, CaretHt: Integer;
  InText: Boolean;
begin
  if (vsHiliting in FViewerState) and (Sel1 >= 0) then
    with FSectionList do
    begin
      YWin := Min(Max(0, Y), Height);
      Curs := FindCursor(PaintPanel.Canvas, X, YWin + YOff, XR, YR, CaretHt, InText);
      if (Curs >= 0) and not NoSelect then
      begin
        if Curs > Sel1 then
        begin
          SelE := Curs;
          SelB := Sel1;
        end
        else
        begin
          SelB := Curs;
          SelE := Sel1;
        end;
        InvalidateRect(PaintPanel.Handle, nil, True);
      end;
      CaretPos := Curs;
    end;
end;

{----------------THtmlViewer.WMMouseScroll}

procedure THtmlViewer.WMMouseScroll(var Message: TMessage);
const
  Ticks: DWord = 0;
var
  Pos: Integer;
  Pt: TPoint;
begin
  GetCursorPos(Pt);
  Ticks := 0;
  with VScrollBar do
  begin
    Pt := PaintPanel.ScreenToClient(Pt);
    while (vsMouseScrolling in FViewerState) and
         ((vsLeftButtonDown in FViewerState) and ((Pt.Y <= 0) or (Pt.Y > Self.Height))) or
         ((vsMiddleScrollOn in FViewerState) and (Abs(Pt.Y - MiddleY) > ScrollGap))
    do
    begin
      //BG, 23.10.2010; Issue 34: Possible to get AV in THtmlViewer.WMMouseScroll
      // What steps will reproduce the problem?
      //  1. Code enters THtmlViewer.WMMouseScroll (due to scrolling)
      //  2. That code contains a bad busy-loop and calls to Application.ProcessMessages
      //  3. The calls to Application.ProcessMessages could cause the component and fields in it to be freed.
      //  4. Next time in the loop when FSectionList.SetYOffset(Pos) is called FSectionList is nil and it crashes with an AV
      // Thus stop looping, if document (FSectionList) has been nilled:
      if FSectionList = nil then
        break;
      if GetTickCount > Ticks + 100 then
      begin
        Ticks := GetTickCount;
        if vsLeftButtonDown in FViewerState then
        begin
          if Pt.Y < -15 then
            Pos := Position - Integer(SmallChange * 8)
          else if Pt.Y <= 0 then
            Pos := Position - SmallChange
          else if Pt.Y > Self.Height + 15 then
            Pos := Position + Integer(SmallChange * 8)
          else
            Pos := Position + SmallChange;
        end
        else
        begin {MiddleScrollOn}
          Pos := Pt.Y - MiddleY;
          if Pos > 0 then
          begin
            Dec(Pos, ScrollGap - 4);
            PaintPanel.Cursor := DownOnlyCursor;
          end
          else
          begin
            Inc(Pos, ScrollGap - 4);
            PaintPanel.Cursor := UpOnlyCursor
          end;
          Pos := Position + Pos div 4;
        end;
        Pos := Math.Max(0, Math.Min(Pos, FMaxVertical - PaintPanel.Height));
        FSectionList.SetYOffset(Pos);
        SetPosition(Pos);
        DoHilite(Pt.X, Pt.Y);
        PaintPanel.Invalidate;
        GetCursorPos(Pt);
        Pt := PaintPanel.ScreenToClient(Pt);
      end;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Application.ProcessMessages;
    end;
  end;
  Exclude(FViewerState, vsMouseScrolling);
  if vsMiddleScrollOn in FViewerState then
    PaintPanel.Cursor := UpDownCursor;
end;

function THtmlViewer.PositionTo(Dest: ThtString): Boolean;
var
  I: Integer;
  Obj: TObject;
begin
  Result := False;
  if Dest = '' then
    Exit;
  if Dest[1] = '#' then
    System.Delete(Dest, 1, 1);
  I := FNameList.IndexOf(UpperCase(Dest));
  if I > -1 then
  begin
    Obj := FNameList.Objects[I];
    if (Obj is TIDObject) then
      ScrollTo(TIDObject(Obj).YPosition);

    HScrollBar.Position := 0;
    AddVisitedLink(FCurrentFile + '#' + Dest);
    Result := True;
  end;
end;

function THtmlViewer.GetURL(X, Y: Integer; var UrlTarg: TUrlTarget;
  var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType;
begin
  Result := FSectionList.GetURL(PaintPanel.Canvas, X, Y + FSectionList.YOff,
    UrlTarg, FormControl, ATitle);
end;

//-- BG ---------------------------------------------------------- 23.11.2010 --
procedure THtmlViewer.SetViewerStateBit(Index: THtmlViewerStateBit; Value: Boolean);
begin
  if Value then
    Include(FViewerState, Index)
  else
    Exclude(FViewerState, Index);
end;

procedure THtmlViewer.SetViewImages(Value: Boolean);
var
  OldPos: Integer;
  OldCursor: TCursor;
begin
  if IsProcessing then
    Exit;
  if Value <> FSectionList.ShowImages then
  begin
    OldCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      SetProcessing(True);
      FSectionList.ShowImages := Value;
      if FSectionList.Count > 0 then
      begin
        FSectionList.GetBackgroundBitmap; {load any background bitmap}
        OldPos := Position;
        DoLogic;
        Position := OldPos;
        Invalidate;
      end;
    finally
      Screen.Cursor := OldCursor;
      SetProcessing(False);
    end;
  end;
end;

{----------------THtmlViewer.InsertImage}
function THtmlViewer.InsertImage(const Src: ThtString; Stream: TStream): Boolean;
begin
  FInsertedImages.AddObject(Src, Stream);
  if not FImagesInserted.Enabled then
    FImagesInserted.Enabled := True
  else if FInsertedImages.Count >= 10 then
    ImagesInsertedTimer(nil);
  Result := True;
end;

//-- BG ---------------------------------------------------------- 10.08.2011 --
procedure THtmlViewer.ImagesInsertedTimer(Sender: TObject);
var
  OldPos: Integer;
  Reformat, ImageReformat: Boolean;
begin
  if IsProcessing then
    Exit;
    
  SetProcessing(True);
  try
    FImagesInserted.Enabled := False;
    Reformat := FImagesReformat;
    FImagesReformat := False;
    while FInsertedImages.Count > 0 do
    begin
      FSectionList.InsertImage(FInsertedImages[0], TStream(FInsertedImages.Objects[0]), ImageReformat);
      if ImageReformat then
        Reformat := True;
      FInsertedImages.Delete(0);
    end;
    FSectionList.GetBackgroundBitmap; {in case it's the one placed}
    if Reformat then
    begin
      if FSectionList.Count > 0 then
      begin
        FSectionList.GetBackgroundBitmap; {load any background bitmap}
        OldPos := Position;
        DoLogic;
        Position := OldPos;
      end;
    end;
    Invalidate;
  finally
    SetProcessing(False);
  end;
end;

procedure THtmlViewer.SetBase(Value: ThtString);
begin
  FBase := Value;
  BaseEx := Value;
end;

function THtmlViewer.GetViewImages: Boolean;
begin
  Result := FSectionList.ShowImages;
end;

procedure THtmlViewer.SetDefBackground(const Value: TColor);
begin
  if IsProcessing then
    Exit;
  inherited;
  if FSectionList <> nil then
    FSectionList.Background := Value;
  if PaintPanel <> nil then
    PaintPanel.Color := Value;
  Invalidate;
end;

procedure THtmlViewer.SetBorderStyle(Value: THTMLBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    DrawBorder;
  end;
end;

procedure THtmlViewer.KeyDown(var Key: Word; Shift: TShiftState);
var
  Pos: Integer;
  OrigPos: Integer;
  TheChange: Integer;
begin
  inherited KeyDown(Key, Shift);

  if vsMiddleScrollOn in FViewerState then
  begin
    Exclude(FViewerState, vsMiddleScrollOn);
    PaintPanel.Cursor := Cursor;
    Exit;
  end;
  with VScrollBar do
  begin
    Pos := Position;
    OrigPos := Pos;
    case Key of

      VK_PRIOR:
        if Shift = [] then
          Dec(Pos, LargeChange);

      VK_NEXT:
        if Shift = [] then
          Inc(Pos, LargeChange);

      VK_UP:
        if Shift = [] then
          Dec(Pos, SmallChange);

      VK_DOWN:
        if Shift = [] then
          Inc(Pos, SmallChange);

      VK_HOME:
        if Shift = [ssCtrl] then
          Pos := 0;

      VK_END:
        if Shift = [ssCtrl] then
          Pos := FMaxVertical;

      VK_SPACE:
        if Shift = [] then
          Inc(Pos, LargeChange)
        else if Shift = [ssShift] then
          Dec(Pos, LargeChange);
    end;
    if Pos < 0 then
      Pos := 0;
    Pos := Math.Max(0, Math.Min(Pos, FMaxVertical - PaintPanel.Height));

    if Pos <> OrigPos then
    begin
      Position := Pos;
      FSectionList.SetYOffset(Pos);

      TheChange := OrigPos - Pos;
      if not (vsBGFixed in FViewerState) and (abs(TheChange) = SmallChange) then
      begin {update only the scrolled part}
        ScrollWindow(PaintPanel.Handle, 0, TheChange, nil, nil);
        PaintPanel.Update;
      end
      else
        PaintPanel.Invalidate;
    end;
  end;

  with HScrollBar do
  begin
    Pos := Position;
    OrigPos := Pos;
    case Key of
      VK_LEFT:
        if Shift = [] then
          Dec(Pos, SmallChange);

      VK_RIGHT:
        if Shift = [] then
          Inc(Pos, SmallChange);
    end;
    if Pos < 0 then
      Pos := 0;
    Pos := Math.Min(Pos, Max - PaintPanel.Width);
    
    if Pos <> OrigPos then
    begin
      Position := Pos;
      PaintPanel.Invalidate;
    end;
  end;
end;

procedure THtmlViewer.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {else don't get the arrow keys}
end;

function THtmlViewer.GetPosition: Integer;
var
  Index: Integer;
  TopPos, Pos: Integer;
  S: TSectionBase;
begin
  Pos := Integer(VScrollBar.Position);
  S := FSectionList.FindSectionAtPosition(Pos, TopPos, Index);
  if Assigned(S) then
    Result := Integer(Index + 1) shl 16 + ((Pos - TopPos) and $FFFF)
  else
    Result := Pos;
{Hiword is section # plus 1, Loword is displacement from top of section
 HiWord = 0 is top of display}
end;

procedure THtmlViewer.SetPosition(Value: Integer);
var
  TopPos: Integer;
begin
  if HiWord(Value) = 0 then
    ScrollTo(LoWord(Value))
  else if (Hiword(Value) - 1 < FSectionList.PositionList.Count) then
  begin
    TopPos := TSectionBase(FSectionList.PositionList[HiWord(Value) - 1]).YPosition;
    ScrollTo(TopPos + LoWord(Value));
  end;
end;

function THtmlViewer.GetScrollPos: Integer;
begin
  Result := VScrollBar.Position;
end;

procedure THtmlViewer.SetScrollPos(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  Value := Min(Value, FMaxVertical - PaintPanel.Height);
  if Value <> GetScrollPos then
    ScrollTo(Value);
end;

function THtmlViewer.GetScrollBarRange: Integer;
begin
  Result := FMaxVertical - PaintPanel.Height;
end;

function THtmlViewer.GetHScrollPos: Integer;
begin
  Result := HScrollBar.Position;
end;

procedure THtmlViewer.SetHScrollPos(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  Value := Min(Value, HScrollBar.Max - PaintPanel.Width);
  HScrollbar.Position := Value;
  Invalidate;
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
function THtmlViewer.GetHistoryIndex: Integer;
begin
  Result := History.Index;
end;

function THtmlViewer.GetHScrollBarRange: Integer;
begin
  Result := HScrollBar.Max - PaintPanel.Width;
end;

function THtmlViewer.GetPalette: HPALETTE;
begin
  if ThePalette <> 0 then
    Result := ThePalette
  else
    Result := inherited GetPalette;
  Invalidate;
end;

function THtmlViewer.HTMLExpandFilename(const Filename: ThtString): ThtString;
begin
  {pass http: and other protocols except for file:///}
  if (Pos('://', Filename) > 1) and (Pos('file://', Lowercase(Filename)) = 0) then
    Result := Filename
  else
  begin
    Result := HTMLServerToDos(Filename, ServerRoot);
    if Pos('\', Result) = 1 then
      Result := ExpandFilename(Result)
    else if (Pos(':', Result) <> 2) and (Pos('\\', Result) <> 1) then
      if CompareText(FBase, 'DosPath') = 0 then {let Dos find the path}
      else if FBase <> '' then
        Result := CombineDos(HTMLToDos(FBase), Result)
      else
        Result := ExpandFilename(ExtractFilePath(HTMLToDos(FCurrentFile)) + Result);
  end;

  //BG, 19.09.2010: Issue 7: Slow UNC Lookups for Images
  //  An event allows to modify the resulting filename:
  if assigned(FOnFilenameExpanded) then
    FOnFilenameExpanded(self, Result);
end;

{----------------THtmlViewer.BumpHistory}

procedure THtmlViewer.BumpHistory(const OldFileName, OldTitle: ThtString;
  OldPos: Integer; OldFormData: TFreeList; OldDocType: ThtmlFileType);
var
  I: Integer;
  SameName: Boolean;
  HI: ThvHistoryItem;
begin
  SameName := OldFileName = FCurrentFile;
  if (HistoryMaxCount > 0) and (FCurrentFile <> '') and ((not SameName) or (FCurrentFileType <> OldDocType) or (OldPos <> Position)) then
  begin
    // update current history item
    HI := FHistory[FHistory.Index];
    if (HI <> nil) and (OldFilename <> '') then
    begin
      HI.Url := OldFilename;
      HI.Title := OldTitle;
      HI.Position := OldPos;
      HI.FileType := OldDocType;
      if not SameName then {only stored when documents changed}
        HI.FormData := OldFormData
      else
        OldFormData.Free;
      // remove items up to the top item (which is at index 0)
      for I := 0 to FHistory.Index - 1 do
        FHistory.Delete(0);
    end;

    // insert new history item
    FHistory.Index := 0;
    HI := ThvHistoryItem.Create;
    HI.Url := FCurrentFile;
    HI.Title := FTitle;
    HI.Position := Position;
    HI.FileType := FCurrentFileType;
    FHistory.Insert(0, HI);

    // delete excessive history items
    while FHistory.Count > HistoryMaxCount do
      FHistory.Delete(HistoryMaxCount);

    // notify
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
  end
  else
    OldFormData.Free;
end;

procedure THtmlViewer.SetHistoryIndex(Value: Integer);
var
  HI: ThvHistoryItem;
begin
  if IsProcessing then
    Exit;
  if (Value <> FHistory.Index) and (Value >= 0) and (Value < FHistory.Count) then
  begin
    // update current history item
    if FCurrentFile <> '' then
    begin {save the current information}
      HI := FHistory[FHistory.Index];
      HI.Url := FCurrentFile;
      HI.Title := FTitle;
      HI.Position := Position;
      HI.FileType := FCurrentFileType;
      FHistory[FHistory.GetLowestIndexOfUrl(FCurrentFile, FHistory.Index)].FormData := GetFormData;
    end;

    {reestablish the new desired history position}
    HI := FHistory[Value];
    if (FCurrentFile <> HI.Url) or (FCurrentFileType <> HI.FileType) then
      LoadFile(HI.Url, HI.FileType);
    Position := HI.Position;
    HI := FHistory[FHistory.GetLowestIndexOfUrl(HI.Url, Value)];
    SetFormData(HI.FormData); {reload the forms if any}
    HI.FormData.Free;
    HI.FormData := nil;
    FHistory.Index := Value;

    // notify
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
  end;
end;

procedure THtmlViewer.SetHistoryMaxCount(const Value: Integer);
begin
  if (Value = HistoryMaxCount) or (Value < 0) then
    Exit;
  if Value < HistoryMaxCount then
    ClearHistory;
  inherited;
end;

procedure THtmlViewer.ClearHistory;
var
  CountWas: Integer;
begin
  CountWas := FHistory.Count;
  FHistory.Clear;
  FCurrentFile := '';
  if (CountWas > 0) and Assigned(OnHistoryChange) then
    OnHistoryChange(Self);
end;

procedure THtmlViewer.SetPreFontName(const Value: TFontName);
begin
  if CompareText(Value, DefPreFontName) <> 0 then
  begin
    inherited;
    if FSectionList <> nil then
      FSectionList.PreFontName := DefPreFontname;
  end;
end;

function THtmlViewer.GetFormControlList: TFormControlObjList;
begin
  Result := FSectionList.FormControlList;
end;

function THtmlViewer.GetNameList: ThtStringList;
begin
  Result := FNameList;
end;

function THtmlViewer.GetLinkList: TList;
begin
  Result := FSectionList.LinkList;
end;

procedure THtmlViewer.SetHotSpotColor(const Value: TColor);
begin
  inherited;
  if FSectionList <> nil then
    FSectionList.HotSpotColor := Value;
end;

procedure THtmlViewer.SetVisitedColor(const Value: TColor);
begin
  inherited;
  if FSectionList <> nil then
    FSectionList.LinkVisitedColor := Value;
end;

procedure THtmlViewer.SetActiveColor(const Value: TColor);
begin
  inherited;
  if FSectionList <> nil then
    FSectionList.LinkActiveColor := Value;
end;

procedure THtmlViewer.SetVisitedMaxCount(const Value: Integer);
var
  I: Integer;
begin
  // avoid setting count < 0
  if Value < 0 then
  begin
    SetVisitedMaxCount(0);
    exit;
  end;

  // Value is >= 0 here
  if Value <> VisitedMaxCount then
  begin
    inherited;
    if Visited <> nil then
      if VisitedMaxCount = 0 then
      begin
        Visited.Clear;
        for I := 0 to SectionList.LinkList.Count - 1 do
          TFontObj(LinkList[I]).Visited := False;
        Invalidate;
      end
      else
      begin
        for I := Visited.Count - 1 downto VisitedMaxCount do
          Visited.Delete(I);
      end;
  end;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
function THtmlViewer.ShowFocusRect: Boolean;
begin
  Result := not(htNoFocusRect in htOptions);
end;

function THtmlViewer.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

//-- BG ---------------------------------------------------------- 27.12.2010 --
function THtmlViewer.GetDocumentSource: ThtString;
var
  Pos: Integer;
begin
  if FDocument <> nil then
  begin
    Pos := FDocument.Position;
    FDocument.Position := 0;
    Result := FDocument.AsString;
    FDocument.Position := Pos;
  end
  else
    Result := '';
end;

procedure THtmlViewer.SetCursor(Value: TCursor);
begin
  if Value = OldThickIBeamCursor then {no longer used}
    Value := crIBeam;
{$ifdef LCL}
  inherited setCursor(Value);
{$else}
  inherited Cursor := Value;
{$endif}
end;

function THtmlViewer.FullDisplaySize(FormatWidth: Integer): TSize;
var
  Curs: Integer;
  CopyList: ThtDocument;
begin
  Result.cx := 0; {error return}
  Result.cy := 0;
  if FormatWidth > 0 then
  begin
    CopyList := ThtDocument.CreateCopy(FSectionList);
    try
      Curs := 0;
      Result.cy := CopyList.DoLogic(PaintPanel.Canvas, 0, FormatWidth, 300, 0, Result.cx, Curs);
    finally
      CopyList.Free;
    end;
  end;
end;

procedure THtmlViewer.DoBackground1(ACanvas: TCanvas; ATop, AWidth, AHeight, FullHeight: Integer);
var
  ARect: TRect;
  Image: ThtImage;
  PRec: PtPositionRec;
  BW, BH, X, Y, X2, Y2, IW, IH, XOff, YOff: Integer;
  Fixed: Boolean;

begin
  ARect := Rect(0, 0, AWidth, AHeight);
  Image := FSectionList.BackgroundImage;
  if FSectionList.ShowImages and Assigned(Image) then
  begin
    BW := Image.Width;
    BH := Image.Height;
    PRec := FSectionList.BackgroundPRec;
    Fixed := PRec.X.Fixed;
    if Fixed then
    begin {fixed background}
      XOff := 0;
      YOff := 0;
      IW := AWidth;
      IH := AHeight;
    end
    else
    begin {scrolling background}
      XOff := 0;
      YOff := ATop;
      IW := AWidth;
      IH := FullHeight;
    end;

  {Calculate where the tiled background images go}
    CalcBackgroundLocationAndTiling(PRec, ARect, XOff, YOff, IW, IH, BW, BH, X, Y, X2, Y2);

    DrawBackground(ACanvas, ARect, X, Y, X2, Y2, Image, BW, BH, PaintPanel.Color);
  end
  else
  begin {no background image, show color only}
    DrawBackground(ACanvas, ARect, 0, 0, 0, 0, nil, 0, 0, PaintPanel.Color);
  end;
end;

procedure THtmlViewer.DoBackground2(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: Integer; AColor: TColor);

  procedure DrawBackground2(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: Integer;
    Image: ThtImage; BGColor: TColor);
  {Called by DoBackground2 (Print and PrintPreview)}
  {draw the background color and any tiled images on it}
  {ARect, the cliprect, drawing outside this will not show but images may overhang
   XStart, YStart are first image position already calculated for the cliprect and parameters.
   XLast, YLast   Tiling stops here.
  }
  var
    OldBrush: HBrush;
    OldPal: HPalette;
    DC: HDC;
    OldBack, OldFore: TColor;
  begin
    DC := ACanvas.handle;
    if DC <> 0 then
    begin
      OldPal := SelectPalette(DC, ThePalette, False);
      RealizePalette(DC);
      ACanvas.Brush.Color := BGColor or PalRelative;
      OldBrush := SelectObject(DC, ACanvas.Brush.Handle);
      OldBack := SetBkColor(DC, clWhite);
      OldFore := SetTextColor(DC, clBlack);
      try
        ACanvas.FillRect(ARect); {background color}
        if Assigned(Image) then {tile the Image}
          Image.PrintTiled(ACanvas, XStart, YStart, XLast, YLast, Image.Width, Image.Height, BGColor);
      finally
        SelectObject(DC, OldBrush);
        SelectPalette(DC, OldPal, False);
        RealizePalette(DC);
        SetBkColor(DC, OldBack);
        SetTextColor(DC, OldFore);
      end;
    end;
  end;

{called by Print and PrintPreview}
var
  ARect: TRect;
  Image: ThtImage;
  PRec: PtPositionRec;
  BW, BH, X, Y, X2, Y2, IW, IH, XOff, YOff: Integer;
  NewBitmap, NewMask: TBitmap;
  NewImage: ThtImage;
begin
  ARect := Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
  Image := FSectionList.BackgroundImage;
  if FSectionList.ShowImages and Assigned(Image) then
  begin
    BW := Image.Width;
    BH := Image.Height;
    PRec := FSectionList.BackgroundPRec;
    XOff := -ALeft;
    YOff := -ATop;
    IW := AWidth;
    IH := AHeight;

  {Calculate where the tiled background images go}
    CalcBackgroundLocationAndTiling(PRec, ARect, XOff, YOff, IW, IH, BW, BH, X, Y, X2, Y2);

    if (BW = 1) or (BH = 1) then
    begin {this is for people who try to tile 1 pixel images}
      NewMask := nil;
      NewBitmap := EnlargeImage(Image, X2 - X, Y2 - Y);
      try
        if Assigned(Image.Mask) then
          NewMask := EnlargeImage(Image.Mask, X2 - X, Y2 - Y)
        else
          NewMask := nil;
        NewImage := ThtBitmapImage.Create(NewBitmap, NewMask, LLCorner);
        // NewBitmap and NewMask will be freed with NewImage
        NewBitmap := nil;
        NewMask := nil;
        try
          DrawBackground2(ACanvas, ARect, X, Y, X2, Y2, NewImage, AColor);
        finally
          NewImage.Free;
        end;
      finally
        NewMask.Free;
        NewBitmap.Free;
      end;
    end
    else
      DrawBackground2(ACanvas, ARect, X, Y, X2, Y2, Image, AColor);
  end
  else
  begin {no background image, show color only}
    DrawBackground2(ACanvas, ARect, 0, 0, 0, 0, nil, AColor);
  end;
end;

//-- BG ---------------------------------------------------------- 20.11.2010 --
// extracted from MakeBitmap() and MakeMetaFile()
procedure THtmlViewer.Draw(Canvas: TCanvas; YTop, FormatWidth, Width, Height: Integer);
var
  CopyList: ThtDocument;
  Dummy: Integer;
  Curs: Integer;
  DocHeight: Integer;
begin
  CopyList := ThtDocument.CreateCopy(FSectionList);
  try
    Curs := 0;
    DocHeight := CopyList.DoLogic(Canvas, 0, FormatWidth, Height, 300, Dummy, Curs);
    DoBackground1(Canvas, YTop, Width, Height, DocHeight);

    CopyList.SetYOffset(Max(0, YTop));
    CopyList.Draw(Canvas, Rect(0, 0, Width, Height), MaxHScroll, 0, 0, 0, 0);
  finally
    CopyList.Free;
  end;
end;

procedure THtmlViewer.HTMLPaint(ACanvas: TCanvas; const ARect: TRect);
var
  Image: ThtImage;
  NewBitmap, NewMask: TBitmap;
  PRec: PtPositionRec;
  BW, BH, X, Y, X2, Y2, IW, IH, XOff, YOff: Integer;
  NewImage: ThtImage;
begin
  if FSectionList.Printing then
    Exit; {no background}

  Image := FSectionList.BackgroundImage;
  if FSectionList.ShowImages and Assigned(Image) then
  begin
    BW := Image.Width;
    BH := Image.Height;
    PRec := FSectionList.BackgroundPRec;
    SetViewerStateBit(vsBGFixed, PRec.X.Fixed);
    if vsBGFixed in FViewerState then
    begin {fixed background}
      XOff := 0;
      YOff := 0;
      IW := PaintPanel.ClientRect.Right;
      IH := PaintPanel.ClientRect.Bottom;
    end
    else
    begin {scrolling background}
      XOff := HScrollbar.Position;
      YOff := FSectionList.YOff;
      IW := HScrollbar.Max;
      IH := Max(MaxVertical, PaintPanel.ClientRect.Bottom);
    end;

  {Calculate where the tiled background images go}
    CalcBackgroundLocationAndTiling(PRec, ARect, XOff, YOff, IW, IH, BW, BH, X, Y, X2, Y2);

    if (BW = 1) or (BH = 1) then
    begin {this is for people who try to tile 1 pixel images}
      NewMask := nil;
      NewBitmap := EnlargeImage(Image, X2 - X, Y2 - Y); // as TBitmap;
      try
        if Assigned(Image.Mask) then
          NewMask := EnlargeImage(Image.Mask, X2 - X, Y2 - Y);

        NewImage := ThtBitmapImage.Create(NewBitmap, NewMask, LLCorner);
        NewMask := nil;
        NewBitmap := nil;
        try
          DrawBackground(ACanvas, ARect, X, Y, X2, Y2, NewImage, NewImage.Width, NewImage.Height, ACanvas.Brush.Color);
        finally
          NewImage.Free;
        end;
      finally
        NewMask.Free;
        NewBitmap.Free;
      end;
    end
    else {normal situation}
      DrawBackground(ACanvas, ARect, X, Y, X2, Y2, Image, BW, BH, ACanvas.Brush.Color);
  end
  else
  begin {no background image, show color only}
    Exclude(FViewerState, vsBGFixed);
    DrawBackground(ACanvas, ARect, 0, 0, 0, 0, nil, 0, 0, ACanvas.Brush.Color);
  end;

  FSectionList.Draw(ACanvas, ARect, MaxHScroll, -HScrollBar.Position, 0, 0, 0);
end;

function THtmlViewer.MakeBitmap(YTop, FormatWidth, Width, Height: Integer): TBitmap;
begin
  Result := nil;
  if IsProcessing or (FSectionList.Count = 0) then
    Exit;
  if Height > 4000 then
    raise EExcessiveSizeError.Create('Vertical Height exceeds 4000');
  Result := TBitmap.Create;
  try
    Result.HandleType := bmDIB;
    Result.PixelFormat := pf24Bit;
    Result.Width := Width;
    Result.Height := Height;
    Draw(Result.Canvas, YTop, FormatWidth, Width, Height);
  except
    Result.Free;
    raise;
    //Result := nil;
  end;
end;

{$ifndef NoMetafile}

function THtmlViewer.MakeMetaFile(YTop, FormatWidth, Width, Height: Integer): TMetaFile;
var
  Canvas: TMetaFileCanvas;
begin
  Result := nil;
  if IsProcessing or (FSectionList.Count = 0) then
    Exit;
  if Height > 4000 then
    raise EExcessiveSizeError.Create('Vertical Height exceeds 4000');
  Result := TMetaFile.Create;
  Result.Width := Width;
  Result.Height := Height;
  Canvas := TMetaFileCanvas.Create(Result, 0);
  try
    try
      Draw(Canvas, YTop, FormatWidth, Width, Height);
    except
      Result.Free;
      raise;
      //Result := nil;
    end;
  finally
    Canvas.Free;
  end;
end;

function THtmlViewer.MakePagedMetaFiles(Width, Height: Integer): TList;
var
  ARect, CRect: TRect;
  CopyList: ThtDocument;
  HTop, OldTop, Dummy, Curs, I: Integer;
  Done: Boolean;
  VPixels: Integer;
  Canvas: TMetaFileCanvas;
  MF: TMetaFile;
  TablePart: TablePartType;
  SavePageBottom: Integer;
  hrgnClip1: hRgn;

  procedure PaintBackground(Canvas: TCanvas; Top, Bot: Integer);
  begin
    Canvas.Brush.Color := CopyList.Background;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, Top, Width + 1, Bot));
  end;

begin
  Done := False;
  Result := nil;
  TablePartRec := nil;
  if IsProcessing or (SectionList.Count = 0) then
    Exit;
  CopyList := ThtDocument.CreateCopy(SectionList);
  try
    CopyList.NoOutput := False;
    CopyList.Printing := True;
    CopyList.LinkDrawnEvent := FOnLinkDrawn;
    Result := TList.Create;
    try
      HTop := 0;
      OldTop := 0;
      Curs := 0;
      VPixels := 0;
      ARect := Rect(0, 0, Width, Height);
      while not Done do
      begin
        MF := TMetaFile.Create;
        try
          MF.Width := Width;
          MF.Height := Height;
          Canvas := TMetaFileCanvas.Create(MF, 0);
          try
            if HTop = 0 then {DoLogic the first time only}
              VPixels := CopyList.DoLogic(Canvas, 0, Width, Height, 0, Dummy, Curs);
            CopyList.SetYOffset(HTop);
            PaintBackground(Canvas, 0, Height);

            repeat
              if Assigned(TablePartRec) then
                TablePart := TablePartRec.TablePart
              else
                TablePart := Normal;
              case TablePart of
                Normal:
                  begin
                    CopyList.Draw(Canvas, ARect, Width, 0, 0, 0, 0);
                    PaintBackground(Canvas, CopyList.PageBottom - HTop, Height + 1);
                  end;

                DoHead:
                  begin
                    CopyList.SetYOffset(TablePartRec.PartStart);
                    CRect := ARect;
                    CRect.Bottom := CRect.Top + TablePartRec.PartHeight;
                    hrgnClip1 := CreateRectRgn(0, 0, Width + 1, CRect.Bottom);
                    SelectClipRgn(Canvas.Handle, hrgnClip1);
                    DeleteObject(hrgnClip1);
                    CopyList.Draw(Canvas, CRect, Width, 0, 0, 0, 0);
                  end;

                DoBody1, DoBody3:
                  begin
                    CRect := ARect;
                    CRect.Top := CopyList.PageBottom - 1 - CopyList.YOff;
                    CopyList.SetYOffset(TablePartRec.PartStart - CRect.top);
                    CopyList.Draw(Canvas, CRect, Width, 0 + 3 * Width, 0, 0, 0); {off page}

                    TablePartRec.TablePart := DoBody2;
                    CRect.Bottom := CopyList.PageBottom - CopyList.YOff + 1;
                    hrgnClip1 := CreateRectRgn(0, CRect.Top, Width + 1, CRect.Bottom);
                    SelectClipRgn(Canvas.Handle, hrgnClip1);
                    DeleteObject(hrgnClip1);
                    CopyList.Draw(Canvas, CRect, Width, 0, 0, 0, 0); {onpage}
                    if not Assigned(TablePartRec)
                      or not (TablePartRec.TablePart in [Normal]) then
                      PaintBackground(Canvas, CopyList.PageBottom - CopyList.YOff, Height + 1);
                  end;

                DoFoot:
                  begin
                    SavePageBottom := CopyList.PageBottom;
                    CRect := ARect;
                    CRect.Top := CopyList.PageBottom - CopyList.YOff;
                    CRect.Bottom := CRect.Top + TablePartRec.PartHeight;
                    hrgnClip1 := CreateRectRgn(0, CRect.Top, Width + 1, CRect.Bottom);
                    SelectClipRgn(Canvas.Handle, hrgnClip1);
                    DeleteObject(hrgnClip1);
                    CopyList.SetYOffset(TablePartRec.PartStart - CRect.top);
                    CopyList.Draw(Canvas, CRect, Width, 0, 0, 0, 0);
                    CopyList.PageBottom := SavePageBottom;
                  end;
              end;
            until not Assigned(TablePartRec)
              or (TablePartRec.TablePart in [Normal, DoHead, DoBody3]);
          finally
            Canvas.Free;
          end;
        except
          MF.Free;
          raise;
        end;
        Result.Add(MF);
        HTop := CopyList.PageBottom;
        Inc(CopyList.LinkPage);
        Application.ProcessMessages;
        if (HTop >= VPixels) or (HTop <= OldTop) then {see if done or endless loop}
          Done := True;
        OldTop := HTop;
      end;
    except
      for I := 0 to Result.Count - 1 do
        TMetaFile(Result.Items[I]).Free;
      FreeAndNil(Result);
      raise;
    end;
  finally
    CopyList.Free;
  end;
end;

{$endif}

function THtmlViewer.CreateHeaderFooter: THtmlViewer;
begin
{
  was: Result := THtmlViewer.Create(Nil);

  If the html viewer itself is invisible and created parented this would
  break. Better create the header and footer parented to support background
  preparation.

  2008-May-26 by Arvid Winkelsdorf
}
  Result := THtmlViewer.CreateParented(HWND(HWND_MESSAGE));
  Result.Visible := False;
  Result.Parent := Parent;
  Result.DefBackground := DefBackground;
  Result.DefFontName := DefFontName;
  Result.DefFontSize := DefFontSize;
  Result.DefFontColor := DefFontColor;
  Result.CharSet := Charset;
  Result.MarginHeight := 0;

  with Result.FSectionList do
  begin
    PrintBackground := True;
    PrintTableBackground := True;
  end;
end;

//-- BG ---------------------------------------------------------- 16.11.2011 --
function THtmlViewer.CreateIFrameControl: TViewerBase;
begin
  if assigned(FOnCreateIFrameControl) then
    Result := FOnCreateIFrameControl(Self, PaintPanel)
  else
    Result := THtmlViewerClass(ClassType).CreateCopy(PaintPanel, Self);
  Result.Left := -4000; {so will be invisible until placed}
end;

{$ifndef NoMetafile}

procedure THtmlViewer.Print(FromPage, ToPage: Integer; Prn: TvwPrinter);
var
  Done: Boolean;
begin
  if assigned(OnPrinting) then
  begin
    Done := False;
    OnPrinting(self, Done);
    if Done then
      exit;
  end;
  if Prn <> nil then
    Print(Prn, FromPage, ToPage, ppMultiPrint)
  else if vwP <> nil then
    Print(vwP, FromPage, ToPage, ppMultiPrint)
  else
  begin
    Prn := TvwPrinter.Create;
    try
      Print(Prn, FromPage, ToPage, ppSinglePrint);
    finally
      Prn.Free;
    end;
  end;
  if assigned(OnPrinted) then
    OnPrinted(self);
end;

procedure THtmlViewer.OpenPrint;
begin
  if vwP = nil then
    vwP := TvwPrinter.Create;
end;

procedure THtmlViewer.ClosePrint;
begin
  if vwP <> nil then
  begin
    if vwP.Printing then
      vwP.EndDoc;
    FreeAndNil(vwP);
  end;
end;

procedure THtmlViewer.AbortPrint;
begin
  if vwP <> nil then
  begin
    if vwP.Printing then
      vwP.Abort;
    FreeAndNil(vwP);
  end;
end;
{$endif}

function THtmlViewer.NumPrinterPages: Integer;
var
  Dummy: Double;
begin
  Result := NumPrinterPages(Dummy);
end;

function THtmlViewer.NumPrinterPages(out WidthRatio: Double): Integer;
{$ifndef NoMetafile}
var
  MFPrinter: TMetaFilePrinter;
{$endif}
begin
{$ifndef NoMetafile}
  MFPrinter := TMetaFilePrinter.Create(nil);
  FOnPageEvent := nil;
  try
    PrintPreview(MFPrinter, True);
    Result := MFPrinter.LastAvailablePage;
    WidthRatio := FWidthRatio;
  finally
    MFPrinter.Free;
  end;
{$else}
  Result := 0;
{$endif}
end;

{$ifndef NoMetafile}
procedure THtmlViewer.NumPrinterPages(MFPrinter: TMetaFilePrinter; out Width, Height: Integer);
var
  LOnPageEvent: TPageEvent;
begin
  LOnPageEvent := FOnPageEvent;
  FOnPageEvent := nil;
  try
    PrintPreview(MFPrinter, False);
    Width := FPrintedSize.X;
    Height := FPrintedSize.Y;
  finally
    FOnPageEvent := LOnPageEvent;
  end;
end;

function THtmlViewer.PrintPreview(Prn: TMetaFilePrinter; NoOutput: Boolean; FromPage, ToPage: Integer): Integer;
var
  Mode: ThtPrintPreviewMode;
begin
  if NoOutput then
    Mode := ppNoOutput
  else
    Mode := ppPreview;
  Result := Print(Prn, FromPage, ToPage, Mode);
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
function THtmlViewer.Print(Prn: ThtPrinter; FromPage: Integer; ToPage: Integer; Mode: ThtPrintPreviewMode): Integer;
// extracted from Print() and PrintPreview().
var
  SavedFont: TFont;
  SavedPen: TPen;
  SavedBrush: TBrush;

  procedure SaveCanvasItems(Canvas: TCanvas);
  begin { preserve current settings of the Canvas}
    SavedPen.Assign(Canvas.Pen);
    SavedFont.Assign(Canvas.Font);
    SavedBrush.Assign(Canvas.Brush);
  end;

  procedure RestoreCanvasItems(Canvas: TCanvas);
  begin { restore initial Canvas settings }
    Canvas.Pen.Assign(SavedPen);
    Canvas.Font.Assign(SavedFont);
    Canvas.Brush.Assign(SavedBrush);
  end;

var
  XOrigin: Integer;
  MLeft: Integer;
  MLeftPrn: Integer;
  TopPixels: Integer;
  TopPixelsPrn: Integer;
  XDpi, WDpi, YDpi: Integer;
  W, H: Integer;
  HPrn, WPrn: Integer;

  procedure WhiteoutArea(Canvas: TCanvas; Y: Integer);
  {White out excess printing.  Y is top of the bottom area to be blanked.}
  var
    hrgnClip1: THandle;
  begin
    if Mode = ppNoOutput then
      exit;
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clWhite;
    Canvas.Rectangle(MLeft, 0, W + MLeft + 1, TopPixels - 2);
    Canvas.Rectangle(MLeft, Y, W + MLeft + 1, TopPixels + H + 1);
    if (htPrintBackground in FOptions) and (Y - TopPixels < H) then
    begin {need to reprint background in whited out area}
      hrgnClip1 := CreateRectRgn(MLeftPrn, Trunc(Y * YDpi / WDpi) + 2, MLeftPrn + WPrn, TopPixelsPrn + HPrn);
      SelectClipRgn(Canvas.Handle, hrgnClip1);
      DeleteObject(hrgnClip1);
      DoBackground2(Canvas, MLeft + XOrigin, TopPixels, W, H, PaintPanel.Color);
    end;
    RestoreCanvasItems(Canvas);
  end;

var
  PrintList: ThtDocument;
  VPixels: Integer;
  ScaledPgHt: Integer;
  ScaleX, ScaleY: Single;
  Done: Boolean;

  procedure DoHTMLHeaderFooter(Footer: Boolean; Event: ThtmlPagePrinted; HFViewer: THtmlViewer);
  var
    YOrigin, YOff, Ht: Integer;
    HFCopyList: ThtDocument;
    BRect: TRect;
    DocHeight, XL, XR: Integer;
    Dummy1, Dummy2: Integer;
  begin
    if not Assigned(Event) then
      Exit;
    try
      XL := MLeft;
      XR := MLeft + W;
      Event(Self, HFViewer, FPage, PrintList.PageBottom > VPixels, XL, XR, Done); {call event handler}
      HFCopyList := ThtDocument.CreateCopy(HFViewer.SectionList);
      try
        HFCopyList.Printing := True;
        HFCopyList.NoOutput := Mode = ppNoOutput;
        HFCopyList.ScaleX := ScaleX;
        HFCopyList.ScaleY := ScaleY;
        Dummy1 := 0;
        Dummy2 := 0;
        DocHeight := HFCopyList.DoLogic(Prn.Canvas, 0, XR - XL, 300, 0, Dummy1, Dummy2);
        if not Footer then
        begin {Header}
          YOrigin := 0;
          Ht := TopPixels;
          YOff := DocHeight - TopPixels;
        end
        else
        begin {Footer}
          YOrigin := -(TopPixels + H);
          Ht := Min(ScaledPgHt - (TopPixels + H), DocHeight);
          YOff := 0;
        end;
        SetWindowOrgEx(Prn.Canvas.Handle, 0, YOrigin, nil);
        HFViewer.DoBackground2(Prn.Canvas, XL, -YOff, XR - XL, DocHeight, HFViewer.PaintPanel.Color);
        HFCopyList.SetYOffset(YOff);
        BRect := Rect(XL, 0, XR, Ht);
        HFCopyList.Draw(Prn.Canvas, BRect, XR - XL, XL, 0, 0, 0);
      finally
        HFCopyList.Free;
      end;
    except
    end;
  end;

var
  ARect, CRect: TRect;
  HTop, OldTop: Integer;
  Curs: Integer;
  DC: HDC;

  Margins: TRect; { this will contain Top/Left and Bottom/Right unprintable area width}
  MRightPrn: Integer;
  MTopPrn: Integer;
  MBottomPrn: Integer;
  hrgnClip: THandle;
  hrgnClip1: THandle;
  hrgnClip2: THandle;
  Align: Integer;
  ScaledPgWid: Integer;
  FootViewer, HeadViewer: THtmlViewer;

  DeltaMarginTop: Double;
  SavePrintMarginTop: Double;
  DeltaPixelsPrn: Integer;
  DeltaPixels: Integer;
  OrigTopPixels: Integer;
  OrigTopPixelsPrn: Integer;
  OrigHprn: Integer;
  OrigH: Integer;
  LastPrintMarginTop: Double;
  SavePageBottom: Integer;
  MLeftSide: Integer;
  TablePart: TablePartType;

  HPages: Integer;
  HPageIndex: Integer;
  ScrollWidth: Integer;
begin
  if Prn = nil then
    raise EIllegalArgument.Create('No printer to print or preview to.');

  case Mode of
    ppAuto:
      if not (Prn is TMetaFilePrinter) then
        Mode := ppMultiPrint
      else
        Mode := ppPreview;

    ppPreview:
      if not (Prn is TMetaFilePrinter) then
        raise EIllegalArgument.CreateFmt('Previewing a print requires a printer based on TMetaFilePrinter but not a %s', [Prn.ClassName]);

    ppNoOutput:
      if not (Prn is TMetaFilePrinter) then
        raise EIllegalArgument.CreateFmt('Getting the total number of pages to print requires a printer based on TMetaFilePrinter but not a %s', [Prn.ClassName]);
  end;

  Result := 0;
  FPage := 0;
  Done := False;
  FootViewer := nil;
  HeadViewer := nil;
  TablePartRec := nil;
  if Assigned(FOnPageEvent) then
    FOnPageEvent(Self, 0, Done);
  if IsProcessing or (SectionList.Count = 0) then
    Exit;
  PrintList := ThtDocument.CreateCopy(SectionList);
  PrintList.SetYOffset(0);
  SavePrintMarginTop := PrintMarginTop;
  try
    SavedFont := TFont.Create;
    SavedPen := TPen.Create;
    SavedBrush := TBrush.Create;
    try
      FPage := 1;
      hrgnClip := 0;
      hrgnClip2 := 0;
      PrintList.Printing := True;
      PrintList.NoOutput := Mode = ppNoOutput;
      PrintList.SetBackground(clWhite);
      try
        if DocumentTitle <> '' then
          Prn.Title := DocumentTitle;
        if not Prn.Printing then
          Prn.BeginDoc
        else
          Prn.NewPage;
        DC := Prn.Canvas.Handle;

        YDpi := Prn.PixelsPerInchY;
        XDpi := Prn.PixelsPerInchX;
        WDpi := Round(Screen.PixelsPerInch * PrintScale);
        ScaleX := 100.0 / YDpi;
        ScaleY := 100.0 / XDpi;
        PrintList.ScaleX := ScaleX;
        PrintList.ScaleY := ScaleY;
        SetMapMode(DC, mm_AnIsotropic);
        SetWindowExtEx(DC, WDpi, WDpi, nil);
        SetViewPortExtEx(DC, XDpi, YDpi, nil);

        { calculate the amount of space that is non-printable }

        { get upper left physical offset for the printer... -> printable area <> paper size }
        Margins.Left := Prn.OffsetX;
        Margins.Top := Prn.OffsetY;

        { now that we know the TOP and LEFT offset we finally can compute the BOTTOM and RIGHT offset: }
        Margins.Right := Prn.PaperWidth - Prn.PageWidth - Margins.Left;
        { we don't want to have negative values}
        if Margins.Right < 0 then
          Margins.Right := 0; { assume no right printing offset }

        Margins.Bottom := Prn.PaperHeight - Prn.PageHeight - Margins.Top;
        { we don't want to have negative values}
        if Margins.Bottom < 0 then
          Margins.Bottom := 0; { assume no bottom printing offset }

        { which results in LowerRightPoint containing the BOTTOM and RIGHT unprintable area offset;
          using these we modify the (logical, true) borders...}

        MLeftPrn  := Trunc(PrintMarginLeft / 2.54 * XDpi) - Margins.Left;
        MRightPrn := Trunc(PrintMarginRight / 2.54 * XDpi) - Margins.Right;
        WPrn := Prn.PageWidth - (MLeftPrn + MRightPrn);

        MTopPrn    := Trunc(PrintMarginTop / 2.54 * YDpi) - Margins.Top;
        MBottomPrn := Trunc(PrintMarginBottom / 2.54 * YDpi) - Margins.Bottom;
        HPrn := Prn.PageHeight - (MTopPrn + MBottomPrn);

        MLeft := Trunc(MLeftPrn * WDpi / XDpi);
        W := Trunc(WPrn * WDpi / XDpi); {scaled pageWidth}
        H := Trunc(HPrn * WDpi / YDpi); {scaled pageHeight}
        ScaledPgWid := Trunc(Prn.PageWidth * WDpi / XDpi);
        ScaledPgHt := Trunc(Prn.PageHeight * WDpi / YDpi);

        TopPixelsPrn := MTopPrn;
        TopPixels := Trunc(TopPixelsPrn * WDpi / YDpi);
        HTop := 0;
        OldTop := 0;
        Curs := 0;
        VPixels := PrintList.DoLogic(Prn.Canvas, 0, W, H, 0, ScrollWidth, Curs);
        if Mode in [ppNoOutput, ppPreview] then
        begin
          FPrintedSize.X := ScrollWidth;
          FPrintedSize.Y := VPixels;
          FWidthRatio := ScrollWidth / W;
          if FWidthRatio > 1.0 then
            FWidthRatio := Round(WDpi * FWidthRatio + 0.5) / WDpi;
        end;

        {This one is primarily used to clip the top and bottom margins to insure
         nothing is output there.  It's also constrained to the print region
         in case the margins are misadjusted.}
        hrgnClip := CreateRectRgn(MLeftPrn, Max(0, TopPixelsPrn),
          Min(Prn.PageWidth, WPrn + MLeftPrn + 2),
          Min(Prn.PageHeight, TopPixelsPrn + HPrn));
        {This one clips to the allowable print region so that the preview is limited to that region also}
        hrgnClip2 := CreateRectRgn(0, 0, Prn.PageWidth, Prn.PageHeight);

        Application.ProcessMessages;
        if Assigned(FOnPageEvent) then
          FOnPageEvent(Self, FPage, Done);
        ARect := Rect(MLeft, TopPixels, W + MLeft, TopPixels + H);

        if Assigned(FOnPrintHTMLHeader) then
          HeadViewer := CreateHeaderFooter;
        if Assigned(FOnPrintHTMLFooter) then
          FootViewer := CreateHeaderFooter;

        OrigTopPixels := TopPixels;
        OrigTopPixelsPrn := TopPixelsPrn;
        OrigHPrn := HPrn;
        OrigH := H;
        LastPrintMarginTop := PrintMarginTop;

        //BG, 05.03.2006
        HPages := ceil(ScrollWidth / W);
        if HPages > PrintMaxHPages then
          HPages := PrintMaxHPages;
        XOrigin := 0;
        while (FPage <= ToPage) and not Done do
        begin
          PrintList.SetYOffset(HTop - TopPixels);

          for HPageIndex := 0 to HPages - 1 do
          begin
            XOrigin := HPageIndex * W;

            DC := Prn.Canvas.Handle; // the canvas may change with each new page
            SaveCanvasItems(Prn.Canvas);
            SetMapMode(DC, mm_AnIsotropic);
            SetWindowExtEx(DC, WDpi, WDpi, nil);
            SetViewPortExtEx(DC, XDpi, YDpi, nil);
            SetWindowOrgEx(DC, XOrigin, 0, nil);
            SelectClipRgn(DC, hrgnClip);

            if FPage >= FromPage then
            begin
              if (htPrintBackground in FOptions) then
                DoBackground2(Prn.Canvas, MLeft + XOrigin, TopPixels, W, H, PaintPanel.Color);
              MLeftSide := MLeft;
            end
            else
              MLeftSide := MLeft + 3 * W; {to print off page}

            repeat
              if Assigned(TablePartRec) then
                TablePart := TablePartRec.TablePart
              else
                TablePart := Normal;
              case TablePart of
                Normal:
                  begin
                    PrintList.Draw(Prn.Canvas, ARect, W, MLeftSide, 0, 0, 0);
                    WhiteoutArea(Prn.Canvas, PrintList.PageBottom - PrintList.YOff);
                  end;

                DoHead:
                  begin
                    PrintList.SetYOffset(TablePartRec.PartStart - TopPixels);
                    CRect := ARect;
                    CRect.Bottom := CRect.Top + TablePartRec.PartHeight;
                    hrgnClip1 := CreateRectRgn(MLeftPrn, TopPixelsPrn, WPrn + MLeftPrn + 2,
                      MulDiv(CRect.Bottom, YDpi, WDpi));
                    SelectClipRgn(DC, hrgnClip1);
                    DeleteObject(hrgnClip1);
                    PrintList.Draw(Prn.Canvas, CRect, W, MLeftSide, 0, 0, 0);
                  end;

                DoBody1, DoBody3:
                  begin
                    CRect := ARect;
                    CRect.Top := PrintList.PageBottom - 1 - PrintList.YOff;
                    PrintList.SetYOffset(TablePartRec.PartStart - CRect.top);
                    PrintList.Draw(Prn.Canvas, CRect, W, MLeftSide + 3 * W, 0, 0, 0); {off page}

                    TablePartRec.TablePart := DoBody2;
                    CRect.Bottom := PrintList.PageBottom - PrintList.YOff + 1;
                    hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(CRect.Top, YDpi, WDpi),
                      WPrn + MLeftPrn + 2, MulDiv(CRect.Bottom, YDpi, WDpi));
                    SelectClipRgn(DC, hrgnClip1);
                    DeleteObject(hrgnClip1);
                    PrintList.Draw(Prn.Canvas, CRect, W, MLeftSide, 0, 0, 0); {onpage}
                    if not Assigned(TablePartRec) or not (TablePartRec.TablePart in [Normal]) then
                      WhiteoutArea(Prn.Canvas, PrintList.PageBottom - PrintList.YOff);
                  end;

                DoFoot:
                  begin
                    SavePageBottom := PrintList.PageBottom;
                    CRect := ARect;
                    CRect.Top := PrintList.PageBottom - PrintList.YOff;
                    CRect.Bottom := CRect.Top + TablePartRec.PartHeight;
                    hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(CRect.Top, YDpi, WDpi),
                      WPrn + MLeftPrn + 2, MulDiv(CRect.Bottom, YDpi, WDpi));
                    SelectClipRgn(DC, hrgnClip1);
                    DeleteObject(hrgnClip1);
                    PrintList.SetYOffset(TablePartRec.PartStart - CRect.top);
                    PrintList.Draw(Prn.Canvas, CRect, W, MLeftSide, 0, 0, 0);
                    PrintList.PageBottom := SavePageBottom;
                  end;
              end;
            until not Assigned(TablePartRec) or (TablePartRec.TablePart in [Normal, DoHead, DoBody3]);

{.$Region 'HeaderFooter'}
            SelectClipRgn(DC, 0);
            if (FPage <= ToPage) then {print header and footer}
            begin
              Prn.Canvas.Pen.Assign(savedPen);
              Align := SetTextAlign(DC, TA_Top or TA_Left or TA_NOUPDATECP);
              SelectClipRgn(DC, hrgnClip2);

              if Assigned(OnPrintHeader) then
              begin
                SetWindowOrgEx(DC, XOrigin, 0, nil);
                OnPrintHeader(Self, Prn.Canvas, FPage, ScaledPgWid, TopPixels, Done);
              end;

              if Assigned(OnPrintFooter) then
              begin
                SetWindowOrgEx(DC, XOrigin, -(TopPixels + H), nil);
                OnPrintFooter(Self, Prn.Canvas, FPage, ScaledPgWid, ScaledPgHt - (TopPixels + H), Done);
              end;
              DoHTMLHeaderFooter(False, FOnPrintHTMLHeader, HeadViewer);
              DoHTMLHeaderFooter(True, FOnPrintHTMLFooter, FootViewer);
              SetTextAlign(DC, Align);
              SelectClipRgn(DC, 0);
            end;

            if PrintMarginTop <> LastPrintMarginTop then
            begin
              DeltaMarginTop := PrintMarginTop - SavePrintMarginTop;
              DeltaPixelsPrn := Trunc(DeltaMarginTop / 2.54 * YDpi);
              DeltaPixels := Trunc(DeltaMarginTop / 2.54 * WDpi);
              TopPixels := OrigTopPixels + DeltaPixels;
              TopPixelsPrn := OrigTopPixelsPrn + DeltaPixelsPrn;
              HPrn := OrigHprn - DeltaPixelsPrn;
              H := OrigH - DeltaPixels;
              ARect := Rect(MLeft + XOrigin, TopPixels, W + MLeft + XOrigin, TopPixels + H);
              hrgnClip := CreateRectRgn(MLeftPrn, Max(0, TopPixelsPrn),
                Min(Prn.PageWidth, WPrn + MLeftPrn + 2),
                Min(Prn.PageHeight, TopPixelsPrn + HPrn));
              LastPrintMarginTop := PrintMarginTop;
            end;
{.$EndRegion}
            if HPageIndex < HPages - 1 then
            begin
              Prn.NewPage;
              Inc(FPage);
            end;
          end;
          HTop := PrintList.PageBottom;
          Application.ProcessMessages;
          if Assigned(FOnPageEvent) then
            FOnPageEvent(Self, FPage, Done);
          if (HTop >= VPixels - MarginHeight) or (HTop <= OldTop) then {see if done or endless loop}
            Done := True;
          OldTop := HTop;
          if not Done and (FPage >= FromPage) and (FPage < ToPage) then
            // Pages before FromPage aren't printed. Tus no new page required.
            Prn.NewPage;
          // Increment page number no matter if we start a new page at the printer, as it does not count
          // the number of sheets printed, but is the number of the currently processed document page.
          Inc(FPage);
        end;
      finally
        FreeAndNil(HeadViewer);
        FreeAndNil(FootViewer);
        if hRgnClip <> 0 then
          DeleteObject(hrgnClip);
        if hRgnClip2 <> 0 then
          DeleteObject(hrgnClip2);
        case Mode of
          ppNoOutput,
          ppPreview:
            Prn.EndDoc;

          ppSinglePrint:
            if FPage < FromPage then
              Prn.Abort
            else
              Prn.EndDoc;

          ppMultiPrint:
            // User must call Prn.EndDoc after printing all pages
            ;
        end;
        Dec(FPage);
      end;
    finally
      SavedFont.Free;
      SavedPen.Free;
      SavedBrush.Free;
    end;
  finally
    PrintMarginTop := SavePrintMarginTop;
    PrintList.Free;
  end;
  Result := FPage;
end;
{$endif}

procedure THtmlViewer.BackgroundChange(Sender: TObject);
begin
  PaintPanel.Color := (Sender as ThtDocument).Background or PalRelative;
end;

procedure THtmlViewer.SetOnBitmapRequest(const Handler: TGetBitmapEvent);
begin
  inherited;
  FSectionList.GetBitmap := Handler;
end;

//-- BG ---------------------------------------------------------- 20.11.2010 --
function THtmlViewer.GetOnExpandName: TExpandNameEvent;
begin
  Result := FSectionList.ExpandName;
end;

procedure THtmlViewer.SetOnExpandName(Handler: TExpandNameEvent);
begin
  FSectionList.ExpandName := Handler;
end;

//-- BG ---------------------------------------------------------- 20.11.2010 --
function THtmlViewer.GetOnFileBrowse: TFileBrowseEvent;
begin
  Result := FSectionList.FileBrowse;
end;

procedure THtmlViewer.SetOnFileBrowse(Handler: TFileBrowseEvent);
begin
  FSectionList.FileBrowse := Handler;
end;

procedure THtmlViewer.SetOnImageRequest(const Handler: TGetImageEvent);
begin
  inherited;
  FSectionList.GetImage := Handler;
end;

procedure THtmlViewer.SetOnImageRequested(const Handler: TGottenImageEvent);
begin
  inherited;
  FSectionList.GottenImage := Handler;
end;

procedure THtmlViewer.SetOnObjectBlur(const Handler: ThtObjectEvent);
begin
  inherited;
  FSectionList.ObjectBlur := Handler;
end;

procedure THtmlViewer.SetOnObjectChange(const Handler: ThtObjectEvent);
begin
  inherited;
  FSectionList.ObjectChange := Handler;
end;

procedure THtmlViewer.SetOnObjectClick(const Handler: TObjectClickEvent);
begin
  inherited;
  FSectionList.ObjectClick := Handler;
end;

procedure THtmlViewer.SetOnObjectFocus(const Handler: ThtObjectEvent);
begin
  inherited;
  FSectionList.ObjectFocus := Handler;
end;

procedure THtmlViewer.SetOnPanelCreate(const Handler: TPanelCreateEvent);
begin
  inherited;
  FSectionList.PanelCreateEvent := Handler;
end;

procedure THtmlViewer.SetOnPanelDestroy(const Handler: TPanelDestroyEvent);
begin
  inherited;
  FSectionList.PanelDestroyEvent := Handler;
end;

procedure THtmlViewer.SetOnPanelPrint(const Handler: TPanelPrintEvent);
begin
  inherited;
  FSectionList.PanelPrintEvent := Handler;
end;

procedure THtmlViewer.SetOnScript(const Handler: TScriptEvent);
begin
  inherited;
  FSectionList.ScriptEvent := Handler;
end;

procedure THtmlViewer.SetOnFormSubmit(Handler: TFormSubmitEvent);
begin
  FOnFormSubmit := Handler;
  if Assigned(Handler) then
    FSectionList.SubmitForm := SubmitForm
  else
    FSectionList.SubmitForm := nil;
end;

procedure THtmlViewer.SubmitForm(Sender: TObject; const Action, Target, EncType, Method: ThtString;
  Results: ThtStringList);
begin
  if Assigned(FOnFormSubmit) then
  begin
    FAction := Action;
    FMethod := Method;
    FFormTarget := Target;
    FEncType := EncType;
    FStringList := Results;
    PostMessage(Handle, wm_FormSubmit, 0, 0);
  end
  //BG, 03.01.2010: memory leak:
  else
    Results.Free;
end;

procedure THtmlViewer.WMFormSubmit(var Message: TMessage);
begin
  FOnFormSubmit(Self, FAction, FFormTarget, FEncType, FMethod, FStringList);
end; {user disposes of the ThtStringList}

function THtmlViewer.Find(const S: UnicodeString; MatchCase: Boolean): Boolean;
begin
  Result := FindEx(S, MatchCase, False);
end;

function THtmlViewer.FindEx(const S: UnicodeString; MatchCase, Reverse: Boolean): Boolean;
var
  Curs: Integer;
  X: Integer;
  Y, Pos: Integer;
  S1: UnicodeString;
begin
  Result := False;
  if S = '' then
    Exit;
  with FSectionList do
  begin
    if MatchCase then
      S1 := S
    else
      S1 := htLowerCase(S);
    if Reverse then
      Curs := FindStringR(CaretPos, S1, MatchCase)
    else
      Curs := FindString(CaretPos, S1, MatchCase);
    if Curs >= 0 then
    begin
      Result := True;
      SelB := Curs;
      SelE := Curs + Length(S);
      if Reverse then
        CaretPos := SelB
      else
        CaretPos := SelE;
      if CursorToXY(PaintPanel.Canvas, Curs, X, Y) then
      begin
        Pos := VScrollBarPosition;
        if (Y < Pos) or
          (Y > Pos + ClientHeight - 20) then
          VScrollBarPosition := (Y - ClientHeight div 2);

        Pos := HScrollBarPosition;
        if (X < Pos) or
          (X > Pos + ClientWidth - 50) then
          HScrollBarPosition := (X - ClientWidth div 2);
        Invalidate;
      end;
    end;
  end;
end;

procedure THtmlViewer.FormControlEnterEvent(Sender: TObject);
var
  Y, Pos: Integer;
begin
  if Sender is TFormControlObj then
  begin
    Y := TFormControlObj(Sender).YValue;
    Pos := VScrollBarPosition;
    if (Y < Pos) or (Y > Pos + ClientHeight - 20) then
    begin
      VScrollBarPosition := (Y - ClientHeight div 2);
      Invalidate;
    end;
  end
  else if Sender is TFontObj and not NoJump then
  begin
    Y := TFontObj(Sender).YValue;
    Pos := VScrollBarPosition;
    if (Y < Pos) then
      VScrollBarPosition := Y
    else if (Y > Pos + ClientHeight - 30) then
      VScrollBarPosition := (Y - ClientHeight div 2);
    Invalidate;
  end
end;

procedure THtmlViewer.SelectAll;
begin
  with FSectionList do
    if (Count > 0) and not NoSelect then
    begin
      SelB := 0;
      with TSectionBase(Items[Count - 1]) do
        SelE := StartCurs + Len;
      Invalidate;
    end;
end;

{----------------THtmlViewer.InitLoad}

procedure THtmlViewer.InitLoad;
begin
  if not Assigned(FSectionList.ImageCache) then
  begin
    FSectionList.ImageCache := ThtImageCache.Create;
    FSectionList.ImageCache.Sorted := True;
    FSectionList.ImageCache.SetCacheCount(ImageCacheCount);
    Include(FViewerState, vsLocalImageCache);
  end;
  FSectionList.Clear;
  FSectionList.UseQuirksMode := FUseQuirksMode;
  UpdateImageCache;
  FSectionList.SetFonts(
    DefFontName, DefPreFontName, DefFontSize, DefFontColor,
    DefHotSpotColor, DefVisitedLinkColor, DefOverLinkColor, DefBackground,
    htOverLinksActive in FOptions, not (htNoLinkUnderline in FOptions),
    CharSet, MarginHeight, MarginWidth);
end;

{----------------THtmlViewer.Clear}

procedure THtmlViewer.Clear;
{Note: because of Frames do not clear history list here}
begin
  if IsProcessing then
    Exit;
  HTMLTimer.Enabled := False;
  FSectionList.UseQuirksMode := FUseQuirksMode;
  FSectionList.Clear;
  if vsLocalImageCache in FViewerState then
    FSectionList.ImageCache.Clear;

  FSectionList.SetFonts(
    DefFontName, DefPreFontName, DefFontSize, DefFontColor,
    DefHotSpotColor, DefVisitedLinkColor, DefOverLinkColor, DefBackground,
    htOverLinksActive in FOptions, not (htNoLinkUnderline in FOptions),
    CharSet, MarginHeight, MarginWidth);
  FBase := '';
  FBaseEx := '';
  FBaseTarget := '';
  FTitle := '';
  VScrollBar.Visible := False;
  HScrollBar.Visible := False;
  CaretPos := 0;
  Sel1 := -1;
  if Assigned(OnSoundRequest) then
    OnSoundRequest(Self, '', 0, True);
  Invalidate;
end;

procedure THtmlViewer.PaintWindow(DC: HDC);
begin
  PaintPanel.RePaint;
  BorderPanel.RePaint;
  VScrollbar.RePaint;
  HScrollbar.RePaint;
end;

procedure THtmlViewer.CopyToClipboard;
const
  // about clipboard format: http://msdn.microsoft.com/en-us/library/aa767917%28v=vs.85%29.aspx
  StartFrag: ThtString = '<!--StartFragment-->';
  EndFrag: ThtString = '<!--EndFragment-->';
  DocType: ThtString = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">'#13#10;
var
  Leng: Integer;
  StSrc, EnSrc: Integer;
  HTML: ThtString;
  CF_HTML: UINT;

  procedure copyFormatToClipBoard(const Source: ThtString);
  // Put SOURCE on the clipboard, using FORMAT as the clipboard format
{$ifdef LCL}
  var
    Utf8: UTF8String;
  begin
    Clipboard.Clear;
    Clipboard.AddFormat(CF_UNICODETEXT, PWideChar(Source)[0], Length(Source) * sizeof(WIDECHAR));
    Utf8 := UTF8Encode(Source);
    Clipboard.AddFormat(CF_HTML, Utf8, Length(Utf8));
  end;
{$else}
  var
    Len: Integer;
    Mem: HGLOBAL;
    Buf: PAnsiChar;
    Wuf: PWideChar;
    L: Integer;
  begin
    Clipboard.Clear;
    Len := Length(Source);
    Mem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, Len * 4 + 1);
    try
      Buf := GlobalLock(Mem);
      try
        L := WideCharToMultiByte(CP_UTF8, 0, PWideChar(Source), Length(Source), Buf, Len, nil, nil);
        Buf[L] := #0;
        Clipboard.SetAsHandle(CF_HTML, Mem);
      finally
        GlobalUnlock(Mem);
      end;
    except
      GlobalFree(Mem);
    end;
    Mem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, (Len + 1) * SizeOf(ThtChar));
    try
      Wuf := GlobalLock(Mem);
      try
        Move(Source[1], Wuf^, Len * SizeOf(ThtChar));
        Wuf[Len] := #0;
        Clipboard.SetAsHandle(CF_UNICODETEXT, Mem);
      finally
        GlobalUnlock(Mem);
      end;
    except
      GlobalFree(Mem);
    end;
  end;
{$endif}

  function GetHeader(const HTML: ThtString): ThtString;
  const
    Version = 'Version:1.0'#13#10;
    StartHTML = 'StartHTML:';
    EndHTML = 'EndHTML:';
    StartFragment = 'StartFragment:';
    EndFragment = 'EndFragment:';
    SourceURL = 'SourceURL:';
    NumberLengthAndCR = 10;

    // Let the compiler determine the description length.
    PreliminaryLength = Length(Version) + Length(StartHTML) +
      Length(EndHTML) + Length(StartFragment) +
      Length(EndFragment) + 4 * NumberLengthAndCR +
      2; {2 for last CRLF}
  var
    URLString: ThtString;
    StartHTMLIndex,
      EndHTMLIndex,
      StartFragmentIndex,
      EndFragmentIndex: Integer;
  begin
    if CurrentFile = '' then
      UrlString := SourceURL + 'unsaved:///THtmlViewer.htm'
    else if Pos('://', CurrentFile) > 0 then
      URLString := SourceURL + CurrentFile {already has protocol}
    else
      URLString := SourceURL + 'file://' + CurrentFile;
    StartHTMLIndex := PreliminaryLength + Length(URLString);
    EndHTMLIndex := StartHTMLIndex + Length(HTML);
    StartFragmentIndex := StartHTMLIndex + Pos(StartFrag, HTML) + Length(StartFrag) - 1;
    EndFragmentIndex := StartHTMLIndex + Pos(EndFrag, HTML) - 1;

    Result := Version +
      SysUtils.Format('%s%.8d', [StartHTML, StartHTMLIndex]) + #13#10 +
      SysUtils.Format('%s%.8d', [EndHTML, EndHTMLIndex]) + #13#10 +
      SysUtils.Format('%s%.8d', [StartFragment, StartFragmentIndex]) + #13#10 +
      SysUtils.Format('%s%.8d', [EndFragment, EndFragmentIndex]) + #13#10 +
      URLString + #13#10;
  end;

  procedure RemoveTag(const Tag: ThtString);
  {remove all the tags that look like "<tag .....>" }
  var
    I, J: Integer;
    LTML: ThtString;
    C: ThtChar;
  begin
    C := #0; // valium for Delphi 2009+
    LTML := htLowerCase(HTML);
    repeat
      I := Pos(Tag, LTML);
      if I = 0 then
        break;
      J := I - 1 + Length(Tag);
      repeat
        Inc(J);
        if J = Length(LTML) then
          break;
        C := LTML[J];
      until (C = GreaterChar) or (C = EofChar);
      Delete(HTML, I, J - I + 1);
      Delete(LTML, I, J - I + 1);
    until False;
  end;

  procedure MessUp(const S: ThtString);
  var
    I: Integer;
    L: ThtString;
  begin
    L := htLowerCase(HTML);
    I := Pos(S, L);
    while (I > 0) do
    begin
      Delete(HTML, I, 1);
      L := htLowerCase(HTML);
      I := Pos(S, L);
    end;
  end;

  procedure InsertDefaultFontInfo;
  var
    I: Integer;
    S, L: ThtString;
    HeadFound: Boolean;
  begin
    L := LowerCase(HTML);
    I := Pos('<head>', L);
    HeadFound := I > 0;
    if not HeadFound then
      I := Pos('<html>', L);
    if I <= 0 then
      I := 1;
    S := '<style> body {font-size: ' + IntToStr(DefFontSize) + 'pt; font-family: "' +
      DefFontName + '"; }</style>';
    if not HeadFound then
      S := '<head>' + S + '</head>';
    Insert(S, HTML, I);
  end;

  function BackupToContent: ThtString;
  var
    C: ThtChar;
    I: Integer;

    procedure GetC; {reads characters backwards}
    begin
      if I - 1 > StSrc then
      begin
        Dec(I);
        C := HTML[I];
      end
      else
        C := #0;
    end;

  begin
    I := EnSrc;
    repeat
      repeat {skip past white space}
        GetC;
      until (C > ' ') or (C = EofChar);
      if C = '>' then
        repeat {read thru a tag}
          repeat
            GetC;
          until (C = LessChar) or (C = EofChar);
          GetC;
        until C <> '>';
    until (C > ' ') or (C = EofChar); {until found some content}
    if C = EofChar then
      Dec(I);
    Result := Copy(HTML, 1, I); {truncate the tags}
  end;

begin
  Leng := FSectionList.GetSelLength;
  if Leng = 0 then
    Exit;
  FSectionList.CopyToClipboardA(Leng + 1);

  HTML := DocumentSource;
  StSrc := FindSourcePos(FSectionList.SelB) + 1;
  EnSrc := FindSourcePos(FSectionList.SelE);
  if EnSrc < 0 then {check to see if end selection is at end of document}
  begin
    EnSrc := Length(HTML);
    if HTML[EnSrc] = '>' then
    begin
      HTML := HTML + ' ';
      Inc(EnSrc);
    end;
  end
  else
    EnSrc := EnSrc + 1;
{Truncate beyond EnSrc}
  HTML := Copy(HTML, 1, EnSrc - 1);
{Also remove any tags on the end}
  HTML := BackupToContent;
{insert the StartFrag ThtString}
  Insert(StartFrag, HTML, StSrc);
{Remove all Meta tags, in particular the ones that specify language, but others
 seem to cause problems also}
  RemoveTag('<meta');
{Remove <!doctype> in preparation to having one added}
  RemoveTag('<!doctype');
{page-break-... stylesheet properties cause a hang in Word -- mess them up}
  MessUp('page-break-');
{Add in default font information which wouldn't be in the HTML}
  InsertDefaultFontInfo;
{Add Doctype tag at start and append the EndFrag ThtString}
  HTML := DocType + HTML + EndFrag;
{Add the header to start}
  HTML := GetHeader(HTML) + HTML;

  CF_HTML := RegisterClipboardFormat('HTML Format'); {not sure this is necessary}
  CopyFormatToClipBoard(HTML);
end;

function THtmlViewer.GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
begin
  if BufSize <= 0 then
    Result := 0
  else
    Result := FSectionList.GetSelTextBuf(Buffer, BufSize);
end;

function THtmlViewer.GetSelText: UnicodeString;
var
  Len: Integer;
begin
  Len := FSectionList.GetSelLength;
  if Len > 0 then
  begin
    SetString(Result, nil, Len);
    FSectionList.GetSelTextBuf(Pointer(Result), Len + 1);
  end
  else
    Result := '';
end;

function THtmlViewer.GetSelLength: Integer;
begin
  with FSectionList do
    if FCaretPos = SelB then
      Result := SelE - SelB
    else
      Result := SelB - SelE;
end;

procedure THtmlViewer.SetSelLength(Value: Integer);
begin
  with FSectionList do
  begin
    if Value >= 0 then
    begin
      SelB := FCaretPos;
      SelE := FCaretPos + Value;
    end
    else
    begin
      SelE := FCaretPos;
      SelB := FCaretPos + Value;
    end;
    Invalidate;
  end;
end;

procedure THtmlViewer.SetSelStart(Value: Integer);
begin
  with FSectionList do
  begin
    FCaretPos := Value;
    SelB := Value;
    SelE := Value;
    Invalidate;
  end;
end;

procedure THtmlViewer.SetNoSelect(const Value: Boolean);
begin
  if Value <> NoSelect then
  begin
    inherited;
    if Value = True then
    begin
      FSectionList.SelB := -1;
      FSectionList.SelE := -1;
      Invalidate; //RePaint;
    end;
  end;
end;

procedure THtmlViewer.UpdateImageCache;
begin
  FSectionList.ImageCache.BumpAndCheck;
end;

procedure THtmlViewer.SetImageCacheCount(const Value: Integer);
begin
  if Value < 0 then
  begin
    SetImageCacheCount(0);
    exit;
  end;
  if Value > 20 then
  begin
    SetImageCacheCount(20);
    exit;
  end;

  // at this point Value is between 0 and 20 incl.
  if Value <> ImageCacheCount then
  begin
    inherited;
    if Assigned(FSectionList) and Assigned(FSectionList.ImageCache) then
      FSectionList.ImageCache.SetCacheCount(ImageCacheCount);
  end;
end;

procedure THtmlViewer.SetImageCache(ImageCache: ThtImageCache);
begin
  FSectionList.ImageCache := ImageCache;
  Exclude(FViewerState, vsLocalImageCache);
end;

procedure THtmlViewer.DrawBorder;
begin
  if (Focused and (FBorderStyle = htFocused)) or (FBorderStyle = htSingle)
    or (csDesigning in ComponentState)
  then
  begin
//LCL sets BorderStyle no matter if it changes or not, which ends up in an endless window recreation.
{$ifdef LCL}
    if BorderPanel.BorderStyle <> bsSingle then
{$endif}
    BorderPanel.BorderStyle := bsSingle;
  end
  else
  begin
{$ifdef LCL}
    if BorderPanel.BorderStyle <> bsNone then
{$endif}
    BorderPanel.BorderStyle := bsNone;
  end;
end;

procedure THtmlViewer.DoEnter;
begin
  inherited DoEnter;
  DrawBorder;
end;

procedure THtmlViewer.DoExit;
begin
  inherited DoExit;
  DrawBorder;
end;

procedure THtmlViewer.SetScrollBars(Value: TScrollStyle);
begin
  if (Value <> FScrollBars) then
  begin
    FScrollBars := Value;
    if not (csLoading in ComponentState) and HandleAllocated then
    begin
      SetProcessing(True);
      try
        DoLogic;
      finally
        SetProcessing(False);
      end;
      Invalidate;
    end;
  end;
end;

{----------------THtmlViewer.Reload}

procedure THtmlViewer.Reload; {reload the last file}
var
  Pos: Integer;
begin
  if FCurrentFile <> '' then
  begin
    Pos := Position;
    LoadFromFile(FCurrentFile, FCurrentFileType);
    Position := Pos;
  end;
end;

{----------------THtmlViewer.GetOurPalette:}

function THtmlViewer.GetOurPalette: HPalette;
begin
  if ColorBits = 8 then
    Result := CopyPalette(ThePalette)
  else
    Result := 0;
end;

{----------------THtmlViewer.SetOurPalette}

procedure THtmlViewer.SetOurPalette(Value: HPalette);
var
  NewPalette: HPalette;
begin
  if (Value <> 0) and (ColorBits = 8) then
  begin
    NewPalette := CopyPalette(Value);
    if NewPalette <> 0 then
    begin
      if ThePalette <> 0 then
        DeleteObject(ThePalette);
      ThePalette := NewPalette;
    end;
  end;
end;

procedure THtmlViewer.SetCaretPos(Value: Integer);
begin
  if Value >= 0 then
  begin
    FCaretPos := Value;
  end;
end;

procedure THtmlViewer.SetCharset(const Value: TFontCharset);
begin
  inherited;

end;

function THtmlViewer.FindSourcePos(DisplayPos: Integer): Integer;
begin
  Result := FSectionList.FindSourcePos(DisplayPos);
end;

function THtmlViewer.FindDisplayPos(SourcePos: Integer; Prev: Boolean): Integer;
begin
  Result := FSectionList.FindDocPos(SourcePos, Prev);
end;

function THtmlViewer.DisplayPosToXy(DisplayPos: Integer; var X, Y: Integer): Boolean;
begin
  Result := FSectionList.CursorToXY(PaintPanel.Canvas, DisplayPos, X, Integer(Y)); {Integer() req'd for delphi 2}
end;

{----------------THtmlViewer.SetProcessing}

procedure THtmlViewer.SetProcessing(Value: Boolean);
begin
  if (vsProcessing in FViewerState) <> Value then
  begin
    SetViewerStateBit(vsProcessing, Value);
    if Assigned(OnProcessing) and not (csLoading in ComponentState) then
      OnProcessing(Self, vsProcessing in FViewerState);
  end;
end;

procedure THtmlViewer.HandleMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
var
  DelTime, I: Integer;
begin
  if Assigned(OnMeta) then
    OnMeta(Self, HttpEq, Name, Content);
  if Assigned(OnMetaRefresh) then
    if CompareText(Lowercase(HttpEq), 'refresh') = 0 then
    begin
      I := Pos(';', Content);
      if I > 0 then
        DelTime := StrToIntDef(copy(Content, 1, I - 1), -1)
      else
        DelTime := StrToIntDef(Content, -1);
      if DelTime < 0 then
        Exit
      else if DelTime = 0 then
        DelTime := 1;
      I := Pos('url=', Lowercase(Content));
      if I > 0 then
        FRefreshURL := Copy(Content, I + 4, Length(Content) - I - 3)
      else
        FRefreshURL := '';
      FRefreshDelay := DelTime;
    end;
end;

procedure THtmlViewer.SetOptions(Value: ThtmlViewerOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    if Assigned(FSectionList) then
      with FSectionList do
      begin
        LinksActive := htOverLinksActive in FOptions;
        PrintTableBackground := (htPrintTableBackground in FOptions) or
          (htPrintBackground in FOptions);
        PrintBackground := htPrintBackground in FOptions;
        PrintMonoBlack := htPrintMonochromeBlack in FOptions;
        ShowDummyCaret := htShowDummyCaret in FOptions;
      end;
  end;
end;

procedure THtmlViewer.Repaint;
var
  I: Integer;
begin
  for I := 0 to FormControlList.count - 1 do
    FormControlList[I].Hide;
  BorderPanel.BorderStyle := bsNone;
  inherited Repaint;
end;

procedure THtmlViewer.SetOnDragDrop(const Value: TDragDropEvent);
begin
  inherited;
  if Assigned(Value) then
    PaintPanel.OnDragDrop := ViewerDragDrop
  else
    PaintPanel.OnDragDrop := nil;
end;

procedure THtmlViewer.SetOnDragOver(const Value: TDragOverEvent);
begin
  inherited;
  if Assigned(Value) then
    PaintPanel.OnDragOver := ViewerDragOver
  else
    PaintPanel.OnDragOver := nil;
end;

function THtmlViewer.GetFormData: TFreeList;
begin
  if Assigned(SectionList) then
    Result := SectionList.GetFormControlData
  else
    Result := nil;
end;

procedure THtmlViewer.SetFormData(T: TFreeList);
begin
  if Assigned(SectionList) and Assigned(T) then
    SectionList.SetFormControlData(T);
end;

procedure THtmlViewer.ReplaceImage(const NameID: ThtString; NewImage: TStream);
var
  I: Integer;
  OldPos: Integer;
begin
  if FNameList.Find(NameID, I) then
    if FNameList.Objects[I] is TImageObj then
    begin
      TImageObj(FNameList.Objects[I]).ReplaceImage(NewImage);
      if not TImageObj(FNameList.Objects[I]).ClientSizeKnown then
        if FSectionList.Count > 0 then
        begin
          FSectionList.GetBackgroundBitmap; {load any background bitmap}
          OldPos := Position;
          DoLogic;
          Position := OldPos;
        end;
    end;
end;

function THtmlViewer.GetIDControl(const ID: ThtString): TIDObject;
var
  I: Integer;
  Obj: TIDObject;
begin
  Result := nil;
  with FSectionList.IDNameList do
    if Find(ID, I) then
    begin
      Obj := Objects[I];
      if Obj is TFormControlObj then
        //BG, 15.01.2011: always return Obj rather than the actual WinControl.
        Result := Obj
      else if Obj is TImageObj then
        Result := Obj;
    end;
end;

function THtmlViewer.GetIDDisplay(const ID: ThtString): TPropDisplay;
var
  I: Integer;
  Obj: TIDObject;
begin
  Result := pdUnassigned;
  with FSectionList.IDNameList do
    if Find(ID, I) then
    begin
      Obj := Objects[I];
      if Obj is TBlock then
        Result := TBlock(Obj).Display;
    end;
end;

procedure THtmlViewer.SetIDDisplay(const ID: ThtString; Value: TPropDisplay);
var
  I: Integer;
  Obj: TIDObject;
begin
  with FSectionList.IDNameList do
    if Find(ID, I) then
    begin
      Obj := Objects[I];
      if Obj is TBlock then
        if TBlock(Obj).Display <> Value then
        begin
          FSectionList.HideControls;
          TBlock(Obj).Display := Value;
        end;
    end;
end;

procedure THtmlViewer.SetPrintScale(const Value: Double);
//BG, 29.08.2009: changed zoom factor limits from 0.25..4 to 0.125..8
//BG, 07.10.2009: constants for min/max zoom factor.
const
  CMinPrintScale = 0.125;
  CMaxPrintScale = 8.0;
begin
  if Value > CMaxPrintScale then
    SetPrintScale(CMaxPrintScale)
  else if Value < CMinPrintScale then
    SetPrintScale(CMinPrintScale)
  else
    inherited;
end;

procedure THtmlViewer.Reformat;
var
  Pt: TPoint;
begin
  Layout;
  Update;
  GetCursorPos(Pt);
  SetCursorPos(Pt.X, Pt.Y); {trigger a mousemove to keep cursor correct}
end;

procedure THtmlViewer.htProgressInit;
begin
  if Assigned(OnProgress) then
    OnProgress(Self, psStarting, 0);
end;

procedure THtmlViewer.htProgress(Percent: Integer);
begin
  if Assigned(OnProgress) then
    OnProgress(Self, psRunning, Percent);
end;

procedure THtmlViewer.htProgressEnd;
begin
  if Assigned(OnProgress) then
    OnProgress(Self, psEnding, 100);
end;

{ TPaintPanel }

constructor TPaintPanel.CreateIt(AOwner: TComponent; Viewer: THtmlViewer);
begin
  inherited Create(AOwner);
  FViewer := Viewer;
{$ifdef OwnPaintPanelDoubleBuffering}
{$else}
  DoubleBuffered := True;
{$endif}
end;


procedure TPaintPanel.Paint;
{$ifdef OwnPaintPanelDoubleBuffering}
var
  MemDC: HDC;
  Bm: HBitmap;
  Rect: TRect;
  OldPal: HPalette;
  Canvas2: TCanvas;
  X, Y, W, H: Integer;
{$else}
{$endif}
begin
  if vsDontDraw in FViewer.FViewerState then
    Exit;

{$ifdef OwnPaintPanelDoubleBuffering}
  //BG, 19.02.2011: Issue 57: bypass problems with component's double buffering and XP theme.
  OldPal := 0;
  MemDC := 0;
  Bm := 0;
  Canvas2 := TCanvas.Create;
  try
    Rect := Canvas.ClipRect;
    X := Rect.Left;
    Y := Rect.Top;
    W := Rect.Right - Rect.Left;
    H := Rect.Bottom - Rect.Top;
    MemDC := CreateCompatibleDC(Canvas.Handle);
    Bm := CreateCompatibleBitmap(Canvas.Handle, W, H);
    if (Bm = 0) and (W <> 0) and (H <> 0) then
      raise EOutOfResources.Create('Out of Resources');
    try
      SelectObject(MemDC, Bm);
      SetWindowOrgEx(MemDC, X, Y, nil);
      Canvas2.Font := Font;
      Canvas2.Handle := MemDC;
      Canvas2.Brush.Color := Color;
      Canvas2.Brush.Style := bsSolid;
      FViewer.DrawBorder;
      FViewer.HTMLPaint(Canvas2, Rect);
      OldPal := SelectPalette(Canvas.Handle, ThePalette, False);
      RealizePalette(Canvas.Handle);
      BitBlt(Canvas.Handle, X, Y, W, H, MemDC, X, Y, SrcCopy);
    finally
      if OldPal <> 0 then
        SelectPalette(MemDC, OldPal, False);
      Canvas2.Handle := 0;
    end;
  finally
    Canvas2.Destroy;
    DeleteDC(MemDC);
    DeleteObject(Bm);
  end;
{$else}
//BG, 23.01.2011: paint on panel canvas as Lazarus does the double buffering for us.
//  Additionally this fixes a mysterious bug (Lazarus only), that mixes up the frames
//  of frameviewer, if some images have to be shown
//  (Happened in FrameDemo on page 'samples' with images pengbrew and pyramids).
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  FViewer.DrawBorder;
  FViewer.HTMLPaint(Canvas, Canvas.ClipRect);
{$endif}
end;


procedure TPaintPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1; {it's erased}
end;


procedure TPaintPanel.WMLButtonDblClk(var Message: TWMMouse);
begin
  if Message.Keys and MK_LButton <> 0 then
    THtmlViewer(FViewer).HTMLMouseDblClk(Message);
end;

{ PositionObj }

destructor PositionObj.Destroy;
begin
  FormData.Free;
  inherited;
end;

{ ThvHistoryItem }

//-- BG ---------------------------------------------------------- 02.01.2012 --
destructor ThvHistoryItem.Destroy;
begin
  FFormData.Free;
  inherited;
end;

{ THistoryBase }

//-- BG ---------------------------------------------------------- 02.01.2012 --
procedure THistory.Clear;
begin
  FIndex := 0;
  FHistory.Clear;
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
constructor THistory.Create;
begin
  inherited Create;
  FHistory := TObjectList.Create;
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
procedure THistory.Delete(Index: Integer);
begin
  FHistory.Delete(Index);
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
destructor THistory.Destroy;
begin
  FHistory.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
function THistory.GetCount: Integer;
begin
  Result := FHistory.Count;
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
function THistory.GetItem(Index: Integer): THistoryItem;
begin
  if Count = 0 then
    Result := nil
  else
    Result := THistoryItem(FHistory[Index]);
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
function THistory.GetLowestIndexOfUrl(const Url: ThtString; Start: Integer): Integer;
// extracted from THtmlViewer.SetHistoryIndex
begin
  Result := Start;
  while (Result > 0) and (Items[Result - 1].Url = Url) do
    Dec(Result);
end;

//-- BG ---------------------------------------------------------- 02.01.2012 --
procedure THistory.Insert(Index: Integer; Item: THistoryItem);
begin
  FHistory.Insert(Index, Item);
end;


{ ThvHistory }

//-- BG ---------------------------------------------------------- 02.01.2012 --
function ThvHistory.GetItem(Index: Integer): ThvHistoryItem;
begin
  Result := ThvHistoryItem(inherited getItem(Index));
end;

end.
