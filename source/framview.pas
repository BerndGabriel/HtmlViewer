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

unit FramView;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc, Forms,
{$else}
  Windows,
{$endif}
  SysUtils, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Math, Contnrs,
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
  UrlConn,
  URLSubs,
  HtmlGlobals,
  HtmlBuffer,
  HtmlImages, 
  HTMLSubs,
  Htmlview,
  HtmlSymb,
  HTMLUn2,
  ReadHTML;

type
  {common to TFrameViewer and TFrameBrowser}
  TWindowRequestEvent = procedure(Sender: TObject; const Target, URL: ThtString) of object;
  fvOptionEnum = (
    fvMetaRefresh, fvNoBorder, fvNoLinkUnderline, fvOverLinksActive,
    fvPrintMonochromeBlack, fvPrintTableBackground, fvPrintBackground,
    fvShowVScroll, fvNoFocusRect, fvShowDummyCaret, fvNoWheelMouse,
    fvNoLinkHilite, fvAllowHotSpotDblClick);
  TFrameViewerOptions = set of fvOptionEnum;

  {for TFrameViewer}

  TBufferRequestEvent = procedure(Sender: TObject; const SRC: ThtString; var Buffer: TBuffer) of object;
  TFileRequestEvent = procedure(Sender: TObject; const SRC: ThtString; var NewName: ThtString) of object;
  TStreamRequestEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TStream) of object;
  TStringsRequestEvent = procedure(Sender: TObject; const SRC: ThtString; var Strings: ThtStrings) of object;

  {TFVBase is common base class for TFrameViewer and TFrameBrowser}

  TViewerFrameBase = class;
  TViewerFrameClass = class of TViewerFrameBase;
  TFrameSetBase = class;
  TFrameSetClass = class of TFrameSetBase;
  TSubFrameSetBase = class;
  TSubFrameSetClass = class of TSubFrameSetBase;

  TFVBase = class(TFrameViewerBase) {TFrameViewerBase is in HtmlUn2.pas}
  private
    FImageCache: ThtImageCache;
    FCursor: TCursor;
    FHistory, FTitleHistory: ThtStringList;
    FHistoryIndex: Integer;
    FOptions: TFrameViewerOptions;
    FPosition: TList;
    FProcessing, FViewerProcessing: Boolean;
    FViewerList: ThtStringList;
    FViewImages: Boolean;
    FText: ThtString;
    //
    FOnBlankWindowRequest: TWindowRequestEvent;
    FOnFileBrowse: TFileBrowseEvent;
    FOnPrintHTMLHeader, FOnPrintHTMLFooter: ThtmlPagePrinted;
    FOnRightClick: TRightClickEvent;
    FOnViewerClear: TNotifyEvent;
    //
    function GetFwdButtonEnabled: Boolean;
    function GetBackButtonEnabled: Boolean;
    procedure SetBase(Value: ThtString);
    procedure SetCaretPos(Value: Integer);
    procedure SetCursor(Value: TCursor); reintroduce;
    procedure SetHistoryIndex(Value: Integer);
    procedure SetOnFileBrowse(Handler: TFileBrowseEvent);
    procedure SetOnPrintHtmlFooter(Handler: THtmlPagePrinted);
    procedure SetOnPrintHtmlHeader(Handler: THtmlPagePrinted);
    procedure SetOnRightClick(Handler: TRightClickEvent);
    procedure SetOptions(Value: TFrameViewerOptions);
    procedure SetOurPalette(Value: HPalette);
    procedure SetProcessing(Local, Viewer: Boolean);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetViewImages(Value: Boolean);
    procedure SetText(const Value: ThtString);
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
  protected
    FBaseEx: ThtString;
    FURL: ThtString;
    FLinkAttributes: ThtStringList;
    FLinkText: ThtString;
    FCurFrameSet: TFrameSetBase; {the TFrameSet being displayed}
    ProcessList: TList; {list of viewers that are processing}
    Visited: ThtStringList; {visited URLs}
    function CreateViewer(Owner: THtmlFrameBase): THtmlViewer; virtual;
    function CreateIFrameControl(Sender: TObject; Owner: TComponent): TViewerBase;
    function GetActiveViewer: THtmlViewer;
    function GetBase: ThtString;
    function GetBaseTarget: ThtString;
    function GetCaretPos: Integer;
    function GetCurrentFile: ThtString;
    function GetCurViewer(I: Integer): THtmlViewer;
    function GetCurViewerCount: Integer;
    function GetFrameSetClass: TFrameSetClass; virtual; abstract;
    function GetOurPalette: HPalette;
    function GetProcessing: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: UnicodeString;
    function GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
    function GetSubFrameSetClass: TSubFrameSetClass; virtual; abstract;
    function GetTitle: ThtString;
    function GetUseQuirksMode: Boolean; override;
    function GetViewerBase(Viewer: THtmlViewer): ThtString;
    function GetViewerClass: THtmlViewerClass; virtual;
    function GetViewers: ThtStringList;
    function GetViewerTarget(Viewer: THtmlViewer): ThtString;
    function HotSpotClickHandled(const FullUrl: ThtString; const Target: ThtString {= ''}): Boolean;
    procedure AddVisitedLink(const S: ThtString);
    procedure BeginProcessing; virtual;
    procedure BumpHistory(OldFrameSet: TFrameSetBase; OldPos: Integer);
    procedure BumpHistory1(const FileName, Title: ThtString; OldPos: Integer; ft: ThtmlFileType);
    procedure BumpHistory2(OldPos: Integer);
    procedure CheckProcessing(Sender: TObject; ProcessingOn: Boolean);
    procedure CheckVisitedLinks; virtual; abstract;
    procedure ChkFree(Obj: TObject);
    procedure DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList); virtual; abstract;
    procedure DoGetImage(Sender: TObject; const SRC: ThtString; var Stream: TStream); virtual;
    procedure DoURLRequest(Sender: TObject; const SRC: ThtString; var RStream: TStream); virtual; abstract;
    procedure EndProcessing; virtual;
    procedure HotSpotClick(Sender: TObject; const AnURL: ThtString;var Handled: Boolean); virtual; abstract;
    procedure HotSpotCovered(Sender: TObject; const SRC: ThtString); virtual; abstract;
    procedure Loaded; override;
    procedure LoadFromStringInternal(const Text, Name, Dest: ThtString);
    procedure SetActiveColor(const Value: TColor); override;
    procedure SetCharset(const Value: TFontCharset); override;
    procedure SetDefBackground(const Value: TColor); override;
    procedure SetFontColor(const Value: TColor); override;
    procedure SetFontName(const Value: TFontName); override;
    procedure SetFontSize(const Value: Integer); override;
    procedure SetHistoryMaxCount(const Value: Integer); override;
    procedure SetHotSpotColor(const Value: TColor); override;
    procedure SetImageCacheCount(const Value: Integer); override;
    procedure SetLoadCursor(const Value: TCursor); override;
    procedure SetMarginHeight(const Value: Integer); override;
    procedure SetMarginWidth(const Value: Integer); override;
    procedure SetNoSelect(const Value: Boolean); override;
    procedure SetOnBitmapRequest(const Handler: TGetBitmapEvent); override;
    procedure SetOnBitmapRequested(const Handler: TGottenBitmapEvent); override;
    procedure SetOnDragDrop(const Value: TDragDropEvent); override;
    procedure SetOnDragOver(const Value: TDragOverEvent); override;
    procedure SetOnImageClick(const Handler: TImageClickEvent); override;
    procedure SetOnImageOver(const Handler: TImageOverEvent); override;
    procedure SetOnImageRequested(const Handler: TGottenImageEvent); override;
    procedure SetOnLink(const Handler: TLinkType); override;
    procedure SetOnMeta(const Handler: TMetaType); override;
    procedure SetOnMouseDouble(const Handler: TMouseEvent); override;
    procedure SetOnObjectBlur(const Handler: ThtObjectEvent); override;
    procedure SetOnObjectChange(const Handler: ThtObjectEvent); override;
    procedure SetOnObjectClick(const Handler: TObjectClickEvent); override;
    procedure SetOnObjectFocus(const Handler: ThtObjectEvent); override;
    procedure SetOnObjectTag(const Handler: TObjectTagEvent); override;
    procedure SetOnPanelCreate(const Handler: TPanelCreateEvent); override;
    procedure SetOnPanelDestroy(const Handler: TPanelDestroyEvent); override;
    procedure SetOnPanelPrint(const Handler: TPanelPrintEvent); override;
    procedure SetOnParseBegin(const Handler: TParseEvent); override;
    procedure SetOnParseEnd(const Handler: TNotifyEvent); override;
    procedure SetOnPrintFooter(const Handler: TPagePrinted); override;
    procedure SetOnPrintHeader(const Handler: TPagePrinted); override;
    procedure SetOnProgress(const Handler: ThtProgressEvent); override;
    procedure SetOnScript(const Handler: TScriptEvent); override;
    procedure SetPreFontName(const Value: TFontName); override;
    procedure SetPrintMarginBottom(const Value: Double); override;
    procedure SetPrintMarginLeft(const Value: Double); override;
    procedure SetPrintMarginRight(const Value: Double); override;
    procedure SetPrintMarginTop(const Value: Double); override;
    procedure SetPrintScale(const Value: double); override;
    procedure SetQuirksMode(const AValue: THtQuirksMode); override;
    procedure SetVisitedColor(const Value: TColor); override;
    procedure SetVisitedMaxCount(const Value: Integer); override;
    procedure ScaleChanged; override;
    procedure StyleChanged; override;
{$ifdef DEBUG}
    procedure SetName(const NewName: TComponentName); override;
{$endif}
{$ifdef has_StyleElements}
    procedure UpdateStyleElements; override;
{$endif}
    property Base: ThtString read GetBase write SetBase;
    property BaseTarget: ThtString read GetBaseTarget;
    property CurFrameSet: TFrameSetBase read FCurFrameSet;
    property CurViewer[I: Integer]: THtmlViewer read GetCurViewer;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateCopy(Owner: TComponent; Source: TViewerBase); override;
    destructor Destroy; override;
    function CreateSubFrameSet(FrameSet: TObject): TObject; override;
    function Find(const S: UnicodeString; MatchCase: Boolean): Boolean;
    function FindEx(const S: UnicodeString; MatchCase, Reverse: Boolean): Boolean;
    function ViewerFromTarget(const Target: ThtString): THtmlViewer;
{$ifndef NoMetafile}
    function NumPrinterPages(out WidthRatio: double): Integer; overload;
    function NumPrinterPages: Integer; overload;
    procedure Print(FromPage, ToPage: Integer);
{$endif}
    function IsFrame(Doc: TBuffer): Boolean;
    procedure ParseFrame(FrameSet: TObject; Doc: TBuffer; const FName: ThtString; AMetaEvent: TMetaType);
    //
    procedure AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: ThtString); override;
    procedure Clear;
    procedure ClearHistory;
    procedure CopyToClipboard;
    procedure DoAttributes(FrameSet: TObject; Attr: TAttributeList); override;
    procedure GoBack;
    procedure GoFwd;
    procedure LoadFromFile(const Name: ThtString); virtual; abstract;
    procedure LoadFromString(const Text: ThtString; const Name: ThtString = ''; const Dest: ThtString = '');
    procedure Retext;
    procedure Reload;
    procedure Repaint; override;
    procedure SelectAll;
    procedure SetFocus; override;

    property ActiveViewer: THtmlViewer read GetActiveViewer;
    property CaretPos: Integer read GetCaretPos write SetCaretPos;
    property CurrentFile: ThtString read GetCurrentFile;
    property DocumentTitle: ThtString read GetTitle;
    property History: ThtStringList read FHistory;
    property LinkAttributes: ThtStringList read FLinkAttributes;
    property LinkText: ThtString read FLinkText;
    property Palette: HPalette read GetOurPalette write SetOurPalette;
    property Processing: Boolean read GetProcessing;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: UnicodeString read GetSelText;
//    property Target: ThtString read GetTarget;
    property TitleHistory: ThtStringList read FTitleHistory;
    property URL: ThtString read FURL;
    property Viewers: ThtStringList read GetViewers;
    property FwdButtonEnabled: Boolean read GetFwdButtonEnabled;
    property BackButtonEnabled: Boolean read GetBackButtonEnabled;
  published
    property CharSet;
    property CodePage default DEFAULT_CHARSET;
    property Cursor: TCursor read FCursor write SetCursor default crIBeam;
    property DefBackground;
    property DefFontColor;
    property DefFontName;
    property DefFontSize;
    property DefHotSpotColor;
    property DefOverLinkColor;
    property DefPreFontName;
    property DefVisitedLinkColor;
    property fvOptions: TFrameViewerOptions read FOptions write SetOptions default [fvPrintTableBackground, fvPrintMonochromeBlack];
    property HistoryIndex: Integer read FHistoryIndex write SetHistoryIndex;
    property HistoryMaxCount;
    property ImageCacheCount;
    property LoadCursor;
    property MarginHeight;
    property MarginWidth;
    property NoSelect;
    property PrintMarginBottom;
    property PrintMarginLeft;
    property PrintMarginRight;
    property PrintMarginTop;
    property PrintMaxHPages;
    property PrintScale;
    property QuirksMode;
    property Text: ThtString read FText write SetText;
    property ViewImages: Boolean read FViewImages write SetViewImages default True;
    property VisitedMaxCount;
    //
    property OnBitmapRequest;
    property OnBitmapRequested;
    property OnBlankWindowRequest: TWindowRequestEvent read FOnBlankWindowRequest write FOnBlankWindowRequest;
    property OnDragDrop;
    property OnDragOver;
    property OnFileBrowse: TFileBrowseEvent read FOnFileBrowse write SetOnFileBrowse;
    property OnHistoryChange;
    property OnHotSpotTargetClick;
    property OnHotSpotTargetCovered;
    property OnImageClick;
    property OnImageOver;
    property OnImageRequest;
    property OnImageRequested;
    property OnInclude;
    property OnLink;
    property OnMeta;
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
    property OnPrintFooter;
    property OnPrintHeader;
    property OnPrintHTMLFooter: ThtmlPagePrinted read FOnPrintHTMLFooter write SetOnPrintHTMLFooter;
    property OnPrintHTMLHeader: ThtmlPagePrinted read FOnPrintHTMLHeader write SetOnPrintHTMLHeader;
    property OnProcessing;
    property OnProgress;
    property OnRightClick: TRightClickEvent read FOnRightClick write SetOnRightClick;
    property OnScript;
    property OnSoundRequest;
    property OnViewerClear: TNotifyEvent read FOnViewerClear write FOnViewerClear;

    property Align;
    property Anchors;
    property Enabled;
    property Height default 150;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property Width default 150;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
{$ifdef HasGestures}
    property Touch;
    property OnGesture;
{$endif}
  end;

{TFrameViewer Types}

  TFrameBase = class(THtmlFrameBase) {base class for other classes}
  private
    FMasterSet: TFrameSetBase; {Points to top (master) TFrameSetBase}
    FOwner: TSubFrameSetBase;
    FQuirksMode: THtQuirksMode;
    function GetPixelsPerInch: Integer;
  protected
    UnLoaded: Boolean;
    LocalCharSet: TFontCharset;
    LocalCodePage: TBuffCodePage;
    procedure SetQuirksMode(const AValue: THtQuirksMode); virtual;
    procedure FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    function CheckNoResize(out Lower, Upper: Boolean): Boolean; virtual; abstract;
    procedure UnloadFiles; virtual; abstract;

    procedure AddFrameNames; virtual; abstract;
    procedure ClearFrameNames; virtual; abstract;
    procedure UpdateFrameList; virtual; abstract;
  public
    procedure LoadFiles(); virtual; abstract;
    procedure ReLoadFiles(APosition: Integer); virtual; abstract;
    procedure Reload; override;
    procedure InitializeDimensions(X, Y, Wid, Ht: Integer); virtual; abstract;
    property LOwner: TSubFrameSetBase read FOwner;
    property MasterSet: TFrameSetBase read FMasterSet; {Points to top (master) TFrameSetBase}
    property QuirksMode: THtQuirksMode read FQuirksMode write SetQuirksMode;
    property PixelsPerInch: Integer read GetPixelsPerInch;
  end;


  TPositionObj = class
  public
    Pos: Integer;
    Seq: Integer;
    FormData: TFormData;
    destructor Destroy; override;
  end;

{$ifdef UseGenerics}
  TPositionObjList = class(TObjectList<TPositionObj>);
  TFrameBaseList = class(TObjectList<TFrameBase>);
{$else}
  TPositionObjList = class(TObjectList)
  private
    function Get(Index: Integer): TPositionObj; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TPositionObj read Get; default;
  end;

  TFrameBaseList = class(TObjectList)
  private
    function Get(Index: Integer): TFrameBase; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TFrameBase read Get; default;
  end;
{$endif}

  TViewerFrameBase = class(TFrameBase) {TViewerFrameBase holds a THtmlViewer or TSubFrameSetBase}
  protected
    FViewer: THtmlViewer; {the THtmlViewer it holds if any}
    FFrameSet: TSubFrameSetBase; {or the TSubFrameSetBase it holds}
    NoScroll: Boolean;
    frMarginHeight, frMarginWidth: Integer;
    frHistory: ThtStringList;
    frPositionHistory: TPositionObjList;
    frHistoryIndex: Integer;
    RefreshTimer: TTimer;
    NextFile: ThtString;
  protected
{$ifdef has_StyleElements}
    procedure UpdateStyleElements; override;
{$endif}
    function CheckNoResize(out Lower, Upper: Boolean): Boolean; override;
    function ExpandSourceName(const Base, Path, S: ThtString): ThtString; virtual; abstract;
    function GetSubFrameSetClass: TSubFrameSetClass; virtual; abstract;
    procedure AddFrameNames; override;
    procedure ClearFrameNames; override;
    procedure CreateViewer; virtual;
    procedure frBumpHistory(const NewName: ThtString; NewPos, OldPos: Integer; OldFormData: TFormData);
    procedure frBumpHistory1(const NewName: ThtString; Pos: Integer);
    procedure frLoadFromFile(const FName, Dest: ThtString; Bump, Reload: Boolean); virtual; abstract;
    procedure frSetHistoryIndex(Value: Integer);
    procedure FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RefreshEvent(Sender: TObject; Delay: Integer; const URL: ThtString); virtual; abstract;
    procedure RefreshTimerTimer(Sender: TObject); virtual; abstract;
    procedure ReloadFile(const FName: ThtString; APosition: Integer);
    procedure UnloadFiles; override;
    procedure UpdateFrameList; override;
  public
    ViewerPosition: Integer;
    ViewerFormData: TFormData;
    Source, {Dos filename or URL for this frame}
      OrigSource, {Original Source name}
      Destination: ThtString; {Destination offset for this frame}
    WinName: ThtString; {window name, if any, for this frame}
    NoReSize: Boolean;

    constructor CreateIt(AOwner: TComponent; L: TAttributeList; Master: TFrameSetBase; const Path: ThtString); virtual;
    destructor Destroy; override;
    procedure InitializeDimensions(X, Y, Wid, Ht: Integer); override;
    procedure Repaint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Viewer: THtmlViewer read FViewer;
    property FrameSet: TSubFrameSetBase read FFrameSet;
  end;

  TSubFrameSetBase = class(TFrameBase) {can contain one or more TFrames and/or TSubFrameSets}
  protected
    FBase: ThtString;
    FBaseTarget: ThtString;
    FTitle: ThtString;
    OuterBorder: Integer;
    BorderSize: Integer;
    FRefreshURL: ThtString;
    FRefreshDelay: Integer;
    RefreshTimer: TTimer;
    NextFile: ThtString;
    OldRect: TRect;

    function GetFrameClass: TViewerFrameClass; virtual; abstract;
    function CheckNoResize(out Lower, Upper: Boolean): Boolean; override;
    function GetRect: TRect;
    function NearBoundary(X, Y: Integer): Boolean;
    procedure AddFrameNames; override;
    procedure Clear; virtual;
    procedure ClearFrameNames; override;
    procedure FindLineAndCursor(Sender: TObject; X, Y: Integer);
    procedure FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure HandleMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
    procedure LoadFromFile(const FName, Dest: ThtString);
    procedure RefreshTimerTimer(Sender: Tobject); virtual;
    procedure SetRefreshTimer;
    procedure UpdateFrameList; override;
  public
    First: Boolean; {First time thru}
    Rows: Boolean; {set if row frameset, else column frameset}
    List: TFrameBaseList; {list of TFrames and TSubFrameSets in this TSubFrameSetBase}
    Dim, {col width or row height as read.  Blanks may have been added}
      DimF, {col width or row height in pixels as calculated and displayed}
      Lines {pixel pos of lines, Lines[1]=0, Lines[DimCount]=width|height}
      : array[0..20] of SmallInt;
    Fixed {true if line not allowed to be dragged}
      : array[0..20] of Boolean;
    DimCount: Integer;
    DimFTot: Integer;
    LineIndex: Integer;

    constructor CreateIt(AOwner: TComponent; Master: TFrameSetBase); virtual;
    destructor Destroy; override;
    function AddFrame(Attr: TAttributeList; const FName: ThtString): TViewerFrameBase;
    procedure Parsed(const Title, Base, BaseTarget: ThtString); virtual;
    procedure DoAttributes(L: TAttributeList);
    procedure LoadFiles(); override;
    procedure ReLoadFiles(APosition: Integer); override;
    procedure UnloadFiles; override;
    procedure InitializeDimensions(X, Y, Wid, Ht: Integer); override;
    procedure CalcSizes(Sender: TObject);
    property Base: ThtString read FBase;
    property BaseTarget: ThtString read FBaseTarget;
    property Title: ThtString read FTitle;
  end;

  TFrameSetBase = class(TSubFrameSetBase) {only one of these showing, others may be held as History}
  protected
    FActive: THtmlViewer;  // the most recently active viewer
    FCurrentFile: ThtString;  // current filename or URL
    FFrameViewer: TFvBase;
    FrameNames: ThtStringList; {list of Window names and their TFrames}
    Frames: TList; {list of all the Frames contained herein}
    HotSet: TFrameBase; {owner of line we're moving}
    NestLevel: Integer;
    OldWidth, OldHeight: Integer;
    Viewers: TList; {list of all THtmlViewer pointers}

    function RequestEvent: Boolean; virtual; abstract;
    procedure ClearForwards;
    procedure StopTimers;
    procedure UpdateFrameList; override;
  protected
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure CheckActive(Sender: TObject);
    function GetActive: THtmlViewer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Parsed(const Title, Base, BaseTarget: ThtString); override;
    procedure Clear; override;
    procedure CalcSizes(Sender: TObject);
    procedure LoadFromString(const Source, Name, Dest: ThtString);
    procedure Repaint; override;
    property FrameViewer: TFvBase read FFrameViewer;
  end;

  // TFrameViewer shows files from file system only

  PEventRec = ^EventRec;
  EventRec = record
    NewName: ThtString;
    Doc: TBuffer;
  end;

  TFrameViewer = class;
  TFrameSet = class;

  TfvFrame = class(TViewerFrameBase)
  protected
    function ExpandSourceName(const Base, Path, S: ThtString): ThtString; override;
    function GetSubFrameSetClass: TSubFrameSetClass; override;
    function MasterSet: TFrameSet; {$ifdef UseInline} inline; {$endif}
    procedure frLoadFromFile(const FName, Dest: ThtString; Bump, Reload: Boolean); override;
    procedure LoadFiles(PEV: PEventRec); reintroduce; overload;
    procedure RefreshEvent(Sender: TObject; Delay: Integer; const URL: ThtString); override;
    procedure RefreshTimerTimer(Sender: TObject); override;
  public
    procedure LoadFiles(); overload; override;
    procedure ReLoadFiles(APosition: Integer); override;
  end;

  TSubFrameSet = class(TSubFrameSetBase)
  protected
    function GetFrameClass: TViewerFrameClass; override;
  end;

  TFrameSet = class(TFrameSetBase)
  protected
    function FrameViewer: TFrameViewer; {$ifdef UseInline} inline; {$endif}
    function GetFrameClass: TViewerFrameClass; override;
    function RequestEvent: Boolean; override;
    function TriggerEvent(const Src: ThtString; out NewName: ThtString; out Doc: TBuffer): Boolean;
    procedure LoadFromFile(const FName, Dest: ThtString);
    procedure RefreshTimerTimer(Sender: Tobject); override;
  end;

  TFrameViewer = class(TFVBase)
  private
    FOnBufferRequest: TBufferRequestEvent;
    FOnFileRequest: TFileRequestEvent;
    FOnFormSubmit: TFormSubmitEvent;
    FOnStreamRequest: TStreamRequestEvent;
    FOnStringsRequest: TStringsRequestEvent;
    UrlRequestStream: TMemoryStream;
  protected
    function CurFrameSet: TFrameSet; {$ifdef UseInline} inline; {$endif}
    function GetFrameSetClass: TFrameSetClass; override;
    function GetSubFrameSetClass: TSubFrameSetClass; override;
    procedure CheckVisitedLinks; override;
    procedure DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList); override;
    procedure DoURLRequest(Sender: TObject; const SRC: ThtString; var RStream: TStream); override;
    procedure HotSpotCovered(Sender: TObject; const SRC: ThtString); override;
    procedure LoadFromFileInternal(const FileName, Dest: ThtString);
  public
    constructor CreateCopy(Owner: TComponent; Source: TViewerBase); override;
    destructor Destroy; override;
    function HtmlExpandFilename(const Filename: ThtString; const CurrentFilename: ThtString = ''): ThtString; override;
    procedure HotSpotClick(Sender: TObject; const AnURL: ThtString;var Handled: Boolean); override;
    procedure Load(const SRC: ThtString); override;
    procedure LoadFromFile(const FileName: ThtString); override;
    //procedure LoadImageFile(const FName: ThtString); deprecated; // use LoadFromFile() instead
    procedure LoadTargetFromFile(const Target, FileName: ThtString);
  published
    property OnBufferRequest: TBufferRequestEvent read FOnBufferRequest write FOnBufferRequest;
    property OnFileRequest: TFileRequestEvent read FOnFileRequest write FOnFileRequest;
    property OnFormSubmit: TFormSubmitEvent read FOnFormSubmit write FOnFormSubmit;
    property OnStreamRequest: TStreamRequestEvent read FOnStreamRequest write FOnStreamRequest;
    property OnStringsRequest: TStringsRequestEvent read FOnStringsRequest write FOnStringsRequest;

    property OnBitmapRequest;
    property ServerRoot;
{$ifdef has_StyleElements}
    property StyleElements;
{$endif}
  end;

implementation
{$ifdef Compiler24_Plus}
uses System.Types;
{$endif}

const
  Sequence: Integer = 10;

{----------------FileToString}

function FileToString(const Name: ThtString): AnsiString;
var
  FS: TFileStream;
  Tmp: AnsiString;
begin
  Result := '';
  FS := TFileStream.Create( htStringToString(Name), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Tmp, FS.Size);
    FS.ReadBuffer(Tmp[1], FS.Size * SizeOf(AnsiChar));
    Result := Tmp;
  finally
    FS.Free;
  end;
end;

{----------------TViewerFrameBase.CreateIt}

{$ifdef has_StyleElements}
procedure TViewerFrameBase.UpdateStyleElements;
var i : Integer;
begin
  inherited UpdateStyleElements;
  if Assigned(MasterSet.Viewers) then begin
    for i := 0 to MasterSet.Viewers.Count - 1 do begin
      (MasterSet.FrameViewer as TFrameViewer).StyleElements := StyleElements;
    end;
  end;
end;
{$endif}

//-- BG ---------------------------------------------------------- 06.10.2016 --
procedure TViewerFrameBase.AddFrameNames;
begin
  if Assigned(MasterSet) and (WinName <> '') and Assigned(MasterSet.FrameNames) then
    MasterSet.FrameNames.AddObject(htUpperCase(WinName), Self);
end;

//-- BG ---------------------------------------------------------- 06.10.2016 --
procedure TViewerFrameBase.ClearFrameNames;
var
  I: Integer;
begin
  I := -1;
  if Assigned(MasterSet) and (WinName <> '')
    and Assigned(MasterSet.FrameNames)
    and MasterSet.FrameNames.Find(WinName, I)
  then
    MasterSet.FrameNames.Delete(I);
end;

constructor TViewerFrameBase.CreateIt(AOwner: TComponent; L: TAttributeList;
  Master: TFrameSetBase; const Path: ThtString);
var
  I: Integer;
  S: ThtString;
begin
  inherited Create(AOwner);
  if AOwner is TSubFrameSetBase then
    LocalCodePage := TSubFrameSetBase(AOwner).LocalCodePage;
  FOwner := AOwner as TSubFrameSetBase;
  FMasterSet := Master;
  QuirksMode := MasterSet.FrameViewer.QuirksMode;
  frMarginWidth := MasterSet.FrameViewer.MarginWidth;
  frMarginHeight := MasterSet.FrameViewer.MarginHeight;
{$ifndef LCL}
  BevelInner := bvNone;
  if LOwner.BorderSize = 0 then
    BevelOuter := bvNone
  else
  begin
    BevelOuter := bvLowered;
    BevelWidth := LOwner.BorderSize;
  end;
{$endif}
  ParentColor := True;
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
      with L[I] do
        case Which of
          SrcSy:
            begin
              SplitDest(Trim(Name), S, Destination);
              Source := ExpandSourceName(MasterSet.Base, Path, S);
              OrigSource := Source;
            end;
          NameSy: WinName := Name;
          NoResizeSy: NoResize := True;
          ScrollingSy:
            if htCompareText(Name, 'NO') = 0 then {auto and yes work the same}
              NoScroll := True;
          MarginWidthSy: frMarginWidth := Value;
          MarginHeightSy: frMarginHeight := Value;
        end;
  if WinName <> '' then {add it to the Window name list}
    (AOwner as TSubFrameSetBase).MasterSet.FrameNames.AddObject(htUpperCase(WinName), Self);
  OnMouseDown := FVMouseDown;
  OnMouseMove := FVMouseMove;
  OnMouseUp := FVMouseUp;
  frHistory := ThtStringList.Create;
  frPositionHistory := TPositionObjList.Create;
end;

{----------------TViewerFrameBase.Destroy}

destructor TViewerFrameBase.Destroy;
var
  I: Integer;
begin
  if Assigned(MasterSet) then
  begin
    I := -1;
    if (WinName <> '')
      and Assigned(MasterSet.FrameNames) and MasterSet.FrameNames.Find(WinName, I)
      and (MasterSet.FrameNames.Objects[I] = Self) then
      MasterSet.FrameNames.Delete(I);
    if Assigned(Viewer) then
    begin
      if Assigned(MasterSet.Viewers) then
        MasterSet.Viewers.Remove(Viewer);
      if Assigned(MasterSet.Frames) then
        MasterSet.Frames.Remove(Self);
      if Viewer = MasterSet.FActive then
        MasterSet.FActive := nil;
    end;
  end;
  FreeAndNil(FViewer);
  FreeAndNil(FFrameSet);
  FreeAndNil(frHistory);
  FreeAndNil(frPositionHistory);
  ViewerFormData.Free;
  RefreshTimer.Free;
  inherited Destroy;
end;

procedure TViewerFrameBase.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
{in most cases, SetBounds results in a call to CalcSizes.  However, to make sure
 for case where there is no actual change in the bounds.... }
  if Assigned(FrameSet) then
    FrameSet.CalcSizes(nil);
end;

procedure TfvFrame.RefreshEvent(Sender: TObject; Delay: Integer; const URL: ThtString);
begin
  if not (fvMetaRefresh in MasterSet.FrameViewer.fvOptions) then
    Exit;
  if URL = '' then
    NextFile := Source
  else
    NextFile := MasterSet.FrameViewer.HTMLExpandFilename(URL);
  if not MasterSet.RequestEvent and not htFileExists(NextFile) then
    Exit;
  if not Assigned(RefreshTimer) then
    RefreshTimer := TTimer.Create(Self);
  RefreshTimer.OnTimer := RefreshTimerTimer;
  RefreshTimer.Interval := Delay * 1000;
  RefreshTimer.Enabled := True;
end;

procedure TfvFrame.RefreshTimerTimer(Sender: TObject);
var
  S, D: ThtString;
begin
  RefreshTimer.Enabled := False;
  if Unloaded then
    Exit;
  SplitDest(NextFile, S, D);
  if (MasterSet.Viewers.Count = 1) then {load a new FrameSet}
  begin
    if htCompareStr(NextFile, MasterSet.FCurrentFile) = 0 then
      MasterSet.FrameViewer.Reload
    else
      MasterSet.FrameViewer.LoadFromFileInternal(S, D);
  end
  else
    frLoadFromFile(S, D, True, True); {reload set}
end;

procedure TViewerFrameBase.RePaint;
begin
  if Assigned(Viewer) then
    Viewer.RePaint
  else if Assigned(FrameSet) then
    FrameSet.RePaint;
  inherited RePaint;
end;

{----------------TViewerFrameBase.FVMouseDown}

procedure TViewerFrameBase.FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  (Parent as TSubFrameSetBase).FVMouseDown(Sender, Button, Shift, X + Left, Y + Top);
end;

{----------------TViewerFrameBase.FVMouseMove}

procedure TViewerFrameBase.FVMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not NoResize then
    (Parent as TSubFrameSetBase).FVMouseMove(Sender, Shift, X + Left, Y + Top);
end;

{----------------TViewerFrameBase.FVMouseUp}

procedure TViewerFrameBase.FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  (Parent as TSubFrameSetBase).FVMouseUp(Sender, Button, Shift, X + Left, Y + Top);
end;

{----------------TViewerFrameBase.CheckNoResize}

function TViewerFrameBase.CheckNoResize(out Lower, Upper: Boolean): Boolean;
begin
  Result := NoResize;
  Lower := NoResize;
  Upper := NoResize;
end;

{----------------TViewerFrameBase.InitializeDimensions}

procedure TViewerFrameBase.InitializeDimensions(X, Y, Wid, Ht: Integer);
begin
  if Assigned(FrameSet) then
    FrameSet.InitializeDimensions(X, Y, Wid, Ht);
end;

{----------------TViewerFrameBase.CreateViewer}

procedure TViewerFrameBase.CreateViewer;
begin
  FViewer := MasterSet.FrameViewer.CreateViewer(Self); {the Viewer for the frame}
  Viewer.Width := ClientWidth;
  Viewer.Height := ClientHeight;
  Viewer.Align := alClient;
  Viewer.QuirksMode := QuirksMode;
  if (MasterSet.BorderSize = 0) or (fvNoFocusRect in MasterSet.FrameViewer.fvOptions) then
    Viewer.BorderStyle := htNone;
  Viewer.OnHotspotClick := LOwner.MasterSet.FrameViewer.HotSpotClick;
  Viewer.OnHotspotCovered := LOwner.MasterSet.FrameViewer.HotSpotCovered;
  if NoScroll then
    Viewer.Scrollbars := ssNone;
  Viewer.Parent := Self;
  Viewer.Tabstop := True;
  Viewer.CodePage := LocalCodePage;
  MasterSet.Viewers.Add(Viewer);
  with MasterSet.FrameViewer do
  begin
    Viewer.OnMetaRefresh := RefreshEvent;
    if MasterSet.RequestEvent then
      Viewer.OnhtStreamRequest := DoURLRequest;
  end;
  Viewer.MarginWidth := frMarginWidth;
  Viewer.MarginHeight := frMarginHeight;
  Viewer.OnEnter := MasterSet.CheckActive;
end;

{----------------TViewerFrameBase.LoadFiles}

procedure TfvFrame.LoadFiles();
begin
  LoadFiles(nil);
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TfvFrame.ExpandSourceName(const Base, Path, S: ThtString): ThtString;
begin
  Result := S;
  if not MasterSet.RequestEvent then
  begin
    Result := HTMLServerToDos(Result, MasterSet.FrameViewer.ServerRoot);
    if Pos(':', Result) = 0 then
    begin
      if Base <> '' then
        if htCompareText(Base, 'DosPath') = 0 then
          Result := ExpandFilename(Result)
        else
          Result := CombineDos(HTMLToDos(Base), Result)
      else
        Result := Path + Result;
    end;
  end;
end;

procedure TfvFrame.LoadFiles(PEV: PEventRec);
var
  I: Integer;
  Upper, Lower: Boolean;
  EV: EventRec;
  Src: ThtString;
  Stream: TStream;
  ft: ThtDocType;
  Ext: ThtString;
begin
  if ((Source <> '') or Assigned(PEV)) and (MasterSet.NestLevel < 4) then
  begin
    ft := HTMLType;
    if not MasterSet.RequestEvent then
      ft := GetFileType(Source);
    EV.Doc := nil;
    if ft in [ImgType, TextType] then
      EV.NewName := MasterSet.FrameViewer.HTMLExpandFilename(Source)
    else
    begin
      if Assigned(PEV) then
      begin
        EV := PEV^;
      end
      else if Copy(Source, 1, 9) = 'source://' then
      begin
        EV.NewName := Source;
        Src := Copy(Source, 10, MaxInt);
        EV.Doc := TBuffer.Create(Src, EV.NewName);
      end
      else
      begin
        if not MasterSet.TriggerEvent(Source, EV.NewName, EV.Doc) then
        begin
          EV.NewName := MasterSet.FrameViewer.HTMLExpandFilename(Source);
          if htFileExists(Ev.NewName) then
          begin
            Stream := TFileStream.Create( htStringToString(EV.NewName), fmOpenRead or fmShareDenyWrite);
            try
              EV.Doc := TBuffer.Create(Stream, EV.NewName);
            finally
              Stream.Free;
            end;
          end;
        end;
      end;
    end;
    Inc(MasterSet.NestLevel);
    try
      if not (ft in [ImgType, TextType]) and MasterSet.FrameViewer.IsFrame(EV.Doc) then
      begin
        FFrameSet := GetSubFrameSetClass.CreateIt(Self, MasterSet);
        FrameSet.Align := alClient;
        FrameSet.Visible := False;
        InsertControl(FrameSet);
        FrameSet.SendToBack;
        FrameSet.Visible := True;
        MasterSet.FrameViewer.ParseFrame(FrameSet, EV.Doc, EV.NewName, FrameSet.HandleMeta);
{$ifndef LCL}
        BevelOuter := bvNone;
{$endif}
        frBumpHistory1(Source, 0);
        with FrameSet do
        begin
          for I := 0 to List.Count - 1 do
            List[I].LoadFiles();
          CheckNoresize(Lower, Upper);
          if FRefreshDelay > 0 then
            SetRefreshTimer;
        end;
      end
      else
      begin
        CreateViewer;
        Viewer.Base := MasterSet.FBase;
        case ft of
          ImgType,
          TextType:
            Viewer.LoadFromFile(EV.NewName, ft);
        else
          if EV.Doc <> nil then
          begin
            Ext := htLowerCase(ExtractFileExt(Source));
            if Length(Ext) > 0 then
              Delete(Ext, 1, 1);
            if Copy(Ev.NewName, 1, 9) <> 'source://' then
              ft := FileExt2DocType(Ext)
            else if ft = OtherType then
              ft := HTMLType;
            Viewer.LoadFromDocument(EV.Doc, Source, ft);
            Viewer.PositionTo(Destination);
          end
          else
          begin
            Viewer.LoadFromFile(EV.NewName + Destination, ft)
          end;
        end;
        frBumpHistory1(Source, Viewer.Position);
      end;
    except
      if not Assigned(Viewer) then
        CreateViewer;
      FreeAndNil(FFrameSet);
      Viewer.Text := '<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + EV.NewName; {load an error message}
    end;
    Dec(MasterSet.NestLevel);
  end
  else
  begin {so blank area will perform like the TFrameViewer}
    OnMouseDown := MasterSet.FrameViewer.OnMouseDown;
    OnMouseMove := MasterSet.FrameViewer.OnMouseMove;
    OnMouseUp := MasterSet.FrameViewer.OnMouseUp;
  end;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TfvFrame.MasterSet: TFrameSet;
begin
  Result := TFrameSet(inherited MasterSet);
end;

{----------------TfvFrame.ReloadFiles}

procedure TfvFrame.ReloadFiles(APosition: Integer);
var
  I: Integer;
  Upper, Lower: Boolean;
  EV: EventRec;
  ft: THtmlFileType;
  Src: ThtString;

  procedure DoError;
  begin
    Viewer.Text := '<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + Source; {load an error message}
  end;

begin
  if Source <> '' then
    if Assigned(FrameSet) then
    begin
      with FrameSet do
      begin
        for I := 0 to List.Count - 1 do
          List[I].ReloadFiles(APosition);
        CheckNoResize(Lower, Upper);
      end;
    end
    else if Assigned(Viewer) then
    begin
      Viewer.Base := MasterSet.FBase;
{$ifdef Compiler31_Plus}
      // If compiled with Delphi and we started on a monitor <> 96 DPI and we move
      // to a 96 DPI monitor for the first time the viewer is not automatically
      // scaled to 96 DPI:
      if Viewer.PixelsPerInch <> PixelsPerInch then
        Viewer.ScaleForPPI(PixelsPerInch);
{$endif}
{$ifdef LCL}
      if Application.Scaled and Viewer.ParentForm.Scaled then
        if Viewer.PixelsPerInch <> PixelsPerInch then
          Viewer.AutoAdjustLayout(lapAutoAdjustForDPI, Viewer.PixelsPerInch, PixelsPerInch, 0, 0);
{$endif}
      ft := GetFileType(Source);
      case ft of
        ImgType,
        TextType:
          try
            Viewer.LoadFromFile(Source, ft);
          except
            {leave blank on error}
          end;
      else
        try
          if MasterSet.TriggerEvent(Source, EV.NewName, EV.Doc) then
            if EV.Doc <> nil then
              Viewer.LoadFromDocument(EV.Doc, '')
            else
              Viewer.LoadFromFile(EV.NewName, ft)
          else if Copy(Source, 1, 9) = 'source://' then
          begin
            EV.NewName := Source;
            Src := Copy(Source, 10, MaxInt);
            EV.Doc := TBuffer.Create(Src, EV.NewName);
            Viewer.LoadFromDocument(EV.Doc, '');
          end
          else
            Viewer.LoadFromFile(Source, ft);

          if APosition < 0 then
            Viewer.Position := ViewerPosition
          else
            Viewer.Position := APosition; {its History Position}
          Viewer.FormData := ViewerFormData;
          ViewerFormData.Free;
          ViewerFormData := nil;
        except
          DoError;
        end;
      end;
    end;
  Unloaded := False;
end;

{----------------TViewerFrameBase.UnloadFiles}

procedure TViewerFrameBase.UnloadFiles;
var
  I: Integer;
begin
  if Assigned(RefreshTimer) then
    RefreshTimer.Enabled := False;
  if Assigned(FrameSet) then
  begin
    with FrameSet do
    begin
      for I := 0 to List.Count - 1 do
        List[I].UnloadFiles;
    end;
  end
  else if Assigned(Viewer) then
  begin
    ViewerPosition := Viewer.Position;
    ViewerFormData := Viewer.FormData;
    if Assigned(MasterSet.FrameViewer.OnViewerClear) then
      MasterSet.FrameViewer.OnViewerClear(Viewer);
    Viewer.Clear;
    if MasterSet.FActive = Viewer then
      MasterSet.FActive := nil;
    Viewer.OnSoundRequest := nil;
  end;
  Unloaded := True;
end;

{----------------TViewerFrameBase.frLoadFromFile}

procedure TfvFrame.frLoadFromFile(const FName, Dest: ThtString; Bump, Reload: Boolean);
{Note: if FName not '' and there is no RequestEvent, it has been HTML expanded
 and contains the path}
var
  OldPos: Integer;
  HS, OldTitle, OldName: ThtString;
  OldFormData: TFormData;
  SameName: Boolean;
  OldViewer: THtmlViewer;
  OldFrameSet: TSubFrameSetBase;
  EV: EventRec;
  Upper, Lower, FrameFile: Boolean;
  Item: TFrameBase;
  I: Integer;
  Stream: TStream;
  ft: THtmlFileType;
begin
  if Assigned(RefreshTimer) then
    RefreshTimer.Enabled := False;
  OldName := Source;
  EV.NewName := FName;
  if EV.NewName = '' then
    EV.NewName := OldName;
  Source := EV.NewName;
  HS := EV.NewName;
  SameName := htCompareText(Source, OldName) = 0;
{if SameName, will not have to reload anything}
  ft := HTMLType;
  if not MasterSet.RequestEvent then
    ft := GetFileType(Source);
  if not (ft in [ImgType, TextType]) and not SameName then
    if not MasterSet.TriggerEvent(Source, EV.NewName, EV.Doc) then
    begin
      EV.NewName := MasterSet.FrameViewer.HTMLExpandFilename(Source);
      if htFileExists(Ev.NewName) then
      begin
        Stream := TFileStream.Create( htStringToString(EV.NewName), fmOpenRead or fmShareDenyWrite);
        try
          EV.Doc := TBuffer.Create(Stream, EV.NewName);
        finally
          Stream.Free;
        end;
      end;
    end;
  try
    if not SameName then
      try
        FrameFile := not (ft in [ImgType, TextType]) and MasterSet.FrameViewer.IsFrame(EV.Doc);
      except
        on E: Exception do
          raise EhtLoadError.CreateFmt('Can''t locate ''%s'': %s', [EV.NewName, E.Message]);
      end
    else
      FrameFile := not Assigned(Viewer);
    if SameName then
      if Assigned(Viewer) then
      begin
        OldPos := Viewer.Position;
        if Reload then
        begin {this for Meta Refresh only}
          if EV.Doc <> nil then
            Viewer.LoadFromDocument(EV.Doc, '')
          else
            Viewer.LoadFromFile(EV.NewName + Dest);
          Viewer.Position := OldPos;
        end
        else
        begin
          Viewer.PositionTo(Dest);
          if Bump and (Viewer.Position <> OldPos) then
          {Viewer to Viewer}
            frBumpHistory(HS, Viewer.Position, OldPos, nil);
        end;
        MasterSet.FrameViewer.AddVisitedLink(EV.NewName + Dest);
      end
      else
      begin
        with FrameSet do
        begin
          for I := 0 to List.Count - 1 do
          begin
            Item := TFrameBase(List.Items[I]);
            if Item is TViewerFrameBase then
              with TViewerFrameBase(Item) do
                if htCompareText(Source, OrigSource) <> 0 then
                begin
                  frLoadFromFile(OrigSource, '', True, False);
                end;
          end;
        end;
        Exit;
      end
    else if Assigned(Viewer) and not FrameFile then {not Same Name}
    begin {Viewer already assigned and it's not a Frame file}
      OldPos := Viewer.Position;
      OldTitle := Viewer.DocumentTitle;
      OldFormData := Viewer.FormData;
      try
        case ft of
          ImgType,
          TextType:
            Viewer.LoadFromFile(EV.NewName + Dest, ft);
        else
          Viewer.Base := MasterSet.FBase;
          if EV.Doc <> nil then
            Viewer.LoadFromDocument(EV.Doc, EV.NewName + Dest, ft)
          else
            Viewer.LoadFromFile(EV.NewName + Dest, ft);
        end;
        MasterSet.FrameViewer.AddVisitedLink(EV.NewName + Dest);
        if MasterSet.Viewers.Count > 1 then
        begin
          if Bump then
           {Viewer to Viewer}
            frBumpHistory(HS, Viewer.Position, OldPos, OldFormData)
          else
            OldFormData.Free;
        end
        else
          OldFormData.Free;
      except
        OldFormData.Free;
        raise;
      end;
      if (MasterSet.Viewers.Count = 1) and Bump then
      {a single viewer situation, bump the history here}
        with MasterSet do
        begin
          FCurrentFile := Viewer.CurrentFile;
          FTitle := Viewer.DocumentTitle;
          FBase := Viewer.Base;
          FBaseTarget := Viewer.BaseTarget;
          FrameViewer.BumpHistory1(OldName, OldTitle, OldPos, HTMLType);
        end;
    end
    else
    begin {Viewer is not assigned or it is a Frame File} {not Same Name here either}
    {keep the old viewer or frameset around (free later) to minimize blink}
      OldViewer := Viewer;       FViewer := nil;
      OldFrameSet := FrameSet; FFrameSet := nil;
      if OldFrameSet <> nil then
        OldFrameSet.ClearFrameNames;
      if not (ft in [ImgType, TextType]) and FrameFile then
      begin {it's a frame file}
        FFrameSet := GetSubFrameSetClass.CreateIt(Self, MasterSet);
        FrameSet.Align := alClient;
        FrameSet.Visible := False;
        InsertControl(FrameSet);
        FrameSet.SendToBack; {to prevent blink}
        FrameSet.Visible := True;
        MasterSet.FrameViewer.ParseFrame(FrameSet, EV.Doc, EV.NewName, FrameSet.HandleMeta);
        MasterSet.FrameViewer.AddVisitedLink(EV.NewName);
{$ifndef LCL}
        BevelOuter := bvNone;
{$endif}
        with FrameSet do
        begin
          for I := 0 to List.Count - 1 do
            List[I].LoadFiles();
          CheckNoresize(Lower, Upper);
          if FRefreshDelay > 0 then
            SetRefreshTimer;
        end;
        if Assigned(OldViewer) then
          frBumpHistory(HS, 0, OldViewer.Position, OldViewer.FormData)
        else
          frBumpHistory(EV.NewName, 0, 0, nil);
      end
      else
      begin {not a frame file but needs a viewer}
        CreateViewer;
        case ft of
          ImgType,
          TextType:
            Viewer.LoadFromFile(EV.NewName + Dest, ft)
        else
          Viewer.Base := MasterSet.FBase;
          if EV.Doc <> nil then
            Viewer.LoadFromDocument(EV.Doc, EV.NewName + Dest, ft)
          else
            Viewer.LoadFromFile(EV.NewName + Dest, ft);
        end;
        MasterSet.FrameViewer.AddVisitedLink(EV.NewName + Dest);
      {FrameSet to Viewer}
        frBumpHistory(HS, Viewer.Position, 0, nil);
      end;
      if Assigned(FrameSet) then
        with FrameSet do
        begin
          with ClientRect do
            InitializeDimensions(Left, Top, Right - Left, Bottom - Top);
          CalcSizes(nil);
        end;
      if Assigned(Viewer) then
      begin
{$ifndef LCL}
        if MasterSet.BorderSize = 0 then
          BevelOuter := bvNone
        else
        begin
          BevelOuter := bvLowered;
          BevelWidth := MasterSet.BorderSize;
        end;
{$endif}
        if (Dest <> '') then
          Viewer.PositionTo(Dest);
      end;
      if Assigned(OldViewer) then
      begin
        MasterSet.Viewers.Remove(OldViewer);
        if MasterSet.FActive = OldViewer then
          MasterSet.FActive := nil;
        OldViewer.Free;
      end
      else if Assigned(OldFrameSet) then
      begin
        OldFrameSet.UnloadFiles;
        OldFrameSet.Visible := False;
        OldFrameSet.DestroyHandle;
      end;
      Invalidate;
    end;
  except
    Source := OldName;
    raise;
  end;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TfvFrame.GetSubFrameSetClass: TSubFrameSetClass;
begin
  Result := TSubFrameSet;
end;

{----------------TViewerFrameBase.ReloadFile}

procedure TViewerFrameBase.ReloadFile(const FName: ThtString; APosition: Integer);
{It's known that there is only a single viewer, the file is not being changed,
 only the position}
begin
  Viewer.Position := APosition;
end;

{----------------TViewerFrameBase.frBumpHistory}

procedure TViewerFrameBase.frBumpHistory(const NewName: ThtString;
  NewPos, OldPos: Integer; OldFormData: TFormData);
{applies to TFrames which hold a THtmlViewer}{Viewer to Viewer}
var
  PO: TPositionObj;
begin
  with frHistory do
  begin
    if (Count > 0) then
    begin
      TPositionObjList(frPositionHistory)[frHistoryIndex].Pos := OldPos;
      if frHistory[frHistoryIndex] <> NewName then
        TPositionObjList(frPositionHistory)[frHistoryIndex].FormData := OldFormData
      else
        OldFormData.Free;
    end
    else
      OldFormData.Free;
    MasterSet.ClearForwards; {clear the history list forwards}
    frHistoryIndex := 0;
    InsertObject(0, NewName, FrameSet); {FrameSet may be Nil here}
    PO := TPositionObj.Create;
    PO.Pos := NewPos;
    PO.Seq := Sequence;
    Inc(Sequence);
    frPositionHistory.Insert(0, PO);
    MasterSet.UpdateFrameList;
    with MasterSet.FrameViewer do
      if Assigned(OnHistoryChange) then
        OnHistoryChange(MasterSet.FrameViewer);
  end;
end;

{----------------TViewerFrameBase.frBumpHistory1}

procedure TViewerFrameBase.frBumpHistory1(const NewName: ThtString; Pos: Integer);
{called from a fresh TViewerFrameBase.  History list is empty}
var
  PO: TPositionObj;
begin
  with frHistory do
  begin
    frHistoryIndex := 0;
    InsertObject(0, NewName, FrameSet); {FrameSet may be Nil here}
    PO := TPositionObj.Create;
    PO.Pos := Pos;
    PO.Seq := Sequence;
    Inc(Sequence);
    frPositionHistory.Insert(0, PO);
    MasterSet.UpdateFrameList;
    with MasterSet.FrameViewer do
      if Assigned(OnHistoryChange) then
        OnHistoryChange(MasterSet.FrameViewer);
  end;
end;

{----------------TViewerFrameBase.frSetHistoryIndex}

procedure TViewerFrameBase.frSetHistoryIndex(Value: Integer);
begin
  with frHistory do
    if (Value <> frHistoryIndex) and (Value >= 0) and (Value < Count) then
    begin
      if Assigned(RefreshTimer) then
        RefreshTimer.Enabled := False; {cut off any timing underway}
      if Assigned(Viewer) then {current is Viewer}
        with TPositionObjList(frPositionHistory)[frHistoryIndex] do
        begin
          Pos := Viewer.Position; {save the old position}
        {note that frHistoryIndex can only change by 1}
          FormData := Viewer.FormData;
        end
      else
      begin {Current is FrameSet}
        FrameSet.UnloadFiles;
        FrameSet.DestroyHandle;
        FrameSet.ClearFrameNames;
        FrameSet.Visible := False;
        FFrameSet := nil; {it's not destroyed,though}
      end;

      if Objects[Value] is TSubFrameSetBase then
      begin
        FFrameSet := TSubFrameSetBase(Objects[Value]);
        FrameSet.Visible := True;
        FrameSet.ReloadFiles(-1);
        FrameSet.AddFrameNames;
        if Assigned(Viewer) then
        begin
          if Assigned(MasterSet.Viewers) then
            MasterSet.Viewers.Remove(Viewer);
          if MasterSet.FActive = Viewer then
            MasterSet.FActive := nil;
          FreeAndNil(FViewer);
        end;
      end
      else
      begin
        if not Assigned(Viewer) then
          CreateViewer;
        with TPositionObjList(frPositionHistory)[Value] do
        begin
          if (Source <> Strings[Value]) then
            frLoadFromFile(Strings[Value], '', False, False);
          Viewer.FormData := FormData;
          FormData.Free;
          FormData := nil;
          Viewer.Position := Pos;
        end;
      end;
      Source := Strings[Value];
      frHistoryIndex := Value;
      MasterSet.UpdateFrameList;
      with MasterSet.FrameViewer do
        if Assigned(OnHistoryChange) then
          OnHistoryChange(MasterSet.FrameViewer);
      MasterSet.FrameViewer.CheckVisitedLinks;
    end;
end;

{----------------TViewerFrameBase.UpdateFrameList}

procedure TViewerFrameBase.UpdateFrameList;
begin
  MasterSet.Frames.Add(Self);
  if Assigned(FrameSet) then
    FrameSet.UpdateFrameList;
end;

{----------------TSubFrameSetBase.CreateIt}

constructor TSubFrameSetBase.CreateIt(AOwner: TComponent; Master: TFrameSetBase);
begin
  inherited Create(AOwner);
  FMasterSet := Master;
  if AOwner is TFrameBase then
    LocalCodePage := TFrameBase(AOwner).LocalCodePage;
  if Self <> Master then
  begin
    BorderSize := Master.BorderSize;
    QuirksMode := Master.QuirksMode;
  end;
  OuterBorder := 0; {no border for subframesets}
  First := True;
  List := TFrameBaseList.Create;
  OnResize := CalcSizes;
  OnMouseDown := FVMouseDown;
  OnMouseMove := FVMouseMove;
  OnMouseUp := FVMouseUp;
{$IFDEF delphi7_plus}
  ParentBackground := False;
{$ENDIF}
  ParentColor := True;
{$ifdef has_StyleElements}
  StyleElements := FMasterSet.StyleElements;
{$endif}
end;

{----------------TSubFrameSetBase.ClearFrameNames}

procedure TSubFrameSetBase.ClearFrameNames;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].ClearFrameNames;
end;

{----------------TSubFrameSetBase.AddFrameNames}

procedure TSubFrameSetBase.AddFrameNames;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].AddFrameNames;
end;

{----------------TSubFrameSetBase.Destroy}

destructor TSubFrameSetBase.Destroy;
begin
  List.Free;
  List := nil;
  RefreshTimer.Free;
  inherited Destroy;
end;

{----------------TSubFrameSetBase.AddFrame}

function TSubFrameSetBase.AddFrame(Attr: TAttributeList; const FName: ThtString): TViewerFrameBase;
{called by the parser when <Frame> is encountered within the <Frameset>
 definition}
begin
  Result := GetFrameClass.CreateIt(Self, Attr, MasterSet, ExtractFilePath(FName));
  List.Add(Result);
  Result.SetBounds(OuterBorder, OuterBorder, Width - 2 * OuterBorder, Height - 2 * OuterBorder);
  InsertControl(Result);
end;

{----------------TSubFrameSetBase.DoAttributes}

procedure TSubFrameSetBase.DoAttributes(L: TAttributeList);
{called by the parser to process the <Frameset> attributes}
var
  T: TAttribute;
  S: ThtString;
  Numb: ThtString;

  procedure GetDims;
  const
    EOL = CrChar;
  var
    Ch: ThtChar;
    I, N: Integer;

    procedure GetCh;
    begin
      if I > Length(S) then
        Ch := EOL
      else
      begin
        Ch := S[I];
        Inc(I);
      end;
    end;

  begin
{$ifdef DEBUG}
    // For debugging we name our internal components.
    // So we cannot use the name to control reading.
    S := T.Name;
{$else}
    if Name = '' then
      S := T.Name
    else
      Exit;
{$endif}
    I := 1; DimCount := 0;
    repeat
      Inc(DimCount);
      Numb := '';
      GetCh;
      while True do
        case Ch of
          '0'..'9', '*', EOL, ',':
            break;
        else
          GetCh;
        end;

      case Ch of
        '0'..'9':
          begin
            while IsDigit(Ch) do
            begin
              htAppendChr(Numb, Ch);
              GetCh;
            end;
            N := Max(1, StrToInt( htStringToString(Numb) )); {no zeros}
            while True do
              case Ch of
                '*', '%', ',', EOL:
                  break;
              else
                GetCh;
              end;

            case Ch of
              '*':
              begin
                Dim[DimCount] := -Min(99, N); {store '*' relatives as negative, -1..-99}
                GetCh;
              end;

              '%':
              begin {%'s stored as -(100 + %),  i.e. -110 is 10% }
                Dim[DimCount] := -Min(1000, N + 100); {limit to 900%}
                GetCh;
              end;

            else
              Dim[DimCount] := Min(N, 5000); {limit absolute to 5000}
            end;
          end;

        '*', ',', EOL:
          begin
            Dim[DimCount] := -1;
            if Ch = '*' then
              GetCh;
          end;
      end;

      while True do
        case Ch of
          ',', EOL:
            break;
        else
          GetCh;
        end;
    until (Ch = EOL) or (DimCount = 20);
  end;

begin
{read the row or column widths into the Dim array}
  T := nil;
  if L.Find(RowsSy, T) then
  begin
    Rows := True;
    GetDims;
  end;
  if L.Find(ColsSy, T) and (DimCount <= 1) then
  begin
    Rows := False;
    DimCount := 0;
    GetDims;
  end;
  if (Self = MasterSet) and not (fvNoBorder in MasterSet.FrameViewer.fvOptions) then
                               {BorderSize already defined as 0}
    if L.Find(BorderSy, T) or L.Find(FrameBorderSy, T) then
    begin
      BorderSize := T.Value;
      OuterBorder := Max(2 - BorderSize, 0);
{$ifndef LCL}
      if OuterBorder >= 1 then
      begin
        BevelWidth := OuterBorder;
        BevelOuter := bvLowered;
      end;
{$endif}
    end
    else
      BorderSize := 2;
end;

{----------------TSubFrameSetBase.LoadFiles}

procedure TSubFrameSetBase.LoadFiles;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].LoadFiles();
end;

{----------------TSubFrameSetBase.ReloadFiles}

procedure TSubFrameSetBase.ReloadFiles(APosition: Integer);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].ReloadFiles(APosition);
  if (FRefreshDelay > 0) and Assigned(RefreshTimer) then
    SetRefreshTimer;
  Unloaded := False;
end;

{----------------TSubFrameSetBase.UnloadFiles}

procedure TSubFrameSetBase.UnloadFiles;
var
  I: Integer;
begin
  if Assigned(RefreshTimer) then
    RefreshTimer.Enabled := False;
  for I := 0 to List.Count - 1 do
    List[I].UnloadFiles;
  if Assigned(MasterSet.FrameViewer.OnSoundRequest) then
    MasterSet.FrameViewer.OnSoundRequest(MasterSet, '', 0, True);
  Unloaded := True;
end;

{----------------TSubFrameSetBase.Parsed}

procedure TSubFrameSetBase.Parsed(const Title, Base, BaseTarget: ThtString);
{called by the parser when </FrameSet> is encountered}
var
  I: Integer;
begin
  if List.Count > DimCount then {a value left out}
  begin {fill in any blanks in Dim array}
    for I := DimCount + 1 to List.Count do
    begin
      Dim[I] := -1; {1 relative unit}
      Inc(DimCount);
    end;
  end
  else
    while DimCount > List.Count do {or add Frames if more Dims than Count}
      AddFrame(nil, '');
  FTitle := Title;
  if Base <> '' then
    FBase := Base
  else
    FBase := MasterSet.FrameViewer.FBaseEx;
  FBaseTarget := BaseTarget;
end;

{----------------TSubFrameSetBase.InitializeDimensions}

procedure TSubFrameSetBase.InitializeDimensions(X, Y, Wid, Ht: Integer);
var
  I, Total, PixTot, PctTot, RelTot, Rel, Sum,
    Remainder, PixDesired, PixActual: Integer;

begin
  if Rows then
    Total := Ht
  else
    Total := Wid;
  PixTot := 0; RelTot := 0; PctTot := 0; DimFTot := 0;
  for I := 1 to DimCount do {count up the total pixels, %'s and relatives}
    if Dim[I] >= 0 then
      PixTot := PixTot + Dim[I]
    else if Dim[I] <= -100 then
      PctTot := PctTot + (-Dim[I] - 100)
    else
      RelTot := RelTot - Dim[I];
  Remainder := Total - PixTot;
  if Remainder <= 0 then
  begin {% and Relative are 0, must scale absolutes}
    for I := 1 to DimCount do
    begin
      if Dim[I] >= 0 then
        DimF[I] := MulDiv(Dim[I], Total, PixTot) {reduce to fit}
      else
        DimF[I] := 0;
      Inc(DimFTot, DimF[I]);
    end;
  end
  else {some remainder left for % and relative}
  begin
    PixDesired := MulDiv(Total, PctTot, 100);
    if PixDesired > Remainder then
      PixActual := Remainder
    else
      PixActual := PixDesired;
    Dec(Remainder, PixActual); {Remainder will be >= 0}
    if RelTot > 0 then
      Rel := Remainder div RelTot {calc each relative unit}
    else
      Rel := 0;
    for I := 1 to DimCount do {calc the actual pixel widths (heights) in DimF}
    begin
      if Dim[I] >= 0 then
        DimF[I] := Dim[I]
      else if Dim[I] <= -100 then
        DimF[I] := MulDiv(-Dim[I] - 100, PixActual, PctTot)
      else
        DimF[I] := -Dim[I] * Rel;
      Inc(DimFTot, DimF[I]);
    end;
  end;

  Sum := 0;
  for I := 0 to List.Count - 1 do {intialize the dimensions of contained items}
  begin
    if Rows then
      List[I].InitializeDimensions(X      , Y + Sum, Wid        , DimF[I + 1])
    else
      List[I].InitializeDimensions(X + Sum, Y      , DimF[I + 1], Ht         );
    Inc(Sum, DimF[I + 1]);
  end;
end;

{----------------TSubFrameSetBase.CalcSizes}
{OnResize event comes here}

procedure TSubFrameSetBase.CalcSizes(Sender: TObject);
var
  I, Step, Sum, ThisTotal: Integer;
  ARect: TRect;
begin
{Note: this method gets called during Destroy as it's in the OnResize event.
 Hence List may be Nil.}
  if Assigned(List) and (List.Count > 0) then
  begin
    ARect := ClientRect;
    InflateRect(ARect, -OuterBorder, -OuterBorder);
    Sum := 0;
    if Rows then
      ThisTotal := ARect.Bottom - ARect.Top
    else
      ThisTotal := ARect.Right - ARect.Left;
    for I := 0 to List.Count - 1 do
    begin
      Step := MulDiv(DimF[I + 1], ThisTotal, DimFTot);
      if Rows then
        List[I].SetBounds(ARect.Left, ARect.Top + Sum, ARect.Right - ARect.Left, Step)
      else
        List[I].SetBounds(ARect.Left + Sum, ARect.Top, Step, ARect.Bottom - Arect.Top);
      Sum := Sum + Step;
      Lines[I + 1] := Sum;
    end;
  end;
end;

{----------------TSubFrameSetBase.NearBoundary}

function TSubFrameSetBase.NearBoundary(X, Y: Integer): Boolean;
begin
  Result := (Abs(X) < 4) or (Abs(X - Width) < 4) or
    (Abs(Y) < 4) or (Abs(Y - Height) < 4);
end;

{----------------TSubFrameSetBase.GetRect}

function TSubFrameSetBase.GetRect: TRect;
{finds the FocusRect to draw when draging boundaries}
var
  Pt, Pt1, Pt2: TPoint;
begin
  Pt1 := Point(0, 0);
  Pt1 := ClientToScreen(Pt1);
  Pt2 := Point(ClientWidth, ClientHeight);
  Pt2 := ClientToScreen(Pt2);
  Pt  := Point(0, 0);
  GetCursorPos(Pt);
  if Rows then
    Result := Rect(Pt1.X, Pt.Y - 1, Pt2.X, Pt.Y + 1)
  else
    Result := Rect(Pt.X - 1, Pt1.Y, Pt.X + 1, Pt2.Y);
  OldRect := Result;
end;

{----------------DrawRect}

procedure DrawRect(ARect: TRect);
{Draws a Focus Rect}
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    DrawFocusRect(DC, ARect);
  finally
    ReleaseDC(0, DC);
  end;
end;

{----------------TSubFrameSetBase.FVMouseDown}

procedure TSubFrameSetBase.FVMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACursor: TCursor;
  RP: record
    case Boolean of
      True: (P1, P2: TPoint);
      False: (R: TRect);
  end;
begin
  if Button <> mbLeft then
    Exit;
  if NearBoundary(X, Y) then
  begin
    if Parent is TFrameBase then
      (Parent as TFrameBase).FVMouseDown(Sender, Button, Shift, X + Left, Y + Top)
    else
      Exit;
  end
  else
  begin
    ACursor := (Sender as TFrameBase).Cursor;
    if (ACursor = crVSplit) or (ACursor = crHSplit) then
    begin
      MasterSet.HotSet := Self;
      with RP do
      begin {restrict cursor to lines on both sides}
        if Rows then
          R := Rect(0, Lines[LineIndex - 1] + 1, ClientWidth, Lines[LineIndex + 1] - 1)
        else
          R := Rect(Lines[LineIndex - 1] + 1, 0, Lines[LineIndex + 1] - 1, ClientHeight);
        P1 := ClientToScreen(P1);
        P2 := ClientToScreen(P2);
        ClipCursor(@R);
      end;
      DrawRect(GetRect);
    end;
  end;
end;

{----------------TSubFrameSetBase.FindLineAndCursor}

procedure TSubFrameSetBase.FindLineAndCursor(Sender: TObject; X, Y: Integer);
var
  ACursor: TCursor;
  Gap, ThisGap, Line, I: Integer;
begin
  if not Assigned(MasterSet.HotSet) then
  begin {here we change the cursor as mouse moves over lines,button up or down}
    if Rows then
      Line := Y
    else
      Line := X;
    Gap := 9999;
    for I := 1 to DimCount - 1 do
    begin
      ThisGap := Line - Lines[I];
      if Abs(ThisGap) < Abs(Gap) then
      begin
        Gap := Line - Lines[I];
        LineIndex := I;
      end
      else if Abs(ThisGap) = Abs(Gap) then {happens if 2 lines in same spot}
        if ThisGap >= 0 then {if Pos, pick the one on right (bottom)}
          LineIndex := I;
    end;

    if (Abs(Gap) <= 4) and not Fixed[LineIndex] then
    begin
      if Rows then
        ACursor := crVSplit
      else
        ACursor := crHSplit;
      (Sender as TFrameBase).Cursor := ACursor;
    end
    else
      (Sender as TFrameBase).Cursor := MasterSet.FrameViewer.Cursor;
  end
  else
    with TSubFrameSetBase(MasterSet.HotSet) do
    begin
      DrawRect(OldRect);
      DrawRect(GetRect);
    end;
end;

{----------------TSubFrameSetBase.FVMouseMove}

procedure TSubFrameSetBase.FVMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if NearBoundary(X, Y) then
    (Parent as TFrameBase).FVMouseMove(Sender, Shift, X + Left, Y + Top)
  else
    FindLineAndCursor(Sender, X, Y);
end;

{----------------TSubFrameSetBase.FVMouseUp}

procedure TSubFrameSetBase.FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  if Button <> mbLeft then
    Exit;
  if MasterSet.HotSet = Self then
  begin
    MasterSet.HotSet := nil;
    DrawRect(OldRect);
    ClipCursor(nil);
    if Rows then
      Lines[LineIndex] := Y
    else
      Lines[LineIndex] := X;
    for I := 1 to DimCount do
      if I = 1 then
        DimF[1] := MulDiv(Lines[1], DimFTot, Lines[DimCount])
      else
        DimF[I] := MulDiv((Lines[I] - Lines[I - 1]), DimFTot, Lines[DimCount]);
    CalcSizes(Self);
    Invalidate;
  end
  else if (Parent is TFrameBase) then
    (Parent as TFrameBase).FVMouseUp(Sender, Button, Shift, X + Left, Y + Top);
end;

{----------------TSubFrameSetBase.CheckNoResize}

function TSubFrameSetBase.CheckNoResize(out Lower, Upper: Boolean): Boolean;
var
  Lw, Up: Boolean;
  I: Integer;
begin
  Result := False;
  Lower := False;
  Upper := False;
  for I := 0 to List.Count - 1 do
    with List[I] do
      if CheckNoResize(Lw, Up) then
      begin
        Result := True; {sides are fixed}
        Fixed[I] := True; {these edges are fixed}
        Fixed[I + 1] := True;
        if Lw and (I = 0) then
          Lower := True;
        if Up and (I = List.Count - 1) then
          Upper := True;
      end;
end;

{----------------TSubFrameSetBase.Clear}

procedure TSubFrameSetBase.Clear;
begin
  List.Clear;
  DimCount := 0;
  First := True;
  Rows := False;
  FillChar(Fixed, Sizeof(Fixed), 0);
  FillChar(Lines, Sizeof(Lines), 0);
  FBase := '';
  FBaseTarget := '';
end;

{----------------TSubFrameSetBase.LoadFromFile}

procedure TSubFrameSetBase.LoadFromFile(const FName, Dest: ThtString);
var
  Frame: TViewerFrameBase;
begin
  Clear;
  Frame := AddFrame(nil, '');
  Frame.Source := FName;
  Frame.Destination := Dest;
  Parsed('', '', '');
  Frame.LoadFiles();
  if Assigned(Frame.FrameSet) then
    with Frame.FrameSet do
    begin
      with ClientRect do
        InitializeDimensions(Left, Top, Right - Left, Bottom - Top);
      CalcSizes(nil);
    end
  else if Assigned(Frame.Viewer) then
    Frame.Viewer.PositionTo(Dest);
  MasterSet.FrameViewer.AddVisitedLink(FName + Dest);
end;

{----------------TSubFrameSetBase.UpdateFrameList}

procedure TSubFrameSetBase.UpdateFrameList;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    List[I].UpdateFrameList;
end;

{----------------TSubFrameSetBase.HandleMeta}

procedure TSubFrameSetBase.HandleMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
var
  DelTime, I: Integer;
  CP: TBuffCodePage;
begin
  if htCompareText(HttpEq, 'content-type') = 0 then
  begin
    CP := StrToCodePage(Content);
    if CP <> CP_UNKNOWN then
    begin
      //LocalCharSet := Info.CharSet;
      LocalCodePage := CP;
    end;
  end;

  if Assigned(MasterSet.FrameViewer.OnMeta) then
    MasterSet.FrameViewer.OnMeta(Sender, HttpEq, Name, Content);
  if not (fvMetaRefresh in MasterSet.FrameViewer.fvOptions) then
    Exit;

  if htCompareText(HttpEq, 'refresh') = 0 then
  begin
    I := Pos(';', Content);
    if I > 0 then
      DelTime := StrToIntDef( Copy( htStringToString(Content), 1, I - 1), -1)
    else
      DelTime := StrToIntDef( htStringToString(Content), -1);
    if DelTime < 0 then
      Exit
    else if DelTime = 0 then
      DelTime := 1;
    I := Pos('url=', Lowercase(Content));
    if I > 0 then
      FRefreshURL := Copy(Content, I + 4, Length(Content) - I - 3)
    else if Owner is TViewerFrameBase then
      FRefreshURL := TViewerFrameBase(Owner).Source
    else
      FRefreshURL := '';
    FRefreshDelay := DelTime;
  end;
end;

{----------------TSubFrameSetBase.SetRefreshTimer}

procedure TSubFrameSetBase.SetRefreshTimer;
begin
  NextFile := HTMLToDos(FRefreshURL);
  if not htFileExists(NextFile) then
    Exit;
  if not Assigned(RefreshTimer) then
    RefreshTimer := TTimer.Create(Self);
  RefreshTimer.OnTimer := RefreshTimerTimer;
  RefreshTimer.Interval := FRefreshDelay * 1000;
  RefreshTimer.Enabled := True;
end;

{----------------TSubFrameSetBase.RefreshTimerTimer}

procedure TSubFrameSetBase.RefreshTimerTimer(Sender: Tobject);
var
  S, D: ThtString;
begin
  RefreshTimer.Enabled := False;
  if Unloaded then
    Exit;
  if Owner is TViewerFrameBase then
  begin
    SplitDest(NextFile, S, D);
    TViewerFrameBase(Owner).frLoadFromFile(S, D, True, True);
  end;
end;

{----------------TFrameSetBase.Create}

constructor TFrameSetBase.Create(AOwner: TComponent);
begin
  inherited CreateIt(AOwner, Self);
  FFrameViewer := AOwner as TFvBase;
{$ifdef DEBUG}
  {$if defined(LCL) or defined(Compiler22_Plus)}
  Name := FFrameViewer.Name + '_FrameSetBase' + IntToHex(Integer(Self));
  {$else}
  Name := FFrameViewer.Name + '_FrameSetBase' + IntToHex(Integer(Self), 8);
  {$ifend}
{$endif}
  LocalCodePage := FrameViewer.CodePage;
  if fvNoBorder in FrameViewer.fvOptions then
    BorderSize := 0
  else
    BorderSize := 2;
{$ifndef LCL}
  BevelOuter := bvNone;
{$endif}
  FTitle := '';
  FCurrentFile := '';
  FrameNames := ThtStringList.Create;
  FrameNames.Sorted := True;
  Viewers := TList.Create;
  Frames := TList.Create;
  OnResize := CalcSizes;
end;

{----------------TFrameSetBase.Destroy}

destructor TFrameSetBase.Destroy;
begin
  FrameNames.Free;
  FrameNames := nil; {is tested later}
  Viewers.Free;
  Viewers := nil;
  Frames.Free;
  Frames := nil;
  inherited Destroy;
end;

{----------------TFrameSetBase.Clear}

procedure TFrameSetBase.Clear;
begin
  inherited Clear;
  FrameNames.Clear;
  Viewers.Clear;
  Frames.Clear;
  HotSet := nil;
  FTitle := '';
  FCurrentFile := '';
  OldHeight := 0;
  OldWidth := 0;
  FActive := nil;
end;

procedure TFrameSetBase.RePaint;
var
  I: Integer;
begin
  if Assigned(Frames) then
    for I := 0 to Frames.Count - 1 do
      TWinControl(Frames[I]).RePaint;
  inherited;
end;

//-- BG ---------------------------------------------------------- 05.03.2015 --
procedure TFrameSetBase.StopTimers;
var
  I: Integer;
begin
  for I := 0 to Viewers.Count - 1 do
    THtmlViewer(Viewers[I]).StopTimers;
end;

{----------------TFrameSetBase.RequestEvent}

function TFrameSet.RequestEvent: Boolean;
begin
  with FrameViewer do
    Result := Assigned(FOnStringsRequest) or Assigned(FOnStreamRequest)
      or Assigned(FOnBufferRequest) or Assigned(FOnFileRequest);
end;

{----------------TFrameSet.TriggerEvent}

function TFrameSet.TriggerEvent(const Src: ThtString; out NewName: ThtString; out Doc: TBuffer): Boolean;
var
  Strings: ThtStrings;
  Stream: TStream;
begin
  Result := False;
  Doc := nil;
  NewName := Src;
  with FrameViewer do
    if Assigned(FOnStringsRequest) then
    begin
      Strings := nil;
      FOnStringsRequest(Self, Src, Strings);
      Result := Assigned(Strings);
      if Result then
        Doc := TBuffer.Create(Strings.Text, Src);
    end
    else if Assigned(FOnStreamRequest) then
    begin
      Stream := nil;
      FOnStreamRequest(Self, Src, Stream);
      Result := Assigned(Stream);
      if Result then
      begin
        Stream.Position := 0;
        Doc := TBuffer.Create(Stream, Src);
      end;
    end
    else if Assigned(FOnBufferRequest) then
    begin
      FOnBufferRequest(Self, Src, Doc);
      Result := Doc <> nil;
      if Result then
      begin
        Doc.Position := 0;
        Doc := TBuffer.Create(Doc);
      end;
    end
    else if Assigned(FOnFileRequest) then
    begin
      FOnFileRequest(Self, Src, NewName);
      Result := NewName <> '';
      if Result then
      begin
        Stream := TFileStream.Create( htStringToString(NewName), fmOpenRead or fmShareDenyWrite);
        try
          Doc := TBuffer.Create(Stream, NewName);
        finally
          Stream.Free;
        end;
      end;
    end;
end;

{----------------TFrameSetBase.Parsed}

procedure TFrameSetBase.Parsed(const Title, Base, BaseTarget: ThtString);
begin
  inherited;
  with ClientRect do
    InitializeDimensions(Left, Top, Right - Left, Bottom - Top);
end;

{----------------TFrameSetBase.CalcSizes}
{OnResize event comes here}

procedure TFrameSetBase.CalcSizes(Sender: TObject);
var
  ARect: TRect;
begin
  ARect := ClientRect;
  InflateRect(ARect, -OuterBorder, -OuterBorder);
  with ARect do
  begin
    if (OldWidth <> Right - Left) or (OldHeight <> Bottom - Top) then
    begin
      InitializeDimensions(Left, Top, Right - Left, Bottom - Top);
      inherited CalcSizes(Sender);
    end;
    OldWidth := Right - Left;
    OldHeight := Bottom - Top;
  end;
end;

{----------------TFrameSetBase.CheckActive}

procedure TFrameSetBase.CheckActive(Sender: TObject);
begin
  if Sender is THtmlViewer then
    FActive := THtmlViewer(Sender);
end;

{----------------TFrameSetBase.GetActive}

function TFrameSetBase.GetActive: THtmlViewer;
begin
  if Viewers.Count = 1 then
    Result := THtmlViewer(Viewers[0])
  else
  try
    if FActive is THtmlViewer then
      Result := FActive
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

{----------------TFrameSetBase.FVMouseMove}

procedure TFrameSetBase.FVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FindLineAndCursor(Sender, X, Y);
  if (LineIndex = 0) or (LineIndex = DimCount) then
  begin {picked up the outer boundary}
    (Sender as TFrameBase).Cursor := MasterSet.FrameViewer.Cursor;
    Cursor := MasterSet.FrameViewer.Cursor;
  end;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TFrameSet.FrameViewer: TFrameViewer;
begin
  Result := TFrameViewer(inherited FrameViewer);
end;

{----------------TFrameSetBase.LoadFromFile}

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TFrameSet.GetFrameClass: TViewerFrameClass;
begin
  Result := TfvFrame;
end;

procedure TFrameSet.LoadFromFile(const FName, Dest: ThtString);
var
  I: Integer;
  Frame: TfvFrame;
  Lower, Upper: Boolean;
  EV: EventRec;
  EventPointer: PEventRec;
  Stream: TStream;
  ft: ThtmlFileType;
begin
  Clear;
  NestLevel := 0;
  if not htFileExists(FName) then
    Exit;

  ft := HTMLType;
  if not MasterSet.RequestEvent then
    ft := GetFileType(FName);
  EV.Doc := nil;
  if (ft in [ImgType, TextType]) or not TriggerEvent(FName, EV.NewName, EV.Doc) then
  begin
    EV.NewName := FName; //BG, 13.11.2016: FName should have been expanded before. // ExpandFileName(FName);
    Stream := TFileStream.Create( htStringToString(EV.NewName), fmOpenRead or fmShareDenyWrite);
    try
      EV.Doc := TBuffer.Create(Stream, EV.NewName);
    finally
      Stream.Free;
    end;
    FCurrentFile := EV.NewName;
  end
  else
  begin {triggerevent}
    FCurrentFile := FName;
  end;
  FRefreshDelay := 0;
  if not (ft in [ImgType, TextType]) and MasterSet.FrameViewer.IsFrame(EV.Doc) then
  begin {it's a Frameset html file}
    MasterSet.FrameViewer.ParseFrame(Self, EV.Doc, EV.NewName, HandleMeta);
    for I := 0 to List.Count - 1 do
      List[I].LoadFiles;
    CalcSizes(Self);
    CheckNoresize(Lower, Upper);
    if FRefreshDelay > 0 then
      SetRefreshTimer;
  end
  else
  begin {it's a non frame file}
    Frame := TfvFrame(AddFrame(nil, ''));
    if not (ft in [ImgType, TextType]) and RequestEvent then
    begin
      Frame.Source := FName;
      EventPointer := @EV;
    end
    else
    begin
      Frame.Source := EV.NewName;
      FreeAndNil(EV.Doc);
      EventPointer := nil;
    end;
    Frame.Destination := Dest;
    Parsed('', '', '');
    CalcSizes(Self);
    Frame.Loadfiles(EventPointer);
    FTitle := Frame.Viewer.DocumentTitle;
    FBaseTarget := Frame.Viewer.BaseTarget;
  end;
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
procedure TFrameSetBase.LoadFromString(const Source, Name, Dest: ThtString);
var
  I: Integer;
  Frame: TfvFrame;
  Lower, Upper: Boolean;
  EV: EventRec;
  PEV: PEventRec;
begin
  Clear;
  NestLevel := 0;
  EV.Doc := TBuffer.Create(Source, Name);
  try
    if Name <> '' then
      EV.NewName := Name
    else
      EV.NewName := 'source://' + Source;
    FCurrentFile := EV.NewName;
    FRefreshDelay := 0;
    if MasterSet.FrameViewer.IsFrame(EV.Doc) then
    begin {it's a Frameset html file}
      MasterSet.FrameViewer.ParseFrame(Self, EV.Doc, EV.NewName, HandleMeta);
      FreeAndNil(EV.Doc);
      for I := 0 to List.Count - 1 do
        List[I].LoadFiles();
      CalcSizes(Self);
      CheckNoresize(Lower, Upper);
      if FRefreshDelay > 0 then
        SetRefreshTimer;
    end
    else
    begin {it's a non frame file}
      Frame := TfvFrame(AddFrame(nil, ''));
      if RequestEvent then
      begin
        Frame.Source := Source;
        PEV := @EV;
      end
      else
      begin
        Frame.Source := EV.NewName;
        PEV := nil;
        FreeAndNil(EV.Doc);
      end;
      Frame.Destination := Dest;
      Parsed('', '', '');
      CalcSizes(Self);
      Frame.LoadFiles(PEV);
      // now Doc is owned by Frame. We must not free it.
      if PEV <> nil then
        EV.Doc := nil;
      FTitle := Frame.Viewer.DocumentTitle;
      FBaseTarget := Frame.Viewer.BaseTarget;
    end;
  finally
    EV.Doc.Free;
  end;
end;

procedure TFrameSet.RefreshTimerTimer(Sender: Tobject);
var
  S, D: ThtString;
begin
  RefreshTimer.Enabled := False;
  if (Self = MasterSet.FrameViewer.CurFrameSet) then
  begin
    SplitDest(NextFile, S, D);
    FrameViewer.LoadFromFileInternal(S, D);
  end;
end;

{----------------TFrameSetBase.ClearForwards}

procedure TFrameSetBase.ClearForwards;
{clear all the forward items in the history lists}
var
  I, J: Integer;
  Frame: TViewerFrameBase;
  AList: TList;
  Obj: TObject;
begin
  AList := TList.Create;
  for J := 0 to Frames.Count - 1 do
  begin
    Frame := TViewerFrameBase(Frames[J]);
    with Frame do
    begin
      for I := 0 to frHistoryIndex - 1 do
      begin
        Obj := frHistory.Objects[0];
        if Assigned(Obj) and (AList.IndexOf(Obj) < 0) then
          AList.Add(Obj);
        frHistory.Delete(0);
        frPositionHistory.Delete(0);
      end;
      frHistoryIndex := 0;
    end;
  end;
  for J := 0 to Frames.Count - 1 do {now see which Objects are no longer used}
  begin
    Frame := TViewerFrameBase(Frames[J]);
    with Frame do
    begin
      for I := 0 to frHistory.Count - 1 do
      begin
        Obj := frHistory.Objects[I];
        if Assigned(Obj) and (AList.IndexOf(Obj) > -1) then
          AList.Remove(Obj); {remove it if it's there}
      end;
    end;
  end;
  for I := 0 to AList.Count - 1 do {destroy what's left}
    TObject(AList[I]).Free;
  AList.Free;
end;

{----------------TFrameSetBase.UpdateFrameList}

procedure TFrameSetBase.UpdateFrameList;
{Fill Frames with a list of all current TFrames}
begin
  Frames.Clear;
  inherited UpdateFrameList;
end;

{----------------TFrameViewer.Create}

constructor TFVBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 150;
  Width := 150;
  ParentColor := False;
  ParentFont := False;
  ProcessList := TList.Create;
  FLinkAttributes := ThtStringList.Create;
  FViewImages := True;
  FImageCache := ThtImageCache.Create;
  FHistory := ThtStringList.Create;
  FPosition := TList.Create;
  FTitleHistory := ThtStringList.Create;
  FCursor := crIBeam;
  TabStop := False;
  FOptions := [fvPrintTableBackground, fvPrintMonochromeBlack];
  Visited := ThtStringList.Create;

  FCurFrameSet := GetFrameSetClass.Create(Self);
  if fvNoBorder in FOptions then
  begin
    CurFrameSet.OuterBorder := 0;
{$ifndef LCL}
    CurFrameSet.BevelOuter := bvNone;
{$endif}
  end
  else
  begin
    CurFrameSet.OuterBorder := 2;
{$ifndef LCL}
    CurFrameSet.BevelWidth := 2;
    CurFrameSet.BevelOuter := bvLowered;
{$endif}
  end;
  CurFrameSet.Align := alClient;
  CurFrameSet.OnDragDrop := OnDragDrop;
  CurFrameSet.OnDragOver := OnDragOver;
{$ifdef has_StyleElements}
  CurFrameSet.StyleElements := StyleElements;
{$endif}
  InsertControl(CurFrameSet);
end;

//-- BG ---------------------------------------------------------- 25.11.2011 --
constructor TFVBase.CreateCopy(Owner: TComponent; Source: TViewerBase);
var
  Viewer: TFVBase absolute Source;
begin
  inherited;
  if Source is TFVBase then
  begin
    OnBitmapRequest := Viewer.OnBitmapRequest;
    OnBlankWindowRequest := Viewer.OnBlankWindowRequest;
    OnFileBrowse := Viewer.OnFileBrowse;
    OnImageRequest := Viewer.OnImageRequest;
    OnImageRequested := Viewer.OnImageRequested;
    OnPrintHTMLHeader := Viewer.OnPrintHTMLHeader;
    OnPrintHTMLFooter := Viewer.OnPrintHTMLFooter;
    OnRightClick := Viewer.OnRightClick;
    OnViewerClear := Viewer.OnViewerClear;
  end;
end;

//-- BG ---------------------------------------------------------- 25.11.2011 --
function TFVBase.CreateIFrameControl(Sender: TObject; Owner: TComponent): TViewerBase;
begin
  Result := TViewerBaseClass(ClassType).CreateCopy(Owner, Self);
end;

{----------------TFrameViewer.Destroy}

destructor TFVBase.Destroy;
begin
  ProcessList.Free;
  FLinkAttributes.Free;
  FHistory.Free;
  FPosition.Free;
  FTitleHistory.Free;
  Visited.Free;
  FViewerList.Free;
  inherited;
  FImageCache.Free;
end;

{----------------TFrameViewer.Destroy}

destructor TFrameViewer.Destroy;
begin
  UrlRequestStream.Free;
  inherited;
end;

{----------------TFrameViewer.Clear}

procedure TFVBase.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if not Processing then
  begin
    for I := 0 to FHistory.Count - 1 do
      with FHistory do
      begin
        Obj := Objects[0];
        Delete(0);
        if Obj <> CurFrameset then
          ChkFree(Obj);
      end;
    with CurFrameSet do
    begin
      Clear;
{$ifndef LCL}
      BevelOuter := bvLowered;
      BevelWidth := 2;
{$endif}
    end;
    FImageCache.Clear;
    FURL := '';
    FBaseEx := '';
    FHistoryIndex := 0;
    FPosition.Clear;
    FTitleHistory.Clear;
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
    Visited.Clear;
    if Assigned(FViewerList) then
      FViewerList.Clear;
  end;
end;

{----------------TFrameViewer.LoadFromFile}

procedure TFrameViewer.LoadFromFile(const FileName: ThtString);
var
  Name, Dest: ThtString;
begin
  if Processing then
    Exit;

  SplitDest(FileName, Name, Dest);
  if not htFileExists(Name) then
    raise(EhtLoadError.Create('Can''t locate file: ' + htStringToString(Name) ));

  LoadFromFileInternal(Name, Dest);
end;

{----------------TFrameViewer.LoadFromFileInternal}

procedure TFrameViewer.LoadFromFileInternal(const FileName, Dest: ThtString);
var
  OldFrameSet: TFrameSet;
  OldPos: Integer;
  Tmp: TObject;
begin
  BeginProcessing;
  IOResult; {remove any pending file errors}
  SendMessage(Handle, wm_SetRedraw, 0, 0);
  try
    ProcessList.Clear;
    if Assigned(OnSoundRequest) then
      OnSoundRequest(Self, '', 0, True);
    OldPos := 0;
    if CurFrameSet.Viewers.Count = 1 then
    begin
      Tmp := CurFrameSet.Viewers[0];
      if Tmp is THtmlViewer then
        OldPos := THtmlViewer(Tmp).Position;
    end;
    if htCompareStr(CurFrameSet.FCurrentFile, FileName) <> 0 then
    begin
      OldFrameSet := CurFrameSet;
      FCurFrameSet := GetFrameSetClass.Create(Self);
      try
        CurFrameSet.Align := alClient;
        CurFrameSet.Parent := Self;
        CurFrameSet.SendToBack;
        CurFrameSet.LoadFromFile(FileName, Dest);
        CurFrameSet.FCurrentFile := FileName;
      except
        CurFrameSet.Free;
        FCurFrameSet := OldFrameSet;
        raise;
      end;
      CurFrameSet.Visible := True;
      if Visible then
        CurFrameSet.BringToFront;
      OldFrameSet.Visible := False;
      OldFrameSet.UnloadFiles;
      BumpHistory(OldFrameSet, OldPos);
    end
    else
    begin {Same Name}
      CurFrameSet.LoadFromFile(FileName, Dest);
      BumpHistory2(OldPos); {not executed if exception occurs}
    end;
    AddVisitedLink(FileName + Dest);
    CheckVisitedLinks;
  finally
    SendMessage(Handle, wm_SetRedraw, 1, 0);
    EndProcessing;
    Invalidate;
  end;
end;

{----------------TFrameViewer.Load}

procedure TFrameViewer.Load(const SRC: ThtString);
var
  Name, Dest: ThtString;
begin
  SplitDest(SRC, Name, Dest);
  LoadFromFileInternal(Name, Dest);
end;

{----------------TFrameViewer.LoadTargetFromFile}

procedure TFrameViewer.LoadTargetFromFile(const Target, FileName: ThtString);
var
  I: Integer;
  FrameTarget: TFrameBase;
  Name, Dest: ThtString;
begin
  if Processing then
    Exit;

  SplitDest(FileName, Name, Dest);
  I := -1;
  if CurFrameSet.FrameNames.Find(Target, I) then
  begin
    FrameTarget := (CurFrameSet.FrameNames.Objects[I] as TViewerFrameBase);

    if not htFileExists(Name) and not Assigned(OnStreamRequest) then
      raise EhtLoadError.CreateFmt('Can''t locate ''%s''.', [Name]);

    BeginProcessing;
    try
      if FrameTarget is TViewerFrameBase then
        TViewerFrameBase(FrameTarget).frLoadFromFile(Name, Dest, True, False)
      else if FrameTarget is TSubFrameSetBase then
        TSubFrameSetBase(FrameTarget).LoadFromFile(Name, Dest);
    finally
      EndProcessing;
    end;
  end
  else if (Target = '') or
    (htCompareText(Target, '_top') = 0) or
    (htCompareText(Target, '_parent') = 0) or
    (htCompareText(Target, '_self') = 0)
  then
    LoadFromFileInternal(Name, Dest)
  else {_blank or unknown target}
    if Assigned(OnBlankWindowRequest) then
      OnBlankWindowRequest(Self, Target, FileName);
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
function TFrameViewer.GetFrameSetClass: TFrameSetClass;
begin
  Result := TFrameSet;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TFrameViewer.GetSubFrameSetClass: TSubFrameSetClass;
begin
  Result := TSubFrameSet;
end;

{----------------TFrameViewer.Reload}

procedure TFVBase.Reload;
begin
  BeginProcessing;
  try
    ProcessList.Clear;
    if CurFrameSet.List.Count = 0 then
      LoadFromStringInternal( FText, '', '' )
    else
    begin
      CurFrameSet.UnloadFiles;
      CurFrameSet.ReloadFiles(-1);
    end;
    CheckVisitedLinks;
  finally
    EndProcessing;
  end;
end;

procedure TFVBase.Retext;
begin
  BeginProcessing;
  try
//    ProcessList.Clear;
//    CurFrameSet.UnloadFiles;
//    CurFrameSet.ReloadFiles(-1);
    Text := Text;
    CheckVisitedLinks;
  finally
    EndProcessing;
  end;
end;

{----------------TFrameViewer.GetFwdButtonEnabled}

function TFVBase.GetFwdButtonEnabled: Boolean;
var
  I: Integer;
  Frame: TViewerFrameBase;
begin
  Result := fHistoryIndex >= 1;
  if not Result then
    for I := 0 to CurFrameSet.Frames.Count - 1 do
    begin
      Frame := TViewerFrameBase(CurFrameSet.Frames[I]);
      with Frame do
        if frHistoryIndex >= 1 then
        begin
          Result := True;
          Exit;
        end;
    end;
end;

{----------------TFrameViewer.GetBackButtonEnabled}

function TFVBase.GetBackButtonEnabled: Boolean;
var
  I: Integer;
  Frame: TViewerFrameBase;
begin
  Result := HistoryIndex <= fHistory.Count - 2;
  if not Result then
    for I := 0 to CurFrameSet.Frames.Count - 1 do
    begin
      Frame := TViewerFrameBase(CurFrameSet.Frames[I]);
      with Frame do
        if frHistoryIndex <= frHistory.Count - 2 then
        begin
          Result := True;
          Exit;
        end;
    end;
end;

procedure TFVBase.GoFwd;
var
  I, Smallest, Index: Integer;
  Frame, TheFrame: TViewerFrameBase;
begin
  Smallest := 9999;
  Index := 0; TheFrame := nil; {to quiet the warnings}
  for I := 0 to CurFrameSet.Frames.Count - 1 do
  begin
    Frame := TViewerFrameBase(CurFrameSet.Frames[I]);
    with Frame do
      if frHistoryIndex >= 1 then
        with TPositionObjList(frPositionHistory)[frHistoryIndex - 1] do
          if Seq < Smallest then
          begin
            Smallest := Seq;
            TheFrame := Frame;
            Index := frHistoryIndex;
          end;
  end;
  if Smallest < 9999 then
    TheFrame.frSetHistoryIndex(Index - 1)
  else
    SetHistoryIndex(fHistoryIndex - 1);
  if Assigned(OnSoundRequest) then
    OnSoundRequest(Self, '', 0, True);
end;

procedure TFVBase.GoBack;
var
  I, Largest, Index: Integer;
  Frame, TheFrame: TViewerFrameBase;
begin
  Largest := -1;
  Index := 0; TheFrame := nil; {to quiet the warnings}
  for I := 0 to CurFrameSet.Frames.Count - 1 do
  begin
    Frame := TViewerFrameBase(CurFrameSet.Frames[I]);
    with Frame do
      if frHistoryIndex <= frHistory.Count - 2 then
        with TPositionObjList(frPositionHistory)[frHistoryIndex] do
          if Seq > Largest then
          begin
            Largest := Seq;
            TheFrame := Frame;
            Index := frHistoryIndex;
          end;
  end;
  if Largest >= 0 then
    TheFrame.frSetHistoryIndex(Index + 1)
  else
    SetHistoryIndex(fHistoryIndex + 1);
  if Assigned(OnSoundRequest) then
    OnSoundRequest(Self, '', 0, True);
end;

{----------------TFVBase.HotSpotClickHandled}

function TFVBase.HotSpotClickHandled(const FullUrl: ThtString; const Target: ThtString): Boolean;
begin
  Result := False;
  if Assigned(OnHotSpotTargetClick) then
    OnHotSpotTargetClick(Self, Target, FullUrl, Result);
end;

{----------------TFrameViewer.HotSpotClick}

procedure TFrameViewer.HotSpotClick(Sender: TObject; const AnURL: ThtString; var Handled: Boolean);
var
  I: Integer;
  Viewer: THtmlViewer;
  FrameTarget: TFrameBase;
  S, Dest, Query, Target, ExpURL: ThtString;
begin
  Handled := Processing;
  if Handled then
    Exit;

  Viewer := (Sender as THtmlViewer);
  Target := GetViewerTarget(Viewer);
  FLinkAttributes.Text := Viewer.LinkAttributes.Text;
  FLinkText := Viewer.LinkText;

  SplitDest(AnURL, S, Dest);
  SplitQuery(S, Query);
  if (S <> '') and not CurFrameSet.RequestEvent then
    S := Viewer.HTMLExpandFileName(S);
  ExpURL := S + Query + Dest;

  if not HotSpotClickHandled(ExpURL, Target) then
  begin
    Handled := True;
    I := -1;
    if (Target = '') or (htCompareText(Target, '_self') = 0) then {no target or _self target}
    begin
      FrameTarget := Viewer.FrameOwner as TViewerFrameBase;
      if not Assigned(FrameTarget) then
        Exit;
    end
    else if CurFrameSet.FrameNames.Find(Target, I) then
      FrameTarget := (CurFrameSet.FrameNames.Objects[I] as TViewerFrameBase)
    else if htCompareText(Target, '_top') = 0 then
      FrameTarget := CurFrameSet
    else if htCompareText(Target, '_parent') = 0 then
    begin
      FrameTarget := (Viewer.FrameOwner as TViewerFrameBase).Owner as TFrameBase;
      while Assigned(FrameTarget) and not (FrameTarget is TViewerFrameBase)
        and not (FrameTarget is TFrameSetBase) do
        FrameTarget := FrameTarget.Owner as TFrameBase;
    end
    else
    begin
      if Assigned(OnBlankWindowRequest) then
      begin
        AddVisitedLink(ExpURL);
        CheckVisitedLinks;
        OnBlankWindowRequest(Self, Target, ExpURL);
        Handled := True;
      end
      else
        Handled := Target <> ''; {true if can't find target window}
      Exit;
    end;

    FURL := AnURL;
    BeginProcessing;
    if (FrameTarget is TViewerFrameBase) and (CurFrameSet.Viewers.Count = 1) and (S <> '')
      and (htCompareText(S, CurFrameSet.FCurrentFile) <> 0) then
      FrameTarget := CurFrameSet; {force a new FrameSet on name change}
    try
      if FrameTarget is TViewerFrameBase then
        TViewerFrameBase(FrameTarget).frLoadFromFile(S, Dest, True, False)
      else if FrameTarget is TFrameSetBase then
        Self.LoadFromFileInternal(S, Dest)
      else if FrameTarget is TSubFrameSetBase then
        TSubFrameSetBase(FrameTarget).LoadFromFile(S, Dest);
      if Query <> '' then
        AddVisitedLink(ExpURL);
      CheckVisitedLinks;
    finally
      //BG, 05.01.2010: this was the location, where a comment told us that resetting FProcessing was moved before the notification.
      EndProcessing;
    end;
  end;
end;

function TFVBase.GetCurViewerCount: Integer;
begin
  if CurFrameSet <> nil then
    Result := CurFrameSet.Viewers.Count
  else
    Result := 0;
end;

function TFVBase.GetCurViewer(I: Integer): THtmlViewer;
begin
  Result := CurFrameSet.Viewers[I];
end;

{----------------TFrameViewer.HotSpotCovered}

procedure TFrameViewer.HotSpotCovered(Sender: TObject; const SRC: ThtString);
begin
  if Assigned(OnHotSpotTargetCovered) then
    with (Sender as THtmlViewer) do
    begin
      FLinkText := LinkText;
      FLinkAttributes.Text := LinkAttributes.Text;
      OnHotSpotTargetCovered(Sender, Target, Src);
    end;
end;

{----------------TFrameViewer.GetActiveTarget}

function TFVBase.GetViewerTarget(Viewer: THtmlViewer): ThtString;
var
  Done: Boolean;
  FSet: TSubFrameSetBase;
begin
  Result := '';
  if Assigned(Viewer) then
  begin
    Result := Viewer.Target;
    if Result = '' then
      Result := Viewer.BaseTarget;
    Done := False;
    FSet := TViewerFrameBase(Viewer.FrameOwner).LOwner;
    while (Result = '') and Assigned(FSet) and not Done do
    begin
      Result := FSet.FBaseTarget;
      Done := FSet = CurFrameSet;
      if not Done then
        FSet := FSet.LOwner;
    end;
  end;
end;

{----------------TFrameViewer.GetActiveBase}

function TFVBase.GetViewerBase(Viewer: THtmlViewer): ThtString;
var
  Done: Boolean;
  FSet: TSubFrameSetBase;
begin
  Result := '';
  if Assigned(Viewer) then
  begin
    Result := Viewer.Base;
    Done := False;
    FSet := TViewerFrameBase(Viewer.FrameOwner).LOwner;
    while (Result = '') and Assigned(FSet) and not Done do
    begin
      Result := FSet.FBase;
      Done := FSet = CurFrameSet;
      if not Done then
        FSet := FSet.LOwner;
    end;
  end;
end;

{----------------TFrameViewer.HTMLExpandFilename}

function TFrameViewer.HtmlExpandFilename(const Filename, CurrentFilename: ThtString): ThtString;
var
  Viewer: THtmlViewer;
begin
  Result := HTMLServerToDos(Trim(Filename), ServerRoot);
  if not IsAbsolutePath(Result) then
  begin
    Viewer := ActiveViewer;
    if Viewer <> nil then
      Result := Viewer.HTMLExpandFilename(Result, CurFrameSet.FCurrentFile);
  end;
end;

function TFVBase.GetBase: ThtString;
begin
  Result := CurFrameSet.FBase;
end;

procedure TFVBase.SetBase(Value: ThtString);
begin
  CurFrameSet.FBase := Value;
  FBaseEx := Value;
end;

function TFVBase.GetBaseTarget: ThtString;
begin
  Result := CurFrameSet.FBaseTarget;
end;

function TFVBase.GetTitle: ThtString;
begin
  Result := CurFrameSet.FTitle;
end;

function TFVBase.GetUseQuirksMode: Boolean;
begin
  Result := (ActiveViewer <> nil) and (ActiveViewer.UseQuirksMode);
end;

function TFVBase.GetCurrentFile: ThtString;
begin
  Result := CurFrameSet.FCurrentFile;
end;

{----------------TFrameViewer.GetActiveViewer}

function TFVBase.GetActiveViewer: THtmlViewer;
begin
  Result := CurFrameSet.GetActive;
end;

{----------------TFrameViewer.BumpHistory}

procedure TFVBase.BumpHistory(OldFrameSet: TFrameSetBase; OldPos: Integer);
{OldFrameSet never equals CurFrameSet when this method called}
var
  I: Integer;
  Obj: TObject;
begin
  if (HistoryMaxCount > 0) and (CurFrameSet.FCurrentFile <> '') then
  begin
    if FHistory.Count > 0 then
    begin
      FHistory.Strings[FHistoryIndex] := OldFrameSet.FCurrentFile;
      FHistory.Objects[FHistoryIndex] := OldFrameSet;
      FTitleHistory[FHistoryIndex] := OldFrameSet.FTitle;
      FPosition[FHistoryIndex] := TObject(OldPos);
      OldFrameSet.StopTimers;
      OldFrameSet.Parent := nil;
      OldFrameSet.ClearForwards;
    end
    else
      OldFrameSet.Free;
    for I := 0 to FHistoryIndex - 1 do
    begin
      Obj := FHistory.Objects[0];
      FHistory.Delete(0);
      ChkFree(Obj);
      FTitleHistory.Delete(0);
      FPosition.Delete(0);
    end;
    FHistoryIndex := 0;
    FHistory.Insert(0, CurFrameSet.FCurrentFile);
    FHistory.Objects[0] := CurFrameSet;
    FTitleHistory.Insert(0, CurFrameSet.FTitle);
    FPosition.Insert(0, nil);
    if FHistory.Count > HistoryMaxCount then
    begin
      Obj := FHistory.Objects[HistoryMaxCount];
      FHistory.Delete(HistoryMaxCount);
      ChkFree(Obj);
      FTitleHistory.Delete(HistoryMaxCount);
      FPosition.Delete(HistoryMaxCount);
    end;
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
  end
  else
    OldFrameSet.Free;
end;

{----------------TFrameViewer.BumpHistory1}

procedure TFvBase.BumpHistory1(const FileName, Title: ThtString; OldPos: Integer; ft: ThtmlFileType);
{This variation called when CurFrameSet contains only a single viewer before and after the change}
var
  I: Integer;
  Obj: TObject;
begin
  if (HistoryMaxCount > 0) and (Filename <> '') then
  begin
    if (FHistory.Count > 0) then
    begin
      FHistory.Strings[FHistoryIndex] := Filename;
      FHistory.Objects[FHistoryIndex] := CurFrameSet;
      FTitleHistory[FHistoryIndex] := Title;
      FPosition[FHistoryIndex] := TObject(OldPos);
    end;
    for I := 0 to FHistoryIndex - 1 do
    begin
      Obj := FHistory.Objects[0];
      FHistory.Delete(0);
      ChkFree(Obj);
      FTitleHistory.Delete(0);
      FPosition.Delete(0);
    end;
    FHistoryIndex := 0;
    FHistory.Insert(0, CurFrameSet.FCurrentFile);
    FHistory.Objects[0] := CurFrameSet;
    FTitleHistory.Insert(0, CurFrameSet.FTitle);
    FPosition.Insert(0, nil);
    if FHistory.Count > HistoryMaxCount then
    begin
      Obj := FHistory.Objects[HistoryMaxCount];
      FHistory.Delete(HistoryMaxCount);
      ChkFree(Obj);
      FTitleHistory.Delete(HistoryMaxCount);
      FPosition.Delete(HistoryMaxCount);
    end;
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
  end;
end;

{----------------TFrameViewer.BumpHistory2}

procedure TFvBase.BumpHistory2(OldPos: Integer);
{CurFrameSet has not changed when this method called}
var
  I: Integer;
  Obj: TObject;
begin
  if (HistoryMaxCount > 0) and (CurFrameSet.FCurrentFile <> '') then
  begin
    if (FHistory.Count > 0) then
    begin
      FHistory.Strings[FHistoryIndex] := CurFrameSet.FCurrentFile;
      FHistory.Objects[FHistoryIndex] := CurFrameSet;
      FTitleHistory[FHistoryIndex] := CurFrameSet.FTitle;
      FPosition[FHistoryIndex] := TObject(OldPos);
    end;
    for I := 0 to FHistoryIndex - 1 do
    begin
      Obj := FHistory.Objects[0];
      FHistory.Delete(0);
      ChkFree(Obj);
      FTitleHistory.Delete(0);
      FPosition.Delete(0);
    end;
    FHistoryIndex := 0;
    FHistory.Insert(0, CurFrameSet.FCurrentFile);
    FHistory.Objects[0] := CurFrameSet;
    FTitleHistory.Insert(0, CurFrameSet.FTitle);
    FPosition.Insert(0, nil);
    if FHistory.Count > HistoryMaxCount then
    begin
      Obj := FHistory.Objects[HistoryMaxCount];
      FHistory.Delete(HistoryMaxCount);
      ChkFree(Obj);
      FTitleHistory.Delete(HistoryMaxCount);
      FPosition.Delete(HistoryMaxCount);
    end;
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
  end;
end;

{----------------TFrameViewer.SetHistoryIndex}

procedure TFVBase.SetHistoryIndex(Value: Integer);
var
  FrameSet, FrameSet1: TFrameSetBase;
  Tmp: TObject;
begin
  if (Value <> FHistoryIndex) and (Value >= 0) and (Value < FHistory.Count) and not Processing then
  begin
    if CurFrameSet.Viewers.Count > 0 then
      Tmp := CurFrameSet.Viewers[0]
    else
      Tmp := nil;
    if CurFrameSet.FCurrentFile <> '' then
    begin
    {Objects[FHistoryIndex] should have CurFrameSet here}
      FTitleHistory[FHistoryIndex] := CurFrameSet.FTitle;
      if (Tmp is THtmlViewer) then
        FPosition[FHistoryIndex] := TObject((Tmp as THtmlViewer).Position)
      else
        FPosition[FHistoryIndex] := nil;
    end;
    FrameSet := FHistory.Objects[Value] as TFrameSetBase;
    if FrameSet <> CurFrameSet then
    begin
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        FrameSet1 := CurFrameSet; {swap framesets}
        FCurFrameSet := FrameSet;
        CurFrameSet.OldWidth := 0; {encourage recalc of internal layout}
        CurFrameSet.Visible := False;
        Self.InsertControl(CurFrameSet);
        if CurFrameSet.Viewers.Count = 1 then
          CurFrameSet.ReloadFiles(Integer(FPosition[Value]))
        else
          CurFrameSet.ReloadFiles(-1);
        CurFrameSet.Visible := True;
        FrameSet1.Unloadfiles;
        Self.RemoveControl(FrameSet1);
      finally
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Invalidate;
      end;
    end
    else
    begin
      if (Tmp is THtmlViewer) then
        TViewerFrameBase(THtmlViewer(Tmp).FrameOwner).ReloadFile(FHistory[Value],
          Integer(FPosition[Value]));
    end;

    FHistoryIndex := Value;
    if Assigned(OnHistoryChange) then
      OnHistoryChange(Self);
    CheckVisitedLinks;
  end;
end;

{----------------TFrameViewer.ChkFree}

procedure TFVBase.ChkFree(Obj: TObject);
{Frees a TFrameSetBase only if it no longer exists in FHistory}
var
  I: Integer;
begin
  for I := 0 to FHistory.Count - 1 do
    if Obj = FHistory.Objects[I] then
      Exit;
  (Obj as TFrameSetBase).Free;
end;

{----------------TFrameViewer.ClearHistory}

procedure TFVBase.ClearHistory;
var
  I: Integer;
  Obj: TObject;
  DidSomething: Boolean;
begin
  DidSomething := FHistory.Count > 0;
  for I := FHistory.Count - 1 downto 0 do
  begin
    Obj := FHistory.Objects[I];
    FHistory.Delete(I);
    if Obj <> CurFrameSet then
      ChkFree(Obj);
  end;
  if Assigned(FCurFrameSet) then
    for I := 0 to CurFrameSet.Frames.Count - 1 do
      with TViewerFrameBase(CurFrameSet.Frames[I]) do
      begin
        DidSomething := DidSomething or (frHistory.Count > 0);
        frHistoryIndex := 0;
        frHistory.Clear;
        frPositionHistory.Clear;
      end;
  FHistory.Clear;
  FTitleHistory.Clear;
  FPosition.Clear;
  FHistoryIndex := 0;
  if DidSomething and Assigned(OnHistoryChange) then
    OnHistoryChange(Self);
end;

//-- BG ------------------------------------------------------- 10.12.2022 --
procedure TFVBase.CMParentColorChanged(var Message: TMessage);
var
  I: Integer;
begin
  if FInCMParentColorChanged = 0 then // in FPC inherited produces recursive calls to CMParentColorChanged when ParentColor changed to true
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].ParentColor := ParentColor;

  inherited;
end;

//-- BG ------------------------------------------------------- 10.12.2022 --
procedure TFVBase.CMParentFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].ParentFont := ParentFont;

  inherited;
end;

procedure TFVBase.SetOnProgress(const Handler: ThtProgressEvent);
var
  I: Integer;
begin
  inherited;
  with CurFrameSet do
    for I := 0 to Viewers.Count - 1 do
      with THtmlViewer(Viewers[I]) do
        OnProgress := Handler;
end;

procedure TFVBase.SetOnDragDrop(const Value: TDragDropEvent);
var
  I: Integer;
  E: TDragDropEvent;
begin
  inherited;
  if Assigned(Value) then
    E := ViewerDragDrop
  else
    E := nil;

  if Assigned(FCurFrameSet) then
    CurFrameSet.OnDragDrop := E;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnDragDrop := E;
end;

procedure TFVBase.SetOnDragOver(const Value: TDragOverEvent);
var
  I: Integer;
  E: TDragOverEvent;
begin
  inherited;
  if Assigned(Value) then
    E := ViewerDragOver
  else
    E := nil;

  if Assigned(FCurFrameSet) then
    CurFrameSet.OnDragOver := E;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnDragOver := E;
end;

procedure TFVBase.SetOnBitmapRequested(const Handler: TGottenBitmapEvent);
var
  I: Integer;
begin
  inherited;
  with CurFrameSet do
    for I := 0 to Viewers.Count - 1 do
      THtmlViewer(Viewers[I]).OnBitmapRequested := Handler;
end;

procedure TFVBase.SetOnImageRequested(const Handler: TGottenImageEvent);
var
  I: Integer;
begin
  inherited;
  with CurFrameSet do
    for I := 0 to Viewers.Count - 1 do
      THtmlViewer(Viewers[I]).OnImageRequested := Handler;
end;

function TFVBase.ViewerFromTarget(const Target: ThtString): THtmlViewer;
var
  I: Integer;
begin
  I := -1;
  if Assigned(FCurFrameSet) and Assigned(CurFrameSet.FrameNames)
    and CurFrameSet.FrameNames.Find(Target, I)
    and (CurFrameSet.FrameNames.Objects[I] <> nil)
    and Assigned((CurFrameSet.FrameNames.Objects[I] as TViewerFrameBase).Viewer) then
    Result := TViewerFrameBase(CurFrameSet.FrameNames.Objects[I]).Viewer as THtmlViewer
  else
    Result := nil;
end;

procedure TFVBase.RePaint;
begin
  if Assigned(FCurFrameSet) then
    CurFrameSet.RePaint;
end;

//-- BG ---------------------------------------------------------- 12.05.2013 --
function FvOptionsToHtOptions(Value: TFrameViewerOptions; HtOptions: THtmlViewerOptions = []): THtmlViewerOptions;
begin
  if (fvOverLinksActive in Value) then
    Include(HtOptions, htOverLinksActive)
  else
    Exclude(HtOptions, htOverLinksActive);

  if (fvNoLinkUnderline in Value) then
    Include(HtOptions, htNoLinkUnderline)
  else
    Exclude(HtOptions, htNoLinkUnderline);

  if (fvPrintTableBackground in Value) then
    Include(HtOptions, htPrintTableBackground)
  else
    Exclude(HtOptions, htPrintTableBackground);

  if (fvPrintBackground in Value) then
    Include(HtOptions, htPrintBackground)
  else
    Exclude(HtOptions, htPrintBackground);

  if (fvPrintMonochromeBlack in Value) then
    Include(HtOptions, htPrintMonochromeBlack)
  else
    Exclude(HtOptions, htPrintMonochromeBlack);

  if (fvShowVScroll in Value) then
    Include(HtOptions, htShowVScroll)
  else
    Exclude(HtOptions, htShowVScroll);

  if (fvNoWheelMouse in Value) then
    Include(HtOptions, htNoWheelMouse)
  else
    Exclude(HtOptions, htNoWheelMouse);

  if (fvShowDummyCaret in Value) then
    Include(HtOptions, htShowDummyCaret)
  else
    Exclude(HtOptions, htShowDummyCaret);

  if (fvNoLinkHilite in Value) then
    Include(HtOptions, htNoLinkHilite)
  else
    Exclude(HtOptions, htNoLinkHilite);

  //BG, 03.01.2010: added fvNoFocusRect handling according to mik kvitchko's patch MK20091107
  if (fvNoFocusRect in Value) then
    Include(HtOptions, htNoFocusRect)
  else
    Exclude(HtOptions, htNoFocusRect);

  if (fvAllowHotSpotDblClick in Value) then
    Include(HtOptions, htAllowHotSpotDblClick)
  else
    Exclude(HtOptions, htAllowHotSpotDblClick);

  Result := HtOptions;
end;

procedure TFVBase.SetOptions(Value: TFrameViewerOptions);
var
  I: Integer;
begin
  if (fvNoBorder in FOptions) <> (fvNoBorder in Value) then
    if fvNoBorder in Value then
    begin
      CurFrameSet.OuterBorder := 0;
{$ifndef LCL}
      CurFrameSet.BevelOuter := bvNone;
{$endif}
      CurFrameSet.BorderSize := 0;
    end
    else
    begin
{$ifndef LCL}
      CurFrameSet.BevelWidth := 2;
      CurFrameSet.BevelOuter := bvLowered;
{$endif}
      CurFrameSet.BorderSize := 2;
    end;

  for I := 0 to CurFrameSet.Viewers.Count - 1 do
    with THtmlViewer(CurFrameSet.Viewers[I]) do
    begin
      HtOptions := FvOptionsToHtOptions(Value, HtOptions);

      //BG, 03.01.2010: added fvNoFocusRect handling according to mik kvitchko's patch MK20091107
      if (fvNoFocusRect in Value) or (fvNoBorder in Value) then
        BorderStyle := htNone
      else
        BorderStyle := htFocused;
    end;
  FOptions := Value;
end;

procedure TFVBase.AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: ThtString);
begin
  (FrameSet as TSubFrameSetBase).AddFrame(Attr, FName);
end;

function TFVBase.CreateSubFrameSet(FrameSet: TObject): TObject;
var
  NewFrameSet, FS: TSubFrameSetBase;
begin
  FS := (FrameSet as TSubFrameSetBase);
  NewFrameSet := GetSubFrameSetClass.CreateIt(FS, CurFrameSet);
  FS.List.Add(NewFrameSet);
  FS.InsertControl(NewFrameSet);
  Result := NewFrameSet;
end;

//-- BG ---------------------------------------------------------- 25.11.2011 --
constructor TFrameViewer.CreateCopy(Owner: TComponent; Source: TViewerBase);
var
  Viewer: TFrameViewer absolute Source;
begin
  inherited;
  if Source is TFrameViewer then
  begin
    OnBufferRequest := Viewer.OnBufferRequest;
    OnFileRequest := Viewer.OnFileRequest;
    OnFormSubmit := Viewer.OnFormSubmit;
    OnStreamRequest := Viewer.OnStreamRequest;
    OnStringsRequest := Viewer.OnStringsRequest;
  end;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
function TFVBase.CreateViewer(Owner: THtmlFrameBase): THtmlViewer;
begin
  Result := GetViewerClass.CreateCopy(Owner, Self); {the Viewer for the frame}
{$ifdef DEBUG}
  {$if defined(LCL) or defined(Compiler22_Plus)}
  Result.Name := Name + '_Viewer' + IntToHex(Integer(Result));
  {$else}
  Result.Name := Name + '_Viewer' + IntToHex(Integer(Result), 8);
  {$ifend}
{$endif}
  Result.ViewImages := ViewImages;
  Result.SetImageCache(FImageCache);
  Result.HtOptions := FvOptionsToHtOptions(FOptions, Result.HtOptions);
  Result.OnImageRequest := DoGetImage;
  Result.OnFormSubmit := DoFormSubmitEvent;
//  Result.OnMetaRefresh := RefreshEvent;
  Result.OnPrintHtmlFooter := OnPrintHtmlFooter;
  Result.OnPrintHtmlHeader := OnPrintHtmlHeader;
  Result.OnRightClick := OnRightClick;
  Result.OnCreateIFrameControl := CreateIFrameControl;
  Result.QuirksMode := QuirksMode;
{$ifdef has_StyleElements}
  Result.StyleElements := StyleElements;
{$endif}
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TFrameViewer.CurFrameSet: TFrameSet;
begin
  Result := TFrameSet(inherited CurFrameSet);
end;

procedure TFVBase.DoAttributes(FrameSet: TObject; Attr: TAttributeList);
begin
  (FrameSet as TSubFrameSetBase).DoAttributes(Attr);
end;

//-- BG ---------------------------------------------------------- 23.03.2012 --
procedure TFVBase.DoGetImage(Sender: TObject; const SRC: ThtString; var Stream: TStream);
begin
  if Assigned(OnImageRequest) then
    OnImageRequest(Sender, SRC, Stream);
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
procedure TFrameViewer.DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType,
  Method: ThtString; Results: ThtStringList);
begin
  if Assigned(FOnFormSubmit) then
    FOnFormSubmit(Sender, Action, Target, EncType, Method, Results);
end;

{----------------TFVBase.AddVisitedLink}

procedure TFVBase.AddVisitedLink(const S: ThtString);
var
  I: Integer;
begin
  if VisitedMaxCount = 0 then
    Exit;
  I := Visited.IndexOf(S);
  if I = 0 then
    Exit
  else if I > 0 then
    Visited.Delete(I); {thus moving it to the top}
  Visited.Insert(0, S);
  for I := Visited.Count - 1 downto VisitedMaxCount do
    Visited.Delete(I);
end;

{----------------TFrameViewer.CheckVisitedLinks}

procedure TFrameViewer.CheckVisitedLinks;
var
  I, J, K: Integer;
  S, S1, Src: ThtString;
  Viewer: THtmlViewer;
  RequestEvent: Boolean;
begin
  if VisitedMaxCount = 0 then
    Exit;
  RequestEvent := CurFrameSet.RequestEvent;
  for K := 0 to CurFrameSet.Viewers.Count - 1 do
  begin
    Viewer := THtmlViewer(CurFrameSet.Viewers[K]);
    if RequestEvent then
      Src := TViewerFrameBase(Viewer.FrameOwner).Source;
    for I := 0 to Visited.Count - 1 do
    begin
      S := Visited[I];
      for J := 0 to Viewer.LinkList.Count - 1 do
        with Viewer.LinkList[J] do
        begin
          if not RequestEvent then
          begin
            if (Url <> '') and (Url[1] = '#') then
              S1 := Viewer.CurrentFile + Url
            else
              S1 := Viewer.HTMLExpandFilename(Url);
          end
          else if (Url <> '') and (Url[1] = '#') then
            S1 := Src + Url
          else
            S1 := URL;
          if htCompareText(S, S1) = 0 then
            Visited := True;
        end;
    end;
    Viewer.Invalidate;
  end;
end;

{----------------TFrameViewer.DoURLRequest}

procedure TFrameViewer.DoURLRequest(Sender: TObject; const SRC: ThtString; var RStream: TStream);
var
  NewName: ThtString;
  Doc: TBuffer;
begin
  if CurFrameSet.TriggerEvent(Src, NewName, Doc) then
  begin
    if not Assigned(UrlRequestStream) then
      UrlRequestStream := TMemoryStream.Create;
    if Doc <> nil then
      Doc.AssignTo(UrlRequestStream)
    else
      UrlRequestStream.LoadFromFile( htStringToString(NewName));
    RStream := UrlRequestStream;
  end;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
function TFVBase.GetViewerClass: THtmlViewerClass;
begin
  Result := THtmlViewer;
end;

{----------------TFrameViewer.GetViewers:}

function TFVBase.GetViewers: ThtStringList;
var
  I: Integer;
  S: ThtString;
  AFrame: TViewerFrameBase;
  Viewer: THtmlViewer;
  Pt1, Pt2: TPoint;
begin
  if not Assigned(FViewerList) then
    FViewerList := ThtStringList.Create
  else
    FViewerList.Clear;
  for I := 0 to CurFrameSet.Viewers.Count - 1 do
  begin
    Viewer := CurFrameSet.Viewers[I];
    if Viewer.SectionList.Count > 0 then
    begin
      S := '';
      AFrame := TViewerFrameBase(Viewer.FrameOwner);
      Pt1 := AFrame.ClientToScreen(Point(0, 0));
      Pt2 := CurFrameSet.ClientToScreen(Point(0, 0));
      if Pt1.X <= Pt2.X + 2 then
        S := S + 'l';
      if Pt1.Y <= Pt2.Y + 2 then
        S := S + 't';
      Pt1 := AFrame.ClientToScreen(Point(AFrame.ClientWidth, AFrame.ClientHeight));
      Pt2 := CurFrameSet.ClientToScreen(Point(CurFrameSet.ClientWidth, CurFrameSet.ClientHeight));
      if Pt1.X >= Pt2.X - 2 then
        S := S + 'r';
      if Pt1.Y >= Pt2.Y - 2 then
        S := S + 'b';
      FViewerList.AddObject(S, Viewer);
    end;
  end;
  Result := FViewerList;
end;

procedure TFVBase.SetViewImages(Value: Boolean);
var
  I: Integer;
begin
  if (FViewImages <> Value) and not Processing then
  begin
    FViewImages := Value;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].ViewImages := Value;
  end;
end;

procedure TFVBase.SetImageCacheCount(const Value: Integer);
var
  I: Integer;
begin
  if (Value <> ImageCacheCount) and not Processing then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].ImageCacheCount := Value;
  end;
end;

procedure TFVBase.SetLoadCursor(const Value: TCursor);
var
  I: Integer;
begin
  if (Value <> LoadCursor) and not Processing then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].LoadCursor := Value;
  end;
end;

{$ifdef DEBUG}
procedure TFVBase.SetName(const NewName: TComponentName);
begin
  inherited;
  {$if defined(LCL) or defined(Compiler22_Plus)}
  FCurFrameSet.Name := NewName + '_CurFrameSet' + IntToHex(Integer(FCurFrameSet));
  {$else}
  FCurFrameSet.Name := NewName + '_CurFrameSet' + IntToHex(Integer(FCurFrameSet), 8);
  {$ifend}
end;
{$endif}

{----------------TFVBase.SetNoSelect}

procedure TFVBase.SetNoSelect(const Value: Boolean);
var
  I: Integer;
begin
  if Value <> NoSelect then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].NoSelect := Value;
  end;
end;

procedure TFVBase.SetOnBitmapRequest(const Handler: TGetBitmapEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnBitmapRequest := Handler;
end;

procedure TFVBase.SetOnMeta(const Handler: TMetaType);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnMeta := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TFVBase.SetOnLink(const Handler: TLinkType);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnLink := Handler;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TFVBase.SetOnScript(const Handler: TScriptEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnScript := Handler;
end;

procedure TFVBase.SetOnImageOver(const Handler: TImageOverEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnImageOver := Handler;
end;

procedure TFVBase.SetOnImageClick(const Handler: TImageClickEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnImageClick := Handler;
end;

procedure TFVBase.SetOnRightClick(Handler: TRightClickEvent);
var
  I: Integer;
begin
  FOnRightClick := Handler;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnRightClick := Handler;
end;

procedure TFVBase.SetOnObjectFocus(const Handler: ThtObjectEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnObjectFocus := Handler;
end;

procedure TFVBase.SetOnObjectBlur(const Handler: ThtObjectEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnObjectBlur := Handler;
end;

procedure TFVBase.SetOnObjectChange(const Handler: ThtObjectEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnObjectChange := Handler;
end;

procedure TFVBase.SetOnFileBrowse(Handler: TFileBrowseEvent);
var
  I: Integer;
begin
  FOnFileBrowse := Handler;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnFileBrowse := Handler;
end;

procedure TFVBase.SetOnObjectClick(const Handler: TObjectClickEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnObjectClick := Handler;
end;

procedure TFVBase.SetOnMouseDouble(const Handler: TMouseEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnMouseDouble := Handler;
end;

procedure TFVBase.SetPrintMarginLeft(const Value: Double);
var
  I: Integer;
begin
  if PrintMarginLeft <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].PrintMarginLeft := Value;
  end;
end;

procedure TFVBase.SetPrintMarginRight(const Value: Double);
var
  I: Integer;
begin
  if PrintMarginRight <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].PrintMarginRight := Value;
  end;
end;

procedure TFVBase.SetPrintMarginTop(const Value: Double);
var
  I: Integer;
begin
  if PrintMarginTop <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].PrintMarginTop := Value;
  end;
end;

procedure TFVBase.SetPrintMarginBottom(const Value: Double);
var
  I: Integer;
begin
  if PrintMarginBottom <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].PrintMarginBottom := Value;
  end;
end;

procedure TFVBase.SetPrintScale(const Value: Double);
var
  I: Integer;
begin
  if PrintScale <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].PrintScale := Value;
  end;
end;

//-- BG ---------------------------------------------------------- 25.10.2012 --
procedure TFVBase.SetQuirksMode(const AValue: THtQuirksMode);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].QuirksMode := AValue;
end;

procedure TFVBase.SetMarginWidth(const Value: Integer);
var
  I: Integer;
begin
  if MarginWidth <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].MarginWidth := Value;
  end;
end;

procedure TFVBase.SetMarginHeight(const Value: Integer);
var
  I: Integer;
begin
  if MarginHeight <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].MarginHeight := Value;
  end;
end;

procedure TFVBase.SetOnPrintHeader(const Handler: TPagePrinted);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPrintHeader := Handler;
end;

procedure TFVBase.SetOnPrintFooter(const Handler: TPagePrinted);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPrintFooter := Handler;
end;

procedure TFVBase.SetOnPrintHtmlHeader(Handler: THtmlPagePrinted);
var
  I: Integer;
begin
  FOnPrintHtmlHeader := Handler;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPrintHtmlHeader := Handler;
end;

procedure TFVBase.SetOnPrintHtmlFooter(Handler: THtmlPagePrinted);
var
  I: Integer;
begin
  FOnPrintHtmlFooter := Handler;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPrintHtmlFooter := Handler;
end;

procedure TFVBase.SetVisitedMaxCount(const Value: Integer);
var
  I, J: Integer;
begin
  if Value < 0 then
  begin
    SetVisitedMaxCount(0);
    exit;
  end;

  if Value <> VisitedMaxCount then
  begin
    inherited;
    if Visited <> nil then
      if VisitedMaxCount = 0 then
      begin
        Visited.Clear;
        for I := 0 to GetCurViewerCount - 1 do
          with CurViewer[I] do
            for J := 0 to SectionList.LinkList.Count - 1 do
              TFontObj(LinkList[J]).Visited := False;
        RePaint;
      end
      else
      begin
        for I := Visited.Count - 1 downto VisitedMaxCount do
          Visited.Delete(I);
      end;
  end;
end;

procedure TFVBase.StyleChanged;
begin
  inherited;
  // abstract method and we have nothing to here.
end;

procedure TFVBase.SetDefBackground(const Value: TColor);
var
  I: Integer;
begin
  if DefBackground <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefBackground := Value;

    if not ParentColor then
      Color := Value;
  end;
end;

{$ifdef has_StyleElements}
procedure TFVBase.UpdateStyleElements;
var
  I: Integer;
begin
  CurFrameSet.StyleElements := StyleElements;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].StyleElements := StyleElements;
  inherited UpdateStyleElements;
end;
{$endif}

procedure TFVBase.SetFontName(const Value: TFontName);
var
  I: Integer;
begin
  if CompareText(Value, DefFontName) <> 0 then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefFontName := DefFontName;
  end;
end;

procedure TFVBase.SetPreFontName(const Value: TFontName);
var
  I: Integer;
begin
  if CompareText(Value, DefPreFontName) <> 0 then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefPreFontName := DefPreFontName;
  end;
end;

procedure TFVBase.SetFontSize(const Value: Integer);
var
  I: Integer;
begin
  if DefFontSize <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefFontSize := DefFontSize;
  end;
end;

procedure TFVBase.SetFontColor(const Value: TColor);
var
  I: Integer;
begin
  if DefFontColor <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefFontColor := DefFontColor;
  end;
end;

procedure TFVBase.SetHotSpotColor(const Value: TColor);
var
  I: Integer;
begin
  if DefHotSpotColor <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefHotSpotColor := DefHotSpotColor;
  end;
end;

procedure TFVBase.SetActiveColor(const Value: TColor);
var
  I: Integer;
begin
  if DefOverLinkColor <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefOverLinkColor := DefOverLinkColor;
  end;
end;

procedure TFVBase.SetVisitedColor(const Value: TColor);
var
  I: Integer;
begin
  if DefVisitedLinkColor <> Value then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].DefVisitedLinkColor := DefVisitedLinkColor;
  end;
end;

procedure TFVBase.SetHistoryMaxCount(const Value: Integer);
var
  I: Integer;
begin
  if (Value = HistoryMaxCount) or (Value < 0) then
    Exit;

  if (Value = HistoryMaxCount) or (Value < 0) then
    Exit;

  ClearHistory;
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    with CurViewer[I] do
    begin
      ClearHistory;
      HistoryMaxCount := Self.HistoryMaxCount;
    end;
end;

procedure TFVBase.SetCursor(Value: TCursor);
var
  I: Integer;
begin
  if Value = OldThickIBeamCursor then
    Value := crIBeam;
  if (FCursor <> Value) then
  begin
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].Cursor := Value;
    FCursor := Value;
  end;
end;

procedure TFVBase.SetOnPanelCreate(const Handler: TPanelCreateEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPanelCreate := Handler;
end;

procedure TFVBase.SetOnPanelDestroy(const Handler: TPanelDestroyEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPanelDestroy := Handler;
end;

procedure TFVBase.SetOnPanelPrint(const Handler: TPanelPrintEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnPanelPrint := Handler;
end;

procedure TFVBase.SetOnParseBegin(const Handler: TParseEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnParseBegin := Handler;
end;

procedure TFVBase.SetOnParseEnd(const Handler: TNotifyEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnParseEnd := Handler;
end;

function TFVBase.GetSelLength: Integer;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    Result := AViewer.SelLength
  else
    Result := 0;
end;

procedure TFVBase.SetSelLength(Value: Integer);
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    AViewer.SelLength := Value;
end;

function TFVBase.GetSelStart: Integer;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    Result := AViewer.SelStart
  else
    Result := 0;
end;

procedure TFVBase.SetSelStart(Value: Integer);
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    AViewer.SelStart := Value;
end;

procedure TFVBase.SetText(const Value: ThtString);
begin
  if csLoading in ComponentState then
    FText := Value
  else
    LoadFromString(Value);
end;

procedure TFVBase.SetCharset(const Value: TFontCharset);
var
  I: Integer;
begin
  if (Charset <> Value) then
  begin
    inherited;
    for I := 0 to GetCurViewerCount - 1 do
      CurViewer[I].Charset := Charset;
  end;
end;

procedure TFVBase.SetOnObjectTag(const Handler: TObjectTagEvent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to GetCurViewerCount - 1 do
    CurViewer[I].OnObjectTag := Handler;
end;

{----------------TFVBase.GetOurPalette:}

function TFVBase.GetOurPalette: HPalette;
begin
  if ColorBits = 8 then
    Result := CopyPalette(ThePalette)
  else
    Result := 0;
end;

{----------------TFVBase.SetOurPalette}

procedure TFVBase.SetOurPalette(Value: HPalette);
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

//{----------------TFVBase.SetDither}
//
//procedure TFVBase.SetDither(Value: Boolean);
//begin
//  if (Value <> FDither) and (ColorBits = 8) then
//  begin
//    FDither := Value;
//  end;
//end;

function TFVBase.GetCaretPos: Integer;
var
  Vw: THtmlViewer;
begin
  Vw := GetActiveViewer;
  if Assigned(Vw) then
    Result := Vw.CaretPos
  else
    Result := 0;
end;

procedure TFVBase.SetCaretPos(Value: Integer);
var
  Vw: THtmlViewer;
begin
  Vw := GetActiveViewer;
  if Assigned(Vw) then
    Vw.CaretPos := Value;
end;

function TFVBase.GetSelText: UnicodeString;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    Result := AViewer.SelText
  else
    Result := '';
end;

function TFVBase.GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
var
  AViewer: THtmlViewer;
begin
  if BufSize <= 0 then
    Result := 0
  else
  begin
    AViewer := GetActiveViewer;
    if Assigned(AViewer) then
      Result := AViewer.GetSelTextBuf(Buffer, BufSize)
    else
    begin
      Buffer[0] := #0;
      Result := 1;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 13.03.2011 --
function TFVBase.IsFrame(Doc: TBuffer): Boolean;
var
  Parser: THtmlParser;
begin
  Parser := THtmlParser.Create(Doc);
  try
    Result := Parser.IsFrame(Self);
  finally
    Parser.Free;
  end;
end;

procedure TFVBase.SetFocus;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Showing then
    if Assigned(AViewer) and AViewer.Showing and AViewer.CanFocus then
      try
        AViewer.SetFocus;
      except {just in case}
        inherited SetFocus;
      end
    else
      inherited SetFocus;
end;

{----------------TFVBase.SetProcessing}

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TFVBase.BeginProcessing;
begin
  FProcessing := True;
  if Assigned(OnProcessing) then
    OnProcessing(Self, True);
end;

function TFVBase.GetProcessing: Boolean;
begin
  Result := FProcessing or FViewerProcessing;
end;

procedure TFVBase.SetProcessing(Local, Viewer: Boolean);
var
  Change: Boolean;
begin
  Change := (Local or Viewer <> FProcessing or FViewerProcessing);
  FProcessing := Local;
  FViewerProcessing := Viewer;
  if Change and Assigned(OnProcessing) then
    OnProcessing(Self, Local or Viewer);
end;

procedure TFVBase.CheckProcessing(Sender: TObject; ProcessingOn: Boolean);
begin
  with ProcessList do
  begin
    if ProcessingOn then
    begin
      if IndexOf(Sender) = -1 then
        Add(Sender);
    end
    else
      Remove(Sender);
    SetProcessing(FProcessing, Count > 0);
  end;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
procedure TFVBase.EndProcessing;
begin
  FProcessing := False;
  if Assigned(OnProcessing) then
    OnProcessing(Self, False);
end;

//-- BG ---------------------------------------------------------- 13.03.2011 --
procedure TFVBase.ParseFrame(FrameSet: TObject; Doc: TBuffer; const FName: ThtString; AMetaEvent: TMetaType);
var
  Parser: THtmlParser;
begin
  Parser := THtmlParser.Create(Doc);
  try
    Parser.ParseFrame(Self, FrameSet, FName, AMetaEvent);
  finally
    Parser.Free;
  end;
end;

{$ifndef NoMetafile}

{----------------TFVBase.Print}

procedure TFVBase.Print(FromPage, ToPage: Integer);
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    AViewer.Print(FromPage, ToPage);
end;

{----------------TFVBase.NumPrinterPages}

function TFVBase.NumPrinterPages(out WidthRatio: double): Integer;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    Result := AViewer.NumPrinterPages(WidthRatio)
  else
    Result := 0;
end;

function TFVBase.NumPrinterPages: Integer;
var
  Dummy: double;
begin
  Result := NumPrinterPages(Dummy);
end;

{$endif}

{----------------TFVBase.CopyToClipboard}

procedure TFVBase.CopyToClipboard;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) and (AViewer.SelLength <> 0) then
    AViewer.CopyToClipboard;
end;

//-- BG ------------------------------------------------------- 03.10.2022 --
procedure TFVBase.ScaleChanged;
begin
  inherited;

  // while loading we will set the Text in Loaded;
  if not(csLoading in ComponentState) then
    Reload;
end;

procedure TFVBase.SelectAll;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    AViewer.SelectAll;
end;

{----------------TFVBase.Find}

function TFVBase.Find(const S: UnicodeString; MatchCase: Boolean): Boolean;
begin
  Result := FindEx(S, MatchCase, False);
end;

{----------------TFVBase.FindEx}

function TFVBase.FindEx(const S: UnicodeString; MatchCase, Reverse: Boolean): Boolean;
var
  AViewer: THtmlViewer;
begin
  AViewer := GetActiveViewer;
  if Assigned(AViewer) then
    Result := AViewer.FindEx(S, MatchCase, Reverse)
  else
    Result := False;
end;

procedure TFVBase.Loaded;
begin
  inherited;

  if Length(FText) <> 0 then
    LoadFromString(FText);
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
procedure TFVBase.LoadFromString(const Text, Name, Dest: ThtString);
begin
  if not Processing then
    LoadFromStringInternal(Text, Name, Dest);
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
procedure TFVBase.LoadFromStringInternal(const Text, Name, Dest: ThtString);
var
  OldFrameSet: TFrameSetBase;
  S: ThtString;
  OldPos: Integer;
  Tmp: TObject;
begin
  BeginProcessing;
  IOResult; {remove any pending file errors}
  try
    ProcessList.Clear;
    if Assigned(OnSoundRequest) then
      OnSoundRequest(Self, '', 0, True);
    OldPos := 0;
    if (CurFrameSet.Viewers.Count = 1) then
    begin
      Tmp := CurFrameSet.Viewers[0];
      if Tmp is THtmlViewer then
        OldPos := THtmlViewer(Tmp).Position;
    end;
    if Name <> '' then
      S := Name
    else
      S := 'source://' + Text;
    if htCompareText(CurFrameSet.FCurrentFile, S) <> 0 then
    begin
      OldFrameSet := CurFrameSet;
      FCurFrameSet := GetFrameSetClass.Create(Self);
      try
        CurFrameSet.Align := alClient;
        CurFrameSet.Parent := Self;
        CurFrameSet.SendToBack;
        CurFrameSet.LoadFromString(Text, Name, Dest);
      except
        CurFrameSet.Free;
        FCurFrameSet := OldFrameSet;
        raise;
      end;

      CurFrameSet.Visible := True;
      if Visible then
        CurFrameSet.BringToFront;
      OldFrameSet.Visible := False;
      OldFrameSet.UnloadFiles;
      BumpHistory(OldFrameSet, OldPos);
    end
    else
    begin {Same Name}
      CurFrameSet.LoadFromString(Text, Name, Dest);
      BumpHistory2(OldPos); {not executed if exception occurs}
    end;
    AddVisitedLink(S + Dest);
    CheckVisitedLinks;
  finally
    FText := Text;
    EndProcessing;
  end;
end;

{TPositionObj}

destructor TPositionObj.Destroy;
begin
  FormData.Free;
  inherited;
end;

{ TSubFrameSet }

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TSubFrameSet.GetFrameClass: TViewerFrameClass;
begin
  Result := TfvFrame;
end;

{ TFrameBase }

//-- BG ---------------------------------------------------------- 14.10.2014 --
procedure TFrameBase.Reload;
begin
  inherited;
  if FMasterSet <> nil then
    if FMasterSet.FFrameViewer <> nil then
      FMasterSet.FFrameViewer.Reload;
end;

//-- BG ---------------------------------------------------------- 03.10.2022 --
function TFrameBase.GetPixelsPerInch: Integer;
begin
  Result := FMasterSet.FrameViewer.PixelsPerInch;
end;

procedure TFrameBase.SetQuirksMode(const AValue: THtQuirksMode);
begin
  FQuirksMode := AValue;
end;

{$ifdef UseGenerics}
{$else}

{ TFrameBaseList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TFrameBaseList.Get(Index: Integer): TFrameBase;
begin
  Result := inherited Get(Index);
end;

{ TPositionObjList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TPositionObjList.Get(Index: Integer): TPositionObj;
begin
  Result := inherited Get(Index);
end;

{$endif}

end.
