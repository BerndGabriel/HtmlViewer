{Version 9.45}
{*********************************************************}
{*                     HTMLVIEW.PAS                      *}
{*********************************************************}
{
Copyright (c) 1995-2008 by L. David Baldwin

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

Note that the source modules, HTMLGIF1.PAS, PNGZLIB1.PAS, DITHERUNIT.PAS, and
URLCON.PAS are covered by separate copyright notices located in those modules.
}

{$i htmlcons.inc}

unit Htmlview;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, StdCtrls,
  vwPrint, MetafilePrinter, mmSystem,
  HTMLUn2, Forms, Dialogs, ExtCtrls, ReadHTML, HTMLSubs, StyleUn, Printers, Menus,
  GDIPL2A;

const
  wm_FormSubmit = wm_User+100;
  wm_MouseScroll = wm_User+102;
  wm_UrlAction = wm_User+103;   

type
  THTMLViewer = class;

  THTMLBorderStyle = (htFocused, htNone, htSingle);
  TRightClickParameters = Class(TObject)
    URL, Target: string;
    Image: TImageObj;
    ImageX, ImageY: integer;
    ClickWord: WideString;   
    end;
  TRightClickEvent = procedure(Sender: TObject; Parameters: TRightClickParameters) of Object;
  THotSpotEvent = procedure(Sender: TObject; const SRC: string) of Object;
  THotSpotClickEvent = procedure(Sender: TObject; const SRC: string;    
                     var Handled: boolean) of Object;
  TProcessingEvent = procedure(Sender: TObject; ProcessingOn: boolean) of Object;
  TPagePrinted = procedure( Sender: TObject;
                              Canvas : TCanvas ;
                              NumPage, W, H: Integer ;
                              var StopPrinting : Boolean) of Object;
  ThtmlPagePrinted = procedure(Sender: TObject; HFViewer: ThtmlViewer;   
                              NumPage: Integer; LastPage: boolean;
                              var XL, XR: integer;
                              var StopPrinting: Boolean) of Object;
  TImageClickEvent = procedure(Sender, Obj: TObject; Button: TMouseButton;
                       Shift: TShiftState; X, Y: Integer) of Object;   
  TImageOverEvent = procedure(Sender, Obj: TObject; Shift: TShiftState;
                       X, Y: Integer) of Object;
  TMetaRefreshType = procedure(Sender: TObject; Delay: integer; const URL: string) of Object;
  TParseEvent = procedure(Sender: TObject; var Source: string) of Object;    

  htOptionEnum = (htOverLinksActive,htNoLinkUnderline,htPrintTableBackground,
                  htPrintBackground, htPrintMonochromeBlack, htShowDummyCaret,
                  htShowVScroll, htNoWheelMouse, htNoLinkHilite);
  ThtmlViewerOptions = set of htOptionEnum;
  ThtProgressEvent = procedure(Sender: TObject; Stage: TProgressStage;
                   PercentDone: integer) of Object;  

  TPaintPanel = class(TCustomPanel)
  private
    FOnPaint: TNotifyEvent;
    FViewer: ThtmlViewer;
    Canvas2: TCanvas;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LButtonDblClk;
    procedure DoBackground(ACanvas: TCanvas);
    constructor CreateIt(AOwner: TComponent; Viewer: ThtmlViewer);
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    procedure Paint; override;
  end;

  T32ScrollBar = Class(TScrollBar)   {a 32 bit scrollbar}
  private
    FPosition: integer;
    FMin, FMax, FPage: integer;
    procedure SetPosition(Value: integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
  public
    property Position: integer read FPosition write SetPosition;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    procedure SetParams(APosition, APage, AMin, AMax: Integer);
  end;

  ThtmlFileType = (HTMLType, TextType, ImgType, OtherType);

  THTMLViewer = class(TWinControl)
  private
    vwP, OldPrinter: TvwPrinter;       
    fScaleX, fScaleY: single;
    FCodePage: integer;
    function GetCursor: TCursor;   
    procedure SetCursor(Value: TCursor);      
  protected
    InCreate: boolean;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    DontDraw: boolean;
    FTitle: String;
    FURL: String;
    FTarget: String;
    FBase, FBaseEx: String;
    FBaseTarget: String;
    FCurrentFile: String;
    FNameList: TStringList;
    FCurrentFileType: ThtmlFileType;
    FOnHotSpotCovered: THotSpotEvent;
    FOnHotSpotClick: THotSpotClickEvent;
    FOnBitmapRequest: TGetBitmapEvent;
    FOnImageRequest: TGetImageEvent;
    FOnScript: TScriptEvent;
    FOnFormSubmit: TFormSubmitEvent;
    FOnHistoryChange: TNotifyEvent;
    FOnProcessing: TProcessingEvent;
    FOnInclude: TIncludeType;
    FOnSoundRequest: TSoundType;
    FOnLink: TLinkType;
    FOnMeta: TMetaType;
    FOnMetaRefresh: TMetaRefreshType;
    FOnPanelCreate: TPanelCreateEvent;
    FOnPanelDestroy: TPanelDestroyEvent;
    FOnPanelPrint: TPanelPrintEvent;
    FRefreshURL: string;
    FRefreshDelay: Integer;
    FOnRightClick: TRightClickEvent;
    FOnImageClick: TImageClickEvent;
    FOnImageOver: TImageOverEvent;
    FOnObjectClick: TObjectClickEvent;
    FOnFileBrowse: TFileBrowseEvent;
    FOnObjectFocus: ThtObjectEvent;
    FOnObjectBlur: ThtObjectEvent;
    FOnObjectChange: ThtObjectEvent;
    FOnProgress: ThtProgressEvent;
    FHistory, FTitleHistory: TStrings;
    FPositionHistory: TFreeList;
    FHistoryIndex: integer;
    FHistoryMaxCount: integer;
    FFontName: TFontName;
    FPreFontName: String;
    FFontColor: TColor;
    FHotSpotColor, FVisitedColor, FOverColor: TColor;
    FVisitedMaxCount: integer;
    FBackGround: TColor;
    FFontSize: integer;
    FProcessing: boolean;
    FAction, FFormTarget, FEncType, FMethod: String;
    FStringList: TStringList;
    FImageCacheCount: integer;
    FNoSelect: boolean;
    FScrollBars: TScrollStyle;
    FBorderStyle: THTMLBorderStyle;
    FDither: boolean;
    FCaretPos: integer;
    FOptions: ThtmlViewerOptions;
    sbWidth: integer;
    ScrollWidth: integer;
    FMaxVertical: integer;
    MouseScrolling: boolean;
    LeftButtonDown: boolean;
    MiddleScrollOn: boolean;
    MiddleY: integer;
    Hiliting: boolean;
    FPrintMarginLeft,
    FPrintMarginRight,
    FPrintMarginTop,
    FPrintMarginBottom: double;
    FCharset: TFontCharset;   {see htmlun2.pas for Delphi 2 TFontCharSet definition}
    FOnPrintHeader, FOnPrintFooter: TPagePrinted;
    FOnPrintHTMLHeader, FOnPrintHTMLFooter: ThtmlPagePrinted;
    FPage: integer;
    FOnPageEvent: TPageEvent;
    FOnMouseDouble: TMouseEvent;
    HotSpotAction: boolean;
    FMarginHeight, FMarginWidth: integer;
    FServerRoot: string;
    FSectionList: TSectionList;
    FImageStream: TMemoryStream;
    FOnExpandName: TExpandNameEvent;
    HTMLTimer: TTimer;
    FOnhtStreamRequest: TGetStreamEvent;
    LocalBitmapList: boolean;
    FDocumentSource: string;
    FOnParseBegin: TParseEvent;
    FOnParseEnd: TNotifyEvent;
    FTitleAttr: string;
    BGFixed: boolean;
    FPrintScale: double;
    NoJump: boolean;
    FOnLinkDrawn: TLinkDrawnEvent;
    FLinkAttributes: TStringList;     
    FLinkText: WideString;
    FLinkStart: TPoint;        
    FWidthRatio: double;
    FOnObjectTag: TObjectTagEvent; 

    function CreateHeaderFooter: ThtmlViewer;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure ScrollTo(Y: integer);
    procedure Scroll(Sender: TObject; ScrollCode: TScrollCode;
           var ScrollPos: Integer);
    procedure Layout;
    procedure SetViewImages(Value: boolean);
    function GetViewImages: boolean;
    procedure SetColor(Value: TColor);
    function GetBase: string;
    procedure SetBase(Value: string);
    function GetBaseTarget: string;
    function GetTitle: string;
    function GetCurrentFile: string;
    procedure SetBorderStyle(Value: THTMLBorderStyle);
    function GetPosition: integer;
    procedure SetPosition(Value: integer);
    function GetScrollPos: integer;
    procedure SetScrollPos(Value: integer);
    function GetScrollBarRange: integer;
    function GetHScrollPos: integer;      
    procedure SetHScrollPos(Value: integer);  
    function GetHScrollBarRange: integer;   
    procedure SetHistoryIndex(Value: integer);
    function GetPreFontName: TFontName;
    procedure SetPreFontName(Value: TFontName);
    procedure SetFontSize(Value: integer);
    procedure SetHotSpotColor(Value: TColor);
    procedure SetActiveColor(Value: TColor);
    procedure SetVisitedColor(Value: TColor);   
    procedure SetVisitedMaxCount(Value: integer);  
    procedure SetOnBitmapRequest(Handler: TGetBitmapEvent);
    procedure SetOnImageRequest(Handler: TGetImageEvent);
    procedure SetOnScript(Handler: TScriptEvent);
    procedure SetOnFormSubmit(Handler: TFormSubmitEvent);
    function GetOurPalette: HPalette;
    procedure SetOurPalette(Value: HPalette);
    procedure SetDither(Value: boolean);
    procedure SetCaretPos(Value: integer);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure BackgroundChange(Sender: TObject);
    procedure SubmitForm(Sender: TObject; const Action, Target, EncType, Method: string;
                Results: TStringList);
    procedure SetImageCacheCount(Value: integer);
    procedure WMFormSubmit(var Message: TMessage); message WM_FormSubmit;
    procedure WMMouseScroll(var Message: TMessage); message WM_MouseScroll;
    procedure WMUrlAction(var Message: TMessage); message WM_UrlAction; 
    procedure SetSelLength(Value: integer);
    procedure SetSelStart(Value: integer);  
    function GetSelLength: integer;   
    function GetSelText: WideString;  
    procedure SetNoSelect(Value: boolean);
    procedure SetHistoryMaxCount(Value: integer);
    procedure DrawBorder;
    procedure DoHilite(X, Y: integer); virtual;    
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetProcessing(Value: boolean);
    procedure SetCharset(Value: TFontCharset);
    function GetFormControlList: TList;
    function GetNameList: TStringList;
    function GetLinkList: TList;
    procedure SetServerRoot(Value: string);
    procedure SetOnFileBrowse(Handler: TFileBrowseEvent);   
    procedure SetOnObjectClick(Handler: TObjectClickEvent);
    procedure SetOnObjectFocus(Handler: ThtObjectEvent);
    procedure SetOnObjectBlur(Handler: ThtObjectEvent);
    procedure SetOnObjectChange(Handler: ThtObjectEvent);  
    procedure FormControlEnterEvent(Sender: TObject);
    procedure HandleMeta(Sender: TObject; const HttpEq, Name, Content: string);
    procedure SetOptions(Value: ThtmlViewerOptions);
    procedure DoImage(Sender: TObject; const SRC: string; var Stream: TMemoryStream);
    procedure SetOnExpandName(Handler: TExpandNameEvent); 
    function GetWordAtCursor(X, Y: integer; var St, En: integer; var AWord: WideString): boolean;
    procedure SetOnPanelCreate(Handler: TPanelCreateEvent);
    procedure SetOnPanelDestroy(Handler: TPanelDestroyEvent);
    procedure SetOnPanelPrint(Handler: TPanelPrintEvent);
    procedure HTMLTimerTimer(Sender: TObject);
    function GetDragDrop: TDragDropEvent;
    function GetDragOver: TDragOverEvent;
    procedure SetDragDrop(const Value: TDragDropEvent);
    procedure SetDragOver(const Value: TDragOverEvent);
    procedure HTMLDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure HTMLDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure InitLoad;
    function GetFormData: TFreeList;
    procedure SetFormData(T: TFreeList);
    function GetIDControl(const ID: string): TObject;    
    function GetIDDisplay(const ID: string): boolean;    
    procedure SetIDDisplay(const ID: string; Value: boolean);
    procedure SetPrintScale(Value: double);

  protected
    { Protected declarations }
    PaintPanel: TPaintPanel;
    BorderPanel: TPanel;
    Sel1: integer;

    procedure DoLogic;
    procedure DoScrollBars;
    procedure SetupAndLogic;
    function GetURL(X, Y: integer; var UrlTarg: TUrlTarget;
             var FormControl: TImageFormControlObj; var ATitle: string): guResultType;
    function GetPalette: HPALETTE; override;
    procedure HTMLPaint(Sender: TObject); virtual;
    procedure HTMLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
{$ifdef ver120_plus}
    procedure HTMLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint);
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
{$endif}
    procedure HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); virtual;
    procedure HTMLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseDblClk(Message: TWMMouse);
    function HotSpotClickHandled: boolean; dynamic;
    procedure LoadFile(const FileName: string; ft: ThtmlFileType); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateImageCache;
    procedure DrawBackground2(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: integer;
              Image: TGpObject; Mask: TBitmap; BW, BH: integer; BGColor: TColor);
    procedure DoBackground1(ACanvas: TCanvas; ATop, AWidth, AHeight, FullHeight: integer);
    procedure DoBackground2(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: integer; AColor: TColor);
    procedure LoadString(const Source, Reference: string; ft: ThtmlFileType);

  public
    { Public declarations }
    FrameOwner: TObject;
    VScrollBar: T32ScrollBar;
    HScrollBar: TScrollBar;
    TablePartRec: TTablePartRec;
    Visited: TStringList;     {visited URLs}

    procedure AddVisitedLink(const S: string);  
    procedure CheckVisitedLinks;
    procedure UrlAction;
    procedure TriggerUrlAction;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HTMLExpandFilename(const Filename: string): string; virtual;
    procedure LoadFromFile(const FileName: string);
    procedure LoadTextFromString(const S: string);
    {$ifdef ver120_plus}  {Delphi 4 and higher}
    procedure LoadFromString(const S: string; const Reference: string = ''); overload;
    {$ifdef Delphi6_Plus}
    procedure LoadFromString(const WS: WideString; const Reference: string = ''); overload;
    {$endif}
    procedure LoadFromStream(const AStream: TStream; const Reference: string = '');
    procedure LoadStrings(const Strings: TStrings; const Reference: string = '');
    procedure LoadFromBuffer(Buffer: PChar; BufSize: integer; const Reference: string = '');
    {$else}
    procedure LoadFromString(const S: string; const Reference: string);
    procedure LoadFromStream(const AStream: TStream; const Reference: string);
    procedure LoadStrings(const Strings: TStrings; const Reference: string);
    procedure LoadFromBuffer(Buffer: PChar; BufSize: integer; const Reference: string);
    {$endif}
    procedure LoadTextFile(const FileName: string);
    procedure LoadImageFile(const FileName: string);
    procedure LoadTextStrings(Strings: TStrings);
    procedure LoadStream(const URL: string; AStream: TMemoryStream; ft: ThtmlFileType);
    procedure Print(FromPage, ToPage: integer);
    function NumPrinterPages: integer; overload;
    function NumPrinterPages(var WidthRatio: double): integer; overload;
    function PrintPreview(MFPrinter: TMetaFilePrinter; NoOutput: boolean = False): integer; 
    function PositionTo(Dest: string): boolean;
    function Find(const S: WideString; MatchCase: boolean): boolean;
    function FindEx(const S: WideString; MatchCase, Reverse: boolean): boolean;
    procedure Clear; virtual;
    procedure CopyToClipboard;
    procedure SelectAll;
    procedure ClearHistory;
    procedure Reload;
    procedure BumpHistory(const FileName, Title: string;
                 OldPos: integer; OldFormData: TFreeList; ft: ThtmlFileType);
    function GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;
    function InsertImage(const Src: string; Stream: TMemoryStream): boolean;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Repaint; override;
    function FindSourcePos(DisplayPos: integer): integer;
    function FindDisplayPos(SourcePos: integer; Prev: boolean): integer;
    function DisplayPosToXy(DisplayPos: integer; var X, Y: integer): boolean;
    function PtInObject(X, Y: integer; var Obj: TObject): boolean;  {X, Y, are client coord}
    procedure SetStringBitmapList(BitmapList: TStringBitmapList);
    function XYToDisplayPos(X, Y: integer): integer;
    procedure ReplaceImage(const NameID: string; NewImage: TStream);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Reformat;
    procedure htProgress(Percent: Integer);
    procedure htProgressEnd;
    procedure htProgressInit;
    function FullDisplaySize(FormatWidth: integer): TSize;
    function MakeBitmap(YTop, FormatWidth, Width, Height: integer): TBitmap;
    function MakeMetaFile(YTop, FormatWidth, Width, Height: integer): TMetaFile;
    function MakePagedMetaFiles(Width, Height: integer): TList;   
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetCharAtPos(Pos: integer; var Ch: WideChar;
                 var Font: TFont): boolean;
    function GetTextByIndices(AStart, ALast: integer): WideString; 
    procedure OpenPrint;     
    procedure ClosePrint;
    procedure AbortPrint;

    property DocumentTitle: string read GetTitle;
    property URL: string read FURL write FURL;
    property Base: string read GetBase write SetBase;
    property BaseTarget: string read GetBaseTarget;
    property Position: integer read GetPosition write SetPosition;
    property VScrollBarPosition: integer read GetScrollPos write SetScrollPos;
    property VScrollBarRange: integer read GetScrollBarRange;
    property HScrollBarPosition: integer read GetHScrollPos write SetHScrollPos;  
    property HScrollBarRange: integer read GetHScrollBarRange;
    property CurrentFile: string read GetCurrentFile;
    property History: TStrings read FHistory;
    property TitleHistory: TStrings read FTitleHistory;
    property HistoryIndex: integer read FHistoryIndex write SetHistoryIndex;
    property Processing: boolean read FProcessing;
    property SelStart: integer read FCaretPos write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText;   
    property Target: string read FTarget write FTarget;   
    property Palette: HPalette read GetOurPalette write SetOurPalette;
    property Dither: boolean read FDither write SetDither default True;
    property CaretPos: integer read FCaretPos write SetCaretPos;
    property FormControlList: TList read GetFormControlList;
    property NameList: TStringList read GetNameList;
    property LinkList: TList read GetLinkList;
    property SectionList: TSectionList read FSectionList;
    property OnPageEvent: TPageEvent read FOnPageEvent write FOnPageEvent;
    property OnExpandName: TExpandNameEvent read FOnExpandName write SetOnExpandName;
    property FormData: TFreeList read GetFormData write SetFormData;
    property DocumentSource: string read FDocumentSource;
    property MaxVertical: integer read FMaxVertical;
    property TitleAttr: string read FTitleAttr;
    property IDDisplay[const ID: string]: boolean read GetIDDisplay write SetIDDisplay;
    property IDControl[const ID: string]: TObject read GetIDControl;
    property OnLinkDrawn: TLinkDrawnEvent read FOnLinkDrawn write FOnLinkDrawn;
    property LinkAttributes: TStringList read FLinkAttributes;     
    Property LinkText: WideString read FLinkText write FLinkText;
    Property LinkStart: TPoint read FLinkStart;
    property CodePage: integer read FCodePage write FCodePage;    

  published
    { Published declarations }
    property OnHotSpotCovered: THotSpotEvent read FOnHotSpotCovered
             write FOnHotSpotCovered;
    property OnHotSpotClick: THotSpotClickEvent read FOnHotSpotClick
             write FOnHotSpotClick;
    property OnBitmapRequest: TGetBitmapEvent read FOnBitmapRequest
             write SetOnBitmapRequest;
    property OnImageRequest: TGetImageEvent read FOnImageRequest
             write SetOnImageRequest;
    property OnScript: TScriptEvent read FOnScript
             write SetOnScript;
    property OnFormSubmit: TFormSubmitEvent read FOnFormSubmit
             write SetOnFormSubmit;
    property OnHistoryChange: TNotifyEvent read FOnHistoryChange
             write FOnHistoryChange;
    property OnProgress: ThtProgressEvent read FOnProgress write FOnProgress;
    property ViewImages: boolean read GetViewImages write SetViewImages default True;
    property Enabled;
    property TabStop;
    property TabOrder;
    property Align;
    property Name;
    property Tag;
    property PopupMenu;
    property ShowHint;
    {$ifdef ver120_plus}
    property Anchors;       
    {$endif}
    property Height default 150;
    property Width default 150;
    property DefBackground: TColor read FBackground write SetColor default clBtnFace;
    property BorderStyle: THTMLBorderStyle read FBorderStyle write SetBorderStyle;
    property Visible;
    property HistoryMaxCount: integer read FHistoryMaxCount write SetHistoryMaxCount;
    property DefFontName: TFontName read FFontName write FFontName;
    property DefPreFontName: TFontName read GetPreFontName write SetPreFontName;
    property DefFontSize: integer read FFontSize write SetFontSize default 12;
    property DefFontColor: TColor read FFontColor write FFontColor
             default clBtnText;
    property DefHotSpotColor: TColor read FHotSpotColor write SetHotSpotColor
             default clBlue;
    property DefVisitedLinkColor: TColor read FVisitedColor write SetVisitedColor
             default clPurple;
    property DefOverLinkColor: TColor read FOverColor write SetActiveColor
             default clBlue;
    property VisitedMaxCount: integer read FVisitedMaxCount write SetVisitedMaxCount default 50;
    property ImageCacheCount: integer read FImageCacheCount
                write SetImageCacheCount default 5;
    property NoSelect: boolean read FNoSelect write SetNoSelect;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property CharSet: TFontCharset read FCharSet write SetCharset;
    property MarginHeight: integer read FMarginHeight write FMarginHeight default 5;
    property MarginWidth: integer read FMarginWidth write FMarginWidth default 10;   
    property ServerRoot: string read FServerRoot write SetServerRoot;
    property PrintMarginLeft: double read FPrintMarginLeft write FPrintMarginLeft;
    property PrintMarginRight: double read FPrintMarginRight write FPrintMarginRight;
    property PrintMarginTop: double read FPrintMarginTop write FPrintMarginTop;
    property PrintMarginBottom: double read FPrintMarginBottom write FPrintMarginBottom;
    property PrintScale: double read FPrintScale write SetPrintScale;
    property htOptions: ThtmlViewerOptions read FOptions write SetOptions
                 default [htPrintTableBackground, htPrintMonochromeBlack];

    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    {$ifdef ver120_plus}
    property OnMouseWheel;    
    {$endif}
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnEnter;
    property OnExit;     
    property OnProcessing: TProcessingEvent read FOnProcessing write FOnProcessing;
    property OnPrintHeader: TPagePrinted read FOnPrintHeader write FOnPrintHeader;
    property OnPrintFooter: TPagePrinted read FOnPrintFooter write FOnPrintFooter;
    property OnPrintHTMLHeader: ThtmlPagePrinted read FOnPrintHTMLHeader write FOnPrintHTMLHeader;
    property OnPrintHTMLFooter: ThtmlPagePrinted read FOnPrintHTMLFooter write FOnPrintHTMLFooter;
    property OnInclude: TIncludeType read FOnInclude write FOnInclude;
    property OnSoundRequest: TSoundType read FOnSoundRequest write FOnSoundRequest;
    property OnMeta: TMetaType read FOnMeta write FOnMeta;
    property OnLink: TLinkType read FOnLink write FOnLink;
    property OnMetaRefresh: TMetaRefreshType read FOnMetaRefresh write FOnMetaRefresh;
    property OnImageClick: TImageClickEvent read FOnImageClick write FOnImageClick;
    property OnImageOver: TImageOverEvent read FOnImageOver write FOnImageOver;
    property OnFileBrowse: TFileBrowseEvent read FOnFileBrowse write SetOnFileBrowse;    
    property OnObjectClick: TObjectClickEvent read FOnObjectClick write SetOnObjectClick;
    property OnObjectFocus: ThtObjectEvent read FOnObjectFocus write SetOnObjectFocus;
    property OnObjectBlur: ThtObjectEvent read FOnObjectBlur write SetOnObjectBlur;
    property OnObjectChange: ThtObjectEvent read FOnObjectChange write SetOnObjectChange;  
    property OnRightClick:  TRightClickEvent read FOnRightClick write FOnRightClick;
    property OnMouseDouble: TMouseEvent read FOnMouseDouble write FOnMouseDouble;
    property OnPanelCreate: TPanelCreateEvent read FOnPanelCreate write SetOnPanelCreate;
    property OnPanelDestroy: TPanelDestroyEvent read FOnPanelDestroy write SetOnPanelDestroy;
    property OnPanelPrint: TPanelPrintEvent read FOnPanelPrint write SetOnPanelPrint;
    property OnDragDrop: TDragDropEvent read GetDragDrop write SetDragDrop;
    property OnDragOver: TDragOverEvent read GetDragOver write SetDragOver;
    property OnhtStreamRequest: TGetStreamEvent read FOnhtStreamRequest
                  write FOnhtStreamRequest;
    property OnParseBegin: TParseEvent read FOnParseBegin write FOnParseBegin;
    property OnParseEnd: TNotifyEvent read FOnParseEnd write FOnParseEnd;
    property OnObjectTag: TObjectTagEvent read FOnObjectTag write FOnObjectTag;
    property Cursor: TCursor read GetCursor write SetCursor default crIBeam; 
    end;

implementation

uses
  Clipbrd, htmlgif2;    

const
  ScrollGap = 20;

type
  PositionObj = class(TObject)
    Pos: integer;
    FileType: ThtmlFileType;
    FormData: TFreeList;       
    destructor Destroy; override;
    end;

constructor THTMLViewer.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
InCreate := True;
ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
  csSetCaption, csDoubleClicks]; 
Height := 150;
Width := 150;
SetCursor(crIBeam);    
FPrintMarginLeft := 2.0;
FPrintMarginRight := 2.0;
FPrintMarginTop := 2.0;
FPrintMarginBottom := 2.0;
FPrintScale := 1.0;
FCharset := DEFAULT_CHARSET;
FMarginHeight := 5;
FMarginWidth := 10;

BorderPanel := TPanel.Create(Self);  
BorderPanel.BevelInner := bvNone;
BorderPanel.BevelOuter := bvNone;
BorderPanel.Ctl3D := False;
BorderPanel.Align := alClient;
BorderPanel.ParentCtl3D := False;
{$ifdef delphi7_plus}
BorderPanel.ParentBackground := False;
{$endif}

BorderPanel.Parent := Self;

PaintPanel := TPaintPanel.CreateIt(Self, Self);
PaintPanel.ParentFont := False;
PaintPanel.Parent := Self;
PaintPanel.Top := 1;
PaintPanel.Left := 1;
PaintPanel.BevelOuter := bvNone;
PaintPanel.BevelInner := bvNone;
PaintPanel.ctl3D := False;

PaintPanel.OnPaint := HTMLPaint;
PaintPanel.OnMouseDown := HTMLMouseDown;
PaintPanel.OnMouseMove := HTMLMouseMove;
PaintPanel.OnMouseUp := HTMLMouseUp;

VScrollBar := T32ScrollBar.Create(Self);
VScrollBar.Kind := sbVertical;
VScrollBar.SmallChange := 16;
VScrollBar.Visible := False;
VScrollBar.TabStop := False;
sbWidth := VScrollBar.Width;
VScrollBar.Parent := Self;

HScrollBar := TScrollBar.Create(Self);
HScrollBar.Kind := sbHorizontal;
HScrollBar.SmallChange := 15;
HScrollBar.OnScroll := Scroll;
HScrollBar.Visible := False;
HScrollBar.TabStop := False;
HScrollBar.Parent := Self;
HScrollBar.Width := sbWidth;  

FScrollBars := ssBoth;

FSectionList := TSectionList.Create(Self, PaintPanel);
FSectionList.ControlEnterEvent := FormControlEnterEvent;
FSectionList.OnBackgroundChange := BackgroundChange;
FSectionList.ShowImages := True;    
FNameList := FSectionList.IDNameList;

DefBackground := clBtnFace;
DefFontColor                                                                                             := clBtnText;
DefHotSpotColor := clBlue;
DefOverLinkColor := clBlue;
DefVisitedLinkColor := clPurple;
FVisitedMaxCount := 50;
DefFontSize := 12;
DefFontName := 'Times New Roman';
DefPreFontName := 'Courier New';
SetImageCacheCount(5);
SetOptions([htPrintTableBackground, htPrintMonochromeBlack]);

FHistory := TStringList.Create;
FPositionHistory := TFreeList.Create;
FTitleHistory := TStringList.Create;
FDither := True;

Visited := TStringList.Create;
HTMLTimer := TTimer.Create(Self);   
HTMLTimer.Enabled := False;
HTMLTimer.Interval := 200;
HTMLTimer.OnTimer := HTMLTimerTimer;
FLinkAttributes := TStringList.Create;   
InCreate := False;
end;

destructor ThtmlViewer.Destroy;
begin
if LocalBitmapList then
  begin
  FSectionList.Clear;
  FSectionList.BitmapList.Free;
  end;
FSectionList.Free;
FHistory.Free;
FPositionHistory.Free;
FTitleHistory.Free;
Visited.Free;
HTMLTimer.Free;
FLinkAttributes.Free;   
AbortPrint;    
inherited Destroy;
end;

procedure THtmlViewer.SetupAndLogic;
begin
FTitle := ReadHTML.Title;
if ReadHTML.Base <> '' then
  FBase := ReadHTML.Base
else FBase := FBaseEx;
FBaseTarget := ReadHTML.BaseTarget;
if Assigned(FOnParseEnd) then   
  FOnParseEnd(Self);
try
  DontDraw := True;
  {Load the background bitmap if any and if ViewImages set}
  FSectionList.GetBackgroundBitmap;

DoLogic;

finally
  DontDraw := False;
  end;
end;

procedure ThtmlViewer.LoadFile(const FileName: string; ft: ThtmlFileType);
var
  I: integer;
  Dest, FName, OldFile: string;
  SBuffer: string;
  OldCursor: TCursor;
  FS: TFileStream;       
begin
with Screen do
  begin
  OldCursor := Cursor;
  Cursor := crHourGlass;
  end;
IOResult;   {eat up any pending errors}
FName := FileName;
I := Pos('#', FName);
if I > 0 then
  begin
  Dest := Copy(FName, I+1, Length(FName)-I);  {positioning information} 
  FName := Copy(FName, 1, I-1);
  end
else Dest := '';
FRefreshDelay := 0;
try
  SetProcessing(True);
  if not FileExists(FName) then
    Raise(EInOutError.Create('Can''t locate file: '+FName));
  FSectionList.ProgressStart := 75;  
  htProgressInit;    
  DontDraw := True;  
  InitLoad;
  CaretPos := 0;
  Sel1 := -1;
  try
    OldFile := FCurrentFile;
    FCurrentFile := ExpandFileName(FName);
    FCurrentFileType := ft;
    if ft in [HTMLType, TextType] then
      begin
      FS := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);   
      try
        SetLength(FDocumentSource, FS.Size);
        FS.ReadBuffer(FDocumentSource[1], FS.Size);
      finally
        FS.Free;
        end;
      end
    else FDocumentSource := '';
    if Assigned(FOnParseBegin) then   
      FOnParseBegin(Self, FDocumentSource);
    if ft = HTMLType then
      begin
      if Assigned(FOnSoundRequest) then
        FOnSoundRequest(Self, '', 0, True);
      ParseHTMLString(FDocumentSource, FSectionList, FOnInclude, FOnSoundRequest, HandleMeta, FOnLink);
      end
    else if ft = TextType then
      ParseTextString(FDocumentSource, FSectionList)
    else
      begin
      SBuffer := '<img src="'+FName+'">';
      ParseHTMLString(SBuffer, FSectionList, Nil, Nil, Nil, Nil);
      end;
  finally
    SetupAndLogic;
    CheckVisitedLinks;
    if (Dest <> '') and PositionTo(Dest) then  {change position, if applicable}
    else if FCurrentFile <> OldFile then
       begin
       ScrollTo(0);
       HScrollBar.Position := 0;
       end;
    {else if same file leave position alone}
    DontDraw := False;      
    PaintPanel.Invalidate;
  end;
finally
  Screen.Cursor := OldCursor;
  htProgressEnd;    
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

procedure ThtmlViewer.LoadFromFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: integer;
  OldType: ThtmlFileType;
  OldFormData: TFreeList;
  (*Stream: TMemoryStream;  //debugging aid
  Indent, Tree: string; *)
begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile;
  OldTitle := FTitle;
  OldPos := Position;
  OldType := FCurrentFileType;
  OldFormData := GetFormData;   
  try                    
    LoadFile(FileName, HTMLType);

    (*Indent := '';     //debugging aid
    Tree := '';
    FSectionList.FormTree(Indent, Tree);

    Stream := TMemoryStream.Create;
    Stream.Size := Length(Tree);
    Move(Tree[1], Stream.Memory^, Length(Tree));
    Stream.SaveToFile('C:\css2\exec\Tree.txt');
    Stream.Free; *) 


    if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
      BumpHistory(OldFile, OldTitle, OldPos, OldFormData, OldType)
    else OldFormData.Free;
  except
    OldFormData.Free;
    Raise;
    end;
  end;
end;

{----------------ThtmlViewer.LoadTextFile}
procedure ThtmlViewer.LoadTextFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: integer;
  OldType: ThtmlFileType;
  OldFormData: TFreeList;  
begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile;
  OldTitle := FTitle;
  OldPos := Position;
  OldType := FCurrentFileType;
  OldFormData := GetFormData;
  try
    LoadFile(FileName, TextType);
    if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
      BumpHistory(OldFile, OldTitle, OldPos, OldFormData, OldType)
    else OldFormData.Free;
  except
    OldFormData.Free;
    Raise;
    end;
  end;
end;

{----------------ThtmlViewer.LoadImageFile}
procedure ThtmlViewer.LoadImageFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: integer;
  OldType: ThtmlFileType;
  OldFormData: TFreeList;  

begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile;
  OldTitle := FTitle;
  OldPos := Position;
  OldType := FCurrentFileType;
  OldFormData := GetFormData;   
  try
    LoadFile(FileName, ImgType);
    if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
      BumpHistory(OldFile, OldTitle, OldPos, OldFormData, OldType)      
    else OldFormData.Free;
  except
    OldFormData.Free;
    Raise;
    end;
  end;
end;

{----------------THtmlViewer.LoadStrings}
procedure THtmlViewer.LoadStrings(const Strings: TStrings; const Reference: string);
begin
LoadString(Strings.Text, Reference, HTMLType);
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------THtmlViewer.LoadTextStrings}
procedure THtmlViewer.LoadTextStrings(Strings: TStrings);
begin
LoadString(Strings.Text, '', TextType);
end;

{----------------ThtmlViewer.LoadFromBuffer}
procedure ThtmlViewer.LoadFromBuffer(Buffer: PChar; BufSize: integer; const Reference: string);
var
  S: string;
begin
SetLength(S, BufSize);
Move(Buffer^, S[1], BufSize);
LoadString(S, Reference, HTMLType);
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------ThtmlViewer.LoadTextFromString}
procedure ThtmlViewer.LoadTextFromString(const S: string);
begin
LoadString(S, '', TextType);
end;

{----------------ThtmlViewer.LoadFromString}
procedure ThtmlViewer.LoadFromString(const S: string; const Reference: string);
begin
LoadString(S, Reference, HTMLType);
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{$ifdef Delphi6_Plus}
procedure ThtmlViewer.LoadFromString(const WS: WideString; const Reference: string);
begin
LoadFromString(#$EF+#$BB+#$BF+UTF8Encode(WS), Reference);
end;
{$endif}

{----------------ThtmlViewer.LoadString}
procedure ThtmlViewer.LoadString(const Source, Reference: string; ft: ThtmlFileType);  
var
  I: integer;
  Dest, FName, OldFile: string;

begin
if FProcessing then Exit;
SetProcessing(True);
FRefreshDelay := 0;
FName := Reference;
I := Pos('#', FName);
if I > 0 then
  begin
  Dest := Copy(FName, I+1, Length(FName)-I);  {positioning information} 
  FName := Copy(FName, 1, I-1);
  end
else Dest := '';
DontDraw := True;    
try
  OldFile := FCurrentFile;
  FCurrentFile := ExpandFileName(FName);
  FCurrentFileType := ft;
  FSectionList.ProgressStart := 75;
  htProgressInit;    
  InitLoad;
  CaretPos := 0;
  Sel1 := -1;
  if Assigned(FOnSoundRequest) then
    FOnSoundRequest(Self, '', 0, True);
  FDocumentSource := Source;
  if Assigned(FOnParseBegin) then
    FOnParseBegin(Self, FDocumentSource);
  if Ft = HTMLType then
    ParseHTMLString(FDocumentSource, FSectionList, FOnInclude, FOnSoundRequest, HandleMeta, FOnLink)
  else
    ParseTextString(FDocumentSource, FSectionList);
  SetupAndLogic;
  CheckVisitedLinks;
  if (Dest <> '') and PositionTo(Dest) then  {change position, if applicable}
  else if (FCurrentFile = '') or (FCurrentFile <> OldFile) then
     begin
     ScrollTo(0);
     HScrollBar.Position := 0;
     end;
  {else if same file leave position alone}
  PaintPanel.Invalidate;
finally
  htProgressEnd;
  SetProcessing(False);
  DontDraw := False;    
  end;
end;

{----------------ThtmlViewer.LoadFromStream}
procedure ThtmlViewer.LoadFromStream(const AStream: TStream; const Reference: string);
var
  Stream: TMemoryStream;
  S: string;
begin
Stream := TMemoryStream.Create;
try
  Stream.LoadFromStream(AStream);
  SetLength(S, Stream.Size);
  Move(Stream.Memory^, S[1], Stream.Size);
  LoadString(S, Reference, HTMLType);
  if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
    FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
finally
  Stream.Free;
  end;
end;

procedure ThtmlViewer.DoImage(Sender: TObject; const SRC: string; var Stream: TMemoryStream);
begin
Stream := FImageStream;
end;

{----------------ThtmlViewer.LoadStream}
procedure ThtmlViewer.LoadStream(const URL: string; AStream: TMemoryStream; ft: ThtmlFileType);
var
  SaveOnImageRequest: TGetImageEvent;
  SBuffer: string;
begin
if FProcessing or not Assigned(AStream) then Exit;
SetProcessing(True);
FRefreshDelay := 0;
DontDraw := True;     
try
  FSectionList.ProgressStart := 75;  
  htProgressInit;      
  InitLoad;
  CaretPos := 0;
  Sel1 := -1;

  if ft in [HTMLType, TextType] then
    begin
    SetLength(FDocumentSource, AStream.Size);
    Move(AStream.Memory^, FDocumentSource[1], AStream.Size);
    end
  else FDocumentSource := '';
  if Assigned(FOnParseBegin) then
    FOnParseBegin(Self, FDocumentSource);
  if ft = HTMLType then
    begin
    if Assigned(FOnSoundRequest) then
      FOnSoundRequest(Self, '', 0, True);
    ParseHTMLString(FDocumentSource, FSectionList, FOnInclude, FOnSoundRequest, HandleMeta, FOnLink);
    SetupAndLogic;
    end
  else if ft = TextType then
    begin
    ParseTextString(FDocumentSource, FSectionList);
    SetupAndLogic;
    end
  else
    begin
    SaveOnImageRequest := FOnImageRequest;
    SetOnImageRequest(DoImage);
    FImageStream := AStream;
    SBuffer := '<img src="'+URL+'">';
    try
      ParseHTMLString(SBuffer, FSectionList, Nil, Nil, Nil, Nil);
      SetupAndLogic;
    finally
      SetOnImageRequest(SaveOnImageRequest);
      end;
    end;
  ScrollTo(0);
  HScrollBar.Position := 0;
  PaintPanel.Invalidate;
  FCurrentFile := URL;
finally
  htProgressEnd;
  DontDraw := False;      
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------ThtmlViewer.DoScrollBars}
procedure ThtmlViewer.DoScrollBars;
var
  VBar, VBar1, HBar: boolean;   
  Wid, HWidth, WFactor, WFactor2, VHeight: integer;
  ScrollInfo :TScrollInfo;

begin
ScrollWidth := IntMin(ScrollWidth, MaxHScroll);
if FBorderStyle = htNone then
  begin
  WFactor := 0;
  PaintPanel.Top := 0;
  PaintPanel.Left := 0;
  BorderPanel.Visible := False;
  end
else
  begin
  WFactor := 1;
  PaintPanel.Top := 1;
  PaintPanel.Left := 1;
  BorderPanel.Visible := False;
  BorderPanel.Visible := True;
  end;
WFactor2 := 2*WFactor;

VBar := False;
VBar1 := False;
if (not (htShowVScroll in htOptions) and (FMaxVertical <= Height-WFactor2) and (ScrollWidth <= Width-WFactor2)) 
         or (FScrollBars = ssNone) then
  {there are no scrollbars}
  HBar := False
else
  if FScrollBars in [ssBoth, ssVertical] then
    begin  {assume a vertical scrollbar}
    VBar1 := (FMaxVertical >= Height-WFactor2) or
            ((FScrollBars in [ssBoth, ssHorizontal]) and
             (FMaxVertical >= Height-WFactor2-sbWidth) and
             (ScrollWidth > Width-sbWidth-WFactor2));
    HBar := (FScrollBars in [ssBoth, ssHorizontal]) and
            ((ScrollWidth > Width-WFactor2) or
             ((VBar1 or (htShowVScroll in FOptions)) and
                 (ScrollWidth > Width-sbWidth-WFactor2)));
    VBar := Vbar1 or  (htShowVScroll in htOptions);
    end
  else
    {there is no vertical scrollbar}
    HBar := (FScrollBars = ssHorizontal) and (ScrollWidth > Width-WFactor2);

if VBar or ((htShowVScroll in FOptions) and (FScrollBars in [ssBoth, ssVertical])) then
  Wid := Width - sbWidth
else
  Wid := Width;
PaintPanel.Width := Wid - WFactor2;
if HBar then
  begin
  PaintPanel.Height := Height - WFactor2 - sbWidth;
  VHeight := Height - sbWidth - WFactor2;        
  end
else
  Begin
  PaintPanel.Height := Height - WFactor2;
  VHeight := Height - WFactor2;        
  end;
HWidth := IntMax(ScrollWidth, Wid-WFactor2);
HScrollBar.Visible := HBar;
HScrollBar.LargeChange := IntMax(1, Wid - 20);
HScrollBar.SetBounds(WFactor, Height-sbWidth-WFactor, Wid -WFactor, sbWidth);
VScrollBar.SetBounds(Width-sbWidth-WFactor, WFactor, sbWidth, VHeight);    
VScrollBar.LargeChange := PaintPanel.Height - VScrollBar.SmallChange;
if htShowVScroll in FOptions then
  begin
  VScrollBar.Visible := ( FScrollBars in [ssBoth, ssVertical] );
  VScrollBar.Enabled := VBar1;
  end
else VScrollBar.Visible := VBar;

HScrollBar.Max := IntMax(0, HWidth);
VScrollBar.SetParams(VScrollBar.Position, PaintPanel.Height+1, 0, FMaxVertical);
ScrollInfo.cbSize := SizeOf(ScrollInfo);
ScrollInfo.fMask := SIF_PAGE;
ScrollInfo.nPage := Wid;
SetScrollInfo(HScrollBar.Handle,SB_CTL,ScrollInfo,TRUE);
end;

{----------------ThtmlViewer.DoLogic}
procedure ThtmlViewer.DoLogic;
var
  Wid, WFactor: integer;

  function HasVScrollbar: boolean;     
  begin
  Result := (FMaxVertical > Height-WFactor) or   
            ((FScrollBars in [ssBoth, ssHorizontal]) and
             (FMaxVertical >= Height-WFactor-sbWidth) and
             (ScrollWidth > Width-sbWidth-WFactor));
  end;

  function HasVScrollbar1: boolean;     
  begin
  Result := (FMaxVertical > Height-WFactor) or
            ((FScrollBars in [ssBoth, ssHorizontal]) and
             (FMaxVertical >= Height-WFactor-sbWidth) and
             (ScrollWidth > Width-WFactor));
  end;

  function FSectionListDoLogic(Width: integer): integer;
  var
    Curs: integer;
  begin
  Curs := 0;
  ScrollWidth := 0;
  Result := FSectionList.DoLogic(PaintPanel.Canvas, 0,
          Width, ClientHeight-WFactor, 0, ScrollWidth, Curs);    
  end;
begin
HandleNeeded;
try
  DontDraw := True;
  if FBorderStyle = htNone then WFactor := 0
    else WFactor := 2;
  Wid := Width - WFactor;
  if FScrollBars in [ssBoth, ssVertical] then
    begin
    if not (htShowVScroll in FOptions) and (Length(FDocumentSource) < 10000) then   
      begin   {see if there is a vertical scrollbar with full width}
      FMaxVertical := FSectionListDoLogic(Wid);
      if HasVScrollBar then {yes, there is vertical scrollbar, allow for it}
        begin
        FMaxVertical := FSectionListDoLogic(Wid-sbWidth);
        if not HasVScrollBar1 then
          FMaxVertical := FSectionListDoLogic(Wid);
        end;
      end
    else {assume a vertical scrollbar}
      FMaxVertical := FSectionListDoLogic(Wid-sbWidth);
    end
  else {there is no vertical scrollbar}
    FMaxVertical := FSectionListDoLogic(Wid);

  DoScrollbars;
finally
  DontDraw := False;
  end;
end;

procedure ThtmlViewer.HTMLPaint(Sender: TObject);
var
  ARect: TRect;
begin
if not DontDraw then
  begin
  ARect := Rect(0, 1, PaintPanel.Width, PaintPanel.Height);
  FSectionList.Draw(PaintPanel.Canvas2, ARect, MaxHScroll,
                         -HScrollBar.Position, 0, 0, 0);
  end;
end;

procedure ThtmlViewer.WMSize(var Message: TWMSize);
begin
inherited;
if InCreate then    
  Exit;
if not FProcessing then
  Layout
else
  DoScrollBars;
if FMaxVertical < PaintPanel.Height then
  Position := 0
else ScrollTo(VScrollBar.Position);   {keep aligned to limits}
with HScrollBar do
  Position := IntMin(Position, Max - PaintPanel.Width);
end;

procedure ThtmlViewer.Scroll(Sender: TObject; ScrollCode: TScrollCode;
       var ScrollPos: Integer);
{only the horizontal scrollbar comes here}
begin  
ScrollPos := IntMin(ScrollPos, HScrollBar.Max - PaintPanel.Width);
PaintPanel.Invalidate;
end;

procedure ThtmlViewer.ScrollTo(Y: integer);
begin
Y := IntMin(Y, FMaxVertical - PaintPanel.Height);
Y := IntMax(Y, 0);
VScrollBar.Position := Y;
FSectionList.SetYOffset(Y);
Invalidate;
end;

procedure ThtmlViewer.Layout;
var
  OldPos: integer;
begin
if FProcessing then Exit;
SetProcessing(True);
try
  OldPos := Position;
  FSectionList.ProgressStart := 0;   
  htProgressInit;
  DoLogic;
  Position := OldPos;   {return to old position after width change}
finally
  htProgressEnd;     
  SetProcessing(False);
  end;
end;

function ThtmlViewer.HotSpotClickHandled: boolean;
var
  Handled: boolean;
begin
Handled := False;
if Assigned(FOnHotSpotClick) then
  FOnHotSpotClick(Self, URL, Handled);
Result := Handled;
end;

procedure ThtmlViewer.TriggerUrlAction;  
begin
PostMessage(Handle, wm_UrlAction, 0, 0);
end;

procedure ThtmlViewer.WMUrlAction(var Message: TMessage); 
begin
UrlAction;
end;

procedure ThtmlViewer.URLAction;
var
  S, Dest: string;
  Ext: string[5];
  I: integer;
  OldPos: integer;

begin
if not HotSpotClickHandled then
  begin
  OldPos := Position;
  S := URL;
  I := Pos('#', S);  {# indicates a position within the document}
  if I = 1 then
    begin
    if PositionTo(S) then    {no filename with this one}
      begin
      BumpHistory(FCurrentFile, FTitle, OldPos, Nil, FCurrentFileType);   
      AddVisitedLink(FCurrentFile+S);    
      end;
    end
  else
    begin
    if I >= 1 then
      begin
      Dest := System.Copy(S, I, Length(S)-I+1);  {local destination}   
      S := System.Copy(S, 1, I-1);     {the file name}
      end
    else
      Dest := '';    {no local destination}
    S := HTMLExpandFileName(S);
    Ext := Uppercase(ExtractFileExt(S));
    if (Ext = '.HTM') or (Ext = '.HTML')  then
      begin              {an html file}
      if S <> FCurrentFile then
        begin
        LoadFromFile(S + Dest);
        AddVisitedLink(S+Dest);
        end
      else
        if PositionTo(Dest) then   {file already loaded, change position}
          begin
          BumpHistory(FCurrentFile, FTitle, OldPos, Nil, HTMLType);
          AddVisitedLink(S+Dest);
          end;
      end
    else if (Ext = '.BMP') or (Ext = '.GIF') or (Ext = '.JPG') or (Ext = '.JPEG')
                or (Ext = '.PNG') then
      LoadImageFile(S);
    end;
    {Note: Self may not be valid here}
  end;
end;

{----------------ThtmlViewer.AddVisitedLink}
procedure ThtmlViewer.AddVisitedLink(const S: string);    
var
  I, J: integer;
  S1, UrlTmp: string;
begin
if Assigned(FrameOwner) or (FVisitedMaxCount = 0) then
  Exit;      {TFrameViewer will take care of visited links}
I := Visited.IndexOf(S);
if I = 0 then Exit
else if I < 0 then
  begin
  for J := 0 to SectionList.LinkList.Count-1 do
    with TFontObj(SectionList.LinkList[J]) do
      begin
      UrlTmp := Url;
      if Length(UrlTmp) > 0 then
        begin
        if Url[1] = '#' then
          S1 := FCurrentFile+UrlTmp
        else
          S1 := HTMLExpandFilename(UrlTmp);
        if CompareText(S, S1) = 0 then
          Visited := True;
        end;
      end;
  end
else Visited.Delete(I);   {thus moving it to the top}
Visited.Insert(0, S);
for I :=  Visited.Count-1 downto FVisitedMaxCount do
  Visited.Delete(I);
end;

{----------------ThtmlViewer.CheckVisitedLinks}
procedure ThtmlViewer.CheckVisitedLinks;
var
  I, J: integer;
  S, S1: string;
begin
if FVisitedMaxCount = 0 then
  Exit;
for I := 0 to Visited.Count-1 do
  begin
  S := Visited[I];
  for J := 0 to SectionList.LinkList.Count-1 do
    with TFontObj(SectionList.LinkList[J]) do
      begin
      if (Url <> '') and (Url[1] = '#') then
        S1 := FCurrentFile+Url
      else
        S1 := HTMLExpandFilename(Url);
      if CompareText(S, S1) = 0 then
        Visited := True;
      end;
  end;
end;

{----------------ThtmlViewer.HTMLMouseDown}
procedure ThtmlViewer.HTMLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  XR, CaretHt: integer;
  YR: integer;
  InText: boolean;
  Dummy : TUrlTarget;     
  DummyFC: TImageFormControlObj;    
  DummyTitle: string;   
begin
inherited MouseDown(Button, Shift, X, Y);

SetFocus;
HotSpotAction := False;
if MiddleScrollOn then
  begin
  MiddleScrollOn := False;
  PaintPanel.Cursor := Cursor;
  MouseScrolling := False;
  end
else if (Button = mbMiddle) and not (htNoWheelMouse in htOptions) then  {comment this out to disable mouse middle button scrolling}
  begin
  MiddleScrollOn := True;
  MiddleY := Y;
  PaintPanel.Cursor := UpDownCursor;
  end
else if (Button = mbLeft) then
  begin
  LeftButtonDown := True;
  if not (htNoLinkHilite in FOptions)
       or not (guUrl in GetURL(X, Y, Dummy, DummyFC, DummyTitle)) then        
    HiLiting := True;
  with FSectionList do
    begin
    Sel1 := FindCursor(PaintPanel.Canvas, X, Y+YOff, XR, YR, CaretHt, InText);
    if Sel1 > -1 then
      begin
      if (SelB <> SelE) or (ssShift in Shift) then    
        InvalidateRect(PaintPanel.Handle, Nil, True);
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
    LButtonDown(True);   {signal to TSectionList}
    end;
  end;
end;

procedure ThtmlViewer.HTMLTimerTimer(Sender: TObject);  
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
    if Assigned(FOnHotSpotCovered) then FOnHotSpotCovered(Self, '');
    end;
  end;
end;

function ThtmlViewer.PtInObject(X, Y: integer; var Obj: TObject): boolean;  {X, Y, are client coord} {css}
var
  IX, IY: integer;
begin
Result := PtInRect(ClientRect, Point(X, Y)) and
            FSectionList.PtInObject(X, Y+FSectionList.YOff, Obj, IX, IY);
end;

procedure ThtmlViewer.ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Dummy : TUrlTarget;
  DummyFC: TImageFormControlObj;
begin
if Sender is TFormControlObj then
  with TFormControlObj(Sender), TheControl do
    begin
    FTitleAttr:= Title;
    if FTitleAttr = '' then
      begin
      Dummy := Nil;
      GetURL(X+Left, Y+Top, Dummy, DummyFC, FTitleAttr);
      if Assigned(Dummy) then
        Dummy.Free;
      end;
    Inherited MouseMove(Shift,X,Y);
    end;
end;

function ThtmlViewer.GetTextByIndices(AStart, ALast: integer): WideString;
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
else Result := '';
end;

{----------------ThtmlViewer.HTMLMouseMove}
procedure ThtmlViewer.HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  UrlTarget : TUrlTarget;
  Url, Target: string;
  FormControl: TImageFormControlObj;
  Obj: TObject;
  IX, IY: integer;
  XR, CaretHt: integer;
  YR: integer;
  InText: boolean;
  NextCursor: TCursor;
  guResult: guResultType; 
begin
Inherited MouseMove(Shift,X,Y);

if MiddleScrollOn then
  begin
  if not MouseScrolling and (Abs(Y-MiddleY) > ScrollGap) then
    begin
    MouseScrolling := True;
    PostMessage(Handle, wm_MouseScroll, 0, 0);
    end;
  Exit;
  end;

UrlTarget := Nil;
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
if (Assigned(FOnImageClick) or Assigned(FOnImageOver)) and
     FSectionList.PtInObject(X, Y+FSectionList.YOff, Obj, IX, IY) then
  begin
  if NextCursor <> HandCursor then  {in case it's also a Link}
    NextCursor := crArrow;
  if Assigned(FOnImageOver) then FOnImageOver(Self, Obj, Shift, IX, IY);
  end
else if (FSectionList.FindCursor(PaintPanel.Canvas, X, Y+FSectionList.YOff, XR, YR, CaretHt, InText) >= 0)
          and InText and (NextCursor <> HandCursor) then
  NextCursor := Cursor;

PaintPanel.Cursor := NextCursor;

if ((NextCursor = HandCursor) or (SectionList.ActiveImage <> Nil)) then
  HTMLTimer.Enabled := True
else HTMLTimer.Enabled := False;

if (URL <> FURL) or (Target <> FTarget) then
  begin
  FURL := URL;
  FTarget := Target;
  if Assigned(FOnHotSpotCovered) then FOnHotSpotCovered(Self, URL);
  end;
if (ssLeft in Shift) and not MouseScrolling
       and ((Y <= 0) or (Y >= Self.Height)) then
  begin
  MouseScrolling := True;
  PostMessage(Handle, wm_MouseScroll, 0, 0);
  end;
if (ssLeft in Shift) and not FNoSelect then
  DoHilite(X, Y);
end;

procedure ThtmlViewer.HTMLMouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
var
  UrlTarget: TUrlTarget;
  FormControl: TImageFormControlObj;
  Obj: TObject;
  IX, IY: integer;
  InImage, TmpLeft: boolean;  
  Parameters: TRightClickParameters;
  AWord: WideString;
  St, En: integer;
  guResult: guResultType;
  I, ThisID: integer;
  ParentForm: TCustomForm;  
begin
if MiddleScrollOn then
  begin
  {cancel unless it's middle button and has moved}
  if (Button <> mbMiddle) or (Y <> MiddleY) then
    begin
    MiddleScrollOn := False;
    PaintPanel.Cursor := Cursor;
    end;
  Exit;
  end;

inherited MouseUp(Button, Shift, X, Y);

if Assigned(FOnImageClick) or Assigned(FOnRightClick) then
  begin
  InImage := FSectionList.PtInObject(X, Y+FSectionList.YOff, Obj, IX, IY);
  if Assigned(FOnImageClick) and InImage then
    FOnImageClick(Self, Obj, Button, Shift, IX, IY);
  if (Button = mbRight) and Assigned(FOnRightClick) then
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
      FOnRightClick(Self, Parameters);
    finally
      HTMLTimer.Enabled := True;   
      Parameters.Free;
      end;
    end;
  end;

if (Button = mbLeft) and not (ssShift in Shift) then  
  begin
  MouseScrolling := False;
  DoHilite(X, Y);
  Hiliting := False;
  FSectionList.LButtonDown(False);
  TmpLeft := LeftButtonDown;
  LeftButtonDown := False;
  if TmpLeft and (FSectionList.SelE <= FSectionList.SelB) then
    begin
    guResult := GetURL(X, Y, UrlTarget, FormControl, FTitleAttr);
    if guControl in guResult then
      FormControl.ImageClick(Nil)   
    else if guUrl in guResult then
      begin
      FURL := UrlTarget.Url;
      FTarget := UrlTarget.Target;
      FLinkAttributes.Text := UrlTarget.Attr;  
      FLinkText := GetTextByIndices(UrlTarget.Start, UrlTarget.Last);   
      ThisID := UrlTarget.ID;
      for I := 0 to LinkList.Count-1 do  
        with TFontObj(LinkList.Items[I]) do
          if (ThisID = UrlTarget.ID) and Assigned(TabControl) then
            begin
            ParentForm := GetParentForm(TabControl);    
            if Assigned(ParentForm) and TabControl.CanFocus then
              begin
              NoJump := True;    {keep doc from jumping position on mouse click}
              try
                ParentForm.ActiveControl := TabControl;
              finally
                NoJump := False;
                end;
              end;
            break;
            end;
      UrlTarget.Free;
      HotSpotAction := True;   {prevent double click action}
      URLAction;
      {Note:  Self pointer may not be valid after URLAction call (TFrameViewer, HistoryMaxCount=0)}
      end;
    end;
  end;
end;

{----------------ThtmlViewer.HTMLMouseWheel}
{$ifdef ver120_plus}
procedure ThtmlViewer.HTMLMouseWheel(Sender: TObject; Shift: TShiftState;   
      WheelDelta: Integer; MousePos: TPoint);
var
  Lines: integer;
begin
Lines := Mouse.WheelScrollLines;
if Lines > 0 then
  if WheelDelta > 0 then
     VScrollBarPosition := VScrollBarPosition - (Lines * 16)
  else
     VScrollBarPosition := VScrollBarPosition + (Lines * 16)
else VScrollBarPosition := VScrollBarPosition - WheelDelta div 2;
end;

function ThtmlViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;   
                                         MousePos: TPoint): Boolean;
begin
result:= inherited DoMouseWheel(shift, wheelDelta, mousePos);
if not result and not (htNoWheelMouse in htOptions) then   
  begin
  HTMLMouseWheel(Self, Shift, WheelDelta, MousePos);
  Result := True;
  end;
end;
{$endif}

 {----------------ThtmlViewer.XYToDisplayPos}
function ThtmlViewer.XYToDisplayPos(X, Y: integer): integer;
var
  InText: boolean;
  XR, YR, CaretHt: integer;
begin
with SectionList do
  Result := FindCursor(PaintPanel.Canvas, X, Y+YOff, XR, YR, CaretHt, InText);
if not InText then
  Result := -1;
end;

{----------------ThtmlViewer.GetCharAtPos}
function ThtmlViewer.GetCharAtPos(Pos: integer; var Ch: WideChar;
             var Font: TFont): boolean;   
var
  Obj: TObject;
  FO: TFontObj;
  Index: integer;
begin
Result := FSectionList.GetChAtPos(Pos, Ch, Obj);
if Result and (Obj is TSection) then
  with TSection(Obj) do
    begin
    FO := Fonts.GetFontObjAt(Pos-StartCurs, Index);
    Font := FO.TheFont;
    end;
end;

{----------------ThtmlViewer.GetWordAtCursor}
function ThtmlViewer.GetWordAtCursor(X, Y: integer; var St, En: integer; var AWord: WideString): boolean;
var
  XR, X1, CaretHt: integer;
  YR, Y1: integer;
  Obj: TObject;
  Ch: WideChar;
  InText: boolean;
  Tmp: WideString;

  function AlphaNum(Ch: WideChar): boolean;
  begin
  Result := (Ch in [WideChar('a')..WideChar('z'), WideChar('A')..WideChar('Z'), WideChar('0')..WideChar('9')]) 
             or (Ch >= #192);
  end;

  function GetCh(Pos: integer): WideChar;
  var
    Ch: WideChar;
    Obj1: TObject;
  begin
  Result := ' ';
  if not FSectionList.GetChAtPos(Pos, Ch, Obj1) or (Obj1 <> Obj) then Exit;
  Result := Ch;
  end;

begin
Result := False;
AWord := '';
with FSectionList do
  begin
  InText := False;   
  CaretPos := FindCursor(PaintPanel.Canvas, X,
         Y+YOff, XR, YR, CaretHt, InText);
  CursorToXy(PaintPanel.Canvas, CaretPos, X1, Y1);
  if InText then   {else cursor is past end of row}
    begin
    en := CaretPos;
    st := en-1;
    if GetChAtPos(en, Ch, Obj) and AlphaNum(Ch)  then
      begin
      AWord := Ch;
      Result := True;
      Inc(en);
      Ch := GetCh(en);
      while AlphaNum(Ch) do
        begin
        Tmp := Ch;     {Delphi 3 needs this nonsense}
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

{----------------ThtmlViewer.HTMLMouseDblClk}
procedure ThtmlViewer.HTMLMouseDblClk(Message: TWMMouse);
var
  st, en: integer;
  AWord: WideString;
begin
FSectionList.LButtonDown(True);   
if FProcessing or HotSpotAction then Exit;  
if not FNoSelect and GetWordAtCursor(Message.XPos, Message.YPos, St, En, AWord) then
  begin
  FSectionList.SelB := st+1;
  FSectionList.SelE := en;
  FCaretPos := st+1;
  InvalidateRect(PaintPanel.Handle, Nil, True);
  end;
if Assigned(FOnMouseDouble) then
  with Message do
    FOnMouseDouble(Self, mbLeft, KeysToShiftState(Keys), XPos, YPos);
end;

procedure ThtmlViewer.DoHilite(X, Y: integer);
var
  Curs, YR, YWin: integer;
  XR, CaretHt: integer;
  InText: boolean;
begin
if Hiliting and (Sel1 >= 0) then
  with FSectionList do
    begin
    YWin := IntMin(IntMax(0, Y), Height);
    Curs := FindCursor(PaintPanel.Canvas, X, YWin+YOff, XR, YR, CaretHt, InText);
    if (Curs >= 0) and not FNoSelect then
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
      InvalidateRect(PaintPanel.Handle, Nil, True);
      end;
    CaretPos := Curs;
    end;
end;

{----------------ThtmlViewer.WMMouseScroll}
procedure ThtmlViewer.WMMouseScroll(var Message: TMessage);
const
  Ticks: DWord = 0;
var
  Pos: integer;
  Pt: TPoint;
begin
GetCursorPos(Pt);
Ticks := 0;
with VScrollBar do
  begin
  Pt := PaintPanel.ScreenToClient(Pt);
  while MouseScrolling and (LeftButtonDown and((Pt.Y <= 0) or (Pt.Y > Self.Height)))
                  or (MiddleScrollOn and (Abs(Pt.Y - MiddleY) > ScrollGap)) do     
    begin
    if GetTickCount > Ticks +100 then
      begin
      Ticks := GetTickCount;
      Pos := Position;
      if LeftButtonDown then
        begin
        if Pt.Y < -15 then
          Pos := Position - SmallChange * 8
        else if Pt.Y <= 0 then
          Pos := Position - SmallChange
        else if Pt.Y > Self.Height+15 then
          Pos := Position + SmallChange * 8
        else
          Pos := Position + SmallChange;
        end
      else
        begin   {MiddleScrollOn}    
        if Pt.Y-MiddleY < -3*ScrollGap then
          Pos := Position - 32
        else if Pt.Y-MiddleY < -ScrollGap then
          Pos := Position - 8
        else if Pt.Y-MiddleY > 3*ScrollGap then
          Pos := Position + 32
        else if Pt.Y-MiddleY > ScrollGap then
          Pos := Position + 8;
        if Pos < Position then
          PaintPanel.Cursor := UpOnlyCursor
        else if Pos > Position then
          PaintPanel.Cursor := DownOnlyCursor;
        end;
      Pos := IntMax(0, IntMin(Pos, FMaxVertical - PaintPanel.Height));
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
MouseScrolling := False;
if MiddleScrollOn then
  PaintPanel.Cursor := UpDownCursor;  
end;

function ThtmlViewer.PositionTo(Dest: string): boolean;
var
  I: integer;
  Obj: TObject;
begin
Result := False;
If Dest = '' then Exit;
if Dest[1] = '#' then
  System.Delete(Dest, 1, 1);
I := FNameList.IndexOf(UpperCase(Dest));
if I > -1 then
  begin
  Obj := FNameList.Objects[I];
  if (Obj is TIDObject) then
     ScrollTo(TIDObject(Obj).YPosition); 

  HScrollBar.Position := 0;
  Result := True;
  AddVisitedLink(FCurrentFile+'#'+Dest);   
  end;
end;

function ThtmlViewer.GetURL(X, Y: integer; var UrlTarg: TUrlTarget;
          var FormControl: TImageFormControlObj; var ATitle: string): guResultType;
begin
Result := FSectionList.GetURL(PaintPanel.Canvas, X, Y+FSectionList.YOff,
          UrlTarg, FormControl, ATitle);
end;

procedure THTMLViewer.SetViewImages(Value: boolean);
var
  OldPos: integer;
  OldCursor: TCursor;
begin
if (Value <> FSectionList.ShowImages) and not FProcessing then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FSectionList.ShowImages := Value;
    if FSectionList.Count > 0 then
      begin
      FSectionList.GetBackgroundBitmap;    {load any background bitmap}
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

{----------------ThtmlViewer.InsertImage}
function ThtmlViewer.InsertImage(const Src: string; Stream: TMemoryStream): boolean;
var
  OldPos: integer;
  ReFormat: boolean;
begin
Result := False;
if FProcessing then Exit;
try
  SetProcessing(True);
  FSectionList.InsertImage(Src, Stream, Reformat);
  FSectionList.GetBackgroundBitmap;     {in case it's the one placed}
  if Reformat then
    if FSectionList.Count > 0 then
      begin
      FSectionList.GetBackgroundBitmap;    {load any background bitmap}
      OldPos := Position;
      DoLogic;
      Position := OldPos;
      end;
  Invalidate;
finally
  SetProcessing(False);
  Result := True;
  end;
end;

function THTMLViewer.GetBase: string;
begin
Result := FBase;
end;

procedure THTMLViewer.SetBase(Value: string);
begin
FBase := Value;
FBaseEx := Value;
end;

function THTMLViewer.GetBaseTarget: string;
begin
Result := FBaseTarget;
end;

function THTMLViewer.GetTitle: string;
begin
Result := FTitle;
end;

function THTMLViewer.GetCurrentFile: string;
begin
Result := FCurrentFile;
end;

function THTMLViewer.GetViewImages: boolean;
begin
Result := FSectionList.ShowImages;
end;

procedure THTMLViewer.SetColor(Value: TColor);
begin
if FProcessing then Exit;
FBackground := Value;
FSectionList.Background:= Value;
PaintPanel.Color := Value;   
Invalidate;
end;

procedure THTMLViewer.SetBorderStyle(Value: THTMLBorderStyle);
begin
if Value <> FBorderStyle then
  begin
  FBorderStyle := Value;
  DrawBorder;
  end;
end;

procedure ThtmlViewer.KeyDown(var Key: Word; Shift: TShiftState);
var
  Pos: integer;
  OrigPos:    integer;
  TheChange: integer;
begin
inherited KeyDown(Key, Shift);      
if Shift <> [] then    
  Exit;
if MiddleScrollOn then    
  begin
  MiddleScrollOn := False;
  PaintPanel.Cursor := Cursor;
  Exit;
  end;
with VScrollBar do
  if Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_HOME, VK_END] then
    begin
    Pos := Position;
    OrigPos := Pos;
    case Key of
      VK_PRIOR : Dec(Pos, LargeChange);
      VK_NEXT  : Inc(Pos, LargeChange);
      VK_UP    : Dec(Pos, SmallChange);
      VK_DOWN  : Inc(Pos, SmallChange);
      VK_Home  : Pos := 0;
      VK_End   : Pos := FMaxVertical;
      end;
    if Pos < 0 then Pos := 0;
    Pos := IntMax(0, IntMin(Pos, FMaxVertical - PaintPanel.Height));

    Position := Pos;
    FSectionList.SetYOffset(Pos);

    TheChange := OrigPos-Pos;
    if not BGFixed and (abs(TheChange) = SmallChange) then
      begin  {update only the scrolled part}
      ScrollWindow(PaintPanel.Handle, 0, TheChange, NIL, NIL);
      PaintPanel.Update;
      end
    else PaintPanel.Invalidate;
    end;

with HScrollBar do
  if Key in [VK_LEFT, VK_RIGHT] then
    begin
    Pos := Position;
    case Key of
      VK_LEFT  : Dec(Pos, SmallChange);
      VK_RIGHT : Inc(Pos, SmallChange);
      end;
    if Pos < 0 then Pos := 0;
    Pos := IntMin(Pos, Max - PaintPanel.Width);
    Position := Pos;
    PaintPanel.Invalidate;
    end;
end;

procedure ThtmlViewer.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {else don't get the arrow keys}
end;

function ThtmlViewer.GetPosition: integer;
var
  Index: integer;
  TopPos, Pos: integer;
  S: TSectionBase;
begin
Pos := integer(VScrollBar.Position);
S:= FSectionList.FindSectionAtPosition(Pos, TopPos, Index);
if Assigned(S) then
  Result := integer(Index+1) shl 16 + ((Pos - TopPos) and $FFFF)
else Result := Pos;
{Hiword is section # plus 1, Loword is displacement from top of section
 HiWord = 0 is top of display}
end;

procedure ThtmlViewer.SetPosition(Value: integer);
var
  TopPos: integer;
begin
if HiWord(Value) = 0 then
  ScrollTo(LoWord(Value))
else if (Hiword(Value)-1 < FSectionList.PositionList.Count) then
  begin
  TopPos := TSectionBase(FSectionList.PositionList[HiWord(Value)-1]).YPosition;
  ScrollTo(TopPos + LoWord(Value));
  end;
end;

function ThtmlViewer.GetScrollPos: integer;
begin
Result := VScrollBar.Position;
end;

procedure ThtmlViewer.SetScrollPos(Value: integer);
begin
if Value < 0 then Value := 0;
Value := IntMin(Value, FMaxVertical - PaintPanel.Height);
if Value <> GetScrollPos then
  ScrollTo(Value);
end;

function ThtmlViewer.GetScrollBarRange: integer;
begin
Result := FMaxVertical - PaintPanel.Height;
end;

function ThtmlViewer.GetHScrollPos: integer;    
begin
Result := HScrollBar.Position;
end;

procedure ThtmlViewer.SetHScrollPos(Value: integer);   
begin
if Value < 0 then Value := 0;
Value := IntMin(Value, HScrollBar.Max-PaintPanel.Width);
HScrollbar.Position := Value;
Invalidate;
end;

function ThtmlViewer.GetHScrollBarRange: integer;  
begin
Result := HScrollBar.Max - PaintPanel.Width;
end;

function ThtmlViewer.GetPalette: HPALETTE;
begin
if ThePalette <> 0 then
  Result := ThePalette
else Result := inherited GetPalette;
Invalidate;
end;

function ThtmlViewer.HTMLExpandFilename(const Filename: string): string;
var
  Tmp: string;
begin
{pass http: and other protocols except for file:///}    
if (Pos('://', Filename) > 1) and (Pos('file://', Lowercase(Filename)) = 0) then
  Result := Filename
else
  begin
  Result := HTMLServerToDos(Trim(Filename), FServerRoot);

  if Pos('\', Result) = 1 then
    Result := ExpandFilename(Result)
  else if (Pos(':', Result)<> 2) and (Pos('\\', Result) <> 1) then
    if CompareText(FBase, 'DosPath') = 0 then  {let Dos find the path}
    else if FBase <> '' then
      begin
      Tmp := ExtractFilePath(HTMLToDos(FBase));    
      Result := ExpandFilename(Tmp + Result)
      end
    else
      Result := ExpandFilename(ExtractFilePath(FCurrentFile) + Result);
  end;
end;

{----------------ThtmlViewer.BumpHistory}
procedure ThtmlViewer.BumpHistory(const FileName, Title: string;
              OldPos: integer; OldFormData: TFreeList; ft: ThtmlFileType);  
var
  I: integer;
  PO: PositionObj;
  SameName: boolean;
begin
SameName := FileName = FCurrentFile;  
if (FHistoryMaxCount > 0) and  (FCurrentFile <> '') and
         ((not SameName) or (FCurrentFileType <> ft)
         or (OldPos <> Position)) then
  with FHistory do
    begin
    if (Count > 0) and (Filename <> '') then
      begin
      Strings[FHistoryIndex] := Filename;
      with PositionObj(FPositionHistory[FHistoryIndex]) do
        begin
        Pos := OldPos;
        FileType := ft;
        if not SameName then  {only stored when documents changed}
          FormData := OldFormData    
        else OldFormData.Free;
        end;
      FTitleHistory[FHistoryIndex] := Title;
      for I := 0 to FHistoryIndex-1 do
        begin
        Delete(0);
        FTitleHistory.Delete(0);
        PositionObj(FPositionHistory[0]).Free;    
        FPositionHistory.Delete(0);
        end;
      end;
    FHistoryIndex := 0;
    Insert(0, FCurrentFile);
    PO := PositionObj.Create;
    PO.Pos := Position;
    PO.FileType := FCurrentFileType;
    FPositionHistory.Insert(0, PO);
    FTitleHistory.Insert(0, FTitle);
    if Count > FHistoryMaxCount then
      begin
      Delete(FHistoryMaxCount);
      FTitleHistory.Delete(FHistoryMaxCount);
      PositionObj(FPositionHistory[FHistoryMaxCount]).Free;
      FPositionHistory.Delete(FHistoryMaxCount);
      end;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end
else OldFormData.Free;  
end;

procedure ThtmlViewer.SetHistoryIndex(Value: integer);
var
  I: integer;

  function GetLowestSameFileIndex(Start: integer): integer;  
  begin
  Result := Start;
  while (Result > 0) and (FHistory[Result-1] = FCurrentFile) do
    Dec(Result);
  end;

begin
with FHistory do
  if (Value <> FHistoryIndex) and (Value >= 0) and (Value < Count)
            and not FProcessing then
    begin
    if FCurrentFile <> '' then
      begin          {save the current information}
      Strings[FHistoryIndex] := FCurrentFile;
      with PositionObj(FPositionHistory[FHistoryIndex]) do
        begin
        Pos := Position;
        FileType := FCurrentFileType;
        I := GetLowestSameFileIndex(FHistoryIndex);   
        PositionObj(FPositionHistory[I]).FormData := GetFormData;     
        end;
      FTitleHistory[FHistoryIndex] := FTitle;
      end;
    with PositionObj(FPositionHistory[Value]) do
      begin                 {reestablish the new desired history position}
      if (FCurrentFile <> Strings[Value]) or (FCurrentFileType <> FileType) then
        Self.LoadFile(Strings[Value], FileType);
      Position := Pos;
      I := GetLowestSameFileIndex(Value);
      with PositionObj(FPositionHistory[I]) do
        begin
        SetFormData(FormData);    {reload the forms if any}
        FormData.Free;           
        FormData := Nil;
        end;
      end;
    FHistoryIndex := Value;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

procedure ThtmlViewer.SetHistoryMaxCount(Value: integer);
begin
if (Value = FHistoryMaxCount) or (Value < 0) then Exit;
if Value < FHistoryMaxCount then
  ClearHistory;
FHistoryMaxCount := Value;
end;

procedure ThtmlViewer.ClearHistory;
var
  CountWas: integer;
begin
CountWas := FHistory.Count;
FHistory.Clear;
FTitleHistory.Clear;
FPositionHistory.Clear;
FHistoryIndex := 0;
FCurrentFile := '';   
if (CountWas > 0) and Assigned(FOnHistoryChange) then
  FOnHistoryChange(Self);
end;

function ThtmlViewer.GetPreFontName: TFontName;
begin
Result := FPreFontName;
end;

procedure ThtmlViewer.SetPreFontName(Value: TFontName);
begin
if  CompareText(Value, FSectionList.PreFontName) <> 0 then
  begin
  FPreFontName := Value;
  FSectionList.PreFontName := Value;
  end;
end;

procedure ThtmlViewer.SetFontSize(Value: integer);
begin
FFontSize := Value;
end;

procedure ThtmlViewer.SetCharset(Value: TFontCharset);
begin
FCharset := Value;
end;

function ThtmlViewer.GetFormControlList: TList;
begin
Result := FSectionList.FormControlList;
end;

function ThtmlViewer.GetNameList: TStringList;
begin
Result := FNameList;
end;

function ThtmlViewer.GetLinkList: TList;
begin
Result := FSectionList.LinkList;
end;

procedure ThtmlViewer.SetHotSpotColor(Value: TColor);
begin
FHotSpotColor := Value;
FSectionList.HotSpotColor := Value;
end;

procedure ThtmlViewer.SetVisitedColor(Value: TColor);
begin
FVisitedColor := Value;
FSectionList.LinkVisitedColor := Value;
end;

procedure ThtmlViewer.SetActiveColor(Value: TColor);
begin
FOverColor := Value;
FSectionList.LinkActiveColor := Value;
end;

procedure ThtmlViewer.SetVisitedMaxCount(Value: integer);
var
  I: integer;
begin
Value := IntMax(Value, 0);
if Value <> FVisitedMaxCount then
  begin
  FVisitedMaxCount := Value;
  if FVisitedMaxCount = 0 then
    begin
    Visited.Clear;
    for I := 0 to SectionList.LinkList.Count-1 do
      TFontObj(LinkList[I]).Visited := False;
    Invalidate;
    end
  else
    begin
    FVisitedMaxCount := Value;
    for I := Visited.Count-1 downto FVisitedMaxCount do
      Visited.Delete(I);
    end;
  end;
end;

function ThtmlViewer.GetCursor: TCursor;     
begin
Result := inherited Cursor;
end;

procedure ThtmlViewer.SetCursor(Value: TCursor);  
begin
if Value = OldThickIBeamCursor then   {no longer used}
  Value := crIBeam;
inherited Cursor := Value;
end;

function ThtmlViewer.FullDisplaySize(FormatWidth: integer): TSize;
var
  Curs: integer;
  CopyList: TSectionList;
begin
Result.cx := 0;  {error return}
Result.cy := 0;
if FormatWidth > 0 then
  begin
  CopyList := TSectionList.CreateCopy(FSectionList);
  try
    Curs := 0;
    Result.cy := CopyList.DoLogic(PaintPanel.Canvas, 0, FormatWidth, 300, 0, Result.cx, Curs);
  finally
    CopyList.Free;
    end;
  end;
end;

{----------------CalcBackgroundLocationAndTiling}
procedure CalcBackgroundLocationAndTiling(const PRec: PtPositionRec; ARect: TRect;
            XOff, YOff, IW, IH, BW, BH: integer; var X, Y, X2, Y2: integer);

{PRec has the CSS information on the background image, it's starting location and
 whether it is tiled in x, y, neither, or both.
 ARect is the cliprect, no point in drawing tiled images outside it.
 XOff, YOff are offsets which allow for the fact that the viewable area may not be at 0,0.
 IW, IH are the total width and height of the document if you could see it all at once.
 BW, BH are bitmap dimensions used to calc tiling.
 X, Y are the position (window coordinates) where the first background iamge will be drawn.
 X2, Y2 are tiling limits.  X2 and Y2 may be such that 0, 1, or many images will
   get drawn.  They're calculated so that only images within ARect are drawn.
}
var
  I: integer;
  P: array[1..2] of integer;
begin
{compute the location of the prime background image. Tiling can go either way
 from this image}
P[1] := 0;  P[2] := 0;
for I := 1 to 2 do   {I = 1 is X info, I = 2 is Y info}
  with PRec[I] do
    begin
    case PosType of
      pTop:
        P[I] := - YOff;
      pCenter:
        if I = 1 then
          P[1] := IW div 2 - BW div 2 - XOff
        else P[2] := IH div 2 - BH div 2 - YOff;
      pBottom:
        P[I] := IH - BH - YOff;
      pLeft:
        P[I] := -XOff;
      pRight:
        P[I] := IW - BW - XOff;
      PPercent:
        if I = 1 then
          P[1] := ((IW-BW) * Value) div 100  - XOff
        else P[2] := ((IH-BH) * Value div 100) - YOff;
      pDim:
        if I = 1 then
          P[I] := Value-XOff
        else P[I] := Value-YOff;
      end;
    end;

{Calculate the tiling keeping it within the cliprect boundaries}
X := P[1];
Y := P[2];
if PRec[2].RepeatD then
  begin    {y repeat}
  {figure a starting point for tiling.  This will be less that one image height
   outside the cliprect}
  if Y < ARect.Top then
    Y := Y+ ((ARect.Top-Y)div BH)*BH
  else if Y > ARect.Top then
    Y := Y - ((Y-ARect.Top)div BH)*BH - BH;
  Y2 := ARect.Bottom;
  end
else
  begin   {a single image or row}
  Y2 := Y; {assume it's not in the cliprect and won't be output}
  if not((Y > ARect.Bottom) or (Y+BH < ARect.Top)) then
    Inc(Y2); {it is in the clip rect, show it}
  end;
if PRec[1].RepeatD then
  begin       {x repeat}
  {figure a starting point for tiling.  This will be less that one image width
   outside the cliprect}
  if X < ARect.Left then
    X := X+ ((ARect.Left-X)div BW)*BW
  else if X > ARect.Left then
    X := X - ((X-ARect.Left)div BW)*BW - BW;
  X2 := ARect.Right;
  end
else
  begin    {single image or column}
  X2 := X;  {assume it's not in the cliprect and won't be output}
  if not((X > ARect.Right) or (X+BW < ARect.Left)) then
    Inc(X2); {it is in the clip rect, show it}
  end;
end;

{----------------DrawBackground}
procedure DrawBackground(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: integer;
              Image: TGpObject; Mask: TBitmap; AniGif: TGifImage; BW, BH: integer; BGColor: TColor);
{draw the background color and any tiled images on it}
{ARect, the cliprect, drawing outside this will not show but images may overhang
 XStart, YStart are first image position already calculated for the cliprect and parameters.
 XLast, YLast   Tiling stops here.
 BW, BH  bitmap dimensions.
}
var
  X, Y: integer;
  OldBrush: HBrush;
  OldPal: HPalette;
  DC: HDC;
  OldBack, OldFore: TColor;
  Bitmap: TBitmap;
  graphics: TGpGraphics;
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
    ACanvas.FillRect(ARect);   {background color}
    if Assigned(AniGif) then   {tile the animated gif}
      begin
      Y := YStart;
      while Y < YLast do
        begin
        X := XStart;
        while X < XLast do
          begin
          AniGif.Draw(ACanvas,X, Y, BW, BH);
          Inc(X, BW);
          end;
        Inc(Y, BH);
        end;
      end
    else if Assigned(Image) then   {tile the bitmap}
      if Image is TBitmap then
      begin
      Bitmap := TBitmap(Image);
      Y := YStart;
      while Y < YLast do
        begin
        X := XStart;
        while X < XLast do
          begin
          if Mask = Nil then
            BitBlt(DC, X, Y, BW, BH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
          else
            begin
            BitBlt(dc, X, Y, BW, BH, Bitmap.Canvas.Handle, 0, 0, SrcInvert);
            BitBlt(dc, X, Y, BW, BH, Mask.Canvas.Handle, 0, 0, SrcAnd);
            BitBlt(dc, X, Y, BW, BH, Bitmap.Canvas.Handle, 0, 0, SrcPaint);
            end;
          Inc(X, BW);
          end;
        Inc(Y, BH);
        end;
        end
      {$ifndef NoMetafile}
      else if Image is ThtMetafile then   
        begin
        Y := YStart;
        try
          while Y < YLast do
            begin
            X := XStart;
            while X < XLast do
              begin
              ACanvas.Draw(X, Y, ThtMetaFile(Image));
              Inc(X, BW);
              end;
            Inc(Y, BH);
            end;
        except
          end;
        end
      {$endif}
      else
        begin
        Y := YStart;
        graphics := TGPGraphics.Create(DC);
        try
          while Y < YLast do
            begin
            X := XStart;
            while X < XLast do
              begin
              graphics.DrawImage(TGpImage(Image), X, Y, BW, BH);
              Inc(X, BW);
              end;
            Inc(Y, BH);
            end;
        except
          end;
        Graphics.Free;
        end;
  finally
    SelectObject(DC, OldBrush);
    SelectPalette(DC, OldPal, False);
    RealizePalette(DC);
    SetBkColor(DC, OldBack);     
    SetTextColor(DC, OldFore);
    end;
  end;
end;

{----------------ThtmlViewer.DrawBackground2}
procedure ThtmlViewer.DrawBackground2(ACanvas: TCanvas; ARect: TRect; XStart, YStart, XLast, YLast: integer;
              Image: TGpObject; Mask: TBitmap; BW, BH: integer; BGColor: TColor);
{Called by DoBackground2 (Print and PrintPreview)}
{draw the background color and any tiled images on it}
{ARect, the cliprect, drawing outside this will not show but images may overhang
 XStart, YStart are first image position already calculated for the cliprect and parameters.
 XLast, YLast   Tiling stops here.
 BW, BH  Image dimensions.
}
var
  X, Y: integer;
  OldBrush: HBrush;
  OldPal: HPalette;
  DC: HDC;
  OldBack, OldFore: TColor;
  Bitmap: TBitmap;
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
    ACanvas.FillRect(ARect);   {background color}
    if Assigned(Image) then   {tile the Image}
      if Image is TBitmap then
        begin
        Bitmap := TBitmap(Image);
        Y := YStart;
        while Y < YLast do
          begin
          X := XStart;
          while X < XLast do
            begin
            if Mask = Nil then
              PrintBitmap(ACanvas, X, Y, BW, BH, Bitmap.Handle)
            else
              begin
              PrintTransparentBitmap3(ACanvas, X, Y, BW, BH, Bitmap, Mask, 0, Bitmap.Height);
              end;
            Inc(X, BW);
            end;
          Inc(Y, BH);
          end;
        end
      {$ifndef NoMetafile}
      else if Image is ThtMetafile then
        begin
        Y := YStart;
        try
          while Y < YLast do
            begin
            X := XStart;
            while X < XLast do
              begin
              ACanvas.Draw(X, Y, ThtMetaFile(Image));
              Inc(X, BW);
              end;
            Inc(Y, BH);
            end;
        except
          end;
        end
      {$endif}
      else
        begin
        Y := YStart;
        try
          while Y < YLast do
            begin
            X := XStart;
            while X < XLast do
              begin
              StretchPrintGpImageOnColor(ACanvas, TGPImage(Image), X, Y, BW, BH, BGColor);
              Inc(X, BW);
              end;
            Inc(Y, BH);
            end;
        except
          end;
        end;
  finally
    SelectObject(DC, OldBrush);
    SelectPalette(DC, OldPal, False);
    RealizePalette(DC);
    SetBkColor(DC, OldBack);
    SetTextColor(DC, OldFore);
    end;
  end;
end;

procedure ThtmlViewer.DoBackground1(ACanvas: TCanvas; ATop, AWidth, AHeight, FullHeight: integer);
var
  ARect: TRect;
  Image: TGpObject;
  Mask: TBitmap;
  PRec: PtPositionRec;
  BW, BH, X, Y, X2, Y2, IW, IH, XOff, YOff: integer;
  Fixed: boolean;

begin
ARect := Rect(0, 0, AWidth, AHeight);
Image := FSectionList.BackgroundBitmap;
if FSectionList.ShowImages and Assigned(Image) then
  begin
  Mask := FSectionList.BackgroundMask;
  BW := GetImageWidth(Image);
  BH := GetImageHeight(Image);
  PRec := FSectionList.BackgroundPRec;
  Fixed := PRec[1].Fixed;
  if Fixed then
    begin  {fixed background}
    XOff := 0;
    YOff := 0;
    IW := AWidth;
    IH := AHeight;
    end
  else
    begin   {scrolling background}
    XOff := 0;
    YOff := ATop;
    IW := AWidth;
    IH := FullHeight;
    end;

  {Calculate where the tiled background images go}
  CalcBackgroundLocationAndTiling(PRec, ARect, XOff, YOff, IW, IH, BW, BH, X, Y, X2, Y2);

  DrawBackground(ACanvas, ARect, X, Y, X2, Y2, Image, Mask, Nil, BW, BH, PaintPanel.Color);
  end
else
  begin  {no background image, show color only}
  DrawBackground(ACanvas, ARect, 0,0,0,0, Nil, Nil, Nil, 0, 0, PaintPanel.Color);   
  end;
end;

procedure ThtmlViewer.DoBackground2(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: integer; AColor: TColor);
{called by Print and PrintPreview}
var
  ARect: TRect;
  Image: TGpObject;
  Mask: TBitmap;
  PRec: PtPositionRec;
  BW, BH, X, Y, X2, Y2, IW, IH, XOff, YOff: integer;
  NewBitmap, NewMask: TBitmap;

begin
ARect := Rect(ALeft, ATop, ALeft+AWidth, ATop+AHeight);
Image := FSectionList.BackgroundBitmap;
if FSectionList.ShowImages and Assigned(Image) then
  begin
  Mask := FSectionList.BackgroundMask;
  BW := GetImageWidth(Image);
  BH := GetImageHeight(Image);
  PRec := FSectionList.BackgroundPRec;
  XOff := -ALeft;
  YOff := -ATop;
  IW := AWidth;
  IH := AHeight;

  {Calculate where the tiled background images go}
  CalcBackgroundLocationAndTiling(PRec, ARect, XOff, YOff, IW, IH, BW, BH, X, Y, X2, Y2);

  if (BW = 1) or (BH = 1) then
    begin  {this is for people who try to tile 1 pixel images}
    NewBitmap := EnlargeImage(Image, X2-X, Y2-Y);
    try
      if Assigned(Mask) then
        NewMask := TBitmap(EnlargeImage(Mask, X2-X, Y2-Y))
      else NewMask := Nil;
      try
        DrawBackground2(ACanvas, ARect, X, Y, X2, Y2, NewBitmap, NewMask, NewBitmap.Width, NewBitmap.Height, AColor);
      finally
        NewMask.Free;
        end;
    finally
      NewBitmap.Free;
      end;
    end
  else
    DrawBackground2(ACanvas, ARect, X, Y, X2, Y2, Image, Mask, BW, BH, AColor);
  end
else
  begin  {no background image, show color only}
  DrawBackground2(ACanvas, ARect, 0,0,0,0, Nil, Nil, 0, 0, AColor);
  end;
end;

type
  EExcessiveSizeError = Class(Exception);

function ThtmlViewer.MakeMetaFile(YTop, FormatWidth, Width, Height: integer): TMetaFile;
var
  CopyList: TSectionList;
  Dummy: integer;
  Curs: integer;
  Canvas: TMetaFileCanvas;
  DocHeight: integer;
begin
Result := Nil;
if FProcessing or (FSectionList.Count = 0) then
  Exit;
if Height > 4000 then
  Raise EExcessiveSizeError.Create('Vertical Height exceeds 4000');
CopyList := TSectionList.CreateCopy(FSectionList);
try
  Result := TMetaFile.Create;
  Result.Width := Width;
  Result.Height := Height;
  Canvas := TMetaFileCanvas.Create(Result, 0);
  try
    Curs := 0;
    DocHeight := CopyList.DoLogic(Canvas, 0, FormatWidth, Height, 0, Dummy, Curs);
    DoBackground1(Canvas, YTop, Width, Height, DocHeight);

    CopyList.SetYOffset(IntMax(0, YTop));
    CopyList.Draw(Canvas, Rect(0, 0, Width, Height), MaxHScroll, 0, 0, 0,0);
  except
    Result.Free;
    Result := Nil;
    end;
  Canvas.Free;
finally
  CopyList.Free;
  end;
end;

function ThtmlViewer.MakePagedMetaFiles(Width, Height: integer): TList;   
var
  ARect, CRect: TRect;
  CopyList: TSectionList;
  HTop, OldTop, Dummy, Curs, I: integer;
  Done: boolean;
  VPixels: integer;
  Canvas: TMetaFileCanvas;
  MF: TMetaFile;
  TablePart: TablePartType;
  SavePageBottom: Integer;
  hrgnClip1: hRgn;

  procedure PaintBackground(Canvas: TCanvas; Top, Bot: integer);
  begin
  Canvas.Brush.Color := CopyList.Background;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect(0, Top, Width+1, Bot));
  end;

begin
Done := False;
Result := Nil;
TablePartRec := Nil;   
if FProcessing or (SectionList.Count = 0) then
  Exit;
CopyList := TSectionList.CreateCopy(SectionList);
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
          if HTop = 0 then  {DoLogic the first time only}
            VPixels := CopyList.DoLogic(Canvas, 0, Width, Height, 0, Dummy, Curs);
          CopyList.SetYOffset(HTop);
          PaintBackground(Canvas, 0, Height);

          repeat
            if Assigned(TablePartRec) then
              TablePart := TablePartRec.TablePart
            else TablePart := Normal;
            case TablePart of
              Normal:
                begin
          CopyList.Draw(Canvas, ARect, Width, 0, 0, 0,0);
                PaintBackground(Canvas, CopyList.PageBottom-HTop, Height+1);
                end;
              DoHead:
                begin
                CopyList.SetYOffset(TablePartRec.PartStart);
                CRect := ARect;
                CRect.Bottom := CRect.Top+TablePartRec.PartHeight;
                hrgnClip1 := CreateRectRgn(0, 0, Width+1, CRect.Bottom);
                SelectClipRgn(Canvas.Handle, hrgnClip1);
                DeleteObject(hrgnClip1);
                CopyList.Draw(Canvas, CRect, Width, 0, 0, 0,0);
                end;
              DoBody1, DoBody3:
                begin
                CRect := ARect;
                CRect.Top := CopyList.PageBottom-1-CopyList.YOff;
                CopyList.SetYOffset(TablePartRec.PartStart-CRect.top);
                CopyList.Draw(Canvas, CRect, Width, 0+3*Width, 0, 0,0);      {off page}

                TablePartRec.TablePart := DoBody2;
                CRect.Bottom := CopyList.PageBottom-CopyList.YOff+1;    
                hrgnClip1 := CreateRectRgn(0, CRect.Top, Width+1, CRect.Bottom);
                SelectClipRgn(Canvas.Handle, hrgnClip1);
                DeleteObject(hrgnClip1);
                CopyList.Draw(Canvas, CRect, Width, 0, 0, 0,0);      {onpage}
                if not Assigned(TablePartRec)
                       or not (TablePartRec.TablePart in [Normal]) then
                  PaintBackground(Canvas, CopyList.PageBottom-CopyList.YOff, Height+1);
                end;
              DoFoot:
                begin
                SavePageBottom := CopyList.PageBottom;
                CRect := ARect;
                CRect.Top := CopyList.PageBottom-CopyList.YOff;
                CRect.Bottom := CRect.Top + TablePartRec.PartHeight;
                hrgnClip1 := CreateRectRgn(0, CRect.Top, Width+1, CRect.Bottom);
                SelectClipRgn(Canvas.Handle, hrgnClip1);
                DeleteObject(hrgnClip1);
                CopyList.SetYOffset(TablePartRec.PartStart-CRect.top);
                CopyList.Draw(Canvas, CRect, Width, 0, 0, 0,0);
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
        Raise;
        end;
      Result.Add(MF);
      HTop := CopyList.PageBottom;
      Inc(CopyList.LinkPage);      
      Application.ProcessMessages;
      if (HTop >= VPixels) or (HTop <= OldTop) then  {see if done or endless loop}
        Done := True;
      OldTop := HTop;
      end;
  except
    for I := 0 to Result.Count-1 do
      TMetaFile(Result.Items[I]).Free;
    FreeAndNil(Result);
    Raise;
  end;
finally
  CopyList.Free;
  end;
end;

function ThtmlViewer.MakeBitmap(YTop, FormatWidth, Width, Height: integer): TBitmap;
var
  CopyList: TSectionList;
  Dummy: integer;
  Curs: integer;
  DocHeight: integer;

begin
Result := Nil;
if FProcessing or (FSectionList.Count = 0) then
  Exit;
if Height > 4000 then
  Raise EExcessiveSizeError.Create('Vertical Height exceeds 4000');
CopyList := TSectionList.CreateCopy(FSectionList);
try
  Result := TBitmap.Create;
  try
    Result.HandleType := bmDIB;
    Result.PixelFormat := pf24Bit;
    Result.Width := Width;
    Result.Height := Height;
    Curs := 0;
    DocHeight := CopyList.DoLogic(Result.Canvas, 0, FormatWidth, Height, 300, Dummy, Curs);
    DoBackground1(Result.Canvas, YTop, Width, Height, DocHeight);

    CopyList.SetYOffset(IntMax(0, YTop));
    CopyList.Draw(Result.Canvas, Rect(0, 0, Width, Height), MaxHScroll, 0, 0, 0,0);
  except
    Result.Free;
    Result := Nil;
    end;
finally
  CopyList.Free;
  end;
end;

function ThtmlViewer.CreateHeaderFooter: ThtmlViewer;
begin
Result := ThtmlViewer.Create(Nil);
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

procedure ThtmlViewer.Print(FromPage, ToPage: integer);
var
  ARect, CRect: TRect;
  PrintList: TSectionList;
  P1, P2, P3, W, H, HTop, OldTop, Dummy: integer;  
  Curs: integer;
  Done: boolean;
  DC : HDC;
  PrinterOpen: boolean;   

  UpperLeftPagePoint, { these will contain Top/Left and Bottom/Right unprintable area}
  LowerRightPagePoint: TPoint;
  MLeft: integer;
  MLeftPrn: integer;
  MRightPrn: integer;
  MTopPrn: integer;
  MBottomPrn: integer;
  TopPixels, TopPixelsPrn, HPrn, WPrn: integer;
  hrgnClip, hrgnClip1: THandle;
  savedFont : TFont ;
  savedPen : TPen ;
  savedBrush : TBrush ;
  Align, ScaledPgHt, ScaledPgWid, VPixels: integer;
  FootViewer, HeadViewer: ThtmlViewer;  

  DeltaMarginTop: Double;     
  SavePrintMarginTop: Double;
  DeltaPixelsPrn: Integer;
  DeltaPixels: Integer;
  OrigTopPixels: Integer;
  OrigTopPixelsPrn: Integer;
  OrigHprn: Integer;
  OrigH: Integer;
  LastPrintMarginTop: Double;
  MLeftSide: Integer;
  TablePart: TablePartType;
  SavePageBottom: Integer;

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

  procedure WhiteoutArea(Canvas: TCanvas; Y: integer);
  {White out excess printing.  Y is top of the bottom area to be blanked.}
  begin
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Color   := clWhite;
  Canvas.Rectangle(MLeft, 0, W + MLeft+1, TopPixels-1);
  Canvas.Rectangle(MLeft, Y, W + MLeft+1, TopPixels+H+1);
  if (htPrintBackground in FOptions) and (Y-TopPixels < H) then
    begin   {need to reprint background in whited out area}
    hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(Y, P3, P2)-2,
                     MLeftPrn+WPrn, TopPixelsPrn+HPrn);
    SelectClipRgn(Canvas.Handle, hrgnClip1);
    DeleteObject(hrgnClip1);
    DoBackground2(Canvas, MLeft, TopPixels, W, H, PaintPanel.Color);
    end;
  RestoreCanvasItems(Canvas);
  end;

  procedure DoHTMLHeaderFooter(Footer: boolean; Event: ThtmlPagePrinted; HFViewer: ThtmlViewer);
  var
    YOrigin, YOff, Ht: integer;
    HFCopyList: TSectionList;
    BRect: TRect;
    DocHeight, XL, XR: integer;
  begin
  if not Assigned(Event) then
    Exit;
  try
    XL := MLeft;
    XR := MLeft + W;
    Event(Self, HFViewer, FPage, PrintList.PageBottom > VPixels, XL, XR, Done);  {call event handler} 
    HFCopyList := TSectionList.CreateCopy(HFViewer.SectionList);
    try
      HFCopyList.Printing := True;
      HFCopyList.ScaleX := fScaleX;     
      HFCopyList.ScaleY := fScaleY;
      Curs := 0;
      DocHeight := HFCopyList.DoLogic(vwP.Canvas, 0, XR-XL, 300, 0, Dummy, Curs);
      if not Footer then
        begin  {Header}
        YOrigin := 0;
        Ht := TopPixels;
        YOff := DocHeight-TopPixels;
        end
      else
        begin   {Footer}
        YOrigin := -(TopPixels+H);
        Ht := IntMin(ScaledPgHt-(TopPixels+H), DocHeight);
        YOff := 0;
        end;
      SetWindowOrgEx(DC, 0, YOrigin, nil);
      HFViewer.DoBackground2(vwP.Canvas, XL, -YOff, XR-XL, DocHeight, HFViewer.PaintPanel.Color);
      HFCopyList.SetYOffset(YOff);
      BRect := Rect(XL, 0, XR, Ht);
      HFCopyList.Draw(vwP.Canvas, BRect, XR-XL, XL, 0, 0,0);
    finally
      HFCopyList.Free;
      end;
  except
    end;
  end;

begin
Done := False;
FootViewer := Nil;
HeadViewer := Nil;
TablePartRec := Nil;   
if Assigned(FOnPageEvent) then
  FOnPageEvent(Self, 0, Done);
FPage := 0;
if FProcessing or (FSectionList.Count = 0) then Exit;
PrintList := TSectionList.CreateCopy(FSectionList);
PrintList.SetYOffset(0);
SavePrintMarginTop := FPrintMarginTop;  
try
  savedFont := TFont.Create ;
  savedPen := TPen.Create ;
  savedBrush := TBrush.Create ;
  try
    PrintList.Printing := True;
    PrintList.SetBackground(clWhite);
    if not assigned(vwP) then    
      begin
      vwP := TvwPrinter.Create;
      OldPrinter := vwSetPrinter(vwP);
      PrinterOpen := False;
      end
    else PrinterOpen := True;
    FPage := 1;
    hrgnClip := 0;
    try
      with vwP do
        begin
        if (DocumentTitle <> '') then
            vwP.Title := DocumentTitle ;
        if not Printing then       
          BeginDoc
        else NewPage;
        SaveCanvasItems(Canvas); 
        DC := Canvas.Handle;
        P3 := GetDeviceCaps(DC, LOGPIXELSY);
        P1 := GetDeviceCaps(DC, LOGPIXELSX);
        P2 := Round(Screen.PixelsPerInch * FPrintScale);
        fScaleX := 100.0/P3;
        fScaleY := 100.0/P1;
        PrintList.ScaleX := fScaleX;
        PrintList.ScaleY := fScaleY;
        SetMapMode(DC, mm_AnIsotropic);
        SetWindowExtEx(DC, P2, P2, Nil);
        SetViewPortExtEx(DC, P1,P3, Nil);


        { calculate the amount of space that is non-printable }

        { get PHYSICAL page width }
              LowerRightPagePoint.X := GetDeviceCaps(Printer.Handle, PhysicalWidth);
              LowerRightPagePoint.Y := GetDeviceCaps(Printer.Handle, PhysicalHeight);

              { now compute a complete unprintable area rectangle
               (composed of 2*width, 2*height) in pixels...}
              with LowerRightPagePoint do
                 begin
                   Y := Y - Printer.PageHeight;
                   X := X - Printer.PageWidth;
                 end;

        { get upper left physical offset for the printer... ->
          printable area <> paper size }
              UpperLeftPagePoint.X := GetDeviceCaps(Printer.Handle, PhysicalOffsetX);
              UpperLeftPagePoint.Y := GetDeviceCaps(Printer.Handle, PhysicalOffsetY);

        { now that we know the TOP and LEFT offset we finally can
          compute the BOTTOM and RIGHT offset: }
        with LowerRightPagePoint do
        begin
          x := x - UpperLeftPagePoint.x;
          { we don't want to have negative values}
          if x < 0 then
            x := 0; { assume no right printing offset }

          y := y - UpperLeftPagePoint.y;
          { we don't want to have negative values}
          if y < 0 then
            y := 0; { assume no bottom printing offset }
        end;
        { which results in LowerRightPoint containing the BOTTOM
          and RIGHT unprintable
          area offset; using these we modify the (logical, true)
          borders...}

        MLeftPrn := trunc(FPrintMarginLeft/2.54 * P1);
        MLeftPrn := MLeftPrn - UpperLeftPagePoint.x; { subtract physical offset }
        MLeft := MulDiv(MLeftPrn, P2, P1);

        MRightPrn := trunc(FPrintMarginRight/2.54 * P1);
        MRightPrn := MRightPrn - LowerRightPagePoint.x; { subtract physical offset }

        WPrn := PageWidth - (MLeftPrn + MRightPrn);

        W := MulDiv(WPrn, P2, P1);

        MTopPrn := trunc(FPrintMarginTop/2.54 * P3);
        MTopPrn := MTopPrn - UpperLeftPagePoint.y; { subtract physical offset }

        MBottomPrn := trunc(FPrintMarginBottom/2.54 * P3);
        MBottomPrn := MBottomPrn - LowerRightPagePoint.y; { subtract physical offset }

        TopPixelsPrn := MTopPrn;
        TopPixels := MulDiv(TopPixelsPrn, P2, P3);  

        HPrn := PageHeight-(MTopPrn+MBottomPrn);
        H := MulDiv(HPrn, P2, P3);  {scaled pageHeight}

        Curs := 0;
        VPixels := PrintList.DoLogic(Canvas, 0, W, H, 0, Dummy, Curs);

        Done := False;
        HTop := 0;
        OldTop := 0;
        ScaledPgHt := MulDiv(PageHeight, P2, P3);
        ScaledPgWid := MulDiv(PageWidth, P2, P3);
        hrgnClip := CreateRectRgn(MLeftPrn, TopPixelsPrn-1, WPrn + MLeftPrn+2,
                  TopPixelsPrn + HPrn+2);
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
        LastPrintMarginTop := FPrintMarginTop;

        while (FPage <= ToPage) and not Done do
          begin
          PrintList.SetYOffset(HTop-TopPixels);
          SetMapMode(DC, mm_AnIsotropic);
          SetWindowExtEx(DC, P2, P2, Nil);
          SetViewPortExtEx(DC, P1,P3, Nil);
          SetWindowOrgEx(DC, 0, 0, Nil);
          SelectClipRgn(DC, hrgnClip);

          if FPage >= FromPage then
            begin
            if (htPrintBackground in FOptions) then
              DoBackground2(Canvas, MLeft, TopPixels, W, H, PaintPanel.Color);
            MLeftSide := MLeft;
            end
          else MLeftSide := MLeft+3*W;   {to print off page}

          repeat
            if Assigned(TablePartRec) then
              TablePart := TablePartRec.TablePart
            else TablePart := Normal;
            case TablePart of
              Normal:
                begin
                PrintList.Draw(Canvas, ARect, W, MLeftSide, 0, 0,0);
                WhiteoutArea(Canvas, PrintList.PageBottom-PrintList.YOff);
                end;
              DoHead:
                begin
                PrintList.SetYOffset(TablePartRec.PartStart-TopPixels);
                CRect := ARect;
                CRect.Bottom := CRect.Top+TablePartRec.PartHeight;
                hrgnClip1 := CreateRectRgn(MLeftPrn, TopPixelsPrn, WPrn+MLeftPrn+2,
                       MulDiv(CRect.Bottom, P3, P2));
              SelectClipRgn(DC, hrgnClip1);
              DeleteObject(hrgnClip1);
                PrintList.Draw(Canvas, CRect, W, MLeftSide, 0, 0,0);
              end;
              DoBody1, DoBody3:
                begin
                CRect := ARect;
                CRect.Top := PrintList.PageBottom-1-PrintList.YOff;
                PrintList.SetYOffset(TablePartRec.PartStart-CRect.top);
                PrintList.Draw(Canvas, CRect, W, MLeftSide+3*W, 0, 0,0);      {off page}

                TablePartRec.TablePart := DoBody2;
                CRect.Bottom := PrintList.PageBottom-PrintList.YOff+1;
                hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(CRect.Top, P3, P2),
                       WPrn+MLeftPrn+2, MulDiv(CRect.Bottom, P3, P2));
                SelectClipRgn(DC, hrgnClip1);
                DeleteObject(hrgnClip1);
                PrintList.Draw(Canvas, CRect, W, MLeftSide, 0, 0,0);      {onpage}
                if not Assigned(TablePartRec)
                       or not (TablePartRec.TablePart in [Normal]) then
                WhiteoutArea(Canvas, PrintList.PageBottom-PrintList.YOff);
                end;
              DoFoot:
                begin
                SavePageBottom := PrintList.PageBottom;
                CRect := ARect;
                CRect.Top := PrintList.PageBottom-PrintList.YOff;
                CRect.Bottom := CRect.Top + TablePartRec.PartHeight;
                hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(CRect.Top, P3, P2),
                       WPrn+MLeftPrn+2, MulDiv(CRect.Bottom, P3, P2));
                SelectClipRgn(DC, hrgnClip1);
                DeleteObject(hrgnClip1);
                PrintList.SetYOffset(TablePartRec.PartStart-CRect.top);
                PrintList.Draw(Canvas, CRect, W, MLeftSide, 0, 0,0);
                PrintList.PageBottom := SavePageBottom;
                end;
              end;
            until not Assigned(TablePartRec)
                  or (TablePartRec.TablePart in [Normal, DoHead, DoBody3]);

{.$Region 'Do HeaderFooter'}
            SelectClipRgn(DC, 0);
          if (FPage <= ToPage) then  {print header and footer}
            begin
            Canvas.Pen.Assign(savedPen);
            Align := SetTextAlign(DC, TA_Top or TA_Left or TA_NOUPDATECP);
            if Assigned(FOnPrintHeader) then
              begin
              SetWindowOrgEx(DC, 0, 0, Nil);
              FOnPrintHeader(Self, Canvas, FPage, ScaledPgWid, TopPixels, Done);
              end;
            if Assigned(FOnPrintFooter) then
              begin
              SetWindowOrgEx(DC, 0, -(TopPixels+H), Nil);
              FOnPrintFooter(Self, Canvas, FPage, ScaledPgWid,
                  ScaledPgHt-(TopPixels+H), Done);
              end;
            DoHTMLHeaderFooter(False, FOnPrintHTMLHeader, HeadViewer);
            DoHTMLHeaderFooter(True, FOnPrintHTMLFooter, FootViewer);
            SetTextAlign(DC, Align);
            RestoreCanvasItems(Canvas);
            end;
          if FPrintMarginTop <> LastPrintMarginTop then  
            begin  
            DeltaMarginTop := FPrintMarginTop - SavePrintMarginTop;
            DeltaPixelsPrn:= Trunc(DeltaMarginTop/2.54 * P3);
            DeltaPixels := Trunc(DeltaMarginTop/2.54 * P2);
            TopPixels := OrigTopPixels + DeltaPixels;
            TopPixelsPrn := OrigTopPixelsPrn +DeltaPixelsPrn;
            HPrn := OrigHprn - DeltaPixelsPrn;
            H := OrigH - DeltaPixels;
            ARect := Rect(MLeft, TopPixels, W + MLeft, TopPixels + H);
            hrgnClip := CreateRectRgn(MLeftPrn, TopPixelsPrn, WPrn + MLeftPrn+2,
                      TopPixelsPrn + HPrn);
            LastPrintMarginTop := FPrintMarginTop;
            end;
{.$EndRegion}
          HTop := PrintList.PageBottom;
          Application.ProcessMessages;
          if Assigned(FOnPageEvent) then
            FOnPageEvent(Self, FPage, Done);
          if (HTop >= VPixels-MarginHeight) or (HTop <= OldTop) then  {see if done or endless loop}
            Done := True;
          OldTop := HTop;  
          if not Done and (FPage >= FromPage) and (FPage < ToPage) then
            NewPage;
          Inc(FPage);
          end;
        end;
    finally
      FreeAndNil(HeadViewer);
      FreeAndNil(FootViewer);
      if hRgnClip <> 0 then DeleteObject(hrgnClip);
      if not PrinterOpen then   
        begin
        if (FromPage > FPage) then
          vwPrinter.Abort
        else
          vwPrinter.EndDoc;
        vwSetPrinter(OldPrinter);
        FreeAndNil(vwP);
        end;
      Dec(FPage);
      end;
  finally
    savedFont.Free ;
    savedPen.Free ;
    savedBrush.Free ;
    end;
finally
  FPrintMarginTop := SavePrintMarginTop;  
  PrintList.Free;
  end;
end;

procedure ThtmlViewer.OpenPrint;   
begin
if not assigned(vwP) then
  begin
  vwP := TvwPrinter.Create;
  OldPrinter := vwSetPrinter(vwP);
  end;
end;

procedure ThtmlViewer.ClosePrint;   
begin
if Assigned(vwP) then
  begin
  if vwP.Printing then
    vwPrinter.EndDoc;
  vwSetPrinter(OldPrinter);
  FreeAndNil(vwP);
  end;
end;

procedure ThtmlViewer.AbortPrint;   
begin
if Assigned(vwP) then
  begin
  if vwP.Printing then
    vwPrinter.Abort;
  vwSetPrinter(OldPrinter);
  FreeAndNil(vwP);
  end;
end;

function ThtmlViewer.NumPrinterPages: integer;
var
  Dummy: double;
begin
Result := NumPrinterPages(Dummy);
end;

function ThtmlViewer.NumPrinterPages(var WidthRatio: double): integer;
var
  MFPrinter: TMetaFilePrinter;
begin
MFPrinter := TMetaFilePrinter.Create(Nil);
FOnPageEvent := Nil;
try
  PrintPreview(MFPrinter, True);
  Result := MFPrinter.LastAvailablePage;
  WidthRatio := FWidthRatio;
finally
  MFPrinter.Free;
  end;
end;

function ThtmlViewer.PrintPreview(MFPrinter: TMetaFilePrinter; NoOutput: boolean = False): integer;
var
  ARect, CRect  : TRect;        
  PrintList     : TSectionList;
  P1, P2, P3    : integer;
  W, H          : integer;
  HTop          : integer;
  OldTop        : integer;
  ScrollWidth   : integer;
  Curs          : integer;
  Done          : boolean;
  DC            : HDC;
  PrnDC         : HDC; {metafile printer's DC}

  UpperLeftPagePoint, { these will contain Top/Left and Bottom/Right unprintable area}
  LowerRightPagePoint: TPoint;

  MLeft         : integer;
  MLeftPrn      : integer;
  MRightPrn     : integer;
  MTopPrn       : integer;
  MBottomPrn    : integer;
  TopPixels     : integer;
  TopPixelsPrn  : integer;
  HPrn, WPrn    : integer;
  hrgnClip      : THandle;
  hrgnClip1      : THandle;
  hrgnClip2     : THandle;
  SavedFont     : TFont;
  SavedPen      : TPen;
  SavedBrush    : TBrush;
  Align         : integer;
  ScaledPgHt    : integer;
  ScaledPgWid   : integer;
  VPixels       : integer;
  FootViewer, HeadViewer: ThtmlViewer;

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
  TablePart: TablePartType;

  procedure Fill(Canvas: TCanvas);
  var
    BrushColor: TColor;
  begin
  BrushColor := Canvas.Brush.Color;
  Canvas.Brush.Color := $eeeeff;
  Canvas.Rectangle(MLeft, 0, W + MLeft+200, 4000);
  Canvas.Brush.Color := BrushColor;
  end;

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

  procedure WhiteoutArea(Canvas: TCanvas; Y: integer);
  {White out excess printing.  Y is top of the bottom area to be blanked.}
  begin
  Canvas.Brush.Color := clWhite;   
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Color   := clWhite;
  Canvas.Rectangle(MLeft, 0, W + MLeft+1, TopPixels-1);
  Canvas.Rectangle(MLeft, Y, W + MLeft+1, TopPixels+H+1);
  if (htPrintBackground in FOptions) and (Y-TopPixels < H) then
    begin   {need to reprint background in whited out area}
    hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(Y, P3, P2)+2,
                     MLeftPrn+WPrn, TopPixelsPrn+HPrn);
    SelectClipRgn(Canvas.Handle, hrgnClip1);
    DeleteObject(hrgnClip1); 
    DoBackground2(Canvas, MLeft, TopPixels, W, H, PaintPanel.Color);
    end;
  RestoreCanvasItems(Canvas);
  end;

  procedure DoHTMLHeaderFooter(Footer: boolean; Event: ThtmlPagePrinted; HFViewer: ThtmlViewer);
  var
    YOrigin, YOff, Ht: integer;
    HFCopyList: TSectionList;
    BRect: TRect;
    DocHeight, XL, XR: integer;
  begin
  if not Assigned(Event) then  
    Exit;
  try
    XL := MLeft;
    XR := MLeft + W;
    Event(Self, HFViewer, FPage, PrintList.PageBottom > VPixels, XL, XR, Done);  {call event handler}
    HFCopyList := TSectionList.CreateCopy(HFViewer.SectionList);
    try
      HFCopyList.Printing := True;
      HFCopyList.NoOutput := NoOutput;   
      HFCopyList.ScaleX := fScaleX;     
      HFCopyList.ScaleY := fScaleY;
      Curs := 0;
      DocHeight := HFCopyList.DoLogic(MFPrinter.Canvas, 0, XR-XL, 300, 0, ScrollWidth, Curs);
      if not Footer then
        begin  {Header}
        YOrigin := 0;
        Ht := TopPixels;
        YOff := DocHeight-TopPixels;
        end
      else
        begin   {Footer}
        YOrigin := -(TopPixels+H);
        Ht := IntMin(ScaledPgHt-(TopPixels+H), DocHeight);
        YOff := 0;
        end;
      SetWindowOrgEx(DC, 0, YOrigin, nil);
      HFViewer.DoBackground2(MFPrinter.Canvas, XL, -YOff, XR-XL, DocHeight, HFViewer.PaintPanel.Color);
      HFCopyList.SetYOffset(YOff);
      BRect := Rect(XL, 0, XR, Ht);
      HFCopyList.Draw(MFPrinter.Canvas, BRect, XR-XL, XL, 0, 0, 0);
    finally
      HFCopyList.Free;
      end;
  except
    end;
  end;

begin
Done := False;
FootViewer := Nil;
HeadViewer := Nil;
TablePartRec := Nil;   
if Assigned(FOnPageEvent) then
  FOnPageEvent(Self, 0, Done);
FPage := 0;
Result := 0;
if FProcessing or (SectionList.Count = 0) then Exit;
PrintList := TSectionList.CreateCopy(SectionList);
PrintList.SetYOffset(0);
SavePrintMarginTop := FPrintMarginTop;  
try
  SavedPen := TPen.Create;
  SavedFont := TFont.Create;
  SavedBrush := TBrush.Create;
  try
    PrintList.Printing := True;
    PrintList.NoOutput := NoOutput;
    PrintList.SetBackground(clWhite);

    FPage := 1;
    hrgnClip := 0;
    hrgnClip2 := 0;
    try
      with MFPrinter do
        begin
        if DocumentTitle <> '' then
           Title := DocumentTitle;

        BeginDoc;
        DC := Canvas.Handle;
        PrnDC := PrinterDC;

        P3 := GetDeviceCaps(PrnDC, LOGPIXELSY);
        P1 := GetDeviceCaps(PrnDC, LOGPIXELSX);
        P2 := Round(Screen.PixelsPerInch * FPrintScale);
        fScaleX := 100.0/P3;
        fScaleY := 100.0/P1;
        PrintList.ScaleX := fScaleX;
        PrintList.ScaleY := fScaleY;
        SetMapMode(DC, mm_AnIsotropic);
        //P1 := GetDeviceCaps(PrnDC, LOGPIXELSX);
        SetWindowExtEx(DC, P2, P2, nil);
        SetViewPortExtEx(DC, P1, P3, nil);

        { calculate the amount of space that is non-printable }

        { get PHYSICAL page width }
        LowerRightPagePoint.X := GetDeviceCaps(PrnDC, PhysicalWidth);
        LowerRightPagePoint.Y := GetDeviceCaps(PrnDC, PhysicalHeight);

        { now compute a complete unprintable area rectangle
         (composed of 2*width, 2*height) in pixels...}
        with LowerRightPagePoint do
          begin
          Y := Y - MFPrinter.PageHeight;
          X := X - MFPrinter.PageWidth;
          end;

        { get upper left physical offset for the printer... ->
          printable area <> paper size }
        UpperLeftPagePoint.X := GetDeviceCaps(PrnDC, PhysicalOffsetX);
        UpperLeftPagePoint.Y := GetDeviceCaps(PrnDC, PhysicalOffsetY);

        { now that we know the TOP and LEFT offset we finally can
          compute the BOTTOM and RIGHT offset: }
        with LowerRightPagePoint do
          begin
          X := X - UpperLeftPagePoint.X;
          { we don't want to have negative values}
          if X < 0 then
            X := 0; { assume no right printing offset }

          Y := Y - UpperLeftPagePoint.Y;
          { we don't want to have negative values}
          if Y < 0 then
            Y := 0; { assume no bottom printing offset }
          end;

        { which results in LowerRightPoint containing the BOTTOM
          and RIGHT unprintable area offset; using these we modify
          the (logical, true) borders...}

        MLeftPrn := Trunc(FPrintMarginLeft/2.54 * P1);
        MLeftPrn := MLeftPrn - UpperLeftPagePoint.X;  { subtract physical offset }
        MLeft := MulDiv(MLeftPrn, P2, P1);

        MRightPrn := Trunc(FPrintMarginRight/2.54 * P1);
        MRightPrn := MRightPrn - LowerRightPagePoint.X;  { subtract physical offset }

        WPrn := PageWidth - (MLeftPrn + MRightPrn);

        W := MulDiv(WPrn, P2, P1);

        MTopPrn := Trunc(FPrintMarginTop/2.54 * P3);
        MTopPrn := MTopPrn - UpperLeftPagePoint.Y;  { subtract physical offset }

        MBottomPrn := Trunc(FPrintMarginBottom/2.54 * P3);
        MBottomPrn := MBottomPrn - LowerRightPagePoint.Y;  { subtract physical offset }

        TopPixelsPrn := MTopPrn;
        TopPixels    := MulDiv(TopPixelsPrn, P2, P3);

        HPrn := PageHeight-(MTopPrn+MBottomPrn);
        H    := MulDiv(HPrn, P2, P3);  {scaled pageHeight}
        HTop := 0;
        OldTop := 0;   

        Curs := 0;
        VPixels := PrintList.DoLogic(Canvas, 0, W, H, 0, ScrollWidth, Curs);
        FWidthRatio := ScrollWidth/W;   
        if FWidthRatio > 1.0 then 
          FWidthRatio := Round(P2*FWidthRatio +0.5)/P2;

        ScaledPgHt  := MulDiv(PageHeight, P2, P3);
        ScaledPgWid := MulDiv(PageWidth,  P2, P3);

        {This one clips to the allowable print region so that the preview is
         limited to that region also}
        hrgnClip2 := CreateRectRgn(0, 0, MFPrinter.PageWidth, MFPrinter.PageHeight);
        {This one is primarily used to clip the top and bottom margins to insure
         nothing is output there.  It's also constrained to the print region
         in case the margins are misadjusted.}
        hrgnClip := CreateRectRgn(MLeftPrn, IntMax(0, TopPixelsPrn),
               IntMin(MFPrinter.PageWidth, WPrn+MLeftPrn+2),
               IntMin(MFPrinter.PageHeight, TopPixelsPrn+HPrn));
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
        LastPrintMarginTop := FPrintMarginTop;

        while not Done do
          begin
          PrintList.SetYOffset(HTop-TopPixels);

          {next line is necessary because the canvas changes with each new page }
          DC := Canvas.Handle;
          SaveCanvasItems(Canvas);

          SetMapMode(DC, mm_AnIsotropic);
          SetWindowExtEx(DC, P2, P2, nil);
          SetViewPortExtEx(DC, P1, P3, nil);
          SetWindowOrgEx(DC, 0, 0, nil);
          SelectClipRgn(DC, hrgnClip);

          if (htPrintBackground in FOptions) then  
            DoBackground2(Canvas, MLeft, TopPixels, W, H, PaintPanel.Color);

          repeat
            if Assigned(TablePartRec) then
              TablePart := TablePartRec.TablePart
            else TablePart := Normal;
            case TablePart of
              Normal:
                begin
                PrintList.Draw(Canvas, ARect, W, MLeft, 0, 0,0);
                WhiteoutArea(Canvas, PrintList.PageBottom-PrintList.YOff);
                end;
              DoHead:
                begin
                PrintList.SetYOffset(TablePartRec.PartStart-TopPixels);
                CRect := ARect;
                CRect.Bottom := CRect.Top+TablePartRec.PartHeight;
                hrgnClip1 := CreateRectRgn(MLeftPrn, TopPixelsPrn, WPrn+MLeftPrn+2,
                       MulDiv(CRect.Bottom, P3, P2));
                SelectClipRgn(DC, hrgnClip1);
                DeleteObject(hrgnClip1); 
                PrintList.Draw(Canvas, CRect, W, MLeft, 0, 0,0);
                end;
              DoBody1, DoBody3:                               
                begin
                CRect := ARect;
                CRect.Top := PrintList.PageBottom-1-PrintList.YOff;
                PrintList.SetYOffset(TablePartRec.PartStart-CRect.top);
                PrintList.Draw(Canvas, CRect, W, MLeft+3*W, 0, 0,0);      {off page}

                TablePartRec.TablePart := DoBody2;
                CRect.Bottom := PrintList.PageBottom-PrintList.YOff+1;
                hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(CRect.Top, P3, P2),
                       WPrn+MLeftPrn+2, MulDiv(CRect.Bottom, P3, P2));
                SelectClipRgn(DC, hrgnClip1);
                DeleteObject(hrgnClip1);  
                PrintList.Draw(Canvas, CRect, W, MLeft, 0, 0,0);      {onpage}
                if not Assigned(TablePartRec)
                       or not (TablePartRec.TablePart in [Normal]) then
                  WhiteoutArea(Canvas, PrintList.PageBottom-PrintList.YOff);
                end;
              DoFoot:
                begin
                SavePageBottom := PrintList.PageBottom;
                CRect := ARect;
                CRect.Top := PrintList.PageBottom-PrintList.YOff;
                CRect.Bottom := CRect.Top + TablePartRec.PartHeight;   
                hrgnClip1 := CreateRectRgn(MLeftPrn, MulDiv(CRect.Top, P3, P2),
                       WPrn+MLeftPrn+2, MulDiv(CRect.Bottom, P3, P2));
                SelectClipRgn(DC, hrgnClip1);
                DeleteObject(hrgnClip1);
                PrintList.SetYOffset(TablePartRec.PartStart-CRect.top);
                PrintList.Draw(Canvas, CRect, W, MLeft, 0, 0,0);
                PrintList.PageBottom := SavePageBottom;
                end;
              end;
            until not Assigned(TablePartRec)
                  or (TablePartRec.TablePart in [Normal, DoHead, DoBody3]);
{.$Region 'HeaderFooter'}
          SelectClipRgn(DC, 0);
          Align := SetTextAlign(DC, TA_Top or TA_Left or TA_NOUPDATECP);
          SelectClipRgn(DC, hrgnClip2);

          if Assigned(FOnPrintHeader) then
            begin
            SetWindowOrgEx(DC, 0, 0, Nil);
            FOnPrintHeader(Self, Canvas, FPage, ScaledPgWid, TopPixels, Done);
            end;

          if Assigned(FOnPrintFooter) then
            begin
            SetWindowOrgEx(DC, 0, -(TopPixels+H), nil);
            FOnPrintFooter(Self, Canvas, FPage, ScaledPgWid,
                           ScaledPgHt-(TopPixels+H), Done);
            end;
          DoHTMLHeaderFooter(False, FOnPrintHTMLHeader, HeadViewer);  
          DoHTMLHeaderFooter(True, FOnPrintHTMLFooter, FootViewer);
          SetTextAlign(DC, Align);
          SelectClipRgn(DC, 0);

          if FPrintMarginTop <> LastPrintMarginTop then  
            begin
            DeltaMarginTop := FPrintMarginTop - SavePrintMarginTop;
            DeltaPixelsPrn:= Trunc(DeltaMarginTop/2.54 * P3);
            DeltaPixels := Trunc(DeltaMarginTop/2.54 * P2);
            TopPixels := OrigTopPixels + DeltaPixels;
            TopPixelsPrn := OrigTopPixelsPrn +DeltaPixelsPrn;
            HPrn := OrigHprn - DeltaPixelsPrn;
            H := OrigH - DeltaPixels;
            ARect := Rect(MLeft, TopPixels, W + MLeft, TopPixels + H);
            hrgnClip := CreateRectRgn(MLeftPrn, IntMax(0, TopPixelsPrn),
                   IntMin(MFPrinter.PageWidth, WPrn+MLeftPrn+2),
                   IntMin(MFPrinter.PageHeight, TopPixelsPrn+HPrn));
            LastPrintMarginTop := FPrintMarginTop;
            end;
{.$EndRegion}
          HTop := PrintList.PageBottom;
          Application.ProcessMessages;
          if Assigned(FOnPageEvent) then
              FOnPageEvent(Self, FPage, Done);
          if (HTop >= VPixels-MarginHeight) or (HTop <= OldTop) then  {see if done or endless loop}
            Done := True;
          OldTop := HTop;  
          if not Done then
            NewPage;
          Inc(FPage);
          end;
        EndDoc;
        end;
    finally
      FreeAndNil(HeadViewer);   
      FreeAndNil(FootViewer);   
      if hRgnClip <> 0 then
        DeleteObject(hrgnClip);
      if hRgnClip2 <> 0 then
        DeleteObject(hrgnClip2);
      Dec(FPage);
    end;
  finally
    SavedPen.Free;
    SavedFont.Free;
    SavedBrush.Free;
    end;
finally
  FPrintMarginTop := SavePrintMarginTop;  
  PrintList.Free;
  Result := FPage;
  end;
end;

procedure ThtmlViewer.BackgroundChange(Sender: TObject);
begin
PaintPanel.Color := (Sender as TSectionList).Background or PalRelative;  
end;

procedure ThtmlViewer.SetOnBitmapRequest(Handler: TGetBitmapEvent);
begin
FOnBitmapRequest := Handler;
FSectionList.GetBitmap := Handler;
end;

procedure ThtmlViewer.SetOnImageRequest(Handler: TGetImageEvent);
begin
FOnImageRequest := Handler;
FSectionList.GetImage := Handler;
end;

procedure ThtmlViewer.SetOnExpandName(Handler: TExpandNameEvent);  
begin
FOnExpandName := Handler;
FSectionList.ExpandName := Handler;
end;

procedure ThtmlViewer.SetOnScript(Handler: TScriptEvent);
begin
FOnScript := Handler;
FSectionList.ScriptEvent := Handler;
end;

procedure ThtmlViewer.SetOnFileBrowse(Handler: TFileBrowseEvent);  
begin
FOnFileBrowse := Handler;
FSectionList.FileBrowse := Handler;
end;

procedure ThtmlViewer.SetOnObjectClick(Handler: TObjectClickEvent);
begin
FOnObjectClick := Handler;
FSectionList.ObjectClick := Handler;
end;

procedure ThtmlViewer.SetOnObjectFocus(Handler: ThtObjectEvent);
begin
FOnObjectFocus := Handler;
FSectionList.ObjectFocus := Handler;
end;

procedure ThtmlViewer.SetOnObjectBlur(Handler: ThtObjectEvent);
begin
FOnObjectBlur := Handler;
FSectionList.ObjectBlur := Handler;
end;

procedure ThtmlViewer.SetOnObjectChange(Handler: ThtObjectEvent);
begin
FOnObjectChange := Handler;
FSectionList.ObjectChange := Handler;
end;

procedure ThtmlViewer.SetOnPanelCreate(Handler: TPanelCreateEvent);
begin
FOnPanelCreate := Handler;
FSectionList.PanelCreateEvent := Handler;
end;

procedure ThtmlViewer.SetOnPanelDestroy(Handler: TPanelDestroyEvent);
begin
FOnPanelDestroy := Handler;
FSectionList.PanelDestroyEvent := Handler;
end;

procedure ThtmlViewer.SetOnPanelPrint(Handler: TPanelPrintEvent);  
begin
FOnPanelPrint := Handler;
FSectionList.PanelPrintEvent := Handler;
end;

procedure ThtmlViewer.SetOnFormSubmit(Handler: TFormSubmitEvent);
begin
FOnFormSubmit := Handler;
if Assigned(Handler) then
  FSectionList.SubmitForm := SubmitForm
else FSectionList.SubmitForm := Nil;
end;

procedure ThtmlViewer.SubmitForm(Sender: TObject; const Action, Target, EncType, Method: string;
                    Results: TStringList);
begin
if Assigned(FOnFormSubmit) then
  begin
  FAction := Action;
  FMethod := Method;
  FFormTarget := Target;
  FEncType:= EncType;
  FStringList := Results;
  PostMessage(Handle, wm_FormSubmit, 0, 0);
  end;
end;

procedure ThtmlViewer.WMFormSubmit(var Message: TMessage);
begin
FOnFormSubmit(Self, FAction, FFormTarget, FEncType, FMethod, FStringList);
end;     {user disposes of the TStringList}

function ThtmlViewer.Find(const S: WideString; MatchCase: boolean): boolean;
begin
Result := FindEx(S, MatchCase, False);
end;

function ThtmlViewer.FindEx(const S: WideString; MatchCase, Reverse: boolean): boolean;  
var
  Curs: integer;
  X: integer;
  Y, Pos: integer;
  S1: WideString;
begin
Result := False;
if S = '' then Exit;
with FSectionList do
  begin
  if MatchCase then
    S1 := S
  else S1 := WideLowerCase1(S);
  if Reverse then
    Curs := FindStringR(CaretPos, S1, MatchCase)
  else
    Curs := FindString(CaretPos, S1, MatchCase);
  if Curs >= 0 then
    begin
    Result := True;
    SelB := Curs;
    SelE := Curs+Length(S);
    if Reverse then
      CaretPos := SelB
    else
      CaretPos := SelE;
    if CursorToXY(PaintPanel.Canvas, Curs, X, Y) then
      begin
      Pos := VScrollBarPosition;
      if (Y < Pos) or
             (Y > Pos +ClientHeight-20) then
        VScrollBarPosition := (Y - ClientHeight div 2);

      Pos := HScrollBarPosition;
      if (X < Pos) or
             (X > Pos +ClientWidth-50) then
        HScrollBarPosition := (X - ClientWidth div 2);
      Invalidate;
      end;
    end;
  end;
end;

procedure ThtmlViewer.FormControlEnterEvent(Sender: TObject);
var
  Y, Pos: integer;
begin
if Sender is TFormControlObj then
  begin
  Y := TFormControlObj(Sender).YValue;
  Pos := VScrollBarPosition;
  if (Y < Pos) or (Y > Pos +ClientHeight-20) then
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
  else if (Y > Pos +ClientHeight-30) then
    VScrollBarPosition := (Y - ClientHeight div 2);
  Invalidate;
  end
end;

procedure ThtmlViewer.SelectAll;
begin
with FSectionList do
  if (Count > 0) and not FNoSelect then
    begin
    SelB := 0;
    with TSectionBase(Items[Count-1]) do
      SelE := StartCurs + Len;
    Invalidate;
    end;
end;

{----------------ThtmlViewer.InitLoad}
procedure ThtmlViewer.InitLoad;   
begin
if not Assigned(FSectionList.BitmapList) then
  begin
  FSectionList.BitmapList := TStringBitmapList.Create;
  FSectionList.BitmapList.Sorted := True;
  FSectionList.BitmapList.SetCacheCount(FImageCacheCount);
  LocalBitmapList := True;
  end;
FSectionList.Clear;
UpdateImageCache;
FSectionList.SetFonts(FFontName, FPreFontName, FFontSize, FFontColor,
             FHotSpotColor, FVisitedColor, FOverColor, FBackground,
             htOverLinksActive in FOptions, not (htNoLinkUnderline in FOptions),
             FCharSet, FMarginHeight, FMarginWidth);
end;

{----------------ThtmlViewer.Clear}
procedure ThtmlViewer.Clear;
{Note: because of Frames do not clear history list here}
begin
if FProcessing then Exit;
HTMLTimer.Enabled := False;
FSectionList.Clear;
if LocalBitmapList then
  FSectionList.BitmapList.Clear;
FSectionList.SetFonts(FFontName, FPreFontName, FFontSize, FFontColor,
           FHotSpotColor, FVisitedColor, FOverColor, FBackground,
           htOverLinksActive in FOptions, not (htNoLinkUnderline in FOptions),
           FCharSet, FMarginHeight, FMarginWidth);
FBase := '';
FBaseEx := '';
FBaseTarget := '';
FTitle := '';
VScrollBar.Max := 0;
VScrollBar.Visible := False;
VScrollBar.Height := PaintPanel.Height;
HScrollBar.Visible := False;
CaretPos := 0;
Sel1 := -1;
if Assigned(FOnSoundRequest) then
  FOnSoundRequest(Self, '', 0, True);
Invalidate;
end;

procedure ThtmlViewer.PaintWindow(DC: HDC);
begin
PaintPanel.RePaint;
BorderPanel.RePaint;   
VScrollbar.RePaint;   
HScrollbar.RePaint;  
end;

procedure ThtmlViewer.CopyToClipboard;
const
  StartFrag = '<!--StartFragment-->';
  EndFrag = '<!--EndFragment-->';
  DocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">'#13#10;
var
  Leng: integer;
  StSrc, EnSrc: integer;
  HTML: string;
  format : UINT;

  procedure copyFormatToClipBoard(const source: string; format : UINT);
  // Put SOURCE on the clipboard, using FORMAT as the clipboard format
  // Based on http://www.lorriman.com/programming/cf_html.html
  var
    gMem: HGLOBAL;
    lp: pchar;
  begin
    clipboard.Open;
    try
      //an extra "1" for the null terminator
      gMem := globalalloc(GMEM_DDESHARE + GMEM_MOVEABLE, length(source)+1);
      lp := globallock(gMem);
      copymemory(lp, pchar(source), length(source)+1);
      globalunlock(gMem);
      setClipboarddata(format, gMem);
    finally
      clipboard.Close;
    end
  end;

  function GetHeader(const HTML: string): string;
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
                        2;  {2 for last CRLF}
  var
    URLString: string;
    StartHTMLIndex,
    EndHTMLIndex,
    StartFragmentIndex,
    EndFragmentIndex: integer;
  begin
  if CurrentFile = '' then
    UrlString := SourceURL+'unsaved:///ThtmlViewer.htm'
  else if Pos('://', CurrentFile) > 0 then
    URLString := SourceURL+CurrentFile    {already has protocol}
  else
    URLString := SourceURL+'file://'+CurrentFile;
  StartHTMLIndex := PreliminaryLength + Length(URLString);
  EndHTMLIndex := StartHTMLIndex + Length(HTML);
  StartFragmentIndex := StartHTMLIndex + Pos(StartFrag, HTML) + Length(StartFrag)-1;
  EndFragmentIndex := StartHTMLIndex + Pos(EndFrag, HTML)-1;

  Result := Version +
    SysUtils.Format('%s%.8d', [StartHTML, StartHTMLIndex]) + #13#10 +
    SysUtils.Format('%s%.8d', [EndHTML, EndHTMLIndex]) + #13#10 +
    SysUtils.Format('%s%.8d', [StartFragment, StartFragmentIndex]) + #13#10 +
    SysUtils.Format('%s%.8d', [EndFragment, EndFragmentIndex]) + #13#10 +
    URLString + #13#10;
  end;

  function Truncate(const S: string): string;
  var
    I: integer;
  begin
  I := Pos(EndFrag, S);
  Result := S;
  if I > 0 then
    Result := Copy(Result, 1, I+Length(EndFrag)-1);
  end;

  procedure RemoveTag(const Tag: string);
  {remove all the tags that look like "<tag .....>" }
  var
    I: integer;
    L: string;
    C: char;
  begin
  L := Lowercase(HTML);
  I := Pos(Tag, L);
  while (I > 0) do
    begin
    Delete(HTML, I, Length(Tag));
    repeat
      if I  <= Length(HTML) then
        C := HTML[I]
      else C := #0;
      Delete(HTML, I, 1);
    until C in ['>', #0];
    L := Lowercase(HTML);
    I := Pos(Tag, L);
    end;
  end;

  procedure MessUp(const S: string);
  var
    I: integer;
    L: string;
  begin
  L := Lowercase(HTML);
  I := Pos(S, L);
  while (I > 0) do
    begin
    Delete(HTML, I, 1);
    L := Lowercase(HTML);
    I := Pos(S, L);
    end;
  end;

  function ConvertToUTF8(const S: string): string;
  var
    Len, Len1: integer;
    WS: WideString;
  begin
  if CodePage = CP_UTF8 then   
    begin
    Result := S;
    Exit;
    End;
  Len := Length(S);
  SetLength(WS, Len);
  Len := MultibyteToWideChar(CodePage, 0, PChar(S), Len, PWideChar(WS), Len);
  Len1 := 4*Len;
  SetLength(Result, Len1);
  Len1 := WideCharToMultibyte(CP_UTF8, 0, PWideChar(WS), Len, PChar(Result), Len1, Nil, Nil);
  SetLength(Result, Len1);
  end;

  procedure InsertDefaultFontInfo;
  var
    I: integer;
    S, L: string;
    HeadFound: boolean;
  begin
  L := LowerCase(HTML);
  I := Pos('<head>', L);
  HeadFound := I > 0;
  if not HeadFound then
    I := Pos('<html>', L);
  if I <= 0 then
    I := 1;
  S := '<style> body {font-size: '+IntToStr(DefFontSize)+'pt; font-family: "'+
       DefFontName+'"; }</style>';
  if not HeadFound then
    S := '<head>'+S+'</head>';
  Insert(S, HTML, I);
  end;

  procedure BackupToContent;
  var
    C: char;
    I: integer;

    procedure GetC;  {reads characters backwards}
    begin
    if I-1 > StSrc then
      begin
      Dec(I);
      C := HTML[I];
      end
    else C  := #0;
    end;

  begin
  I := EnSrc;
  repeat
    repeat     {skip past white space}
      GetC;
    until C in [#0, '!'..#255];
    if C = '>' then
      repeat   {read thru a tag}
        repeat
          GetC;
        until C in [#0, '<'];
        GetC;
      until C <> '>';
  until C in [#0, '!'..#255]; {until found some content}
  if C = #0 then Dec(I);
  HTML := Copy(HTML, 1, I);  {truncate the tags}
  end;

begin
Leng := FSectionList.GetSelLength;
if Leng = 0 then
  Exit;
FSectionList.CopyToClipboardA(Leng+1);

HTML := DocumentSource;
StSrc := FindSourcePos(FSectionList.SelB)+1;
EnSrc := FindSourcePos(FSectionList.SelE);
if EnSrc < 0 then    {check to see if end selection is at end of document}
  begin
  EnSrc := Length(HTML);
  if HTML[EnSrc] = '>' then
    begin
    HTML := HTML + ' ';
    Inc(EnSrc);
    end;
  end
else EnSrc := EnSrc + 1;
{Truncate beyond EnSrc}
HTML := Copy(HTML, 1, EnSrc-1);
{Also remove any tags on the end}
BackupToContent;
{insert the StartFrag string}
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
{Convert character set to UTF-8}
HTML := ConvertToUTF8(HTML);
{Add Doctype tag at start}
HTML := DocType+HTML;
{Append the EndFrag string}
HTML := HTML+EndFrag;
{Add the header to start}
HTML := GetHeader(HTML)+HTML;

format := RegisterClipboardFormat('HTML Format'); {not sure this is necessary}
CopyFormatToClipBoard(HTML, format);
end;

function ThtmlViewer.GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;
begin
if BufSize <= 0 then Result := 0
else Result := FSectionList.GetSelTextBuf(Buffer, BufSize);
end;

function ThtmlViewer.GetSelText: WideString;   
var
  Len: integer;
begin
Len := FSectionList.GetSelLength;
if Len > 0 then
  begin
  SetString(Result, Nil, Len);
  FSectionList.GetSelTextBuf(Pointer(Result), Len+1);
  end
else Result := '';
end;

function ThtmlViewer.GetSelLength: integer;   
begin
with FSectionList do
  if FCaretPos = SelB then
    Result := SelE - SelB
  else
    Result := SelB - SelE;
end;

procedure ThtmlViewer.SetSelLength(Value: integer); 
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

procedure ThtmlViewer.SetSelStart(Value: integer);  
begin
with FSectionList do
  begin
  FCaretPos := Value;     
  SelB := Value;
  SelE := Value;
  Invalidate;
  end;
end;

procedure ThtmlViewer.SetNoSelect(Value: boolean);
begin
if Value <> FNoSelect then
  begin
  FNoSelect := Value;
  if Value = True then
    begin
    FSectionList.SelB := -1;
    FSectionList.SelE := -1;
    RePaint;
    end;
  end;
end;

procedure ThtmlViewer.UpdateImageCache;
begin
FSectionList.BitmapList.BumpAndCheck;    
end;

procedure ThtmlViewer.SetImageCacheCount(Value: integer);
begin
Value := IntMax(0, Value);
Value := IntMin(20, Value);
if Value <> FImageCacheCount then
  begin
  FImageCacheCount := Value;
  if Assigned(FSectionList.BitmapList) then   
    FSectionList.BitmapList.SetCacheCount(FImageCacheCount);
  end;
end;

procedure ThtmlViewer.SetStringBitmapList(BitmapList: TStringBitmapList);
begin
FSectionList.BitmapList := BitmapList;
LocalBitmapList := False;
end;

procedure ThtmlViewer.DrawBorder;
begin
if (Focused and (FBorderStyle = htFocused)) or (FBorderStyle = htSingle)
       or (csDesigning in ComponentState) then
  BorderPanel.BorderStyle := bsSingle
else
  BorderPanel.BorderStyle := bsNone;
end;

procedure ThtmlViewer.DoEnter;
begin
inherited DoEnter;
DrawBorder;
end;

procedure ThtmlViewer.DoExit;
begin
inherited DoExit;
DrawBorder;
end;

procedure ThtmlViewer.SetScrollBars(Value: TScrollStyle);
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

{----------------ThtmlViewer.Reload}
procedure ThtmlViewer.Reload;     {reload the last file}
var
  Pos: integer;
begin
if FCurrentFile <> '' then
  begin
  Pos := Position;
  if FCurrentFileType = HTMLType then
    LoadFromFile(FCurrentFile)
  else if FCurrentFileType = TextType then
    LoadTextFile(FCurrentFile)
  else LoadImageFile(FCurrentFile);
  Position := Pos;
  end;
end;

{----------------ThtmlViewer.GetOurPalette:}
function ThtmlViewer.GetOurPalette: HPalette;
begin
if ColorBits = 8 then
  Result := CopyPalette(ThePalette)
else Result := 0;
end;

{----------------ThtmlViewer.SetOurPalette}
procedure ThtmlViewer.SetOurPalette(Value: HPalette);
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
    if FDither then SetGlobalPalette(ThePalette);
    end;
  end;
end;

{----------------ThtmlViewer.SetDither}
procedure ThtmlViewer.SetDither(Value: boolean);
begin
if (Value <> FDither) and (ColorBits = 8) then
  begin
  FDither := Value;
  if Value then SetGlobalPalette(ThePalette)
  else SetGLobalPalette(0);
  end;
end;

procedure ThtmlViewer.SetCaretPos(Value: integer);
begin
if Value >= 0 then
  begin
  FCaretPos := Value;
  end;
end;

function ThtmlViewer.FindSourcePos(DisplayPos: integer): integer;  
begin
Result := FSectionList.FindSourcePos(DisplayPos);
end;

function ThtmlViewer.FindDisplayPos(SourcePos: integer; Prev: boolean): integer;
begin
Result := FSectionList.FindDocPos(SourcePos, Prev);
end;

function ThtmlViewer.DisplayPosToXy(DisplayPos: integer; var X, Y: integer): boolean;
begin
Result := FSectionList.CursorToXY(PaintPanel.Canvas, DisplayPos, X, integer(Y));  {integer() req'd for delphi 2}
end;

{----------------ThtmlViewer.SetProcessing}
procedure ThtmlViewer.SetProcessing(Value: boolean);
begin
if FProcessing <> Value then
  begin
  FProcessing := Value;
  if Assigned(FOnProcessing) and not (csLoading in ComponentState) then
        FOnProcessing(Self, FProcessing);
  end;
end;

procedure THTMLViewer.SetServerRoot(Value: string);
begin
Value := Trim(Value);
if (Length(Value) >= 1) and (Value[Length(Value)] = '\') then
  SetLength(Value, Length(Value)-1);
FServerRoot := Value;
end;

procedure THTMLViewer.HandleMeta(Sender: TObject; const HttpEq, Name, Content: string);
var
  DelTime, I: integer;
begin
if Assigned(FOnMeta) then FOnMeta(Self, HttpEq, Name, Content);
if Assigned(FOnMetaRefresh) then
  if CompareText(Lowercase(HttpEq), 'refresh') = 0 then
    begin
    I := Pos(';', Content);
    if I > 0 then
      DelTime := StrToIntDef(copy(Content, 1, I-1), -1)
    else DelTime := StrToIntDef(Content, -1);        
    if DelTime < 0 then Exit
    else if DelTime = 0 then DelTime := 1;
    I := Pos('url=', Lowercase(Content));
    if I > 0 then   
      FRefreshURL := Copy(Content, I+4, Length(Content)-I-3)
    else FRefreshURL := '';
    FRefreshDelay := DelTime;   
    end;
end;

procedure THTMLViewer.SetOptions(Value: ThtmlViewerOptions); 
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

procedure ThtmlViewer.Repaint;
var
  I: integer;
begin
for I := 0 to FormControlList.count-1 do
  with TFormControlObj(FormControlList.Items[I]) do
    if Assigned(TheControl) then
      TheControl.Hide;  
BorderPanel.BorderStyle := bsNone;
inherited Repaint;
end;

function THTMLViewer.GetDragDrop: TDragDropEvent;
begin
Result := FOnDragDrop;
end;

procedure THTMLViewer.SetDragDrop(const Value: TDragDropEvent);
begin
FOnDragDrop := Value;
if Assigned(Value) then
  PaintPanel.OnDragDrop := HTMLDragDrop
else PaintPanel.OnDragDrop := Nil;
end;

procedure THTMLViewer.HTMLDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
if Assigned(FOnDragDrop) then
  FOnDragDrop(Self, Source, X, Y);
end;

function THTMLViewer.GetDragOver: TDragOverEvent;
begin
Result := FOnDragOver;
end;

procedure THTMLViewer.SetDragOver(const Value: TDragOverEvent);
begin
FOnDragOver := Value;
if Assigned(Value) then
  PaintPanel.OnDragOver := HTMLDragOver
else PaintPanel.OnDragOver := Nil;
end;

procedure THTMLViewer.HTMLDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
if Assigned(FOnDragOver) then
  FOnDragOver(Self, Source, X, Y, State, Accept);
end;

function THTMLViewer.GetFormData: TFreeList;     
begin
if Assigned(SectionList) then
  Result := SectionList.GetFormControlData
else Result := Nil;
end;

procedure THTMLViewer.SetFormData(T: TFreeList);
begin
if Assigned(SectionList) and Assigned(T) then
  with SectionList do
    begin
    ObjectClick := Nil;
    SetFormControlData(T);
    ObjectClick := FOnObjectClick;
    end;
end;

procedure THTMLViewer.ReplaceImage(const NameID: string; NewImage: TStream);
var
  I: integer;
  OldPos: integer;
begin
if FNameList.Find(NameID, I) then
  if FNameList.Objects[I] is TImageObj then
    begin
    TImageObj(FNameList.Objects[I]).ReplaceImage(NewImage);
    if not TImageObj(FNameList.Objects[I]).ImageKnown then
      if FSectionList.Count > 0 then
        begin
        FSectionList.GetBackgroundBitmap;    {load any background bitmap}
        OldPos := Position;
        DoLogic;
        Position := OldPos;
        end;
    end;
end;

function THTMLViewer.GetIDControl(const ID: string): TObject;
var
  I: integer;
  Obj: TObject;
begin
Result := Nil;
with FSectionList.IDNameList do
  if Find(ID, I) then
    begin
    Obj := Objects[I];
    if (Obj is TFormControlObj) then   
      begin
      if (Obj is THiddenFormControlObj) then
        Result := Obj
      else Result := TFormControlObj(Obj).TheControl;
      end
    else if (Obj is TImageObj) then
      Result := Obj;
    end;
end;

function THTMLViewer.GetIDDisplay(const ID: string): boolean;
var
  I: integer;
  Obj: TObject;
begin
Result := False;
with FSectionList.IDNameList do
  if Find(ID, I) then
    begin
    Obj := Objects[I];
    if (Obj is TBlock) then
      Result := not TBlock(Obj).DisplayNone;
    end;
end;

procedure THTMLViewer.SetIDDisplay(const ID: string; Value: boolean);
var
  I: integer;
  Obj: TObject;
begin
with FSectionList.IDNameList do
  if Find(ID, I) then
    begin
    Obj := Objects[I];
    if (Obj is TBlock) and (TBlock(Obj).DisplayNone = Value) then
      begin
      FSectionList.HideControls;
      TBlock(Obj).DisplayNone := not Value;
      end;
    end;
end;

procedure THTMLViewer.SetPrintScale(Value: double);
begin
If Value > 4.0 then
  FPrintScale := 4.0
else if Value < 0.25 then
  FPrintScale := 0.25
else FPrintScale := Value;
end;

procedure THTMLViewer.Reformat;
var
  Pt: TPoint;
begin
Layout;
Update;
GetCursorPos(Pt);
SetCursorPos(Pt.X, Pt.Y); {trigger a mousemove to keep cursor correct}
end;

procedure THTMLViewer.htProgressInit;   
begin
if Assigned(FOnProgress) then
  FOnProgress(Self, psStarting, 0);
end;

procedure THTMLViewer.htProgress(Percent: Integer); 
begin
if Assigned(FOnProgress) then
  FOnProgress(Self, psRunning, Percent);
end;

procedure THTMLViewer.htProgressEnd;
begin
if Assigned(FOnProgress) then
  FOnProgress(Self, psEnding, 100);
end;

{----------------TPaintPanel.CreateIt}
constructor TPaintPanel.CreateIt(AOwner: TComponent; Viewer: ThtmlViewer);

begin
  inherited Create(AOwner);
  FViewer := Viewer;
end;

{----------------TPaintPanel.Paint}
procedure TPaintPanel.Paint;
var
  MemDC: HDC;
  ABitmap: HBitmap;
  ARect: TRect;
  OldPal: HPalette;
begin
if FViewer.DontDraw or (Canvas2 <> Nil) then    
  Exit;
FViewer.DrawBorder;
OldPal := 0;
Canvas.Font := Font;
Canvas.Brush.Color := Color;
ARect := Canvas.ClipRect;
Canvas2 := TCanvas.Create;   {paint on a memory DC}
try
  MemDC := CreateCompatibleDC(Canvas.Handle);
  ABitmap := 0;
  try
    with ARect do
      begin
      ABitmap := CreateCompatibleBitmap(Canvas.Handle, Right-Left, Bottom-Top);
      if (ABitmap = 0) and (Right-Left + Bottom-Top <> 0) then
             raise EOutOfResources.Create('Out of Resources');  
      try
        SelectObject(MemDC, ABitmap);
        SetWindowOrgEx(memDC, Left, Top, Nil);
        Canvas2.Handle := MemDC;
        DoBackground(Canvas2);
        if Assigned(FOnPaint) then FOnPaint(Self);
        OldPal := SelectPalette(Canvas.Handle, ThePalette, False);
        RealizePalette(Canvas.Handle);
        BitBlt(Canvas.Handle, Left, Top, Right-Left, Bottom-Top,
                              MemDC, Left, Top, SrcCopy);
     finally
        if OldPal <> 0 then SelectPalette(MemDC, OldPal, False);
        Canvas2.Handle := 0;
        end;
      end;
  finally
    DeleteDC(MemDC);
    DeleteObject(ABitmap);
  end;
finally
  FreeAndNil(Canvas2);   
  end;
end;

procedure TPaintPanel.DoBackground(ACanvas: TCanvas);
var
  ARect: TRect;
  Image: TGpObject;
  Mask, NewBitmap, NewMask: TBitmap;
  PRec: PtPositionRec;
  BW, BH, X, Y, X2, Y2, IW, IH, XOff, YOff: integer;
  AniGif: TGifImage;  

begin
with FViewer do
  begin
  if FSectionList.Printing then
    Exit;    {no background}

  ARect := Canvas.ClipRect;
  Image := FSectionList.BackgroundBitmap;
  if FSectionList.ShowImages and Assigned(Image) then
    begin
    Mask := FSectionList.BackgroundMask;
    BW := GetImageWidth(Image);
    BH := GetImageHeight(Image);
    PRec := FSectionList.BackgroundPRec;
    BGFixed := PRec[1].Fixed;
    if BGFixed then
      begin  {fixed background}
      XOff := 0;
      YOff := 0;
      IW := Self.ClientRect.Right;    
      IH := Self.ClientRect.Bottom;    
      end
    else
      begin   {scrolling background}
      XOff := HScrollbar.Position;
      YOff := FSectionList.YOff;
      IW := HScrollbar.Max;
      IH := IntMax(MaxVertical, Self.ClientRect.Bottom);  
      end;

    {Calculate where the tiled background images go}
    CalcBackgroundLocationAndTiling(PRec, ARect, XOff, YOff, IW, IH, BW, BH, X, Y, X2, Y2);

    if (BW = 1) or (BH = 1) then
      begin  {this is for people who try to tile 1 pixel images}
        NewBitmap := EnlargeImage(Image, X2-X, Y2-Y);// as TBitmap;
      try
        if Assigned(Mask) then
            NewMask := TBitmap(EnlargeImage(Mask, X2-X, Y2-Y))
        else NewMask := Nil;
        try
          DrawBackground(ACanvas, ARect, X, Y, X2, Y2, NewBitmap, NewMask, Nil, NewBitmap.Width, NewBitmap.Height, Self.Color);
        finally
          NewMask.Free;
          end;
      finally
        NewBitmap.Free;
        end;
      end
    else  {normal situation}
      begin
      AniGif := FSectionList.BackgroundAniGif;
      DrawBackground(ACanvas, ARect, X, Y, X2, Y2, Image, Mask, AniGif, BW, BH, Self.Color);
      end;
    end
  else
    begin  {no background image, show color only}
    BGFixed := False;
    DrawBackground(ACanvas, ARect, 0,0,0,0, Nil, Nil, Nil, 0, 0, Self.Color);  
    end;
  end;
end;

procedure TPaintPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
Message.Result := 1;   {it's erased}
end;

{----------------TPaintPanel.WMLButtonDblClk} 
procedure TPaintPanel.WMLButtonDblClk(var Message: TWMMouse);
begin
if Message.Keys and MK_LButton <> 0 then
  ThtmlViewer(FViewer).HTMLMouseDblClk(Message);  
end;

{----------------T32ScrollBar.SetParams}
procedure T32ScrollBar.SetParams(APosition, APage, AMin, AMax: Integer);
var
  ScrollInfo: TScrollInfo;
begin
if (APosition <> FPosition) or (APage <> FPage) or (AMin <> FMin)
              or (AMax <> FMax) then
  with ScrollInfo do
    begin
    cbSize := SizeOf(ScrollInfo);
    fMask := SIF_ALL;
    if htShowVScroll in (Owner as ThtmlViewer).FOptions then
      fMask := fMask or SIF_DISABLENOSCROLL;
    nPos := APosition;
    nPage := APage;
    nMin := AMin;
    nMax := AMax;
    SetScrollInfo(Handle, SB_CTL, ScrollInfo, True);
    FPosition := APosition;
    FPage := APage;
    FMin := AMin;
    FMax := AMax;
    end;
end;

procedure T32ScrollBar.SetPosition(Value: integer);
var
  SavePos: integer;
begin
SavePos := FPosition;
SetParams(Value, FPage, FMin, FMax);
if FPosition <> SavePos then
  Change;           
end;

procedure T32ScrollBar.SetMin(Value: Integer);
begin
  SetParams(FPosition, FPage, Value, FMax);
end;

procedure T32ScrollBar.SetMax(Value: Integer);
begin
  SetParams(FPosition, FPage, FMin, Value);
end;

procedure T32ScrollBar.CNVScroll(var Message: TWMVScroll);
var
  SPos: integer;
  ScrollInfo: TScrollInfo;
  OrigPos: integer;
  TheChange: integer;
begin
with ThtmlViewer(Parent) do
  begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Self.Handle, SB_CTL, ScrollInfo);
  if TScrollCode(Message.ScrollCode) = scTrack then
    begin
    OrigPos := ScrollInfo.nPos;
    SPos := ScrollInfo.nTrackPos;
    end
  else
    begin
    SPos := ScrollInfo.nPos;
    OrigPos := SPos;
    case TScrollCode(Message.ScrollCode) of
      scLineUp:
        Dec(SPos, SmallChange);
      scLineDown:
        Inc(SPos, SmallChange);
      scPageUp:
        Dec(SPos, LargeChange);
      scPageDown:
        Inc(SPos, LargeChange);
      scTop:
        SPos := 0;
      scBottom:
        SPos := (FMaxVertical - PaintPanel.Height);
      end;
    end;
  SPos := IntMax(0, IntMin(SPos, (FMaxVertical - PaintPanel.Height)));

  Self.SetPosition(SPos);

  FSectionList.SetYOffset(SPos);
  if BGFixed then
    PaintPanel.Invalidate
  else
    begin      {scroll background}
    TheChange := OrigPos-SPos;
    ScrollWindow(PaintPanel.Handle,0,TheChange,NIL,NIL);
    PaintPanel.Update;
    end;
  end;
end;

{ PositionObj }

destructor PositionObj.Destroy;
begin
FormData.Free;
inherited;
end;             
      
end.

