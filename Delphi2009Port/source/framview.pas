{Version 9.45}
{*********************************************************}
{*                     FRAMVIEW.PAS                      *}
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

unit FramView;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Menus, htmlsubs, htmlview, htmlun2,
  readHTML;

type
  {common to TFrameViewer and TFrameBrowser}
  THotSpotTargetClickEvent = procedure(Sender: TObject; const Target, URL: string;
                     var Handled: boolean) of Object;
  THotSpotTargetEvent = procedure(Sender: TObject; const Target, URL: string) of Object;
  TWindowRequestEvent = procedure(Sender: TObject; const Target, URL: string) of Object;
  fvOptionEnum = (fvMetaRefresh, fvNoBorder, fvNoLinkUnderline, fvOverLinksActive,
                  fvPrintMonochromeBlack, fvPrintTableBackground, fvPrintBackground,  
                  fvShowVScroll, fvNoFocusRect, fvShowDummyCaret, fvNoWheelMouse,
                  fvNoLinkHilite);     
  TFrameViewerOptions = set of fvOptionEnum;

  {for TFrameViewer}
  TStreamRequestEvent = procedure(Sender: TObject; const SRC: string;
                           var Stream: TStream) of Object;
  TBufferRequestEvent = procedure(Sender: TObject; const SRC: string;
                           var Buffer: PChar; var BuffSize: integer) of Object;
  TStringsRequestEvent = procedure(Sender: TObject; const SRC: string;
                           var Strings: TStrings) of Object;
  TFileRequestEvent = procedure(Sender: TObject; const SRC: string;
                           var NewName: string) of Object;
  EfvLoadError = class(Exception);

{common base class for TFrameViewer and TFrameBrowser}
  TFVBase = class(TFrameViewerBase)  {TFrameViewerBase is in ReadHTML.pas}
  protected
    FURL: string;
    FTarget: string;
    FLinkText: string;  
    FLinkAttributes: TStringList;  
    FOnHotSpotTargetClick: THotSpotTargetClickEvent;
    FOnHotSpotTargetCovered: THotSpotTargetEvent;
    ProcessList: TList;   {list of viewers that are processing}
    FViewImages: boolean;
    FImageCacheCount: integer;
    FBitmapList: TStringBitmapList;
    FProcessing, FViewerProcessing: boolean;
    FNoSelect: boolean;
    FOnHistoryChange: TNotifyEvent;
    FOnBitmapRequest: TGetBitmapEvent;
    FOnImageRequest: TGetImageEvent;
    FOnBlankWindowRequest: TWindowRequestEvent;
    FOnMeta: TMetaType;
    FOnScript: TScriptEvent;
    FOnImageClick: TImageClickEvent;
    FOnImageOver: TImageOverEvent;
    FOnFileBrowse: TFileBrowseEvent;  
    FOnObjectClick: TObjectClickEvent;
    FOnObjectFocus: ThtObjectEvent;
    FOnObjectBlur: ThtObjectEvent;
    FOnObjectChange: ThtObjectEvent; 
    FOnRightClick: TRightClickEvent;
    FOnMouseDouble: TMouseEvent;
    FServerRoot: string;
    FOnInclude: TIncludeType;
    FOnSoundRequest: TSoundType;
    FPrintMarginLeft,
    FPrintMarginRight,
    FPrintMarginTop,
    FPrintMarginBottom: double;
    FMarginWidth, FMarginHeight: integer;   
    FOnPrintHeader, FOnPrintFooter: TPagePrinted;
    FOnPrintHTMLHeader, FOnPrintHTMLFooter: ThtmlPagePrinted;  
    FVisitedMaxCount: integer;
    FBackground: TColor;
    FFontName: string;
    FPreFontName: string;
    FFontColor: TColor;
    FHotSpotColor, FVisitedColor, FOverColor: TColor; 
    FFontSize: integer;
    FCursor: TCursor;
    FHistoryMaxCount: integer;
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    FCharset: TFontCharset;
    {$endif}
    FOnProcessing: TProcessingEvent;
    FHistory, FTitleHistory: TStrings;
    FDither: boolean;
    FOnPanelCreate: TPanelCreateEvent;   
    FOnPanelDestroy: TPanelDestroyEvent;   
    FOnPanelPrint: TPanelPrintEvent;    
    Visited: TStringList;     {visited URLs}
    FOnDragDrop: TDragDropEvent;  
    FOnDragOver: TDragOverEvent;
    FViewerList: TStrings;    
    FOnParseBegin: TParseEvent;
    FOnParseEnd: TNotifyEvent;
    FPrintScale: double;
    FOnObjectTag: TObjectTagEvent;  

    function GetCurViewerCount: integer; virtual; abstract;
    function GetCurViewer(I: integer): ThtmlViewer; virtual; abstract;
    function GetFURL: string;
    function GetProcessing: boolean;
    function GetTarget: String;
    procedure SetViewImages(Value: boolean);
    procedure SetImageCacheCount(Value: integer);
    procedure SetNoSelect(Value: boolean);
    procedure SetOnBitmapRequest(Handler: TGetBitmapEvent);
    procedure SetOnMeta(Handler: TMetaType);
    procedure SetOnLink(Handler: TLinkType);
    procedure SetOnScript(Handler: TScriptEvent);
    procedure SetImageOver(Handler: TImageOverEvent);
    procedure SetImageClick(Handler: TImageClickEvent);
    procedure SetOnFileBrowse(Handler: TFileBrowseEvent);  
    procedure SetOnObjectClick(Handler: TObjectClickEvent);
    procedure SetOnObjectFocus(Handler: ThtObjectEvent);
    procedure SetOnObjectBlur(Handler: ThtObjectEvent);
    procedure SetOnObjectChange(Handler: ThtObjectEvent);  
    procedure SetOnRightClick(Handler: TRightClickEvent);
    procedure SetMouseDouble(Handler: TMouseEvent);  
    procedure SetServerRoot(Value: string);
    procedure SetPrintMarginLeft(Value: Double);
    procedure SetPrintMarginRight(Value: Double);
    procedure SetPrintMarginTop(Value: Double);
    procedure SetPrintMarginBottom(Value: Double);
    procedure SetPrintScale(Value: double);
    procedure SetPrintHeader(Handler: TPagePrinted);
    procedure SetPrintFooter(Handler: TPagePrinted);
    procedure SetPrintHtmlHeader(Handler: THtmlPagePrinted);
    procedure SetPrintHtmlFooter(Handler: THtmlPagePrinted);
    procedure SetMarginHeight(Value: integer);
    procedure SetMarginWidth(Value: integer);   
    procedure SetVisitedMaxCount(Value: integer);
    procedure SetColor(Value: TColor);
    function GetFontName: TFontName;
    procedure SetFontName(Value: TFontName);
    function GetPreFontName: TFontName;
    procedure SetPreFontName(Value: TFontName);
    procedure SetFontSize(Value: integer);
    procedure SetFontColor(Value: TColor);
    procedure SetHotSpotColor(Value: TColor);
    procedure SetActiveColor(Value: TColor);
    procedure SetVisitedColor(Value: TColor);
    procedure SetHistoryMaxCount(Value: integer);
    procedure SetCursor(Value: TCursor);
    function GetSelLength: integer;
    procedure SetSelLength(Value: integer);
    function GetSelStart: integer;
    procedure SetSelStart(Value: integer);
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    procedure SetCharset(Value: TFontCharset);
    {$endif}
    function GetOurPalette: HPalette;
    procedure SetOurPalette(Value: HPalette);
    procedure SetDither(Value: boolean);
    function GetCaretPos: integer;
    procedure SetCaretPos(Value: integer);
    function GetSelText: WideString;   
    function GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;
    procedure SetProcessing(Local, Viewer: boolean);
    procedure CheckProcessing(Sender: TObject; ProcessingOn: boolean);
    procedure SetOnPanelCreate(Handler: TPanelCreateEvent);
    procedure SetOnPanelDestroy(Handler: TPanelDestroyEvent);
    procedure SetOnPanelPrint(Handler: TPanelPrintEvent);  
    procedure SetOnParseBegin(Handler: TParseEvent);
    procedure SetOnParseEnd(Handler: TNotifyEvent);
    procedure SetOnObjectTag(Handler: TObjectTagEvent);  

    function GetActiveViewer: ThtmlViewer;  virtual; abstract;
    function GetViewers: TStrings;  virtual; abstract; 

    property CurViewer[I: integer]: ThtmlViewer read GetCurViewer;
    property OnBitmapRequest: TGetBitmapEvent read FOnBitmapRequest
             write SetOnBitmapRequest;
    property ServerRoot: string read FServerRoot write SetServerRoot;
  public
    procedure ClearHistory; virtual; abstract;
    procedure SetFocus; override;
    function InsertImage(Viewer: ThtmlViewer; const Src: string; Stream: TMemoryStream): boolean;
    function NumPrinterPages: integer; overload;
    function NumPrinterPages(var WidthRatio: double): integer; overload;
    procedure Print(FromPage, ToPage: integer);

    property URL: string read GetFURL;
    property Target: string read GetTarget;
    property Processing: boolean read GetProcessing;
    property ActiveViewer: ThtmlViewer read GetActiveViewer;
    property History: TStrings read FHistory;
    property TitleHistory: TStrings read FTitleHistory;
    property Palette: HPalette read GetOurPalette write SetOurPalette;
    property Dither: boolean read FDither write SetDither default True;
    property CaretPos: integer read GetCaretPos write SetCaretPos;
    property SelLength: integer read GetSelLength write SetSelLength;  
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: WideString read GetSelText;  
    procedure CopyToClipboard;
    procedure SelectAll;
    function FindEx(const S: WideString; MatchCase, Reverse: boolean): boolean;  
    function Find(const S: WideString; MatchCase: boolean): boolean;
    property Viewers: TStrings read GetViewers;
    property LinkText: string read FLinkText;   
    property LinkAttributes: TStringList read FLinkAttributes;  

  published
    property OnHotSpotTargetCovered: THotSpotTargetEvent read FOnHotSpotTargetCovered
             write FOnHotSpotTargetCovered;
    property OnHotSpotTargetClick: THotSpotTargetClickEvent read FOnHotSpotTargetClick
             write FOnHotSpotTargetClick;
    property ViewImages: boolean read FViewImages write SetViewImages default True;
    property ImageCacheCount: integer read FImageCacheCount
             write SetImageCacheCount default 5;
    property OnHistoryChange: TNotifyEvent read FOnHistoryChange
             write FOnHistoryChange;
    property NoSelect: boolean read FNoSelect write SetNoSelect;

    property OnBlankWindowRequest: TWindowRequestEvent read FOnBlankWindowRequest
             write FOnBlankWindowRequest;
    property OnScript: TScriptEvent read FOnScript write SetOnScript;
    property OnImageClick: TImageClickEvent read FOnImageClick write SetImageClick;
    property OnImageOver: TImageOverEvent read FOnImageOver write SetImageOver;
    property OnFileBrowse: TFileBrowseEvent read FOnFileBrowse write SetOnFileBrowse;  
    property OnObjectClick: TObjectClickEvent read FOnObjectClick write SetOnObjectClick;
    property OnObjectFocus: ThtObjectEvent read FOnObjectFocus write SetOnObjectFocus;
    property OnObjectBlur: ThtObjectEvent read FOnObjectBlur write SetOnObjectBlur;
    property OnObjectChange: ThtObjectEvent read FOnObjectChange write SetOnObjectChange;  
    property OnRightClick:  TRightClickEvent read FOnRightClick write SetOnRightClick;
    property OnMouseDouble: TMouseEvent read FOnMouseDouble write SetMouseDouble;  
    property OnInclude: TIncludeType read FOnInclude write FOnInclude;
    property OnSoundRequest: TSoundType read FOnSoundRequest write FOnSoundRequest;
    property PrintMarginLeft: double read FPrintMarginLeft write SetPrintMarginLeft;
    property PrintMarginRight: double read FPrintMarginRight write SetPrintMarginRight;
    property PrintMarginTop: double read FPrintMarginTop write SetPrintMarginTop;
    property PrintMarginBottom: double read FPrintMarginBottom write SetPrintMarginBottom;
    property PrintScale: double read FPrintScale write SetPrintScale;
    property MarginWidth: integer read FMarginWidth write SetMarginWidth default 10;   
    property MarginHeight: integer read FMarginHeight write SetMarginHeight default 5;   
    property OnPrintHeader: TPagePrinted read FOnPrintHeader write SetPrintHeader;
    property OnPrintFooter: TPagePrinted read FOnPrintFooter write SetPrintFooter;
    property OnPrintHTMLHeader: ThtmlPagePrinted read FOnPrintHTMLHeader write SetPrintHTMLHeader;
    property OnPrintHTMLFooter: ThtmlPagePrinted read FOnPrintHTMLFooter write SetPrintHTMLFooter;
    property OnMeta: TMetaType read FOnMeta write SetOnMeta;
    property OnLink: TLinkType read FOnLink write SetOnLink;
    property OnPanelCreate: TPanelCreateEvent read FOnPanelCreate write SetOnPanelCreate;
    property OnPanelDestroy: TPanelDestroyEvent read FOnPanelDestroy write SetOnPanelDestroy;
    property OnPanelPrint: TPanelPrintEvent read FOnPanelPrint write SetOnPanelPrint;   
    property OnParseBegin: TParseEvent read FOnParseBegin write SetOnParseBegin;
    property OnParseEnd: TNotifyEvent read FOnParseEnd write SetOnParseEnd;

    property DefBackground: TColor read FBackground write SetColor default clBtnFace;

    property DefFontName: TFontName read GetFontName write SetFontName;
    property DefPreFontName: TFontName read GetPreFontName write SetPreFontName;
    property DefFontSize: integer read FFontSize write SetFontSize default 12;
    property DefFontColor: TColor read FFontColor write SetFontColor
             default clBtnText;
    property DefHotSpotColor: TColor read FHotSpotColor write SetHotSpotColor
             default clBlue;
    property DefVisitedLinkColor: TColor read FVisitedColor write SetVisitedColor
             default clPurple;       
    property DefOverLinkColor: TColor read FOverColor write SetActiveColor
             default clBlue;        
    property VisitedMaxCount: integer read FVisitedMaxCount write SetVisitedMaxCount default 50; 
    property HistoryMaxCount: integer read FHistoryMaxCount write SetHistoryMaxCount;
    property Cursor: TCursor read FCursor write SetCursor default crIBeam;
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    property CharSet: TFontCharset read FCharSet write SetCharset;
    {$endif}
    property OnProcessing: TProcessingEvent read FOnProcessing write FOnProcessing;
    property OnObjectTag:  TObjectTagEvent read FOnObjectTag write SetOnObjectTag; 


    property Align;
    {$ifdef ver120_plus}
    property Anchors;     
    {$endif}
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property Height default 150;
    property Width default 150;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

{TFrameViewer Types}
  PEventRec = ^EventRec;    
  EventRec = record
    LStyle: LoadStyleType;
    NewName: string;
    AString: string;
    end;

  TFrameSet = class;
  TSubFrameSet = class;

  TFrameBase = class(TCustomPanel)   {base class for other classes}
    MasterSet: TFrameSet;   {Points to top (master) TFrameSet}
  private
    UnLoaded: boolean;
    procedure UpdateFrameList; virtual; abstract;
  protected
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    LocalCharSet: TFontCharset;
    {$endif}
    procedure FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X,
           Y: Integer); virtual; abstract;
    procedure FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    function CheckNoResize(var Lower, Upper: boolean): boolean; virtual; abstract;
    procedure LoadFiles(PEV: PEventRec); virtual; abstract;
    procedure ReLoadFiles(APosition: integer); virtual; abstract;
    procedure UnloadFiles; virtual; abstract;

  public
    LOwner: TSubFrameSet;
    procedure InitializeDimensions(X, Y, Wid, Ht: integer); virtual; abstract;
  end;

  TfvFrame = class(TFrameBase) {TfvFrame holds a ThtmlViewer or TSubFrameSet}
  protected
    NoScroll: boolean;
    frMarginHeight, frMarginWidth: integer;   
    frHistory: TStringList;
    frPositionHistory: TFreeList;
    frHistoryIndex: integer;
    RefreshTimer: TTimer;
    NextFile: string;

    procedure CreateViewer;
    procedure frBumpHistory(const NewName: string; NewPos, OldPos: integer;
                  OldFormData: TFreeList);  
    procedure frBumpHistory1(const NewName: string; Pos: integer);
    procedure frSetHistoryIndex(Value: integer);
    procedure UpdateFrameList; override;
    procedure RefreshEvent(Sender: TObject; Delay: integer; const URL: string);
    procedure RefreshTimerTimer(Sender: TObject);

  protected
    procedure FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); override;
    procedure FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function CheckNoResize(var Lower, Upper: boolean): boolean; override;
    procedure LoadFiles(PEV: PEventRec); override;
    procedure ReLoadFiles(APosition: integer); override;
    procedure UnloadFiles; override;
    procedure frLoadFromFile(const FName, Dest: string; Bump, Reload: boolean);  
    procedure ReloadFile(const FName: string; APosition: integer);
  public
    Viewer: ThtmlViewer;    {the ThtmlViewer it holds if any}
    ViewerPosition: integer;
    ViewerFormData: TFreeList;   
    FrameSet: TSubFrameSet; {or the TSubFrameSet it holds}
    Source,         {Dos filename or URL for this frame}
    OrigSource,      {Original Source name}  
    Destination: string;    {Destination offset for this frame}
    WinName: string;     {window name, if any, for this frame}
    NoReSize: boolean;

    constructor CreateIt(AOwner: TComponent; L: TAttributeList;
              Master: TFrameSet; const Path: string);
    destructor Destroy; override;
    procedure InitializeDimensions(X, Y, Wid, Ht: integer); override;
    procedure Repaint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;  
  end;

  TSubFrameSet = class(TFrameBase)  {can contain one or more TFrames and/or TSubFrameSets}
  Protected
    FBase: string;
    FBaseTarget: string;
    OuterBorder: integer;
    BorderSize: integer;
    FRefreshURL: string;
    FRefreshDelay: integer;
    RefreshTimer: TTimer;
    NextFile: string;

    procedure ClearFrameNames;
    procedure AddFrameNames;
    procedure UpdateFrameList; override;
    procedure HandleMeta(Sender: TObject; const HttpEq, Name, Content: string);
    procedure SetRefreshTimer;
    procedure RefreshTimerTimer(Sender: Tobject); virtual;
  protected
    OldRect: TRect;
    function GetRect: TRect;
    procedure FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); override;
    procedure FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure FindLineAndCursor(Sender: TObject; X, Y: integer);
    function NearBoundary(X, Y: integer): boolean;
    function CheckNoResize(var Lower, Upper: boolean): boolean; override;
    procedure Clear; virtual;
    procedure LoadFromFile(const FName, Dest: string);
  public
    First: boolean;     {First time thru}
    Rows: boolean;          {set if row frameset, else column frameset}
    List: TFreeList;   {list of TFrames and TSubFrameSets in this TSubFrameSet}
    Dim,    {col width or row height as read.  Blanks may have been added}
    DimF,   {col width or row height in pixels as calculated and displayed}
    Lines   {pixel pos of lines, Lines[1]=0, Lines[DimCount]=width|height}
         : array[0..20] of SmallInt;
    Fixed   {true if line not allowed to be dragged}
         : array[0..20] of boolean;
    DimCount: integer;
    DimFTot: integer;
    LineIndex: integer;

    constructor CreateIt(AOwner: TComponent; Master: TFrameSet);
    destructor Destroy; override;
    function AddFrame(Attr: TAttributeList; const FName: string): TfvFrame;
    procedure EndFrameSet; virtual;
    procedure DoAttributes(L: TAttributeList);
    procedure LoadFiles(PEV: PEventRec); override;
    procedure ReLoadFiles(APosition: integer); override;
    procedure UnloadFiles; override;
    procedure InitializeDimensions(X, Y, Wid, Ht: integer); override;
    procedure CalcSizes(Sender: TObject);
  end;

  TFrameViewer = class;

  TFrameSet = class(TSubFrameSet)  {only one of these showing, others may be held as History}
  protected
    FTitle: string;
    FCurrentFile: string;
    FrameNames: TStringList; {list of Window names and their TFrames}
    Viewers: TList;   {list of all ThtmlViewer pointers}
    Frames: TList;    {list of all the Frames contained herein}
    HotSet: TFrameBase;     {owner of line we're moving}
    OldWidth, OldHeight: integer;
    NestLevel: integer;
    FActive: ThtmlViewer;   {the most recently active viewer}

    function RequestEvent: boolean;
    function TriggerEvent(const Src: string; PEV: PEventRec): boolean;
    procedure ClearForwards;
    procedure UpdateFrameList; override;
    procedure RefreshTimerTimer(Sender: Tobject); override;

  protected
    procedure FVMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); override;
    procedure CheckActive(Sender: TObject);
    function GetActive: ThtmlViewer;
  public
    FrameViewer: TFrameViewer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EndFrameSet; override;
    procedure LoadFromFile(const FName, Dest: string);
    procedure Clear; override;
    procedure CalcSizes(Sender: TObject);
    procedure Repaint; override;
  end;

  TFrameViewer = class(TFVBase)
  protected
    FPosition: TList;
    FHistoryIndex: integer;
    FOnFormSubmit: TFormSubmitEvent;
    FOptions: TFrameViewerOptions;
    UrlRequestStream: TMemoryStream;  

    FOnStreamRequest: TStreamRequestEvent;
    FOnBufferRequest: TBufferRequestEvent;
    FOnStringsRequest: TStringsRequestEvent;
    FOnFileRequest: TFileRequestEvent;
    FOnProgress: ThtProgressEvent;

    FBaseEx: string;

    procedure SetOnImageRequest(Handler: TGetImageEvent);

    function GetBase: string;
    procedure SetBase(Value: string);
    function GetBaseTarget: string;
    function GetTitle: string;
    function GetCurrentFile: string;
    procedure HotSpotCovered(Sender: TObject; const SRC: string);
    procedure SetHistoryIndex(Value: integer);

    procedure SetOnFormSubmit(Handler: TFormSubmitEvent);
    procedure ChkFree(Obj: TObject);
    function GetActiveBase: string;
    function GetActiveTarget: string;
    function GetFwdButtonEnabled: boolean;
    function GetBackButtonEnabled: boolean;
    procedure SetOptions(Value: TFrameViewerOptions);
    procedure fvDragDrop(Sender, Source: TObject; X, Y: Integer);  
    procedure fvDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SetDragDrop(const Value: TDragDropEvent);
    procedure SetDragOver(const Value: TDragOverEvent);
    function GetViewers: TStrings;  override;
    procedure SetOnProgress(Handler: ThtProgressEvent);   

  protected
    CurFrameSet: TFrameSet;  {the TFrameSet being displayed}

    function GetCurViewerCount: integer; override;
    function GetCurViewer(I: integer): ThtmlViewer; override;
    function GetActiveViewer: ThtmlViewer;  override;


    procedure BumpHistory(OldFrameSet: TFrameSet; OldPos: integer);
    procedure BumpHistory1(const FileName, Title: string;
                 OldPos: integer; ft: ThtmlFileType);
    procedure BumpHistory2(OldPos: integer);
    function HotSpotClickHandled: boolean;
    procedure LoadFromFileInternal(const FName: string);

    procedure AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: string); override;
    function CreateSubFrameSet(FrameSet: TObject): TObject; override;
    procedure DoAttributes(FrameSet: TObject; Attr: TAttributeList); override;
    procedure EndFrameSet(FrameSet: TObject); override;
    procedure AddVisitedLink(const S: string);
    procedure CheckVisitedLinks;
    procedure DoURLRequest(Sender: TObject; const SRC: string; var RStream: TMemoryStream);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FName: string);
    procedure Load(const SRC: string);
    procedure LoadTargetFromFile(const Target, FName: string);
    procedure LoadImageFile(const FName: string);
    procedure Reload;
    procedure Clear;
    procedure HotSpotClick(Sender: TObject; const AnURL: string;
              var Handled: boolean);
    function HTMLExpandFilename(const Filename: string): string; virtual;
    procedure ClearHistory; override;
    function ViewerFromTarget(const Target: string): ThtmlViewer;
    procedure GoBack;
    procedure GoFwd;
    procedure Repaint; override;

    property Base: string read GetBase write SetBase;
    property BaseTarget: string read GetBaseTarget;
    property DocumentTitle: string read GetTitle;
    property CurrentFile: string read GetCurrentFile;
    property HistoryIndex: integer read FHistoryIndex write SetHistoryIndex;

  published
    property OnImageRequest: TGetImageEvent read FOnImageRequest
             write SetOnImageRequest;
    property OnFormSubmit: TFormSubmitEvent read FOnFormSubmit
             write SetOnFormSubmit;
    property FwdButtonEnabled: boolean read GetFwdButtonEnabled;
    property BackButtonEnabled: boolean read GetBackButtonEnabled;
    property fvOptions: TFrameViewerOptions read FOptions write SetOptions
                 default [fvPrintTableBackground, fvPrintMonochromeBlack];

    property OnStreamRequest: TStreamRequestEvent read FOnStreamRequest write FOnStreamRequest;
    property OnStringsRequest: TStringsRequestEvent read FOnStringsRequest write FOnStringsRequest;
    property OnBufferRequest: TBufferRequestEvent read FOnBufferRequest write FOnBufferRequest;
    property OnFileRequest: TFileRequestEvent read FOnFileRequest write FOnFileRequest;

    property OnBitmapRequest;
    property ServerRoot;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write SetDragDrop;  
    property OnDragOver: TDragOverEvent read FOnDragOver write SetDragOver;
    property OnProgress: ThtProgressEvent read FOnProgress write SetOnProgress; 
  end;

implementation

const
  Sequence: integer = 10;

type
  PositionObj = class(TObject)
    Pos: integer;
    Seq: integer;
    FormData: TFreeList;
    destructor Destroy; override;
    end;

function ImageFile(Const S: string): boolean;
var
  Ext: string;
begin
Ext := Lowercase(ExtractFileExt(S));
Result := (Ext = '.gif') or (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.bmp')
       or (Ext = '.png');
end;

function TexFile(Const S: string): boolean;
var
  Ext: string;
begin
Ext := Lowercase(ExtractFileExt(S));
Result := (Ext = '.txt');
end;

{----------------FileToString}
function FileToString(const Name: String): string;
var
  FS: TFileStream;
  Tmp: AnsiString;
begin
Result := '';
FS := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
try
  SetLength(Tmp, FS.Size);
  FS.ReadBuffer(Tmp[1], FS.Size * SizeOf(AnsiChar));
  Result := Tmp;
finally
  FS.Free;
  end;
end;

{----------------SplitURL}
procedure SplitURL(const Src: string; var FName, Dest: string);
{Split an URL into filename and Destination}
var
  I: integer;
begin
I := Pos('#', Src);
if I >= 1 then
  begin
  Dest := System.Copy(Src, I, Length(Src)-I+1);  {local destination}
  FName := System.Copy(Src, 1, I-1);     {the file name}
  end
else
  begin
  FName := Src;
  Dest := '';    {no local destination}
  end;
end;

{----------------TfvFrame.CreateIt}
constructor TfvFrame.CreateIt(AOwner: TComponent; L: TAttributeList;
                   Master: TFrameSet; const Path: string);
var
  I: integer;
  S, Dest: string;
begin
inherited Create(AOwner);
{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
if AOwner is TSubFrameSet then
  LocalCharSet := TSubFrameset(AOwner).LocalCharSet;
{$endif}
LOwner := AOwner as TSubFrameSet;
MasterSet := Master;
BevelInner := bvNone;
frMarginWidth := MasterSet.FrameViewer.MarginWidth;
frMarginHeight := MasterSet.FrameViewer.MarginHeight;
if LOwner.BorderSize = 0 then
  BevelOuter := bvNone
else
  begin
  BevelOuter := bvLowered;
  BevelWidth := LOwner.BorderSize;
  end;
ParentColor := True;
if Assigned(L) then
  for I := 0 to L.Count-1 do
    with TAttribute(L[I]) do
      case Which of
        SrcSy:
          begin
          SplitUrl(Trim(Name), S, Dest);
          Destination := Dest;
          if not Master.RequestEvent then
            begin
            S := HTMLServerToDos(S, Master.FrameViewer.ServerRoot);
            if Pos(':', S) = 0 then
              begin
              if ReadHTML.Base <> '' then   {a Base was found}
                if CompareText(ReadHTML.Base, 'DosPath') = 0 then
                   S := ExpandFilename(S)
                else
                  S := ExtractFilePath(HTMLToDos(ReadHTML.Base)) + S
              else S := Path + S;
              end;
            end;
          Source := S;
          OrigSource := S;
          end;
        NameSy: WinName := Name;
        NoResizeSy:  NoResize := True;
        ScrollingSy:
          if CompareText(Name, 'NO') = 0 then  {auto and yes work the same}
            NoScroll := True;
        MarginWidthSy:  frMarginWidth := Value;
        MarginHeightSy: frMarginHeight := Value;
        end;
if WinName <> '' then   {add it to the Window name list}
  (AOwner as TSubFrameSet).MasterSet.FrameNames.AddObject(Uppercase(WinName), Self);
OnMouseDown := FVMouseDown;
OnMouseMove := FVMouseMove;
OnMouseUp := FVMouseUp;
frHistory := TStringList.Create;
frPositionHistory := TFreeList.Create;
end;

{----------------TfvFrame.Destroy}
destructor TfvFrame.Destroy;
var
  I: integer;
begin
if Assigned(MasterSet) then
  begin
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
    if Viewer = MasterSet.FActive then MasterSet.FActive := Nil;
    end;
  end;
if Assigned(Viewer) then
  begin
  Viewer.Free;
  Viewer := Nil;
  end
else if Assigned(FrameSet) then
  begin
  FrameSet.Free;
  FrameSet := Nil;
  end;
frHistory.Free;  frHistory := Nil;
frPositionHistory.Free;  frPositionHistory := Nil;
ViewerFormData.Free;
RefreshTimer.Free;
inherited Destroy;
end;

procedure TfvFrame.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
inherited;
{in most cases, SetBounds results in a call to CalcSizes.  However, to make sure
 for case where there is no actual change in the bounds.... }
if Assigned(FrameSet) then
  FrameSet.CalcSizes(Nil);
end;

procedure TfvFrame.RefreshEvent(Sender: TObject; Delay: integer; const URL: string);
begin
if not (fvMetaRefresh in MasterSet.FrameViewer.FOptions) then
  Exit;
if URL = '' then
  NextFile := Source
else NextFile := MasterSet.FrameViewer.HTMLExpandFilename(URL);
if not FileExists(NextFile) and not MasterSet.RequestEvent then
  Exit;
if not Assigned(RefreshTimer) then
  RefreshTimer := TTimer.Create(Self);
RefreshTimer.OnTimer := RefreshTimerTimer;
RefreshTimer.Interval := Delay*1000;
RefreshTimer.Enabled := True;
end;

procedure TfvFrame.RefreshTimerTimer(Sender: TObject);
begin
RefreshTimer.Enabled := False;
if Unloaded then Exit;
if (MasterSet.Viewers.Count = 1) then    {load a new FrameSet}
  begin
  if CompareText(NextFile, MasterSet.FCurrentFile) = 0 then
    MasterSet.FrameViewer.Reload
  else MasterSet.FrameViewer.LoadFromFileInternal(NextFile);
  end
else
  frLoadFromFile(NextFile, '', True, True);     {reload set}
end;

procedure TfvFrame.RePaint;
begin
if Assigned(Viewer) then Viewer.RePaint
else if Assigned(FrameSet) then FrameSet.RePaint;
inherited RePaint;
end;

{----------------TfvFrame.FVMouseDown}
procedure TfvFrame.FVMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
(Parent as TSubFrameSet).FVMouseDown(Sender, Button, Shift, X+Left, Y+Top);
end;

{----------------TfvFrame.FVMouseMove}
procedure TfvFrame.FVMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
if not NoResize then
  (Parent as TSubFrameSet).FVMouseMove(Sender, Shift, X+Left, Y+Top);
end;

{----------------TfvFrame.FVMouseUp}
procedure TfvFrame.FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
(Parent as TSubFrameSet).FVMouseUp(Sender, Button, Shift, X+Left, Y+Top);
end;

{----------------TfvFrame.CheckNoResize}
function TfvFrame.CheckNoResize(var Lower, Upper: boolean): boolean;
begin
Result := NoResize;
Lower := NoResize;
Upper := NoResize;
end;

{----------------TfvFrame.InitializeDimensions}
procedure TfvFrame.InitializeDimensions(X, Y, Wid, Ht: integer);
begin
if Assigned(FrameSet) then
  FrameSet.InitializeDimensions(X, Y, Wid, Ht);
end;

{----------------TfvFrame.CreateViewer}
procedure TfvFrame.CreateViewer;
begin
Viewer := ThtmlViewer.Create(Self);  {the Viewer for the frame}
Viewer.FrameOwner := Self;
Viewer.Width := ClientWidth;
Viewer.Height := ClientHeight;
Viewer.Align := alClient;
if (MasterSet.BorderSize = 0) or (fvNoFocusRect in MasterSet.FrameViewer.fvOptions) then
  Viewer.BorderStyle := htNone;
Viewer.OnHotspotClick := LOwner.MasterSet.FrameViewer.HotSpotClick;
Viewer.OnHotspotCovered := LOwner.MasterSet.FrameViewer.HotSpotCovered;
if NoScroll then
  Viewer.Scrollbars := ssNone;
Viewer.DefBackground := MasterSet.FrameViewer.FBackground;
Viewer.Visible := False;
InsertControl(Viewer);
Viewer.SendToBack;
Viewer.Visible := True;
Viewer.Tabstop := True;
{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
Viewer.CharSet := LocalCharset;
{$endif}
MasterSet.Viewers.Add(Viewer);
with MasterSet.FrameViewer do
  begin
  Viewer.ViewImages := FViewImages;
  Viewer.SetStringBitmapList(FBitmapList);
  Viewer.ImageCacheCount := FImageCacheCount;
  Viewer.NoSelect := FNoSelect;
  Viewer.DefFontColor := FFontColor;
  Viewer.DefHotSpotColor := FHotSpotColor;
  Viewer.DefVisitedLinkColor := FVisitedColor;
  Viewer.DefOverLinkColor := FOverColor;
  Viewer.DefFontSize := FFontSize;
  Viewer.DefFontName := FFontName;
  Viewer.DefPreFontName := FPreFontName;
  Viewer.OnBitmapRequest := FOnBitmapRequest;
  if fvOverLinksActive in FOptions then
    Viewer.htOptions := Viewer.htOptions + [htOverLinksActive];
  if fvNoLinkUnderline in FOptions then
    Viewer.htOptions := Viewer.htOptions + [htNoLinkUnderline];
  if not (fvPrintTableBackground in FOptions) then
    Viewer.htOptions := Viewer.htOptions - [htPrintTableBackground];
  if (fvPrintBackground in FOptions) then
    Viewer.htOptions := Viewer.htOptions + [htPrintBackground];
  if not (fvPrintMonochromeBlack in FOptions) then
    Viewer.htOptions := Viewer.htOptions - [htPrintMonochromeBlack];
  if fvShowVScroll in FOptions then
    Viewer.htOptions := Viewer.htOptions + [htShowVScroll];
  if fvNoWheelMouse in FOptions then
    Viewer.htOptions := Viewer.htOptions + [htNoWheelMouse];
  if fvShowDummyCaret in FOptions then
    Viewer.htOptions := Viewer.htOptions + [htShowDummyCaret];
  if fvNoLinkHilite in FOptions then
    Viewer.htOptions := Viewer.htOptions + [htNoLinkHilite];
  Viewer.OnImageRequest := FOnImageRequest;
  Viewer.OnFormSubmit := FOnFormSubmit;
  Viewer.OnLink := FOnLink;
  Viewer.OnMeta := FOnMeta;
  Viewer.OnMetaRefresh := RefreshEvent;
  Viewer.OnRightClick := FOnRightClick;
  Viewer.OnProcessing := CheckProcessing;
  Viewer.OnMouseDown := OnMouseDown;
  Viewer.OnMouseMove := OnMouseMove;
  Viewer.OnMouseUp := OnMouseUp;
  Viewer.OnKeyDown := OnKeyDown;
  Viewer.OnKeyUp := OnKeyUp;
  Viewer.OnKeyPress := OnKeyPress;
  Viewer.Cursor := Cursor;
  Viewer.HistoryMaxCount := FHistoryMaxCount;
  Viewer.OnScript := FOnScript;
  Viewer.PrintMarginLeft := FPrintMarginLeft;
  Viewer.PrintMarginRight := FPrintMarginRight;
  Viewer.PrintMarginTop := FPrintMarginTop;
  Viewer.PrintMarginBottom := FPrintMarginBottom;
  Viewer.PrintScale := FPrintScale;
  Viewer.OnPrintHeader := FOnPrintHeader;
  Viewer.OnPrintFooter := FOnPrintFooter;
  Viewer.OnPrintHtmlHeader := FOnPrintHtmlHeader;
  Viewer.OnPrintHtmlFooter := FOnPrintHtmlFooter;
  Viewer.OnInclude := FOnInclude;
  Viewer.OnSoundRequest := FOnSoundRequest;
  Viewer.OnImageOver := FOnImageOver;
  Viewer.OnImageClick := FOnImageClick;
  Viewer.OnFileBrowse := FOnFileBrowse;
  Viewer.OnObjectClick := FOnObjectClick;
  Viewer.OnObjectFocus := FOnObjectFocus;
  Viewer.OnObjectBlur := FOnObjectBlur;
  Viewer.OnObjectChange := FOnObjectChange;
  Viewer.ServerRoot := ServerRoot;
  Viewer.OnMouseDouble := FOnMouseDouble;
  Viewer.OnPanelCreate := FOnPanelCreate;
  Viewer.OnPanelDestroy := FOnPanelDestroy;
  Viewer.OnPanelPrint := FOnPanelPrint;
  Viewer.OnDragDrop := fvDragDrop;
  Viewer.OnDragOver := fvDragOver;
  Viewer.OnParseBegin := FOnParseBegin;
  Viewer.OnParseEnd := FOnParseEnd;
  Viewer.OnProgress := FOnProgress;
  Viewer.OnObjectTag := OnObjectTag;
  if MasterSet.RequestEvent then
    Viewer.OnhtStreamRequest := DoURLRequest;
  end;
Viewer.MarginWidth := frMarginWidth;
Viewer.MarginHeight := frMarginHeight;
Viewer.OnEnter := MasterSet.CheckActive;
end;

{----------------TfvFrame.LoadFiles}
procedure TfvFrame.LoadFiles(PEV: PEventRec);
var
  Item: TFrameBase;
  I: integer;
  Upper, Lower, Image, Tex: boolean;
  Msg: string;
  EV: EventRec;
  Event: boolean;
begin
if ((Source <> '') or Assigned(PEV)) and (MasterSet.NestLevel < 4) then
  begin
  Image := ImageFile(Source) and not MasterSet.RequestEvent;
  Tex := TexFile(Source) and not MasterSet.RequestEvent;
  EV.LStyle := lsFile;
  if Image or Tex then
    EV.NewName := MasterSet.FrameViewer.HTMLExpandFilename(Source)
  else
    begin
    if Assigned(PEV) then
      begin
      Event := True;
      EV := PEV^;
      end
    else
      Event := MasterSet.TriggerEvent(Source, @EV);
    if not Event then
      EV.NewName := MasterSet.FrameViewer.HTMLExpandFilename(Source);
    end;
  Inc(MasterSet.NestLevel);
  try
    if not Image and not Tex and IsFrameString(EV.LStyle, EV.NewName, EV.AString, MasterSet.FrameViewer) then
      begin
      FrameSet := TSubFrameSet.CreateIt(Self, MasterSet);
      FrameSet.Align := alClient;
      FrameSet.Visible := False;
      InsertControl(FrameSet);
      FrameSet.SendToBack;
      FrameSet.Visible := True;
      FrameParseString(MasterSet.FrameViewer, FrameSet, EV.LStyle, EV.NewName, EV.AString,
               FrameSet.HandleMeta);
      Self.BevelOuter := bvNone;
      frBumpHistory1(Source, 0);
      with FrameSet do
        begin
        for I := 0 to List.Count-1 do
          Begin
          Item := TFrameBase(List.Items[I]);
          Item.LoadFiles(Nil);
          end;
        CheckNoresize(Lower, Upper);
        if FRefreshDelay > 0 then
          SetRefreshTimer;
        end;
      end
    else
      begin
      CreateViewer;
      Viewer.Base := MasterSet.FBase;
      if Image then
        Viewer.LoadImageFile(EV.NewName)
      else if Tex then
        Viewer.LoadTextFile(EV.NewName)
      else
        begin
        case EV.LStyle of
          lsFile: Viewer.LoadFromFile(EV.NewName+Destination);
          lsString:
            Viewer.LoadFromString(EV.AString, Source);
          end;
        if EV.LStyle <> lsFile then
          Viewer.PositionTo(Destination);
        end;
      frBumpHistory1(Source, Viewer.Position);
      end;
  except
    if not Assigned(Viewer) then
      CreateViewer;
    if Assigned(FrameSet) then
      begin
      FrameSet.Free;
      FrameSet := Nil;
      end;
    Msg := '<p><img src="qw%&.bmp" alt="Error"> Can''t load '+EV.NewName;
    Viewer.LoadFromBuffer(@Msg[1], Length(Msg), '');  {load an error message}
    end;
  Dec(MasterSet.NestLevel);
  end
else
  begin  {so blank area will perform like the TFrameViewer}
  OnMouseDown := MasterSet.FrameViewer.OnMouseDown;
  OnMouseMove := MasterSet.FrameViewer.OnMouseMove;
  OnMouseUp := MasterSet.FrameViewer.OnMouseUp;
  end;
end;

{----------------TfvFrame.ReloadFiles}
procedure TfvFrame.ReloadFiles(APosition: integer);
var
  Item: TFrameBase;
  I: integer;
  Upper, Lower: boolean;
  EV: EventRec;

  procedure DoError;
  var
    Msg: string;
  begin
  Msg := '<p><img src="qw%&.bmp" alt="Error"> Can''t load '+Source;
  Viewer.LoadFromBuffer(@Msg[1], Length(Msg), '');  {load an error message}
  end;

begin
if (Source <> '') then
  if Assigned(FrameSet) then
    begin
    with FrameSet do
      begin
      for I := 0 to List.Count-1 do
        Begin
        Item := TFrameBase(List.Items[I]);
        Item.ReloadFiles(APosition);
        end;
      CheckNoresize(Lower, Upper);
      end;
    end
  else if Assigned(Viewer) then
    begin
    Viewer.Base := MasterSet.FBase;
    if ImageFile(Source) then
      try
        Viewer.LoadImageFile(Source)
      except end   {leave blank on error}
    else if TexFile(Source) then
      try
        Viewer.LoadTextFile(Source)
      except end
    else
      begin
      try
        if MasterSet.TriggerEvent(Source, @EV) then
          case EV.LStyle of
            lsFile: Viewer.LoadFromFile(EV.NewName);
            lsString: Viewer.LoadFromString(EV.AString, '');
            end
          else
            Viewer.LoadFromFile(Source);
        if APosition < 0 then
          Viewer.Position := ViewerPosition
        else Viewer.Position := APosition;    {its History Position}
        Viewer.FormData := ViewerFormData;
        ViewerFormData.Free;
        ViewerFormData := Nil;
      except
        DoError;
        end;
      end;
    end;
Unloaded := False;
end;

{----------------TfvFrame.UnloadFiles}
procedure TfvFrame.UnloadFiles;
var
  Item: TFrameBase;
  I: integer;
begin
if Assigned(RefreshTimer) then
  RefreshTimer.Enabled := False;
if Assigned(FrameSet) then
  begin
  with FrameSet do
    begin
    for I := 0 to List.Count-1 do
      Begin
      Item := TFrameBase(List.Items[I]);
      Item.UnloadFiles;
      end;
    end;
  end
else if Assigned(Viewer) then
  begin
  ViewerPosition := Viewer.Position;
  ViewerFormData := Viewer.FormData;
  Viewer.Clear;
  if MasterSet.FActive = Viewer then
    MasterSet.FActive := Nil;
  Viewer.OnSoundRequest := Nil;
  end;
Unloaded := True;
end;

{----------------TfvFrame.frLoadFromFile}
procedure TfvFrame.frLoadFromFile(const FName, Dest: string; Bump, Reload: boolean);
{Note: if FName not '' and there is no RequestEvent, it has been HTML expanded
 and contains the path}
var
  OldPos: integer;
  HS, OldTitle, OldName: string;
  OldFormData: TFreeList;
  SameName, Tex, Img: boolean;
  OldViewer: ThtmlViewer;
  OldFrameSet: TSubFrameSet;
  EV: EventRec;
  Upper, Lower, FrameFile: boolean;
  Item: TFrameBase;
  I: integer;

begin
if Assigned(RefreshTimer) then RefreshTimer.Enabled := False;
OldName := Source;
EV.NewName := FName;
if EV.NewName = '' then EV.NewName := OldName;
Source := EV.NewName;
HS := EV.NewName;
SameName := CompareText(EV.NewName, OldName)= 0;
{if SameName, will not have to reload anything}
Img := ImageFile(EV.NewName) and not MasterSet.RequestEvent;
Tex := TexFile(EV.NewName) and not MasterSet.RequestEvent;
EV.LStyle := lsFile;
if not Img and not Tex and not SameName then
  MasterSet.TriggerEvent(EV.NewName, @EV);

try
  if not SameName then
    try
      FrameFile := not Img and not Tex and
           IsFrameString(EV.LStyle, EV.NewName, EV.AString, MasterSet.FrameViewer);
    except
      Raise(EfvLoadError.Create('Can''t load: '+EV.NewName));
      end
  else FrameFile := not Assigned(Viewer);
  if SameName then
    if Assigned(Viewer) then
      begin
      OldPos := Viewer.Position;
      if Reload then
        begin     {this for Meta Refresh only}
        case EV.LStyle of
          lsFile: Viewer.LoadFromFile(EV.NewName+Dest);
          lsString:
            Viewer.LoadFromString(EV.AString, '');
          end;
        Viewer.Position := OldPos;
        end
      else
        begin
        Viewer.PositionTo(Dest);
        if Bump and (Viewer.Position <> OldPos) then
          {Viewer to Viewer}
          frBumpHistory(HS, Viewer.Position, OldPos, Nil);
        end;
      MasterSet.FrameViewer.AddVisitedLink(EV.NewName+Dest);
      end
    else
      begin
      with FrameSet do
        begin
        for I := 0 to List.Count-1 do
          Begin
          Item := TFrameBase(List.Items[I]);
          if (Item is TfvFrame) then
            with TfvFrame(Item) do
              if CompareText(Source, OrigSource) <> 0 then
                begin
                frLoadFromFile(OrigSource, '', True, False);
                end;
          end;
        end;
      Exit;
      end
  else if Assigned(Viewer) and not FrameFile then   {not Same Name}
    begin  {Viewer already assigned and it's not a Frame file}
    OldPos := Viewer.Position;
    OldTitle := Viewer.DocumentTitle;
    OldFormData := Viewer.FormData;
    try
      if Img then Viewer.LoadImageFile(EV.NewName)
      else if Tex then Viewer.LoadTextFile(EV.NewName + Dest)
      else
        begin
        Viewer.Base := MasterSet.FBase;
        case EV.LStyle of
          lsFile: Viewer.LoadFromFile(EV.NewName+Dest);
          lsString:
            Viewer.LoadFromString(EV.AString, '');
          end;
        if (EV.LStyle <> lsFile) and (Dest <> '') then
          Viewer.PositionTo(Dest);
        end;
      MasterSet.FrameViewer.AddVisitedLink(EV.NewName+Dest);
      if MasterSet.Viewers.Count > 1 then
        begin
        if Bump then
           {Viewer to Viewer}
          frBumpHistory(HS, Viewer.Position, OldPos, OldFormData)
        else OldFormData.Free;
        end
      else OldFormData.Free;
    except
      OldFormData.Free;
      Raise;
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
    begin {Viewer is not assigned or it is a Frame File}   {not Same Name here either}
    {keep the old viewer or frameset around (free later) to minimize blink}
    OldViewer := Viewer;  Viewer := Nil;
    OldFrameSet := FrameSet;  FrameSet := Nil;
    if OldFrameSet <> Nil then OldFrameSet.ClearFrameNames;
    if not Img and not Tex and FrameFile then
      begin   {it's a frame file}
      FrameSet := TSubFrameSet.CreateIt(Self, MasterSet);
      FrameSet.Align := alClient;
      FrameSet.Visible := False;
      InsertControl(FrameSet);
      FrameSet.SendToBack;    {to prevent blink}
      FrameSet.Visible := True;
      FrameParseString(MasterSet.FrameViewer, FrameSet, EV.LStyle, EV.NewName,
          EV.AString, FrameSet.HandleMeta);
      MasterSet.FrameViewer.AddVisitedLink(EV.NewName);
      Self.BevelOuter := bvNone;
      with FrameSet do
        begin
        for I := 0 to List.Count-1 do
          Begin
          Item := TFrameBase(List.Items[I]);
          Item.LoadFiles(Nil);
          end;
        CheckNoresize(Lower, Upper);
        if FRefreshDelay > 0 then
          SetRefreshTimer;
        end;
      if Assigned(OldViewer) then
        frBumpHistory(HS, 0, OldViewer.Position, OldViewer.FormData)
      else frBumpHistory(EV.NewName, 0, 0, Nil);
      end
    else
      begin   {not a frame file but needs a viewer}
      CreateViewer;
      if Img then
        Viewer.LoadImageFile(EV.NewName)
      else if Tex then
        Viewer.LoadTextFile(EV.NewName)
      else
        begin
        Viewer.Base := MasterSet.FBase;
        case EV.LStyle of
          lsFile: Viewer.LoadFromFile(EV.NewName+Dest);
          lsString:
            Viewer.LoadFromString(EV.AString, '');
          end;
        if EV.LStyle <> lsFile then
          Viewer.PositionTo(Dest);
        end;
      MasterSet.FrameViewer.AddVisitedLink(EV.NewName+Dest);
      {FrameSet to Viewer}
      frBumpHistory(HS, Viewer.Position, 0, Nil);
      end;
    if Assigned(FrameSet) then
      with FrameSet do
        begin
        with ClientRect do
          InitializeDimensions(Left, Top, Right-Left, Bottom-Top);
        CalcSizes(Nil);
        end;
    if Assigned(Viewer) then
      begin
      if MasterSet.BorderSize = 0 then
        BevelOuter := bvNone
      else
        begin
        BevelOuter := bvLowered;
        BevelWidth := MasterSet.BorderSize;
        end;
      if (Dest <> '') then
        Viewer.PositionTo(Dest);
      end;
    if Assigned(OldViewer) then
      begin
      MasterSet.Viewers.Remove(OldViewer);
      if MasterSet.FActive = OldViewer then
        MasterSet.FActive := Nil;
      OldViewer.Free;
      end
    else if Assigned(OldFrameSet) then
      begin
      OldFrameSet.UnloadFiles;
      OldFrameSet.Visible := False;
      OldFrameSet.DestroyHandle;
      end;
    RePaint;
    end;
  except
    Source := OldName;
    Raise;
    end;
end;

{----------------TfvFrame.ReloadFile}
procedure TfvFrame.ReloadFile(const FName: string; APosition: integer);
{It's known that there is only a single viewer, the file is not being changed,
 only the position}
begin
Viewer.Position := APosition;
end;

{----------------TfvFrame.frBumpHistory}
procedure TfvFrame.frBumpHistory(const NewName: string;
              NewPos, OldPos: integer; OldFormData: TFreeList);
{applies to TFrames which hold a ThtmlViewer}{Viewer to Viewer}
var
  PO: PositionObj;
begin
with frHistory do
  begin
  if (Count > 0) then
    begin
    PositionObj(frPositionHistory[frHistoryIndex]).Pos := OldPos;
    if frHistory[frHistoryIndex] <> NewName then
      PositionObj(frPositionHistory[frHistoryIndex]).FormData := OldFormData
    else OldFormData.Free;
    end
  else OldFormData.Free;
  MasterSet.ClearForwards;   {clear the history list forwards}
  frHistoryIndex := 0;
  InsertObject(0, NewName, FrameSet);  {FrameSet may be Nil here}
  PO := PositionObj.Create;
  PO.Pos := NewPos;
  PO.Seq := Sequence;
  Inc(Sequence);
  frPositionHistory.Insert(0, PO);
  MasterSet.UpdateFrameList;
  with MasterSet.FrameViewer do
    if Assigned(FOnHistoryChange) then
      FOnHistoryChange(MasterSet.FrameViewer);
  end;
end;

{----------------TfvFrame.frBumpHistory1}
procedure TfvFrame.frBumpHistory1(const NewName: string; Pos: integer);
{called from a fresh TfvFrame.  History list is empty}
var
  PO: PositionObj;
begin
with frHistory do
  begin
  frHistoryIndex := 0;
  InsertObject(0, NewName, FrameSet);  {FrameSet may be Nil here}
  PO := PositionObj.Create;
  PO.Pos := Pos;
  PO.Seq := Sequence;
  Inc(Sequence);
  frPositionHistory.Insert(0, PO);
  MasterSet.UpdateFrameList;
  with MasterSet.FrameViewer do
    if Assigned(FOnHistoryChange) then
      FOnHistoryChange(MasterSet.FrameViewer);
  end;
end;

{----------------TfvFrame.frSetHistoryIndex}
procedure TfvFrame.frSetHistoryIndex(Value: integer);
begin
with frHistory do
  if (Value <> frHistoryIndex) and (Value >= 0) and (Value < Count) then
    begin
    if Assigned(RefreshTimer) then
      RefreshTimer.Enabled := False;    {cut off any timing underway}
    if Assigned(Viewer) then   {current is Viewer}
      with PositionObj(frPositionHistory[frHistoryIndex]) do
        begin
        Pos := Viewer.Position;   {save the old position}
        {note that frHistoryIndex can only change by 1}
        PositionObj(frPositionHistory[frHistoryIndex]).FormData := Viewer.FormData;
        end
    else
      begin    {Current is FrameSet}
      FrameSet.UnloadFiles;
      FrameSet.DestroyHandle;
      FrameSet.ClearFrameNames;
      FrameSet.Visible := False;
      FrameSet := Nil;   {it's not destroyed,though}
      end;

    if Objects[Value] is TSubFrameSet then
      begin
      FrameSet := TSubFrameSet(Objects[Value]);
      FrameSet.Visible := True;
      FrameSet.ReloadFiles(-1);
      FrameSet.AddFrameNames;
      if Assigned(Viewer) then
        begin
        if Assigned(MasterSet.Viewers) then
          MasterSet.Viewers.Remove(Viewer);
        if MasterSet.FActive = Viewer then
          MasterSet.FActive := Nil;
        Viewer.Free;
        Viewer := Nil;
        end;
      end
    else
      begin
      if not Assigned(Viewer) then
        CreateViewer;
      with PositionObj(frPositionHistory[Value]) do
        begin
        if (Source <> Strings[Value]) then
          begin
          frLoadFromFile(Strings[Value], '', False, False);
          Viewer.FormData := FormData;
          FormData.Free;
          FormData := Nil;
          end;
        Viewer.Position := Pos;
        end;
      end;
    Source := Strings[Value];
    frHistoryIndex := Value;
    MasterSet.UpdateFrameList;
    with MasterSet.FrameViewer do
      if Assigned(FOnHistoryChange) then
        FOnHistoryChange(MasterSet.FrameViewer);
    MasterSet.FrameViewer.CheckVisitedLinks;
    end;
end;

{----------------TfvFrame.UpdateFrameList}
procedure TfvFrame.UpdateFrameList;
begin
MasterSet.Frames.Add(Self);
if Assigned(FrameSet) then
  FrameSet.UpdateFrameList;
end;

{----------------TSubFrameSet.CreateIt}
constructor TSubFrameSet.CreateIt(AOwner: TComponent; Master: TFrameSet);
begin
inherited Create(AOwner);
MasterSet := Master;
{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
if AOwner is TFrameBase then
  LocalCharSet := TSubFrameset(AOwner).LocalCharSet;
{$endif}
OuterBorder := 0;   {no border for subframesets}
if Self <> Master then
  BorderSize := Master.BorderSize;
First := True;
List := TFreeList.Create;
OnResize := CalcSizes;
OnMouseDown := FVMouseDown;
OnMouseMove := FVMouseMove;
OnMouseUp := FVMouseUp;
{$ifdef delphi7_plus}
ParentBackground := False;
{$endif}
ParentColor := True;
end;

{----------------TSubFrameSet.ClearFrameNames}
procedure TSubFrameSet.ClearFrameNames;
var
  I, J: integer;
begin
for J := 0 to List.Count-1 do
  if (TFrameBase(List[J]) is TfvFrame) then
    begin
    with TfvFrame(List[J]) do
      if Assigned(MasterSet) and (WinName <> '')
            and Assigned(MasterSet.FrameNames)
              and MasterSet.FrameNames.Find(WinName, I) then
                MasterSet.FrameNames.Delete(I);
    end
  else if (TFrameBase(List[J]) is TSubFrameSet) then
    TSubFrameSet(List[J]).ClearFrameNames;
end;

{----------------TSubFrameSet.AddFrameNames}
procedure TSubFrameSet.AddFrameNames;
var
  J: integer;
  Frame: TfvFrame;
begin
for J := 0 to List.Count-1 do
  if (TFrameBase(List[J]) is TfvFrame) then
    begin
    Frame := TfvFrame(List[J]);
    with Frame do
      if Assigned(MasterSet) and (WinName <> '')
            and Assigned(MasterSet.FrameNames) then
              begin
              MasterSet.FrameNames.AddObject(Uppercase(WinName), Frame);
              end;
    end
  else if (TFrameBase(List[J]) is TSubFrameSet) then
    TSubFrameSet(List[J]).AddFrameNames;
end;

{----------------TSubFrameSet.Destroy}
destructor TSubFrameSet.Destroy;
begin
List.Free;
List := Nil;
RefreshTimer.Free;
inherited Destroy;
end;

{----------------TSubFrameSet.AddFrame}
function TSubFrameSet.AddFrame(Attr: TAttributeList; const FName: string): TfvFrame;
{called by the parser when <Frame> is encountered within the <Frameset>
 definition}
begin
Result := TfvFrame.CreateIt(Self, Attr, MasterSet, ExtractFilePath(FName));
List.Add(Result);
Result.SetBounds(OuterBorder, OuterBorder, Width-2*OuterBorder, Height-2*OuterBorder);
InsertControl(Result);
end;

{----------------TSubFrameSet.DoAttributes}
procedure TSubFrameSet.DoAttributes(L: TAttributeList);
{called by the parser to process the <Frameset> attributes}
var
  T: TAttribute;
  S: string;
  Numb: string;

  procedure GetDims;
  const
    EOL = ^M;
  var
    Ch: char;
    I, N: integer;

    procedure GetCh;
    begin
    if I > Length(S) then Ch := EOL
    else
      begin
      Ch := S[I];
      Inc(I);
      end;
    end;

  begin
  if Name = '' then S := T.Name
  else Exit;
  I := 1;   DimCount := 0;
  repeat
    Inc(DimCount);
    Numb := '';
    GetCh;
    while not (Ch in ['0'..'9', '*', EOL, ',']) do GetCh;
    if Ch in ['0'..'9'] then
      begin
      while Ch in ['0'..'9'] do
        begin
        Numb := Numb+Ch;
        GetCh;
        end;
      N := IntMax(1, StrToInt(Numb));   {no zeros}
      while not (Ch in ['*', '%', ',', EOL]) do GetCh;
      if ch = '*' then
        begin
        Dim[DimCount] := -IntMin(99, N);{store '*' relatives as negative, -1..-99}
        GetCh;
        end
      else if Ch = '%' then
        begin  {%'s stored as -(100 + %),  i.e. -110 is 10% }
        Dim[DimCount] := -IntMin(1000, N+100);  {limit to 900%}
        GetCh;
        end
      else Dim[DimCount] := IntMin(N, 5000);  {limit absolute to 5000}
      end
    else if Ch in ['*', ',', EOL] then
      begin
      Dim[DimCount] := -1;
      if Ch = '*' then GetCh;
      end;
    while not (Ch in [',', EOL]) do GetCh;
  until (Ch = EOL) or (DimCount = 20);
  end;

begin
{read the row or column widths into the Dim array}
If L.Find(RowsSy, T) then
  begin
  Rows := True;
  GetDims;
  end;
if L.Find(ColsSy, T) and (DimCount <=1) then
  begin
  Rows := False;
  DimCount := 0;
  GetDims;
  end;
if (Self = MasterSet) and not (fvNoBorder in MasterSet.FrameViewer.FOptions) then
                               {BorderSize already defined as 0}
  if L.Find(BorderSy, T) or L.Find(FrameBorderSy, T)then
    begin
    BorderSize := T.Value;
    OuterBorder := IntMax(2-BorderSize, 0);
    if OuterBorder >= 1 then
      begin
      BevelWidth := OuterBorder;
      BevelOuter := bvLowered;
      end;
    end
  else BorderSize := 2;
end;

{----------------TSubFrameSet.LoadFiles}
procedure TSubFrameSet.LoadFiles;
var
  I: integer;
  Item: TFrameBase;
begin
for I := 0 to List.Count-1 do
  begin
  Item := TFrameBase(List.Items[I]);
  Item.LoadFiles(Nil);
  end;
end;

{----------------TSubFrameSet.ReloadFiles}
procedure TSubFrameSet.ReloadFiles(APosition: integer);
var
  I: integer;
  Item: TFrameBase;
begin
for I := 0 to List.Count-1 do
  begin
  Item := TFrameBase(List.Items[I]);
  Item.ReloadFiles(APosition);
  end;
if (FRefreshDelay > 0) and Assigned(RefreshTimer) then
  SetRefreshTimer;
Unloaded := False;
end;

{----------------TSubFrameSet.UnloadFiles}
procedure TSubFrameSet.UnloadFiles;
var
  I: integer;
  Item: TFrameBase;
begin
if Assigned(RefreshTimer) then
  RefreshTimer.Enabled := False;
for I := 0 to List.Count-1 do
  begin
  Item := TFrameBase(List.Items[I]);
  Item.UnloadFiles;
  end;
if Assigned(MasterSet.FrameViewer.FOnSoundRequest) then
  MasterSet.FrameViewer.FOnSoundRequest(MasterSet, '', 0, True);
Unloaded := True;
end;

{----------------TSubFrameSet.EndFrameSet}
procedure TSubFrameSet.EndFrameSet;
{called by the parser when </FrameSet> is encountered}
var
  I: integer;
begin
if List.Count > DimCount then  {a value left out}
  begin  {fill in any blanks in Dim array}
  for I := DimCount+1 to List.Count do
    begin
    Dim[I] := -1;      {1 relative unit}
    Inc(DimCount);
    end;
  end
else while DimCount > List.Count do  {or add Frames if more Dims than Count}
  AddFrame(Nil, '');
if ReadHTML.Base <> '' then
  FBase := ReadHTML.Base
else FBase := MasterSet.FrameViewer.FBaseEx;
FBaseTarget := ReadHTML.BaseTarget;
end;

{----------------TSubFrameSet.InitializeDimensions}
procedure TSubFrameSet.InitializeDimensions(X, Y, Wid, Ht: integer);
var
  I, Total, PixTot, PctTot, RelTot, Rel, Sum,
  Remainder, PixDesired, PixActual: integer;

begin
if Rows then
  Total := Ht
else Total := Wid;
PixTot := 0;  RelTot := 0;  PctTot := 0; DimFTot := 0;
for I := 1 to DimCount do   {count up the total pixels, %'s and relatives}
  if Dim[I] >= 0 then
    PixTot := PixTot + Dim[I]
  else if Dim[I] <= -100 then
    PctTot :=  PctTot + (-Dim[I]-100)
  else RelTot := RelTot - Dim[I];
Remainder := Total - PixTot;
if Remainder <= 0 then
  begin    {% and Relative are 0, must scale absolutes}
  for I := 1 to DimCount do
    begin
    if Dim[I] >= 0 then
      DimF[I] := MulDiv(Dim[I], Total, PixTot)  {reduce to fit}
    else DimF[I] := 0;
    Inc(DimFTot, DimF[I]);
    end;
  end
else    {some remainder left for % and relative}
  begin
  PixDesired := MulDiv(Total, PctTot, 100);
  if PixDesired > Remainder then
    PixActual := Remainder
  else PixActual := PixDesired;
  Dec(Remainder, PixActual);   {Remainder will be >= 0}
  if RelTot > 0 then
    Rel := Remainder div RelTot  {calc each relative unit}
  else Rel := 0;
  for I := 1 to DimCount do  {calc the actual pixel widths (heights) in DimF}
    begin
    if Dim[I] >= 0 then
      DimF[I] := Dim[I]
    else if Dim[I] <= -100 then
      DimF[I] := MulDiv(-Dim[I]-100, PixActual, PctTot)
    else DimF[I] := -Dim[I] * Rel;
    Inc(DimFTot, DimF[I]);
    end;
  end;

Sum := 0;
for I := 0 to List.Count-1 do  {intialize the dimensions of contained items}
  begin
  if Rows then
    TFrameBase(List.Items[I]).InitializeDimensions(X, Y+Sum, Wid, DimF[I+1])
  else
    TFrameBase(List.Items[I]).InitializeDimensions(X+Sum, Y, DimF[I+1], Ht);
  Sum := Sum+DimF[I+1];
  end;
end;

{----------------TSubFrameSet.CalcSizes}
{OnResize event comes here}
procedure TSubFrameSet.CalcSizes(Sender: TObject);
var
  I, Step, Sum, ThisTotal: integer;
  ARect: TRect;
begin
{Note: this method gets called during Destroy as it's in the OnResize event.
 Hence List may be Nil.}
if Assigned(List) and (List.Count > 0) then
  begin
  ARect := ClientRect;
  InflateRect(ARect, -OuterBorder, -OuterBorder);
  Sum := 0;
  if Rows then ThisTotal := ARect.Bottom - ARect.Top
  else ThisTotal := ARect.Right-ARect.Left;
  for I := 0 to List.Count-1 do
    begin
    Step := MulDiv(DimF[I+1], ThisTotal, DimFTot);
    if Rows then
      TFrameBase(List.Items[I]).SetBounds(ARect.Left, ARect.Top+Sum, ARect.Right-ARect.Left, Step)
    else
      TFrameBase(List.Items[I]).SetBounds(ARect.Left+Sum, ARect.Top, Step, ARect.Bottom-Arect.Top);
    Sum := Sum+Step;
    Lines[I+1] := Sum;
    end;
  end;
end;

{----------------TSubFrameSet.NearBoundary}
function TSubFrameSet.NearBoundary(X, Y: integer): boolean;
begin
Result := (Abs(X) < 4) or (Abs(X - Width) < 4) or
             (Abs(Y) < 4) or (Abs(Y-Height) < 4);
end;

{----------------TSubFrameSet.GetRect}
function TSubFrameSet.GetRect: TRect;
{finds the FocusRect to draw when draging boundaries}
var
  Pt, Pt1, Pt2: TPoint;
begin
Pt1 := Point(0, 0);
Pt1 := ClientToScreen(Pt1);
Pt2 := Point(ClientWidth, ClientHeight);
Pt2 := ClientToScreen(Pt2);
GetCursorPos(Pt);
if Rows then
  Result := Rect(Pt1.X, Pt.Y-1, Pt2.X, Pt.Y+1)
else
  Result := Rect(Pt.X-1, Pt1.Y, Pt.X+1, Pt2.Y);
OldRect := Result;
end;

{----------------DrawRect}
procedure DrawRect(ARect: TRect);
{Draws a Focus Rect}
var
  DC: HDC;
begin
DC := GetDC(0);
DrawFocusRect(DC, ARect);
ReleaseDC(0, DC);
end;

{----------------TSubFrameSet.FVMouseDown}
procedure TSubFrameSet.FVMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
var
  ACursor: TCursor;
  RP: record
      case boolean of
        True: (P1, P2: TPoint);
        False:(R: TRect);
        end;
begin
if Button <> mbLeft then Exit;
if NearBoundary(X, Y) then
  begin
  if Parent is TFrameBase then
    (Parent as TFrameBase).FVMouseDown(Sender, Button, Shift, X+Left, Y+Top)
  else
    Exit;
  end
else
  begin
  ACursor := (Sender as TFrameBase).Cursor;
  if (ACursor = crVSplit) or(ACursor = crHSplit) then
    begin
    MasterSet.HotSet := Self;
    with RP do
      begin   {restrict cursor to lines on both sides}
      if Rows then
        R := Rect(0, Lines[LineIndex-1]+1, ClientWidth, Lines[LineIndex+1]-1)
      else
        R := Rect(Lines[LineIndex-1]+1, 0, Lines[LineIndex+1]-1, ClientHeight);
      P1 := ClientToScreen(P1);
      P2 := ClientToScreen(P2);
      ClipCursor(@R);
      end;
    DrawRect(GetRect);
    end;
  end;
end;

{----------------TSubFrameSet.FindLineAndCursor}
procedure TSubFrameSet.FindLineAndCursor(Sender: TObject; X, Y: integer);
var
  ACursor: TCursor;
  Gap, ThisGap, Line, I: integer;
begin
if  not Assigned(MasterSet.HotSet) then
  begin  {here we change the cursor as mouse moves over lines,button up or down}
  if Rows then Line := Y else Line := X;
  Gap := 9999;
  for I := 1 to DimCount-1 do
    begin
    ThisGap := Line-Lines[I];
    if Abs(ThisGap) < Abs(Gap) then
      begin
      Gap := Line - Lines[I];
      LineIndex := I;
      end
    else if Abs(ThisGap) = Abs(Gap) then  {happens if 2 lines in same spot}
      if ThisGap >= 0 then  {if Pos, pick the one on right (bottom)}
        LineIndex := I;
    end;

  if (Abs(Gap) <= 4) and not Fixed[LineIndex] then
    begin
    if Rows then
      ACursor := crVSplit
    else ACursor := crHSplit;
    (Sender as TFrameBase).Cursor := ACursor;
    end
  else (Sender as TFrameBase).Cursor := MasterSet.FrameViewer.Cursor;
  end
else
  with TSubFrameSet(MasterSet.HotSet) do
    begin
    DrawRect(OldRect);
    DrawRect(GetRect);
    end;
end;

{----------------TSubFrameSet.FVMouseMove}
procedure TSubFrameSet.FVMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
if NearBoundary(X, Y) then
  (Parent as TFrameBase).FVMouseMove(Sender, Shift, X+Left, Y+Top)
else
  FindLineAndCursor(Sender, X, Y);
end;

{----------------TSubFrameSet.FVMouseUp}
procedure TSubFrameSet.FVMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  I: integer;
begin
if Button <> mbLeft then Exit;
if MasterSet.HotSet = Self then
    begin
    MasterSet.HotSet := Nil;
    DrawRect(OldRect);
    ClipCursor(Nil);
    if Rows then
      Lines[LineIndex] := Y else Lines[LineIndex] := X;
    for I := 1 to DimCount do
      if I = 1 then DimF[1] := MulDiv(Lines[1], DimFTot, Lines[DimCount])
      else DimF[I] := MulDiv((Lines[I] - Lines[I-1]), DimFTot, Lines[DimCount]);
    CalcSizes(Self);
    Invalidate;
    end
else if (Parent is TFrameBase) then
  (Parent as TFrameBase).FVMouseUp(Sender, Button, Shift, X+Left, Y+Top);
end;

{----------------TSubFrameSet.CheckNoResize}
function TSubFrameSet.CheckNoResize(var Lower, Upper: boolean): boolean;
var
  Lw, Up: boolean;
  I: integer;
begin
Result := False; Lower := False;  Upper := False;
for I := 0 to List.Count-1 do
  with TFrameBase(List[I]) do
    if CheckNoResize(Lw, Up) then
      begin
      Result := True;  {sides are fixed}
      Fixed[I] := True;  {these edges are fixed}
      Fixed[I+1] := True;
      If Lw and (I = 0) then Lower := True;
      If Up and (I = List.Count-1) then Upper := True;
      end;
end;

{----------------TSubFrameSet.Clear}
procedure TSubFrameSet.Clear;
var
  I: integer;
  X: TFrameBase;
begin
for I := List.Count-1 downto 0 do
  begin
  X := List.Items[I];
  List.Delete(I);
  RemoveControl(X);
  X.Free;
  end;
DimCount := 0;
First := True;
Rows := False;
FillChar(Fixed, Sizeof(Fixed), 0);
FillChar(Lines, Sizeof(Lines), 0);
FBase := '';
FBaseTarget := '';
end;

{----------------TSubFrameSet.LoadFromFile}
procedure TSubFrameSet.LoadFromFile(const FName, Dest: string);
var
  Frame: TfvFrame;
begin
Clear;
Frame := AddFrame(Nil, '');
Frame.Source := FName;
Frame.Destination := Dest;
EndFrameSet;
Frame.LoadFiles(Nil);
if Assigned(Frame.FrameSet) then
  with Frame.FrameSet do
    begin
    with ClientRect do
      InitializeDimensions(Left, Top, Right-Left, Bottom-Top);
    CalcSizes(Nil);
    end
else if Assigned(Frame.Viewer) then
  Frame.Viewer.PositionTo(Dest);
MasterSet.FrameViewer.AddVisitedLink(FName+Dest);
end;

{----------------TSubFrameSet.UpdateFrameList}
procedure TSubFrameSet.UpdateFrameList;
var
  I: integer;
begin
for I := 0 to List.Count-1 do
  TFrameBase(List[I]).UpdateFrameList;
end;

{----------------TSubFrameSet.HandleMeta}
procedure TSubFrameSet.HandleMeta(Sender: TObject; const HttpEq, Name, Content: string);
var
  DelTime, I: integer;
begin
{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
if CompareText(HttpEq, 'content-type') = 0 then
  TranslateCharset(Content, LocalCharset);
{$endif}

with MasterSet.FrameViewer do
  begin
  if Assigned(FOnMeta) then FOnMeta(Sender, HttpEq, Name, Content);
  if not (fvMetaRefresh in FOptions) then Exit;
  end;

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
  else if Owner is TfvFrame then
    FRefreshURL := TfvFrame(Owner).Source
  else FRefreshURL := '';
  FRefreshDelay := DelTime;
  end;
end;

{----------------TSubFrameSet.SetRefreshTimer}
procedure TSubFrameSet.SetRefreshTimer;
begin
NextFile := HTMLToDos(FRefreshURL);
if not FileExists(NextFile) then
  Exit;
if not Assigned(RefreshTimer) then
  RefreshTimer := TTimer.Create(Self);
RefreshTimer.OnTimer := RefreshTimerTimer;
RefreshTimer.Interval := FRefreshDelay*1000;
RefreshTimer.Enabled := True;
end;

{----------------TSubFrameSet.RefreshTimerTimer}
procedure TSubFrameSet.RefreshTimerTimer(Sender: Tobject);
var
  S, D: string;
begin
RefreshTimer.Enabled := False;
if Unloaded then Exit;
if Owner is TfvFrame then
  begin
  SplitURL(NextFile, S, D);
  TfvFrame(Owner).frLoadFromFile(S, D, True, True);
  end;
end;

{----------------TFrameSet.Create}
constructor TFrameSet.Create(AOwner: TComponent);
begin
inherited CreateIt(AOwner, Self);
FrameViewer := AOwner as TFrameViewer;
{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
LocalCharSet := FrameViewer.FCharset;
{$endif}
if fvNoBorder in FrameViewer.FOptions then
  BorderSize := 0
else
  BorderSize := 2;
BevelOuter := bvNone;
FTitle := '';
FCurrentFile:= '';
FrameNames := TStringList.Create;
FrameNames.Sorted := True;
Viewers := TList.Create;
Frames := TList.Create;
OnResize := CalcSizes;
end;

{----------------TFrameSet.Destroy}
destructor TFrameSet.Destroy;
begin
FrameNames.Free;
FrameNames := Nil;   {is tested later}
Viewers.Free;
Viewers := Nil;
Frames.Free;
Frames := Nil;
inherited Destroy;
end;

{----------------TFrameSet.Clear}
procedure TFrameSet.Clear;
begin
inherited Clear;
FrameNames.Clear;
Viewers.Clear;
Frames.Clear;
HotSet := Nil;
FTitle := '';
FCurrentFile := '';
OldHeight := 0;
OldWidth := 0;
FActive := Nil;
end;

procedure TFrameSet.RePaint;
var
  I: integer;
begin
if Assigned(Frames) then
  for I := 0 to Frames.Count-1 do
    TWinControl(Frames[I]).RePaint;
inherited;
end;

{----------------TFrameSet.RequestEvent}
function TFrameSet.RequestEvent: boolean;
begin
with FrameViewer do
  Result := Assigned(FOnStringsRequest) or Assigned(FOnStreamRequest)
     or Assigned(FOnBufferRequest) or Assigned(FOnFileRequest);
end;

{----------------TFrameSet.TriggerEvent}
function TFrameSet.TriggerEvent(const Src: string; PEV: PEventRec): boolean;
var
  AName: string;
  Strings: TStrings;
  Stream: TStream;
  Buffer: PChar;
  BuffSize: integer;
begin
with PEV^ do
  begin
  Result := False;  LStyle := lsFile;
  Buffer := Nil; BuffSize := 0;
  AName := '';
  AString := '';
  Stream := Nil;
  with FrameViewer do
    if Assigned(FOnStringsRequest) then
      begin
      FOnStringsRequest(Self, Src, Strings);
      Result := Assigned(Strings);
      if Result then
        begin
        LStyle := lsString;
        AString := Strings.Text;
        end;
      end
    else if Assigned(FOnStreamRequest) then
      begin
      FOnStreamRequest(Self, Src, Stream);
      Result := Assigned(Stream);
      if Result then
        begin
        LStyle := lsString;
        AString := StreamToString(Stream);
        end;
      end
    else if Assigned(FOnBufferRequest) then
      begin
      FOnBufferRequest(Self, Src, Buffer, BuffSize);
      Result := (BuffSize > 0) and Assigned(Buffer);
      if Result then
        begin
        LStyle := lsString;
        SetLength(AString, BuffSize div SizeOf(Char));
        Move(Buffer^, AString[1], BuffSize * SizeOf(Char));
        end;
      end
    else if Assigned(FOnFileRequest) then
      begin
      FOnFileRequest(Self, Src, AName);
      Result := AName <> '';
      if Result then
        begin
        LStyle := lsFile;
        NewName := AName;
        AString := FileToString(NewName);
        end;
      end;
  end;
end;

{----------------TFrameSet.EndFrameSet}
procedure TFrameSet.EndFrameSet;
begin
FTitle := ReadHTML.Title;
inherited EndFrameSet;
with ClientRect do
  InitializeDimensions(Left, Top, Right-Left, Bottom-Top);
end;

{----------------TFrameSet.CalcSizes}
{OnResize event comes here}
procedure TFrameSet.CalcSizes(Sender: TObject);
var
  ARect: TRect;
begin
ARect := ClientRect;
InflateRect(ARect, -OuterBorder, -OuterBorder);
with ARect do
  begin
  if (OldWidth <> Right-Left) or (OldHeight <> Bottom-Top) then
    begin
    InitializeDimensions(Left, Top, Right-Left, Bottom-Top);
    inherited CalcSizes(Sender);
    end;
  OldWidth := Right-Left;
  OldHeight := Bottom-Top;
  end;
end;

{----------------TFrameSet.CheckActive}
procedure TFrameSet.CheckActive(Sender: TObject);
begin
if Sender is ThtmlViewer then
  FActive := ThtmlViewer(Sender);
end;

{----------------TFrameSet.GetActive}
function TFrameSet.GetActive: ThtmlViewer;
begin
if Viewers.Count = 1 then
  Result := ThtmlViewer(Viewers[0])
else
  try
    if FActive is ThtmlViewer then Result := FActive
      else Result := Nil;
  except
    Result := Nil;
  end;
end;

{----------------TFrameSet.FVMouseMove}
procedure TFrameSet.FVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
FindLineAndCursor(Sender, X, Y);
if (LineIndex = 0) or (LineIndex = DimCount) then
  begin    {picked up the outer boundary}
  (Sender as TFrameBase).Cursor := MasterSet.FrameViewer.Cursor;
  Cursor := MasterSet.FrameViewer.Cursor;
  end;
end;

{----------------TFrameSet.LoadFromFile}
procedure TFrameSet.LoadFromFile(const FName, Dest: string);
var
  I: integer;
  Item: TFrameBase;
  Frame: TfvFrame;
  Lower, Upper: boolean;
  EV: EventRec;
  EventPointer: PEventRec;
  Img, Tex: boolean;

begin
Clear;
NestLevel := 0;
EV.LStyle := lsFile;
Img := ImageFile(FName) and not RequestEvent;
Tex := TexFile(FName) and not RequestEvent;
if Img or Tex or
    not TriggerEvent(FName, @EV) then
  begin
  EV.NewName := ExpandFileName(FName);
  FCurrentFile := EV.NewName;
  end
else
  begin  {triggerevent}
  FCurrentFile := FName;
  end;
FRefreshDelay := 0;
if not Img and not Tex
       and IsFrameString(EV.LStyle, EV.NewName, EV.AString, MasterSet.FrameViewer) then
  begin    {it's a Frameset html file}
  FrameParseString(MasterSet.FrameViewer, Self, EV.LStyle, EV.NewName, EV.AString, HandleMeta);
  for I := 0 to List.Count-1 do
    Begin
    Item := TFrameBase(List.Items[I]);
    Item.LoadFiles(Nil);
    end;
  CalcSizes(Self);
  CheckNoresize(Lower, Upper);
  if FRefreshDelay > 0 then
    SetRefreshTimer;
  end
else
  begin   {it's a non frame file}
  Frame := AddFrame(Nil, '');
  if not Img and not Tex and RequestEvent then
    begin
    Frame.Source := FName;
    EventPointer := @EV;
    end
  else
    begin
    Frame.Source := EV.NewName;
    EventPointer := Nil;
    end;
  Frame.Destination := Dest;
  EndFrameSet;
  CalcSizes(Self);
  Frame.Loadfiles(EventPointer);
  FTitle := ReadHTML.Title;
  FBaseTarget := ReadHTML.BaseTarget;
  end;
end;

procedure TFrameSet.RefreshTimerTimer(Sender: Tobject);
begin
RefreshTimer.Enabled := False;
if (Self = MasterSet.FrameViewer.CurFrameSet) then
  FrameViewer.LoadFromFileInternal(NextFile);
end;

{----------------TFrameSet.ClearForwards}
procedure TFrameSet.ClearForwards;
{clear all the forward items in the history lists}
var
  I, J: integer;
  Frame: TfvFrame;
  AList: TList;
  Obj: TObject;
begin
AList := TList.Create;
for J := 0 to Frames.Count-1 do
  begin
  Frame := TfvFrame(Frames[J]);
  with Frame do
    begin
    for I := 0 to frHistoryIndex-1 do
      begin
      Obj := frHistory.Objects[0];
      if Assigned(Obj) and (AList.IndexOf(Obj) < 0) then
        AList.Add(Obj);
      frHistory.Delete(0);
      PositionObj(frPositionHistory[0]).Free;
      frPositionHistory.Delete(0);
      end;
    frHistoryIndex := 0;
    end;
  end;
for J := 0 to Frames.Count-1 do {now see which Objects are no longer used}
  begin
  Frame := TfvFrame(Frames[J]);
  with Frame do
    begin
    for I := 0 to frHistory.Count-1 do
      begin
      Obj := frHistory.Objects[I];
      if Assigned(Obj) and (AList.IndexOf(Obj) > -1) then
        AList.Remove(Obj);  {remove it if it's there}
      end;
    end;
  end;
for I := 0 to AList.Count-1 do   {destroy what's left}
  TObject(AList[I]).Free;
AList.Free;
end;

{----------------TFrameSet.UpdateFrameList}
procedure TFrameSet.UpdateFrameList;
{Fill Frames with a list of all current TFrames}
begin
Frames.Clear;
inherited UpdateFrameList;
end;

{----------------TFrameViewer.Create}
constructor TFrameViewer.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
Height := 150;
Width := 150;
ProcessList := TList.Create;
FLinkAttributes := TStringList.Create;
FViewImages := True;
FBitmapList := TStringBitmapList.Create;
FImageCacheCount := 5;
FHistory := TStringList.Create;
FPosition := TList.Create;
FTitleHistory := TStringList.Create;
FBackground := clBtnFace;
FFontColor := clBtnText;
FHotSpotColor := clBlue;
FVisitedColor := clPurple;
FOverColor := clBlue;
FVisitedMaxCount := 50;
FFontSize := 12;
FFontName := 'Times New Roman';
FPreFontName := 'Courier New';
FCursor := crIBeam;
FDither := True;
TabStop := False;
FPrintMarginLeft := 2.0;
FPrintMarginRight := 2.0;
FPrintMarginTop := 2.0;
FPrintMarginBottom := 2.0;
FPrintScale := 1.0;
FMarginWidth := 10;
FMarginHeight := 5;
FOptions := [fvPrintTableBackground, fvPrintMonochromeBlack];

{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
FCharset := DEFAULT_CHARSET;
{$endif}
Visited := TStringList.Create;

CurFrameSet := TFrameSet.Create(Self);
if fvNoBorder in FOptions then
  begin
  CurFrameSet.OuterBorder := 0;
  CurFrameSet.BevelOuter := bvNone;
  end
else
  begin
  CurFrameSet.OuterBorder := 2;
  CurFrameSet.BevelWidth := 2;
  CurFrameSet.BevelOuter := bvLowered;
  end;
CurFrameSet.Align := alClient;
CurFrameSet.OnDragDrop := FOnDragDrop;
CurFrameSet.OnDragOver := FOnDragOver;
InsertControl(CurFrameSet);
end;

{----------------TFrameViewer.Destroy}
destructor TFrameViewer.Destroy;
begin
UrlRequestStream.Free;
ProcessList.Free;
FLinkAttributes.Free;
FHistory.Free;
FPosition.Free;
FTitleHistory.Free;
Visited.Free;
FViewerList.Free;
inherited Destroy;
FBitmapList.Free;
end;

{----------------TFrameViewer.Clear}
procedure TFrameViewer.Clear;
var
  I: integer;
  Obj: TObject;
begin
if not Processing then
  begin
  for I := 0 to FHistory.Count-1 do
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
    BevelOuter := bvLowered;
    BevelWidth := 2;
    end;
  FBitmapList.Clear;
  FURL := '';
  FTarget := '';
  FBaseEx := '';
  FHistoryIndex := 0;
  FPosition.Clear;
  FTitleHistory.Clear;
  if Assigned(FOnHistoryChange) then
    FOnHistoryChange(Self);
  Visited.Clear;
  if Assigned(FViewerList) then
    FViewerList.Clear;
  end;
end;

{----------------TFrameViewer.LoadFromFile}
procedure TFrameViewer.LoadFromFile(const FName: string);
var
  S, Dest: string;
begin
if not Processing then
  begin
  SplitURL(FName, S, Dest);
  if not FileExists(S) then
    Raise(EfvLoadError.Create('Can''t locate file: '+S));
  LoadFromFileInternal(FName);
  end;
end;

{----------------TFrameViewer.LoadFromFileInternal}
procedure TFrameViewer.LoadFromFileInternal(const FName: string);
var
  OldFrameSet: TFrameSet;
  OldFile, S, Dest: string;
  OldPos: integer;
  Tmp: TObject;
  SameName: boolean;
  {$ifdef Windows}
  Dummy: integer;
  {$endif}
begin
FProcessing := True;
if Assigned(FOnProcessing) then
  FOnProcessing(Self, True);
{$ifdef windows}
Dummy :=
{$endif}
IOResult;     {remove any pending file errors}
SplitURL(FName, S, Dest);
try
  OldFile := CurFrameSet.FCurrentFile;
  ProcessList.Clear;
  if Assigned(FOnSoundRequest) then
    FOnSoundRequest(Self, '', 0, True);
  SameName := CompareText(OldFile, S) = 0;
  if not SameName then
    begin
    OldFrameSet := CurFrameSet;
    CurFrameSet := TFrameSet.Create(Self);
    CurFrameSet.Align := alClient;
    CurFrameSet.visible := False;
    InsertControl(CurFrameSet);
    CurFrameSet.SendToBack;
    CurFrameSet.Visible := True;

    try
      CurFrameSet.LoadFromFile(S, Dest);
    except
      RemoveControl(CurFrameSet);
      CurFrameSet.Free;
      CurFrameSet := OldFrameSet;
      Raise;
      end;

    OldPos := 0;
    if (OldFrameSet.Viewers.Count = 1) then
      begin
      Tmp := OldFrameSet.Viewers[0];
      if Tmp is ThtmlViewer then
        OldPos := ThtmlViewer(Tmp).Position;
      end;
    OldFrameSet.UnloadFiles;
    CurFrameSet.Visible := True;
    if Visible then
      begin
      SendMessage(Handle, wm_SetRedraw, 0, 0);
      try
        CurFrameSet.BringToFront;
      finally
        SendMessage(Handle, wm_SetRedraw, 1, 0);
        Repaint;
        end;
      CurFrameSet.Repaint;
      end;
    RemoveControl(OldFrameSet);
    BumpHistory(OldFrameSet, OldPos);
    end
  else
    begin     {Same Name}
    OldPos := 0;
    if (CurFrameSet.Viewers.Count = 1) then
      begin
      Tmp := CurFrameSet.Viewers[0];
      if Tmp is ThtmlViewer then
        OldPos := ThtmlViewer(Tmp).Position;
      end;
    SendMessage(Handle, wm_SetRedraw, 0, 0);
    try
      CurFrameSet.LoadFromFile(S, Dest);
    finally
      SendMessage(Handle, wm_SetRedraw, 1, 0);
      Repaint;
      end;
    BumpHistory2(OldPos);   {not executed if exception occurs}
    end;
  AddVisitedLink(S+Dest);
  CheckVisitedLinks;
finally
  FProcessing := False;
  if Assigned(FOnProcessing) then
    FOnProcessing(Self, False);
  end;
end;

{----------------TFrameViewer.Load}
procedure TFrameViewer.Load(const SRC: string);
begin
if Assigned(FOnStringsRequest) or Assigned(FOnStreamRequest)
     or Assigned(FOnBufferRequest) or Assigned(FOnFileRequest) then
  LoadFromFileInternal(SRC);
end;

{----------------TFrameViewer.LoadTargetFromFile}
procedure TFrameViewer.LoadTargetFromFile(const Target, FName: string);
var
  I: integer;
  FrameTarget: TFrameBase;
  S, Dest: string;

begin
if Processing then Exit;

if CurFrameSet.FrameNames.Find(Target, I) then
  FrameTarget := (CurFrameSet.FrameNames.Objects[I] as TfvFrame)
else if (Target = '') or (CompareText(Target, '_top') = 0) or
   (CompareText(Target, '_parent') = 0) or (CompareText(Target, '_self') = 0)then
      begin
      LoadFromFileInternal(Fname);
      Exit;
      end
else
  begin  {_blank or unknown target}
  if Assigned(FOnBlankWindowRequest) then
    FOnBlankWindowRequest(Self, Target, FName);
  Exit;
  end;

SplitURL(FName, S, Dest);

if not FileExists(S) and not Assigned(OnStreamRequest) then
  Raise(EfvLoadError.Create('Can''t locate file: '+S));

FProcessing := True;
if Assigned(FOnProcessing) then
  FOnProcessing(Self, True);

try
  if FrameTarget is TfvFrame then
    TfvFrame(FrameTarget).frLoadFromFile(S, Dest, True, False)
  else if FrameTarget is TSubFrameSet then
    TSubFrameSet(FrameTarget).LoadFromFile(S, Dest);
finally
  if Assigned(FOnProcessing) then
    FOnProcessing(Self, False);
  FProcessing := False;
  end;
end;

{----------------TFrameViewer.LoadImageFile}
procedure TFrameViewer.LoadImageFile(const FName: string);
begin
if ImageFile(FName) then
  LoadFromFile(FName);
end;

{----------------TFrameViewer.Reload}
procedure TFrameViewer.Reload;
begin
FProcessing := True;
if Assigned(FOnProcessing) then
  FOnProcessing(Self, True);
try
  ProcessList.Clear;
  CurFrameSet.UnloadFiles;
  CurFrameSet.ReloadFiles(-1);
  CheckVisitedLinks;
finally
  FProcessing := False;
  if Assigned(FOnProcessing) then
    FOnProcessing(Self, False);
  end;
end;

{----------------TFrameViewer.GetFwdButtonEnabled}
function TFrameViewer.GetFwdButtonEnabled: boolean;
var
  I: integer;
  Frame: TfvFrame;
begin
Result := fHistoryIndex >= 1;
if not Result then
  for I := 0 to CurFrameSet.Frames.Count-1 do
    begin
    Frame := TfvFrame(CurFrameSet.Frames[I]);
    with Frame do
      if frHistoryIndex >= 1 then
        begin
        Result := True;
        Exit;
        end;
    end;
end;

{----------------TFrameViewer.GetBackButtonEnabled}
function TFrameViewer.GetBackButtonEnabled: boolean;
var
  I: integer;
  Frame: TfvFrame;
begin
Result := fHistoryIndex <= fHistory.Count-2;
if not Result then
  for I := 0 to CurFrameSet.Frames.Count-1 do
    begin
    Frame := TfvFrame(CurFrameSet.Frames[I]);
    with Frame do
      if frHistoryIndex <= frHistory.Count-2 then
        begin
        Result := True;
        Exit;
        end;
    end;
end;

procedure TFrameViewer.GoFwd;
var
  I, Smallest, Index: integer;
  Frame, TheFrame: TfvFrame;
begin
Smallest := 9999;
Index := 0; TheFrame := Nil;   {to quiet the warnings}
for I := 0 to CurFrameSet.Frames.Count-1 do
  begin
  Frame := TfvFrame(CurFrameSet.Frames[I]);
  with Frame do
    if frHistoryIndex >= 1 then
      with PositionObj(frPositionHistory[frHistoryIndex-1]) do
        if Seq < Smallest then
          begin
          Smallest := Seq;
          TheFrame := Frame;
          Index := frHistoryIndex;
          end;
  end;
if Smallest < 9999 then
  TheFrame.frSetHistoryIndex(Index - 1)
else SetHistoryIndex(fHistoryIndex - 1);
if Assigned(FOnSoundRequest) then
  FOnSoundRequest(Self, '', 0, True);
end;

procedure TFrameViewer.GoBack;
var
  I, Largest, Index: integer;
  Frame, TheFrame: TfvFrame;
begin
Largest := -1;
Index := 0; TheFrame := Nil;   {to quiet the warnings}
for I := 0 to CurFrameSet.Frames.Count-1 do
  begin
  Frame := TfvFrame(CurFrameSet.Frames[I]);
  with Frame do
    if frHistoryIndex <= frHistory.Count-2 then
      with PositionObj(frPositionHistory[frHistoryIndex]) do
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
  SetHistoryIndex(fHistoryIndex+1);
if Assigned(FOnSoundRequest) then
  FOnSoundRequest(Self, '', 0, True);
end;

{----------------TFrameViewer.HotSpotClickHandled:}
function TFrameViewer.HotSpotClickHandled: boolean;
var
  Handled: boolean;
begin
Handled := False;
if Assigned(FOnHotSpotTargetClick) then
  FOnHotSpotTargetClick(Self, FTarget, FURL, Handled);
Result := Handled;
end;

{----------------TFrameViewer.HotSpotClick}
procedure TFrameViewer.HotSpotClick(Sender: TObject; const AnURL: string;
          var Handled: boolean);
var
  I: integer;
  Viewer: ThtmlViewer;
  FrameTarget: TFrameBase;
  S, Dest, Query: string;

begin
if Processing then
  begin
  Handled := True;
  Exit;
  end;

Viewer := (Sender as ThtmlViewer);
FURL := AnURL;
FTarget := GetActiveTarget;
FLinkAttributes.Text := Viewer.LinkAttributes.Text;
FLinkText := Viewer.LinkText;
Handled := HotSpotClickHandled;
if not Handled then
  begin
  Handled := True;

  S := AnURL;
  I := Pos('#', S);
  if I >= 1 then
    begin
    Dest := System.Copy(S, I, Length(S)-I+1);  {local destination}
    S := System.Copy(S, 1, I-1);     {the file name}
    end
  else
    Dest := '';    {no local destination}

  I := Pos('?', S);      {remove any query string}
  if I >= 1 then
    begin
    Query := System.Copy(S, I, Length(S)-I+1);
    S := System.Copy(S, 1, I-1);     {the file name}
    end
  else Query := '';

  if (S <> '') and not CurFrameSet.RequestEvent then
    S := Viewer.HTMLExpandFileName(S);

  if (FTarget = '') or (CompareText(FTarget, '_self') = 0) then  {no target or _self target}
    begin
    FrameTarget := Viewer.FrameOwner as TfvFrame;
    if not Assigned(FrameTarget) then Exit;
    end
  else if CurFrameSet.FrameNames.Find(FTarget, I) then
    FrameTarget := (CurFrameSet.FrameNames.Objects[I] as TfvFrame)
  else if CompareText(FTarget, '_top') = 0 then
    FrameTarget := CurFrameSet
  else if CompareText(FTarget, '_parent') = 0 then
    begin
    FrameTarget := (Viewer.FrameOwner as TfvFrame).Owner as TFrameBase;
    while Assigned(FrameTarget) and not (FrameTarget is TfvFrame)
              and not (FrameTarget is TFrameSet) do
       FrameTarget := FrameTarget.Owner as TFrameBase;
    end
  else
    begin
    if Assigned(FOnBlankWindowRequest) then
      begin
      AddVisitedLink(S+Query+Dest);
      CheckVisitedLinks;
      FOnBlankWindowRequest(Self, FTarget, AnURL);
      Handled := True;
      end
    else Handled := FTarget <> '';   {true if can't find target window}
    Exit;
    end;
  FProcessing := True;
  if Assigned(FOnProcessing) then
    FOnProcessing(Self, True);
  if (FrameTarget is TfvFrame) and (CurFrameSet.Viewers.Count = 1) and (S <> '')
        and (CompareText(S, CurFrameSet.FCurrentFile) <> 0) then
    FrameTarget := CurFrameSet;  {force a new FrameSet on name change}
  try
    if FrameTarget is TfvFrame then
      TfvFrame(FrameTarget).frLoadFromFile(S, Dest, True, False)
    else if FrameTarget is TFrameSet then
      Self.LoadFromFileInternal(S + Dest)
    else if FrameTarget is TSubFrameSet then
      TSubFrameSet(FrameTarget).LoadFromFile(S, Dest);
    if Query <> '' then
      AddVisitedLink(S+Query+Dest);
    CheckVisitedLinks;
  finally
    FProcessing := False;     {changed position}
    if Assigned(FOnProcessing) then
      FOnProcessing(Self, False);
    end;
  end;
end;

function TFrameViewer.GetCurViewerCount: integer;
begin
   Result := CurFrameSet.Viewers.Count;
end;

function TFrameViewer.GetCurViewer(I: integer): ThtmlViewer;
begin
   Result := CurFrameSet.Viewers[I];
end;

{----------------TFrameViewer.HotSpotCovered}
procedure TFrameViewer.HotSpotCovered(Sender: TObject; const SRC: string);
begin
if Assigned(FOnHotSpotTargetCovered) then
  with (Sender as ThtmlViewer) do
    begin
    FLinkText := LinkText;
    FLinkAttributes.Text := LinkAttributes.Text;
    FOnHotSpotTargetCovered(Sender, Target, Src);
    end;
end;

{----------------TFrameViewer.GetActiveTarget}
function TFrameViewer.GetActiveTarget: string;
var
   Vw: ThtmlViewer;
   Done: boolean;
   FSet: TSubFrameSet;
begin
Result := '';
Vw := GetActiveViewer;
if Assigned(Vw) then
  begin
  Result := Vw.Target;
  if Result = '' then Result := Vw.BaseTarget;
  Done := False;
  FSet := TfvFrame(Vw.FrameOwner).LOwner;
  while (Result = '') and Assigned(FSet) and not Done do
    begin
    Result := FSet.FBaseTarget;
    Done := FSet = CurFrameSet;
    if not Done then FSet := FSet.LOwner;
    end;
  end;
end;

{----------------TFrameViewer.GetActiveBase}
function TFrameViewer.GetActiveBase: string;
var
   Vw: ThtmlViewer;
   Done: boolean;
   FSet: TSubFrameSet;
begin
Result := '';
Vw := GetActiveViewer;
if Assigned(Vw) then
  begin
  Result := Vw.Base;
  Done := False;
  FSet := TfvFrame(Vw.FrameOwner).LOwner;
  while (Result = '') and Assigned(FSet) and not Done do
    begin
    Result := FSet.FBase;
    Done := FSet = CurFrameSet;
    if not Done then FSet := FSet.LOwner;
    end;
  end;
end;

{----------------TFrameViewer.HTMLExpandFilename}
function TFrameViewer.HTMLExpandFilename(const Filename: string): string;
var
  BasePath: string;
  Viewer: ThtmlViewer;
begin
Result := HTMLServerToDos(Trim(Filename), FServerRoot);
if (Pos(':', Result)<> 2) and (Pos('\\', Result) <> 1) then
  begin
  BasePath := GetActiveBase;
  if CompareText(BasePath, 'DosPath') = 0 then  {let Dos find the path}
  else
    begin
    if BasePath <> '' then
      Result := HTMLToDos(BasePath) + Result
    else
      begin
      Viewer := ActiveViewer;
      if Assigned(Viewer) then
        Result := Viewer.HTMLExpandFilename(Result)
      else
        Result := ExtractFilePath(CurFrameSet.FCurrentFile) + Result;
      end;
    end;
  end;
end;

function TFrameViewer.GetBase: string;
begin
Result := CurFrameSet.FBase;
end;

procedure TFrameViewer.SetBase(Value: string);
begin
CurFrameSet.FBase := Value;
FBaseEx := Value;
end;

function TFrameViewer.GetBaseTarget: string;
begin
Result := CurFrameSet.FBaseTarget;
end;

function TFrameViewer.GetTitle: string;
begin
Result := CurFrameSet.FTitle;
end;

function TFrameViewer.GetCurrentFile: string;
begin
Result := CurFrameSet.FCurrentFile;
end;

{----------------TFrameViewer.GetActiveViewer}
function TFrameViewer.GetActiveViewer: ThtmlViewer;
begin
Result := CurFrameSet.GetActive;
end;

{----------------TFrameViewer.BumpHistory}
procedure TFrameViewer.BumpHistory(OldFrameSet: TFrameSet; OldPos: integer);
{OldFrameSet never equals CurFrameSet when this method called}
var
  I: integer;
  Obj: TObject;
begin
if (FHistoryMaxCount > 0) and (CurFrameSet.FCurrentFile <> '') then
  with FHistory do
    begin
    if (Count > 0) then
      begin
      Strings[FHistoryIndex] := OldFrameSet.FCurrentFile;
      Objects[FHistoryIndex] := OldFrameSet;
      FTitleHistory[FHistoryIndex] := OldFrameSet.FTitle;
      FPosition[FHistoryIndex] := TObject(OldPos);
      OldFrameSet.ClearForwards;
      end
    else OldFrameSet.Free;
    for I := 0 to FHistoryIndex-1 do
      begin
      Obj := Objects[0];
      Delete(0);
      ChkFree(Obj);
      FTitleHistory.Delete(0);
      FPosition.Delete(0);
      end;
    FHistoryIndex := 0;
    Insert(0, CurFrameSet.FCurrentFile);
    Objects[0] := CurFrameSet;
    FTitleHistory.Insert(0, CurFrameSet.FTitle);
    FPosition.Insert(0, Nil);
    if Count > FHistoryMaxCount then
      begin
      Obj := Objects[FHistoryMaxCount];
      Delete(FHistoryMaxCount);
      ChkFree(Obj);
      FTitleHistory.Delete(FHistoryMaxCount);
      FPosition.Delete(FHistoryMaxCount);
      end;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end
else OldFrameSet.Free;
end;

{----------------TFrameViewer.BumpHistory1}
procedure TFrameViewer.BumpHistory1(const FileName, Title: string;
                 OldPos: integer; ft: ThtmlFileType);
{This variation called when CurFrameSet contains only a single viewer before
 and after the change}
var
  I: integer;
  Obj: TObject;
begin
if (FHistoryMaxCount > 0) and (Filename <> '') then
  with FHistory do
    begin
    if (Count > 0) then
      begin
      Strings[FHistoryIndex] := Filename;
      Objects[FHistoryIndex] := CurFrameSet;
      FTitleHistory[FHistoryIndex] := Title;
      FPosition[FHistoryIndex] := TObject(OldPos);
      end;
    for I := 0 to FHistoryIndex-1 do
      begin
      Obj := Objects[0];
      Delete(0);
      ChkFree(Obj);
      FTitleHistory.Delete(0);
      FPosition.Delete(0);
      end;
    FHistoryIndex := 0;
    Insert(0, CurFrameSet.FCurrentFile);
    Objects[0] := CurFrameSet;
    FTitleHistory.Insert(0, CurFrameSet.FTitle);
    FPosition.Insert(0, Nil);
    if Count > FHistoryMaxCount then
      begin
      Obj := Objects[FHistoryMaxCount];
      Delete(FHistoryMaxCount);
      ChkFree(Obj);
      FTitleHistory.Delete(FHistoryMaxCount);
      FPosition.Delete(FHistoryMaxCount);
      end;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

{----------------TFrameViewer.BumpHistory2}
procedure TFrameViewer.BumpHistory2(OldPos: integer);
{CurFrameSet has not changed when this method called}
var
  I: integer;
  Obj: TObject;
begin
if (FHistoryMaxCount > 0) and (CurFrameSet.FCurrentFile <> '') then
  with FHistory do
    begin
    if (Count > 0) then
      begin
      Strings[FHistoryIndex] := CurFrameSet.FCurrentFile;
      Objects[FHistoryIndex] := CurFrameSet;
      FTitleHistory[FHistoryIndex] := CurFrameSet.FTitle;
      FPosition[FHistoryIndex] := TObject(OldPos);
      end;
    for I := 0 to FHistoryIndex-1 do
      begin
      Obj := Objects[0];
      Delete(0);
      ChkFree(Obj);
      FTitleHistory.Delete(0);
      FPosition.Delete(0);
      end;
    FHistoryIndex := 0;
    Insert(0, CurFrameSet.FCurrentFile);
    Objects[0] := CurFrameSet;
    FTitleHistory.Insert(0, CurFrameSet.FTitle);
    FPosition.Insert(0, Nil);
    if Count > FHistoryMaxCount then
      begin
      Obj := Objects[FHistoryMaxCount];
      Delete(FHistoryMaxCount);
      ChkFree(Obj);
      FTitleHistory.Delete(FHistoryMaxCount);
      FPosition.Delete(FHistoryMaxCount);
      end;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

{----------------TFrameViewer.SetHistoryIndex}
procedure TFrameViewer.SetHistoryIndex(Value: integer);
var
  FrameSet, FrameSet1: TFrameSet;
  Tmp: TObject;
begin
with CurFrameSet, FHistory do
  if (Value <> FHistoryIndex) and (Value >= 0) and (Value < Count)
            and not Processing then
    begin
    if CurFrameSet.Viewers.Count > 0 then
      Tmp := CurFrameSet.Viewers[0]
    else Tmp := Nil;
    if FCurrentFile <> '' then
      begin
      {Objects[FHistoryIndex] should have CurFrameSet here}
      FTitleHistory[FHistoryIndex] := CurFrameSet.FTitle;
      if (Tmp is ThtmlViewer) then
        FPosition[FHistoryIndex] := TObject((Tmp as ThtmlViewer).Position)
      else  FPosition[FHistoryIndex] := Nil;
      end;
    FrameSet := Objects[Value] as TFrameSet;
    if FrameSet <> CurFrameSet then
      begin
      FrameSet1 := CurFrameSet;   {swap framesets}
      CurFrameSet := FrameSet;
      CurFrameSet.OldWidth := 0;    {encourage recalc of internal layout}
      CurFrameSet.Visible := False;
      Self.InsertControl(CurFrameSet);
      if CurFrameSet.Viewers.Count = 1 then
        CurFrameSet.ReloadFiles(integer(FPosition[Value]))
      else
        CurFrameSet.ReloadFiles(-1);
      SendMessage(Self.handle, wm_SetRedraw, 0, 0);
      CurFrameSet.Visible := True;
      SendMessage(Self.handle, wm_SetRedraw, 1, 0);
      CurFrameSet.Repaint;
      FrameSet1.Unloadfiles;
      Self.RemoveControl(FrameSet1);
      end
    else
      begin
      if  (Tmp is ThtmlViewer) then
        TfvFrame(ThtmlViewer(Tmp).FrameOwner).ReloadFile(FHistory[Value],
                          integer(FPosition[Value]));
      end;

    FHistoryIndex := Value;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    CheckVisitedLinks;
    end;
end;

{----------------TFrameViewer.ChkFree}
procedure TFrameViewer.ChkFree(Obj: TObject);
{Frees a TFrameSet only if it no longer exists in FHistory}
var
  I: integer;
begin
for I := 0 to FHistory.Count-1 do
  if Obj = FHistory.Objects[I] then Exit;
(Obj as TFrameSet).Free;
end;

{----------------TFrameViewer.ClearHistory}
procedure TFrameViewer.ClearHistory;
var
  I: integer;
  Obj: TObject;
  DidSomething: boolean;
begin
DidSomething := FHistory.Count > 0;
for I := FHistory.Count-1 downto 0 do
  begin
  Obj := FHistory.Objects[I];
  FHistory.Delete(I);
  if Obj <> CurFrameSet then
    ChkFree(Obj);
  end;
if Assigned(CurFrameSet) then
  for I := 0 to CurFrameSet.Frames.Count-1 do
    with TfvFrame(CurFrameSet.Frames[I]) do
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
if DidSomething and Assigned(FOnHistoryChange) then
  FOnHistoryChange(Self);
end;

procedure TFrameViewer.SetOnFormSubmit(Handler: TFormSubmitEvent);
var
  I: integer;
begin
FOnFormSubmit := Handler;
with CurFrameSet do
  for I := 0 to Viewers.Count-1 do
    with ThtmlViewer(Viewers[I]) do
      OnFormSubmit := Handler;
end;

procedure TFrameViewer.SetOnProgress(Handler: ThtProgressEvent);
var
  I: integer;
begin
FOnProgress := Handler;
with CurFrameSet do
  for I := 0 to Viewers.Count-1 do
    with ThtmlViewer(Viewers[I]) do
      OnProgress := Handler;
end;

procedure TFrameViewer.SetDragDrop(const Value: TDragDropEvent);
var
  I: integer;
begin
FOnDragDrop := Value;
if Assigned(CurFrameSet) then
  if Assigned(Value) then
    CurFrameSet.OnDragDrop := fvDragDrop
  else CurFrameSet.OnDragDrop := Nil;
for I := 0 to GetCurViewerCount-1 do
  if Assigned(Value) then
    CurViewer[I].OnDragDrop := fvDragDrop
  else CurViewer[I].OnDragDrop := Nil;
end;

procedure TFrameViewer.fvDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
if Assigned(FOnDragDrop) then
  FOnDragDrop(Self, Source, X, Y);
end;

procedure TFrameViewer.SetDragOver(const Value: TDragOverEvent);
var
  I: integer;
begin
FOnDragOver := Value;
if Assigned(CurFrameSet) then
  if Assigned(Value) then
    CurFrameSet.OnDragOver := fvDragOver
  else CurFrameSet.OnDragOver := Nil;
for I := 0 to GetCurViewerCount-1 do
  if Assigned(Value) then
    CurViewer[I].OnDragOver := fvDragOver
  else CurViewer[I].OnDragOver := Nil;
end;

procedure TFrameViewer.fvDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
if Assigned(FOnDragOver) then
  FOnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TFrameViewer.SetOnImageRequest(Handler: TGetImageEvent);
var
  I: integer;
begin
FOnImageRequest := Handler;
  with CurFrameSet do
    for I := 0 to Viewers.Count-1 do
      with ThtmlViewer(Viewers[I]) do
        OnImageRequest := Handler;
end;

function TFrameViewer.ViewerFromTarget(const Target: string): ThtmlViewer;
var
  I: integer;
begin
if Assigned(CurFrameSet) and Assigned(CurFrameSet.FrameNames)
     and CurFrameSet.FrameNames.Find(Target, I)
     and (CurFrameSet.FrameNames.Objects[I] <> Nil)
     and Assigned((CurFrameSet.FrameNames.Objects[I] as TfvFrame).Viewer) then
  Result := TfvFrame(CurFrameSet.FrameNames.Objects[I]).Viewer as ThtmlViewer
else Result := Nil;
end;

procedure TFrameViewer.RePaint;
begin
if Assigned(CurFrameSet) then
  CurFrameSet.RePaint;
end;

procedure TFrameViewer.SetOptions(Value: TFrameViewerOptions);
var
  I: integer;
begin
if (fvNoBorder in FOptions) <> (fvNoBorder in Value) then
  if fvNoBorder in Value then
    begin
    CurFrameSet.OuterBorder := 0;
    CurFrameSet.BevelOuter := bvNone;
    CurFrameSet.BorderSize := 0;
    end
  else
    begin
    CurFrameSet.BevelWidth := 2;
    CurFrameSet.BevelOuter := bvLowered;
    CurFrameSet.BorderSize := 2;
    end;
for I := 0 to CurFrameSet.Viewers.Count-1 do
  with ThtmlViewer(CurFrameSet.Viewers[I]) do
    begin
    if (fvOverLinksActive in Value) then
      htOptions := htOptions + [htOverLinksActive]
    else htOptions := htOptions - [htOverLinksActive];

    if (fvNoLinkUnderline in Value) then
      htOptions := htOptions + [htNoLinkUnderline]
    else htOptions := htOptions - [htNoLinkUnderline];

    if (fvPrintTableBackground in Value) then
      htOptions := htOptions + [htPrintTableBackground]
    else htOptions := htOptions - [htPrintTableBackground];

    if (fvPrintBackground in Value) then
      htOptions := htOptions + [htPrintBackground]
    else htOptions := htOptions - [htPrintBackground];

    if (fvPrintMonochromeBlack in Value) then
      htOptions := htOptions + [htPrintMonochromeBlack]
    else htOptions := htOptions - [htPrintMonochromeBlack];

    if (fvShowVScroll in Value) then
      htOptions := htOptions + [htShowVScroll]
    else htOptions := htOptions - [htShowVScroll];

    if (fvNoWheelMouse in Value) then
      htOptions := htOptions + [htNoWheelMouse]
    else htOptions := htOptions - [htNoWheelMouse];

    if (fvShowDummyCaret in Value) then
      htOptions := htOptions + [htShowDummyCaret]
    else htOptions := htOptions - [htShowDummyCaret];

    if (fvNoLinkHilite in Value) then
      htOptions := htOptions + [htNoLinkHilite]
    else htOptions := htOptions - [htNoLinkHilite];

    if (fvNoFocusRect in Value) or (fvNoBorder in Value) then
      BorderStyle := htNone
    else BorderStyle := htFocused;
    end;
FOptions := Value;
end;

procedure TFrameViewer.AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: string);
begin
(FrameSet as TSubFrameSet).AddFrame(Attr, FName);
end;

function TFrameViewer.CreateSubFrameSet(FrameSet: TObject): TObject;
var
  NewFrameSet, FS: TSubFrameSet;
begin
FS := (FrameSet as TSubFrameSet);
NewFrameSet := TSubFrameSet.CreateIt(FS, CurFrameSet);
FS.List.Add(NewFrameSet);
FS.InsertControl(NewFrameSet);
Result := NewFrameSet;
end;

procedure TFrameViewer.DoAttributes(FrameSet: TObject; Attr: TAttributeList);
begin
(FrameSet as TSubFrameSet).DoAttributes(Attr);
end;

procedure TFrameViewer.EndFrameSet(FrameSet: TObject);
begin
(FrameSet as TSubFrameSet).EndFrameSet;
end;

{----------------TFrameViewer.AddVisitedLink}
procedure TFrameViewer.AddVisitedLink(const S: string);
var
  I: integer;
begin
if (FVisitedMaxCount = 0) then
  Exit;
I := Visited.IndexOf(S);
if I = 0 then
  Exit
else if I > 0 then
  Visited.Delete(I);   {thus moving it to the top}
Visited.Insert(0, S);
for I :=  Visited.Count-1 downto FVisitedMaxCount do
  Visited.Delete(I);
end;

{----------------TFrameViewer.CheckVisitedLinks}
procedure TFrameViewer.CheckVisitedLinks;
var
  I, J, K: integer;
  S, S1, Src: string;
  Viewer: ThtmlViewer;
  RequestEvent: boolean;
begin
if FVisitedMaxCount = 0 then
  Exit;
RequestEvent := CurFrameSet.RequestEvent;
for K := 0 to CurFrameSet.Viewers.Count-1 do
  begin
  Viewer := ThtmlViewer(CurFrameSet.Viewers[K]);
  if RequestEvent then
    Src := TfvFrame(Viewer.FrameOwner).Source;
  for I := 0 to Visited.Count-1 do
    begin
    S := Visited[I];
    for J := 0 to Viewer.LinkList.Count-1 do
      with TFontObj(Viewer.LinkList[J]) do
        begin
        if not RequestEvent then
          begin
          if (Url <> '') and (Url[1] = '#') then
            S1 := Viewer.CurrentFile+Url
          else
            S1 := Viewer.HTMLExpandFilename(Url);
          end
        else
          if (Url <> '') and (Url[1] = '#') then
            S1 := Src+Url
          else
            S1 := URL;
        if CompareText(S, S1) = 0 then
          Visited := True;
        end;
    end;
  Viewer.Invalidate;
  end;
end;

{----------------TFrameViewer.DoURLRequest}
procedure TFrameViewer.DoURLRequest(Sender: TObject; const SRC: string;
                                    var RStream: TMemoryStream);
var
  EV: EventRec;
begin
if CurFrameSet.TriggerEvent(Src, @EV) then
  with EV do
    begin
    if not Assigned(UrlRequestStream) then
      UrlRequestStream := TMemoryStream.Create;
    case LStyle of
      lsFile:
        UrlRequestStream.LoadFromFile(NewName);
      lsString:
        begin
        UrlRequestStream.SetSize(Length(AString) * SizeOf(Char));
        System.Move(AString[1], UrlRequestStream.Memory^, Length(AString) * SizeOf(Char));
        end;
      end;
    RStream := UrlRequestStream;
    end;
end;

{----------------TFrameViewer.GetViewers:}
function TFrameViewer.GetViewers: TStrings;   
var
  I: integer;
  S: string;
  AFrame: TfvFrame;
  Viewer: ThtmlViewer;
  Pt1, Pt2: TPoint;
begin
if not Assigned(FViewerList) then
  FViewerList := TStringList.Create
else FViewerList.Clear;
for I := 0 to CurFrameSet.Viewers.Count-1 do
  begin
  Viewer := CurFrameSet.Viewers[I];
  if Viewer.SectionList.Count > 0  then
    begin
    S := '';
    AFrame := TfvFrame(Viewer.FrameOwner);
    Pt1 := AFrame.ClientToScreen(Point(0,0));
    Pt2 := CurFrameSet.ClientToScreen(Point(0,0));
    if Pt1.X <= Pt2.X +2 then
      S := S+'l';
    if Pt1.Y <= Pt2.Y +2 then
      S := S+'t';
    Pt1 := AFrame.ClientToScreen(Point(AFrame.ClientWidth, AFrame.ClientHeight));
    Pt2 := CurFrameSet.ClientToScreen(Point(CurFrameSet.ClientWidth, CurFrameSet.ClientHeight));
    if Pt1.X >= Pt2.X -2 then
      S := S+'r';
    if Pt1.Y >= Pt2.Y -2 then
      S := S+'b';
    FViewerList.AddObject(S, Viewer);
    end;
  end;
Result := FViewerList;
end;

{----------------TFVBase.GetFURL} {base class for TFrameViewer and TFrameBrowser}
function TFVBase.GetFURL: string;
begin
Result := FURL;
end;

function TFVBase.GetTarget: string;
begin
Result := FTarget;
end;

procedure TFVBase.SetViewImages(Value: boolean);
var
  I : integer;
begin
if (FViewImages <> Value) and not Processing then
  begin
  FViewImages := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].ViewImages := Value;
  end;
end;

procedure TFVBase.SetImageCacheCount(Value: integer);
var
  I : integer;
begin
if (FImageCacheCount <> Value) and not Processing then
  begin
  FImageCacheCount := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].ImageCacheCount := Value;
  end;
end;

function TFVBase.GetProcessing: boolean;
begin
Result := FProcessing or FViewerProcessing; 
end;

{----------------TFVBase.SetNoSelect}
procedure TFVBase.SetNoSelect(Value: boolean);
var
  I: integer;
begin
if Value <> FNoSelect then
  begin
  FNoSelect := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].NoSelect := Value;
  end;
end;

procedure TFVBase.SetOnBitmapRequest(Handler: TGetBitmapEvent);
var
  I: integer;
begin
FOnBitmapRequest := Handler;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].OnBitmapRequest := Handler;
end;

procedure TFVBase.SetOnMeta(Handler: TMetaType);
var
  I: integer;
begin
FOnMeta := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnMeta := Handler;
end;

procedure TFVBase.SetOnLink(Handler: TLinkType);
var
  I: integer;
begin
FOnLink := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnLink := Handler;
end;

procedure TFVBase.SetOnScript(Handler: TScriptEvent);
var
  I: integer;
begin
FOnScript := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnScript := Handler;
end;

procedure TFVBase.SetImageOver(Handler: TImageOverEvent);
var
  I: integer;
begin
FOnImageOver := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnImageOver := Handler;
end;

procedure TFVBase.SetImageClick(Handler: TImageClickEvent);
var
  I: integer;
begin
FOnImageClick := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnImageClick := Handler;
end;

procedure TFVBase.SetOnRightClick(Handler: TRightClickEvent);
var
  I: integer;
begin
FOnRightClick := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnRightClick := Handler;
end;

procedure TFVBase.SetOnObjectFocus(Handler: ThtObjectEvent);
var
  I: integer;
begin
FOnObjectFocus := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnObjectFocus := Handler;
end;

procedure TFVBase.SetOnObjectBlur(Handler: ThtObjectEvent);
var
  I: integer;
begin
FOnObjectBlur := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnObjectBlur := Handler;
end;

procedure TFVBase.SetOnObjectChange(Handler: ThtObjectEvent);
var
  I: integer;
begin
FOnObjectChange := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnObjectChange := Handler;
end;

procedure TFVBase.SetOnFileBrowse(Handler: TFileBrowseEvent);    
var
  I: integer;
begin
FOnFileBrowse := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnFileBrowse := Handler;
end;

procedure TFVBase.SetOnObjectClick(Handler: TObjectClickEvent);
var
  I: integer;
begin
FOnObjectClick := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnObjectClick := Handler;
end;

procedure TFVBase.SetMouseDouble(Handler: TMouseEvent);
var
  I: integer;
begin
FOnMouseDouble := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnMouseDouble := Handler;
end;

procedure TFVBase.SetServerRoot(Value: string);
begin
Value := Trim(Value);
if (Length(Value) >= 1) and (Value[Length(Value)] = '\') then
  SetLength(Value, Length(Value)-1);
FServerRoot := Value;
end;

procedure TFVBase.SetPrintMarginLeft(Value: Double);
var
  I: integer;
begin
if FPrintMarginLeft <> Value then
  begin
  FPrintMarginLeft := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].PrintMarginLeft := Value;
  end;
end;

procedure TFVBase.SetPrintMarginRight(Value: Double);
var
  I: integer;
begin
if FPrintMarginRight <> Value then
  begin
  FPrintMarginRight := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].PrintMarginRight := Value;
  end;
end;

procedure TFVBase.SetPrintMarginTop(Value: Double);
var
  I: integer;
begin
if FPrintMarginTop <> Value then
  begin
  FPrintMarginTop := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].PrintMarginTop := Value;
  end;
end;

procedure TFVBase.SetPrintMarginBottom(Value: Double);
var
  I: integer;
begin
if FPrintMarginBottom <> Value then
  begin
  FPrintMarginBottom := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].PrintMarginBottom := Value;
  end;
end;

procedure TFVBase.SetPrintScale(Value: Double);
var
  I: integer;
begin
if FPrintScale <> Value then
  begin
  FPrintScale := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].PrintScale := Value;
  end;
end;

procedure TFVBase.SetMarginWidth(Value: integer);
var
  I: integer;
begin
if FMarginWidth <> Value then
  begin
  FMarginWidth := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].MarginWidth := Value;
  end;
end;

procedure TFVBase.SetMarginHeight(Value: integer);
var
  I: integer;
begin
if FMarginHeight <> Value then
  begin
  FMarginHeight := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].MarginHeight := Value;
  end;
end;

procedure TFVBase.SetPrintHeader(Handler: TPagePrinted);
var
  I: integer;
begin
FOnPrintHeader := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPrintHeader := Handler;
end;

procedure TFVBase.SetPrintFooter(Handler: TPagePrinted);
var
  I: integer;
begin
FOnPrintFooter := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPrintFooter := Handler;
end;

procedure TFVBase.SetPrintHtmlHeader(Handler: THtmlPagePrinted);   
var
  I: integer;
begin
FOnPrintHtmlHeader := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPrintHtmlHeader := Handler;
end;

procedure TFVBase.SetPrintHtmlFooter(Handler: THtmlPagePrinted);   
var
  I: integer;
begin
FOnPrintHtmlFooter := Handler;
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPrintHtmlFooter := Handler;
end;

procedure TFVBase.SetVisitedMaxCount(Value: integer);
var
  I, J: integer;
begin
Value := IntMax(Value, 0);
if Value <> FVisitedMaxCount then
  begin
  FVisitedMaxCount := Value;
  if FVisitedMaxCount = 0 then
    begin
    Visited.Clear;
    for I := 0 to GetCurViewerCount-1 do
      with CurViewer[I] do
        for J := 0 to SectionList.LinkList.Count-1 do
          TFontObj(LinkList[J]).Visited := False;
    RePaint;
    end
  else
    begin
    FVisitedMaxCount := Value;
    for I := Visited.Count-1 downto FVisitedMaxCount do
      Visited.Delete(I);
    end;
  end;
end;

{----------------TFVBase.SetColor}
procedure TFVBase.SetColor(Value: TColor);
var
  I: integer;
begin
if (FBackground <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefBackground := Value;
  FBackground := Value;
  Color := Value;
  end;
end;

function TFVBase.GetFontName: TFontName;
begin
Result := FFontName;
end;

procedure TFVBase.SetFontName(Value: TFontName);
var
  I: integer;
begin
if  CompareText(Value, FFontName) <> 0 then
  begin
  FFontName := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefFontName := Value;
  end;
end;

function TFVBase.GetPreFontName: TFontName;
begin
Result := FPreFontName;
end;

procedure TFVBase.SetPreFontName(Value: TFontName);
var
  I: integer;
begin
if  CompareText(Value, FPreFontName) <> 0 then
  begin
  FPreFontName := Value;
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefPreFontName := Value;
  end;
end;

procedure TFVBase.SetFontSize(Value: integer);
var
  I: integer;
begin
if (FFontSize <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefFontSize := Value;
  FFontSize := Value;
  end;
end;

procedure TFVBase.SetFontColor(Value: TColor);
var
  I: integer;
begin
if (FFontColor <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefFontColor := Value;
  FFontColor := Value;
  end;
end;

procedure TFVBase.SetHotSpotColor(Value: TColor);
var
  I: integer;
begin
if (FHotSpotColor <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefHotSpotColor := Value;
  FHotSpotColor := Value;
  end;
end;

procedure TFVBase.SetActiveColor(Value: TColor);
var
  I: integer;
begin
if (FOverColor <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefOverLinkColor := Value;
  FOverColor := Value;
  end;
end;

procedure TFVBase.SetVisitedColor(Value: TColor);
var
  I: integer;
begin
if (FVisitedColor <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].DefVisitedLinkColor := Value;
  FVisitedColor := Value;
  end;
end;

{----------------TFVBase.SetHistoryMaxCount}
procedure TFVBase.SetHistoryMaxCount(Value: integer);
var
  I: integer;
begin
if (Value = FHistoryMaxCount) or (Value < 0) then Exit;
ClearHistory;
for I := 0 to GetCurViewerCount-1 do
  with CurViewer[I] do
    begin
    ClearHistory;
    HistoryMaxCount := Value;
    end;
FHistoryMaxCount := Value;
end;

procedure TFVBase.SetCursor(Value: TCursor);
var
  I: integer;
begin
if Value = OldThickIBeamCursor then
  Value := crIBeam;
if (FCursor <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].Cursor := Value;
  FCursor := Value;
  end;
end;

procedure TFVBase.SetOnPanelCreate(Handler: TPanelCreateEvent);
var
  I: integer;
begin
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPanelCreate := Handler;
FOnPanelCreate := Handler;
end;

procedure TFVBase.SetOnPanelDestroy(Handler: TPanelDestroyEvent);
var
  I: integer;
begin
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPanelDestroy := Handler;
FOnPanelDestroy := Handler;
end;

procedure TFVBase.SetOnPanelPrint(Handler: TPanelPrintEvent);
var
  I: integer;
begin
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnPanelPrint := Handler;
FOnPanelPrint := Handler;
end;

procedure TFVBase.SetOnParseBegin(Handler: TParseEvent);
var
  I: integer;
begin
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnParseBegin:= Handler;
FOnParseBegin := Handler;
end;

procedure TFVBase.SetOnParseEnd(Handler: TNotifyEvent);
var
  I: integer;
begin
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnParseEnd:= Handler;
FOnParseEnd := Handler;
end;

Function TFVBase.GetSelLength: integer;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  Result := AViewer.SelLength
else Result := 0;
end;

procedure TFVBase.SetSelLength(Value: integer);
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  AViewer.SelLength := Value;
end;

Function TFVBase.GetSelStart: integer;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  Result := AViewer.SelStart
else Result := 0;
end;

procedure TFVBase.SetSelStart(Value: integer);
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  AViewer.SelStart := Value;
end;

{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
procedure TFVBase.SetCharset(Value: TFontCharset);
var
  I: integer;
begin
if (FCharset <> Value) then
  begin
  for I := 0 to GetCurViewerCount-1 do
    CurViewer[I].Charset := Value;
  FCharset := Value;
  end;
end;
{$endif}

procedure TFVBase.SetOnObjectTag(Handler: TObjectTagEvent);
var
  I: integer;
begin
for I := 0 to GetCurViewerCount-1 do
  CurViewer[I].OnObjectTag := Handler;
FOnObjectTag := Handler;
end;

{----------------TFVBase.GetOurPalette:}
function TFVBase.GetOurPalette: HPalette;
begin
if ColorBits = 8 then
  Result := CopyPalette(ThePalette)
else Result := 0;
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
    if FDither then SetGlobalPalette(ThePalette);
    end;
  end;
end;

{----------------TFVBase.SetDither}
procedure TFVBase.SetDither(Value: boolean);
begin
if (Value <> FDither) and (ColorBits = 8) then
  begin
  FDither := Value;
  if Value then SetGlobalPalette(ThePalette)
  else SetGLobalPalette(0);
  end;
end;

function TFVBase.GetCaretPos: integer;
var
  Vw: ThtmlViewer;
begin
Vw := GetActiveViewer;
if Assigned(Vw) then
  Result := Vw.CaretPos
else Result := 0;
end;

procedure TFVBase.SetCaretPos(Value: integer);
var
  Vw: ThtmlViewer;
begin
Vw := GetActiveViewer;
if Assigned(Vw) then
  Vw.CaretPos := Value;
end;

function TFVBase.GetSelText: WideString;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  Result := AViewer.SelText
else Result := '';
end;

function TFVBase.GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;
var
  AViewer: ThtmlViewer;
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

{----------------TFVBase.InsertImage}
function TFVBase.InsertImage(Viewer: ThtmlViewer; const Src: string;
                Stream: TMemoryStream): boolean;
begin
try
  Result := (Viewer as ThtmlViewer).InsertImage(Src, Stream);
except
  Result := True;  {consider exceptions done}
  end;
end;

procedure TFVBase.SetFocus;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) and AViewer.CanFocus then
  try
    AViewer.SetFocus;
  except       {just in case}
    inherited SetFocus;
    end
else inherited SetFocus;
end;

{----------------TFVBase.SetProcessing}
procedure TFVBase.SetProcessing(Local, Viewer: boolean);
var
  Change: boolean;
begin
Change := (Local or Viewer <> FProcessing or FViewerProcessing);
FProcessing := Local;
FViewerProcessing := Viewer;
if Change and Assigned(FOnProcessing) then
  FOnProcessing(Self, Local or Viewer);
end;

procedure TFVBase.CheckProcessing(Sender: TObject; ProcessingOn: boolean);
begin
with ProcessList do
  begin
  if ProcessingOn then
    begin
    if IndexOf(Sender) = -1 then
      Add(Sender);
    end
  else Remove(Sender);
  SetProcessing(FProcessing, Count > 0);
  end;
end;

{----------------TFVBase.Print}
procedure TFVBase.Print(FromPage, ToPage: integer);
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  AViewer.Print(FromPage, ToPage);
end;

{----------------TFVBase.NumPrinterPages}
function TFVBase.NumPrinterPages(var WidthRatio: double): integer;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  Result := AViewer.NumPrinterPages(WidthRatio)
else Result := 0;
end;

function TFVBase.NumPrinterPages: integer;
var
  Dummy: double;
begin
Result := NumPrinterPages(Dummy);
end;

{----------------TFVBase.CopyToClipboard}
procedure TFVBase.CopyToClipboard;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) and (AViewer.SelLength <> 0) then
  AViewer.CopyToClipboard;
end;

procedure TFVBase.SelectAll;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  AViewer.SelectAll;
end;

{----------------TFVBase.Find}
function TFVBase.Find(const S: WideString; MatchCase: boolean): boolean;
begin
Result := FindEx(S, MatchCase, False);
end;

{----------------TFVBase.FindEx}
function TFVBase.FindEx(const S: WideString; MatchCase, Reverse: boolean): boolean;
var
  AViewer: ThtmlViewer;
begin
AViewer := GetActiveViewer;
if Assigned(AViewer) then
  Result := AViewer.FindEx(S, MatchCase, Reverse)
else Result := False;
end;

{----------------PositionObj}
destructor PositionObj.Destroy;
begin
FormData.Free;
inherited;
end;

end.

