{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
  Windows, SysUtils, Classes, Graphics, ClipBrd, Controls, Messages, Variants,
  {$ifdef LCL}Interfaces, IntfGraphics, FpImage, {$endif}
  {$IFNDEF NoGDIPlus}GDIPL2A, {$ENDIF}
  UrlSubs, StyleUn, HtmlGlobals, HtmlGif2;

const
  VersionNo = '10.2';
  MaxHScroll = 6000; {max horizontal display in pixels}
  HandCursor = 10101;
  OldThickIBeamCursor = 2;
  UpDownCursor = 10103;
  UpOnlyCursor = 10104;
  DownOnlyCursor = 10105;
  Tokenleng = 300;
  TopLim = -200; {drawing limits}
  BotLim = 5000;
  FmCtl = WideChar(#2);
  ImgPan = WideChar(#4);
  BrkCh = WideChar(#8);
//BG, 11.09.2010: moved to this unit to reduce circular dependencies:
  ImageSpace = 3; {extra space for left, right images}
  ListIndent = 35;

{$IFNDEF DOTNET}
{$IFNDEF FPC}
type
    //needed so that in FreePascal, we can use pointers of different sizes
{$IFDEF WIN32}
  PtrInt = LongInt;
  PtrUInt = LongWord;
{$ENDIF}
{$IFDEF WIN64}
  PtrInt = Int64;
  PtrUInt = Int64;
{$ENDIF}
//NOTE:  The code below asumes a 32bit Linux architecture (such as target i386-linux)
{$IFDEF KYLIX}
  PtrInt = LongInt;
  PtrUInt = LongWord;
{$ENDIF}
{$ENDIF}
{$ENDIF}

type
  TgpObject = TObject;
  TSectionBaseList = class;
  TFloatingObj = class;

  { Like TList but frees it's items. Use only descendents of TObject! }
  TFreeList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
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
    ReadonlySy, EolSy);

  TAttribute = class(TObject) {holds a tag attribute}
  public
    Which: Symb; {symbol of attribute such as HrefSy}
    WhichName: ThtString;
    Value: Integer; {numeric value if appropriate}
    Percent: boolean; {if value is in percent}
    Name: ThtString; {ThtString (mixed case), value after '=' sign}
    CodePage: Integer;
    constructor Create(ASym: Symb; AValue: Integer;
      const NameStr, ValueStr: ThtString; ACodePage: Integer);
  end;

  TAttributeList = class(TFreeList) {a list of tag attributes,(TAttributes)}
  private
    Prop: TProperties;
    SaveID: ThtString;
    function GetClass: ThtString;
    function GetID: ThtString;
    function GetTitle: ThtString;
    function GetStyle: TProperties;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function Find(Sy: Symb; var T: TAttribute): boolean;
    function CreateStringList: ThtStringList;
    property TheClass: ThtString read GetClass;
    property TheID: ThtString read GetID;
    property TheTitle: ThtString read GetTitle;
    property TheStyle: TProperties read GetStyle;
  end;

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
  public
    MaxCache: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function AddObject(const S: ThtString; AObject: TObject): Integer; override;
    procedure DecUsage(const S: ThtString);
    procedure IncUsage(const S: ThtString);
    procedure BumpAndCheck;
    procedure PurgeCache;
    function GetImage(I: Integer): TgpObject;
    procedure SetCacheCount(N: Integer);
  end;

  SelTextCount = class(TObject)
  private
    Buffer: PWideChar;
    BufferLeng: Integer;
    Leng: Integer;
  public
    procedure AddText(P: PWideChar; Size: Integer); virtual;
    procedure AddTextCR(P: PWideChar; Size: Integer);
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

  TutText = class {holds start and end point of URL text}
    Start: Integer;
    Last: Integer;
  end;

  TUrlTarget = class(TObject)
  private
    function GetStart: Integer;
    function GetLast: Integer;
  public
    URL,
      Target: ThtString;
    ID: Integer;
    Attr: ThtString;
    utText: TutText;
    TabIndex: Integer;
    constructor Create;
    procedure Copy(UT: TUrlTarget);
    destructor Destroy; override;
    procedure Assign(const AnUrl, ATarget: ThtString; L: TAttributeList; AStart: Integer);
    procedure Clear;
    procedure SetLast(List: TList; ALast: Integer);
    property Start: Integer read GetStart;
    property Last: Integer read GetLast;
  end;

  TMapItem = class(TObject) {holds a client map info}
    MapName: ThtString;
    Areas: ThtStringList; {holds the URL and region handle}
    AreaTargets: ThtStringList; {holds the target window}
    AreaTitles: ThtStringList; {the Title strings}
    constructor Create;
    destructor Destroy; override;
    function GetURL(X, Y: Integer; var URLTarg: TURLTarget; var ATitle: ThtString): boolean;
    procedure AddArea(Attrib: TAttributeList);
  end;

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
    procedure DrawDIB(DC: HDC; X: Integer; Y: Integer; W, H: Integer;
      ROP: DWord);
  end;

  IndentRec = class(TObject)
    X: Integer; {indent for this record}
    YT, YB: Integer; {top and bottom Y values for this record}
    ID: TObject; {level inicator for this record, 0 for not applicable}
    Float: boolean; {set if Floating block boundary}
  end;

  TIndentManager = class(TObject)
  public
    Width, ClipWidth: Integer;
    L, R: TFreeList; {holds info (IndentRec's) on left and right indents}
    CurrentID: TObject; {the current level (a TBlock pointer)}
    LfEdge, RtEdge: Integer; {current extreme edges}
  public
    constructor Create;
    destructor Destroy; override;
    function GetNextLeftXY(var Y: Integer; X, ThisWidth, MaxWidth, MinIndex: Integer): Integer;
    function GetNextWiderY(Y: Integer): Integer;
    function ImageBottom: Integer;
    function LeftIndent(Y: Integer): Integer;
    function RightSide(Y: Integer): Integer;
    function SetLeftIndent(XLeft, Y: Integer): Integer;
    function SetRightIndent(XRight, Y: Integer): Integer;
    procedure Clear;
    procedure FreeLeftIndentRec(I: Integer);
    procedure FreeRightIndentRec(I: Integer);
    procedure GetClearY(var CL, CR: Integer);
    procedure Reset(Lf, Rt: Integer);
    procedure Update(Y: Integer; Img: TFloatingObj);
    procedure UpdateBlock(Y: Integer; IW: Integer; IH: Integer; Justify: AlignmentType);
    procedure UpdateTable(Y: Integer; IW: Integer; IH: Integer; Justify: JustifyType);
  end;

  AllocRec = class(TObject)
    Ptr: Pointer;
    ASize: Integer;
    AHandle: THandle;
  end;

  IndexArray = array[1..TokenLeng] of Integer;
  PIndexArray = ^IndexArray;
  ChrArray = array[1..TokenLeng] of WideChar;

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
    procedure Add(C: ThtChar; Index: Integer);
    procedure Clear;
    procedure Concat(T: TCharCollection);

    property AsString: ThtString read GetAsString;
    property Chars: ThtString read FChars;
    property Indices: PIndexArray read FIndices;
    property Size: Integer read GetSize;
  end;

  TokenObj = class
  private
    St: WideString;
    StringOK: boolean;
    function GetString: WideString;
  public
    C: ^ChrArray;
    I: ^IndexArray;
    MaxIndex, Leng: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure AddUnicodeChar(Ch: WideChar; Ind: Integer);
    procedure AddString(S: TCharCollection; CodePage: Integer);
    procedure Concat(T: TokenObj);
    procedure Clear;
    procedure Remove(N: Integer);
    procedure Replace(N: Integer; Ch: WideChar);

    property S: WideString read GetString;
  end;

  TIDObject = class(TObject)
  protected
    function GetYPosition: Integer; virtual; abstract;
  public
    property YPosition: Integer read GetYPosition;
  end;

  TChPosObj = class(TIDObject)
  private
    FChPos: Integer;
    FList: TSectionBaseList;
  protected
    function GetYPosition: Integer; override;
    property ChPos: Integer read FChPos;
    property List: TSectionBaseList read FList;
  end;

  TIDNameList = class(ThtStringList)
  private
    OwnerList: TSectionBaseList;
  public
    constructor Create(List: TSectionBaseList);
    destructor Destroy; override;
    procedure Clear; override;
    function AddObject(const S: ThtString; AObject: TObject): Integer; override;
    procedure AddChPosObject(const S: ThtString; Pos: Integer);
  end;

{$IFNDEF NoMetafile}
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

  ImageType = (NoImage, Bmp, Gif, Gif89, Png, Jpg);

  htColorArray = packed array[0..3] of TColor;
  htBorderStyleArray = packed array[0..3] of BorderStyleType;

//BG, 11.09.2010: moved to this unit to reduce circular dependencies:

  guResultType = set of (guUrl, guControl, guTitle);

  TFontObjBase = class(TObject) {font information}
  public
    UrlTarget: TUrlTarget;
  end;

  TFloatingObj = class(TIDObject)
  public
    Pos: Integer; {0..Len  index of image position}
    ImageHeight, {does not include VSpace}
    ImageWidth: Integer;
//BG, 17.01.2010: separate vertical and horizontal alignment:
//    ObjAlign: AlignmentType;
    VertAlign: AlignmentType;
    HorzAlign: AlignmentType;
    Indent: Integer;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB: Integer; {horizontal, vertical extra space}
    SpecWidth: Integer; {as specified by <img or panel> tag}
    SpecHeight: Integer; {as specified by <img or panel> tag}
    PercentWidth: boolean; {if width is percent}
    PercentHeight: boolean; {if height is percent}
    ImageTitle: ThtString;
{$ifdef UNICODE}
    FAlt: AnsiString;
    FAltW: ThtString; {the alt= attribute}
{$else}
    FAlt: ThtString; {the alt= attribute}
    FAltW: WideString;
{$endif}

    function GetYPosition: Integer; override;
  public
    ImageKnown: boolean; {know size of image}
    DrawYY: Integer;
    DrawXX: Integer;
    NoBorder: boolean; {set if don't want blue border}
    BorderSize: Integer;
    constructor CreateCopy(T: TFloatingObj);
    procedure SetAlt(CodePage: Integer; const Value: ThtString);
    procedure DrawLogic(SectionList: TSectionBaseList; Canvas: TCanvas;
      FO: TFontObjBase; AvailableWidth, AvailableHeight: Integer); virtual; abstract;
    procedure ProcessProperties(Prop: TProperties);
    property Alt: ThtString read {$ifdef UNICODE} FAltW {$else} FAlt {$endif};
  end;

  TBlockBase = class;

  TSectionBase = class(TIDObject) {abstract base for document sections}
  private
    FDisplay: TPropDisplay;
    FMyBlock: TBlockBase;
    FParentSectionList: TSectionBaseList; {what list it's in}
  protected
    function GetYPosition: Integer; override;
  public
    SectionHeight: Integer; {pixel height of section}
    DrawHeight: Integer; {floating image may overhang}
    StartCurs: Integer;
    Len: Integer;
    ZIndex: Integer;
    ContentTop, ContentBot, ContentLeft: Integer;
    DrawTop, DrawBot, YDraw: Integer;

    constructor Create(AMasterList: TSectionBaseList; ADisplay: TPropDisplay); overload;
    constructor Create(AMasterList: TSectionBaseList; AProp: TProperties); overload;
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); virtual;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; virtual;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; virtual;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer;
      IMgr: TIndentManager; var MaxWidth: Integer; var Curs: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer; var XR: Integer; var YR: Integer;
      var CaretHt: Integer; var Intext: boolean): Integer; virtual;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; virtual;
    function FindSourcePos(DocPos: Integer): Integer; virtual;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; virtual;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; virtual;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; virtual;
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer; var UrlTarg: TUrlTarget;
      var FormControl: TIDObject{TImageFormControlObj}; var ATitle: ThtString): guResultType; virtual;
    function PtInObject(X: Integer; Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean; virtual;
    procedure AddSectionsToList; virtual;
    procedure CopyToClipboard; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); virtual;
    procedure SetParent(List: TSectionBaseList);
    property Display: TPropDisplay read FDisplay write FDisplay;
    property MyBlock: TBlockBase read FMyBlock write FMyBlock;
    property ParentSectionList: TSectionBaseList read FParentSectionList;
  end;

  TBlockBase = class(TSectionBase)
  end;

  TSectionBaseList = class(TFreeList)
  private
    function getItem(Index: Integer): TSectionBase;
  protected
    procedure AddSectionsToPositionList(Sections: TSectionBase); virtual;
  public
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; virtual;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; virtual;
    property Items[Index: Integer]: TSectionBase read getItem; default;
  end;

  TGetStreamEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TMemoryStream) of object;
  TIncludeType = procedure(Sender: TObject; const Command: ThtString; Params: ThtStrings; var IString: ThtString) of object;
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
    procedure SetOnInclude(Handler: TIncludeType); virtual;
    procedure SetOnLink(Handler: TLinkType); virtual;
    procedure SetOnScript(Handler: TScriptEvent); virtual;
    procedure SetOnSoundRequest(Handler: TSoundType); virtual;
  public
    property OnInclude: TIncludeType read FOnInclude write SetOnInclude;
    property OnLink: TLinkType read FOnLink write SetOnLink;
    property OnScript: TScriptEvent read FOnScript write SetOnScript;
    property OnSoundRequest: TSoundType read FOnSoundRequest write SetOnSoundRequest;
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
    procedure htProgress(Percent: Integer); virtual; abstract;
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function HtmlExpandFilename(const Filename: ThtString): ThtString; virtual; abstract;
    function ShowFocusRect: Boolean; virtual; abstract;
  end;

  TFrameViewerBase = class(TViewerBase)
  private
    procedure wmerase(var msg: TMessage); message WM_ERASEBKGND;
  public
    function CreateSubFrameSet(FrameSet: TObject): TObject; virtual; abstract;
    procedure AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: ThtString); virtual; abstract;
    procedure DoAttributes(FrameSet: TObject; Attr: TAttributeList); virtual; abstract;
    procedure EndFrameSet(FrameSet: TObject); virtual; abstract;
  end;

var
  DefBitMap, ErrorBitMap, ErrorBitmapMask: TBitMap;
  WaitStream: TMemoryStream;

function StrLenW(Str: PWideChar): Cardinal;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function StrRScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; var Extent: Integer): Integer;
function WidePos(SubStr, S: WideString): Integer;
function WideTrim(const S: WideString): WideString;
function WideUpperCase1(const S: WideString): WideString; {$ifdef UNICODE} inline; {$endif}
function WideLowerCase1(const S: WideString): WideString; {$ifdef UNICODE} inline; {$endif}
function WideSameText1(const S1, S2: WideString): boolean; {$ifdef UseInline} inline; {$endif}
function WideSameStr1(const S1, S2: WideString): boolean;  {$ifdef UseInline} inline; {$endif}
function PosX(const SubStr, S: ThtString; Offset: Integer = 1): Integer;
   {find substring in S starting at Offset}

function CalcClipRect(Canvas: TCanvas; const Rect: TRect; Printing: boolean): TRect;
procedure GetClippingRgn(Canvas: TCanvas; const ARect: TRect; Printing: boolean; var Rgn, SaveRgn: HRgn);

function LoadImageFromFile(const FName: ThtString; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
function LoadImageFromStream(Stream: TMemoryStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
// unused: function GetImageAndMaskFromFile(const Filename: ThtString; var Transparent: Transparency;
//  var Mask: TBitmap): TgpObject;

  {convert an HTML style filename to one for Dos}
function HTMLServerToDos(FName, Root: ThtString): ThtString;

procedure WrapTextW(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; S: WideString);

procedure FinishTransparentBitmap(ahdc: HDC; InImage, Mask: TBitmap; xStart, yStart, W, H: Integer);
function GetImageMask(Image: TBitmap; ColorValid: boolean; AColor: TColor): TBitmap;
//function GetImageAndMaskFromStream(Stream: TMemoryStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
// unused: function KindOfImageFile(FName: ThtString): ImageType;
//function KindOfImage(Start: Pointer): ImageType;
procedure FillRectWhite(Canvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
procedure FormControlRect(Canvas: TCanvas; X1: Integer;
  Y1: Integer; X2: Integer; Y2: Integer; Raised, PrintMonoBlack, Disabled: boolean; Color: TColor);
function GetXExtent(DC: HDC; P: PWideChar; N: Integer): Integer;

function EnlargeImage(Image: TGpObject; W, H: Integer): TBitmap;
procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
procedure PrintTransparentBitmap3(Canvas: TCanvas; X, Y, NewW, NewH: Integer;
  Bitmap, Mask: TBitmap; YI, HI: Integer);

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

procedure DrawBorder(Canvas: TCanvas; ORect, IRect: TRect; const C: htColorArray;
  S: htBorderStyleArray; BGround: TColor; Print: boolean);

function MultibyteToWideString(CodePage: Integer; const S: AnsiString; Len: Integer = -1): WideString;
function WideStringToMultibyte(CodePage: Integer; W: WideString): Ansistring;
function GetImageHeight(Image: TGpObject): Integer;
function GetImageWidth(Image: TGpObject): Integer;

implementation

uses
  Forms, Math,
  {$ifndef FPC_TODO}jpeg, {$endif}
  {$IFDEF UNICODE} PngImage, {$ENDIF}
  DitherUnit,
  StylePars;

type
  EGDIPlus = class(Exception);

{----------------StrLenW}

function StrLenW(Str: PWideChar): Cardinal;
{returns number of characters in a ThtString excluding the null terminator}

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

{----------------StrPosW}

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

{----------------StrRScanW}

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

{----------------StrScanW}

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

{----------------FitText}

function FitText(DC: HDC; S: PWideChar; Max, Width: Integer; var Extent: Integer): Integer;
  {return count <= Max which fits in Width.  Return X, the extent of chars that fit}

type
  Integers = array[1..1] of Integer;
var
  ExtS: TSize;
  Ints: ^Integers;
  L, H, I: Integer;

begin
  Extent := 0;
  Result := 0;
  if (Width <= 0) or (Max = 0) then
    Exit;

  if not IsWin32Platform then
  begin
    GetMem(Ints, Sizeof(Integer) * Max);
    try
      if GetTextExtentExPointW(DC, S, Max, Width, @Result, @Ints^, ExtS) then
        if Result > 0 then
          Extent := Ints^[Result]
        else
          Extent := 0;
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
      GetTextExtentPoint32W(DC, S, I, ExtS);
      if ExtS.cx < Width then
        L := I + 1
      else
        H := I - 1;
      if ExtS.cx = Width then
        Break;
      I := (L + H) shr 1;
    end;
    Result := I;
    Extent := ExtS.cx;
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

function PosX(const SubStr, S: ThtString; Offset: Integer = 1): Integer;
{find substring in S starting at Offset}
var
  S1: ThtString;
  I: Integer;
begin
  if Offset <= 1 then
    Result := Pos(SubStr, S)
  else
  begin
    S1 := Copy(S, Offset, Length(S) - Offset + 1);
    I := Pos(SubStr, S1);
    if I > 0 then
      Result := I + Offset - 1
    else
      Result := 0;
  end;
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

function HTMLServerToDos(FName, Root: ThtString): ThtString;
{Add Prefix Root only if first character is '\' but not '\\'}
begin
  Result := Trim(HTMLToDos(FName));
  if (Result <> '') and (Root <> '') then
  begin
    if Pos('\\', Result) = 1 then
      Exit;
    if Pos(':', Result) = 2 then
      Exit;
    if Result[1] = '\' then
      Result := Root + Result;
  end;
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

procedure FormControlRect(Canvas: TCanvas; X1: Integer;
  Y1: Integer; X2: Integer; Y2: Integer; Raised, PrintMonoBlack, Disabled: boolean; Color: TColor);
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
  MaxCache := 4;
  {$IFNDEF NoGDIPlus}
  CheckInitGDIPlus;
  {$ENDIF NoGDIPlus}
end;

destructor TStringBitmapList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    (Objects[I] as TBitmapItem).Free;
  {$IFNDEF NoGDIPlus}
  CheckExitGDIPlus;
  {$ENDIF NoGDIPlus}
  inherited Destroy;
end;

function TStringBitmapList.AddObject(const S: ThtString; AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
  if AObject is TBitmapItem then
    Inc(TBitmapItem(AObject).UsageCount);
end;

procedure TStringBitmapList.DecUsage(const S: ThtString);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    with Objects[I] as TBitmapItem do
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
    with Objects[I] as TBitmapItem do
      Inc(UsageCount);
end;

procedure TStringBitmapList.SetCacheCount(N: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with (Objects[I] as TBitmapItem) do
    begin
      if (AccessCount > N) and (UsageCount <= 0) then
      begin
        Delete(I);
        Free;
      end;
    end;
  MaxCache := N;
end;

function TStringBitmapList.GetImage(I: Integer): TgpObject;
begin
  with Objects[I] as TBitmapItem do
  begin
    Result := MImage;
    AccessCount := 0;
    Inc(UsageCount);
  end;
end;

procedure TStringBitmapList.BumpAndCheck;
var
  I: Integer;
  Tmp: TBitmapItem;
begin
  for I := Count - 1 downto 0 do
  begin
    Tmp := (Objects[I] as TBitmapItem);
    with Tmp do
    begin
      Inc(AccessCount);
      if (AccessCount > MaxCache) and (UsageCount <= 0) then
      begin
        Delete(I);
        Free; {the TBitmapItem}
      end;
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
    Tmp := (Objects[I] as TBitmapItem);
    with Tmp do
    begin
      if (UsageCount <= 0) then
      begin
        Delete(I);
        Free; {the TBitmapItem}
      end;
    end;
  end;
end;

procedure TStringBitmapList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    (Objects[I] as TBitmapItem).Free;
  inherited Clear;
end;

constructor TAttribute.Create(ASym: Symb; AValue: Integer;
  const NameStr, ValueStr: ThtString; ACodePage: Integer);
begin
  inherited Create;
  Which := ASym;
  Value := AValue;
  WhichName := NameStr;
  Name := ValueStr;
  CodePage := ACodePage;
end;

{----------------TAttributeList}

destructor TAttributeList.Destroy;
begin
  Prop.Free;
  inherited;
end;

procedure TAttributeList.Clear;
begin
  inherited Clear;
  SaveID := '';
end;

function TAttributeList.Find(Sy: Symb; var T: TAttribute): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if TAttribute(Items[I]).which = Sy then
    begin
      Result := True;
      T := Items[I];
      Exit;
    end;
  Result := False;
end;

function TAttributeList.CreateStringList: ThtStringList;
var
  I: Integer;
begin
  Result := ThtStringList.Create;
  for I := 0 to Count - 1 do
    with TAttribute(Items[I]) do
      Result.Add(WhichName + '=' + Name);
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
  utText := TutText.Create;
  utText.Start := -1;
  utText.Last := -1;
end;

destructor TUrlTarget.Destroy;
begin
  FreeAndNil(utText);
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

procedure TUrlTarget.Copy(UT: TUrlTarget);
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

function TUrlTarget.GetStart: Integer;
begin
  Result := utText.Start
end;

function TUrlTarget.GetLast: Integer;
begin
  Result := utText.Last
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
    if not (P[I] in [FmCtl, ImgPan]) then {ImgPan and FmCtl used to mark images, form controls}
      Inc(Leng);
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
    if not (P[I] in [FmCtl, ImgPan, BrkCh]) then {ImgPan and FmCtl used to mark images, form controls}
      if Leng < SizeM1 then
      begin
        Buffer[Leng] := P[I];
        Inc(Leng);
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

{----------------TMapItem.Create}

constructor TMapItem.Create;
begin
  inherited Create;
  Areas := ThtStringList.Create;
  AreaTargets := ThtStringList.Create;
  AreaTitles := ThtStringList.Create;
end;

destructor TMapItem.Destroy;
var
  I: Integer;
begin
  for I := 0 to Areas.Count - 1 do
    DeleteObject(THandle(Areas.Objects[I]));
  Areas.Free;
  AreaTargets.Free;
  AreaTitles.Free;
  inherited Destroy;
end;

function TMapItem.GetURL(X, Y: Integer; var URLTarg: TUrlTarget; var ATitle: ThtString): boolean;
var
  I: Integer;
begin
  Result := False;
  with Areas do
    for I := 0 to Count - 1 do
      if PtInRegion(THandle(Objects[I]), X, Y) then
      begin
        if Strings[I] <> '' then {could be NoHRef}
        begin
          URLTarg := TUrlTarget.Create;
          URLTarg.URL := Strings[I];
          URLTarg.Target := AreaTargets[I];
          ATitle := AreaTitles[I];
          Result := True;
        end;
        Exit;
      end;
end;

procedure TMapItem.AddArea(Attrib: TAttributeList);
const
  MAXCNT = 300;
var
  I, Cnt, Rad: Integer;
  HRef, S, Target, Title: ThtString;
  S1, Nm: ThtString;
  Coords: array[0..MAXCNT] of Integer;
  Rect: TRect absolute Coords;
  Handle: THandle;
  Shape: (Rec, Circle, Poly);

  procedure GetSubStr;
  var
    J, K: Integer;
  begin
    J := Pos(',', S);
    K := Pos(' ', S); {for non comma situations (bad syntax)}
    if (J > 0) and ((K = 0) or (K > J)) then
    begin
      S1 := copy(S, 1, J - 1);
      Delete(S, 1, J);
    end
    else if K > 0 then
    begin
      S1 := copy(S, 1, K - 1);
      Delete(S, 1, K);
    end
    else
    begin
      S1 := Trim(S);
      S := '';
    end;
    while (Length(S) > 0) and ((S[1] = ',') or (S[1] = ' ')) do
      Delete(S, 1, 1);
  end;

begin
  if Areas.Count >= 1000 then
    Exit;
  HRef := '';
  Target := '';
  Title := '';
  Shape := Rec;
  Cnt := 0;
  Handle := 0;
  for I := 0 to Attrib.Count - 1 do
    with TAttribute(Attrib[I]) do
      case Which of
        HRefSy: HRef := Name;
        TargetSy: Target := Name;
        TitleSy: Title := Name;
        NoHrefSy: HRef := '';
        CoordsSy:
          begin
            S := Trim(Name);
            Cnt := 0;
            GetSubStr;
            while (S1 <> '') and (Cnt <= MAXCNT) do
            begin
              Coords[Cnt] := StrToIntDef(S1, 0);
              GetSubStr;
              Inc(Cnt);
            end;
          end;
        ShapeSy:
          begin
            Nm := copy(Lowercase(Name), 1, 4);
            if Nm = 'circ' then
              Shape := Circle
            else if (Nm = 'poly') then
              Shape := Poly;
          end;
      end;
  case Shape of
    Rec:
      begin
        if Cnt < 4 then
          Exit;
        Inc(Coords[2]);
        Inc(Coords[3]);
        Handle := CreateRectRgnIndirect(Rect);
      end;
    Circle:
      begin
        if Cnt < 3 then
          Exit;
        Rad := Coords[2];
        Dec(Coords[0], Rad);
        Dec(Coords[1], Rad);
        Coords[2] := Coords[0] + 2 * Rad + 1;
        Coords[3] := Coords[1] + 2 * Rad + 1;
        Handle := CreateEllipticRgnIndirect(Rect);
      end;
    Poly:
      begin
        if Cnt < 6 then
          Exit;
        Handle := CreatePolygonRgn(Coords, Cnt div 2, Winding);
      end;
  end;
  if Handle <> 0 then
  begin
    Areas.AddObject(HRef, TObject(Handle));
    AreaTargets.Add(Target);
    AreaTitles.Add(Title);
  end;
end;

//function KindOfImageFile(FName: ThtString): ImageType;
//var
//  Mem: TMemoryStream;
//begin
//  Result := NoImage;
//  if FileExists(FName) then
//  begin
//    Mem := TMemoryStream.Create;
//    try
//      Mem.LoadFromFile(FName);
//      if Mem.Size >= 10 then
//        Result := KindOfImage(Mem.Memory);
//    finally
//      Mem.Free;
//    end;
//  end;
//end;

function KindOfImage(Start: Pointer): ImageType;
type
  ByteArray = array[0..10] of byte;
var
  PB: ^ByteArray absolute Start;
  PW: ^Word absolute Start;
  PL: ^DWord absolute Start;
begin
  if PL^ = $38464947 then
  begin
    if PB^[4] = Ord('9') then
      Result := Gif89
    else
      Result := Gif;
  end
  else if PW^ = $4D42 then
    Result := Bmp
  else if PL^ = $474E5089 then
    Result := Png
  else if PW^ = $D8FF then
    Result := Jpg
  else
    Result := NoImage;
end;

//{$A-} {record field alignment off for this routine}
//
//function IsTransparent(Stream: TStream; var Color: TColor): boolean;
//{Makes some simplifying assumptions that seem to be generally true for single
// images.}
//type
//  RGB = record
//    Red, Green, Blue: byte;
//  end;
//
//  GifHeader = record
//    GIF: array[0..2] of Ansichar;
//    Version: array[0..2] of Ansichar;
//    ScreenWidth, ScreenHeight: Word;
//    Field: Byte;
//    BackGroundColorIndex: byte;
//    AspectRatio: byte;
//  end;
//  ColorArray = array[0..255] of RGB;
//
//var
//  Header: ^GifHeader;
//  X: Integer;
//  Colors: ^ColorArray;
//  Buff: array[0..Sizeof(GifHeader) + Sizeof(ColorArray) + 8] of byte;
//  P: PByte;
//  OldPosition: Integer;
//
//begin
//  Result := False;
//  Fillchar(Buff, Sizeof(Buff), 0); {in case read comes short}
//  OldPosition := Stream.Position;
//  Stream.Position := 0;
//  Stream.Read(Buff, Sizeof(Buff));
//  Stream.Position := OldPosition;
//
//  Header := @Buff;
//  if KindOfImage(Header) <> Gif89 then
//    Exit;
//  Colors := @Buff[Sizeof(GifHeader)];
//  with Header^ do
//  begin
//    X := 1 shl ((Field and 7) + 1) - 1; {X is last item in color table}
//    if X = 0 then
//      Exit; {no main color table}
//  end;
//  P := PByte(PtrInt(Colors) + (X + 1) * Sizeof(RGB));
//  if (P^ <> $21) or (PByte(PtrInt(P) + 1)^ <> $F9) then
//    Exit; {extension block not found}
//  if (ord(PByteArray(P)[3]) and 1 <> 1) then
//    Exit; {no transparent color specified}
//
//  with Colors^[Ord(PByteArray(P)[6])] do
//    Color := Integer(Blue) shl 16 or Integer(Green) shl 8 or Integer(Red);
//  Result := True;
//end;
//
//{$A+}
//
//
//{$A-} {record field alignment off for this routine}
//
//
//function IsTransparentPng(Stream: TStream; var Color: TColor): boolean;
//type
//  RGB = record
//    Red, Green, Blue: byte;
//  end;
//
//  PngHeader = record
//    width: Integer;
//    height: Integer;
//    bitDepth: byte;
//    colorType: byte;
//    compression: byte;
//    filter: byte;
//    interlace: byte;
//  end;
//var
//  Header: PngHeader;
//  CRC: Integer;
//  OldPosition: Integer;
//  pngPalette: array[0..255] of RGB;
//  dataSize: Integer;
//  chunkType: array[0..4] of AnsiChar;
//  chunkTypeStr: ThtString;
//  done: Boolean;
//  Ar: array[0..10] of byte;
//  Alpha: array[0..255] of byte;
//  I: Integer;
//
//  function IntSwap(data: Integer): Integer;
//  var
//    byte0: Integer;
//    byte1: Integer;
//    byte2: Integer;
//    byte3: Integer;
//  begin
//    byte0 := data and $FF;
//    byte1 := (data shr 8) and $FF;
//    byte2 := (data shr 16) and $FF;
//    byte3 := (data shr 24) and $FF;
//
//    result := (byte0 shl 24) or (byte1 shl 16) or (byte2 shl 8) or byte3;
//  end;
//
//begin
//  result := false;
//  OldPosition := Stream.Position;
//
//  try
//    Stream.Position := 0;
//    Stream.Read(Ar, 8);
//
//    if KindOfImage(@Ar) <> Png then
//    begin
//      Stream.Position := OldPosition;
//      Exit;
//    end;
//
//    Stream.Position := 8; {past the PNG Signature}
//    done := False;
//
//{Read Chunks}
//    repeat
//      Stream.Read(dataSize, 4);
//      dataSize := IntSwap(dataSize);
//      Stream.Read(chunkType, 4);
//      chunkType[4] := #0; {make sure ThtString is NULL terminated}
//      chunkTypeStr := StrPas(chunkType);
//      if chunkTypeStr = 'IHDR' then
//      begin
//        Stream.Read(Header, DataSize);
//        Header.width := IntSwap(Header.width);
//        Header.height := IntSwap(Header.height);
//        Stream.Read(CRC, 4); {read it in case we need to read more}
//        if (Header.colorType < 2) or (Header.colorType > 3) then
//          done := True; {only type 2 and 3 use tRNS}
//      end
//      else if chunkTypeStr = 'PLTE' then
//      begin
//        Stream.Read(pngPalette, DataSize);
//        Stream.Read(CRC, 4); {read it in case we need to read more}
//      end
//      else if chunkTypeStr = 'tRNS' then
//      begin
//        if Header.colorType = 3 then
//        begin
//          {there can be DataSize transparent or partial transparent colors.  We only accept one fully transparent color}
//          Stream.Read(Alpha, DataSize);
//          for I := 0 to DataSize - 1 do
//            if Alpha[I] = 0 then {0 means full transparency}
//            begin
//              with pngPalette[I] do
//                Color := Integer(Blue) shl 16 or Integer(Green) shl 8 or Integer(Red);
//              Result := True;
//              break;
//            end;
//        end
//        else {has to have been 2}
//        begin
//            {for now I am ignoring this since I can't make one}
//        end;
//        done := true; {got everything we need at this point}
//      end
//      else if chunkTypeStr = 'IDAT' then
//        done := True {if this chunk is hit there is no tRNS}
//      else
//        Stream.Position := Stream.Position + dataSize + 4; {additional 4 for the CRC}
//      if Stream.Position >= Stream.Size then
//        Done := True;
//    until done = True;
//  except
//  end;
//
//  Stream.Position := OldPosition;
//end;
//
//{$A+}
//
//function TransparentGIF(const FName: ThtString; var Color: TColor): boolean;
//{Looks at a GIF image file to see if it's a transparent GIF.}
//{Needed for OnBitmapRequest Event handler}
//var
//  Stream: TFileStream;
//begin
//  Result := False;
//  try
//    Stream := TFileStream.Create(FName, fmShareDenyWrite or FmOpenRead);
//    try
//      Result := IsTransparent(Stream, Color);
//    finally
//      Stream.Free;
//    end;
//  except
//  end;
//end;

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
  if not Assigned(Bitmap) then
  begin
    Result := nil;
    Exit;
  end;

  if ColorBits > 8 then
  begin
    if Bitmap.PixelFormat <= pf8bit then
      Result := DIBConvert
    else
      Result := Bitmap;
    Exit;
  end;

  if Bitmap.HandleType = bmDIB then
  begin
    Result := GetBitmap(Bitmap);
    Bitmap.Free;
    Exit;
  end;
  Result := DIBConvert;
end;

//{----------------GetImageAndMaskFromFile}
//
//function GetImageAndMaskFromFile(const Filename: ThtString; var Transparent: Transparency;
//  var Mask: TBitmap): TgpObject;
//var
//  Stream: TMemoryStream;
//begin
//  Result := nil;
//  Mask := nil;
//  if not FileExists(Filename) then
//    Exit;
//  {$IFNDEF NoGDIPlus}
//  if GDIPlusActive and (KindOfImageFile(Filename) = Png) then
//  begin
//    Result := TObject(TGPBitmap.Create(Filename));
//  end
//  else
//  {$ENDIF !NoGDIPlus}
//  begin
//    Stream := TMemoryStream.Create;
//    Stream.LoadFromFile(Filename);
//    try
//      Result := GetImageAndMaskFromStream(Stream, Transparent, Mask);
//    finally
//      Stream.Free;
//    end;
//  end;
//end;

{----------------GetBitmapAndMaskFromStream}

function GetBitmapAndMaskFromStream(Stream: TMemoryStream;
  var Transparent: Transparency; var AMask: TBitmap): TBitmap;
var
  jpImage: TJpegImage;
{$ifdef LCL}
  pngImage: TPortableNetworkGraphic;
{$endif}
begin
  Result := nil;
  AMask := nil;
  if not Assigned(Stream) or (Stream.Memory = nil) or (Stream.Size < 20) then
    Exit;
  Stream.Position := 0;
  try
    case KindOfImage(Stream.Memory) of
      Jpg:
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
          Result.Assign(jpImage);
        finally
          jpImage.Free;
        end;
      end;

      Png:
      begin
{$ifdef LCL}
        pngImage := TPortableNetworkGraphic.Create;
        try
          Transparent := TrPng;
          pngImage.LoadFromStream(Stream);
          if ColorBits <= 8 then
            pngImage.PixelFormat := pf8bit
          else
            pngImage.PixelFormat := pf24bit;
          Result := TBitmap.Create;
          Result.Assign(pngImage);
          pngImage.Mask(clDefault);
          if pngImage.MaskHandleAllocated then
          begin
            AMask := TBitmap.Create;
            AMask.LoadFromBitmapHandles(pngImage.MaskHandle, 0);
          end;
        finally
          pngImage.Free;
        end;
{$endif}
      end;

      Bmp:
      begin
        Result := TBitmap.Create;
        Result.LoadFromStream(Stream);
      end;
    end;

    if Transparent = LLCorner then
      AMask := GetImageMask(Result, False, 0);
    Result := ConvertImage(Result);
  except
    freeAndNil(Result);
    freeAndNil(AMask);
  end;
end;

var
  Unique: Integer = 183902;

{----------------GetImageAndMaskFromStream}

function GetImageAndMaskFromStream(Stream: TMemoryStream;
  var Transparent: Transparency; var AMask: TBitmap): TgpObject;
{$IFNDEF NoGDIPlus}
var
  Filename: string;
  Path: PChar;
  F: file;
  I: Integer;
  {$ENDIF !NoGDIPlus}
begin
  Result := nil;
  AMask := nil;
  if not Assigned(Stream) or (Stream.Memory = nil) or (Stream.Size < 20) then
    Exit;
  Stream.Position := 0;

  {$IFNDEF NoGDIPlus}
  if GDIPlusActive and (KindOfImage(Stream.Memory) = png) then
  begin
    try
      Path := StrAlloc(MAX_PATH);
      try
        GetTempPath(Max_Path, Path);
        SetLength(Filename, Max_Path+1);
        GetTempFilename(Path, 'png', Unique, PChar(Filename));
      finally
        StrDispose(Path);
      end;
      Inc(Unique);
      I := Pos(#0, Filename);
      SetLength(Filename, I - 1);
      AssignFile(F, Filename);
      ReWrite(F, 1);
      BlockWrite(F, Stream.Memory^, Stream.Size);
      CloseFile(F);
      Result := TgpImage.Create(Filename, True); {True because it's a temporary file}
      Transparent := NotTransp;
    except
    end;
    Exit;
  end;
  {$ENDIF !NoGDIPlus}

  Result := GetBitmapAndMaskFromStream(Stream, Transparent, AMask);
{$IFNDEF NoMetafile}
  if not Assigned(Result) then
  begin
    Result := ThtMetafile.Create;
    try
      AMask := nil;
      Transparent := NotTransp;
      ThtMetaFile(Result).LoadFromStream(Stream);
    except
      FreeAndNil(Result);
    end;
  end;
{$ENDIF}
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

//function GetImageFromFile(const Filename: ThtString): TBitmap;
//{used only externally in OnBitmapRequest handler}
//var
//  IT: ImageType;
//  Mask: TBitmap;
//  Transparent: Transparency;
//  Stream: TMemoryStream;
//  GpObj: TGpObject;
//
//  function GetGif: TBitmap;
//  var
//    TmpGif: TGifImage;
//    NonAnimated: boolean;
//  begin
//    Result := nil;
//    TmpGif := CreateAGifFromStream(NonAnimated, Stream);
//    if Assigned(TmpGif) then
//    begin
//      Result := TBitmap.Create;
//      try
//        Result.Assign(TmpGif.Bitmap);
//      except
//        Result.Free;
//        Result := nil;
//      end;
//      TmpGif.Free;
//    end
//  end;
//
//begin
//  Result := nil;
//  try
//    Stream := TMemoryStream.Create;
//    try
//      Stream.LoadFromFile(Filename);
//      IT := KindOfImage(Stream.Memory);
//      if IT in [Gif, Gif89] then
//        Result := GetGif
//      else
//      begin
//        GpObj := GetImageAndMaskFromStream(Stream, Transparent, Mask);
//        Mask.Free;
//        if GpObj is TBitmap then
//          Result := TBitmap(GpObj)
//{$IFNDEF NoMetafile}
//        else if GpObj is ThtMetafile then
//        begin
//          Result := TBitmap.Create;
//          Result.Assign(ThtMetafile(GpObj).WhiteBGBitmap);
//          GpObj.Free;
//        end
//{$ENDIF}
//        {$IFNDEF NoGDIPlus}
//        else if GpObj is TGpImage then
//        begin
//          Result := TBitmap.Create;
//          Result.Assign(TGpImage(GpObj).GetTBitmap);
//          GpObj.Free;
//        end;
//        {$ENDIF !NoGDIPlus}
//      end;
//    finally
//      Stream.Free;
//    end;
//  except
//    Result := nil;
//  end;
//end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function LoadImageFromStream(Stream: TMemoryStream; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
// extracted from TSectionList.GetTheBitmap(), TSectionList.InsertImage(), and TSectionList.ReplaceImage()
var
  NonAnimated: boolean;
  Tmp: TGifImage;
begin
  Result := nil;
  if (Stream <> nil) and (Stream.Size > 0) then
  begin
    NonAnimated := True;
    if KindOfImage(Stream.Memory) in [GIF, Gif89] then
      Result := CreateAGifFromStream(NonAnimated, Stream);
    if Result <> nil then
    begin
      if NonAnimated then
      begin {else already have animated GIF}
        Tmp := TGifImage(Result);
        Result := TBitmap.Create;
        TBitmap(Result).Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
        begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TrGif;
        end
        else if Transparent = LLCorner then
          AMask := GetImageMask(TBitmap(Result), False, 0);
        Tmp.Free;
      end;
    end
    else
      Result := GetImageAndMaskFromStream(Stream, Transparent, AMask);
  end;
end;

//-- BG ---------------------------------------------------------- 26.09.2010 --
function LoadImageFromFile(const FName: ThtString; var Transparent: Transparency; var AMask: TBitmap): TgpObject;
// extracted from TSectionList.GetTheBitmap() and redesigned.
// Now the image file is loaded once only (was: 2 to 3 times) and GetImageAndMaskFromFile() is obsolete.
var
  Stream: TMemoryStream;
begin {look for the image file}
  Result := nil;
  if FileExists(FName) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromFile(FName);
      Result := LoadImageFromStream(Stream, Transparent, AMask);
    finally
      Stream.Free;
    end;
  end;
end;

{----------------FinishTransparentBitmap }

procedure FinishTransparentBitmap(ahdc: HDC;
  InImage, Mask: TBitmap; xStart, yStart, W, H: Integer);
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
  BM: Windows.TBitmap;
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
      SetStretchBltMode(ahDC, ColorOnColor);
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
  BM: Windows.TBitmap;
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

procedure TDib.DrawDIB(DC: HDC; X: Integer; Y: Integer; W, H: Integer;
  ROP: DWord);
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

{----------------TIndentManager.Create}

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

procedure TIndentManager.Clear;
begin
  R.Clear;
  L.Clear;
  CurrentID := nil;
end;

{----------------TIndentManager.Reset}

procedure TIndentManager.Reset(Lf, Rt: Integer);
begin
  LfEdge := Lf;
  RtEdge := Rt;
  CurrentID := nil;
end;

procedure TIndentManager.Update(Y: Integer; Img: TFloatingObj);
{Given a new floating image, update the edge information.  Fills  Img.Indent,
 the distance from the left edge to the upper left corner of the image}
var
  IH, IW: Integer;
  IR: IndentRec;
  LIndent: Integer;
begin
  if Assigned(Img) then
  begin
    IW := Img.ImageWidth + Img.HSpaceL + Img.HSpaceR;
    IH := Img.ImageHeight + Img.VSpaceT + Img.VSpaceB;
    if (Img.HorzAlign = ALeft) then
    begin
      IR := IndentRec.Create;
      with IR do
      begin
        LIndent := LeftIndent(Y);
        Img.Indent := LIndent - LfEdge + Img.HSpaceL;
        X := LIndent - LfEdge + IW;
        YT := Y;
        YB := Y + IH;
        L.Add(IR);
      end;
    end
    else if (Img.HorzAlign = ARight) then
    begin
      IR := IndentRec.Create;
      with IR do
      begin
        X := RightSide(Y) - IW;
        Img.Indent := X + Img.HSpaceL;
        YT := Y;
        YB := Y + IH;
        R.Add(IR);
      end;
    end;
  end;
end;

procedure TIndentManager.UpdateBlock(Y: Integer; IW: Integer; IH: Integer;
  Justify: AlignmentType);
{For a floating block, update the edge information. }
var
  IR: IndentRec;
begin
  IR := IndentRec.Create;
  if (Justify = ALeft) then
  begin
    with IR do
    begin
      X := -LfEdge + IW;
      YT := Y;
      YB := Y + IH;
      Float := True; //ID := CurrentID;
      L.Add(IR);
    end;
  end
  else if (Justify = ARight) then
  begin
    with IR do
    begin
      X := RightSide(Y) - IW;
      YT := Y;
      YB := Y + IH;
      Float := True; //ID := CurrentID;
      R.Add(IR);
    end;
  end;
end;

procedure TIndentManager.UpdateTable(Y: Integer; IW: Integer; IH: Integer;
  Justify: JustifyType);
{Given a floating table, update the edge information. }
var
  IR: IndentRec;
begin
  IR := IndentRec.Create;
  if (Justify = Left) then
  begin
    with IR do
    begin
      X := -LfEdge + IW;
      YT := Y;
      YB := Y + IH;
      L.Add(IR);
    end;
  end
  else if (Justify = Right) then
  begin
    with IR do
    begin
      X := RightSide(Y) - IW;
      YT := Y;
      YB := Y + IH;
      R.Add(IR);
    end;
  end;
end;

const
  BigY = 9999999;

function TIndentManager.LeftIndent(Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -99999;
  for I := 0 to L.Count - 1 do
    with IndentRec(L.Items[I]) do
    begin
      if (Y >= YT) and (Y < YB) and (Result < X) then
        if not Assigned(ID) or (ID = CurrentID) then
          Result := X;
    end;
  if Result = -99999 then
    Result := LfEdge
  else
    Inc(Result, LfEdge);
end;

function TIndentManager.RightSide(Y: Integer): Integer;
{returns the current right side dimension as measured from the left, a positive
 number}
var
  I: Integer;
  IR: IndentRec;
begin
  Result := 99999;
  for I := 0 to R.Count - 1 do
  begin
    IR := IndentRec(R.Items[I]);
    with IR do
      if (Y >= YT) and (Y < YB) and (Result > X) then
        if not Assigned(ID) or (ID = CurrentID) then
          Result := X;
  end;
  if Result = 99999 then
    Result := RtEdge
  else
    Inc(Result, LfEdge);
end;

function TIndentManager.ImageBottom: Integer;
{finds the bottom of the last floating image}
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to L.Count - 1 do
    with IndentRec(L.Items[I]) do
      if not Assigned(ID) and (YB > Result) then
        Result := YB;
  for I := 0 to R.Count - 1 do
    with IndentRec(R.Items[I]) do
      if not Assigned(ID) and (YB > Result) then
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
      if not Assigned(ID) and (YB > CL) then
        CL := YB;
  CR := -1;
  for I := 0 to R.Count - 1 do
    with IndentRec(R.Items[I]) do
      if not Assigned(ID) and (YB > CR) then
        CR := YB;
  Inc(CL);
  Inc(CR);
end;

//-- BG ---------------------------------------------------------- 08.06.2008 --
function TIndentManager.GetNextLeftXY(var Y: Integer; X, ThisWidth, MaxWidth, MinIndex: Integer): Integer;
var
  Index: Integer;
  Indent: IndentRec;
  DummyCR: Integer;
begin
  if X < 0 then
  begin
    dec(X, Auto);
    inc(MaxWidth, 2 * Auto);
  end;
  Index := L.Count - 1;
  if Index >= MinIndex then
  begin
    Indent := IndentRec(L[Index]);
    // set y to previous used y == y of current line
    Y := Max(Y, Indent.YT);
    Result := Max(X, Indent.X);
  end
  else
    Result := Max(X, LeftIndent(Y));
  if Result + ThisWidth > MaxWidth + X then
  begin
    Result := X;
    GetClearY(Y, DummyCR);
  end;
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

const
  DefaultBitmap = 1002;
  ErrBitmap = 1001;
  ErrBitmapMask = 1005;
  Hand_Cursor = 1003;
  ThickIBeam_Cursor = 1006;

procedure ThisExit; far;
begin
  DefBitMap.Free;
  ErrorBitMap.Free;
  ErrorBitMapMask.Free;
  WaitStream.Free;
end;

{----------------TIDNameList}

constructor TIDNameList.Create(List: TSectionBaseList);
begin
  inherited Create;
  Sorted := True;
  OwnerList := List;
end;

destructor TIDNameList.Destroy;
begin
  Clear;
  inherited
end;

procedure TIDNameList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  try
    if Objects[I] is TChPosObj then
      Objects[I].Free;
  except
  end;
  inherited Clear;
end;

function TIDNameList.AddObject(const S: ThtString; AObject: TObject): Integer;
var
  I: Integer;
begin
  if Find(S, I) then
  begin
    try
      if Objects[I] is TChPosObj then
        Objects[I].Free;
    except
    end;
    Delete(I);
  end;
  Result := inherited AddObject(S, AObject);
end;

procedure TIDNameList.AddChPosObject(const S: ThtString; Pos: Integer);
var
  ChPosObj: TChPosObj;
begin
  ChPosObj := TChPosObj.Create;
  ChPosObj.FList := OwnerList;
  ChPosObj.FChPos := Pos;
  AddObject(S, ChPosObj);
end;

{----------------TChPosObj.GetYPosition:}

function TChPosObj.GetYPosition: Integer;
var
  Pos, X, Y: Integer;
begin
  Pos := List.FindDocPos(ChPos, False);
  if List.CursorToXY(nil, Pos, X, Y) then
    Result := Y
  else
    Result := 0;
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
{----------------TokenObj.Create}

constructor TokenObj.Create;
begin
  inherited;
  GetMem(C, TokenLeng * Sizeof(WideChar));
  GetMem(I, TokenLeng * Sizeof(Integer));
  MaxIndex := TokenLeng;
  Leng := 0;
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
  if Leng >= MaxIndex then
  begin
    ReallocMem(C, (MaxIndex + 50) * Sizeof(WideChar));
    ReallocMem(I, (MaxIndex + 50) * Sizeof(Integer));
    Inc(MaxIndex, 50);
  end;
  Inc(Leng);
  C^[Leng] := Ch;
  I^[Leng] := Ind;
  StringOK := False;
end;

procedure TokenObj.Clear;
begin
  Leng := 0;
  St := '';
  StringOK := True;
end;

function MultibyteToWideString(CodePage: Integer; const S: AnsiString; Len: Integer): WideString;
{$IFNDEF UNICODE}
var
  NewLen: Integer;
{$ENDIF}
begin
{$IFDEF UNICODE}
  if Len >= 0 then
    Result := Copy(S, 1, Len)
  else
    Result := S;
{$ELSE}
{$IFDEF Delphi6_Plus}
  if IsWin95 and (CodePage = CP_UTF8) then
  begin
  {Provide initial space. The resulting ThtString will never be longer than the
   UTF-8 encoded ThtString.}
    if Len = -1 then
      Len := Length(S);
    SetLength(Result, Len + 1); {add 1 for #0 terminator}
    NewLen := UTF8ToUnicode(PWideChar(Result), Len + 1, PAnsiChar(S), Len) - 1; {subtr 1 as don't want to count null terminator}
  end
  else
{$ENDIF}
  begin
  {Provide initial space. The resulting ThtString will never be longer than the
   UTF-8 or multibyte encoded ThtString.}
    SetLength(Result, 2 * Len);
    NewLen := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), Len, PWideChar(Result), Len);
    if NewLen = 0 then
    { Invalid code page. Try default.}
      NewLen := MultiByteToWideChar(CP_ACP, 0, PAnsiChar(S), Len, PWideChar(Result), Len);
  end;
  SetLength(Result, NewLen);
{$ENDIF}
end;

function WideStringToMultibyte(CodePage: Integer; W: WideString): Ansistring;
var
  NewLen, Len: Integer;
begin
{$IFDEF Delphi6_Plus}
  if CodePage = CP_UTF8 then {UTF-8 encoded ThtString.}
    Result := UTF8Encode(W)
  else
{$ENDIF}
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
    P1 := CharNextExA(CodePage, P, 0);
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

procedure TokenObj.AddString(S: TCharCollection; CodePage: Integer);
// Takes the given ThtString S and converts it to Unicode using the given code page.
// If we are on Windows 95 then CP_UTF8 (and CP_UTF7) are not supported.
// We compensate for this by using a Delphi function.
// Note: There are more code pages (including CP_UTF7), which are not supported
// on all platforms. These are rather esoteric and therefore not considered here.

var
  WS: WideString;
  I, J, N,
    Len, NewLen: Integer;

begin
  Len := S.FCurrentIndex;
{$ifdef UseUnicode}
  if Len > 0 then
    WS := Copy(S.FChars, 1, Len)
  else
    WS := S.FChars;
{$else}
  WS := MultibyteToWideString(CodePage, S.FChars, Len);
{$endif}
  NewLen := Length(WS);

  {Store the wide ThtString and character indices.}
  if Len = NewLen then {single byte character set or at least no multibyte conversion}
    for I := 1 to NewLen do
      AddUnicodeChar(WS[I], S.FIndices[I])
  else
  begin {multibyte character set}
    J := 1;
    for I := 1 to NewLen do
    begin
      AddUnicodeChar(WS[I], S.FIndices[J]);
      {find index for start of next character}
      N := ByteNum(CodePage, @S.FChars[J]);
      if N > 0 then
        J := J + N
      else
        Break;
    end;
  end;
end;

procedure TokenObj.Concat(T: TokenObj);
var
  K: Integer;
begin
  K := Leng + T.Leng;
  if K > MaxIndex then
  begin
    ReallocMem(C, (K + 50) * Sizeof(WideChar));
    ReallocMem(I, (K + 50) * Sizeof(Integer));
    MaxIndex := K + 50;
  end;
  Move(T.C^, C^[Leng + 1], T.Leng * Sizeof(WideChar));
  Move(T.I^, I^[Leng + 1], T.Leng * Sizeof(Integer));
  Leng := K;
  StringOK := False;
end;

procedure TokenObj.Remove(N: Integer);
begin {remove a single character}
  if N <= Leng then
  begin
    Move(C^[N + 1], C^[N], (Leng - N) * Sizeof(WideChar));
    Move(I^[N + 1], I^[N], (Leng - N) * Sizeof(Integer));
    if StringOK then
      Delete(St, N, 1);
    Dec(Leng);
  end;
end;

procedure TokenObj.Replace(N: Integer; Ch: WideChar);
begin {replace a single character}
  if N <= Leng then
  begin
    C^[N] := Ch;
    if StringOK then
      St[N] := Ch;
  end;
end;

function TokenObj.GetString: WideString;
begin
  if not StringOK then
  begin
    SetLength(St, Leng);
    Move(C^, St[1], SizeOf(WideChar) * Leng);
    StringOK := True;
  end;
  Result := St;
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
        ScanLineInc := Integer(bmp.ScanLine[1]) - Integer(ScanLinePtr)
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
        Inc(Integer(ScanLinePtr), ScanLineInc);
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
    NewBitmap := TGpImage(Image).GetTBitmap
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


procedure PrintBitmap(Canvas: TCanvas; X, Y, W, H: Integer; Bitmap: TBitmap);
{Y relative to top of display here}
{$ifdef LCL}
{$else}
var
  OldPal: HPalette;
  DC: HDC;
  Info: PBitmapInfo;
  Image: AllocRec;
  ImageSize: DWord;
  InfoSize: DWord;
{$endif}
begin
{$ifdef LCL}
  Canvas.StretchDraw(Rect(X, Y, W, H), Bitmap);
{$else}
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
  S: htBorderStyleArray; BGround: TColor; Print: boolean);
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

  function Darker(Color: TColor): TColor;
  {find a somewhat darker color for shading purposes}
  const
    F = 0.75; // F < 1 makes color darker
  var
    Red, Green, Blue: Byte;
  begin
    if Color < 0 then
      Color := GetSysColor(Color and $FFFFFF)
    else
      Color := Color and $FFFFFF;
    Red := Color and $FF;
    Green := (Color and $FF00) shr 8;
    Blue := (Color and $FF0000) shr 16;
    Result := RGB(Round(F * Red), Round(F * Green), Round(F * Blue));
  end;

  function Lighter(Color: TColor): TColor;
  {find a somewhat lighter color for shading purposes}
  const
    F = 1.15; // F > 1 makes color lighter
  var
    Red, Green, Blue: Byte;
  begin
    if Color < 0 then
      Color := GetSysColor(Color and $FFFFFF)
    else
      Color := Color and $FFFFFF;
    if Color = 0 then
      Result := 0
    else
    begin
      Red := Color and $FF;
      Green := (Color and $FF00) shr 8;
      Blue := (Color and $FF0000) shr 16;
      Result := RGB(Min(255, Round(F * Red)), Min(255, Round(F * Green)), Min(255, Round(F * Blue)));
    end;
  end;

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
          Windows.movetoEx(Canvas.Handle, PM[I].x, PM[I].y, nil);
          Start := I;
          InPath := True;
        end;
        Windows.LineTo(Canvas.Handle, PM[(I + 1) mod 4].x, PM[(I + 1) mod 4].y);
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

{----------------TSectionBase.Create}

constructor TSectionBase.Create(AMasterList: TSectionBaseList; ADisplay: TPropDisplay);
begin
  inherited Create;
  FParentSectionList := AMasterList;
  FDisplay := ADisplay;
  ContentTop := 999999999; {large number in case it has Display: none; }
end;

//-- BG ---------------------------------------------------------- 20.09.2009 --
constructor TSectionBase.Create(AMasterList: TSectionBaseList; AProp: TProperties);
begin
  create(AMasterList, AProp.Display);
end;

constructor TSectionBase.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
begin
  inherited Create;
  FParentSectionList := AMasterList;
  SectionHeight := T.SectionHeight;
  ZIndex := T.ZIndex;
end;

procedure TSectionBase.CopyToClipboard;
begin
end;

function TSectionBase.GetYPosition: Integer;
begin
  Result := ContentTop;
end;

{ TSectionBase }

function TSectionBase.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
begin
  StartCurs := Curs;
  Result := SectionHeight;
  DrawHeight := SectionHeight;
  MaxWidth := 0;
  ContentTop := Y;
  DrawTop := Y;
  YDraw := Y;
  ContentBot := Y + SectionHeight;
  DrawBot := Y + DrawHeight;
end;

function TSectionBase.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  Y: Integer;
begin
  Y := YDraw;
  Result := Y + SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj};
  var ATitle: ThtString): guResultType;
begin
  Result := [];
end;

function TSectionBase.PtInObject(X: Integer; Y: Integer; var Obj: TObject;
  var IX, IY: Integer): boolean;
begin
  Result := False;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
  var XR: Integer; var YR: Integer; var CaretHt: Integer;
  var Intext: boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindSourcePos(DocPos: Integer): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer;
  var Y: Integer): boolean;
begin
  Result := False;
end;

function TSectionBase.GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
  Result := False;
end;

procedure TSectionBase.SetParent(List: TSectionBaseList);
begin
  FParentSectionList := List;
end;

procedure TSectionBase.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
begin
  Min := 0; Max := 0;
end;

procedure TSectionBase.AddSectionsToList;
begin
  ParentSectionList.addSectionsToPositionList(Self);
end;

{ TSectionBaseList }

function TSectionBaseList.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].CursorToXY(Canvas, Cursor, X, Y);
    if Result then
      Break;
  end;
end;

function TSectionBaseList.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not Prev then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].FindDocPos(SourcePos, Prev);
      if Result >= 0 then
        Break;
    end
  else {Prev, iterate backwards}
    for I := Count - 1 downto 0 do
    begin
      Result := Items[I].FindDocPos(SourcePos, Prev);
      if Result >= 0 then
        Break;
    end
end;

function TSectionBaseList.getItem(Index: Integer): TSectionBase;
begin
  Result := inherited Items[Index];
end;

//-- BG ---------------------------------------------------------- 11.09.2010 --
procedure TSectionBaseList.AddSectionsToPositionList(Sections: TSectionBase);
begin
  // overridden by TSectionList.
end;

{ TFloatingObj }

constructor TFloatingObj.CreateCopy(T: TFloatingObj);
begin
  inherited Create;
  FAlt := T.FAlt;
  FAltW := T.FAltW;
  ImageWidth := T.ImageWidth;
  ImageHeight := T.ImageHeight;
  NoBorder := T.NoBorder;
  BorderSize := T.BorderSize;
  Indent := T.Indent;
  VertAlign := T.VertAlign;
  HorzAlign := T.HorzAlign;
  HSpaceL := T.HSpaceL;
  HSpaceR := T.HSpaceR;
  VSpaceT := T.VSpaceT;
  VSpaceB := T.VSpaceB;
  Pos := T.Pos;
end;

function TFloatingObj.GetYPosition: Integer;
begin
  Result := DrawYY;
end;

procedure TFloatingObj.ProcessProperties(Prop: TProperties);
const
  DummyHtWd = 200;
var
  MargArrayO: TVMarginArray;
  MargArray: TMarginArray;
  Align: AlignmentType;
  EmSize, ExSize: Integer;
begin
  if Prop.GetVertAlign(Align) then
    VertAlign := Align;
  if Prop.GetFloat(Align) and (Align <> ANone) then
  begin
    if HSpaceR = 0 then
    begin {default is different for Align = left/right}
      HSpaceR := ImageSpace;
      HSpaceL := ImageSpace;
    end;
    HorzAlign := Align;
  end;
  if ImageTitle = '' then {a Title attribute will have higher priority than inherited}
    ImageTitle := Prop.PropTitle;
  Prop.GetVMarginArray(MargArrayO);
  EmSize := Prop.EmSize;
  ExSize := Prop.ExSize;
  ConvInlineMargArray(MargArrayO, DummyHtWd, DummyHtWd, EmSize, ExSize, MargArray);

  if MargArray[MarginLeft] <> IntNull then
    HSpaceL := MargArray[MarginLeft];
  if MargArray[MarginRight] <> IntNull then
    HSpaceR := MargArray[MarginRight];
  if MargArray[MarginTop] <> IntNull then
    VSpaceT := MargArray[MarginTop];
  if MargArray[MarginBottom] <> IntNull then
    VSpaceB := MargArray[MarginBottom];

  if Prop.GetBorderStyle <> bssNone then
  begin
    Inc(HSpaceL, MargArray[BorderLeftWidth]);
    Inc(HSpaceR, MargArray[BorderRightWidth]);
    Inc(VSpaceT, MargArray[BorderTopWidth]);
    Inc(VSpaceB, MargArray[BorderBottomWidth]);
  end;

  if MargArray[Width] <> IntNull then
  begin
    PercentWidth := False;
    if MargArray[Width] = Auto then
      SpecWidth := -1
    else if (VarIsStr(MargArrayO[Width]))
      and (System.Pos('%', MargArrayO[Width]) > 0) then
    begin
      PercentWidth := True;
      SpecWidth := MulDiv(MargArray[Width], 100, DummyHtWd);
    end
    else
      SpecWidth := MargArray[Width];
  end;
  if MargArray[Height] <> IntNull then
  begin
    PercentHeight := False;
    if MargArray[Height] = Auto then
      SpecHeight := -1
    else if (VarIsStr(MargArrayO[Height]))
      and (System.Pos('%', MargArrayO[Height]) > 0) then
    begin
      PercentHeight := True;
      SpecHeight := MulDiv(MargArray[Height], 100, DummyHtWd);
    end
    else
      SpecHeight := MargArray[Height];
  end;

  if Prop.GetVertAlign(Align) then
    VertAlign := Align;
  if Prop.GetFloat(Align) and (Align <> ANone) then
    HorzAlign := Align;
  if Prop.BorderStyleNotBlank then
  begin
    NoBorder := True; {will have inline border instead}
    BorderSize := 0;
  end;
end;

//-- BG ---------------------------------------------------------- 30.11.2010 --
procedure TFloatingObj.SetAlt(CodePage: Integer; const Value: ThtString);
begin
{$ifdef UseUnicode}
  FAltW := Value;
  while (Length(FAltW) > 0) and (FAltW[Length(FAltW)] in [CrChar, LfChar]) do
    Delete(FAltW, Length(FAltW), 1);
  FAlt := WideStringToMultibyte(CodePage, FAltW);
{$else}
  FAlt := Value;
  while (Length(FAlt) > 0) and (FAlt[Length(FAlt)] in [CrChar, LfChar]) do
    Delete(FAlt, Length(FAlt), 1);
  FAltW := MultibyteToWideString(CodePage, FAlt);
{$endif}
end;

{$R HTML32.Res}

initialization

  DefBitMap := TBitmap.Create;
  DefBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(DefaultBitmap));
  ErrorBitMap := TBitmap.Create;
  ErrorBitMap.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmap));
  ErrorBitMapMask := TBitmap.Create;
  ErrorBitMapMask.Handle := LoadBitmap(HInstance, MakeIntResource(ErrBitmapMask));
  Screen.Cursors[HandCursor] := LoadCursor(HInstance, MakeIntResource(Hand_Cursor));
  Screen.Cursors[UpDownCursor] := LoadCursor(HInstance, 'UPDOWNCURSOR');
  Screen.Cursors[UpOnlyCursor] := LoadCursor(HInstance, 'UPONLYCURSOR');
  Screen.Cursors[DownOnlyCursor] := LoadCursor(HInstance, 'DOWNONLYCURSOR');

  WaitStream := TMemoryStream.Create;

finalization
  ThisExit;
end.

