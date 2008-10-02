
{Version 9.45}
{*********************************************************}
{*                     HTMLSUBS.PAS                      *}
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


{
This module is comprised mostly of the various Section object definitions.
As the HTML document is parsed, it is divided up into sections.  Some sections
are quite simple, like TParagraphSpace.  Others are more complex such as
TSection which can hold a complete paragraph.

The HTML document is then stored as a list, TSectionList, of the various
sections.

Closely related to TSectionList is TCell.  TCell holds the list of sections for
each cell in a Table (the ThtmlTable section).  In this way each table cell may
contain a document of it's own.

The Section objects each store relevant data for the section such as the text,
fonts, images, and other info needed for formating.

Each Section object is responsible for its own formated layout.  The layout is
done in the DrawLogic method.  Layout for the whole document is done in the
TSectionList.DoLogic method which essentially just calls all the Section
DrawLogic's.  It's only necessary to call TSectionList.DoLogic when a new
layout is required (when the document is loaded or when its width changes).

Each Section is also responsible for drawing itself (its Draw method).  The
whole document is drawn with the TSectionList.Draw method.
}

unit Htmlsubs;
{$R HTML32.Res}

interface
uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, HTMLUn2, HTMLGif2, mmSystem,
  {$ifdef UseTNT}
  TntStdCtrls,
  {$endif}
  {$ifdef UseElPack}
  ElListBox, ElCombos, ElEdits, ElPopBtn,
  {$endif}
  StyleUn;

type
  {$ifdef UseTNT}
  ThtEdit = TTntEdit;
  ThtButton = TTntButton;
  ThtMemo = TTntMemo;
  ThtCombobox = TTntCombobox;
  ThtListbox = TTntListbox;
  {$else}
    {$ifdef UseElPack}
    ThtEdit = TElEdit;
    ThtButton = TElPopupButton;
    ThtMemo = TElMemo;
    ThtCombobox = TElCombobox;
    ThtListbox = TElListbox;
    {$else}
    ThtEdit = TEdit;
    ThtButton = TButton;
    ThtMemo = TMemo;
    ThtCombobox = TCombobox;
    ThtListbox = TListbox;
    {$endif}
  {$endif}

  ThvPanel = Class(TPanel)
  public
    FVisible: boolean;
    procedure SetVisible(Value: boolean);
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  TLinkDrawnEvent = procedure(Sender: TObject; Page: integer; const Url, Target: string;
                        ARect: TRect) of Object;
  TFileBrowseEvent = procedure(Sender, Obj: TObject; var S: string) of Object;
  TGetBitmapEvent = procedure(Sender: TObject; const SRC: string;
                    var Bitmap: TBitmap; var Color: TColor) of Object;
  TGetImageEvent = procedure(Sender: TObject; const SRC: string;
                    var Stream: TMemoryStream) of Object;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: string;
                    Results: TStringList) of Object;
  TPanelCreateEvent = procedure(Sender: TObject; const AName, AType, SRC: string;
                        Panel: ThvPanel) of Object;
  TPanelDestroyEvent = procedure(Sender: TObject; Panel: ThvPanel) of Object;
  TPanelPrintEvent = procedure(Sender: TObject; Panel: ThvPanel; const Bitmap: TBitmap) of Object;
  TObjectTagEvent = procedure(Sender: TObject; Panel: ThvPanel;
                 const Attributes, Params: TStringList;
                        var WantPanel: boolean) of Object;
  TObjectClickEvent = procedure(Sender, Obj: TObject; const OnClick: string) of Object;
  ThtObjectEvent = procedure(Sender, Obj: TObject; const Attribute: string) of Object;
  TExpandNameEvent = procedure(Sender: TObject; const SRC: string; var Result: string) of Object;
  guResultType = set of (guUrl, guControl, guTitle);
  TCell = Class;
  TBlockCell = Class;
  TCellBasic = Class;
  TSectionList = Class;
  TSection = Class;
  TBlock = Class;

  ThtTabcontrol = class(TWinControl)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    property OnEnter;
    property OnExit;
    property TabStop;
    property OnKeyUp;
  public
    destructor Destroy; override;
  end;

  TFontObj = class(TObject)   {font information}
  private
    Section: TSection;
    FVisited, FHover: boolean;
    Title: string;
    FYValue: integer;
    Active: boolean;
    procedure SetVisited(Value: boolean);
    procedure SetHover(Value: boolean);
    function GetURL: string;
    procedure SetAllHovers(List: TList; Value: boolean);
    procedure CreateFIArray;
    {$ifndef NoTabLink}
    procedure EnterEvent(Sender: TObject);
    procedure ExitEvent(Sender: TObject);
    procedure CreateTabControl(TabIndex: integer);
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AssignY(Y: integer);
    {$endif}
  public
    Pos : integer;        {0..Len  Index where font takes effect}
    TheFont : TMyFont;
    FIArray: TFontInfoArray;
    FontHeight,       {tmHeight+tmExternalLeading}
    tmHeight, tmMaxCharWidth,
    Overhang, Descent : integer;
    SScript: AlignmentType;
    UrlTarget: TUrlTarget;
    TabControl: ThtTabControl;
    constructor Create(ASection: TSection; F: TMyFont; Position: integer);
    constructor CreateCopy(ASection: TSection; T: TFontObj);
    destructor Destroy; override;
    procedure ReplaceFont(F: TMyFont);
    procedure ConvertFont(FI: ThtFontInfo);
    procedure FontChanged;
    function GetOverhang : integer;
    function GetHeight(var Desc: integer): integer;

    property URL: string read GetURL;
    property Visited: boolean read FVisited Write SetVisited;
    property Hover: boolean read FHover Write SetHover;
    property YValue: integer read FYValue;
  end;

  TFontList = class(TFreeList)  {a list of TFontObj's}
  Public
    constructor CreateCopy(ASection: TSection; T: TFontList);
    function GetFontAt(Posn : integer; var OHang : integer) : TMyFont;
    function GetFontCountAt(Posn, Leng : integer) : integer;
    function GetFontObjAt(Posn : integer;
                      var Index : integer) : TFontObj;
    procedure Decrement(N: integer; ParentSectionList: TSectionList);
  end;

  TImageFormControlObj = class;

  TFloatingObj = class(TIDObject)
  protected
    Pos : integer;        {0..Len  index of image position}
    ImageHeight,          {does not include VSpace}
    ImageWidth: integer;
    ObjAlign: AlignmentType;
    Indent: integer;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB:  integer;  {horizontal, vertical extra space}
    SpecWidth: integer;   {as specified by <img or panel> tag}
    SpecHeight: integer;   {as specified by <img or panel> tag}
    PercentWidth: boolean;           {if width is percent}
    PercentHeight: boolean;           {if height is percent}
    ImageTitle:  string;
    FAlt: string;          {the alt= attribute}
    FAltW: WideString;

    function GetYPosition: integer; override;
  public
    ImageKnown: boolean;      {know size of image}
    DrawYY: integer;
    DrawXX: integer;
    NoBorder: boolean;        {set if don't want blue border}
    BorderSize: integer;
    constructor CreateCopy(T: TFloatingObj);
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth, AvailableHeight: integer);  virtual; abstract;
    procedure ProcessProperties(Prop: TProperties);
    property Alt: string read FAlt;
  end;

  TPanelObj = class(TFloatingObj)
  private
    fMasterList:TSectionList;
    SetWidth, SetHeight: integer;
    IsCopy: boolean;
  public
    ShowIt: boolean;
    Panel, OPanel: ThvPanel;
    OSender: TObject;
    PanelPrintEvent: TPanelPrintEvent;
    FUserData: TObject;
    FMyPanelObj: TPanelObj;
    constructor Create(AMasterList: TSectionList; Position: integer;
                    L: TAttributeList; ACell: TCellBasic; ObjectTag: boolean);
    constructor CreateCopy(AMasterList: TSectionList; T: TPanelObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth, AvailableHeight: integer);  override;
    procedure Draw(ACanvas: TCanvas; X1, Y1: integer);
  end;

  HoverType = (hvOff, hvOverUp, hvOverDown);

  TImageObj = class(TFloatingObj)   {inline image info}
  private
    FBitmap: TBitmap;
    FHover: HoverType;
    FHoverImage: boolean;
    AltHeight, AltWidth: integer;
    Positioning: PositionType;
    function GetBitmap: TBitmap;
    procedure SetHover(Value: HoverType);
  public
    ObjHeight, ObjWidth: integer;   {width as drawn}
    Source: String;    {the src= attribute}
    Image: TgpObject;  {bitmap possibly converted from GIF, Jpeg, etc or animated GIF}
    OrigImage: TgpObject;  {same as above unless swapped}
    Mask: TBitmap;    {Image's mask if needed for transparency}
    ParentSectionList: TSectionList;
    Transparent: Transparency;    {None, Lower Left Corner, or Transp GIF}
    IsMap, UseMap: boolean;
    MapName: String;
    MyFormControl: TImageFormControlObj;  {if an <INPUT type=image}
    MyCell: TCellBasic;
    Swapped: boolean;   {image has been replaced}
    Missing: boolean;   {waiting for image to be downloaded}

    constructor Create(MasterList: TSectionList; Position: integer; L: TAttributeList);
    constructor SimpleCreate(MasterList: TSectionList; const AnURL: string);
    constructor CreateCopy(AMasterList: TSectionList; T: TImageObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth, AvailableHeight: integer);  override;
    procedure DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: TgpObject; ddMask: TBitmap);
    procedure Draw(Canvas: TCanvas; X: integer; TopY, YBaseline: integer; FO: TFontObj);
    function InsertImage(const UName: String; Error: boolean; var Reformat: boolean): boolean;

    property Bitmap: TBitmap read GetBitmap;
    property Hover: HoverType read FHover write SetHover;
    procedure ReplaceImage(NewImage: TStream);
  end;

  TImageObjList = class(TFreeList)  {a list of TImageObj's and TPanelObj's}
  Public
    constructor CreateCopy(AMasterList: TSectionList; T: TImageObjList);
    function FindImage(Posn: integer): TFloatingObj;
    function GetHeightAt(Posn: integer; var AAlign: AlignmentType;
         var FlObj: TFloatingObj) : Integer;
    function GetWidthAt(Posn: integer; var AAlign: AlignmentType;
           var HSpcL, HSpcR: integer; var FlObj: TFloatingObj) : integer;
    function GetImageCountAt(Posn: integer): integer;
    function PtInImage(X: integer; Y: integer; var IX, IY, Posn: integer;
                var AMap, UMap: boolean; var MapItem: TMapItem;
                var ImageObj: TImageObj): boolean;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
    procedure Decrement(N: integer);
  end;

  IndentManager = class(IndentManagerBasic)
    procedure Update(Y: integer; Img: TFloatingObj);
    procedure UpdateBlock(Y: integer; IW: integer; IH: integer; Justify: AlignmentType);
    end;

  TFormControlObj = class;
  TRadioButtonFormControlObj = class;

  ThtmlForm = class(TObject)
  private
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  Public
    MasterList: TSectionList;
    Method: string;
    Action, Target, EncType: String;
    ControlList: TFreeList;
    NonHiddenCount: integer;
    constructor Create(AMasterList: TSectionList; L : TAttributeList);
    destructor Destroy; override;
    procedure DoRadios(Radio: TRadioButtonFormControlObj);
    procedure InsertControl(Ctrl: TFormControlObj);
    procedure ResetControls;
    function GetFormSubmission: TStringList;
    procedure SubmitTheForm(const ButtonSubmission: string);
    procedure SetFormData(SL: TStringList);
    procedure SetSizes(Canvas: TCanvas);
    procedure ControlKeyPress(Sender: TObject; var Key: char);
  end;

  TFormControlObj = class(TIDObject)
  private
    FYValue: integer;
    Active: boolean;
    PaintBitmap: TBitmap;
    AttributeList: TStringList;
    FTitle: string;
    function GetControl: TWinControl; virtual;
    function GetAttribute(const AttrName: string): string;
  protected
    CodePage: integer;
    procedure DoOnChange;  virtual;
    procedure SaveContents; virtual;
    function GetYPosition: integer; override;
  public
    Pos : integer;        {0..Len  index of control position}
    MasterList: TSectionList;
    MyForm: ThtmlForm;
    Value, FName, FID: String;
    FormAlign: AlignmentType;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB, BordT, BordB: integer;
    FHeight, FWidth: integer;
    PercentWidth: boolean;
    Disabled: boolean;
    Readonly: boolean;
    BkColor: TColor;
    FControl: TWinControl;
    ShowIt: boolean;
    OnClickMessage: String;
    OnFocusMessage: String;
    OnBlurMessage: String;
    OnChangeMessage: String;

    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    constructor CreateCopy(T: TFormControlObj);
    destructor Destroy; override;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ProcessProperties(Prop: TProperties); virtual;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); virtual;
    procedure ResetToValue; virtual;
    function GetSubmission(Index: integer; var S: string): boolean; virtual;
    procedure SetData(Index: integer; const V: String); virtual;
    procedure SetDataInit; virtual;
    procedure SetHeightWidth(Canvas: TCanvas); virtual;
    procedure EnterEvent(Sender: TObject);    {these two would be better private}
    procedure ExitEvent(Sender: TObject);
    procedure FormControlClick(Sender: TObject);

    property TheControl: TWinControl read GetControl;  {the Delphi control, TButton, TMemo, etc}
    property Name: string read FName;  {Name given to control}
    property ID: string read FID;      {ID attribute of control}
    property YValue: integer read FYValue;
    property AttributeValue[const AttrName: string]: string read GetAttribute;
    property Title: string read FTitle write FTitle;
  end;

  TImageFormControlObj = class(TFormControlObj)
  private
    MyImage: TImageObj;
  public
    XPos, YPos, XTmp, YTmp: integer;   {click position}
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ImageClick(Sender: TObject);
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  THiddenFormControlObj = class(TFormControlObj)
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;
  end;

  TEditFormControlObj = class(TFormControlObj)
  private
    EnterContents: string;
    tmAveCharWidth: integer;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    EditSize: integer;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; const Typ: string; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  WhichType = (Submit, ResetB, Button, Browse);

  TButtonFormControlObj = class(TFormControlObj)
  public
    Which: WhichType;
    MyEdit: TEditFormControlObj;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; const Typ: string; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ButtonClick(Sender: TObject);
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  TRadioButtonFormControlObj = class(TFormControlObj)
  private
    WasChecked: boolean;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
    function GetControl: TWinControl; override;
  public
    IsChecked: boolean;
    MyCell: TCellBasic;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; ACell: TCellBasic);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure RadioClick(Sender: TObject);
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;
  end;

  TCheckBoxFormControlObj = class(TFormControlObj)
  private
    WasChecked: boolean;
  public
    IsChecked: boolean;
    constructor Create(AMasterList: TSectionList; Position: integer;
                     L: TAttributeList; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;
    procedure SetDataInit; override;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  end;

  LineRec = class(TObject)  {holds info on a line of text}
  private
    Start: PWideChar;
    SpaceBefore, SpaceAfter,
    LineHt,                 {total height of line}
    LineImgHt,              {top to bottom including any floating image}
    Ln,                     {# chars in line}
    Descent,
    LineIndent : integer;
    DrawXX, DrawWidth: integer;
    DrawY: integer;
    Spaces, Extra:  integer;
    BorderList: TFreeList;   {List of inline borders {BorderRec's) in this Line}
    FirstDraw: boolean;  {set if border processing needs to be done when first drawn}
    FirstX: integer;     {x value at FirstDraw}
    Shy: boolean;
  public
    constructor Create(SL: TSectionList);
    procedure Clear;
    destructor Destroy; override;
    end;

  TSectionBase = class(TIDObject)   {abstract base for document sections}
  protected
    MyBlock: TBlock;
    function GetYPosition: integer; override;
  public
    ParentSectionList: TSectionList;   {what list it's in}
    SectionHeight: integer;            {pixel height of section}
    DrawHeight: integer;               {floating image may overhang}
    StartCurs: integer;
    Len: integer;
    ZIndex: integer;
    ContentTop, ContentBot, ContentLeft: integer;
    DrawTop, DrawBot, YDraw: integer;

    constructor Create(AMasterList: TSectionList);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); virtual;
    procedure CopyToClipboard; virtual;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; virtual;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  virtual;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; virtual;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; virtual;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; virtual;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; virtual;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; virtual;
    function FindSourcePos(DocPos: integer): integer; virtual;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; virtual;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; virtual;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; virtual;
    procedure SetParent(List: TSectionList);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); virtual;
    procedure AddSectionsToList; virtual;
    end;

  TBlock = class(TSectionBase)
  private
    procedure DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect); virtual;
  public
    MargArray: TMarginArray;
    MyCell: TBlockCell;
    EmSize, ExSize, FGColor: integer;
    BorderStyle: BorderStyleType;
    FloatLR: AlignmentType;   {ALeft or ARight if floating}
    ClearAttr: ClearAttrType;
    IsListBlock: boolean;
    PRec: PtPositionRec;
    Positioning: PositionType;  {posStatic, posAbsolute, posRelative}
    Visibility: VisibilityType;
    BottomAuto: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    DisplayNone: boolean;
    HideOverflow: boolean;
    Justify: JustifyType;
    Converted: boolean;

    MargArrayO: TVMarginArray;
    OwnerCell: TCellBasic;
    TagClass: string;       {debugging aid}
    NewWidth: integer;
    ClearAddon: integer;
    Indent: integer;
    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage: TgpObject;
    TiledMask, FullBG: TBitmap;
    TopP, LeftP: integer;
    DrawList: TList;
    NoMask: boolean;
    ClientContentBot: integer;
    BlockTitle: string;
    MyRect: TRect;
    RefIMgr: IndentManager;

    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure CollapseMargins;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; virtual;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X , XRef, YRef : integer) : integer;  override;
    procedure DrawBlock(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, Y, XRef, YRef : integer);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; override;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;
    function FindSourcePos(DocPos: integer): integer; override;
    procedure CopyToClipboard; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;
    procedure DrawSort;
    procedure DrawTheList(Canvas: TCanvas; ARect: TRect; ClipWidth, X,
                          XRef, YRef :integer);
    procedure AddSectionsToList; override;
    procedure FormTree(Indent: string; var Tree: string);
    end;

  ListTypeType = (None, Ordered, Unordered, Definition, liAlone);

  ThtmlTable = class;

  TTableBlock = class(TBlock)
  private
    procedure DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect); override;
  public
    Table: ThtmlTable;
    WidthAttr: integer;
    AsPercent: boolean;
    BkColor: TColor;
    BkGnd: boolean;
    HSpace, VSpace: integer;
    HasCaption: boolean;
    TableBorder: boolean;
    Justify: JustifyType;
    TableIndent: integer;

    constructor Create(Master: TSectionList; Prop: TProperties;
    	AnOwnerCell: TCellBasic; ATable: ThtmlTable; TableAttr: TAttributeList;
        TableLevel: integer);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; override;
    function FindWidth1(Canvas: TCanvas; AWidth, ExtMarg: integer): integer;
    procedure AddSectionsToList; override;
    end;

  TTableAndCaptionBlock = class(TBlock)
  private
    procedure SetCaptionBlock(Value: TBlock);
  public
    TopCaption: boolean;
    TableBlock: TTableBlock;
    FCaptionBlock: TBlock;
    Justify: JustifyType;
    TableID:  string;
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
                    Attributes: TAttributeList; ATableBlock: TTableBlock);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    procedure CancelUsage;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;
    property CaptionBlock: TBlock read FCaptionBlock write SetCaptionBlock;
    end;

  THRBlock = class(TBlock)
  public
    Align: JustifyType;
    MyHRule: TSectionBase;
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; override;
    end;

  TBlockLI = class(TBlock)
    private
      ListType: ListTypeType;
      ListNumb: integer;
      ListStyleType: ListBulletType;

      ListFont: TFont;
      Image: TImageObj;
      FirstLineHt: integer;
    public
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
                Sy: Symb; APlain: boolean; AIndexType: char;
                AListNumb, ListLevel: integer; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    end;

  TBodyBlock = class(TBlock)
    constructor Create(Master: TSectionList; Prop: TProperties;
          AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    end;

  TCellObj = Class;

  TCellList = class(TFreeList)  {a list of TCellObj's to form a table row}
  public
    RowHeight: integer;
    SpecRowHeight, SpecRowHeightPercent: integer;
    RowSpanHeight: integer;   {height of largest rowspan}
    BkGnd: boolean;
    BkColor: TColor;
    BkImage: string;
    APRec: PtPositionRec;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    RowType: TRowType;

    constructor Create(Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellList);
    procedure InitializeRow;
    function DrawLogic1(Canvas : TCanvas; const Widths : array of integer; Span,
          CellSpacing, AHeight, Rows: integer; var Desired: integer; var Spec, More: boolean): integer;
    procedure DrawLogic2(Canvas : TCanvas; Y: integer;
              CellSpacing: integer; var Curs: integer);
    function Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
         const Widths : array of integer; X: integer; Y, YOffset: integer;
         CellSpacing : integer; Border: boolean; Light, Dark: TColor;
         MyRow: integer) : integer;
    procedure Add(CellObj: TCellObj);
    end;

  TColObj = Class
    colWidth: integer;
    colAsPercent: boolean;
    colAlign: string;
    colVAlign: AlignmentType;
    end;

  IntArray = array of Integer;
  TablePartType = (Normal, DoHead, DoBody1, DoBody2, DoBody3, DoFoot);
  TTablePartRec = class
    TablePart: TablePartType;
    PartStart: integer;
    PartHeight: integer;
    FootHeight: integer;
    end;

  ThtmlTable = class(TSectionBase)
  private
    TablePartRec: TTablePartRec;
    HeaderHeight, HeaderRowCount, FootHeight, FootStartRow, FootOffset: Integer;
    BodyBreak: integer;
    HeadOrFoot: Boolean;
    procedure DrawTable(Canvas: TCanvas; const ARect: TRect;
      IMgr: IndentManager; X, Y: Integer);
    procedure DrawTableP(Canvas: TCanvas; const ARect: TRect;
      IMgr: IndentManager; X, Y: integer);
    procedure FindRowHeights(Canvas: TCanvas; AHeight: integer);
  public
    Rows: TFreeList;   {a list of TCellLists}
    ListsProcessed: boolean;
    Indent,                   {table indent}
    Border: integer;          {width of border}
    Float: boolean;           {if floating}
    NumCols,                  {Number columns in table}
    TableWidth,               {width of table}
    tblWidthAttr: integer;    {Width attribute as entered}
    UseAbsolute: boolean;  {width entries are considered absolute}
    TableHeight: integer;     {height of table itself, not incl caption}
    CellPadding, CellSpacing: integer;
    HSpace, VSpace:  integer; {horizontal, vertical extra space}
    BorderColorLight, BorderColorDark: TColor;
    EndList: boolean;     {marker for copy}
    DrawX: integer;
    DrawY: integer;
    BkGnd: boolean;
    BkColor: TColor;
    ColInfo: TFreeList;
    Widths,                   {holds column widths}
    MaxWidths, MinWidths, Heights,
    Percents: IntArray;       {percent widths of columns}

    constructor Create(Master: TSectionList;Attr: TAttributeList;
                    Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure DoColumns(Width: integer; AsPercent: boolean;
                   VAlign: AlignmentType; const Align: string);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    procedure AddDummyCells;
    procedure GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
                TotalMaxWidth: integer);
    procedure GetWidthsAbs(Canvas: TCanvas; TablWidth: integer; Specified: boolean);
    procedure GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: integer;
               TheWidth: integer);
    procedure TableSpecifiedAndWillFit(TheWidth: integer);
    procedure TableNotSpecifiedAndWillFit(TotalMinWidth, TotalMaxWidth, TheWidth: integer);
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindSourcePos(DocPos: integer): integer; override;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;
    procedure CopyToClipboard; override;
    end;

  XArray = array[0..300] of integer;
  PXArray = ^XArray;

  IndexObj = class
    Pos: integer;
    Index: integer;
    end;

  TSection = class(TSectionBase)
  {TSection holds <p>, <li>, many other things, and the base for lists}
  private
    SectionNumber: integer;
    ThisCycle: integer;
    function GetIndexObj(I: integer): IndexObj;
    property PosIndex[I: integer]: IndexObj read GetIndexObj;
    procedure CheckForInlines(LR: Linerec);
  public
    BuffS: WideString; {holds the text for the section}
    Buff: PWideChar; {same as above}
    Brk: string;
    XP: PXArray;
    BuffSize: integer;       {buffer may be larger}
    Fonts : TFontList;   {List of FontObj's in this section}
    Images: TImageObjList;   {list of TImageObj's, the images in section}
    FormControls: TList;      {list of TFormControls in section}
    SIndexList: TFreeList;    {list of Source index changes}
    Lines : TFreeList;   {List of LineRecs,  info on all the lines in section}
    Justify: JustifyType; {Left, Centered, Right}
    ClearAttr: ClearAttrType;
    LineHeight: integer;
    DrawWidth: integer;
    AnchorName: boolean;
    StoredMin, StoredMax: integer;
    FirstLineIndent: integer;
    FLPercent: integer;
    BreakWord: boolean;

    constructor Create(AMasterList: TSectionList;  L: TAttributeList;
                Prop: TProperties; AnURL: TUrlTarget; ACell: TCellBasic; FirstItem: boolean);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure CheckFree;
    procedure Finish;
    procedure AddChar(C: WideChar; Index: integer); virtual;
    procedure AddTokenObj(T : TokenObj); virtual;
    procedure AddOpBrk;
    procedure ProcessText(TagIndex: integer); virtual;
    procedure Allocate(N : integer);
    function AddImage(L: TAttributeList; ACell: TCellBasic; Index: integer): TImageObj;
    function AddPanel(L: TAttributeList;
                     ACell: TCellBasic; Index: integer): TPanelObj;
    procedure AddPanel1(PO: TPanelObj; Index: integer);
    function CreatePanel(L: TAttributeList;
                     ACell: TCellBasic): TPanelObj;
    function AddFormControl(Which: Symb; AMasterList: TSectionList;
         L: TAttributeList; ACell: TCellBasic; Index: integer;
         Prop: TProperties): TFormControlObj;
    procedure ChangeFont(Prop: TProperties);
    procedure HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
              Attributes: TAttributeList; Prop: TProperties);
    function FindCountThatFits(Canvas: TCanvas; Width: integer; Start: PWideChar; Max: integer): integer;
    function FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; Max: integer; X, Y: integer; IMgr: IndentManager;
                    var ImgHt: integer; NxImages: TList) : integer;
    function FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: integer; RemoveSpaces: boolean): integer;
    function FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: integer): integer;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    procedure CopyToClipboard; override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; override;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindSourcePos(DocPos: integer): integer; override;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    end;

  TDrawList = class(TFreeList)
    procedure AddImage(Obj: TImageObj; Canvas: TCanvas; X: integer; TopY,
              YBaseline: integer; FO: TFontObj);
    procedure DrawImages;
    end;

  TCellBasic = class(TFreeList)  {a list which holds sections and blocks}
  public
    MasterList: TSectionList;  {the TSectionList that holds the whole document}
    YValue: integer;   {vertical position at top of cell}
    IMgr: IndentManager;
    StartCurs: integer;
    Len: integer;
    BkGnd: boolean;
    BkColor: TColor;
    tcContentBot, tcDrawTop, tcDrawBot: integer;
    OwnersTag: string;
    Owner:  TBlock;

    constructor Create(Master: TSectionList);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellBasic);
    procedure Add(Item: TSectionBase; TagIndex: integer);
    function CheckLastBottomMargin: boolean;
    procedure CopyToClipboard;
    function DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight, BlHt: integer;
                  var ScrollWidth: integer; var Curs: integer): integer; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef :integer): integer; virtual;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer; var UrlTarg: TUrlTarget;
        var FormControl: TImageFormControlObj; var ATitle: string): guResultType; virtual;
    function PtInObject(X: integer; Y: integer;  var Obj: TObject;
              var IX, IY: integer): boolean;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: integer;
                var XR: integer; var YR: integer; var Ht: integer;
                var Intext: boolean): integer;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
    function FindSourcePos(DocPos: integer): integer;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
    procedure AddSectionsToList;
    procedure FormTree(Indent: string; var Tree: string);
    end;

  TCell = class(TCellBasic)
    DrawYY: integer;

    constructor Create(Master: TSectionList);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellBasic);
    destructor Destroy; override;
    function DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight, BlHt: integer;
                  var ScrollWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef:integer): integer; override;
    end;

  TCellObjCell = class(TCell)
    MyRect: TRect;
    Title: string;
    Url, Target: string;
    constructor CreateCopy(AMasterList: TSectionList; T: TCellObjCell);
    function GetURL(Canvas: TCanvas; X: integer; Y: integer; var UrlTarg: TUrlTarget;
        var FormControl: TImageFormControlObj; var ATitle: string): guResultType; override;
    end;

  TBlockCell = class(TCellBasic)
    CellHeight: integer;

    function DoLogicX(Canvas: TCanvas; X, Y: integer; XRef, YRef, Width, AHeight, BlHt: integer;
                  var ScrollWidth: integer; var Curs: integer): integer;
    end;

  TSectionList = class(TCell)    {a list of all the sections--holds document}
  Private
    procedure AdjustFormControls;

  Public
    ShowImages,              {set if showing images}
    Printing: boolean;       {set if printing -- also see IsCopy}
    YOff: integer;           {marks top of window that's displayed}
    YOffChange: boolean;     {when above changes}
    NoPartialLine: boolean;  {set when printing if no partial line allowed
                              at page bottom}
    SelB, SelE: integer;
    PreFontName : string[lf_FaceSize+1];  {<pre>, <code> font for document}
    LinkVisitedColor, LinkActiveColor,
    HotSpotColor: TColor;
    PrintTableBackground: boolean;
    PrintBackground: boolean;
    PrintMonoBlack: boolean;
    TheOwner: TWinControl;        {the viewer that owns this document}
    PPanel: TWinControl;          {the viewer's PaintPanel}
    GetBitmap: TGetBitmapEvent;   {for OnBitmapRequest Event}
    GetImage: TGetImageEvent;     {for OnImageRequest Event}
    ExpandName: TExpandNameEvent;
    ObjectClick: TObjectClickEvent;
    ObjectFocus: ThtObjectEvent;
    ObjectBlur: ThtObjectEvent;
    ObjectChange: ThtObjectEvent;
    FileBrowse: TFileBrowseEvent;
    BackGround: TColor;

    OnBackgroundChange: TNotifyEvent;
    BackgroundBitmap: TGpObject; //TBitmap;
    BackgroundMask: TBitmap;
    BackgroundAniGif: TGifImage;
    BackgroundPRec: PtPositionRec;
    BitmapName: String;      {name of background bitmap}
    BitmapLoaded: boolean;   {if background bitmap is loaded}
    htmlFormList: TFreeList;
    AGifList: TList;      {list of all animated Gifs}
    SubmitForm: TFormSubmitEvent;
    ScriptEvent: TScriptEvent;
    PanelCreateEvent: TPanelCreateEvent;
    PanelDestroyEvent: TPanelDestroyEvent;
    PanelPrintEvent: TPanelPrintEvent;
    CB: SelTextCount;
    PageBottom: integer;
    PageShortened: boolean;
    MapList: TFreeList;    {holds list of client maps, TMapItems}
    Timer: TTimer;      {for animated GIFs}
    FormControlList:  TList;   {List of all TFormControlObj's in this SectionList}
    PanelList: TList;    {List of all TPanelObj's in this SectionList}
    MissingImages: TStringList;  {images to be supplied later}
    ControlEnterEvent: TNotifyEvent;
    LinkList: TList;    {List of links (TFontObj's)}
    ActiveLink: TFontObj;
    LinksActive: boolean;
    ActiveImage: TImageObj;
    ShowDummyCaret: boolean;
    Styles: TStyleList;    {the stylesheet}
    DrawList: TDrawList;
    FirstLineHtPtr: PInteger;
    IDNameList: TIDNameList;
    PositionList: TList;
    BitmapList: TStringBitmapList;
    SectionCount: integer;
    CycleNumber: integer;
    ProgressStart: integer;
    IsCopy: boolean;         {set when printing or making bitmap/metafile}
    NoOutput: boolean;
    TabOrderList: TStringList;
    FirstPageItem: boolean;
    StopTab: boolean;
    InlineList: TFreeList;  {actually TInlineList, a list of InlineRec's}
    TableNestLevel: integer;
    InLogic2: boolean;
    LinkDrawnEvent: TLinkDrawnEvent;
    LinkPage: integer;
    PrintingTable: ThtmlTable;
    ScaleX, ScaleY: single;
    SkipDraw: boolean;

    constructor Create(Owner, APaintPanel: TWinControl);
    constructor CreateCopy(T: TSectionList);
    procedure Clear;
    procedure ClearLists;
    destructor Destroy; override;
    procedure CheckGIFList(Sender: TObject);
    procedure HideControls;
    procedure SetYOffset(Y: integer);
    function GetSelLength: integer;
    procedure CopyToClipboardA(Leng: integer);
    function GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;
    procedure SetFonts(const Name, PreName: string; ASize: integer;
              AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
              LnksActive: boolean; LinkUnderLine: boolean; ACharSet: TFontCharSet;
              MarginHeight, MarginWidth: integer);
    procedure SetBackground(ABackground: TColor);
    procedure SetBackgroundBitmap(Name: String; const APrec: PtPositionRec);
    procedure GetBackgroundBitmap;
    function FindSectionAtPosition(Pos: integer;
             var TopPos: integer; var Index: integer): TSectionBase;
    procedure CancelActives;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer; var UrlTarg: TUrlTarget;
          var FormControl: TImageFormControlObj; var ATitle: string): guResultType; override;
    procedure LButtonDown(Down: boolean);
    function GetTheBitmap(const BMName: String; var Transparent: Transparency;
               var AMask: TBitmap; var FromCache, Delay: boolean): TgpObject;
    function DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight, BlHt: integer;
                  var ScrollWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef :integer): integer; override;
    procedure InsertImage(const Src: string; Stream: TMemoryStream; var Reformat: boolean);
    function GetFormcontrolData: TFreeList;
    procedure SetFormcontrolData(T: TFreeList);
    function FindDocPos(SourcePos: integer; Prev: boolean): integer;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
    procedure ProcessInlines(SIndex: integer; Prop: TProperties; Start: boolean);
  end;

  TCellObj = class(TObject)  {holds a TCell and some other information}
    ColSpan, RowSpan,      {column and row spans for this cell}
    Wd: integer;  {total width (may cover more than one column)}
    Ht,           {total height (may cover more than one row)}
    VSize: integer;     {Actual vertical size of contents}
    SpecHt: integer;    {Height as specified}
    SpecHtPercent: integer;
    YIndent: integer;   {Vertical indent}
    VAlign: AlignmentType;  {Top, Middle, or Bottom}
    WidthAttr: integer;   {Width attribute (percentage or absolute)}
    AsPercent: boolean;   {it's a percent}
    EmSize, ExSize: integer;
    PRec: PtPositionRec;
    PadTop, PadRight, PadBottom, PadLeft: integer;
    BrdTop, BrdRight, BrdBottom, BrdLeft: integer;
    HzSpace, VrSpace: integer;
    BorderStyle: BorderStyleType;
    Cell: TCellObjCell;

    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage: TGpObject;
    TiledMask, FullBG: TBitmap;
    MargArray: TMarginArray;
    MargArrayO: TVMarginArray;
    NoMask: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;

    constructor Create(Master: TSectionList; AVAlign: AlignmentType;
                Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellObj);
    destructor Destroy; override;
    private
    procedure InitializeCell(TablePadding: integer; const BkImageName: string;
                     const APRec: PtPositionRec; Border: boolean);
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: integer;
               Border: boolean; Light, Dark: TColor);
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: integer;
                var Curs: integer);
    end;

const
  ImageSpace = 3;   {extra space for left, right images}
  ListIndent = 35;

var
  CurrentStyle: TFontStyles;  {as set by <b>, <i>, etc.}
  CurrentForm: ThtmlForm;
  UnicodeControls:  boolean;

implementation

uses
  {$ifdef Delphi6_Plus}
  Variants,
  {$endif}
  HTMLView, ReadHTML, HTMLSbs1, GDIPL2A;

var
  NLevel: integer;  {for debugging}

type
  TSectionClass = Class of TSectionBase;
  EProcessError = class(Exception);

  TFormRadioButton = class(TRadioButton)
  private
    IDName: string;
    FChecked: boolean;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    procedure CreateWnd; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  published
    property Checked: boolean read GetChecked write SetChecked;
  end;

  TFormCheckBox = class(TCheckBox)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  end;

  BorderRec = class   {record for inline borders}
  private
    BStart, BEnd: integer;
    OpenStart, OpenEnd: boolean;
    BRect: TRect;
    MargArray: TMarginArray;
    procedure DrawTheBorder(Canvas: TCanvas; XOffset, YOffSet: integer; Printing: boolean);
    end;

  InlineRec = class
  private
    StartB, EndB, IDB, StartBDoc, EndBDoc: integer;
    MargArray: TMarginArray;
    end;

  TInlineList = class(TFreeList)  {a list of InlineRec's}
  private
    NeedsConverting: boolean;
    Owner: TSectionList;
    procedure AdjustValues;
    function GetStartB(I: integer): integer;
    function GetEndB(I: integer): integer;
  public
    constructor Create(AnOwner: TSectionList);
    procedure Clear;
    property StartB[I: integer]: integer read GetStartB;
    property EndB[I: integer]: integer read GetEndB;
    end;

procedure IndentManager.Update(Y: integer; Img: TFloatingObj);
{Given a new floating image, update the edge information.  Fills  Img.Indent,
 the distance from the left edge to the upper left corner of the image}
var
  IH, IW: integer;
  IR: IndentRec;
  LIndent: integer;
begin
if Assigned(Img) then
  begin
  IW := Img.ImageWidth + Img.HSpaceL + Img.HSpaceR;
  IH := Img.ImageHeight + Img.VSpaceT + Img.VSpaceB;
  if (Img.ObjAlign = ALeft) then
    begin
    IR := IndentRec.Create;
    with IR do
      begin
      LIndent := LeftIndent(Y);
      Img.Indent := LIndent-LfEdge+Img.HSpaceL;
      X := LIndent-LfEdge + IW;
      YT := Y;
      YB := Y + IH;
      L.Add(IR);
      end;
    end
  else if (Img.ObjAlign = ARight) then
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

procedure IndentManager.UpdateBlock(Y: integer; IW: integer; IH: integer;
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
      Float := True;     //ID := CurrentID;
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
      Float := True;     //ID := CurrentID;
      R.Add(IR);
      end;
    end;
end;

constructor TFontObj.Create(ASection: TSection; F: TMyFont; Position: integer);
begin
inherited Create;
Section := ASection;
TheFont := F;
Pos := Position;
UrlTarget := TUrlTarget.Create;
FontChanged;
end;

{$ifndef NoTabLink}
procedure TFontObj.EnterEvent(Sender: TObject);
var
  List: TList;
  I, J: integer;
begin
Active := True;
{Make adjacent fonts in this link active also}
List := Section.ParentSectionList.LinkList;
I := List.IndexOf(Self);
if I >= 0 then
  for J := I+1 to List.Count-1 do
    if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
      TFontObj(List[J]).Active := True
    else Break;
Section.ParentSectionList.ControlEnterEvent(Self);
end;

procedure TFontObj.ExitEvent(Sender: TObject);
var
  List: TList;
  I, J: integer;
begin
Active := False;
{Make adjacent fonts in this link inactive also}
List := Section.ParentSectionList.LinkList;
I := List.IndexOf(Self);
if I >= 0 then
  for J := I+1 to List.Count-1 do
    if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
      TFontObj(List[J]).Active := False
    else Break;
Section.ParentSectionList.PPanel.Invalidate;
end;

procedure TFontObj.AssignY(Y: integer);
var
  List: TList;
  I, J: integer;
begin
if UrlTarget.Url = '' then Exit;
if Assigned(TabControl) then
  FYValue := Y
else
  begin  {Look back for the TFontObj with the TabControl}
  List := Section.ParentSectionList.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I-1 downto 0 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        begin
        if Assigned(TFontObj(List[J]).TabControl) then
          begin
          TFontObj(List[J]).FYValue := Y;
          break;
          end;
        end
      else Break;
  end;
end;

procedure TFontObj.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Viewer: ThtmlViewer;
begin
Viewer := ThtmlViewer(Section.ParentSectionList.TheOwner);
if (Key = vk_Return) then
  begin
  Viewer.Url := UrlTarget.Url;
  Viewer.Target := UrlTarget.Target;
  Viewer.LinkAttributes.Text := UrlTarget.Attr;
  Viewer.LinkText := Viewer.GetTextByIndices(UrlTarget.Start, UrlTarget.Last);
  Viewer.TriggerUrlAction;  {call to UrlAction via message}
  end
else  {send other keys to ThtmlViewer}
  Viewer.KeyDown(Key, Shift);
end;

procedure TFontObj.CreateTabControl(TabIndex: integer);
var
  PntPanel: TPaintPanel;
  I, J: integer;
  List: TList;
begin
if Assigned(TabControl) then
  Exit;
  {Look back for the TFontObj with the TabControl}
List := Section.ParentSectionList.LinkList;
I := List.IndexOf(Self);
if I >= 0 then
  for J := I-1 downto 0 do
    if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
      if Assigned(TFontObj(List[J]).TabControl) then
        Exit;

PntPanel := TPaintPanel(Section.ParentSectionList.PPanel);
TabControl := ThtTabcontrol.Create(PntPanel);
with ThtTabcontrol(TabControl) do
  begin
  Left := -4000  ;   {so will be invisible until placed}
  Width := 1;
  Height := 1;
  TabStop := True;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnKeyDown := Self.AKeyDown;
  end;
TabControl.Parent := PntPanel;

if TabIndex > 0 then
  {Adding leading 0's to the number string allows it to be sorted numerically,
   and the Count takes care of duplicates}
  with Section.ParentSectionList.TabOrderList do
    AddObject(Format('%.5d%.3d', [TabIndex, Count]), TabControl);
end;
{$endif}

procedure TFontObj.CreateFIArray;
begin
if not Assigned(FIArray) then
  FIArray := TFontInfoArray.Create;
end;

procedure TFontObj.ReplaceFont(F: TMyFont);
begin
TheFont.Free;
TheFont := F;
FontChanged;
end;

procedure TFontObj.ConvertFont(FI: ThtFontInfo);
begin
  with TheFont, FI do
    begin
    Name := iName;
    Height := -Round(iSize * Screen.PixelsPerInch / 72);
    Style := iStyle;
    bgColor := ibgColor;
    Color := iColor;
    CharSet:= ICharSet;
    FontChanged;
    end;
end;

constructor TFontObj.CreateCopy(ASection: TSection; T: TFontObj);
begin
inherited Create;
Section := ASection;
Pos := T.Pos;
SScript := T.SScript;
TheFont := TMyFont.Create;
TheFont.Assign(T.TheFont);
if Assigned(T.FIArray) then
  ConvertFont(T.FIArray.Ar[LFont]);
UrlTarget := TUrlTarget.Create;
UrlTarget.Copy(T.UrlTarget);
FontChanged;
end;

destructor TFontObj.Destroy;
begin
FIArray.Free;
TheFont.Free;
UrlTarget.Free;
TabControl.Free;
inherited Destroy;
end;

procedure TFontObj.SetVisited(Value: boolean);
begin
if Value <> FVisited then
  begin
  FVisited := Value;
  if Value then
    if Hover then
      ConvertFont(FIArray.Ar[HVFont])
    else
      ConvertFont(FIArray.Ar[VFont])
  else
    if Hover then
      ConvertFont(FIArray.Ar[HLFont])
    else
      ConvertFont(FIArray.Ar[LFont]);
  FontChanged;
  end;
end;

procedure TFontObj.SetHover(Value: boolean);
begin
if Value <> FHover then
  begin
  FHover := Value;
  if Value then
    if FVisited then
      ConvertFont(FIArray.Ar[HVFont])
    else ConvertFont(FIArray.Ar[HLFont])
  else
    if FVisited then
      ConvertFont(FIArray.Ar[VFont])
    else ConvertFont(FIArray.Ar[LFont]);
  FontChanged;
  end;
end;

procedure TFontObj.SetAllHovers(List: TList; Value: boolean);
{Set/Reset Hover on this item and all adjacent item with the same URL}
var
  I, J: integer;
begin
SetHover(Value);
I := List.IndexOf(Self);
if I >= 0 then
  begin
  J := I+1;
  while (J < List.Count) and (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) do
    begin
    TFontObj(List[J]).Hover := Value;
    Inc(J);
    end;
  J := I-1;
  while (J >= 0) and (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) do
    begin
    TFontObj(List[J]).Hover := Value;
    Dec(J);
    end;
  end;
end;

function TFontObj.GetURL: string;
begin
try
  Result := UrlTarget.Url;
except
  Result := '';
  {$ifdef DebugIt}
  ShowMessage('Bad TFontObj, htmlsubs.pas, TFontObj.GetUrl');
  {$endif}
  end;
end;

procedure TFontObj.FontChanged;
begin
tmHeight := TheFont.tmHeight;
tmMaxCharWidth := TheFont.tmMaxCharWidth;
FontHeight := TheFont.tmHeight + TheFont.tmExternalLeading;
Descent := TheFont.tmDescent;
if fsItalic in TheFont.Style then  {estimated overhang}
  Overhang := TheFont.tmheight div 10
else Overhang := 0;
TheFont.Charset := TheFont.tmCharset;
end;

function TFontObj.GetOverhang: integer;
begin
Result := Overhang;
end;

function TFontObj.GetHeight(var Desc: integer): integer;
begin
Desc := Descent;
Result := FontHeight;
end;

constructor TFontList.CreateCopy(ASection: TSection; T: TFontList);
var
  I: integer;
begin
inherited create;
for I := 0 to T.Count-1 do
  Add(TFontObj.CreateCopy(ASection, TFontObj(T.Items[I])));
end;

function TFontList.GetFontAt(Posn : integer;
                var OHang : integer) : TMyFont;
{given a character index, find the font that's effective there}
var
  I, PosX: integer;
  F : TFontObj;
begin
I := 0;
PosX := 0;
while (I < Count)  do
  begin
  PosX := TFontObj(Items[I]).Pos;
  Inc(I);
  if PosX >= Posn then Break;
  end;
Dec(I);
if PosX > Posn then Dec(I);
F := TFontObj(Items[I]);
OHang := F.Overhang;
Result := F.TheFont;
end;

function TFontList.GetFontCountAt(Posn, Leng : integer) : integer;
{Given a position, return the number of chars before the font changes}
var
  I, PosX : integer;
begin
I := 0;
PosX := 0;
while I < Count do
  begin
  PosX := TFontObj(Items[I]).Pos;
  if PosX >= Posn then Break;
  Inc(I);
  end;
if PosX = Posn then Inc(I);
if I = Count then
  Result := Leng-Posn
else
  Result := TFontObj(Items[I]).Pos - Posn;
end;

{----------------TFontList.GetFontObjAt}
function TFontList.GetFontObjAt(Posn : integer;
                      var Index : integer) : TFontObj;
{Given a position, returns the FontObj which applies there and the index of
 the FontObj in the list}
var
  PosX: integer;
begin
Index := 0;
PosX := 0;
while (Index < Count)  do
  begin
  PosX := TFontObj(Items[Index]).Pos;
  Inc(Index);
  if PosX >= Posn then Break;
  end;
Dec(Index);
if PosX > Posn then Dec(Index);
Result := TFontObj(Items[Index]);
end;

{----------------TFontList.Decrement}
procedure TFontList.Decrement(N: integer; ParentSectionList: TSectionList);
{called when a character is removed to change the Position figure}
var
  I, J: integer;
  FO, FO1: TFontObj;
begin
I := 0;
while I < Count do
  begin
  FO := TFontObj(Items[I]);
  if FO.Pos > N then
      Dec(FO.Pos);
  if (I > 0) and (TFontObj(Items[I-1]).Pos = FO.Pos) then
    begin
    FO1 := TFontObj(Items[I-1]);
    J := ParentSectionList.LinkList.IndexOf(FO1);
    if J >=0 then
      ParentSectionList.LinkList.Delete(J);
    {$ifndef NoTabLink}
    if Assigned(FO1.TabControl) then
      if FO.UrlTarget.Id = FO1.UrlTarget.ID then
        begin      {if the same link, transfer the TabControl to the survivor}
        FO.TabControl := FO1.TabControl;
        FO.TabControl.OnEnter := FO.EnterEvent;
        FO.TabControl.OnExit := FO.ExitEvent;
        FO1.TabControl := Nil;
        end
      else
        begin   {remove the TabControl from the TabOrderList}
        J := ParentSectionList.TabOrderList.IndexOfObject(FO1.TabControl);
        if J >= 0 then
          ParentSectionList.TabOrderList.Delete(J);
        end;
    {$endif}
    FO1.Free;
    Delete(I-1);
    end
  else Inc(I);
  end;
end;

{----------------TImageObj.Create}
constructor TImageObj.Create(MasterList: TSectionList; Position: integer; L: TAttributeList);
var
  I: integer;
  S: string;
  NewSpace: integer;
  T: TAttribute;
begin
inherited Create;
ParentSectionList := MasterList;
Pos := Position;
ObjAlign := ABottom;   {default}
NewSpace := -1;
SpecHeight := -1;
SpecWidth := -1;
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      SrcSy: Source := Trim(Name);
      AltSy:
        begin
        FAlt := Name;
        while (Length(FAlt) > 0) and (FAlt[Length(FAlt)] in [#$D, #$A]) do
          Delete(FAlt, Length(FAlt), 1);
        ImageTitle := FAlt;    {use Alt as default Title}
        FAltW := MultibyteToWideString(CodePage, FAlt);
        end;
      IsMapSy:  IsMap := True;
      UseMapSy:
        begin
        UseMap := True;
        S := Trim(Uppercase(Name));
        if (Length(S) > 1) and (S[1] = '#') then
          System.Delete(S, 1, 1);
        MapName := S;
        end;
      AlignSy:
        begin
        S := UpperCase(Name);
        if S = 'TOP' then ObjAlign := ATop
        else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then ObjAlign := AMiddle
        else if S = 'LEFT' then ObjAlign := ALeft
        else if S = 'RIGHT' then ObjAlign := ARight;
        end;
      BorderSy: begin
                NoBorder := Value = 0;
                BorderSize := IntMin(IntMax(0, Value), 10);
                end;
      TranspSy: Transparent := LLCorner;
      HeightSy:if System.Pos('%', Name) > 0 then
                 begin
                 if (Value >= 0) and (Value <=100) then
                   begin
                   SpecHeight := Value;
                   PercentHeight := True;
                   end;
                 end
               else
                 SpecHeight := Value;
      WidthSy:if System.Pos('%', Name) > 0 then
                begin
                if (Value >= 0) and (Value <=100) then
                  begin
                  SpecWidth := Value;
                  PercentWidth := True;
                  end;
                end
              else
                SpecWidth := Value;
      HSpaceSy:  NewSpace := IntMin(40, Abs(Value));
      VSpaceSy:  VSpaceT := IntMin(40, Abs(Value));
      ActiveSy:  FHoverImage := True;
      NameSy: ParentSectionList.IDNameList.AddObject(Name, Self);
      end;
if L.Find(TitleSy, T) then
  ImageTitle := T.Name;   {has higher priority than Alt loaded above}
if L.TheID <> '' then
  ParentSectionList.IDNameList.AddObject(L.TheID, Self);

if NewSpace >= 0 then
  HSpaceL := NewSpace
else if ObjAlign in [ALeft, ARight] then
  HSpaceL := ImageSpace   {default}
else HSpaceL := 0;
HSpaceR := HSpaceL;
VSpaceB := VSpaceT;
end;

constructor TImageObj.SimpleCreate(MasterList: TSectionList; const AnURL: string);
begin
inherited Create;
ParentSectionList := MasterList;
ObjAlign := ABottom;   {default}
Source := AnURL;
NoBorder := True;
BorderSize := 0;
SpecHeight := -1;
SpecWidth := -1;
end;

procedure TFloatingObj.ProcessProperties(Prop: TProperties);
const
  DummyHtWd = 200;
var
  MargArrayO: TVMarginArray;
  MargArray: TMarginArray;
  Align: AlignmentType;
  EmSize, ExSize: integer;
begin
if Prop.GetVertAlign(Align) then
  ObjAlign := Align;
if Prop.GetFloat(Align) and (Align <> ANone) then
  begin
  if HSpaceR = 0 then
    begin  {default is different for Align = left/right}
    HSpaceR := ImageSpace;
    HSpaceL := ImageSpace;
    end;
  ObjAlign := Align;
  end;
if ImageTitle = '' then  {a Title attribute will have higher priority than inherited}
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
  else if (VarType(MargArrayO[Width]) = varString)
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
  else if (VarType(MargArrayO[Height]) = varString)
           and (System.Pos('%', MargArrayO[Height]) > 0) then
    begin
    PercentHeight := True;
    SpecHeight := MulDiv(MargArray[Height], 100, DummyHtWd);
    end
  else
    SpecHeight := MargArray[Height];
  end;

if Prop.GetVertAlign(Align) then
  ObjAlign := Align;
if Prop.GetFloat(Align) and (Align <> ANone) then
  ObjAlign := Align;
if Prop.BorderStyleNotBlank then
  begin
  NoBorder := True;    {will have inline border instead}
  BorderSize := 0;
  end;
end;

constructor TImageObj.CreateCopy(AMasterList: TSectionList; T: TImageObj);
begin
inherited CreateCopy(T);
ParentSectionList := AMasterList;
ImageKnown := T.ImageKnown;
ObjHeight := T.ObjHeight;
ObjWidth := T.ObjWidth;
SpecHeight := T.SpecHeight;
SpecWidth := T.SpecWidth;
PercentWidth := T.PercentWidth;
PercentHeight := T.PercentHeight;
Image := T.Image;
Mask := T.Mask;
IsMap := T.IsMap;
Transparent := T.Transparent;
FBitmap := Nil;
end;

destructor TImageObj.Destroy;
begin
if not ParentSectionList.IsCopy then
  begin
  if (Source <> '') and Assigned(OrigImage) then
    ParentSectionList.BitmapList.DecUsage(Source);
  if Swapped and (Image <> OrigImage) then
    begin      {not in cache}
    Image.Free;
    Mask.Free;
    end;
  if (OrigImage is TGifImage) and TGifImage(OrigImage).IsCopy then
    OrigImage.Free;
  end;
FreeAndNil(FBitmap);
inherited Destroy;
end;

function TImageObj.GetBitmap: TBitmap;
begin
Result := Nil;
if Image = ErrorBitmap then
  Exit;
if (Image is TGifImage) then
  Result := TGifImage(Image).Bitmap
else if (Image is TBitmap) or (Image is TGpBitmap) then
  begin
  if Assigned(FBitmap) then
    Result := FBitmap
  else
    begin
    if (Image is TBitmap) then
      begin
      FBitmap := TBitmap.Create;
      FBitmap.Assign(TBitmap(Image));
      if ColorBits = 8 then
        FBitmap.Palette := CopyPalette(ThePalette);
      end
    else    {it's a TGpBitmap}
      FBitmap := TGpBitmap(Image).GetTBitmap;
    Result := FBitmap;
    end;
  end
{$ifndef NoMetafile}
else if (Image is ThtMetaFile) then
  Result := ThtMetaFile(Image).WhiteBGBitmap;
{$endif}
end;

procedure TImageObj.SetHover(Value: HoverType);
begin
if (Value <> FHover) and FHoverImage and (Image is TGifImage) then
  with TGifImage(Image) do
    begin
    if Value <> hvOff then
      case NumFrames of
        2: CurrentFrame := 2;
        3: if Value = hvOverDown then
            CurrentFrame := 3
           else CurrentFrame := 2;
      else
        begin
        Animate := True;
        ParentSectionList.AGifList.Add(Image);
        end;
      end
    else
      begin
      Animate := False;
      if NumFrames <= 3 then
        CurrentFrame := 1;
      ParentSectionList.AGifList.Remove(Image);
      end;
    FHover := Value;
    ParentSectionList.PPanel.Invalidate;
    end;
end;

{----------------TImageObj.ReplaceImage}
procedure TImageObj.ReplaceImage(NewImage: TStream);
var
  TmpImage: TGpObject;
  NonAnimated: boolean;
  AMask: TBitmap;
  Stream: TMemoryStream;
  Tmp: TGifImage;
  I: integer;
begin
Transparent := NotTransp;
AMask := Nil;
TmpImage := Nil;
Stream := TMemoryStream.Create;
try
  Stream.LoadFromStream(NewImage);
  if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
    begin
    NonAnimated := True;
    if KindOfImage(Stream.Memory) in [GIF, Gif89] then
      TmpImage := CreateAGifFromStream(NonAnimated, Stream);
    if Assigned(TmpImage) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(TmpImage);
        TmpImage := TBitmap.Create;
        TBitmap(TmpImage).Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end;
        Tmp.Free;
        end;
      end
    else
      TmpImage := GetImageAndMaskFromStream(Stream, Transparent, AMask);
    end;
finally
  Stream.Free;
  end;
if Assigned(TmpImage) then
  begin
  if not Swapped then
    begin
    {OrigImage is left in cache and kept}
    if (Image is TGifImage) then
      ParentSectionList.AGifList.Remove(Image);
    Swapped := True;
    end
  else   {swapped already}
    begin
    if (Image is TGifImage) then
      begin
      ParentSectionList.AGifList.Remove(Image);
      end;
    Image.Free;
    FreeAndNil(Mask);
    end;
  FreeAndNil(FBitmap);
  Image := TmpImage;
  if (Image is TGifImage) then
    begin
    if not FHoverImage then
      begin
      TGifImage(Image).Animate := True;
      ParentSectionList.AGifList.Add(Image);
      end
    else
      begin
      TGifImage(Image).Animate := False;
      SetHover(hvOff);
      end;
    end;
  Mask := AMask;
  if Missing then
    begin    {if waiting for image, no longer want it}
    with ParentSectionList.MissingImages do
      for I := 0 to count-1 do
        if Objects[I] = Self then
          begin
          Delete(I);
          break;
          end;
    Missing := False;
    end;
  ParentSectionList.PPanel.Invalidate;
  end;
end;

{----------------TImageObj.InsertImage}
function TImageObj.InsertImage(const UName: string; Error: boolean; var Reformat: boolean): boolean;
var
  TmpImage: TgpObject;
  FromCache, IsAniGIF, Delay: boolean;
begin
Result := False;
Reformat := False;
if (Image = DefBitmap) then
  begin
  Result := True;
  if Error then
    begin
    Image := ErrorBitmap;
    Mask := ErrorBitmapMask;
    Transparent := LLCorner;
    end
  else
    begin
    TmpImage := ParentSectionList.GetTheBitmap(UName, Transparent, Mask, FromCache, Delay);
    if not Assigned(TmpImage) then
      Exit;
    IsAniGIF := TmpImage is TGifImage;

    if IsAniGIF then
      begin
      if FromCache then   {it would be}
        Image := TGifImage.CreateCopy(TGifImage(TmpImage))  {it's in Cache already, make copy}
      else
        Image := TmpImage;
      if not FHoverImage then
        begin
        ParentSectionList.AGifList.Add(Image);
        TGifImage(Image).Animate := True;
        if Assigned(ParentSectionList.Timer) then
          ParentSectionList.Timer.Enabled := True;
        end
      else TGifImage(Image).Animate := False;
      end
    else Image := TmpImage;
    OrigImage := Image;
    end;
  Missing := False;

  if not ImageKnown then
    Reformat := True;  {need to get the dimensions}
  end;
end;

{----------------TImageObj.DrawLogic}
procedure TImageObj.DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
              FO: TFontObj; AvailableWidth, AvailableHeight: integer);
{calculate the height and width}
var
  TmpImage: TgpObject;
  ImHeight, ImWidth: integer;
  ViewImages, FromCache: boolean;
  Rslt: string;
  ARect: TRect;
  SubstImage: Boolean;
  HasBlueBox: Boolean;

begin
ViewImages := ParentSectionList.ShowImages;

TmpImage := Image;
if ViewImages and not Assigned(TmpImage) then
  begin
  if Source <> '' then
    with SectionList do
      begin
      if not Assigned(GetBitmap) and not Assigned(GetImage) then
        Source := (TheOwner as ThtmlViewer).HTMLExpandFilename(Source)
      else if Assigned(ExpandName) then
        begin
        ExpandName(TheOwner, Source, Rslt);
        Source := Rslt;
        end;
      if MissingImages.IndexOf(Uppercase(Source)) = -1 then
        TmpImage := ParentSectionList.GetTheBitmap(Source, Transparent, Mask, FromCache, Missing)
      else Missing := True;  {already in list, don't request it again}
      end;
  if not Assigned(TmpImage) then
    begin
    if Missing then
      begin
      Image := DefBitmap;
      TmpImage := DefBitmap;
      ParentSectionList.MissingImages.AddObject(Source, Self); {add it even if it's there already}
      end
    else
      begin
      Image := ErrorBitmap;
      TmpImage := ErrorBitmap;
      Mask := ErrorBitmapMask;
      Transparent := LLCorner;
      end;
    end
  else if TmpImage is TGifImage then
    begin
    if FromCache then
      begin  {it's in Cache already, make copy}
      Image := TGifImage.CreateCopy(TGifImage(TmpImage));
      TmpImage := Image;
      end
    else
      Image := TmpImage;
    OrigImage := Image;
    if not FHoverImage then
      ParentSectionList.AGifList.Add(Image)
    else TGifImage(Image).Animate := False;
    end
  else
    begin
    Image := TmpImage; //TBitmap(TmpImage);
    OrigImage := Image;
    end;
  end;
if not ViewImages then
  TmpImage := DefBitMap;

ImHeight := GetImageHeight(TmpImage);
ImWidth := GetImageWidth(TmpImage);

SubstImage := (Image = ErrorBitmap) or (TmpImage = DefBitmap);

if not ImageKnown or PercentWidth or PercentHeight then
  begin
  if PercentWidth then
    begin
    ObjWidth := MulDiv(AvailableWidth-2*BorderSize, SpecWidth, 100);
    if SpecHeight >= 0 then
      if PercentHeight then
        ObjHeight := MulDiv(AvailableHeight-2*BorderSize, SpecHeight, 100)
      else ObjHeight := SpecHeight
    else ObjHeight := MulDiv(ObjWidth, ImHeight, ImWidth);
    end
  else if PercentHeight then
    begin
    ObjHeight := MulDiv(AvailableHeight-2*BorderSize, SpecHeight, 100);
    if SpecWidth >= 0 then ObjWidth := SpecWidth
      else ObjWidth := MulDiv(ObjHeight, ImWidth, ImHeight);
    end
  else if (SpecWidth >= 0) and (SpecHeight >= 0) then
    begin       {Both width and height specified}
    ObjHeight := SpecHeight;
    ObjWidth := SpecWidth;
    ImageKnown := True;
    end
  else if SpecHeight >= 0 then
    begin
    ObjHeight := SpecHeight;
    ObjWidth := MulDiv(SpecHeight, ImWidth, ImHeight);
    ImageKnown := not SubstImage;
    end
  else if SpecWidth >= 0 then
    begin
    ObjWidth := SpecWidth;
    ObjHeight := MulDiv(SpecWidth, ImHeight, ImWidth);
    ImageKnown := not SubstImage;
    end
  else
    begin       {neither height and width specified}
    ObjHeight := ImHeight;
    ObjWidth := ImWidth;
    ImageKnown := not SubstImage;
    end;
  end;

if (not ViewImages or SubstImage) then
  begin
  if (SpecWidth >= 0) or (SpecHeight >= 0) then
    begin  {size to whatever is specified}
    AltWidth := ObjWidth;
    AltHeight := ObjHeight;
    end
  else if FAltW <> '' then   {Alt text and no size specified, take as much space as necessary}
    begin
    Canvas.Font.Name := 'Arial';{use same font as in Draw}
    Canvas.Font.Size := 8;
    ARect := Rect(0, 0, 0, 0);
    DrawTextW(Canvas.Handle, PWideChar(FAltW+CRLF), -1, ARect, DT_CALCRECT);
    with ARect do
      begin
      AltWidth := Right + 16+8+2;
      AltHeight := IntMax(16+8, Bottom);
      end;
    end
  else
    begin  {no Alt text and no size spedified}
    AltWidth := IntMax(ObjWidth, 16+8);
    AltHeight := IntMax(ObjHeight, 16+8);
    end;
  ImageHeight := AltHeight;
  ImageWidth := AltWidth;
  end
else
  begin
  ImageHeight := ObjHeight;
  ImageWidth := ObjWidth;
  end;

HasBlueBox := not NoBorder and Assigned(FO) and (FO.URLTarget.Url <> '');
if HasBlueBox then
  BorderSize := IntMax(1, BorderSize);

if (BorderSize > 0) then
  begin
  Inc(ImageHeight, 2*BorderSize);      {extra pixels top and bottom for border}
  Inc(ImageWidth, 2*BorderSize);
  end;
end;

{----------------TImageObj.DoDraw}
procedure TImageObj.DoDraw(Canvas: TCanvas; XX: integer; Y: integer;
               ddImage: TgpObject; ddMask: TBitmap);
{Y relative to top of display here}
var
  DC: HDC;
  Img: TBitmap;
  W, H: integer;
  BMHandle: HBitmap;
  PrintTransparent: boolean;
begin
DC := Canvas.Handle;
if TObject(ddImage) is TGPImage then
  begin
  if not ParentSectionList.Printing then
    StretchDrawGpImage(DC, TGPImage(ddImage),XX,Y, ObjWidth, ObjHeight)
  else if not ParentSectionList.PrintBackground and (Positioning = posStatic) then  {Printing}
    StretchPrintGpImageOnColor(Canvas, TGpImage(ddImage), XX, Y, ObjWidth, ObjHeight)
  else
    StretchPrintGpImageDirect(DC, TGPImage(ddImage),XX,Y, ObjWidth, ObjHeight,
           ParentSectionList.ScaleX, ParentSectionList.ScaleY);
  Exit;
  end;
if (ddImage is TGifImage) and not ParentSectionList.IsCopy then
  with TGifImage(ddImage) do
    begin
    ShowIt := True;
    Visible := True;
    Draw(Canvas, XX, Y, ObjWidth, ObjHeight);
    Exit;
    end;
try
  if not ParentSectionList.IsCopy then
    begin
    if ((Transparent <> NotTransp) or (ddImage = ErrorBitmap)) and Assigned(ddMask) then
      if ddImage = ErrorBitmap then
        FinishTransparentBitmap(DC, TBitmap(ddImage), Mask, XX, Y,
                    TBitmap(ddImage).Width, TBitmap(ddImage).Height)
      else
        FinishTransparentBitmap(DC, TBitmap(ddImage), Mask, XX, Y, ObjWidth, ObjHeight)
    else
      begin
      Img := TBitmap(ddImage);
      if (ddImage = DefBitMap) or (ddImage = ErrorBitmap) then
        BitBlt(DC, XX, Y, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0, SRCCOPY)
      else if ddImage is TBitmap then
        begin
        SetStretchBltMode(DC, ColorOnColor);
        StretchBlt(DC, XX, Y, ObjWidth, ObjHeight, Img.Canvas.Handle, 0, 0, Img.Width, Img.Height, SRCCOPY);
        end
      {$ifndef NoMetafile}
      else if ddImage is ThtMetaFile then
        Canvas.StretchDraw(Rect(XX, Y, XX+ObjWidth, Y+ObjHeight), ThtMetaFile(ddImage));
      {$endif}
      end;
    end
  else
    begin       {printing}
    if ddImage is TGifImage then
      with TGifImage(ddImage) do
        begin
        ddMask := Mask;
        if Assigned(ddMask) then Transparent := TGif;
        ddImage := MaskedBitmap;
        TBitmap(ddImage).Palette := CopyPalette(ThePalette);
        TBitmap(ddImage).HandleType := bmDIB;
        end;
    if (ddImage = DefBitMap) or (ddImage = ErrorBitmap) then
      begin
      W := TBitmap(ddImage).Width;
      H := TBitmap(ddImage).Height;
      end
    else
      begin
      W := ObjWidth;
      H := ObjHeight;
      end;

    PrintTransparent := ((Transparent <> NotTransp) or (ddImage = ErrorBitmap))
                     and Assigned(ddMask);
    if PrintTransparent then
      PrintTransparentBitmap3(Canvas, XX, Y, W, H, TBitmap(ddImage), ddMask, 0, TBitmap(ddImage).Height)
    else if ddImage is TBitmap then
      begin   {printing, not transparent}
      BMHandle := TBitmap(ddImage).Handle;
      PrintBitmap(Canvas, XX, Y, W, H, BMHandle);
      end
    {$ifndef NoMetafile}
    else if ddImage is ThtMetaFile then
      Canvas.StretchDraw(Rect(XX, Y, XX+ObjWidth, Y+ObjHeight), ThtMetaFile(ddImage));
    {$endif}
    end;
except
  end;
end;

{----------------TImageObj.Draw}
procedure TImageObj.Draw(Canvas: TCanvas; X: integer; TopY, YBaseline: integer;
                                 FO: TFontObj);
var
  TmpImage: TgpObject;
  TmpMask: TBitmap;
  MiddleAlignTop: integer;
  ViewImages: boolean;
  SubstImage: boolean;
  Ofst: integer;
  SaveColor: TColor;
  ARect: TRect;
  SaveWidth: Integer;
  SaveStyle: TPenStyle;
  YY: Integer;

begin
with ParentSectionList do
  begin
  ViewImages := ShowImages;
  Dec(TopY, YOff);
  Dec(YBaseLine, YOff);
  end;
if ViewImages then
  begin
  TmpImage := Image;
  if Image is TBitmap then
    TmpMask := Mask
  else TmpMask := Nil;
  end
else
  begin
  TmpImage := DefBitMap;
  TmpMask := Nil;
  end;
SubstImage := not ViewImages or (TmpImage = ErrorBitmap) or (TmpImage = DefBitmap); {substitute image}

with Canvas do
  begin
  Brush.Style := bsClear;
  Font.Size := 8;
  Font.Name := 'Arial';        {make this a property?}
  Font.Style := Font.Style - [fsBold];
  if SubstImage then Ofst := 4 else Ofst := 0;
  if ObjAlign = AMiddle then
    MiddleAlignTop := YBaseLine+FO.Descent-(FO.tmHeight div 2)-((ImageHeight-VSpaceT+VSpaceB) div 2)
  else MiddleAlignTop := 0;   {not used}

  DrawXX := X;
  case ObjAlign of
      ALeft, ARight, ATop: DrawYY := TopY+VSpaceT;
      AMiddle: DrawYY := MiddleAlignTop;
      ABottom, ABaseline: DrawYY := YBaseLine-ImageHeight-VSpaceB;
      end;
  if (BorderSize > 0) then
    begin
    Inc(DrawXX, BorderSize);
    Inc(DrawYY, BorderSize);
    end;

  if not SubstImage or (AltHeight >= 16+8) and (AltWidth >= 16+8) then
    DoDraw(Canvas, DrawXX+Ofst, DrawYY+Ofst, TmpImage, TmpMask);
  Inc(DrawYY, ParentSectionList.YOff);
  SetTextAlign(Canvas.Handle, TA_Top);
  if SubstImage and (BorderSize = 0) then
    begin
    Font.Color := FO.TheFont.Color;
    {calc the offset from the image's base to the alt= text baseline}
    case ObjAlign of
      ATop, ALeft, ARight:
        begin
        if FAltW <> '' then
          WrapTextW(Canvas, X+24, TopY+Ofst+VSpaceT, X+AltWidth-2, TopY+AltHeight-1+VSpaceT, FAltW);
        RaisedRect(ParentSectionList, Canvas, X, TopY+VSpaceT,
                                      X+AltWidth-1, TopY+AltHeight-1+VSpaceT, False, 1);
        end;
      AMiddle:
        begin   {MiddleAlignTop is always initialized}
        if FAltW <> '' then
          WrapTextW(Canvas, X+24, MiddleAlignTop+Ofst, X+AltWidth-2,
              MiddleAlignTop+AltHeight-1, FAltW);
        RaisedRect(ParentSectionList, Canvas, X, MiddleAlignTop,
                         X+AltWidth-1, MiddleAlignTop+AltHeight-1, False, 1);
        end;
      ABottom, ABaseline:
        begin
        if FAltW <> '' then
          WrapTextW(Canvas, X+24, YBaseLine-AltHeight+Ofst-VSpaceB, X+AltWidth-2,
                   YBaseLine-VSpaceB-1, FAltW);
        RaisedRect(ParentSectionList, Canvas, X, YBaseLine-AltHeight-VSpaceB,
                                      X+AltWidth-1, YBaseLine-VSpaceB-1, False, 1);
        end;
      end;
    end;
  if (BorderSize > 0) then
    begin
    SaveColor := Pen.Color;
    SaveWidth := Pen.Width;
    SaveStyle := Pen.Style;
    Pen.Color := FO.TheFont.Color;
    Pen.Width := BorderSize;
    Pen.Style := psInsideFrame;
    Font.Color := Pen.Color;
    try
      if (FAltW <> '') and SubstImage then  {output Alt message}
        begin
        YY := DrawYY-ParentSectionList.YOff;
        case ObjAlign of
          ALeft, ARight, ATop:
            WrapTextW(Canvas, DrawXX+24, YY+Ofst, DrawXX+AltWidth-2, YY+AltHeight-1, FAltW);
          AMiddle:
            WrapTextW(Canvas, DrawXX+24, YY+Ofst, DrawXX+AltWidth-2,
                YY+AltHeight-1, FAltW);
          ABottom, ABaseline:
            WrapTextW(Canvas, DrawXX+24, YY+Ofst, DrawXX+AltWidth-2,
                     YY+AltHeight-1, FAltW);
          end;
        end;
      case ObjAlign of   {draw border}
        ALeft, ARight, ATop: Rectangle(X, TopY+VSpaceT, X+ImageWidth, TopY+VSpaceT+ImageHeight);
        AMiddle: Rectangle(X, MiddleAlignTop, X+ImageWidth, MiddleAlignTop + ImageHeight);
        ABottom, ABaseline: Rectangle(X, YBaseLine-ImageHeight-VSpaceB, X+ImageWidth, YBaseLine-VSpaceB);
        end;
    finally
      Pen.Color := SaveColor;
      Pen.Width:= SaveWidth;
      Pen.Style:= SaveStyle;
      end;
    end;
  if (Assigned(MyFormControl) and MyFormControl.Active or FO.Active) or
         ParentSectionList.IsCopy and Assigned(ParentSectionList.LinkDrawnEvent)
                             and (FO.UrlTarget.Url <> '') then
    begin
    SaveColor := SetTextColor(Handle, clBlack);
    Brush.Color := clWhite;
    case ObjAlign of
      ALeft, ARight, ATop:
          ARect := Rect(X, TopY+VSpaceT, X+ImageWidth, TopY+VSpaceT+ImageHeight);
      AMiddle:
          ARect := Rect(X, MiddleAlignTop, X+ImageWidth, MiddleAlignTop + ImageHeight);
      ABottom, ABaseline:
          ARect := Rect(X, YBaseLine-ImageHeight-VSpaceB, X+ImageWidth, YBaseLine-VSpaceB);
      end;
    if not ParentSectionList.IsCopy then
      Canvas.DrawFocusRect(ARect)  {draw focus box}
    else
      ParentSectionList.LinkDrawnEvent(ParentSectionList.TheOwner, ParentSectionList.LinkPage,
               FO.UrlTarget.Url, FO.UrlTarget.Target, ARect);
    SetTextColor(handle, SaveColor);
    end;
  end;
end;

{----------------TImageObjList.CreateCopy}
constructor TImageObjList.CreateCopy(AMasterList: TSectionList; T: TImageObjList);
var
  I: integer;
  Item: TObject;
begin
inherited create;
for I := 0 to T.Count-1 do
  begin
  Item := T.Items[I];
  if Item is TImageObj then
    Add(TImageObj.CreateCopy(AMasterList, TImageObj(Item)))
  else Add(TPanelObj.CreateCopy(AMasterList, TPanelObj(Item)));
  end;
end;

function TImageObjList.FindImage(Posn: integer): TFloatingObj;
{find the image at a given character position}
var
  I: integer;
begin
for I := 0 to Count-1 do
  if TFloatingObj(Items[I]).Pos = Posn then
    begin
    Result := Items[I];
    Exit;
    end;
Result := Nil;
end;

function TImageObjList.GetHeightAt(Posn: integer; var AAlign: AlignmentType;
         var FlObj: TFloatingObj) : Integer;
begin
FLObj := FindImage(Posn);
if Assigned(FLObj) then
  begin
  Result := FLObj.ImageHeight+FLObj.VSpaceT+FLObj.VSpaceB;
  AAlign := FLObj.ObjAlign;
  end
else Result := -1;
end;

function TImageObjList.GetWidthAt(Posn: integer; var AAlign: AlignmentType;
                var HSpcL, HSpcR: integer; var FlObj: TFloatingObj) : integer;
begin
FLObj := FindImage(Posn);
if Assigned(FLObj) then
  begin
  Result := FLObj.ImageWidth;
  AAlign := FLObj.ObjAlign;
  HSpcL := FLObj.HSpaceL;
  HSpcR := FLObj.HSpaceR;
  end
else Result := -1;
end;

function TImageObjList.GetImageCountAt(Posn: integer): integer;
{Return count of chars before the next image.  0 if at the image, 99999999 if no
 images after Posn}
var
  I, Pos: integer;
begin
if Count = 0 then
  begin
  Result := 99999999;
  Exit;
  end;
I := 0;
while I < count do
  begin
  Pos := TFloatingObj(Items[I]).Pos;
  if Pos >= Posn then break;
  Inc(I);
  end;
if I = Count then Result := 99999999
else
  Result := TFloatingObj(Items[I]).Pos - Posn;
end;

{----------------TImageObjList.Decrement}
procedure TImageObjList.Decrement(N: integer);
{called when a character is removed to change the Position figure}
var
  I: integer;
begin
for I := 0 to Count-1 do
  with TImageObj(Items[I]) do
    if Pos > N then
      Dec(Pos);
end;

{----------------TImageObjList.PtInImage}
function TImageObjList.PtInImage(X: integer; Y: integer; var IX, IY, Posn: integer;
                var AMap, UMap: boolean; var MapItem: TMapItem;
                var ImageObj: TImageObj): boolean;
var
  I, J, LimX, LimY: integer;
  LIY: integer;
  Obj: TObject;
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Obj := Items[I];
  if Obj is TImageObj then
    with TImageObj(Obj) do
      begin
      IX := X-DrawXX;    {these are actual image, box if any is outside}
      LIY := Y - DrawYY;
      LimX := ImageWidth-2*BorderSize;
      LimY:= ImageHeight-2*BorderSize;
      if (IX >= 0) and (IX < LimX) and (LIY >= 0) and (LIY < LimY) then
        begin
        IY := LIY;
        Result := True;
        AMap := IsMap;
        Posn := Pos;
        UMap := False;
        ImageObj := TImageObj(Obj);
        if UseMap then
          with ParentSectionList.MapList do
            for J := 0 to Count-1 do
              begin
              MapItem := Items[J];
              if MapItem.MapName = MapName then
                begin
                UMap := True;
                Exit;
                end;
              end;
        Exit;
        end;
      end;
  end;
end;

function TImageObjList.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
var
  I, LimX, LimY: integer;
  LIY: integer;
  Item: TObject;
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Item := Items[I];
  if Item is TImageObj then
    with TImageObj(Item) do
      begin
      IX := X-DrawXX;    {these are actual image, box if any is outside}
      LIY := Y - DrawYY;
      LimX := ImageWidth-2*BorderSize;
      LimY:= ImageHeight-2*BorderSize;
      if (IX >= 0) and (IX < LimX) and (LIY >= 0) and (LIY < LimY) then
        begin
        IY := LIY;
        Result := True;
        Obj := Item;
        Exit;
        end;
      end;
  end;
end;

{----------------ThtmlForm.Create}
constructor ThtmlForm.Create(AMasterList: TSectionList; L : TAttributeList);
var
  I: integer;
begin
inherited Create;
MasterList := AMasterList;
AMasterList.htmlFormList.Add(Self);
Method := 'Get';
if Assigned(L) then
  for I := 0 to L.Count-1 do
    with TAttribute(L[I]) do
      case Which of
        MethodSy: Method := Name;
        ActionSy: Action := Name;
        TargetSy: Target := Name;
        EncTypeSy: EncType := Name;
        end;
ControlList := TFreeList.Create;
end;

destructor ThtmlForm.Destroy;
begin
ControlList.Free;
inherited Destroy;
end;

procedure ThtmlForm.InsertControl(Ctrl: TFormControlObj);
begin
ControlList.Add(Ctrl);
if not (Ctrl is THiddenFormControlObj) then Inc(NonHiddenCount);
end;

procedure ThtmlForm.DoRadios(Radio: TRadioButtonFormControlObj);
var
  S: string;
  Ctrl: TFormControlObj;
  I: integer;
begin
if Radio.FName <>'' then
  begin
  S := Radio.FName;
  for I := 0 to ControlList.Count-1 do
    begin
    Ctrl := TFormControlObj(ControlList.Items[I]);
    if  (Ctrl is TRadioButtonFormControlObj) and (Ctrl <> Radio) then
      if CompareText(Ctrl.FName, S) = 0 then
        begin
        TFormRadioButton(Ctrl.TheControl).Checked := False;
        TFormRadioButton(Ctrl.TheControl).TabStop := False;   {first check turns off other tabstops}
        TRadioButtonFormControlObj(Ctrl).DoOnchange;
        end;
    end;
  end;
end;

procedure ThtmlForm.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  S: string;
  Ctrl: TFormControlObj;
  I: integer;
  List: TList;
begin
if (Key in [vk_up, vk_down, vk_left, vk_right]) and (Sender is TFormRadioButton) then
  begin
  S := TFormRadioButton(Sender).IDName;
  List:= TList.Create;
  try
    for I := 0 to ControlList.Count-1 do
      begin
      Ctrl := TFormControlObj(ControlList.Items[I]);
      if  (Ctrl is TRadioButtonFormControlObj) and
                (CompareText(Ctrl.FName, S) = 0) then
          List.Add(TRadioButtonFormControlObj(Ctrl).TheControl);
      end;
    I := List.IndexOf(Sender);
    if I >= 0 then
      begin
      if (Key in [vk_up, vk_left]) then
        begin
        if I > 0 then
          Dec(I);
        end
      else if I < List.Count-1 then
        Inc(I);
      TFormRadioButton(List.Items[I]).SetFocus;
      end;
  finally
    List.Free;
    end;
  end
else  {send other keys to ThtmlViewer}
  ThtmlViewer(MasterList.TheOwner).KeyDown(Key, Shift);
end;

procedure ThtmlForm.ResetControls;
var
  I: integer;
begin
for I := 0 to ControlList.Count-1 do
  TFormControlObj(ControlList.Items[I]).ResetToValue;
end;

procedure ThtmlForm.ControlKeyPress(Sender: TObject; var Key: char);
begin
if (Sender is ThtEdit) then
  if (Key = #13) then
    begin
    SubmitTheForm('');
    Key := #0;
    end;
end;

function ThtmlForm.GetFormSubmission: TStringList;
var
  I, J: integer;
  S: string;
begin
Result := TStringList.Create;
for I := 0 to ControlList.Count-1 do
  with  TFormControlObj(ControlList.Items[I]) do
    begin
    J := 0;
    while GetSubmission(J, S) do
      begin
      if S <> '' then
        Result.Add(S);
      Inc(J);
      end;
    end;
end;

procedure ThtmlForm.SubmitTheForm(const ButtonSubmission: string);
var
  I, J: integer;
  SL: TStringList;
  S: string;
begin
if Assigned(MasterList.SubmitForm) then
  begin
  SL := TStringList.Create;
  for I := 0 to ControlList.Count-1 do
    with  TFormControlObj(ControlList.Items[I]) do
      begin
      J := 0;
      if not Disabled then
        while GetSubmission(J, S) do
          begin
          if S <> '' then
            SL.Add(S);
          Inc(J);
          end;
      end;
  if ButtonSubmission <> '' then
    SL.Add(ButtonSubmission);
  MasterList.SubmitForm(MasterList.TheOwner, Action, Target, EncType, Method, SL);
  end;
end;

procedure ThtmlForm.SetFormData(SL: TStringList);
var
  I, J, K, Index: integer;
  Value: string;
  FormControl: TFormControlObj;
begin
for I := 0 to ControlList.Count-1 do
  begin
  FormControl := TFormControlObj(ControlList[I]);
  FormControl.SetDataInit;
  Index := 0;
  for J := 0 to SL.Count-1 do
    if CompareText(FormControl.FName, SL.Names[J]) = 0 then
      begin
      K := Pos('=', SL[J]);
      if K > 0 then
        begin
        Value := Copy(SL[J], K+1, Length(SL[J])-K);
        FormControl.SetData(Index, Value);
        Inc(Index);
        end;
      end;
  end;
end;

procedure ThtmlForm.SetSizes(Canvas: TCanvas);
var
  I: integer;
begin
for I := 0 to ControlList.Count-1 do
  TFormControlObj(ControlList.Items[I]).SetHeightWidth(Canvas);
end;

{----------------TFormControlObj.Create}
constructor TFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  I: integer;
begin
inherited Create;
Pos := Position;
MasterList := AMasterList;
if not Assigned(CurrentForm) then   {maybe someone forgot the <form> tag}
  CurrentForm := ThtmlForm.Create(AMasterList, Nil);
AMasterList.FormControlList.Add(Self);
MyForm := CurrentForm;
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      ValueSy: Self.Value := Name;
      NameSy: Self.FName := Name;
      IDSy: FID := Name;
      OnClickSy: OnClickMessage := Name;
      OnFocusSy: OnFocusMessage := Name;
      OnBlurSy: OnBlurMessage := Name;
      OnChangeSy: OnChangeMessage := Name;
      TabIndexSy:
          if Value > 0 then
            {Adding leading 0's to the number string allows it to be sorted numerically,
             and the Count takes care of duplicates}
            with AMasterList.TabOrderList do
              AddObject(Format('%.5d%.3d', [Value, Count]), Self);
      TitleSy: FTitle := Name;
      DisabledSy: Disabled := (Lowercase(Name) <> 'no') and (Name <> '0');
      ReadonlySy:  ReadOnly := True;
      end;

if L.TheID <> '' then
  MasterList.IDNameList.AddObject(L.TheID, Self);
AttributeList := L.CreateStringList;
FormAlign := ABottom;     {ABaseline set individually}
MyForm.InsertControl(Self);
end;

constructor TFormControlObj.CreateCopy(T: TFormControlObj);
begin
inherited Create;
System.Move(T.Pos, Pos, DWord(@FControl)-DWord(@Pos));
end;

destructor TFormControlObj.Destroy;
begin
if Assigned(FControl) then {hidden controls are Nil}
  begin
  FControl.Parent := Nil;
  FControl.Free;
  end;
AttributeList.Free;
PaintBitmap.Free;
inherited Destroy;
end;

procedure TFormControlObj.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
ThtmlViewer(MasterList.TheOwner).ControlMouseMove(Self, Shift, X, Y);
end;

function TFormControlObj.GetYPosition: integer;
begin
Result := YValue;
end;

procedure TFormControlObj.ProcessProperties(Prop: TProperties);
var
  MargArrayO: TVMarginArray;
  MargArray: TMarginArray;
  Align: AlignmentType;
  EmSize, ExSize: integer;
begin
Prop.GetVMarginArray(MargArrayO);
EmSize := Prop.EmSize;
ExSize := Prop.ExSize;
PercentWidth := (VarType(MargArrayO[Width]) = VarString) and (System.Pos('%', MargArrayO[Width]) > 0);
ConvInlineMargArray(MargArrayO, 100, 200, EmSize, ExSize, MargArray);

VSpaceT := 1;
VSpaceB := 1;

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
  BordT := MargArray[BorderTopWidth];
  BordB := MargArray[BorderBottomWidth];
  Inc(VSpaceT, BordT);
  Inc(VSpaceB, BordB);
  end;

if MargArray[Width] > 0 then     {excludes IntNull and Auto}
  if PercentWidth then
    begin
    if MargArray[Width] <= 100 then
      FWidth := MargArray[Width]
    else PercentWidth := False;
    end
  else
    FWidth := MargArray[Width];
if MargArray[Height] > 0 then
    FHeight:= MargArray[Height]-BordT-BordB;
if Prop.GetVertAlign(Align) then
  FormAlign := Align;
BkColor := Prop.GetBackgroundColor;
end;

procedure TFormControlObj.EnterEvent(Sender: TObject);
{Once form control entered, insure all form controls are tab active}
{$ifndef FastRadio}
var
  I: integer;
{$endif}
begin
if MasterList.IsCopy then Exit;
Active := True;
MasterList.PPanel.Invalidate;
MasterList.ControlEnterEvent(Self);
{$ifndef FastRadio}
with MasterList.FormControlList do
  begin
  for I := 0 to Count-1 do
    with  TFormControlObj(Items[I]) do
      if not ShowIt and Assigned(FControl) then
        begin
        FControl.Show;   {makes it tab active}
        FControl.Left := -4000; {even if it can't be seen}
        end;
  end;
{$endif}
if Assigned(MasterList.ObjectFocus) and (OnFocusMessage <> '') then
  MasterList.ObjectFocus(MasterList.TheOwner, Self, OnFocusMessage);
if OnChangeMessage <> '' then
  SaveContents;
end;

procedure TFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
end;

procedure TFormControlObj.ExitEvent(Sender: TObject);
begin
{$ifndef FastRadio}
MasterList.AdjustFormControls;
{$endif}
Active := False;
if OnChangeMessage <> '' then
  DoOnChange;
if Assigned(MasterList.ObjectBlur) and (OnBlurMessage <> '') then
  MasterList.ObjectBlur(MasterList.TheOwner, Self, OnBlurMessage);
MasterList.PPanel.Invalidate;
end;

procedure TFormControlObj.DoOnChange;
begin
end;

function TFormControlObj.GetControl: TWinControl;
begin
Result := FControl;
end;

procedure TFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
begin end;

procedure TFormControlObj.ResetToValue;
begin end;

function TFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := False;
end;

procedure TFormControlObj.SetDataInit;
begin
end;

procedure TFormControlObj.SetData(Index: integer; const V: String);
begin
end;

procedure TFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
end;

procedure TFormControlObj.FormControlClick(Sender: TObject);
begin
if Assigned(MasterList.ObjectClick) then
  MasterList.ObjectClick(MasterList.TheOwner, Self, OnClickMessage);
end;

function TFormControlObj.GetAttribute(const AttrName: string): string;
begin
Result := AttributeList.Values[AttrName];
end;

{----------------TImageFormControlObj.Create}
constructor TImageFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
XPos := -1;   {so a button press won't submit image data}

PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TButton.Create(PntPanel);
with TButton(FControl) do
  begin
  Left := -4000  ;   {so will be invisible until placed}
  Width := 1;
  Height := 1;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnClick := ImageClick;
  Enabled := not Disabled;
  end;
FControl.Parent := PntPanel;
end;

procedure TImageFormControlObj.ProcessProperties(Prop: TProperties);
begin
MyImage.ProcessProperties(Prop);
end;

procedure TImageFormControlObj.ImageClick(Sender: TObject);
begin
if FControl.CanFocus then
  FControl.SetFocus;
FormControlClick(Self);
XPos := XTmp; YPos := YTmp;
if not Disabled then
  MyForm.SubmitTheForm('');
end;

function TImageFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := False;
if (Index <= 1) and (XPos >= 0) then
  begin
  S := '';
  if FName <> '' then S := FName+'.';
  if Index = 0 then S := S+'x='+IntToStr(XPos)
  else
    begin  {index = 1}
    S := S+'y='+IntToStr(YPos);
    XPos := -1;
    end;
  Result := True;
  end;
end;

{----------------THiddenFormControlObj.GetSubmission}
function THiddenFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := Index = 0;
if Result then
  S := FName+'='+Value;
end;

procedure THiddenFormControlObj.SetData(Index: integer; const V: String);
begin
Value := V;
end;

{----------------TEditFormControlObj.Create}
constructor TEditFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; const Typ: string; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
  I: integer;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L);
CodePage := Prop.CodePage;
EditSize := 15;
if L.Find(SizeSy, T) then
  begin
  if T.Value > 0 then EditSize := T.Value
  else
    begin    {see if it's comma delimited list}
    I := IntMin(System.Pos(',', T.Name), System.Pos(' ', T.Name));
    if I > 1 then EditSize := StrToIntDef(copy(T.Name, 1, I-1), 20);
    end;
  end;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := ThtEdit.Create(PntPanel);
with ThtEdit(FControl) do
  begin
  Left := -4000  ;   {so will be invisible until placed}
  Width := 120;
  if (Prop.GetBorderStyle <> bssNone) then
    BorderStyle := bsNone;
  Parent := PntPanel;
  Tmp := Prop.GetFont;
  Font.Assign(Tmp);
  FHeight := Height;   {Height can change when font assigned}
  tmAveCharWidth := Tmp.tmAveCharWidth;
  Tmp.Free;
  if UnicodeControls then
    Text := MultibyteToWideString(CodePage, Value)
  else
    Text := Value;
  if L.Find(MaxLengthSy, T) then
    MaxLength := T.Value;
  if Typ = 'password' then
    PassWordChar := '*';
  OnKeyPress := MyForm.ControlKeyPress;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnClick := FormControlClick;
  OnMouseMove := HandleMouseMove;
  Enabled := not Disabled;
  ReadOnly := Self.Readonly;
  end;
end;

procedure TEditFormControlObj.ProcessProperties(Prop: TProperties);
begin
inherited;
if BkColor <> clNone then
  ThtEdit(FControl).Color := BkColor;
end;

procedure TEditFormControlObj.ResetToValue;
begin
if UnicodeControls then
  ThtEdit(FControl).Text := MultibyteToWideString(CodePage, Value)
else
  ThtEdit(FControl).Text := Value;
end;

{$ifdef bloop}
procedure TEditFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2, Addon: integer;
  ARect: TRect;
begin
if ThtEdit(FControl).BorderStyle <> bsNone then
  Addon := 4   {normal 3D border}
else Addon := 2; {inline border, 3D Border removed}
with ThtEdit(FControl) do
  begin
  Canvas.Font := Font;
  H2 := Abs(Font.Height);
  if BorderStyle <> bsNone then
    FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, False, MasterList.PrintMonoBlack, False, Color)
  else FillRectWhite(Canvas, X1, Y1, X1+Width, Y1+Height, Color);
  SetTextAlign(Canvas.handle, TA_Left);
  SetBkMode(Canvas.Handle, Windows.Transparent);
  Canvas.Brush.Style := bsClear;
  ARect := Rect(X1+Addon, Y1, X1+Width-(Addon div 2), Y1+Height);
  if UnicodeControls then
    {$Warnings Off}
    ExtTextOutW(Canvas.Handle, X1+Addon, Y1+(Height-H2)div 2, ETO_CLIPPED, @ARect,
      PWideChar(Text), Length(Text), nil)
    {$Warnings On}
  else
    Canvas.TextRect(ARect, X1+Addon, Y1+(Height-H2)div 2-1, Text);
  end
end;
{$endif}

procedure TEditFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2, Addon: integer;
  ARect: TRect;
begin
if ThtEdit(FControl).BorderStyle <> bsNone then
  Addon := 4   {normal 3D border}
else Addon := 2; {inline border, 3D Border removed}
with ThtEdit(FControl) do
  begin
  Canvas.Font := Font;
  H2 := Abs(Font.Height);
  if BorderStyle <> bsNone then
    FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, False, MasterList.PrintMonoBlack, False, Color)
  else FillRectWhite(Canvas, X1, Y1, X1+Width, Y1+Height, Color);
  SetTextAlign(Canvas.handle, TA_Left);
  SetBkMode(Canvas.Handle, Windows.Transparent);
  Canvas.Brush.Style := bsClear;
  ARect := Rect(X1+Addon, Y1, X1+Width-(Addon div 2), Y1+Height);
  if UnicodeControls then
    {$Warnings Off}
    ExtTextOutW(Canvas.Handle, X1+Addon, Y1+(Height-H2)div 2, ETO_CLIPPED, @ARect,
      PWideChar(Text), Length(Text), nil)
    {$Warnings On}
  else
    Canvas.TextRect(ARect, X1+Addon, Y1+(Height-H2)div 2-1, Text);
  end
end;

function TEditFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
if Index = 0 then
  begin
  Result := True;
  if UnicodeControls then
    S := FName+'='+WideStringToMultibyte(CodePage, ThtEdit(FControl).Text)
  else
    S := FName+'='+ThtEdit(FControl).Text;
  end
else Result := False;
end;

procedure TEditFormControlObj.SetData(Index: integer; const V: String);
begin
if UnicodeControls then
  ThtEdit(FControl).Text := MultibyteToWideString(CodePage, V)
else
  ThtEdit(FControl).Text := V;;
end;

procedure TEditFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
with ThtEdit(FControl) do
  begin
  Canvas.Font := Font;
  if not PercentWidth then
    if (FWidth >= 10) then
      Width := FWidth
    else Width := tmAveCharWidth*EditSize+23
  else
    begin       {percent width set later}
    Left := -4000;
    Width := 10;
    end;
  Height := IntMax(FHeight, IntMax(Canvas.TextHeight('A'), 10));
  end;
end;

procedure TEditFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
EnterContents := ThtEdit(FControl).Text;
end;

procedure TEditFormControlObj.DoOnChange;
begin
if ThtEdit(FControl).Text <> EnterContents then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TButtonFormControlObj.Create}
constructor TButtonFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; const Typ: string;
            Prop: TProperties);
var
  PntPanel: TPaintPanel;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L);
if Typ = 'submit' then
  begin
  Which := Submit;
  if Value = '' then
    Value := 'Submit';
  end
else if Typ = 'reset' then
  begin
  Which := ResetB;
  if Value = '' then
    Value := 'Reset';
  end
else if Typ = 'file' then
  begin
  Which := Browse;
  Value := '';
  FName := '';
  FId := '';
  end
else
  begin
  Which := Button;
  if Value = '' then
    Value := 'Button';
  end;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := ThtButton.Create(PntPanel);
with ThtButton(FControl) do
  begin
  Left := -4000  ;   {so will be invisible until placed}
  Tmp := Prop.GetFont;
  Font.Assign(Tmp);
  Tmp.Free;
  OnClick := Self.ButtonClick;
  if Which = Browse then
    Caption := 'Browse...'
  else if UnicodeControls then
    Caption := MultibyteToWideString(Prop.CodePage, Value)
  else
    Caption := Value;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnMouseMove := HandleMouseMove;
  Enabled := not Disabled;
  end;
FControl.Parent := PntPanel;
{$ifdef UseElPack}
ThtButton(FControl).Color := clBtnFace;
{$endif}
end;

procedure TButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2: integer;
  MonoBlack: boolean;
begin
with ThtButton(FControl) do
  begin
  MonoBlack := MasterList.PrintMonoBlack and (GetDeviceCaps(Canvas.Handle, BITSPIXEL) = 1) and
          (GetDeviceCaps(Canvas.Handle, PLANES) = 1);
  if not MonoBlack then
    begin
    try
      if not Assigned(PaintBitmap) then
        begin
        PaintBitmap := TBitmap.Create;
        PaintBitmap.Width := Width;
        PaintBitmap.Height := Height;
        PaintBitmap.Canvas.Lock;
        PaintTo(PaintBitmap.Canvas.Handle, 0, 0);
        PaintBitmap.Canvas.UnLock;
        end;
      PrintBitmap(Canvas, X1, Y1, Width, Height, PaintBitmap.Handle);
    except end;
    end
  else
    begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Font;
    FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, True, MasterList.PrintMonoBlack, False, clWhite);
    H2 := Canvas.TextHeight('A');
    SetTextAlign(Canvas.handle, TA_Center+TA_Top);
    Canvas.TextRect(Rect(X1, Y1, X1+Width, Y1+Height), X1+(Width div 2),
               Y1+(Height-H2)div 2, Value);
    end;
  end;
end;

procedure TButtonFormControlObj.ButtonClick(Sender: TObject);
var
  S: string;
begin
FormControlClick(Self);
if Which = ResetB then
  MyForm.ResetControls
else if Which = Submit then
  if FName = '' then
    MyForm.SubmitTheForm('')
  else
    begin
    S := FName;
    MyForm.SubmitTheForm(S+'='+Value);
    end
else if Which = Browse then
  if Assigned(MasterList.FileBrowse) and Assigned(MyEdit) and (MyEdit.TheControl is ThtEdit) then
    begin
    S := ThtEdit(MyEdit.TheControl).Text;
    MasterList.FileBrowse(MasterList.TheOwner, MyEdit, S);
    ThtEdit(MyEdit.TheControl).Text := S;
    end;
end;

procedure TButtonFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
with ThtButton(FControl) do
  begin
  Canvas.Font := Font;
  if FHeight >= Canvas.TextHeight('A') then
    Height := FHeight
  else Height := Canvas.TextHeight('A')+8;
  if (FWidth >= 10) and not PercentWidth then    {percent width set later}
    Width := FWidth
  else Width := Canvas.TextWidth(Caption)+20;
  end;
end;

{----------------TCheckBoxFormControlObj.Create}
constructor TCheckBoxFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
if Value = '' then Value := 'on';
FormAlign := ABaseline;
if L.Find(CheckedSy, T) then IsChecked := True;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TFormCheckBox.Create(PntPanel);
with TFormCheckBox(FControl) do
  begin
  Left := -4000  ;   {so will be invisible until placed}
  Width := 13;
  Height := 13;
  OnKeyDown := MyForm.AKeyDown;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnMouseMove := HandleMouseMove;
  Enabled := not Disabled;
  Parent := PntPanel;
  Checked := IsChecked; {must precede setting OnClick}
  OnClick := FormControlClick;
  end;
end;

procedure TCheckBoxFormControlObj.ResetToValue;
begin
TCheckBox(FControl).Checked := IsChecked;
end;

procedure TCheckBoxFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  x, y: integer;
begin
with TCheckBox(FControl) do
  begin
  FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, False, MasterList.PrintMonoBlack, Disabled, clWhite);
  if Checked then
    with Canvas do
      begin
      Pen.Color := clBlack;
      x := X1+3; y := Y1+Height div 2;
      MoveTo(x, y);
      LineTo(x+2, y+2);
      LineTo(x+6, y-2);
      end;
  end;
end;

function TCheckBoxFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
if (Index = 0) and TCheckBox(FControl).Checked then
  begin
  Result := True;
  S := FName+'='+Value;
  end
else Result := False;
end;

procedure TCheckBoxFormControlObj.SetDataInit;
begin
TCheckBox(FControl).Checked := False;  {not checked unless later data says so}
end;

procedure TCheckBoxFormControlObj.SetData(Index: integer; const V: String);
begin
if CompareText(V, Value) = 0 then
  TCheckBox(FControl).Checked := True;
end;

procedure TCheckBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
WasChecked := TCheckBox(FControl).Checked;
end;

procedure TCheckBoxFormControlObj.DoOnChange;
begin
if TCheckBox(FControl).Checked <> WasChecked then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TRadioButtonFormControlObj.Create}
constructor TRadioButtonFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; ACell: TCellBasic);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
  Ctrl: TFormControlObj;
  I: integer;
  SetTabStop: boolean;
begin
inherited Create(AMasterList, Position, L);
MyCell := ACell;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TFormRadioButton.Create(PntPanel);
FormAlign := ABaseline;
if L.Find(CheckedSy, T) then IsChecked := True;
with TFormRadioButton(FControl) do
  begin
  Left := -4000  ;   {so will be invisible until placed}
  if Screen.PixelsPerInch > 100 then
    begin
    Width := 16;
    Height := 16;
    end
  else
    begin
    Width := 13;
    Height := 14;
    end;
  IDName := Self.FName;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnKeyDown := MyForm.AKeyDown;
  OnMouseMove := HandleMouseMove;
  Enabled := not Disabled;
  Parent := PntPanel;     {must precede Checked assignment}

  {The Tabstop for the first radiobutton in a group will be set in case no
   radiobuttons that follow are checked.  This insures that the tab key can
   access the group}
  SetTabStop := True;
  {Examine all other radiobuttons in this group (same FName)}
  for I := 0 to MyForm.ControlList.Count-1 do
    begin
    Ctrl := TFormControlObj(MyForm.ControlList.Items[I]);
    if  (Ctrl is TRadioButtonFormControlObj)
        and (TRadioButtonFormControlObj(Ctrl).TheControl <> FControl) then  {skip the current radiobutton}
      if CompareText(Ctrl.FName, FName) = 0 then  {same group}
        if not IsChecked then
          begin
          {if the current radiobutton is not checked and there are other radio buttons,
           then the tabstop will not be set for the current radio button since it
           is not the first}
          SetTabStop := False;
          Break;
          end
        else
          begin
          {if the current radio button is checked, then uncheck all the others and
           make sure no others have tabstop set}
          TRadioButtonFormControlObj(Ctrl).IsChecked := False;
          TFormRadioButton(TRadioButtonFormControlObj(Ctrl).TheControl).Checked := False;
          TRadioButtonFormControlObj(Ctrl).TheControl.TabStop := False;
          end;
    end;
  if not IsChecked then
    TabStop := SetTabStop;
  Checked := IsChecked;   {must precede setting OnClick}
  OnClick := RadioClick;
  end;
end;

function TRadioButtonFormControlObj.GetControl: TWinControl;
begin
Result := FControl;
end;

procedure TRadioButtonFormControlObj.RadioClick(Sender: TObject);
begin
MyForm.DoRadios(Self);
FormControlClick(Self);
end;

procedure TRadioButtonFormControlObj.ResetToValue;
begin
TFormRadioButton(TheControl).Checked := IsChecked;
end;

procedure TRadioButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  OldStyle: TPenStyle;
  OldWidth, XW, YH, XC, YC: integer;
  OldColor, OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  MonoBlack: boolean;
begin
with Canvas do
  begin
  XW := X1+14;
  YH := Y1+14;
  OldStyle := Pen.Style;
  OldWidth := Pen.Width;
  OldBrushStyle := Brush.Style;
  OldBrushColor := Brush.Color;
  MonoBlack := MasterList.PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
          (GetDeviceCaps(Handle, PLANES) = 1);
  if Disabled and not MonoBlack then
    Brush.Color := clBtnFace
  else Brush.Color := clWhite;
  Pen.Color := clWhite;
  Ellipse(X1, Y1, XW, YH);

  Pen.Style := psInsideFrame;
  if MonoBlack then
    begin
    Pen.Width := 1;
    Pen.Color := clBlack;
    end
  else
    begin
    Pen.Width := 2;
    Pen.Color := clBtnShadow;
    end;
  Arc(X1, Y1, XW, YH, XW, Y1, X1, YH);
  if not MonoBlack then
    Pen.Color := clSilver;
  Arc(X1, Y1, XW, YH, X1, YH, XW, Y1);
  if TFormRadioButton(TheControl).Checked then
    begin
    Pen.Color := clBlack;
    OldColor := Brush.Color;
    Brush.Color := clBlack;
    Brush.Style := bsSolid;
    XC := X1+7;
    YC := Y1+7;
    Ellipse(XC-2, YC-2, XC+2, YC+2);
    Brush.Color := OldColor;
    end;
  Pen.Width := OldWidth;
  Pen.Style := OldStyle;
  Brush.Color := OldBrushColor;
  Brush.Style := OldBrushStyle;
  end;
end;

function TRadioButtonFormControlObj.GetSubmission(Index: integer;
             var S: string): boolean;
begin
if (Index = 0) and TFormRadioButton(TheControl).Checked then
  begin
  Result := True;
  S := FName+'='+Value;
  end
else Result := False;
end;

procedure TRadioButtonFormControlObj.SetData(Index: integer; const V: String);
begin
if CompareText(V, Value) = 0 then
  TFormRadioButton(TheControl).Checked := True;
end;

procedure TRadioButtonFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
WasChecked := TFormRadioButton(TheControl).Checked;
end;

procedure TRadioButtonFormControlObj.DoOnChange;
begin
if TFormRadioButton(TheControl).Checked <> WasChecked then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TCellBasic.Create}
constructor TCellBasic.Create(Master: TSectionList);
begin
inherited Create;
MasterList := Master;
end;

{----------------TCellBasic.CreateCopy}
constructor TCellBasic.CreateCopy(AMasterList: TSectionList; T: TCellBasic);
var
  I: integer;
  Tmp, Tmp1: TSectionBase;
begin
inherited Create;
MasterList := AMasterList;
OwnersTag := T.OwnersTag;
for I := 0 to T.Count-1 do
  begin
  Tmp := T.Items[I];
  Tmp1 := TSectionClass(Tmp.ClassType).CreateCopy(AMasterList, Tmp);
  Add(Tmp1, 0);
  end;
end;

{----------------TCellBasic.Add}
procedure TCellBasic.Add(Item: TSectionBase; TagIndex: integer);
begin
if Assigned(Item) then
  begin
  if (Item is TSection) and Assigned(TSection(Item).XP) then   {XP not assigned if printing}
    begin
    TSection(Item).ProcessText(TagIndex);
    if not (Item is TPreFormated) and (TSection(Item).Len = 0)
         and not TSection(Item).AnchorName and (TSection(Item).ClearAttr = clrNone) then
      begin
      TSection(Item).CheckFree;
      Item.Free;  {discard empty TSections that aren't anchors}
      Exit;
      end;
    end;
  inherited Add(Item);
  Item.SetParent(MasterList);
  end;
end;

function TCellBasic.CheckLastBottomMargin: boolean;
{Look at the last item in this cell.  If its bottom margin was set to Auto,
 set it to 0}
var
  TB: TObject;
  I: integer;
  Done: boolean;
begin
Result := False;
I := Count-1;  {find the preceding block that isn't absolute positioning}
Done := False;
while (I >= 0) and not Done do
  begin
  TB := Items[I];
  if (TB is TBlock) and (TBlock(TB).Positioning <> PosAbsolute) then
    Done := True
  else Dec(I);
  end;
if I >= 0 then
  begin
  TB := Items[I];
  if (TB is TBlock) then
    with TBlock(TB) do
      if BottomAuto then
        begin
        MargArray[MarginBottom] := 0;
        Result := True;
        end;
  if (TB is TBlockLI) then
    Result := TBlockLI(TB).MyCell.CheckLastBottomMargin;
  end;
end;

{----------------TCellBasic.GetURL}
function TCellBasic.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType;
{Y is absolute}
var
  I: integer;
  SB: TSectionBase;
begin
Result := [];
FormControl := Nil;
UrlTarg := Nil;
for I := 0 to Count-1 do
  begin
  SB := TSectionBase(Items[I]);
  with SB do
    begin
    if (Y >= DrawTop) and (Y < DrawBot) then
        begin
        Result := GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
        if Result <> [] then
          Exit;
        end;
    end;
  end;
end;

{----------------TCellBasic.PtInObject}
function TCellBasic.PtInObject(X: integer; Y: integer;  var Obj: TObject;
              var IX, IY: integer): boolean;
{Y is absolute}
var
  I: integer;
begin
Result := False;
Obj := Nil;
for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (Y >= DrawTop) and (Y < DrawBot) then
        begin
        Result := PtInObject(X, Y, Obj, IX, IY);
        if Result then
          Exit;
        end;
    end;
end;

{----------------TCellBasic.FindCursor}
function TCellBasic.FindCursor(Canvas: TCanvas; X: Integer; Y: integer;
                var XR: integer; var YR: integer; var Ht: integer;
                var Intext: boolean): integer;
var
  I: integer;
  SB: TSectionBase;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  SB := TSectionBase(Items[I]);
  with SB do
    begin
    if (Y >= DrawTop) and (Y < DrawBot) then
      Result := TSectionBase(Items[I]).FindCursor(Canvas, X, Y, XR, YR, Ht, InText);
    if Result >= 0 then
      Break;
    end;
  end;
end;

procedure TCellBasic.AddSectionsToList;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TSectionBase(Items[I]).AddSectionsToList;
end;

{----------------TCellBasic.FindString}
function TCellBasic.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
var
  I: integer;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).FindString(From, ToFind, MatchCase);
  if Result >= 0 then
    Break;
  end;
end;

{----------------TCellBasic.FindStringR}
function TCellBasic.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
var
  I: integer;
begin
Result := -1;
for I := Count-1 downto 0 do
  begin
  Result := TSectionBase(Items[I]).FindStringR(From, ToFind, MatchCase);
  if Result >= 0 then
    Break;
  end;
end;

{----------------TCellBasic.FindSourcePos}
function TCellBasic.FindSourcePos(DocPos: integer): integer;
var
  I: integer;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).FindSourcePos(DocPos);
  if Result >= 0 then
    Break;
  end;
end;

procedure TCellBasic.FormTree(Indent: string; var Tree: string);
var
  I: integer;
  Item: TSectionBase;
begin
for I := 0 to Count-1 do
  begin
  Item := Items[I];
  if Item is TBlock then
    TBlock(Item).FormTree(Indent, Tree)
  else if Item is TSection then
    Tree := Tree + Indent + Copy(TSection(Item).BuffS, 1, 10)+^M+^J
  else
    Tree := Tree + Indent + '----'^M+^J;
  end;
end;

{----------------TCellBasic.FindDocPos}
function TCellBasic.FindDocPos(SourcePos: integer; Prev: boolean): integer;
var
  I: integer;
begin
Result := -1;
if not Prev then
  for I := 0 to Count-1 do
    begin
    Result := TSectionBase(Items[I]).FindDocPos(SourcePos, Prev);
    if Result >= 0 then
      Break;
    end
else  {Prev, iterate backwards}
  for I := Count-1 downto 0 do
    begin
    Result := TSectionBase(Items[I]).FindDocPos(SourcePos, Prev);
    if Result >= 0 then
      Break;
    end
end;

{----------------TCellBasic.CursorToXY}
function TCellBasic.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  I: integer;
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).CursorToXY(Canvas, Cursor, X, Y);
  if Result then Break;
  end;
end;

{----------------TCellBasic.GetChAtPos}

function TCellBasic.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
var
  I: integer;
begin
Result := False;
if (Pos >= StartCurs) and (Pos <= StartCurs+Len) then
  for I := 0 to Count-1 do
    begin
    Result := TSectionBase(Items[I]).GetChAtPos(Pos, Ch, Obj);
    if Result then Break;
    end;
end;

{----------------TCellBasic.CopyToClipboard}
procedure TCellBasic.CopyToClipboard;
var
  I: integer;
  SLE, SLB: integer;
begin
if not Assigned(MasterList) then Exit;  {dummy cell}
SLB := MasterList.SelB;
SLE := MasterList.SelE;
if SLE <= SLB then Exit;   {nothing to do}

for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (SLB >= StartCurs + Len) then Continue;
    if (SLE <= StartCurs) then Break;
    CopyToClipboard;
    end;
end;

{----------------TCellBasic.DoLogic}
function TCellBasic.DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight, BlHt: integer;
                 var ScrollWidth: integer; var Curs: integer): integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  I, Sw, TheCount: integer;
  H, Tmp: integer;
  SB: TSectionBase;
begin
  YValue := Y;
  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
    begin
    try
      SB :=  TSectionBase(Items[I]);
      Tmp := SB.DrawLogic(Canvas, 0, Y+H, 0, 0, Width, AHeight, BlHt, IMgr, Sw, Curs);
      H := H+Tmp;
      ScrollWidth := IntMax(ScrollWidth, Sw);
      Inc(I);
    except
      on E:EProcessError do
        begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        TSectionBase(Items[I]).Free;
        Delete(I);
        Dec(TheCount);
        end;
      end;
    end;
  Len := Curs - StartCurs;
  Result := H;
 end;

{----------------TCellBasic.MinMaxWidth}
procedure TCellBasic.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
{Find the Width the cell would take if no wordwrap, Max, and the width if wrapped
 at largest word, Min}
var
  I, Mn, Mx: integer;
begin
Max := 0; Min := 0;
for I := 0 to Count-1 do
  begin
  TSectionBase(Items[I]).MinMaxWidth(Canvas, Mn, Mx);
  Max := IntMax(Max, Mx);
  Min := IntMax(Min, Mn);
  end;
end;

{----------------TCellBasic.Draw}
function TCellBasic.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                            Y, XRef, YRef : integer): integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
var
  I: integer;
  H: integer;
begin
  H := Y;
  for I := 0 to Count-1 do
    begin
    H := TSectionBase(Items[I]).Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
    end;
  Result := H;
end;

{----------------TBlock.Create}
constructor TBlock.Create(Master: TSectionList; Prop: TProperties;
                AnOwnerCell: TCellBasic; Attributes: TAttributeList);
var
  Clr: ClearAttrType;
  S: string;
begin
inherited Create(Master);
OwnerCell := AnOwnerCell;
MyCell := TBlockCell.Create(Master);
MyCell.OwnersTag := Prop.PropTag;
MyCell.Owner := Self;
DrawList := TList.Create;

Prop.GetVMarginArray(MargArrayO);
if Prop.GetClear(Clr) then
  ClearAttr := Clr;
if not Prop.GetFloat(FloatLR) then
  FloatLR := ANone;
BorderStyle := Prop.GetBorderStyle;
FGColor := Prop.Props[Color];
EmSize := Prop.EmSize;
ExSize := Prop.ExSize;
DisplayNone := Prop.DisplayNone;
BlockTitle := Prop.PropTitle;
if not (Self is TBodyBlock) and not (Self is TTableAndCaptionBlock)
             and Prop.GetBackgroundImage(S) and (S <> '') then
  begin  {body handles its own image}
  BGImage := TImageObj.SimpleCreate(Master, S);
  Prop.GetBackgroundPos(EmSize, ExSize, PRec);
  end;

Positioning := Prop.GetPosition;
if Positioning = posAbsolute then
  FloatLR := ANone;
Visibility := Prop.GetVisibility;
Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
if Positioning <> posStatic then
  begin
  ZIndex := 10*Prop.GetZIndex;
  if (Positioning = posAbsolute) and (ZIndex = 0) then
    ZIndex := 1;  {abs on top unless otherwise specified}
  if Positioning = posAbsolute then
    MyCell.IMgr := IndentManager.Create;
  end;
if (FloatLR in [ALeft, ARight]) and (ZIndex = 0) then
  ZIndex := 1;
TagClass := Prop.PropTag+'.'+Prop.PropClass;
if not (Self is TTableBlock) and not (Self is TTableAndCaptionBlock) then
  CollapseMargins;
if Assigned(Attributes) and (Attributes.TheID <> '') then
  Master.IDNameList.AddObject(Attributes.TheID, Self);
HideOverflow := Prop.IsOverflowHidden;
if Prop.Props[TextAlign] = 'right' then
  Justify := Right
else if Prop.Props[TextAlign] = 'center' then
  Justify := Centered
else Justify := Left;
end;

procedure TBlock.CollapseMargins;
{adjacent vertical margins need to be reduced}
var
  TopAuto, Done: boolean;
  TB: TSectionBase;
  LastMargin, Negs, I: integer;
  Tag: string;
begin
ConvVertMargins(MargArrayO, 400,  {height not known at this point}
   EmSize, ExSize, MargArray, TopAuto, BottomAuto);
if Positioning = posAbsolute then
  begin
  if TopAuto then
    MargArray[MarginTop] := 0;
  end
else if FloatLR in [ALeft, ARight] then  {do nothing}
else
  with OwnerCell do
    begin
    I := Count-1;  {find the preceding block that isn't absolute positioning}
    Done := False;
    while (I >= 0) and not Done do
      begin
      TB := TSectionBase(Items[I]);
      if ((TB is TBlock) and (TBlock(TB).Positioning <> PosAbsolute))
             or not (TB is TBlock) then   {allow for a TSection}
        Done := True
      else Dec(I);
      end;
    Tag := OwnerCell.OwnersTag;
    if I < 0 then
      begin {no previous non absolute block, remove any Auto paragraph space}
      if TopAuto then
        begin
        if (Tag = 'li')  then
          begin
          MargArray[MarginTop] := 0;
          end
        else
          MargArray[MarginTop] := 0;
        end
      else if (Tag = 'default') or (Tag = 'body') then
        MargArray[MarginTop] := IntMax(0, MargArray[MarginTop]-OwnerCell.Owner.MargArray[MarginTop]);
      end
    else
      begin
      TB := Items[I];
      if ((TB is TTableBlock) or (TB is TTableAndCaptionBlock)) and
            (TBlock(TB).FloatLR in [ALeft, ARight])
            and TopAuto then
        MargArray[MarginTop] := 0
      else if (TB is TBlock) then
        begin
        LastMargin := TBlock(TB).MargArray[MarginBottom];
        TBlock(TB).MargArray[MarginBottom] := 0;
        if LastMargin >= 0 then  {figure out how many are negative}
          if MargArray[MarginTop] >=0 then
            Negs := 0
          else Negs := 1
        else
          if MargArray[MarginTop] >=0 then
            Negs := 1
          else Negs := 2;
        case Negs of
          0: MargArray[MarginTop] := IntMax(MargArray[MarginTop], LastMargin);
          1: MargArray[MarginTop] := MargArray[MarginTop] + LastMargin;
          2: MargArray[MarginTop] := IntMin(MargArray[MarginTop], LastMargin);
          end;
        end
      else if (Tag = 'li') and TopAuto
           and ((Pos('ul.', TagClass)=1) or (Pos('ol.', TagClass)=1)) then
        MargArray[MarginTop] := 0;  {removes space from nested lists}
      end;
    end;
end;

{----------------TBlock.CreateCopy}
constructor TBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TBlock;
begin
inherited CreateCopy(AMasterList, T);
TT := T as TBlock;
System.Move(TT.MargArray, MargArray, DWord(@Converted)-DWord(@MargArray)+Sizeof(Converted));
MyCell := TBlockCell.CreateCopy(AMasterList, TT.MyCell);
MyCell.Owner := Self;
DrawList := TList.Create;
TagClass := TT.TagClass;
if Assigned(TT.BGImage) and AMasterlist.PrintTableBackground then
  BGImage := TImageObj.CreateCopy(AMasterList, TT.BGImage);
MargArrayO := TT.MargArrayO;
if Positioning = posAbsolute then
  MyCell.IMgr := IndentManager.Create;
end;

destructor TBlock.Destroy;
begin
BGImage.Free;
TiledImage.Free;
TiledMask.Free;
FullBG.Free;
if Positioning = posAbsolute then
  FreeAndNil(MyCell.IMgr);
FreeAndNil(MyCell);
DrawList.Free;
inherited;
end;

procedure TBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
var
  MinCell, MaxCell: integer;
  LeftSide, RightSide, AutoCount: integer;
begin
if DisplayNone or (Positioning = PosAbsolute) then
  begin
  Min := 0;
  Max := 0;
  Exit;
  end;
ConvMargArray(MargArrayO, 0, 400, EmSize, ExSize, BorderStyle, AutoCount, MargArray);
HideOverflow := HideOverflow and (MargArray[Width] <> Auto) and (MargArray[Width] > 20);
if HideOverflow then
  begin
  MinCell := MargArray[Width];
  MaxCell := MinCell;
  end
else MyCell.MinMaxWidth(Canvas, MinCell, MaxCell);
if MargArray[MarginLeft] = Auto then
  MargArray[MarginLeft] := 0;
if MargArray[MarginRight] = Auto then
  MargArray[MarginRight] := 0;
if MargArray[Width] = Auto then
  MargArray[Width] := 0;
LeftSide := MargArray[MarginLeft]+MargArray[BorderLeftWidth]+MargArray[PaddingLeft];
RightSide := MargArray[MarginRight]+MargArray[BorderRightWidth]+MargArray[PaddingRight];
Min := IntMax(MinCell, MargArray[Width]) + LeftSide + RightSide;
if MargArray[Width] > 0 then
  Max := Min
else
  Max := IntMax(MaxCell, MargArray[Width]) + LeftSide + RightSide;
end;

{----------------TBlock.GetURL}
function TBlock.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj; var ATitle: string): guResultType;
begin
if DisplayNone then
  Result := []
else
  begin
  if (BlockTitle <> '') and PtInRect(MyRect, Point(X, Y-ParentSectionList.YOFF)) then
    begin
    ATitle := BlockTitle;
    Include(Result, guTitle);
    end;
  Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  end;
end;

{----------------TBlock.FindString}
function TBlock.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
if DisplayNone then
  Result := -1
else Result := MyCell.FindString(From, ToFind, MatchCase);
end;

{----------------TBlock.FindStringR}
function TBlock.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
if DisplayNone then
  Result := -1
else Result := MyCell.FindStringR(From, ToFind, MatchCase);
end;

{----------------TBlock.FindCursor}
function TBlock.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
var
  I: integer;
begin
if DisplayNone then
  Result := -1
else
  begin          {check this in z order}
  Result := -1;
  with DrawList do
    for I := Count-1 downto 0 do
      with TSectionBase(Items[I]) do
        begin
        if (Y >= DrawTop) and (Y < DrawBot) then
            begin
            Result := FindCursor(Canvas, X, Y, XR, YR, CaretHt, Intext);
            if Result>= 0 then
              Exit;
            end;
        end;
  end;
end;

procedure TBlock.AddSectionsToList;
begin
MyCell.AddSectionsToList;
end;

{----------------TBlock.PtInObject}
function TBlock.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
{Y is absolute}
var
  I: integer;
begin
if DisplayNone then
  Result := False
else
  begin          {check this in z order}
  Result := False;
  Obj := Nil;
  with DrawList do
    for I := Count-1 downto 0 do
      with TSectionBase(Items[I]) do
        begin
        if (Y >= DrawTop) and (Y < DrawBot) then
            begin
            Result := PtInObject(X, Y, Obj, IX, IY);
            if Result then
              Exit;
            end;
        end;
  end;
end;

{----------------TBlock.GetChAtPos}
function TBlock.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
if DisplayNone then
  Result := False
else Result := MyCell.GetChAtPos(Pos, Ch, Obj);
end;

function TBlock.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
begin
if DisplayNone then
  Result := False
else Result := MyCell.CursorToXY(Canvas, Cursor, X, Y);
end;

function TBlock.FindDocPos(SourcePos: integer; Prev: boolean): integer;
begin
if DisplayNone then
  Result := -1
else Result := MyCell.FindDocPos(SourcePos, Prev);
end;

function TBlock.FindSourcePos(DocPos: integer): integer;
begin
if DisplayNone then
  Result := -1
else Result := MyCell.FindSourcePos(DocPos);
end;

procedure TBlock.CopyToClipboard;
begin
if not DisplayNone then
  begin
  MyCell.CopyToClipboard;
  if (Pos('p.', TagClass) = 1) and (ParentSectionList.SelE > MyCell.StartCurs+MyCell.Len) then
    ParentSectionList.CB.AddTextCR('', 0);
  end;
end;

{----------------TBlock.FindWidth}
function TBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  Marg2: integer;
  MinWidth, MaxWidth: integer;

  function BordPad: integer;
  begin
  Result := MargArray[BorderLeftWidth]+MargArray[BorderRightWidth]+
             MargArray[PaddingLeft]+MargArray[PaddingRight];
  end;

  procedure CalcWidth;
  begin
  if Positioning = posAbsolute then
    MargArray[Width] := IntMax(MinWidth,
         AWidth-BordPad-MargArray[MarginLeft]-MargArray[MarginRight]-LeftP)
  else if (FloatLR in [ALeft, ARight]) then
    MargArray[Width] := IntMin(MaxWidth,
         AWidth-BordPad-MargArray[MarginLeft]-MargArray[MarginRight])
  else MargArray[Width] := IntMax(MinWidth,
        AWidth-BordPad-MargArray[MarginLeft]-MargArray[MarginRight]);
  end;

  procedure CalcMargRt;
  begin
  MargArray[MarginRight] := IntMax(0, AWidth-BordPad-MargArray[MarginLeft]-MargArray[Width]);
  end;

  procedure CalcMargLf;
  begin
  MargArray[MarginLeft] := IntMax(0, AWidth-BordPad-MargArray[MarginRight]-MargArray[Width]);
  end;

begin
MyCell.MinMaxWidth(Canvas, MinWidth, MaxWidth);
HideOverflow := HideOverflow and (MargArray[Width] <> Auto) and (MargArray[Width] > 20);

case AutoCount of
  0: begin
     if not HideOverflow then
       MargArray[Width] := IntMax(MinWidth, MargArray[Width]);
     if (Justify in [centered, Right]) and (Positioning = posStatic)
             and not (FloatLR in [ALeft, ARight]) and
             (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
       begin
       Marg2 := IntMax(0, AWidth-MargArray[Width]-BordPad);
       case Justify of
         centered:
           begin
           MargArray[MarginLeft] := Marg2 div 2;
           MargArray[MarginRight] := Marg2 div 2;
           end;
         right:
           MargArray[MarginLeft] := Marg2;
         end;
       end;
     end;
  1: if MargArray[Width] = Auto then
       CalcWidth
     else
       begin
       if not HideOverflow then
         MargArray[Width] := IntMax(MargArray[Width], MinWidth);
       if MargArray[MarginRight] = Auto then
          if (FloatLR in [ALeft, ARight]) then
            MargArray[MarginRight] := 0
          else CalcMargRt
       else CalcMargLf;
       end;
  2: if MargArray[Width] = Auto then
       begin
       if MargArray[MarginLeft] = Auto then
         MargArray[MarginLeft] := 0
       else MargArray[MarginRight] := 0;
       CalcWidth;
       end
     else
       begin
       if not HideOverflow then
         MargArray[Width] := IntMax(MargArray[Width], MinWidth);
       Marg2 := IntMax(0, AWidth-MargArray[Width]-BordPad);
       MargArray[MarginLeft] := Marg2 div 2;
       MargArray[MarginRight] := Marg2 div 2;
       end;
  3: begin
     MargArray[MarginLeft] := 0;
     MargArray[MarginRight] := 0;
     CalcWidth;
     end;
  end;
Result := MargArray[Width];
end;

procedure DoImageStuff(Canvas: TCanvas; IW, IH: integer; BGImage: TImageObj; PRec: PtPositionRec;
  var TiledImage: TgpObject; var TiledMask: TBitmap; var NoMask: boolean);
{Set up for the background image. Allow for tiling, and transparency
  BGImage is the image
  PRec describes the location and tiling
  IW, IH, the width and height of the background
}
var
  I, OW, OH, X, XX, Y, X2, Y2: integer;
  P: array[1..2] of integer;
  TheMask, NewBitmap, NewMask: TBitmap;
  TheGpObj: TGpObject;
  g: TgpGraphics;

  procedure Tile(Bitmap, Mask: TBitmap; W, H: integer);
  begin
  repeat  {tile BGImage in the various dc's}
    XX := X;
    repeat
      TBitmap(TiledImage).Canvas.Draw(XX, Y, Bitmap);
      if Assigned(TheMask) then
        TiledMask.Canvas.Draw(XX, Y, Mask)
      else if not NoMask then
        PatBlt(TiledMask.Canvas.Handle, XX, Y, Bitmap.Width, Bitmap.Height, Blackness);
      Inc(XX, Bitmap.Width);
    until XX >= X2;
    Inc(Y, Bitmap.Height);
  until Y >= Y2;
  end;

  procedure TileGpImage(Image: TgpImage; W, H: integer);
  var
    ImW, ImH: integer;
    graphics: TgpGraphics;
begin
  ImW := Image.Width;
  ImH := Image.Height;
  try
    graphics := TGPGraphics.Create(TgpImage(TiledImage));
    try
      repeat  {tile Image in the various dc's}
        XX := X;
        repeat
          graphics.DrawImage(Image,XX,Y, Image.Width, Image.Height);
          Inc(XX, ImW);
        until XX >= X2;
        Inc(Y, ImH);
      until Y >= Y2;
    except
      end;
    graphics.Free;
  except
    end;
  end;

begin
if (BGImage.Image is TBitmap) then
  begin
  TheGpObj := TBitmap(BGImage.Image);
  TheMask := BGImage.Mask;
  end
else if BGImage.Image is TGifImage then
  begin
  TheGpObj := TGifImage(BGImage.Image).MaskedBitmap;
  TheMask := TGifImage(BGImage.Image).Mask;
  end
{$ifndef NoMetafile}
else if BGImage.Image is ThtMetafile then
  begin
  TheGpObj := ThtMetafile(BGImage.Image).Bitmap;
  TheMask := ThtMetafile(BGImage.Image).Mask;
  end
{$endif}
else
  begin
  TheGpObj := BGImage.Image;
  TheMask := Nil;
  end;

NoMask := not Assigned(TheMask) and PRec[1].RepeatD and PRec[2].RepeatD;

OW := GetImageWidth(BGImage.Image);
OH := GetImageHeight(BGImage.Image);

if (BGImage.Image is TgpImage) and not ((OW = 1) or (OH = 1)) then
  begin  {TiledImage will be a TGpBitmap unless Image needs to be enlarged}
  with TgpBitmap(TiledImage) do
    if Assigned(TiledImage) and ((IW <> Width) or (IH <> Height)) then
      FreeAndNil(TiledImage);
  if not Assigned(TiledImage) then
    TiledImage := TgpBitmap.Create(IW, IH);
  g := TgpGraphics.Create(TgpBitmap(TiledImage));
  g.Clear(0);  {clear to transparent black}
  g.Free;
  end
else
  begin  {TiledImage will be a TBitmap}
  if not Assigned(TiledImage) then
    TiledImage := TBitmap.Create;
  TBitmap(TiledImage).Palette := CopyPalette(ThePalette);
  TBitmap(TiledImage).Height := IH;
  TBitmap(TiledImage).Width := IW;
  PatBlt(TBitmap(TiledImage).Canvas.Handle, 0, 0, IW, IH, Blackness);
  end;

if not NoMask and ((BGImage.Image is TBitmap)
     {$ifndef NoMetafile}
      or (BGImage.Image is ThtMetafile)
     {$endif}
      ) then
  begin
  if not Assigned(TiledMask) then
    TiledMask := TBitmap.Create;
  TiledMask.Monochrome := True;
  TiledMask.Height := IH;
  TiledMask.Width := IW;
  if not Assigned(TheMask) then
    PatBlt(TiledMask.Canvas.Handle, 0, 0, IW, IH, Whiteness);
  end;

{compute the location and tiling of BGImage in the background}
P[1] := 0;  P[2] := 0;
for I := 1 to 2 do
  with PRec[I] do
    begin
    case PosType of
      pTop:
        P[I] := 0;
      pCenter:
        if I = 1 then
          P[1] := IW div 2 - OW div 2
        else P[2] := IH div 2 - OH div 2;
      pBottom:
        P[I] := IH - OH;
      pLeft:
        P[I] := 0;
      pRight:
        P[I] := IW - OW;
      PPercent:
        if I = 1 then
          P[1] := ((IW-OW) * Value) div 100
        else P[2] := ((IH-OH) * Value) div 100;
      pDim:
        P[I] := Value;
      end;
    if I=1 then
      P[1] := Intmin(IntMax(P[1], -OW), IW)
    else P[2] := Intmin(IntMax(P[2], -OH), IH);
    end;

X := P[1];
Y := P[2];
if PRec[2].RepeatD then
  begin
  while Y > 0 do
    Dec(Y, OH);
  Y2 := IH;
  end
else Y2 := Y;
if PRec[1].RepeatD then
  begin
  while X > 0 do
    Dec(X, OW);
  X2 := IW;
  end
else X2 := X;

if ((OW = 1) or (OH = 1)) then
  begin  {in case a 1 pixel bitmap is being tiled.  EnlargeImage returns a
     TBitmap regardless of TheGpObj type.}
  NewBitmap := EnlargeImage(TheGpObj, X2-X+1, Y2-Y+1);
  try
    if Assigned(TheMask) then
      NewMask := EnlargeImage(TheMask, X2-X+1, Y2-Y+1)
    else NewMask := Nil;
    try
      Tile(NewBitmap, NewMask, X2-X+1, Y2-Y+1);
    finally
      NewMask.Free;
      end;
  finally
    NewBitmap.Free;
    end;
  end
else if (TheGpObj is TBitmap) then
  Tile(TBitmap(TheGpObj), TheMask, OW, OH)
else TileGpImage(TgpImage(TheGpObj), OW, OH);
end;

{----------------TBlock.DrawLogic}
function TBlock.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
         var MaxWidth: integer; var Curs: integer): integer;
var
  ScrollWidth, YClear: integer;
  LIndex, RIndex: integer;
  SaveID: TObject;
  TotalWidth, MiscWidths: integer;
  AutoCount: integer;
  BlockHeight: integer;
  IB, Xin: Integer;

  function GetClearSpace(CA: ClearAttrType): integer;
  var
    CL, CR: integer;
  begin
  Result := 0;
  if (CA <> clrNone) then
    begin  {may need to move down past floating image}
    IMgr.GetClearY(CL, CR);
    case CA of
      clLeft:  Result := IntMax(0, CL-Y-1);
      clRight:  Result := IntMax(0, CR-Y-1);
      clAll: Result := IntMax(CL-Y-1, IntMax(0, CR-Y-1));
      end;
    end;
  end;

begin
if DisplayNone then
  begin
  SectionHeight := 0;
  DrawHeight := 0;
  ContentBot := 0;
  DrawBot := 0;
  MaxWidth := 0;
  Result := 0;
  Exit;
  end;
YDraw := Y;
Xin := X;
ClearAddOn := GetClearSpace(ClearAttr);
StartCurs := Curs;
MaxWidth := AWidth;

ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderStyle, AutoCount, MargArray);

TopP := MargArray[TopPos];
LeftP := MargArray[LeftPos];
if Positioning = PosRelative then
  begin
  if TopP = Auto then
    TopP := 0;
  if LeftP = Auto then
    LeftP := 0;
  end
else if  Positioning = PosAbsolute then
  begin
  if TopP = Auto then
    TopP := 0;
  if (LeftP = Auto) then
    if (MargArray[RightPos] <> Auto)
                    and (AutoCount = 0) then
      LeftP := AWidth-MargArray[RightPos]-MargArray[Width]-MargArray[MarginRight]
               -MargArray[MarginLeft]-MargArray[PaddingLeft]-MargArray[PaddingRight]
               -MargArray[BorderLeftWidth]-MargArray[BorderRightWidth]
    else
      LeftP := 0;
  end;

if Positioning = posAbsolute then
  begin
  X := LeftP;
  Y := TopP+YRef;
  end;

NewWidth := FindWidth(Canvas, AWidth, AHeight, AutoCount);

if Positioning <> posAbsolute then
  MyCell.IMgr := IMgr
else
  begin
  RefIMgr := IMgr;
  IMgr := MyCell.IMgr;
  IMgr.Clear;
  IMgr.Reset(0, NewWidth);
  IMgr.Width := NewWidth;
  end;

SaveID := IMgr.CurrentID;
IMgr.CurrentID := Self;

MiscWidths := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]
        +MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth];
TotalWidth := MiscWidths + NewWidth;

YClear := Y+ClearAddon;
if MargArray[MarginTop] > 0 then
  DrawTop := YClear
else DrawTop := YClear + MargArray[MarginTop]; {Border top}
if FloatLR = ALeft then
  begin
  Indent := IntMax(X, IMgr.LeftIndent(YClear)) + MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]-X;
  end
else if FloatLR = ARight then
  Begin
  Indent := IntMin(AWidth, IMgr.RightSide(YClear))- (MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth]) - NewWidth;
  end
else
  begin
  Indent := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
  end;

X := X + Indent;
ContentTop := Y+ClearAddon+MargArray[MarginTop]+MargArray[PaddingTop]+MargArray[BorderTopWidth];

LIndex := IMgr.SetLeftIndent(X, ContentTop);
RIndex := IMgr.SetRightIndent(X+NewWidth, ContentTop);

ContentLeft := X;

if MargArray[Height] > 0 then
  BlockHeight := MargArray[Height]
else if AHeight > 0 then
  BlockHeight := AHeight
else BlockHeight := BlHt;
if Positioning = posRelative then
  MyCell.DoLogicX(Canvas, X, ContentTop+TopP, XRef, ContentTop+TopP, NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs)
else if Positioning = posAbsolute then
  MyCell.DoLogicX(Canvas, X, ContentTop, XRef+LeftP+MargArray[MarginLeft]+MargArray[BorderLeftWidth],
     YRef+TopP+MargArray[MarginTop]+MargArray[BorderTopWidth], NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs)
else MyCell.DoLogicX(Canvas, X, ContentTop, XRef, YRef, NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs);

Len := Curs-StartCurs;

if Positioning in [posAbsolute, posRelative] then
  MaxWidth := ScrollWidth+MiscWidths-MargArray[MarginRight]+LeftP-Xin
else MaxWidth := ScrollWidth+MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth]+Indent;

if Positioning = posRelative then
  ClientContentBot := IntMax(ContentTop, MyCell.tcContentBot-TopP)
else ClientContentBot := IntMax(ContentTop, MyCell.tcContentBot);
if HideOverflow and (MargArray[Height] > 3) then
  ClientContentBot := ContentTop + MargArray[Height]
else
  if ClientContentBot - ContentTop < MargArray[Height] then
    ClientContentBot := ContentTop + MargArray[Height];

if Positioning = posAbsolute then
  begin
  IB := IMgr.ImageBottom;   {check for image overhang}
  if IB > ClientContentBot then
    ClientContentBot := IB;
  end;
ContentBot := ClientContentBot + MargArray[PaddingBottom]+
              MargArray[BorderBottomWidth]+MargArray[MarginBottom];
DrawBot := IntMax(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom]
           +MargArray[BorderBottomWidth];

Result := ContentBot-Y;

if Assigned(BGImage) and ParentSectionList.ShowImages then
  begin
  BGImage.DrawLogic(ParentSectionList, Canvas, Nil, 100, 0);
  if (BGImage.Image = ErrorBitmap) then
    begin
    FreeAndNil(BGImage);
    NeedDoImageStuff := False;
    end
  else
    begin
    BGImage.ImageKnown := True;  {won't need reformat on InsertImage}
    NeedDoImageStuff := True;
    end;
  end;
SectionHeight := Result;
IMgr.FreeLeftIndentRec(LIndex);
IMgr.FreeRightIndentRec(RIndex);
if (FloatLR in [ALeft, ARight]) or (Positioning = posAbsolute) then
  begin
  if Positioning = posAbsolute then
    DrawHeight := 0
  else DrawHeight := SectionHeight;
  if FloatLR = ALeft then
    IMgr.UpdateBlock(Y, X+NewWidth + MargArray[MarginRight]+
            MargArray[PaddingRight]+MargArray[BorderRightWidth], DrawBot-Y, FloatLR)
  else if FloatLR = ARight then
    IMgr.UpdateBlock(Y, TotalWidth, DrawBot-Y, FloatLR);
  SectionHeight := 0;
  Result := 0;
  end
else
  begin
  DrawHeight := IMgr.ImageBottom - Y;  {in case image overhangs}
  if DrawHeight < SectionHeight then
    DrawHeight := SectionHeight;
  end;
IMgr.CurrentID := SaveID;
if DrawList.Count = 0 then
  DrawSort;
end;

{----------------TBlock.DrawSort}
procedure TBlock.DrawSort;
var
  I, ZeroIndx, EndZeroIndx, SBZIndex: integer;
  SB: TSectionBase;

  procedure InsertSB(I1, I2: integer);
  var
    J: integer;
    Inserted: boolean;
  begin
  Inserted := False;
  for J := I1 to I2-1 do
    if SBZIndex < TSectionBase(DrawList[J]).ZIndex then
      begin
      DrawList.Insert(J, SB);
      Inserted := True;
      Break;
      end;
  if not Inserted then
    DrawList.Insert(I2, SB);
  end;

begin
ZeroIndx := 0;
EndZeroIndx := 0;
for I := 0 to MyCell.Count-1 do
  begin
  SB := TSectionBase(MyCell.Items[I]);
  SB.MyBlock := Self;
  SBZIndex := SB.ZIndex;
  if SBZIndex < 0 then
    begin
    InsertSB(0, ZeroIndx);
    Inc(ZeroIndx);
    Inc(EndZeroIndx);
    end
  else if SBZIndex = 0 then  {most items go here}
    begin
    DrawList.Insert(EndZeroIndx, SB);
    Inc(EndZeroIndx);
    end
  else
    InsertSB(EndZeroIndx, DrawList.Count);
  end;
end;

{----------------TBlock.Draw1}
function TBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  Y, YO: integer;
  HeightNeeded, Spacing: Integer;
begin
if DisplayNone then
  begin
  Result := 0;
  Exit;
  end;
Y := YDraw;
YO := Y - ParentSectionList.YOff;
Result := Y+SectionHeight;

if ParentSectionList.SkipDraw then
  begin
  ParentSectionList.SkipDraw := False;
  Exit;
  end;

with ParentSectionList do
  if Printing and (Positioning <> posAbsolute) then
    if BreakBefore and not FirstPageItem then
      begin
      if ARect.Top + YOff < YDraw+MargArray[MarginTop] then  {page-break-before}
        begin
        if YDraw+MargArray[MarginTop] < PageBottom then
          PageBottom := YDraw+MargArray[MarginTop];
        SkipDraw := True;    {prevents next block from drawing a line}
        Exit;
        end;
      end
    else if KeepIntact then
      begin
      {if we're printing and block won't fit on this page and block will fit on
       next page, then don't do block now}
      if (YO > ARect.Top) and (Y+DrawHeight > PageBottom) and
            (DrawHeight-MargArray[MarginTop] < ARect.Bottom - ARect.Top) then
        begin
        if Y+MargArray[MarginTop] < PageBottom then
          PageBottom := Y+MargArray[MarginTop];
        Exit;
        end;
      end
    else if BreakAfter then
      begin
      if ARect.Top + YOff < Result then    {page-break-after}
        if Result < PageBottom then
          PageBottom := Result;
      end
    else if Self is TTableBlock and not TTableBlock(Self).Table.HeadOrFoot then  {ordinary tables}
      {if we're printing and
       we're 2/3 down page and table won't fit on this page and table will fit on
       next page, then don't do table now}
      begin
      if (YO > ARect.Top + ((ARect.Bottom - ARect.Top)*2) div 3) and
            (Y+DrawHeight > PageBottom) and
            (DrawHeight < ARect.Bottom - ARect.Top) then
        begin
        if Y+MargArray[MarginTop] < PageBottom then
          PageBottom := Y+MargArray[MarginTop];
        Exit;
        end;
      end
    else if Self is TTableBlock then {try to avoid just a header and footer at page break}
      with TTableBlock(Self).Table do
        if HeadOrFoot and (ParentSectionList.TableNestLevel = 0)
              and ((ParentSectionList.PrintingTable = Nil) or
                (ParentSectionList.PrintingTable = TTableBlock(Self).Table)) then
          begin
          Spacing := CellSpacing div 2;
          HeightNeeded := HeaderHeight+FootHeight+
                  TCellList(Rows.Items[HeaderRowCount]).RowHeight;
          if (YO > ARect.Top) and (Y+HeightNeeded > ParentSectionList.PageBottom) and
                (HeightNeeded < ARect.Bottom - ARect.Top) then
            begin {will go on next page}
            if Y+Spacing < ParentSectionList.PageBottom then
              begin
              ParentSectionList.PageShortened := True;
              ParentSectionList.PageBottom := Y+Spacing;
              end;
            Exit;
            end;
          end;

if Visibility <> viHidden then
  if Positioning = posRelative then   {for debugging}
      DrawBlock(Canvas, ARect, IMgr, X+LeftP, Y+TopP, XRef, YRef)
  else if Positioning = posAbsolute then
    DrawBlock(Canvas, ARect, IMgr, XRef+LeftP, YRef+TopP, XRef, YRef)
  else if FloatLR in [ALeft, ARight] then
    DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef)
  else DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef);
end;

{----------------TBlock.DrawBlock}
procedure TBlock.DrawBlock(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, Y, XRef, YRef : integer);
var
  YOffset: integer;
  XR, YB, BL, BT, BR, BB, PL, PT, PR, PB, RefX, TmpHt: integer;
  SaveID: TObject;
  ImgOK, HasBackgroundColor: boolean;
  IT, IH, FT: integer;
  Rgn, SaveRgn, SaveRgn1: HRgn;
  OpenRgn: Boolean;

  procedure InitFullBg(W, H: integer);
  begin
  if not Assigned(FullBG) then
    begin
    FullBG := TBitmap.Create;
    if ParentSectionList.IsCopy then
      begin
      FullBG.HandleType := bmDIB;
      if ColorBits <= 8 then
        FullBG.Palette := CopyPalette(ThePalette);
      end;
    end;
  FullBG.Height := IntMax(H, 2);
  FullBG.Width := IntMax(W, 2);
  end;

begin
YOffset := ParentSectionList.YOff;

case FLoatLR of
  ALeft, ARight: RefX := X+Indent-(MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]);
  else
    RefX := X;
  end;

X := X+Indent;

XR := RefX + MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]
        + NewWidth + MargArray[MarginRight]+
            MargArray[PaddingRight]+MargArray[BorderRightWidth];  {current right edge}
if Positioning = posRelative then
  YB := ContentBot - YOffset + TopP
else if FLoatLR in [ALeft, ARight] then
  YB := DrawBot + MargArray[MarginBottom] - YOffset
else YB := ContentBot - YOffset;

BL := RefX + MargArray[MarginLeft];   {Border left and right}
BR := XR - MargArray[MarginRight];
PL := BL + MargArray[BorderLeftWidth]; {Padding left and right}
PR := BR - MargArray[BorderRightWidth];

BT := Y + ClearAddon + MargArray[MarginTop] - YOffset;   {Border Top and Bottom}
BB := YB - MargArray[MarginBottom];
PT := BT + MargArray[BorderTopWidth]; {Padding Top and Bottom}
PB := BB - MargArray[BorderBottomWidth];

IT := IntMax(0, Arect.Top-2-PT);
FT := IntMax(PT, ARect.Top-2);    {top of area drawn, screen coordinates}
IH := IntMin(PB-FT, Arect.Bottom-FT); {height of area actually drawn}

SaveRgn1 := 0;
OpenRgn := (Positioning <> PosStatic) and (ParentSectionList.TableNestLevel > 0);
if OpenRgn then
  begin
  SaveRgn1 := CreateRectRgn(0, 0, 1, 1);
  GetClipRgn(Canvas.Handle, SaveRgn1);
  SelectClipRgn(Canvas.Handle, 0);
  end;

MyRect := Rect(BL, BT, BR, BB);
if (BT <= ARect.Bottom) and (BB >= ARect.Top) then
  begin
  HasBackgroundColor := MargArray[BackgroundColor] <> clNone;
  try
    if NeedDoImageStuff and Assigned(BGImage) and (BGImage.Image <> DefBitmap) then
      begin
      if BGImage.Image = ErrorBitmap then     {Skip the background image}
        FreeAndNil(BGImage)
      else
        try
          if FloatLR in [ALeft, ARight] then
            TmpHt := DrawBot-ContentTop+MargArray[PaddingTop]+MargArray[PaddingBottom]
          else TmpHt := ClientContentBot-ContentTop+MargArray[PaddingTop]+MargArray[PaddingBottom];

          DoImageStuff(Canvas, MargArray[PaddingLeft]+NewWidth+MargArray[PaddingRight],
               TmpHt, BGImage, PRec, TiledImage, TiledMask, NoMask);
          if ParentSectionList.IsCopy and (TiledImage is TBitmap) then
            TBitmap(TiledImage).HandleType := bmDIB;
        except                   {bad image, get rid of it}
          FreeAndNil(BGImage);
          FreeAndNil(TiledImage);
          FreeAndNil(TiledMask);
          end;
      NeedDoImageStuff := False;
      end;

    ImgOK := Not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
                 and ParentSectionList.ShowImages;

    if HasBackgroundColor and
        (not ParentSectionList.Printing or ParentSectionList.PrintTableBackground) then
      begin   {color the Padding Region}
      Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
      Canvas.Brush.Style := bsSolid;
      if ParentSectionList.IsCopy and ImgOK then
        begin
        InitFullBG(PR-PL, IH);
        FullBG.Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
        FullBG.Canvas.Brush.Style := bsSolid;
        FullBG.Canvas.FillRect(Rect(0, 0, PR-PL, IH));
        end
      else Canvas.FillRect(Rect(PL, FT, PR, FT+IH));
      end;

    if ImgOK then
      begin
      if not ParentSectionList.IsCopy then
        if TiledImage is TgpBitmap then
          //DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PL, PT)
          DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PL, FT, 0, IT, PR-PL, IH)
          //BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcCopy)
        else if NoMask then
          BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcCopy)
        else
          begin
          InitFullBG(PR-PL, IH);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, Canvas.Handle, PL, FT, SrcCopy);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
          BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
          end
      else if TiledImage is TgpBitmap then   {printing}
        begin
        if HasBackgroundColor then
          begin
          DrawGpImage(FullBg.Canvas.Handle, TgpImage(TiledImage), 0, 0);
          PrintBitmap(Canvas, PL, FT, PR-PL, IH, FullBG.Handle);
          end
        else
          PrintGpImageDirect(Canvas.Handle, TgpImage(TiledImage), PL, PT,
                     ParentSectionList.ScaleX, ParentSectionList.ScaleY);
        end
      else if NoMask then  {printing}
        PrintBitmap(Canvas, PL, FT, PR-PL, IH, TBitmap(TiledImage).Handle)
      else if HasBackgroundColor then
        begin
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
        PrintBitmap(Canvas, PL, FT, PR-PL, IH, FullBG.Handle);
        end
      else
        PrintTransparentBitmap3(Canvas, PL, FT, PR-PL, IH, TBitmap(TiledImage), TiledMask, IT, IH)
      end;

  except
    end;
  end;

if HideOverflow then
  GetClippingRgn(Canvas, Rect(PL+MargArray[PaddingLeft], PT+MargArray[PaddingTop],
                              PR-MargArray[PaddingRight], PB-MargArray[PaddingBottom]),
                         ParentSectionList.Printing, Rgn, SaveRgn);

SaveID := IMgr.CurrentID;
Imgr.CurrentID := Self;
if Positioning = posRelative then
  DrawTheList(Canvas, ARect, NewWidth, X,
      RefX+MargArray[MarginLeft]+MargArray[BorderLeftWidth]+MargArray[PaddingLeft],
      Y+MargArray[MarginTop]+MargArray[BorderTopWidth]+MargArray[PaddingTop])
else if Positioning = posAbsolute then
  DrawTheList(Canvas, ARect, NewWidth, X,
      RefX+MargArray[MarginLeft]+MargArray[BorderLeftWidth],
      Y+MargArray[MarginTop]+MargArray[BorderTopWidth])
else DrawTheList(Canvas, ARect, NewWidth, X, XRef, YRef);
Imgr.CurrentID := SaveID;

if HideOverflow then  {restore any previous clip region}
  begin
  SelectClipRgn(Canvas.Handle, SaveRgn);
  DeleteObject(Rgn);
  if SaveRgn <> 0 then
    DeleteObject(SaveRgn);
  end;
DrawBlockBorder(Canvas, Rect(BL, BT, BR, BB), Rect(PL, PT, PR, PB));

if OpenRgn then
  begin
  SelectClipRgn(Canvas.Handle, SaveRgn1);
  DeleteObject(SaveRgn1);
  end;
end;

procedure TBlock.DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect);
begin
if BorderStyle <> bssNone then
  DrawBorder(Canvas, ORect, IRect,
                     htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
                     htStyles(BorderStyleType(MargArray[BorderLeftStyle]), BorderStyleType(MargArray[BorderTopStyle]), BorderStyleType(MargArray[BorderRightStyle]), BorderStyleType(MargArray[BorderBottomStyle])),
                     MargArray[BackgroundColor], ParentSectionList.Printing)
end;

procedure TBlock.DrawTheList(Canvas: TCanvas; ARect: TRect; ClipWidth, X,
                          XRef, YRef :integer);
{draw the list sorted by Z order.}
var
  I: integer;
  SB: TSectionBase;
  SaveID: TObject;
begin
if Positioning = posAbsolute then
  with MyCell do
    begin
    SaveID := IMgr.CurrentID;
    IMgr.Reset(RefIMgr.LfEdge, RefIMgr.LfEdge+IMgr.Width);
    IMgr.ClipWidth := ClipWidth;
    IMgr.CurrentID := SaveID;
    end
else MyCell.IMgr.ClipWidth := ClipWidth;
with DrawList do
  for I := 0 to Count-1 do
    begin
    SB := TSectionBase(Items[I]);
    SB.Draw1(Canvas, ARect, MyCell.IMgr, X, XRef, YRef);
    end;
end;

procedure TBlock.FormTree(Indent: string; var Tree: string);
var
  MyIndent: string;
  TM, BM: string;
begin
MyIndent := Indent + '   ';
TM := IntToStr(MargArray[MarginTop]);
BM := IntToStr(MargArray[MarginBottom]);
Tree := Tree+Indent+TagClass+'  '+TM+'  '+BM+^M+^J;
MyCell.FormTree(MyIndent, Tree);
end;

{----------------TTableAndCaptionBlock.Create}
constructor TTableAndCaptionBlock.Create(Master: TSectionList; Prop: TProperties;
    AnOwnerCell: TCellBasic; Attributes: TAttributeList; ATableBlock: TTableBlock);
var
  I: integer;
begin
inherited Create(Master, Prop, AnOwnerCell, Attributes);
TableBlock := ATableBlock;
Justify := TableBlock.Justify;

for I := 0 to Attributes.Count-1 do
  with TAttribute(Attributes[I]) do
    case Which of
      AlignSy:
        if CompareText(Name, 'CENTER') = 0 then
          Justify := Centered
        else if CompareText(Name, 'LEFT') = 0 then
          begin
          if FloatLR = ANone then
            FloatLR := ALeft;
          end
        else if CompareText(Name, 'RIGHT') = 0 then
          begin
          if FloatLR = ANone then
            FloatLR := ARight;
          end;
      end;
TableID := Attributes.TheID;

{CollapseMargins has already been called by TableBlock, copy the results here}
MargArray[MarginTop] := TableBlock.MargArray[MarginTop];
MargArray[MarginBottom] := TableBlock.MargArray[MarginBottom];

TagClass := 'TableAndCaption.';
end;

{----------------TTableAndCaptionBlock.CancelUsage}
procedure TTableAndCaptionBlock.CancelUsage;
{called when it's found that this block isn't needed (no caption)}
begin
{assign the ID back to the Table}
if TableID <> '' then
  ParentSectionList.IDNameList.AddObject(TableID, TableBlock);
end;

{----------------TTableAndCaptionBlock.CreateCopy}
constructor TTableAndCaptionBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TTableAndCaptionBlock;
  Item: TObject;
  I1, I2: integer;
begin
inherited;
TT := T as TTableAndCaptionBlock;
TopCaption := TT.TopCaption;
Justify := TT.Justify;
TagClass := 'TableAndCaption.';
I1 := Ord(TopCaption);
I2 := Ord(not TopCaption);
Item := MyCell.Items[I2];
FCaptionBlock := (Item as TBlock);
Item := MyCell.Items[I1];
TableBlock := (Item as TTableBlock);
end;

procedure TTableAndCaptionBlock.SetCaptionBlock(Value: TBlock);
begin
FCaptionBlock := Value;
TableBlock.HasCaption := True;
end;

{----------------TTableAndCaptionBlock.FindWidth}
function TTableAndCaptionBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  Mx, Mn, FWidth: integer;
begin
BorderStyle := bssNone;   {has no border}
MargArray[BorderLeftWidth] := 0;
MargArray[BorderTopWidth] := 0;
MargArray[BorderRightWidth] := 0;
MargArray[BorderBottomWidth] := 0;
MargArray[PaddingLeft] := 0;
MargArray[PaddingTop] := 0;
MargArray[PaddingRight] := 0;
MargArray[PaddingBottom] := 0;
MargArray[BackgroundColor] := clNone;

TableBlock.FloatLR := ANone;
TableBlock.Table.Float := False;

CaptionBlock.MinMaxWidth(Canvas, Mn, Mx);
FWidth := TableBlock.FindWidth1(Canvas, AWidth, MargArray[MarginLeft]+MargArray[MarginRight]);
Result := IntMax(FWidth, Mn);
if (Result < AWidth) and (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
  case Justify of
    Centered:
      MargArray[MarginLeft] := (AWidth - Result) div 2;
    Right:
      MargArray[MarginLeft] := AWidth - Result;
  end;
TableBlock.Justify := Centered;
end;

{----------------TTableAndCaptionBlock.MinMaxWidth}
procedure TTableAndCaptionBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
var
  Mx, Mn, MxTable, MnTable: integer;
begin
TableBlock.MinMaxWidth(Canvas, MnTable, MxTable);
FCaptionBlock.MinMaxWidth(Canvas, Mn, Mx);
Min := IntMax(MnTable, Mn);
Max := IntMax(MxTable, Mn);
end;

function TTableAndCaptionBlock.FindDocPos(SourcePos: integer; Prev: boolean): integer;
begin
if not Prev then
  begin
  Result := FCaptionBlock.FindDocPos(SourcePos, Prev);
  if Result < 0 then
    Result := TableBlock.FindDocPos(SourcePos, Prev);
  end
else  {Prev, iterate backwards}
  begin
  Result := TableBlock.FindDocPos(SourcePos, Prev);
  if Result < 0 then
    Result := FCaptionBlock.FindDocPos(SourcePos, Prev);
  end;
end;

{----------------TTableBlock.Create}
constructor TTableBlock.Create(Master: TSectionList; Prop: TProperties;
    	AnOwnerCell: TCellBasic; ATable: ThtmlTable; TableAttr: TAttributeList;
        TableLevel: integer);
var
  I, AutoCount: integer;
  Percent: boolean;
  J: PropIndices;
begin
inherited Create(Master, Prop, AnOwnerCell, TableAttr);
Table := ATable;
Justify := NoJustify;

for I := 0 to TableAttr.Count-1 do
  with TAttribute(TableAttr[I]) do
    case Which of
      AlignSy:
        if CompareText(Name, 'CENTER') = 0 then
          Justify := Centered
        else if CompareText(Name, 'LEFT') = 0 then
          begin
          if FloatLR = ANone then
            FloatLR := ALeft;
          end
        else if CompareText(Name, 'RIGHT') = 0 then
          begin
          if FloatLR = ANone then
            FloatLR := ARight;
          end;
      BGColorSy:
        BkGnd := ColorFromString(Name, False, BkColor);
      BackgroundSy:
        if not Assigned(BGImage) then
          begin
          BGImage := TImageObj.SimpleCreate(Master, Name);
          PRec[1].PosType := pDim;
          PRec[1].Value := 0;
          PRec[1].RepeatD := True;
          PRec[2] := PRec[1];
          end;
      HSpaceSy: HSpace := IntMin(40, Abs(Value));
      VSpaceSy: VSpace := IntMin(200, Abs(Value));
      WidthSy:
        if Pos('%', Name) > 0 then
          begin
          if (Value > 0) and (Value <= 100) then WidthAttr := Value*10;
          AsPercent := True;
          end
        else WidthAttr := Value;

      HeightSy:
        if (VarType(MargArrayO[Height]) in VarInt) and (MargArrayO[Height] = IntNull) then
          MargArrayO[Height] := Name;
      end;

if (Table.Border > 0) and (MargArrayO[BorderLeftStyle]=bssNone)
                      and (MargArrayO[BorderTopStyle]=bssNone)
                      and (MargArrayO[BorderRightStyle]=bssNone)
                      and (MargArrayO[BorderBottomStyle]=bssNone) then
  begin  {no CSS border}
  for J := BorderTopWidth to BorderLeftWidth do
    MargArrayO[J] := Table.Border;
  for J := BorderTopStyle to BorderLeftStyle do
    MargArrayO[J] := bssOutSet;
  TableBorder := True;
  end
else TableBorder := False;

{need to see if width is defined in style}
Percent := (VarType(MargArrayO[Width]) = VarString) and (Pos('%', MargArrayO[Width]) > 0);
ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, BorderStyle, AutoCount, MargArray);
if MargArray[Width] > 0 then
  begin
  if Percent then
    begin
    AsPercent := True;
    WidthAttr := IntMin(1000, MargArray[Width] * 10);
    end
  else
    begin
    WidthAttr := MargArray[Width];
    {By custom (not by specs), tables handle CSS Width property differently.  The
     Width includes the padding and border.}
    MargArray[Width] := WidthAttr-MargArray[BorderLeftWidth]-MargArray[BorderRightWidth]
                        -MargArray[PaddingLeft]-MargArray[PaddingRight];
    MargArrayO[Width] := MargArray[Width];
    AsPercent := False;
    end;
  end;

CollapseMargins;
Table.Float := FloatLR in [ALeft, ARight];
if Table.Float and (ZIndex = 0) then
  ZIndex := 1;
end;

{----------------TTableBlock.CreateCopy}
constructor TTableBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TTableBlock;
  Item: TObject;
begin
inherited;
TT := T as TTableBlock;
System.Move(TT.WidthAttr, WidthAttr, DWord(@Justify)-DWord(@WidthAttr)+Sizeof(Justify));
Item := MyCell.Items[0];
Table := Item as ThtmlTable;
end;

{----------------TTableBlock.MinMaxWidth}
procedure TTableBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
var
  TmpWidth: integer;
begin
TmpWidth := 0;
if AsPercent then
  Table.tblWidthAttr := 0
else
  begin
  TmpWidth := IntMax(0, WidthAttr-MargArray[BorderLeftWidth]-MargArray[BorderRightWidth]
                        -MargArray[PaddingLeft]-MargArray[PaddingRight]);
  Table.tblWidthAttr := TmpWidth;
  end;
inherited MinMaxWidth(Canvas, Min, Max);
if TmpWidth > 0 then
  begin
  Min := IntMax(Min, TmpWidth);
  Max := Min;
  end;
end;

{----------------TTableBlock.FindWidth1}
function TTableBlock.FindWidth1(Canvas: TCanvas; AWidth, ExtMarg: integer): integer;
{called by TTableAndCaptionBlock to assist in it's FindWidth Calculation.
 This method is called before TTableBlockFindWidth but is called only if there
 is a caption on the table.  AWidth is the full width available to the
 TTableAndCaptionBlock.}
var
  LeftSide, RightSide: integer;
  Min, Max, Allow: integer;
begin
MargArray[MarginLeft] := 0;
MargArray[MarginRight] := 0;
MargArray[MarginTop] := 0;
MargArray[MarginBottom] := 0;

LeftSide := MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
RightSide := MargArray[PaddingRight]+MargArray[BorderRightWidth];

Table.tblWidthAttr := 0;
if WidthAttr > 0 then
  begin
  if AsPercent then
    Result := IntMin(MulDiv(AWidth, WidthAttr, 1000), AWidth-ExtMarg)
  else
    Result := WidthAttr;
  Result := Result - (LeftSide + RightSide);
  Table.tblWidthAttr := Result;
  Table.MinMaxWidth(Canvas, Min, Max);
  Result := IntMax(Min, Result);
  Table.tblWidthAttr := Result;
  end
else
  begin
  Table.MinMaxWidth(Canvas, Min, Max);
  Allow := AWidth - LeftSide - RightSide;
  if Max <= Allow then
    Result := Max
  else if Min >= Allow then
    Result := Min
  else Result := Allow;
  end;
Result := Result + LeftSide + RightSide;
end;

{----------------TTableBlock.FindWidth}
function TTableBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  LeftSide, RightSide: integer;
  Min, Max, Allow: integer;
begin
if Not HasCaption then
  begin
  if MargArray[MarginLeft] = Auto then
    MargArray[MarginLeft] := 0;
  if MargArray[MarginRight] = Auto then
    MargArray[MarginRight] := 0;

  if FloatLR in [ALeft, ARight] then
    begin
    if MargArray[MarginLeft] = 0 then
      MargArray[MarginLeft] := HSpace;
    if MargArray[MarginRight] = 0 then
      MargArray[MarginRight] := HSpace;
    if MargArray[MarginTop] = 0 then
      MargArray[MarginTop] := VSpace;
    if MargArray[MarginBottom] = 0 then
      MargArray[MarginBottom] := VSpace;
    end;
  end
else
  begin
  MargArray[MarginLeft] := 0;
  MargArray[MarginRight] := 0;
  end;

if BkGnd and (MargArray[BackgroundColor] = clNone) then
  MargArray[BackgroundColor]  := BkColor;
Table.BkGnd := (MargArray[BackgroundColor] <> clNone) and not Assigned(BGImage);
Table.BkColor := MargArray[BackgroundColor];   {to be passed on to cells}

LeftSide := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
RightSide := MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth];

if not HasCaption then
  Table.tblWidthAttr := 0;
if WidthAttr > 0 then
  begin
  if not HasCaption then  {already done if HasCaption}
    begin
    if AsPercent then
      begin
      Result := MulDiv(AWidth, WidthAttr, 1000);
      Dec(Result, (LeftSide+RightSide));
      end
    else
      Result := WidthAttr - (MargArray[PaddingLeft]+MargArray[BorderLeftWidth]
                  +MargArray[PaddingRight]+MargArray[BorderRightWidth]);
    Table.tblWidthAttr := Result;
    Table.MinMaxWidth(Canvas, Min, Max);
    Result := IntMax(Min, Result);
    Table.tblWidthAttr := Result;
    end
  else Result := Table.tblWidthAttr;
  end
else
  begin
  Table.MinMaxWidth(Canvas, Min, Max);
  Allow := AWidth - LeftSide - RightSide;
  if Max <= Allow then
    Result := Max
  else if Min >= Allow then
    Result := Min
  else Result := Allow;
  end;
MargArray[Width] := Result;

if (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) and
     (Result + LeftSide + RightSide < AWidth) then
  case Justify of
    Centered:
      MargArray[MarginLeft] := (AWidth - (Result + LeftSide + RightSide)) div 2;
    Right:
      MargArray[MarginLeft] := AWidth - (Result + LeftSide + RightSide);
  end;
end;

function TTableBlock.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer;
           IMgr: IndentManager; var MaxWidth: integer; var Curs: integer): integer;
var
  X1, Tmp: integer;
begin
if not (FloatLR in [ALeft, ARight]) then
  begin
  Tmp := X;
  X := IntMax(Tmp, IMgr.LeftIndent(Y));
  TableIndent := X-Tmp;
  X1 := IntMin(Tmp+AWidth, IMgr.RightSide(Y));
  AWidth := X1 - X;
  end;
Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
end;

function TTableBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;
begin
X := X+TableIndent;
Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
end;

procedure TTableBlock.DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect);
begin
with Table, IRect do
  if TableBorder then
    begin
    if (BorderColorLight = clBtnHighLight) and (BorderColorDark = clBtnShadow) then
      RaisedRect(ParentSectionList, Canvas, Left-1, Top-1, Right,
                                         Bottom, True, Border)
    else
      RaisedRectColor(ParentSectionList, Canvas, Left-1, Top-1, Right,
                   Bottom, BorderColorLight, BorderColorDark, True, Border);
    end
  else inherited;
end;

procedure TTableBlock.AddSectionsToList;
begin   {Sections in Table not added only table itself}
ParentSectionList.PositionList.Add(Table);
end;

constructor THRBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
begin
inherited;
Align := (T as THRBlock).Align;
end;

{----------------THRBlock.FindWidth}
function THRBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  LeftSide, RightSide, SWidth: integer;
  Diff: integer;
begin
if Positioning = posAbsolute then
  Align := Left;
LeftSide := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
RightSide := MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth];
SWidth := MargArray[Width];

if SWidth > 0 then
   Result := IntMin(SWidth, AWidth - LeftSide - RightSide)
else Result := IntMax(15, AWidth - LeftSide - RightSide);
MargArray[Width] := Result;
{note that above could be inherited; if LeftSide and Rightside were fields
of TBlock}

if Align <> Left then
  begin
  Diff := AWidth-Result-LeftSide-RightSide;
  if Diff > 0 then
    case Align of
      Centered:  Inc(MargArray[MarginLeft], Diff div 2);
      Right:  Inc(MargArray[MarginLeft], Diff);
      end;
  end;
if not ParentSectionList.IsCopy then
  THorzline(MyHRule).VSize := MargArray[StyleUn.Height];
end;

{----------------TBlockLI.Create}
constructor TBlockLI.Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
                Sy: Symb; APlain: boolean; AIndexType: char; AListNumb,
                ListLevel: integer; Attributes: TAttributeList);
var
  Tmp: ListBulletType;
  S: string;
  TmpFont: TMyFont;
begin
inherited Create(Master, Prop, AnOwnerCell, Attributes);
case Sy of
  UlSy, DirSy, MenuSy:
    begin
    ListType := Unordered;
    if APlain then
      ListStyleType := lbNone
    else
      case ListLevel Mod 3 of
        1: ListStyleType := lbDisc;
        2: ListStyleType := lbCircle;
        0: ListStyleType := lbSquare;
        end;
    end;
  OLSy:
      begin
      ListType := Ordered;
      case AIndexType of
        'a': ListStyleType := lbLowerAlpha;
        'A': ListStyleType := lbUpperAlpha;
        'i': ListStyleType := lbLowerRoman;
        'I': ListStyleType := lbUpperRoman;
        else ListStyleType := lbDecimal;
        end;
      end;
  DLSy: ListType := Definition;
  else
    begin
    ListType := liAlone;
    ListStyleType := lbDisc;
    if (VarType(MargArrayO[MarginLeft]) in varInt) and
         ((MargArrayO[MarginLeft] = IntNull) or (MargArrayO[MarginLeft] = 0)) then
      MargArrayO[MarginLeft] := 16;
    end;
  end;
ListNumb := AListNumb;

Tmp := Prop.GetListStyleType;
if Tmp <> lbBlank then
  ListStyleType := Tmp;
ListFont := TMyFont.Create;
TmpFont := Prop.GetFont;
ListFont.Assign(TmpFont);
TmpFont.Free;

S := Prop.GetListStyleImage;
if S <> '' then
  Image := TImageObj.SimpleCreate(Master, S);
end;

constructor TBlockLI.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TBlockLI;
begin
inherited CreateCopy(AMasterList, T);
TT := T as TBlockLI;
ListType := TT.ListType;
ListNumb := TT.ListNumb;
ListStyleType := TT.ListStyleType;
if Assigned(TT.Image) then
  Image := TImageObj.CreateCopy(AMasterList, TT.Image);
ListFont := TMyFont.Create;
ListFont.Assign(TT.ListFont);
end;

destructor TBlockLI.Destroy;
begin
ListFont.Free;
Image.Free;
inherited;
end;

function TBlockLI.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
if Assigned(Image) then
  begin
  Image.DrawLogic(ParentSectionList, Canvas, Nil, 100, 0);
  if (Image.Image = ErrorBitmap) then
    begin
    Image.Free;
    Image := Nil;
    end;
  end;
ParentSectionList.FirstLineHtPtr := @FirstLineHt;
FirstLineHt := 0;
try
  Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
finally
  ParentSectionList.FirstLineHtPtr := Nil;
  end;
end;

{----------------TBlockLI.Draw}
function TBlockLI.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;

const
  MaxRoman = 20;
  LowRoman: array[1..MaxRoman] of string[5] = ('i', 'ii', 'iii', 'iv', 'v', 'vi',
     'vii', 'viii', 'ix', 'x', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvi', 'xvii',
     'xviii', 'xix', 'xx');
  HighRoman: array[1..MaxRoman] of string[5] = ('I', 'II', 'III', 'IV', 'V', 'VI',
     'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 'XV', 'XVI', 'XVII',
     'XVIII', 'XIX', 'XX');
var
  NStr : string[7];
  BkMode, TAlign: integer;
  PenColor, BrushColor: TColor;
  PenStyle: TPenStyle;
  BrushStyle: TBrushStyle;
  YB, AlphaNumb: integer;

  procedure Circle(X, Y, Rad: integer);
  begin
  Canvas.Ellipse(X, Y-Rad, X+Rad, Y);
  end;

begin
Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);

X := X+Indent;

if FirstLineHt > 0 then
  begin
  YB := FirstLineHt-ParentSectionList.YOff;
  if (YB < ARect.Top-50) or (YB > ARect.Bottom+50) then
    Exit;
  if Assigned(Image) and (Image.Image <> DefBitmap) and ParentSectionList.ShowImages then
    begin
    Image.DoDraw(Canvas, X-16, YB-Image.ObjHeight, Image.Image, Image.Mask);
    end

  else if not (ListType in [None, Definition]) then
    begin
    if ListStyleType in [lbDecimal, lbLowerAlpha, lbLowerRoman, lbUpperAlpha, lbUpperRoman] then
      begin
      AlphaNumb := IntMin(ListNumb-1, 25);
      case ListStyleType of
        lbLowerAlpha: NStr := chr(ord('a')+AlphaNumb);
        lbUpperAlpha: NStr := chr(ord('A')+AlphaNumb);
        lbLowerRoman: NStr := LowRoman[IntMin(ListNumb, MaxRoman)];
        lbUpperRoman: NStr := HighRoman[IntMin(ListNumb, MaxRoman)];
        else NStr := IntToStr(ListNumb);
        end;
      Canvas.Font := ListFont;
      NStr := NStr+'.';
      BkMode := SetBkMode(Canvas.Handle, Transparent);
      TAlign := SetTextAlign(Canvas.Handle, TA_BASELINE);
      Canvas.TextOut(X-10-Canvas.TextWidth(NStr), YB, NStr);
      SetTextAlign(Canvas.Handle, TAlign);
      SetBkMode(Canvas.Handle, BkMode);
      end
    else if (ListStyleType in [lbCircle, lbDisc, lbSquare]) then
      with Canvas do
        begin
        PenColor := Pen.Color;
        PenStyle := Pen.Style;
        Pen.Color := ListFont.Color;
        Pen.Style := psSolid;
        BrushStyle := Brush.Style;
        BrushColor := Brush.Color;
        Brush.Style := bsSolid;
        Brush.Color := ListFont.Color;
        case ListStyleType of
          lbCircle:
             begin
             Brush.Style := bsClear;
             Circle(X-16, YB, 7);
             end;
          lbDisc:
             Circle(X-15, YB-1, 5);
          lbSquare: Rectangle(X-15, YB-6, X-10, YB-1);
          end;
        Brush.Color := BrushColor;
        Brush.Style := BrushStyle;
        Pen.Color := PenColor;
        Pen.Style := PenStyle;
        end;
    end;
  end;
end;

{----------------TBodyBlock.Create}
constructor TBodyBlock.Create(Master: TSectionList; Prop: TProperties;
                   AnOwnerCell: TCellBasic; Attributes: TAttributeList);
var
  PRec: PtPositionRec;
  Image: string;
  Val: TColor;
begin
inherited;
positioning := PosStatic;  {7.28}
Prop.GetBackgroundPos(0, 0, PRec);
  if Prop.GetBackgroundImage(Image) and (Image <> '') then
    Master.SetBackgroundBitmap(Image, PRec);
Val := Prop.GetBackgroundColor;
if Val <> clNone then
  Master.SetBackGround(Val or PalRelative);
end;

{----------------TBodyBlock.GetURL}
function TBodyBlock.GetURL(Canvas: TCanvas; X: integer; Y: integer;
     var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
     var ATitle: string): guResultType;
begin
if (BlockTitle <> '') then
  begin
  ATitle := BlockTitle;
  Include(Result, guTitle);
  end;
Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
end;

{----------------TBodyBlock.DrawLogic}
function TBodyBlock.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
         var MaxWidth: integer; var Curs: integer): integer;
var
  ScrollWidth: integer;
  Lindex, RIndex, AutoCount: integer;
  SaveID: TObject;
  ClientContentBot: integer;
begin
YDraw := Y;
StartCurs := Curs;
ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderStyle, AutoCount, MargArray);

NewWidth := IMgr.Width - (MargArray[MarginLeft]+MargArray[PaddingLeft]+
            MargArray[BorderLeftWidth] + MargArray[MarginRight]+
            MargArray[PaddingRight]+MargArray[BorderRightWidth]);

X := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
DrawTop := MargArray[MarginTop];

MyCell.IMgr := IMgr;

SaveID := IMgr.CurrentID;
Imgr.CurrentID := Self;
LIndex := IMgr.SetLeftIndent(X, Y);
RIndex := IMgr.SetRightIndent(X+NewWidth, Y);

ContentTop := Y+MargArray[MarginTop]+MargArray[PaddingTop]+MargArray[BorderTopWidth];
ContentLeft := X;
MyCell.DoLogicX(Canvas, X, ContentTop, 0, 0, NewWidth,
    AHeight-MargArray[MarginTop]-MargArray[MarginBottom], BlHt, ScrollWidth, Curs);

Len := Curs-StartCurs;

ClientContentBot := IntMax(ContentTop, MyCell.tcContentBot);
ContentBot := ClientContentBot + MargArray[PaddingBottom]+
              MargArray[BorderBottomWidth]+MargArray[MarginBottom];
DrawBot := IntMax(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom]
           +MargArray[BorderBottomWidth];

MyCell.tcDrawTop := 0;
MyCell.tcContentBot := 999000;

Result := DrawBot+MargArray[MarginBottom]-Y;
SectionHeight := Result;
IMgr.FreeLeftIndentRec(LIndex);
IMgr.FreeRightIndentRec(RIndex);
DrawHeight := IMgr.ImageBottom - Y;  {in case image overhangs}
Imgr.CurrentID := SaveID;
if DrawHeight < SectionHeight then
  DrawHeight := SectionHeight;
MaxWidth := IntMax(IMgr.Width, IntMax(ScrollWidth, NewWidth)+MargArray[MarginLeft]+MargArray[MarginRight]);
if DrawList.Count = 0 then
  DrawSort;
end;

{----------------TBodyBlock.Draw}
function TBodyBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  SaveID: TObject;
  Y: integer;
begin
Y := YDraw;
Result := Y+SectionHeight;

X := IMgr.LfEdge+MargArray[MarginLeft]+MargArray[BorderLeftWidth]+MargArray[PaddingLeft];
SaveID := IMgr.CurrentID;
Imgr.CurrentID := Self;
DrawTheList(Canvas, ARect, NewWidth, X, IMgr.LfEdge, 0);
Imgr.CurrentID := SaveID;
end;

{----------------TSectionList}
constructor TSectionList.Create(Owner, APaintPanel: TWinControl);
begin
inherited Create(Self);
TheOwner := Owner;
PPanel := APaintPanel;
IDNameList := TIDNameList.Create(Self);
htmlFormList := TFreeList.Create;
AGifList := TList.Create;
MapList := TFreeList.Create;
FormControlList := TList.Create;
MissingImages := TStringList.Create;
MissingImages.Sorted := False;
LinkList := TList.Create;
PanelList := TList.Create;
Styles := TStyleList.Create(Self);
DrawList := TDrawList.Create;
PositionList := TList.Create;
TabOrderList := TStringList.Create;
TabOrderList.Sorted := True;
TabOrderList.Duplicates := dupAccept;
InLineList := TInlineList.Create(Self);
ScaleX := 1.0;
ScaleY:= 1.0;
end;

{----------------TSectionList.CreateCopy}
constructor TSectionList.CreateCopy(T: TSectionList);
begin
PrintTableBackground := T.PrintTableBackground;
PrintBackground := T.PrintBackground;
BitmapList := T.BitmapList;     {same list}
InlineList := T.InlineList;     {same list}
IsCopy := True;
inherited CreateCopy(Self, T);
System.Move(T.ShowImages, ShowImages, DWord(@Background)-Dword(@ShowImages)+Sizeof(integer));
BitmapName := '';
BackgroundBitmap := Nil;
BackgroundMask := Nil;
BackgroundAniGif := Nil;
BitmapLoaded := False;
htmlFormList := TFreeList.Create;    {no copy of list made}
AGifList := TList.Create;
Timer := Nil;
MapList := TFreeList.Create;
MissingImages := TStringList.Create;
PanelList := TList.Create;
DrawList := TDrawList.Create;
ScaleX := 1.0;
ScaleY:= 1.0;
end;

destructor TSectionList.Destroy;
begin
Clear;
IDNameList.Free;
htmlFormList.Free;
MapList.Free;
AGifList.Free;
Timer.Free;
FormControlList.Free;
MissingImages.Free;
LinkList.Free;
PanelList.Free;
Styles.Free;
DrawList.Free;
PositionList.Free;
TabOrderList.Free;
if not IsCopy then
  TInlineList(InlineList).Free;
inherited Destroy;
end;

function TSectionList.GetURL(Canvas: TCanvas; X: integer; Y: integer;
             var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
             var ATitle: string): guResultType;
var
  OldLink: TFontObj;
  OldImage: TImageObj;
begin
OldLink := ActiveLink;
OldImage := ActiveImage;
ActiveLink := Nil;
ActiveImage := Nil;
Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
if LinksActive and (ActiveLink <> OldLink) then
  begin
  if OldLink <> Nil then
    OldLink.SetAllHovers(LinkList, False);
  if ActiveLink <> Nil then
    ActiveLink.SetAllHovers(LinkList, True);
  PPanel.Invalidate;
  end;
if (ActiveImage <> OldImage) then
  begin
  if OldImage <> Nil then
    OldImage.Hover := hvOff;
  end;
if ActiveImage <> Nil then
  if Word(GetKeyState(VK_LBUTTON)) and $8000 <> 0 then
    ActiveImage.Hover := hvOverDown
  else
    ActiveImage.Hover := hvOverUp;
end;

procedure TSectionList.LButtonDown(Down: boolean);
{called from htmlview.pas when left mouse button depressed}
begin
if ActiveImage <> Nil then
  begin
  if Down then
    ActiveImage.Hover := hvOverDown
  else
    ActiveImage.Hover := hvOverUp;
  PPanel.Invalidate;
  end;
end;

procedure TSectionList.CancelActives;
begin
if Assigned(ActiveLink) or Assigned(ActiveImage) then
  PPanel.Invalidate;
if Assigned(ActiveLink) then
  begin
  ActiveLink.SetAllHovers(LinkList, False);
  ActiveLink := Nil;
  end;
if Assigned(ActiveImage) then
  begin
  ActiveImage.Hover := hvOff;
  ActiveImage := Nil;
  end;
end;

procedure TSectionList.CheckGIFList(Sender: TObject);
var
  I: integer;
  Frame: integer;
begin
if IsCopy then Exit;
Frame := 0;
if Assigned(BackgroundAniGif) then
  Frame := BackgroundAniGif.CurrentFrame;
for I := 0 to AGifList.Count-1 do
    with TGifImage(AGifList.Items[I]) do
      if ShowIt then
        begin
        CheckTime(PPanel);
        end;
if Assigned(BackgroundAniGif) and (Frame <> BackgroundAniGif.CurrentFrame) then
  PPanel.Invalidate;
Timer.Interval := 40;
end;

procedure TSectionList.HideControls;
var
  I, J: integer;
begin
  {After next Draw, hide all formcontrols that aren't to be shown}
  for I := 0 to htmlFormList.Count-1 do
    with ThtmlForm(htmlFormList.Items[I]) do
      for J := 0 to ControlList.Count-1 do
        with  TFormControlObj(ControlList.Items[J]) do
          ShowIt := False;
  for I := 0 to PanelList.Count-1 do
    TPanelObj(PanelList[I]).ShowIt := False;   {same for panels}
end;

procedure TSectionList.SetYOffset(Y: integer);
begin
YOff := Y;
YOffChange := True;
HideControls;
end;

procedure TSectionList.Clear;
begin
if not IsCopy then
  begin
  IDNameList.Clear;
  PositionList.Clear;
  TInlineList(InlineList).Clear;
  end;
BackgroundBitmap := Nil;
BackgroundMask := Nil;
BackgroundAniGif := Nil;
if BitmapLoaded and (BitmapName <> '') then
  BitmapList.DecUsage(BitmapName);
BitmapName := '';
BitmapLoaded := False;
AGifList.Clear;
Timer.Free;
Timer := Nil;
SelB := 0;
SelE := 0;
MapList.Clear;
MissingImages.Clear;
if Assigned(LinkList) then
  LinkList.Clear;
ActiveLink := Nil;
ActiveImage := Nil;
PanelList.Clear;
if not IsCopy then
  Styles.Clear;
if Assigned(TabOrderList) then
  TabOrderList.Clear;
inherited Clear;
htmlFormList.Clear;
if Assigned(FormControlList) then
  FormControlList.Clear;
end;

procedure TSectionList.ClearLists;
{called from DoBody to clear some things when starting over}
begin
PanelList.Clear;
if Assigned(FormControlList) then
  FormControlList.Clear;
end;

{----------------TSectionList.GetSelLength:}
function TSectionList.GetSelLength: integer;
var
  I: integer;
begin
Result := 0;
if SelE <= SelB then Exit;   {nothing to do}
CB := SelTextCount.Create;
try
  for I := 0 to Count-1 do
    with TSectionBase(Items[I]) do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
  Result := CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.CopyToClipboard}
procedure TSectionList.CopyToClipboardA(Leng: integer);
var
  I: integer;
  SB: TSectionBase;
begin
if SelE <= SelB then Exit;   {nothing to do}
try
  CB := ClipBuffer.Create(Leng);
  for I := 0 to Count-1 do
    begin
    SB := TSectionBase(Items[I]);
    with SB do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
    end;
  CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.GetSelTextBuf}
function TSectionList.GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;

var
  I: integer;
begin
if BufSize >= 1 then
  begin
  Buffer[0] := #0;
  Result := 1;
  end
else Result := 0;
if SelE <= SelB then Exit;   {nothing to do}
CB := SelTextBuf.Create(Buffer, BufSize);
try
  for I := 0 to Count-1 do
    with TSectionBase(Items[I]) do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
  Result := CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.DoLogic}
function TSectionList.DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight, BlHt: integer;
              var ScrollWidth: integer; var Curs: integer): integer;
var
  I, J: integer;
begin
Inc(CycleNumber);
TableNestLevel := 0;
NLevel := 0;
InLogic2 := False;
if Assigned(Timer) then Timer.Enabled := False;
for I := 0 to htmlFormList.Count-1 do
  ThtmlForm(htmlFormList.Items[I]).SetSizes(Canvas);
SetTextJustification(Canvas.Handle, 0, 0);
TInlineList(InlineList).NeedsConverting := True;

{set up the tab order for form controls according to the TabIndex attributes}
if Assigned(TabOrderList) and (TabOrderList.Count > 0) then
  with TabOrderList do
    begin
    J := 0;  {tab order starts with 0}
    for I := 0 to Count-1 do    {list is sorted into proper order}
      begin
      if Objects[I] is TFormControlObj then
        begin
        with Objects[I] as TFormControlObj do
          if Assigned(FControl) then
            begin
            FControl.TabOrder := J;
            Inc(J);
            end;
        end
      else if Objects[I] is ThtTabControl then
        begin
        ThtTabcontrol(Objects[I]).TabOrder := J;
          Inc(J);
        end
      else
        Assert(False, 'Unexpected item in TabOrderList');
      end;
    TabOrderList.Clear;   {only need do this once}
    end;

Result := inherited DoLogic(Canvas, Y, Width, AHeight, BlHt, ScrollWidth, Curs);

for I := 0 to AGifList.Count-1 do
  with TGifImage(AGifList.Items[I]) do
    begin
    Animate := False;    {starts iteration count from 1}
    if not Self.IsCopy then
      Animate := True;
    end;
if not IsCopy and not Assigned(Timer) then
  begin
  Timer := TTimer.Create(TheOwner as ThtmlViewer);
  Timer.Interval := 50;
  Timer.OnTimer := CheckGIFList;
  end;
if Assigned(Timer) then Timer.Enabled := AGifList.Count >= 1;
AdjustFormControls;
if not IsCopy and (PositionList.Count = 0) then
  begin
  AddSectionsToList;
  end;
end;

procedure TSectionList.AdjustFormControls;
var
  I: integer;
  Control: TControl;
  Showing: boolean;

  {$ifndef FastRadio}
  function ActiveInList: boolean; {see if active control is a form control}
  var
    Control: TWinControl;
    I: integer;
  begin
  with FormControlList do
    begin
    Result := False;
    Control := Screen.ActiveControl;
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if FControl = Control then
          begin
          Result := True;
          Break;
          end;
    end;
  end;
  {$endif}

begin
if IsCopy or (FormControlList.Count = 0) then Exit;
with FormControlList do
  {$ifndef FastRadio}
  if not ActiveInList then
    begin  {if none of the formcontrols are active, turn off tabs for those off screen}
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if not ShowIt and Assigned(FControl) then
          FControl.Hide;   {hides and turns off tabs}
    end
  else
  {$endif}
    begin
    Control := TheOwner;   {ThtmlViewer}
    repeat
      Showing := Control.Visible;
      Control := Control.Parent;
    until not Showing or not Assigned(Control);
    if Showing then
      for I := 0 to Count-1 do
        with  TFormControlObj(Items[I]) do
          if not ShowIt and Assigned(FControl) then
            begin
            FControl.Show;   {turns on tabs}
            FControl.Left := -4000;  {but it still can't be seen}
            end;
    end;
end;

{----------------TSectionList.Draw}
function TSectionList.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef :integer): integer;
var
  OldPal: HPalette;
  I: integer;
begin
PageBottom := ARect.Bottom + YOff;
PageShortened := False;
FirstPageItem := True;
TableNestLevel := 0;
SkipDraw := False;

if Assigned(Timer) then Timer.Enabled := False;
for I := 0 to AGifList.Count-1 do
  with TGifImage(AGifList.Items[I]) do
    begin
    ShowIt := False;
    end;
if Assigned(BackgroundAniGif) and not IsCopy then
  BackgroundAniGif.ShowIt := True;
if (ColorBits <= 8) then
  begin
  OldPal := SelectPalette(Canvas.Handle, ThePalette, True);
  RealizePalette(Canvas.Handle);
  end
else OldPal := 0;
DrawList.Clear;
try
  Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y, XRef, YRef);
  DrawList.DrawImages;
finally
  if OldPal <> 0 then
    SelectPalette(Canvas.Handle, OldPal, True);
  end;
if YOffChange then
  begin
  AdjustFormControls;
  {Hide all TPanelObj's that aren't displayed}
  for I := 0 to PanelList.Count-1 do
    with TPanelObj(PanelList[I]) do
      if not ShowIt then
        Panel.Hide;
  YOffChange := False;
  end;
if Assigned(Timer) then Timer.Enabled := AGifList.Count >= 1;
end;

procedure TSectionList.SetFonts(const Name, PreName: String; ASize: integer;
          AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
          LnksActive: boolean; LinkUnderLine: boolean; ACharSet: TFontCharSet;
          MarginHeight, MarginWidth: integer);
begin
Styles.Initialize(Name, PreName, ASize, AColor, AHotspot, AVisitedColor,
         AActiveColor, LinkUnderLine, ACharSet, MarginHeight, MarginWidth);
InitializeFontSizes(ASize);
PreFontName := PreName;
HotSpotColor := AHotSpot;
LinkVisitedColor := AVisitedColor;
LinkActiveColor := AActiveColor;
LinksActive := LnksActive;
SetBackground(ABackground);
end;

procedure TSectionList.SetBackground(ABackground: TColor);
begin
Background := ABackground;
if Assigned(OnBackGroundChange) then
  OnBackgroundChange(Self);
end;

procedure TSectionList.SetBackgroundBitmap(Name: String; const APrec: PtPositionRec);
begin
BackgroundBitmap := Nil;
BackgroundAniGif := Nil;
BitmapName := Name;
BitmapLoaded := False;
BackgroundPRec := APrec;
end;

{----------------TSectionList.InsertImage}
procedure TSectionList.InsertImage(const Src: string; Stream: TMemoryStream;
               var Reformat: boolean);
var
  UName: string;
  I, J: integer;
  Pair: TBitmapItem;
  NonAnimated, Rformat, Error: boolean;
  Image: TgpObject;
  AMask: TBitmap;
  Tr, Transparent: Transparency;
  Obj: TObject;
  Tmp: TGifImage;
begin
Image := Nil;  AMask := Nil;
Error := False;
Reformat := False;
UName := Trim(Uppercase(Src));
I := BitmapList.IndexOf(UName);  {first see if the bitmap is already loaded}
J := MissingImages.IndexOf(UName); {see if it's in missing image list}
if (I = -1) and (J >= 0) then
  begin
  Transparent := NotTransp;
  if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
    begin
    NonAnimated := True;
    if KindOfImage(Stream.Memory) in [GIF, Gif89] then
      Image := CreateAGifFromStream(NonAnimated, Stream);
    if Assigned(Image) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(Image);
        Image := TBitmap.Create;
        TBitmap(Image).Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end;
        Tmp.Free;
        end;
      end
    else
      Image := GetImageAndMaskFromStream(Stream, Transparent, AMask);
    end;
  if Assigned(Image) then  {put in Cache}
    try
      if Assigned(AMask) then Tr := Transparent
        else Tr := NotTransp;
      Pair := TBitmapItem.Create(Image, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair);  {put new bitmap in list}
        BitmapList.DecUsage(UName);    {this does not count as being used yet}
      except
        Pair.Mask := Nil;
        Pair.MImage:= Nil;
        Pair.Free;
      end;
    except  {accept inability to create}
    end
  else
    Error := True;    {bad stream or Nil}
  end;
if (I >= 0) or Assigned(Image) or Error then  {a valid image in the Cache or Bad stream}
  begin
  while J >= 0 do
    begin
    Obj := MissingImages.Objects[J];
    if (Obj = Self) and not IsCopy and not Error then
      BitmapLoaded := False  {the background image, set to load}
    else if (Obj is TImageObj) then
      begin
      TImageObj(Obj).InsertImage(UName, Error, Rformat);
      Reformat := Reformat or Rformat;
      end;
    MissingImages.Delete(J);
    J := MissingImages.IndexOf(UName);
    end;
  end;
end;

{----------------TSectionList.GetTheBitmap}
function TSectionList.GetTheBitmap(const BMName: String; var Transparent: Transparency;
         var AMask: TBitmap; var FromCache, Delay: boolean): TgpObject;
{Note: bitmaps and Mask returned by this routine are on "loan".  Do not destroy
 them}
{Transparent may be set to NotTransp or LLCorner on entry but may discover it's
 TGif here}

{$ifdef ShareWare}
const
  OneTime: boolean = False;
{$endif}

var
  UName: string;
  I: integer;
  Pair: TBitmapItem;
  Tr: Transparency;
  NonAnimated: boolean;
  Stream: TMemoryStream;
  Color: TColor;
  Tmp: TGifImage;

  function LoadImageFromFile(const FName: string; var AMask: TBitmap;
                var Transparent: Transparency): TgpObject;
  var
    Tmp: TGifImage;
  begin   {look for the image file}
  AMask := Nil;
  NonAnimated := True;
  if KindOfImageFile(FName) in [Gif, Gif89] then
    begin
    Result := CreateAGif(FName, NonAnimated);
    if Assigned(Result) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(Result);
        Result := TBitmap.Create;
        TBitmap(Result).Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end
        else if Transparent = LLCorner then
          AMask := GetImageMask(TBitmap(Result), False, 0);
        Tmp.Free;
        end;
      end;
    end
  else
    Result := GetImageAndMaskFromFile(FName, Transparent, AMask);
  end;

begin
{$ifdef ShareWare}
{$Include DemoVers.inc}
{$endif}
AMask := Nil;
Delay := False;
FromCache := False;
if BMName <> '' then
  begin
  UName := Trim(Uppercase(BMName));
  I := BitmapList.IndexOf(UName);  {first see if the bitmap is already loaded}
  if I > -1 then
    begin         {yes, handle the case where the image is already loaded}
    Result := BitmapList.GetImage(I);
    FromCache := True;
    if Result is TBitmap then
      with  BitmapList.Objects[I] as TBitmapItem do
        begin
        if Transp = TGif then
          Transparent := TGif   {it's a transparent GIF}
        else if Transp = Tpng then
          Transparent := TPng
        else if Transparent = LLCorner then
          begin
          if not Assigned (Mask) then  {1st bitmap may not have been marked transp}
            Mask := GetImageMask(TBitmap(MImage), False, 0);
          if Assigned(Mask) then Transp := LLCorner;
          end;
        AMask := Mask;
        end;
    Exit;
    end;

  {The image is not loaded yet, need to get it}
  Result := Nil;
  if Assigned(GetBitmap) or Assigned(GetImage) then
    begin
    if Assigned(GetBitmap) then
      begin {the OnBitmapRequest event}
      Color := -1;
      GetBitmap(TheOwner, BMName, TBitmap(Result), Color);
      if Assigned(Result) then
        if Color <> -1 then
          begin
          AMask := GetImageMask(TBitmap(Result), True, Color);
          Transparent := TGif;
          end
        else if (Transparent = LLCorner) then
          AMask := GetImageMask(TBitmap(Result), False, 0);
      end;
    if Assigned(GetImage) then
      begin    {the OnImageRequest}
      Stream := Nil;
      GetImage(TheOwner, BMName, Stream);
      if Stream = WaitStream then
        Delay := True
      else if not Assigned(Stream) then
        Result := LoadImageFromFile(ThtmlViewer(TheOwner).HTMLExpandFilename(BMName), AMask, Transparent)
      else if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
        begin
        NonAnimated := True;
        if KindOfImage(Stream.Memory) in [GIF, Gif89] then
          Result := CreateAGifFromStream(NonAnimated, Stream);
        if Assigned(Result) then
          begin
          if NonAnimated then
            begin     {else already have animated GIF}
            Tmp := TGifImage(Result);
            Result := TBitmap.Create;
            TBitmap(Result).Assign(Tmp.MaskedBitmap);
            if Tmp.IsTransparent then
              begin
              AMask := TBitmap.Create;
              AMask.Assign(Tmp.Mask);
              Transparent := TGif;
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
    end
  else
    Result := LoadImageFromFile(BMName, AMask, Transparent);
  if Assigned(Result) then  {put in Image List for use later also}
    try
      if Assigned(AMask) then Tr := Transparent
        else Tr := NotTransp;
      Pair := TBitmapItem.Create(Result, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair);  {put new bitmap in list}
      except
        Pair.Mask := Nil;
        Pair.MImage:= Nil;
        Pair.Free;
      end;
    except  {accept inability to create}
    end;
  end
else Result := Nil;
end;

{----------------TSectionList.FindSectionAtPosition}
function TSectionList.FindSectionAtPosition(Pos: integer;
             var TopPos: integer; var Index: integer): TSectionBase;
var
  I: integer;
begin
with PositionList do
  for I := Count-1 downto 0 do
    if TSectionBase(Items[I]).YPosition <= Pos then
      begin
      Result := TSectionBase(Items[I]);
      TopPos := Result.YPosition;
      Index := I;
      Exit;
      end;
Result := Nil;
end;

procedure TSectionList.GetBackgroundBitmap;
var
  Mask: TBitmap;
  Dummy1: Transparency;
  TmpResult: TgpObject;
  FromCache, Delay: boolean;
  Rslt: string;
begin
if ShowImages and not BitmapLoaded and (BitmapName <> '') then
  begin
  if not Assigned(BackgroundBitmap) then
    begin
    Dummy1 := NotTransp;
    if not Assigned(GetBitmap) and not Assigned(GetImage) then
      BitmapName := (TheOwner as ThtmlViewer).HTMLExpandFilename(BitmapName)
    else if Assigned(ExpandName) then
      begin
      ExpandName(TheOwner, BitmapName, Rslt);
      BitmapName := Rslt;
      end;
    TmpResult := GetTheBitmap(BitmapName, Dummy1, Mask, FromCache, Delay); {might be Nil}
    if (TmpResult is TBitmap) or (TmpResult is TGpImage) then
      begin
      BackgroundBitmap := TmpResult;
      BackgroundMask := Mask;
      end
    else if TmpResult is TGifImage then
      begin
      BackgroundBitmap := TGifImage(TmpResult).MaskedBitmap;
      BackgroundMask := TGifImage(TmpResult).Mask;
      if TGifImage(TmpResult).IsAnimated and not IsCopy then
        begin
        BackgroundAniGif := TGifImage(TmpResult);
        AGifList.Add(BackgroundAniGif);
        BackgroundAniGif.Animate := True;
        end;
      end
    {$ifndef NoMetafile}
    else if TmpResult is ThtMetaFile then
      begin
      BackgroundBitmap := ThtMetaFile(TmpResult);
      end
    {$endif}
    else
      begin
      BackgroundBitmap := Nil;
      if Delay then
        MissingImages.AddObject(BitmapName, Self);
      end;
    BitmapLoaded := True;
    end;
  end;
end;

{----------------TSectionList.GetFormcontrolData}
function TSectionList.GetFormcontrolData: TFreeList;
var
  I: integer;
begin
if htmlFormList.Count > 0 then
  begin
  Result := TFreeList.Create;
  for I := 0 to htmlFormList.Count-1 do
    Result.Add(ThtmlForm(htmlFormList[I]).GetFormSubmission);
  end
else Result := Nil;
end;

procedure TSectionList.SetFormcontrolData(T: TFreeList);
var
  I: integer;
begin
try
  for I := 0 to T.Count-1 do
    if htmlFormList.Count > I then
      ThtmlForm(htmlFormList[I]).SetFormData(TStringList(T[I]));
except end;
end;

{----------------TSectionList.FindDocPos}
function TSectionList.FindDocPos(SourcePos: integer; Prev: boolean): integer;
begin
Result := inherited FindDocPos(SourcePos, Prev);
if Result < 0 then    {if not found return 1 past last char}
  Result := Len;
end;

{----------------TSectionList.CursorToXY}
function TSectionList.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  Beyond: boolean;
begin
Beyond := Cursor >= Len;
if Beyond then
  Cursor := Len-1;
Result := inherited CursorToXY(Canvas, Cursor, X, Y);
if Beyond then
  X := X+15;
end;

procedure TSectionList.ProcessInlines(SIndex: integer; Prop: TProperties; Start: boolean);
{called when an inline property is found to specify a border}
var
  I, EmSize, ExSize: integer;
  IR: InlineRec;
  MargArrayO: TVMarginArray;
  Dummy: BorderStyleType;
  Dummy1: integer;
begin
with InlineList do
  begin
  if Start then
    begin    {this is for border start}
    IR := InlineRec.Create;
    InlineList.Add(IR);
    with IR do
      begin
      StartBDoc := SIndex;  {Source index for border start}
      IDB := Prop.ID;    {property ID}
      EndB := 999999;    {end isn't known yet}
      Prop.GetVMarginArray(MargArrayO);
      EmSize := Prop.EmSize;
      ExSize := Prop.ExSize;
      Dummy := bssNone;
      ConvMargArray(MargArrayO, 200, 200, EmSize, ExSize, Dummy, Dummy1, MargArray);
      end;
    end
  else  {this call has end information}
    for I := Count-1 downto 0 do   {the record we want is probably the last one}
      begin
      IR := InlineRec(Items[I]);
      if Prop.ID = IR.IDB then     {check the ID to make sure}
        begin
        IR.EndBDoc := SIndex; {the source position of the border end}
        Break;
        end;
      end;
  end;
end;

{----------------TInlineList.Create}
constructor TInlineList.Create(AnOwner: TSectionList);
begin
inherited Create;
Owner := AnOwner;
NeedsConverting := True;
end;

procedure TInlineList.Clear;
begin
inherited Clear;
NeedsConverting := True;
end;

procedure TInlineList.AdjustValues;
{convert all the list data from source char positions to display char positions}
var
  I: integer;
begin
for I := 0 to Count-1 do
  with InlineRec(Items[I]) do
    begin
    StartB := Owner.FindDocPos(StartBDoc, False);
    EndB := Owner.FindDocPos(EndBDoc, False);
    if StartB = EndB then
      Dec(StartB);  {this takes care of images, form controls}
    end;
NeedsConverting := False;
end;

function TInlineList.GetStartB(I: integer): integer;
begin
if NeedsConverting then
  AdjustValues;
if (I < Count) and (I >= 0) then
  Result := InlineRec(Items[I]).StartB
else Result := 99999999;
end;

function TInlineList.GetEndB(I: integer): integer;
begin
if NeedsConverting then
  AdjustValues;
if (I < Count) and (I >= 0) then
  Result := InlineRec(Items[I]).EndB
else Result := 99999999;
end;

{----------------TCellObj.Create}
constructor TCellObj.Create(Master: TSectionList; AVAlign: AlignmentType;
             Attr: TAttributeList; Prop: TProperties);
{Note: on entry Attr and Prop may be Nil when dummy cells are being created}
var
  I, AutoCount: integer;
  Color: TColor;
  BackgroundImage: string;
  Percent: boolean;
  Algn: AlignmentType;
begin
inherited Create;
Cell := TCellObjCell.Create(Master);
if Assigned(Prop) then
  Cell.Title := Prop.PropTitle;
ColSpan := 1;
RowSpan := 1;
VAlign := AVAlign;
if Assigned(Attr) then
  for I := 0 to Attr.Count-1 do
    with TAttribute(Attr[I]) do
      case Which of
        ColSpanSy:
          if Value > 1 then ColSpan := Value;
        RowSpanSy:
          if Value > 1 then RowSpan := Value;
        WidthSy:
          if Pos('%', Name) > 0 then
            begin
            if (Value > 0) and (Value <= 100) then
              begin
              WidthAttr := Value*10;
              AsPercent := True;
              end;
            end
          else if (Value > 0) then
            WidthAttr := Value;
        HeightSy:
          if Pos('%', Name) = 0 then
            SpecHt := Value
          else
            SpecHtPercent := IntMax(0, IntMin(Value, 100));
        BGColorSy:
          Cell.BkGnd := ColorFromString(Name, False, Cell.BkColor);
        BackgroundSy: BackgroundImage := Name;
        HRefSy:  Cell.Url := Name;
        TargetSy:  Cell.Target := Name;
        end;

if Assigned(Prop) then
  begin    {Caption does not have Prop}
  if Prop.GetVertAlign(Algn) and  (Algn in [Atop, AMiddle, ABottom]) then
    Valign := Algn;
  Prop.GetVMarginArray(MargArrayO);
  EmSize := Prop.EmSize;
  ExSize := Prop.ExSize;
  Percent := (VarType(MargArrayO[Width]) = VarString) and (Pos('%', MargArrayO[Width]) > 0);
  ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, bssNone, AutoCount, MargArray);
  if MargArray[Width] > 0 then
    if Percent then
      begin
      if MargArray[Width] < 100 then
        begin
        AsPercent := True;
        WidthAttr := MargArray[Width] * 10;
        end;
      end
    else
      begin
      WidthAttr := MargArray[Width];
      AsPercent := False;
      end;
  if MargArray[Height] > 0 then
    SpecHt := MargArray[Height];
  Color := Prop.GetBackgroundColor;
  if Color <> clNone then
    begin
    Cell.BkGnd := True;
    Cell.BkColor := Color;
    end;
  Prop.GetBackgroundImage(BackgroundImage);  {'none' will change string to empty}
  if BackgroundImage <> '' then
    begin
    BGImage := TImageObj.SimpleCreate(Master, BackgroundImage);
    Prop.GetBackgroundPos(EmSize, ExSize, PRec);
    end;

  BorderStyle := Prop.GetBorderStyle;
  {In the following, Padding widths in percent aren't accepted}
  ConvMargArrayForCellPadding(MargArrayO, EmSize, ExSize, MargArray);
  PadTop := MargArray[PaddingTop];
  PadRight := MargArray[PaddingRight];
  PadBottom := MargArray[PaddingBottom];
  PadLeft := MargArray[PaddingLeft];

  if BorderStyle <> bssNone then
    begin
    BrdTop := MargArray[BorderTopWidth];
    BrdRight := MargArray[BorderRightWidth];
    BrdBottom := MargArray[BorderBottomWidth];
    BrdLeft := MargArray[BorderLeftWidth];
    end;
  Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
  end;
end;

constructor TCellObj.CreateCopy(AMasterList: TSectionList; T: TCellObj);
begin
inherited create;
Cell := TCellObjCell.CreateCopy(AMasterList, T.Cell);
Move(T.ColSpan, ColSpan, DWord(@Cell)-DWord(@ColSpan));

if AMasterList.PrintTableBackground then
  begin
  Cell.BkGnd := T.Cell.BkGnd;
  Cell.BkColor := T.Cell.BkColor;
  end
else
  Cell.BkGnd := False;
if Assigned(T.BGImage) and AMasterList.PrintTableBackground then
  BGImage := TImageObj.CreateCopy(AMasterList, T.BGImage);
MargArrayO := T.MargArrayO;
MargArray := T.MargArray;
end;

destructor TCellObj.Destroy;
begin
Cell.Free;
BGImage.Free;
TiledImage.Free;
TiledMask.Free;
FullBG.Free;
inherited Destroy;
end;

{----------------TCellObj.InitializeCell}
procedure TCellObj.InitializeCell(TablePadding: integer; const BkImageName: string;
                   const APRec: PtPositionRec; Border: boolean);
begin
if PadTop < 0 then
  PadTop := TablePadding;
if PadRight < 0 then
  PadRight := TablePadding;
if PadBottom < 0 then
  PadBottom := TablePadding;
if PadLeft < 0 then
  PadLeft := TablePadding;
if Border and (BorderStyle = bssNone) then
  begin
  BrdLeft := IntMax(1, BrdLeft);
  BrdRight := IntMax(1, BrdRight);
  BrdTop := IntMax(1, BrdTop);
  BrdBottom := IntMax(1, BrdBottom);
  end;
HzSpace := PadLeft+BrdLeft+BrdRight+PadRight;
VrSpace := PadTop+BrdTop+BrdBottom+PadBottom;

if (BkImageName <> '') and not Assigned(BGImage) then
  begin
  BGImage := TImageObj.SimpleCreate(Cell.MasterList, BkImageName);
  PRec := APrec;
  end;
end;

{----------------TCellObj.DrawLogic2}
procedure TCellObj.DrawLogic2(Canvas : TCanvas; Y, CellSpacing: integer; var Curs: integer);
var
  Dummy: integer;
  Tmp: integer;
begin
if Cell.Count > 0 then
  begin
  Tmp := Ht - VSize - (VrSpace+CellSpacing);
  case VAlign of
    ATop: YIndent := 0;
    AMiddle: YIndent := Tmp div 2;
    ABottom, ABaseline: YIndent := Tmp;
    end;
  Dummy := 0;
  Cell.DoLogic(Canvas, Y+PadTop+BrdTop+CellSpacing+YIndent, Wd-(HzSpace+CellSpacing),
                Ht-VrSpace-CellSpacing, 0, Dummy, Curs);
  end;
if Assigned(BGImage) and Cell.MasterList.ShowImages then
  begin
  BGImage.DrawLogic(Cell.MasterList, Canvas, Nil, 100, 0);
  if BGImage.Image = ErrorBitmap then
    begin
    BGImage.Free;
    BGImage := Nil;
    end
  else
    begin
    BGImage.ImageKnown := True;  {won't need reformat on InsertImage}
    NeedDoImageStuff := True;
    end;
  end;
end;

{----------------TCellObj.Draw}
procedure TCellObj.Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: integer;
           Border: boolean; Light, Dark: TColor);
var
  YO: integer;
  BL, BT, BR, BB, PL, PT, PR, PB: integer;
  ImgOK: boolean;
  IT, IH, FT, Rslt: integer;
  Rgn, SaveRgn: HRgn;
  Point: TPoint;
  SizeV, SizeW: TSize;
  HF, VF: double;
  BRect: TRect;
  Colors: htColorArray;
  Styles: htBorderStyleArray;

  procedure InitFullBg(W, H: integer);
  begin
  if not Assigned(FullBG) then
    begin
    FullBG := TBitmap.Create;
    if Cell.MasterList.IsCopy then
      begin
      FullBG.HandleType := bmDIB;
      if ColorBits <= 8 then
        FullBG.Palette := CopyPalette(ThePalette);
      end;
    end;
  FullBG.Height := IntMax(H, 2);
  FullBG.Width := IntMax(W, 2);
  end;

begin
YO := Y - Cell.MasterList.YOff;

BL := X + CellSpacing;   {Border left and right}
BR := X + Wd;
PL := BL + BrdLeft; {Padding left and right}
PR := BR - BrdRight;

BT := Y - Cell.MasterList.YOff + Cellspacing;   {Border Top and Bottom}
if CellSpacing >= 0 then
  BB := BT + Ht - CellSpacing
else BB := BT + Ht;
PT := BT + BrdTop; {Padding Top and Bottom}
PB := BB - BrdBottom;

IT := IntMax(0, Arect.Top-2-PT);
FT := IntMax(PT, ARect.Top-2);         {top of area drawn, screen coordinates}
IH := IntMin(PB-FT, Arect.Bottom-FT);  {height of area actually drawn}

Cell.MyRect := Rect(BL, BT, BR, BB);
if not (BT <= ARect.Bottom) and (BB >= ARect.Top) then
  Exit;

try
  if NeedDoImageStuff and (BGImage.Image <> DefBitmap) then
    begin
    if BGImage.Image = ErrorBitmap then     {Skip the background image}
      FreeAndNil(BGImage)
    else
      try
        DoImageStuff(Canvas, Wd-CellSpacing, Ht-CellSpacing,
             BGImage, PRec, TiledImage, TiledMask, NoMask);
        if Cell.MasterList.IsCopy and (TiledImage is TBitmap) then
          TBitmap(TiledImage).HandleType := bmDIB;
      except                   {bad image, get rid of it}
        FreeAndNil(BGImage);
        FreeAndNil(TiledImage);
        FreeAndNil(TiledMask);
        end;
    NeedDoImageStuff := False;
    end;

  ImgOK := Not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
             and Cell.MasterList.ShowImages;

  if Cell.BkGnd then
    begin
    Canvas.Brush.Color := Cell.BkColor or PalRelative;
    Canvas.Brush.Style := bsSolid;
    if Cell.MasterList.IsCopy and ImgOK then
      begin
      InitFullBG(PR-PL, IH);
      FullBG.Canvas.Brush.Color := Cell.BkColor or PalRelative;
      FullBG.Canvas.Brush.Style := bsSolid;
      FullBG.Canvas.FillRect(Rect(0, 0, PR-PL, IH));
      end
    else if BorderStyle = bssNone then
      if Border then
        {slip under border to fill gap when printing}
        Canvas.FillRect(Rect(PL-1, FT-1, PR, FT+IH))
      else
        Canvas.FillRect(Rect(PL, FT, PR, FT+IH))
    else
      begin   {slip the fill under any border}
      BRect := Rect(PL, FT, PR, FT+IH);
      if MargArray[BorderRightWidth] > 0 then
        Inc(BRect.Right);
      if MargArray[BorderBottomWidth] > 0 then
        Inc(BRect.Bottom);
      Canvas.FillRect(BRect);
      end;
    end;
  if ImgOK then
    begin
    if not Cell.MasterList.IsCopy then
      if TiledImage is TgpBitmap then
        DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PL, FT, 0, IT, PR-PL, IH)
      else if NoMask then
        BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcCopy)
      else
        begin
        InitFullBG(PR-PL, IH);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, Canvas.Handle, PL, FT, SrcCopy);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
        BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
        end
    else if TiledImage is TgpBitmap then   {printing}
      begin
      if Cell.BkGnd then
        begin
        DrawGpImage(FullBg.Canvas.Handle, TgpImage(TiledImage), 0, 0);
        PrintBitmap(Canvas, PL, FT, PR-PL, IH, FullBG.Handle);
        end
      else
        PrintGpImageDirect(Canvas.Handle, TgpImage(TiledImage), PL, PT,
                   Cell.MasterList.ScaleX, Cell.MasterList.ScaleY);
      end
    else if NoMask then
      PrintBitmap(Canvas, PL, FT, PR-PL, IH, TBitmap(TiledImage).Handle)
    else if Cell.BkGnd then
      begin
      InitFullBG(PR-PL, IH);
      BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
      BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
      BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
      PrintBitmap(Canvas, PL, FT, PR-PL, IH, FullBG.Handle);
      end
    else
      PrintTransparentBitmap3(Canvas, PL, FT, PR-PL, IH, TBitmap(TiledImage), TiledMask, IT, IH);
    end;
except
  end;

try
  if (Cell.Count > 0) and (YO < ARect.Bottom+200) and (YO+Ht > -200) then
    begin
    {clip cell contents to prevent overflow.  First check to see if there is
     already a clip region}
    SaveRgn := CreateRectRgn(0, 0, 1, 1);
    Rslt := GetClipRgn(Canvas.Handle, SaveRgn);  {Rslt = 1 for existing region, 0 for none}
    {Form the region for this cell}
    GetWindowOrgEx(Canvas.Handle, Point); {when scrolling or animated Gifs, canvas may not start at X=0, Y=0}
    if not Cell.MasterList.Printing then
      if IsWin95 then
        Rgn := CreateRectRgn(X+CellSpacing-Point.X, IntMax(YO+CellSpacing-Point.Y, -32000), X+Wd-Point.X, IntMin(YO+Ht-Point.Y, 32000))
      else
        Rgn := CreateRectRgn(X+CellSpacing-Point.X, YO+CellSpacing-Point.Y, X+Wd-Point.X, YO+Ht-Point.Y)
    else
      begin
      GetViewportExtEx(Canvas.Handle, SizeV);
      GetWindowExtEx(Canvas.Handle, SizeW);
      HF := (SizeV.cx/SizeW.cx);  {Horizontal adjustment factor}
      VF := (SizeV.cy/SizeW.cy);  {Vertical adjustment factor}
      if IsWin95 then
        Rgn := CreateRectRgn(Round(HF*(X+CellSpacing-Point.X)-1), IntMax(Round(VF*(YO+CellSpacing-Point.Y)-1), -32000), Round(HF*(X+Wd-Point.X)+1), IntMin(Round(VF*(YO+Ht-Point.Y)), 32000))
      else
        Rgn := CreateRectRgn(Round(HF*(X+CellSpacing-Point.X)-1), Round(VF*(YO+CellSpacing-Point.Y)-1), Round(HF*(X+Wd-Point.X)+1), Round(VF*(YO+Ht-Point.Y)));
      end;
    if Rslt = 1 then {if there was a region, use the intersection with this region}
      CombineRgn(Rgn, Rgn, SaveRgn, Rgn_And);
    SelectClipRgn(Canvas.Handle, Rgn);
    try

      Cell.Draw(Canvas, ARect, Wd-HzSpace-CellSpacing, X+PadLeft+BrdLeft+CellSpacing,
                    Y+PadTop+BrdTop+YIndent, ARect.Left, 0);   {possibly should be IRgn.LfEdge}
    finally
      if Rslt = 1 then   {restore any previous clip region}
        SelectClipRgn(Canvas.Handle, SaveRgn)
      else
        SelectClipRgn(Canvas.Handle, 0);
      DeleteObject(Rgn);
      DeleteObject(SaveRgn);
      end;
    end;

  Cell.DrawYY := Y;
  if BorderStyle <> bssNone then
    begin
    Styles := htStyles(BorderStyleType(MargArray[BorderLeftStyle]), BorderStyleType(MargArray[BorderTopStyle]), BorderStyleType(MargArray[BorderRightStyle]), BorderStyleType(MargArray[BorderBottomStyle]));
    Colors := htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]);
    if (BrdTop=1) and (BrdRight=1) and (BrdBottom=1) and (BrdLeft=1) and
       (Styles[0]=bssSolid) and (Styles[1]=bssSolid) and (Styles[2]=bssSolid) and (Styles[3]=bssSolid) and
       (Colors[1]=Colors[0]) and (Colors[2]=Colors[0]) and(Colors[3]=Colors[0]) then
      RaisedRectColor(Cell.MasterList, Canvas, X+CellSpacing, YO+CellSpacing,
                                  X+Wd-1, YO+Ht-1, Colors[0], Colors[0], False, 1)
    else
      DrawBorder(Canvas, Rect(BL, BT, BR, BB), Rect(PL, PT, PR, PB),
                       Colors, Styles, MargArray[BackgroundColor], Cell.MasterList.Printing);
    end
  else if Border and (Cell.Count > 0) then
    if (Light = clBtnHighLight) and (Dark = clBtnShadow) then
      RaisedRect(Cell.MasterList, Canvas, X+CellSpacing, YO+CellSpacing,
                                  X+Wd-1, YO+Ht-1, False, 1)
    else
      RaisedRectColor(Cell.MasterList, Canvas, X+CellSpacing, YO+CellSpacing,
                                  X+Wd-1, YO+Ht-1, Light, Dark, False, 1);
except
  end;
end;

{----------------TSectionBase.Create}
constructor TSectionBase.Create(AMasterList: TSectionList);
begin
inherited Create;
ParentSectionList := AMasterList;
ContentTop := 999999999;  {large number in case it has Display: none; }
end;

constructor TSectionBase.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
begin
inherited Create;
ParentSectionList := AMasterList;
SectionHeight := T.SectionHeight;
ZIndex := T.ZIndex;
end;

procedure TSectionBase.CopyToClipboard;
begin
end;

function TSectionBase.GetYPosition: integer;
begin
Result := ContentTop;
end;

{----------------TSectionBase.DrawLogic}
function TSectionBase.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
StartCurs := Curs;
Result := SectionHeight;
DrawHeight := SectionHeight;
MaxWidth := 0;
ContentTop := Y;
DrawTop := Y;
YDraw := Y;
ContentBot := Y+SectionHeight;
DrawBot := Y+DrawHeight;
end;

function TSectionBase.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  Y: integer;
begin
Y := YDraw;
Result := Y+SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X: integer; Y: integer;
     var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
     var ATitle: string): guResultType;
begin
Result := [];
end;

function TSectionBase.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
begin
Result := False;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindSourcePos(DocPos: integer): integer;
begin
Result := -1;
end;

function TSectionBase.FindDocPos(SourcePos: integer; Prev: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
begin
Result := False;
end;

function TSectionBase.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
Result := False;
end;

procedure TSectionBase.SetParent(List: TSectionList);
begin
ParentSectionList := List;
end;

procedure TSectionBase.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
Min := 0;  Max := 0;
end;

procedure TSectionBase.AddSectionsToList;
begin
ParentSectionList.PositionList.Add(Self);
end;

{----------------TCellList.Create}
constructor TCellList.Create(Attr: TAttributeList; Prop: TProperties);
var
  I: integer;
  Color: TColor;
begin
inherited Create;
if Assigned(Attr) then
  for I := 0 to Attr.Count-1 do
    with TAttribute(Attr[I]) do
      case Which of
      BGColorSy:
        BkGnd := ColorFromString(Name, False, BkColor);
      BackgroundSy:
        BkImage := Name;
      HeightSy:
        if Pos('%', Name) = 0 then
          SpecRowHeight := Value
        else
          SpecRowHeightPercent := IntMax(0, IntMin(Value, 100));
      end;
if Assigned(Prop) then
  begin
  Color := Prop.GetBackgroundColor;
  if Color <> clNone then
    begin
    BkGnd := True;
    BkColor := Color;
    end;
  Prop.GetBackgroundImage(BkImage);  {'none' will change string to empty}
  if BkImage <> '' then
    Prop.GetBackgroundPos(Prop.EmSize, Prop.ExSize, APRec);
  Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
  end;
end;

{----------------TCellList.CreateCopy}
constructor TCellList.CreateCopy(AMasterList: TSectionList; T: TCellList);
var
  I: integer;
begin
inherited create;
BreakBefore := T.BreakBefore;
BreakAfter := T.BreakAfter;
KeepIntact := T.KeepIntact;
RowType := T.Rowtype;
for I := 0 to T.Count-1 do
  if Assigned(T.Items[I]) then
    Add(TCellObj.CreateCopy(AMasterList, TCellObj(T.Items[I])))
  else Add(Nil);
end;

procedure TCellList.Add(CellObj: TCellObj);
begin
inherited Add(CellObj);
if Assigned(CellObj) then
  begin
  BreakBefore := BreakBefore or CellObj.BreakBefore;
  BreakAfter := BreakAfter or CellObj.BreakAfter;
  KeepIntact := KeepIntact or CellObj.KeepIntact ;
  if SpecRowHeight > 0 then
    CellObj.SpecHt := IntMax(SpecRowHeight, CellObj.SpecHt)
  else if SpecRowHeightPercent > 0 then
    CellObj.SpecHtPercent := IntMax(SpecRowHeightPercent, CellObj.SpecHt);
  end;
end;

{----------------TCellList.InitializeRow}
procedure TCellList.InitializeRow;
var
  I: integer;
begin
if BkGnd then
  for I := 0 to Count-1 do
    with TCellObj(Items[I]).Cell do
      if not BkGnd then
        begin
        BkGnd := True;
        BkColor := Self.BkColor;
        end;
end;

{----------------TCellList.DrawLogic1}
function TCellList.DrawLogic1(Canvas : TCanvas; const Widths : array of integer; Span,
           CellSpacing, AHeight, Rows: integer; var Desired: integer; var Spec, More: boolean): integer;
{Find vertical size of each cell, Row height of this row.  But final Y position
 is not known at this time.
 Rows is number rows in table.
 AHeight is for calculating percentage heights}
var
  I, J, Dummy: integer;
  DummyCurs, GuessHt: integer;
  CellObj: TCellObj;
begin
Result := 0;
Desired := 0;
Spec := False;
DummyCurs := 0;
More := False;
for I := 0 to Count-1 do
  begin
  CellObj := TCellObj(Items[I]);
  if Assigned(CellObj) then
    with CellObj do
      if ColSpan > 0 then  {skip the dummy cells}
        begin
        Wd := 0;
        for J := I to ColSpan+I-1 do
          Inc(Wd, Widths[J]);   {accumulate column widths}
        if Span = RowSpan then
          begin
          Dummy := 0;
          if SpecHt > 0 then
            GuessHt := SpecHt
          else if SpecHtPercent > 0 then
            GuessHt := MulDiv(SpecHtPercent, AHeight, 100)
          else if Rows = 1 then
            GuessHt := AHeight
          else
            GuessHt := 0;
          VSize := Cell.DoLogic(Canvas, 0, Wd-HzSpace-CellSpacing, IntMax(0, GuessHt-VrSpace), 0,
                   Dummy, DummyCurs);
          Result := IntMax(Result, VSize + VrSpace);
          if SpecHt > 0 then
            begin
            Result := IntMax(Result, IntMax(VSize, SpecHt));
            Spec := True;
            end
          else if SpecHtPercent > 0 then
            begin
            Desired := IntMax(Desired, GuessHt);
            Spec := True;
            end;
          end
        else if RowSpan > Span then More := True;
        end;
  end;
Desired := IntMax(Result, Desired);
end;

{----------------TCellList.DrawLogic2}
procedure TCellList.DrawLogic2(Canvas : TCanvas; Y: integer;
          CellSpacing: integer; var Curs: integer);
{Calc Y indents. Set up Y positions of all cells.}
var
  I: integer;
  CellObj: TCellObj;
begin
for I := 0 to Count-1 do
  begin
  CellObj := TCellObj(Items[I]);
  if Assigned(CellObj) then
    CellObj.DrawLogic2(Canvas, Y, CellSpacing, Curs);
  end;
end;

{----------------TCellList.Draw}
function TCellList.Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
     const Widths : array of integer; X: integer; Y, YOffset: integer;
     CellSpacing : integer; Border: boolean; Light, Dark: TColor;
     MyRow: integer) : integer;
var
  I, Spacing: integer;
  YO: integer;
  CellObj: TCellObj;
begin
YO := Y - YOffset;
Result := RowHeight+Y;
Spacing := CellSpacing div 2;

with MasterList do   {check CSS page break properties}
  if Printing then
    if BreakBefore then
      begin
      if YO > ARect.Top then  {page-break-before}
        begin
        if Y+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Y+Spacing;
          end;
        Exit;
        end;
      end
    else if KeepIntact then
      begin
      {Try to fit this RowSpan on a page by itself}
      if (YO > ARect.Top) and (Y+RowSpanHeight > PageBottom) and
            (RowSpanHeight < ARect.Bottom - ARect.Top) then
        begin
        if Y < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Y;
          end;
        Exit;
        end
      else if  (YO > ARect.Top) and (Y+RowHeight > PageBottom) and
            (RowHeight < ARect.Bottom - ARect.Top) then
        begin
        if Y+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Y+Spacing;
          end;
        Exit;
        end;
      end
    else if BreakAfter then
      if ARect.Top + YOff < Result then    {page-break-after}
        if Result+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Result+Spacing;
          end;

with MasterList do  {avoid splitting any small rows}
  if Printing and  (RowSpanHeight <= 100) and
        (Y + RowSpanHeight > PageBottom) then
    begin
    if Y  < PageBottom then
      begin
      PageShortened := True;
      PageBottom := Y;
      end;
    Exit;
    end;

if (YO+RowSpanHeight >= ARect.Top) and (YO < ARect.Bottom) and
       (not MasterList.Printing or (Y < MasterList.PageBottom)) then
  for I := 0 to Count-1 do
    begin
    CellObj := TCellObj(Items[I]);
    if Assigned(CellObj) then
      CellObj.Draw(Canvas, ARect, X, Y, CellSpacing, Border, Light, Dark);
    X := X + Widths[I];
    end;
end;

{----------------ThtmlTable.Create}
constructor ThtmlTable.Create(Master: TSectionList;Attr: TAttributeList;
            Prop: TProperties);
var
  I: integer;
  BdrColor: TColor;
begin
inherited Create(Master);
Rows := TFreeList.Create;
CellPadding := 1;
CellSpacing := 2;
BorderColorLight := clBtnHighLight;
BorderColorDark := clBtnShadow;
for I := 0 to Attr.Count-1 do
  with TAttribute(Attr[I]) do
    case Which of
      BorderSy:
        if Name = '' then
          Border := 1
        else Border := IntMin(100, IntMax(0, Value));  {Border=0 is no border}
      CellSpacingSy:
        if Value >= -1 then CellSpacing := IntMin(Value, 40);
      CellPaddingSy:
        if Value >= 0 then CellPadding := IntMin(Value, 50);
      BorderColorSy:
        if ColorFromString(Name, False, BdrColor) then
          begin
          BorderColorLight := BdrColor;
          BorderColorDark := BdrColor;
          end;
      BorderColorLightSy:
        ColorFromString(Name, False, BorderColorLight);
      BorderColorDarkSy:
        ColorFromString(Name, False, BorderColorDark);
      end;
if Prop.Collapse then
  Cellspacing := -1;
end;

{----------------ThtmlTable.CreateCopy}
constructor ThtmlTable.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  I: integer;
begin
inherited CreateCopy(AMasterList, T);
Rows := TFreeList.Create;
for I := 0 to ThtmlTable(T).Rows.Count-1 do
  Rows.Add(TCellList.CreateCopy(AMasterList, TCellList(ThtmlTable(T).Rows.Items[I])));

Move((T as ThtmlTable).ListsProcessed, ListsProcessed,
        DWord(@EndList)-DWord(@ListsProcessed));

SetLength(Widths, NumCols);
SetLength(MaxWidths, NumCols);
SetLength(MinWidths, NumCols);
SetLength(Percents, NumCols);

if AMasterList.PrintTableBackground then
  begin
  BkGnd := ThtmlTable(T).BkGnd;
  BkColor := ThtmlTable(T).BkColor;
  end
else
  BkGnd := False;
TablePartRec := TTablePartRec.Create;
TablePartRec.TablePart := Normal;
end;

{----------------ThtmlTable.Destroy}
destructor ThtmlTable.Destroy;
begin
Rows.Free;
TablePartRec.Free;
FreeAndNil(ColInfo);
inherited Destroy;
end;

{----------------ThtmlTable.DoColumns}
procedure ThtmlTable.DoColumns(Width: integer; AsPercent: boolean;
              VAlign: AlignmentType; const Align: string);
{add the <col> info to the ColInfo list}
var
  Col: TColObj;
begin
Col := TColObj.Create;
with Col do
  begin
    ColWidth := Width;
    ColAsPercent := AsPercent;
    colVAlign := VAlign;
    colAlign := Align;
  end;
if not Assigned(colInfo) then
  colInfo := TFreeList.Create;
ColInfo.Add(Col);
end;

{----------------ThtmlTable.AddDummyCells}
procedure ThtmlTable.AddDummyCells;
var
  Cl, Rw, K, RowCount: integer;
  AnyAbsolute: boolean;
  Row: TCellList;
  CellObj: TCellObj;
  SpanEq0: boolean;

  function DummyCell(RSpan: integer): TCellObj;
  begin
  Result := TCellObj.Create(ParentSectionList, ATop, Nil, Nil);
  Result.ColSpan := 0;
  Result.RowSpan := RSpan;
  end;

Begin
RowCount := Rows.Count;
if not ListsProcessed then
  begin   {put dummy cells in rows to make up for ColSpan > 1}
  NumCols := 0;
  AnyAbsolute := False;
  for Rw := 0 to RowCount-1 do
    begin
    with TCellList(Rows[Rw]) do
      begin
      InitializeRow;
      for Cl := Count-1 downto 0 do
        with TCellObj(Items[Cl]) do
          begin
          InitializeCell(CellPadding, BkImage, APRec, Self.Border > 0);
          if WidthAttr > 0 then
            begin
            if not AsPercent then AnyAbsolute := True;
            end;
          if Self.BkGnd and not Cell.BkGnd then    {transfer bgcolor to cells if no Table image}
            begin
            Cell.BkGnd := True;
            Cell.BkColor := Self.BkColor;
            end;
          RowSpan := IntMin(RowSpan, RowCount-Rw);  {So can't extend beyond table}
          for K := 1 to ColSpan-1 do
            if RowSpan > 1 then
              TCellList(Rows[Rw]).Insert(Cl+K, DummyCell(RowSpan)) {these could be
                Nil also except they're needed for expansion in the next section}
            else
              TCellList(Rows[Rw]).Insert(Cl+K, DummyCell(1));
          end;
      end;
    NumCols := IntMax(NumCols, TCellList(Rows[Rw]).Count);  {temporary # cols}
    end;

  {Absolute calc only if  some absolute widths entered}
  UseAbsolute := AnyAbsolute;

  {put dummy cells in cols to make up for RowSpan > 1}
  for Cl := 0 to NumCols-1 do
    for Rw := 0 to RowCount-1 do
      with TCellList(Rows[Rw]) do
        if Count > Cl then
          if Assigned(Items[Cl]) then
            with TCellObj(Items[Cl]) do
              begin
              RowSpan := IntMin(RowSpan, RowCount-Rw);  {practical limit}
              if RowSpan > 1 then
                for K := Rw+1 to Rw+RowSpan-1 do
                  begin  {insert dummy cells in following rows if RowSpan > 1}
                  while TCellList(Rows[K]).Count < Cl do {add padding if row is short}
                     TCellList(Rows[K]).Add(DummyCell(0));
                  TCellList(Rows[K]).Insert(Cl, DummyCell(0));
                  end;
              end;

  {look for excessive Colspans on last cells in each row.  These would be dummy
   cells added above with Colspan = 0}
  if (RowCount > 0) and (NumCols > 0) then
    repeat
      SpanEq0 := True;  {assume there are some}
      for Rw := 0 to RowCount-1 do
        with TCellList(Rows[Rw]) do
          if (Count = NumCols) and (TCellObj(Items[NumCols-1]).ColSpan <> 0) then
            SpanEq0 := False;  {at least one last cell is not a dummy}
      if SpanEq0 then
        begin  {trim off the dummy cells on end and fixup the Colspan value which was to blame}
        for Rw := 0 to RowCount-1 do
          with TCellList(Rows[Rw]) do
            if (Count = NumCols) and (TCellObj(Items[NumCols-1]).ColSpan = 0) then
              begin
              TCellObj(Items[NumCols-1]).Free;
              Delete(NumCols-1);  {trim cell on end}
              K := NumCols-2;
              while K >= 0 do  {find the Colspan value}
                begin
                if TCellObj(Items[K]).ColSpan > 1 then
                  begin
                  Dec(TCellObj(Items[K]).ColSpan); {fix it}
                  Break;
                  end;
                Dec(K);
                end;
              end;
        Dec(NumCols);
        end;
    until not SpanEq0;

  NumCols := 0;  {find the number of columns}
  for Rw := 0 to RowCount-1 do
    begin
    NumCols := IntMax(NumCols, TCellList(Rows[Rw]).Count);
    end;

  {add the width info from the <col> tags to the cells}
  if Assigned(colInfo) then
    begin
    AnyAbsolute := False;
    for Rw := 0 to RowCount-1 do
      begin
      Row := TCellList(Rows[Rw]);
      for Cl := 0 to IntMin(Row.Count-1, NumCols-1) do
        begin
        CellObj := TCellObj(Row[Cl]);
        with CellObj do
          begin
          if Cl < colInfo.Count then
            with TColObj(colInfo[Cl]) do
              begin
              if colWidth > 0 then
                begin
                WidthAttr := colWidth;
                AsPercent := colAsPercent;
                end;
              end;
          if not AsPercent then
            AnyAbsolute := True;
          end;
        end;
      end;
    UseAbsolute := AnyAbsolute;
    FreeAndNil(colInfo);    {no longer needed}
    end;

  SetLength(Widths, NumCols);
  SetLength(MaxWidths, NumCols);
  SetLength(MinWidths, NumCols);
  SetLength(Percents, NumCols);

  ListsProcessed := True;
  end;    {if not ListsProcessed}
end;

{----------------ThtmlTable.GetMinMaxAbs}
procedure ThtmlTable.GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
                TotalMaxWidth: integer);
var
  I, J, Min, Max, N, Span, Addon, D: integer;
  More: boolean;
  CellObj: TCellObj;

Label Two;

Begin
for I := 0 to NumCols-1 do
  begin
  MaxWidths[I] := 0;
  MinWidths[I] := 0;
  end;
SetLength(Heights, 0);
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          begin
          CellObj := TCellObj(Items[I]);
          with CellObj do
            begin
            More := More or (CellObj.ColSpan > Span); {set if need another iteration}
            if ColSpan = Span then
              begin
              Cell.MinMaxWidth(Canvas, Min, Max);
              Addon := CellSpacing + CellObj.HzSpace;
              Inc(Min, Addon);
              Inc(Max, Addon);
              if Span = 1 then
                begin
                if not AsPercent and (CellObj.WidthAttr > 0) then
                  begin
                  Max := IntMax(Min, WidthAttr+Addon);
                  Min := Max;
                  end;
                MinWidths[I] := Intmax(MinWidths[I], Min);
                MaxWidths[I] := Intmax(MaxWidths[I], Max);
                end
              else
                begin
                TotalMinWidth := 0;  TotalMaxWidth := 0;
                for N := I to I+ColSpan-1 do
                  begin   {find the current totals for the span}
                  Inc(TotalMaxWidth, MaxWidths[N]);
                  Inc(TotalMinWidth, MinWidths[N]);
                  end;
                if not AsPercent and (WidthAttr > 0) then
                  begin
                  Min := IntMax(Min, WidthAttr{+Cellspacing});
                  Max := IntMax(Min, WidthAttr{+Cellspacing});
                  end;
                if (TotalMinWidth < Min) then
                  if TotalMinWidth > 0 then
                    begin
                    D := Min - TotalMinWidth;
                    for N := I to I+ColSpan-1 do  {increase the sub widths to match the span}
                      MinWidths[N] := MinWidths[N]+MulDiv(MinWidths[N], D, TotalMinWidth);
                    end
                  else MinWidths[I] := Min;  {this for multiple empty cols}
                if (TotalMaxWidth < Max) then
                  if TotalMaxWidth > 0 then
                    begin     {increase the sub widths to match the span}
                    D := Max - TotalMaxWidth;
                    for N := I to I+ColSpan-1 do  {increase the sub widths to match the span}
                      MaxWidths[N] := MaxWidths[N]+MulDiv(MaxWidths[N], D, TotalMaxWidth);
                    end
                  else MaxWidths[I] := Max;
                end;
              end;
            end;
          end;
      end;
  Inc(Span);
  end;

{Find the total min and max width}
Two:
TotalMaxWidth := 0;  TotalMinWidth := 0;
for I := 0 to NumCols-1 do
  begin
  Inc(TotalMaxWidth, MaxWidths[I]);
  Inc(TotalMinWidth, MinWidths[I]);
  end;

end;

{----------------ThtmlTable.GetWidthsAbs}
procedure ThtmlTable.GetWidthsAbs(Canvas: TCanvas; TablWidth: integer;
            Specified: boolean);
var
  N, D, W, dd, TotalMinWidth, TotalMaxWidth: integer;
  Accum: integer;
Begin
GetMinMaxAbs(Canvas, TotalMinWidth, TotalMaxWidth);

if TotalMinWidth > TablWidth then  {use the minimum column widths, table will expand}
  Widths := Copy(MinWidths)
else if (TotalMaxWidth <= TablWidth) and not Specified then
  {use the max column widths, table will be smaller}
  Widths := Copy(MaxWidths)
else  {make table fit}
  begin
  D := TotalMaxWidth - TotalMinWidth;
  W := TablWidth - TotalMinWidth;
  if D > 0 then  {expand only those columns with some slop in them}
    begin
    Accum := 0;
    for  N := 0 to NumCols-1 do
      begin
      dd := MaxWidths[N] - MinWidths[N];  {some dd's may be 0}
      Widths[N] := MinWidths[N] + MulDiv(dd, W, D);
      Inc(Accum, Widths[N]);
      end;
    dd := Accum-TablWidth;   {check for Roundoff error}
    if dd <> 0 then
      begin
      for N := 0 to NumCols-1 do
        begin
        if dd > 0 then
          begin
          if MaxWidths[N] > MinWidths[N] then
            begin
            Dec(Widths[N]);
            Dec(dd);
            end;
          end
        else
          begin
          Inc(Widths[N]);
          Inc(dd);
          end;
        if dd = 0 then
          break;
        end;
      end;
    end
  else  {no adjustable columns, will have to expand them all}
    for N := 0 to NumCols-1 do
      Widths[N] := MinWidths[N] + MulDiv(MinWidths[N], W, TotalMinWidth);
  end;
end;

{----------------ThtmlTable.GetWidths}
procedure ThtmlTable.GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: integer;
           TheWidth: integer);
var
  I, J, Min, Max, N, Span, Addon, Distributable, TotalPC, Accum,
  ExcessMin, ExcessMax, NonPC, PCWidth, NewTotalPC: integer;
  More: boolean;

begin
{Find the max and min widths of each column}
for I := 0 to NumCols-1 do
  begin
  MaxWidths[I] := 0;
  MinWidths[I] := 0;
  Percents[I] := 0;
  end;
SetLength(Heights, 0);
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            PCWidth := 0;
            if WidthAttr > 0 then
              if AsPercent then PCWidth := WidthAttr
              else if TheWidth > 0 then
                PCWidth := IntMin(1000, MulDiv(WidthAttr, 1000, TheWidth));
            More := More or (ColSpan > Span); {set if need another iteration}
            if ColSpan = Span then
              begin
              Cell.MinMaxWidth(Canvas, Min, Max);
              Addon := CellSpacing + HzSpace;
              Inc(Min, Addon);
              Inc(Max, Addon);
              if Span = 1 then
                begin
                MaxWidths[I] := IntMax(MaxWidths[I],  Max);
                MinWidths[I] := IntMax(MinWidths[I],  Min);
                Percents[I] := Intmax(Percents[I], PCWidth);  {collect percents}
                end
              else
                begin
                TotalMaxWidth := 0;  TotalMinWidth := 0;
                TotalPC := 0;  NonPC := 0;
                for N := I to I+ColSpan-1 do
                  begin   {Total up the pertinant column widths}
                  Inc(TotalMaxWidth, MaxWidths[N]);
                  Inc(TotalMinWidth, MinWidths[N]);
                  if Percents[N] > 0 then
                    Inc(TotalPC, Percents[N])  {total percents}
                  else Inc(NonPC);      {count of cell with no percent}
                  end;
                if Colspan = NumCols then
                  begin
                  TotalMinWidth := Intmax(TotalMinWidth, TheWidth);
                  TotalMaxWidth := Intmax(TotalMaxWidth, TheWidth);
                  end;
                ExcessMin := Min - TotalMinWidth;
                ExcessMax := Max - TotalMaxWidth;
                if (PCWidth > 0) or (TotalPC > 0) then
                  begin   {manipulate for percentages}
                  if NonPC > 0 then
                    {find the extra percentages to divvy up}
                    Distributable := IntMax(0, (PCWidth-TotalPC) div NonPC)
                  else Distributable := 0;
                  if (NonPC = 0) and (PCWidth > TotalPC+1) then
                    begin
                    for  N := I to I+ColSpan-1 do  {stretch percentages to fit}
                      Percents[N] := MulDiv(Percents[N], PCWidth, TotalPC);
                    end
                  else if Distributable > 0 then    {spread colspan percentage excess over the unspecified cols}
                    for N := I to I+ColSpan-1 do
                      if Percents[N] = 0 then
                        Percents[N] := Distributable;
                  NewTotalPC := IntMax(TotalPC, PCWidth);
                  if ExcessMin > 0 then
                    begin
                    if (NonPC > 0) and (TotalMaxWidth > 0) then  {split excess over all cells}
                      begin
                      {proportion the distribution so cells with large MaxWidth get more}
                      for  N := I to I+ColSpan-1 do
                        Inc(MinWidths[N], MulDiv(ExcessMin, MaxWidths[N], TotalMaxWidth));
                      end
                    else
                      for  N := I to I+ColSpan-1 do
                        Inc(MinWidths[N], (MulDiv(ExcessMin, Percents[N], NewTotalPC)));
                    end;
                  if ExcessMax > 0 then
                    for  N := I to I+ColSpan-1 do
                      Inc(MaxWidths[N], (MulDiv(ExcessMax, Percents[N], NewTotalPC)));
                  end
                else
                  begin  {no width dimensions entered}
                  if ExcessMin > 0 then
                    begin
                    Accum := 0;
                    for  N := I to I+ColSpan-1 do
                      begin
                      if TotalMinWidth = 0 then
                        MinWidths[N] := Min div ColSpan
                      else  {split up the widths in proportion to widths already there}
                        MinWidths[N] := MulDiv(Min, MinWidths[N], TotalMinWidth);
                      Inc(Accum, MinWidths[N]);
                      end;
                    if Accum < Min then  {might be a roundoff pixel or two left over}
                      Inc(MinWidths[I], Min-Accum);
                    end;
                  if ExcessMax > 0 then
                    begin
                    Accum := 0;
                    for  N := I to I+ColSpan-1 do
                      begin
                      if TotalMaxWidth = 0 then
                        MaxWidths[N] := Max div ColSpan
                      else   {split up the widths in proportion to widths already there}
                        MaxWidths[N] := MulDiv(Max, MaxWidths[N], TotalMaxWidth);
                      Inc(Accum, MaxWidths[N]);
                      end;
                    if Accum < Max then  {might be a roundoff pixel or two left over}
                      Inc(MaxWidths[I], Max-Accum);
                    end;
                  end;
                end;
              end;
            end;
      end;
  Inc(Span);
  end;

TotalMaxWidth := 0;  TotalMinWidth := 0;
for I := 0 to NumCols-1 do
  begin
  Inc(TotalMaxWidth, MaxWidths[I]);
  Inc(TotalMinWidth, MinWidths[I]);
  end;
end;

{----------------ThtmlTable.MinMaxWidth}
procedure ThtmlTable.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
AddDummyCells;     {in case it hasn't been done}
if UseAbsolute and (tblWidthAttr = 0) then
  GetMinMaxAbs(Canvas, Min, Max)
else
  GetWidths(Canvas, Min, Max, tblWidthAttr);

Inc(Min, CellSpacing);
Inc(Max, CellSpacing);
Min := IntMax(Min, tblWidthAttr);
Max := IntMax(Max, tblWidthAttr);
end;

procedure ThtmlTable.TableSpecifiedAndWillFit(TheWidth: integer);
{Divide up the table into columns.  TheWidth is the specified width of the table.
 At this point, it is known that everything will fit into TheWidth. Percents are
 being used}
var
  I, W, PCNotMinWid, TotalWid, Unsp, UnspDiff, Delta, Addon, Count, d: integer;
  UseMin: array of boolean;
  NoChange: boolean;
  UnspCol: Integer;
begin
if NumCols = 0 then
  Exit;
SetLength(UseMin, NumCols);
for I := 0 to NumCols-1 do
  UseMin[I] := False;
PCNotMinWid := 0;  TotalWid := 0;  Unsp := 0; UnspDiff := 0; UnspCol := -1;
{First calculate everything assuming the data entered is perfectly correct}
for I := 0 to NumCols - 1 do
  begin
  if Percents[I] > 0 then
    begin
    W := MulDiv(TheWidth, Percents[I], 1000);  {width based on percentage}
    if W > MinWidths[I] then
      begin
      Widths[I] := W;
      Inc(PCNotMinWid, Percents[I]);
      end
    else
      begin   {percent is too small, use Min width}
      Widths[I] := MinWidths[I];
      UseMin[I] := True;
      end;
    end
  else
    begin    {no percent}
    Widths[I] := MinWidths[I];
    Inc(Unsp);     {an unspecified column}
    UnspCol := I;  {save location of unspedified column}
    Inc(UnspDiff, IntMax(0, MaxWidths[I]-MinWidths[I])); {total max-min for unspecified cols}
    end;
  Inc(TotalWid, Widths[I]);
  end;

Delta := TotalWid - TheWidth;    {see what the error is}
if Delta < 0 then     {table is too small}
  begin
  if Unsp > 0 then
    begin
    if (UnspDiff > 0) and (UnspDiff >= Abs(Delta) div 4) then
      {increase the unspecified columns widths prop to Max, Min unless the difference is trivial}
      begin
      for I := 0 to NumCols-1 do
        if (Percents[I] = 0) then
          Inc(Widths[I], MulDiv(-Delta, IntMax(0, MaxWidths[I] - MinWidths[I]), UnspDiff));
      end
    else
      begin  {increase the unspecified columns widths uniformly}
      Addon := -Delta div Unsp;
      for I := 0 to NumCols - 1 do
        if (Percents[I] = 0) then
          Inc(Widths[I], Addon);
      end;
    end
  else
    begin            {no unspecified widths, increase the specified columns which are not minimum}
    for I := 0 to NumCols - 1 do
      if (Percents[I] > 0) and not UseMin[I] then
        Inc(Widths[I], MulDiv(-Delta, Percents[I], PCNotMinWid));
    end;
  end
else if Delta > 0 then    {calculated table is too large}
  begin
  Count := 0;
  {make one or more trial run to see what happens when shrinking the columns
   that can be shrunck.  May hit another MinWidth situation}
  repeat
    NoChange := True;
    for I := 0 to NumCols - 1 do
      if (Percents[I] > 0) and not UseMin[I] then
        begin
        W := Widths[I] - MulDiv(Delta, Percents[I], PCNotMinWid);
        if W < MinWidths[I] then
          begin    {new width is smaller than MinWidth, make adustments}
          UseMin[I] := True;
          NoChange := False;
          Dec(PCNotMinWid, Percents[I]);
          Dec(Delta, Widths[I]-MinWidths[I]);
          Widths[I] := MinWidths[I];
          end;
        end;
    Inc(Count);
  until NoChange or (Count >= 4);   {count guards against endless loop}
  for I := 0 to NumCols - 1 do  {now actually change the widths}
    if (Percents[I] > 0) and not UseMin[I] then
      Dec(Widths[I], MulDiv(Delta, Percents[I], PCNotMinWid));
  end;

TotalWid := 0;     {fix up any round off errors}
for I := 0 to NumCols - 1 do
  Inc(TotalWid, Widths[I]);
Delta := TotalWid-TheWidth;     {round off error}
if Delta > 0 then
  begin
  for I := 0 to NumCols-1 do
    if not UseMin[I] then
      begin
      Dec(Widths[I], Delta);   {remove extra from first non minimum}
      Break;
      end;
  end
else if Length(Widths) > 0 then
  if UnspCol >= 0 then
    Inc(Widths[UnspCol], -Delta)   {put it into an unspecified column}
  else
    begin
    Delta := -Delta;   {Delta is now positive}
    While Delta > NumCols do
      begin
      d := Delta div NumCols;
      if d > 0 then
        for I := 0 to NumCols - 1 do
          begin
          Dec(Delta, d);
          Inc(Widths[I], d);
          end
      end;
    {remainder should be less than NumCols here, so tack on 1 each column
     until it's gone}
    for I := 0 to NumCols-1 do
      if Delta > 0 then
        begin
        Inc(Widths[I]);
        Dec(Delta);
        end
      else Break;
    end;
end;

{----------------ThtmlTable.TableNotSpecifiedAndWillFit}
procedure ThtmlTable.TableNotSpecifiedAndWillFit(TotalMinWidth, TotalMaxWidth, TheWidth: integer);
{Find column widths.  Table known to fit within allowed space and its width hasn't
 been specified}
var
  D, W, DS, MaxDS, MaxI, I, Addon,
  Total, TotalPC, Residual, NewResidual, W1, W2, NewTotal, LastNewTotal: integer;
  HasPercents, UsesPercents, Done: boolean;
begin
if NumCols = 0 then
  Exit;
TotalPC := 0;   {see if any percentage widths entered}
for I := 0 to NumCols-1 do
  Inc(TotalPC, Percents[I]);
UsesPercents := (TotalPc > 0) and (TotalPc <= 1000) {ignore ridiculous values}
                 or (tblWidthAttr > 0);

if UsesPercents then
  begin {find the largest width that will accomodate the %'s}
  Residual := 0; W1 := 0;
  for I := 0 to NumCols-1 do
    if Percents[I] > 0 then  {a percent has been entered}
      W1 := IntMax(W1, MulDiv(MaxWidths[I], 1000, Percents[I])) {look for maximum}
    else
      Inc(Residual, MaxWidths[I]);  {accumlate the cols which have no percent}
  if TotalPC < 1000 then
    W2 := MulDiv(Residual, 1000, 1000-TotalPC)
  else if Residual > 0 then W2 := 30000
  else W2 := 0;
  Total := IntMax(W1, W2);
  if Total <= TheWidth then
    begin  {a fit is found using percents and maxwidths}
    if tblWidthAttr > 0 then
      Total := TheWidth;    {don't try to make it smaller than TheWidth}
    NewResidual := MulDiv(Total, 1000-TotalPC, 1000);
    for I := 0 to NumCols-1 do
      if Percents[I] > 0 then   {figure widths to fit this situation}
        Widths[I] := MulDiv(Total, Percents[I], 1000)
      else if Residual > 0 then
        Widths[I] := MulDiv(MaxWidths[I], NewResidual, Residual)
      else Widths[I] := 0;    {this is an table syntax error condition}
    Exit;
    end;

  Done := False;
  LastNewTotal := $FFFFFFF;
  repeat  {with the above possibilites taken care of, we can assume the final
           width will = NewWidth}
    HasPercents := False;
    Total := 0;  Residual := 0;
    for I := 0 to NumCols-1 do
      begin
      if Percents[I] > 0 then
        begin
        W := MulDiv(TheWidth, Percents[I], 1000); {a Percent's width based on TheWidth}
        if W < MinWidths[I] then  {but it must be > MinWidth}
          begin   {eliminate the percentage value as not achievable}
          Percents[I] := 0;
          Inc(Residual, MinWidths[I]);  {and put it in the residuals}
          end
        else
          begin
          HasPercents := True;   {still valid percents}
          Inc(Total, W);
          end;
        end
      else Inc(Residual, MinWidths[I]);
      end;
    if not HasPercents then Break;  {no percents are achievable}
    if Total+Residual <= TheWidth then
      begin  {a solution with at least some percentages can be found}
      Done := True;
      TotalMaxWidth := 0;  TotalMinWidth := 0;  {recalc these}
      for I := 0 to NumCols-1 do
        begin
        if Percents[I] > 0 then
          begin
          MinWidths[I] := MulDiv(TheWidth, Percents[I], 1000);
          MaxWidths[I] := MinWidths[I];  {this fixes the width thru later calculations}
          end;
        Inc(TotalMaxWidth, MaxWidths[I]);
        Inc(TotalMinWidth, MinWidths[I]);
        end;
      end
    else  {it doesn't fit screen, reduce percentages and try again}
      begin
      NewTotal := TheWidth-Residual;  {percent items must fit this}
      while LastNewTotal <= NewTotal do
        Dec(NewTotal);
      LastNewTotal := NewTotal;
      for I := 0 to NumCols-1 do
        if Percents[I] > 0 then
          Percents[I] := MulDiv(Percents[I], NewTotal, Total);
      end;
  until Done;
  end;

D := TotalMaxWidth - TotalMinWidth;

MaxI := 0;
Total := 0;
W := TheWidth - TotalMinWidth;
if (D = 0) and (NumCols > 0) then
  begin
  Addon := W div NumCols;
  for I := 0 to NumCols-1 do
    begin
    Widths[I] := MinWidths[I] + Addon;
    Inc(Total, Widths[I]);
    end;
  end
else
  begin
  MaxDS := 0;
  for I := 0 to NumCols-1 do
    begin
    ds := MaxWidths[I] - MinWidths[I];
    Widths[I] := MinWidths[I] + MulDiv(ds, W, D);
    Inc(Total, Widths[I]);
    if ds > MaxDS then
      begin
      MaxDS := ds;
      MaxI := I;
      end
    end;
  end;
if Total <> TheWidth then   {a round off error}
  Inc(Widths[MaxI], TheWidth-Total);  {adjust column with largest variation}
end;

{----------------ThtmlTable.FindRowHeights}
procedure ThtmlTable.FindRowHeights(Canvas: TCanvas; AHeight: integer);
var
  I, J, K, H, Span, TotalMinHt, TotalDesHt, AddOn,
  Sum, AddedOn, Desired, UnSpec: integer;
  More, Mr, IsSpeced: boolean;
  MinHts, DesiredHts: IntArray;
  SpecHts: array of boolean;
  F: double;
begin
if Rows.Count = 0 then
  Exit;
Dec(AHeight, CellSpacing);  {calculated heights will include one cellspacing each,
  this removes that last odd cellspacing}
if Length(Heights) = 0 then
  SetLength(Heights, Rows.Count);
SetLength(DesiredHts, Rows.Count);
SetLength(MinHts, Rows.Count);
SetLength(SpecHts, Rows.Count);
for I := 0 to Rows.Count-1 do
  begin
  Heights[I] := 0;
  DesiredHts[I] := 0;
  MinHts[I] := 0;
  SpecHts[I] := False;
  end;
{Find the height of each row allowing for RowSpans}
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      if J+Span > Rows.Count then Break;  {otherwise will overlap}
      H := DrawLogic1(Canvas, Widths, Span, CellSpacing, IntMax(0,AHeight-Rows.Count*CellSpacing),
                       Rows.Count, Desired, IsSpeced, Mr) + CellSpacing;
      Inc(Desired, Cellspacing);
      More := More or Mr;
      if Span = 1 then
        begin
        MinHts[J] := H;
        DesiredHts[J] := Desired;
        SpecHts[J] := SpecHts[J] or IsSpeced;
        end
      else if H > Cellspacing then   {if H=Cellspacing then no rowspan for this span}
        begin
        TotalMinHt := 0;  {sum up the heights so far for the rows involved}
        TotalDesHt := 0;
        for K := J to J+Span-1 do
          begin
          Inc(TotalMinHt, MinHts[K]);
          Inc(TotalDesHt, DesiredHts[K]);
          SpecHts[K] := SpecHts[K] or IsSpeced;
          end;
        if H > TotalMinHt then   {apportion the excess over the rows}
          begin
          Addon := ((H-TotalMinHt) div Span);
          AddedOn := 0;
          for K := J to J+Span-1 do
            begin
            Inc(MinHts[K], Addon);
            Inc(AddedOn, Addon);
            end;
          Inc(MinHts[J+Span-1], (H-TotalMinHt)-AddedOn); {make up for round off error}
          end;
        if Desired > TotalDesHt then   {apportion the excess over the rows}
          begin
          Addon := ((Desired-TotalDesHt) div Span);
          AddedOn := 0;
          for K := J to J+Span-1 do
            begin
            Inc(DesiredHts[K], Addon);
            Inc(AddedOn, Addon);
            end;
          Inc(DesiredHts[J+Span-1], (Desired-TotalDesHt)-AddedOn); {make up for round off error}
          end;
        end;
      end;
  Inc(Span);
  end;

TotalMinHt := 0;
TotalDesHt := 0;
UnSpec := 0;
for I := 0 to Rows.Count-1 do
  begin
  Inc(TotalMinHt, MinHts[I]);
  Inc(TotalDesHt, DesiredHts[I]);
  if not SpecHts[I] then
    Inc(UnSpec);
  end;

if TotalMinHt >= AHeight then
  Heights := Copy(MinHts)
else if TotalDesHt < AHeight then
  if UnSpec > 0 then
    begin  {expand the unspeced rows to fit}
    Heights := Copy(DesiredHts);
    Addon := (AHeight-TotalDesHt) div UnSpec;
    Sum := 0;
    for I := 0 to Rows.Count-1 do
      if not SpecHts[I] then
        begin
        Dec(UnSpec);
        if UnSpec > 0 then
          begin
          Inc(Heights[I], AddOn);
          Inc(Sum, Addon);
          end
        else
          begin   {last item, complete everything}
          Inc(Heights[I], AHeight-TotalDesHt-Sum);
          Break;
          end;
        end;
    end
  else
    begin {expand desired hts to fit}
    Sum := 0;
    F := AHeight/TotalDesHt;
    for I := 0 to Rows.Count-2 do
      begin
      Heights[I] := Round(F * DesiredHts[I]);
      Inc(Sum, Heights[I]);
      end;
    Heights[Rows.Count-1] := AHeight - Sum;  {last row is the difference}
    end
else
  begin
  Sum := 0;
  F := (AHeight-TotalMinHt)/(TotalDesHt-TotalMinHt);
  for I := 0 to Rows.Count-2 do
    begin
    Heights[I] := MinHts[I] + Round(F*(DesiredHts[I]-MinHts[I]));
    Inc(Sum, Heights[I]);
    end;
  Heights[Rows.Count-1] := AHeight - Sum;
  end;
end;

{----------------ThtmlTable.DrawLogic}
function ThtmlTable.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
var
  I, J, K,
  TotalMaxWidth, TotalMinWidth: integer;
  NewWidth: integer;
  OwnerWidth: integer;
  Specified: boolean;
  TopY: integer;
  FirstLinePtr: PInteger;
  CellObj: TCellObj;
  HasBody: Boolean;

Begin
Inc(ParentSectionList.TableNestLevel);
Inc(NLevel);
try
  YDraw := Y;
  TopY := Y;
  ContentTop := Y;
  DrawTop := Y;
  StartCurs := Curs;
  If Assigned(ParentSectionList.FirstLineHtPtr) and        {used for List items}
      (ParentSectionList.FirstLineHtPtr^ = 0) then
        FirstLinePtr := ParentSectionList.FirstLineHtPtr  {save for later}
  else FirstLinePtr := Nil;

  OwnerWidth := IMgr.RightSide(Y) - IMgr.LeftIndent(Y);
  if tblWidthAttr > 0 then
    begin
    Specified := True;
    NewWidth := tblWidthAttr;
    end
  else
    begin
    Specified := False;
    NewWidth := OwnerWidth;
    end;
  Dec(NewWidth, (CellSpacing));

  AddDummyCells;

  {Figure the width of each column}
  if UseAbsolute and not Specified then
    begin  {Table width not specified and at least one column has absolute width specified}
    GetWidthsAbs(Canvas, NewWidth, Specified); {fills in the Widths array}
    end
  else
    begin
    GetWidths(Canvas, TotalMinWidth, TotalMaxWidth, NewWidth);

    if (TotalMinWidth >= NewWidth) then
      begin   {table won't fit, use minimun widths}
      if Assigned(MinWidths) then   {Delphi 4 needs this check}
        Widths := Copy(MinWidths);
      end
    else if Specified then
      begin
      TableSpecifiedAndWillFit(NewWidth);
      end
    else
      TableNotSpecifiedAndWillFit(TotalMinWidth, TotalMaxWidth, NewWidth);
    end;

  {Find Table Width}
  TableWidth := CellSpacing;
  for I := 0 to NumCols-1 do
    Inc(TableWidth, Widths[I]);

  if (Length(Heights) = 0)  then
    FindRowHeights(Canvas, AHeight)
  else if ParentSectionList.InLogic2 and (ParentSectionList.TableNestLevel <= 5) then
    FindRowHeights(Canvas, AHeight);

  SectionHeight := 0;
  HeaderHeight := 0;
  HeaderRowCount := 0;
  FootHeight := 0;
  FootStartRow := -1;
  HasBody := False;
  for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      begin
      RowHeight := Heights[J];
      if RowType = THead then
        begin
        Inc(HeaderRowCount);
        Inc(HeaderHeight, RowHeight);
        end
      else if RowType = TFoot then
        begin
        if FootStartRow = -1 then
          begin
          FootStartRow := J;
          FootOffset := SectionHeight;
          end;
        Inc (FootHeight, RowHeight);
        end
      else if Rowtype = TBody then
        HasBody := True;
      RowSpanHeight := 0;
      Inc(SectionHeight, Heights[J]);
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          begin
          CellObj := TCellObj(Items[I]);
          with CellObj do
            begin   {find the actual height, Ht, of each cell}
            Ht := 0;
            for K := J to IntMin(J+RowSpan-1, Rows.Count-1) do
              Inc(Ht, Heights[K]);
            if RowSpanHeight < Ht then RowSpanHeight := Ht;
            end;
          end;
      {DrawLogic2 is only called in nested tables if the outer table is calling DrawLogic2}
      if ParentSectionList.TableNestLevel = 1 then
        ParentSectionList.InLogic2 := True;
      try
        if ParentSectionList.InLogic2 then
          DrawLogic2(Canvas, Y, CellSpacing, Curs);
      finally
        if ParentSectionList.TableNestLevel = 1 then
          ParentSectionList.InLogic2 := False;
        end;
      Inc(Y, RowHeight);
      end;
  HeadOrFoot := ((HeaderHeight > 0) or (FootHeight > 0)) and HasBody;
  Inc(SectionHeight, CellSpacing);
  TableHeight := SectionHeight;

  Len := Curs-StartCurs;
  MaxWidth := TableWidth;
  Result := SectionHeight;
  DrawHeight := Result;
  ContentBot := TopY+SectionHeight;
  DrawBot := TopY+DrawHeight;
  try
    If Assigned(FirstLinePtr) then
      FirstLinePtr^ := YDraw+SectionHeight;
  except
    end;
finally
  Dec(ParentSectionList.TableNestLevel);
  Dec(NLevel);
  end;
end;

{----------------ThtmlTable.Draw}
function ThtmlTable.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  YO, YOffset, Y: integer;
begin
Inc(ParentSectionList.TableNestLevel);
Y := YDraw;
Result := Y+SectionHeight;
if Float then
  Y := Y + VSpace;
YOffset := ParentSectionList.YOff;
YO := Y - YOffset;

if (YO+DrawHeight >= ARect.Top) and (YO < ARect.Bottom)
       or ParentSectionList.Printing then
    if ParentSectionList.Printing and (ParentSectionList.TableNestLevel = 1)
       and HeadOrFoot
       and (Y < ParentSectionList.PageBottom)
       and ((ParentSectionList.PrintingTable = Nil) or
           (ParentSectionList.PrintingTable = Self)) then
      DrawTableP(Canvas, ARect, IMgr, X, Y)
    else
  DrawTable(Canvas, ARect, IMgr, X, Y);
Dec(ParentSectionList.TableNestLevel);
end;

procedure ThtmlTable.DrawTable(Canvas: TCanvas; const ARect: TRect; IMgr: IndentManager; X: integer; Y: integer);
var
  I, XX: integer;
  YY, YOffset: integer;
begin
YOffset := ParentSectionList.YOff;
XX := X+Indent;   {for the table}
YY := Y;
DrawX := XX;
DrawY := YY;
for I := 0 to Rows.Count-1 do
  YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
          XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
          BorderColorDark, I);
end;

procedure ThtmlTable.DrawTableP(Canvas: TCanvas; const ARect: TRect; IMgr: IndentManager; X: integer; Y: integer);
{Printing table with thead and/or tfoot}
var
  I, XX, TopBorder, BottomBorder: integer;
  YY, YOffset: integer;
  SavePageBottom: Integer;
  Spacing, HeightNeeded: Integer;
begin
YOffset := ParentSectionList.YOff;
XX := X+Indent;   {for the table}
YY := Y;
DrawX := XX;
DrawY := YY;

if TTableBlock(MyBlock).TableBorder then
  begin
  TopBorder := Border;
  BottomBorder := Border;
  end
else
  begin
  TopBorder := MyBlock.MargArray[BorderTopWidth];
  BottomBorder := MyBlock.MargArray[BorderBottomWidth];
  end;

case TablePartRec.TablePart of
  {.$Region 'Normal'}
  Normal:
    begin
    ParentSectionList.PrintingTable := Self;
    if ParentSectionList.PageBottom-Y >= TableHeight+BottomBorder then
      begin
      for I := 0 to Rows.Count-1 do   {do whole table now}
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
      ParentSectionList.PrintingTable := Nil;
      end
    else
      begin {see if enough room on this page for header, 1 row, footer}
      if HeadOrFoot then
        begin
        Spacing := CellSpacing div 2;
        HeightNeeded := HeaderHeight+FootHeight+
                TCellList(Rows.Items[HeaderRowCount]).RowHeight;
        if (Y - YOffset > ARect.Top) and (Y+HeightNeeded > ParentSectionList.PageBottom) and
              (HeightNeeded < ARect.Bottom - ARect.Top) then
          begin {not enough room, start table on next page}
          if Y+Spacing < ParentSectionList.PageBottom then
            begin
            ParentSectionList.PageShortened := True;
            ParentSectionList.PageBottom := Y+Spacing;
            end;
          exit;
          end;
        end;
      {start table. it will not be complete and will go to next page}
      SavePageBottom := ParentSectionList.PageBottom;
      ParentSectionList.PageBottom := SavePageBottom - FootHeight-Cellspacing-BottomBorder-5;  {a little to spare}
      for I := 0 to Rows.Count-1 do   {do part of table}
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
      BodyBreak := ParentSectionList.PageBottom;
      if FootStartRow >= 0 then
        begin
        TablePartRec.TablePart := DoFoot;
        TablePartRec.PartStart := Y + FootOffset;
        TablePartRec.PartHeight := FootHeight + IntMax(2*Cellspacing, Cellspacing+1) + BottomBorder;
        ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
        end
      else if HeaderHeight > 0 then
        begin   {will do header next}
        //ParentSectionList.PageBottom := SavePageBottom;
        TablePartRec.TablePart := DoHead;
        TablePartRec.PartStart := DrawY-TopBorder;
        TablePartRec.PartHeight := HeaderHeight+TopBorder;
        ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
        end;
      ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
      end;
    end;
  {.$EndRegion}
  {.$Region 'DoBody1'}
  DoBody1:
    begin
    if ParentSectionList.PageBottom > Y+TableHeight+BottomBorder then
      begin  {can complete table now}
      for I := 0 to Rows.Count-1 do   {do remainder of table now}
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
      ThtmlViewer(ParentSectionList.TheOwner).TablePartRec.TablePart := Normal;
      end
    else
      begin  {will do part of the table now}
      {Leave room for foot later}
      ParentSectionList.PageBottom := ParentSectionList.PageBottom
                               -FootHeight+IntMax(Cellspacing, 1)-BottomBorder;
      for I := 0 to Rows.Count-1 do
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
      BodyBreak := ParentSectionList.PageBottom;
      if FootStartRow >= 0 then
        begin
        TablePartRec.TablePart := DoFoot;
        TablePartRec.PartStart := Y + FootOffset;
        TablePartRec.PartHeight := FootHeight + IntMax(2*Cellspacing, Cellspacing+1) + BottomBorder;//FootHeight+IntMax(CellSpacing, 1);
        ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
        end
      else if HeaderHeight > 0 then
        begin
        TablePartRec.TablePart := DoHead;
        TablePartRec.PartStart := DrawY-TopBorder;
        TablePartRec.PartHeight := HeaderHeight+TopBorder;
        ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
        end;
      ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
      end;
    end;
  {.$EndRegion}
  {.$Region 'DoBody2'}
  DoBody2:
    begin
    if ParentSectionList.PageBottom > Y+TableHeight+BottomBorder then
      begin
      for I := 0 to Rows.Count-1 do   {do remainder of table now}
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
      ThtmlViewer(ParentSectionList.TheOwner).TablePartRec.TablePart := Normal;
        ParentSectionList.PrintingTable := Nil;
      end
    else
      begin
      SavePageBottom := ParentSectionList.PageBottom;
      for I := 0 to Rows.Count-1 do   {do part of table}
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
      BodyBreak := ParentSectionList.PageBottom;
      if FootStartRow >= 0 then
        begin
        TablePartRec.TablePart := DoFoot;
        TablePartRec.PartStart := Y + FootOffset;
        TablePartRec.PartHeight := FootHeight + IntMax(2*Cellspacing, Cellspacing+1) + BottomBorder;//FootHeight+IntMax(CellSpacing, 1);
        ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
        end
      else if HeaderHeight > 0 then
        begin
        ParentSectionList.PageBottom := SavePageBottom;
        TablePartRec.TablePart := DoHead;
        TablePartRec.PartStart := DrawY-TopBorder;
        TablePartRec.PartHeight := HeaderHeight+TopBorder;
        ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
        end;
      ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
      end;
    end;
  {.$EndRegion}
  {.$Region 'DoFoot'}
  DoFoot:
    begin
    YY := TablePartRec.PartStart;
    if FootStartRow >= 0 then
      for I := FootStartRow to Rows.Count-1 do
        YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
                XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
                BorderColorDark, I);
    if HeaderHeight > 0 then
      begin
      TablePartRec.TablePart := DoHead;
      TablePartRec.PartStart := DrawY-TopBorder;
      TablePartRec.PartHeight := HeaderHeight+TopBorder;
      end
    else
      begin    {No THead}
      TablePartRec.TablePart := DoBody3;
      TablePartRec.PartStart := BodyBreak-1;
      TablePartRec.FootHeight := FootHeight+IntMax(Cellspacing, 1);
      end;
    ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
    end;
  {.$EndRegion}
  {.$Region 'DoHead'}
  DoHead:
    begin
    for I := 0 to HeaderRowCount-1 do
      YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, Border > 0, BorderColorLight,
              BorderColorDark, I);
      TablePartRec.TablePart := DoBody1;
      TablePartRec.PartStart := BodyBreak-1;
      TablePartRec.FootHeight := FootHeight+IntMax(Cellspacing, 1)+BottomBorder;
      ThtmlViewer(ParentSectionList.TheOwner).TablePartRec := TablePartRec;
    end;
  {.$$EndRegion}
  end;
end;

{----------------ThtmlTable.GetURL}
function ThtmlTable.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType;
var
  TableOK: boolean;

  function GetTableURL(X: integer; Y: integer): guResultType;
  var
    I, J, XX: integer;
    CellObj: TCellObj;
  begin
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        CellObj := TCellObj(Items[I]);
        if Assigned(CellObj) then
          with CellObj do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY+Ht) then
              begin
              Result := Cell.GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]);
        end;
      end;
    end;
  Result := [];
  end;

begin
Result := [];
if (Y >= ContentTop) and (Y < ContentBot) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  if TableOK then
    Result := GetTableURL(X, Y);
  end;
end;

{----------------ThtmlTable.PtInObject}
function ThtmlTable.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
var
  TableOK: boolean;

  function GetTableObj(X: integer; Y: integer): boolean;
  var
    I, J, XX: integer;
  begin
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY+Ht) then
              begin
              Result := Cell.PtInObject(X, Y, Obj, IX, IY);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]);
        end;
      end;
    end;
  Result := False;
  end;

begin
Result := False;
if (Y >= ContentTop) and (Y < ContentBot) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  if TableOK then
    Result := GetTableObj(X, Y);
  end;
end;

{----------------ThtmlTable.FindCursor}
function ThtmlTable.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
var
  TableOK: boolean;

  function GetTableCursor(X: integer; Y: integer; var XR: integer;
           var YR: integer; var CaretHt: integer; var Intext: boolean): integer;
  var
    I, J, XX: integer;
  begin
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY+Ht) then
              begin
              Result := Cell.FindCursor(Canvas, X, Y, XR, YR, CaretHt, InText);
              if Result >= 0 then
                Exit;
              end;
            end;
        Inc(XX, Widths[I]);
        end;
      end;
    end;
  Result := -1;
  end;

begin
Result := -1;
if ( Y >= ContentTop) and (Y < ContentBot) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  if TableOK then
    Result := GetTableCursor(X, Y, XR, YR, CaretHt, InText);
  end;
end;

{----------------ThtmlTable.CursorToXY}
function ThtmlTable.CursorToXY(Canvas: TCanvas; Cursor: integer;
                                var X: integer; var Y: integer): boolean;
{note: returned X value is not correct here but it isn't used}
var
  I, J: integer;
begin
Result := False;
if (Len = 0) or (Cursor > StartCurs + Len) then Exit;
for J := 0 to Rows.Count-1 do
  with  TCellList(Rows[J]) do
    for I := 0 to Count-1 do
      if Assigned(Items[I]) then
        with TCellObj(Items[I]) do
          begin
          Result := Cell.CursorToXy(Canvas, Cursor, X, Y);
          if Result then Exit;
          end;
end;

{----------------ThtmlTable.GetChAtPos}
function ThtmlTable.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
var
  I, J: integer;
begin
Result := False;
if (Len = 0) or (Pos < StartCurs) or (Pos > StartCurs + Len) then Exit;

for J := 0 to Rows.Count-1 do
  with  TCellList(Rows[J]) do
    for I := 0 to Count-1 do
      if Assigned(Items[I]) then
        with TCellObj(Items[I]) do
          begin
          Result := Cell.GetChAtPos(Pos, Ch, Obj);
          if Result then Exit;
          end;
end;

{----------------ThtmlTable.FindString}
function ThtmlTable.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;

var
  I, J: integer;
begin
Result := -1;
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindString(From, ToFind, MatchCase);
            if Result >= 0 then Exit;
            end;
end;

{----------------ThtmlTable.FindStringR}
function ThtmlTable.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;

var
  I, J: integer;
begin
Result := -1;
for J := Rows.Count-1 downto 0 do
    with  TCellList(Rows[J]) do
      for I := Count-1 downto 0 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindStringR(From, ToFind, MatchCase);
            if Result >= 0 then Exit;
            end;
end;

{----------------ThtmlTable.FindSourcePos}
function ThtmlTable.FindSourcePos(DocPos: integer): integer;
var
  I, J: integer;
begin
Result := -1;
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindSourcePos(DocPos);
            if Result >= 0 then Exit;
            end;
end;

{----------------ThtmlTable.FindDocPos}
function ThtmlTable.FindDocPos(SourcePos: integer; Prev: boolean): integer;
var
  I, J: integer;
  TC: TCellObj;
begin
Result := -1;
if not Prev then
  begin
  for J := 0 to Rows.Count-1 do
    if Assigned(Rows.Items[J]) then
      with  TCellList(Rows[J]) do
        for I := 0 to Count-1 do
          begin
          TC := TCellObj(Items[I]);
          if Assigned(TC) then
            begin
            Result := TC.Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then Exit;
            end;
          end;
  end
else  {Prev , iterate in reverse}
  begin
  for J := Rows.Count-1 downto 0 do
      with  TCellList(Rows[J]) do
        for I := Count-1 downto 0 do
          if Assigned(Items[I]) then
          begin
          TC := TCellObj(Items[I]);
          if Assigned(TC) then
            begin
            Result := TC.Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then Exit;
            end;
          end;
  end;
end;

{----------------ThtmlTable.CopyToClipboard}
procedure ThtmlTable.CopyToClipboard;
var
  I, J: integer;
begin
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            Cell.CopyToClipboard;
end;

{----------------TSection.Create}
constructor TSection.Create(AMasterList: TSectionList; L: TAttributeList;
            Prop: TProperties; AnURL: TUrlTarget; ACell: TCellBasic; FirstItem: boolean);
var
  FO : TFontObj;
  T: TAttribute;
  S: string;
  Clr: ClearAttrType;
  Percent: boolean;
begin
inherited Create(AMasterList);
Buff := PWideChar(BuffS);
Len := 0;
BuffSize := 0;
Fonts := TFontList.Create;

FO := TFontObj.Create(Self, Prop.GetFont, 0);
FO.Title := Prop.PropTitle;
if Assigned(AnURL) and (Length(AnURL.Url) > 0) then
  begin
  FO.CreateFIArray;
  Prop.GetFontInfo(FO.FIArray);
  FO.ConvertFont(FO.FIArray.Ar[LFont]);
  FO.UrlTarget.Copy(AnUrl);
  ParentSectionList.LinkList.Add(FO);
  {$ifndef NoTabLink}
  if not AMasterList.StopTab then
    FO.CreateTabControl(AnUrl.TabIndex);
  {$endif}
  end;

Fonts.Add(FO);

LineHeight := Prop.GetLineHeight(Abs(FO.TheFont.Height));
if FirstItem then
  begin
  FirstLineIndent := Prop.GetTextIndent(Percent);
  if Percent then
    FLPercent := IntMin(FirstLineIndent, 90);
  end;

Images := TImageObjList.Create;
FormControls := TFormControlList.Create;

if Assigned(L) then
  begin
  if L.Find(ClearSy, T) then
    begin
    S := LowerCase(T.Name);
    if (S = 'left') then ClearAttr := clLeft
    else if (S = 'right') then ClearAttr := clRight
    else ClearAttr := clAll;
    end;
  if L.TheID <> '' then
    ParentSectionList.IDNameList.AddObject(L.TheID, Self);
  end;
if Prop.GetClear(Clr) then
  ClearAttr := Clr;

Lines := TFreeList.Create;
if Prop.Props[TextAlign] = 'right' then
  Justify := Right
else if Prop.Props[TextAlign] = 'center' then
  Justify := Centered
else if Prop.Props[TextAlign] = 'justify' then
  Justify := FullJustify
else Justify := Left;
BreakWord := Prop.Props[WordWrap] = 'break-word';
end;

{----------------TSection.CreateCopy}
constructor TSection.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TSection;
  I: integer;
begin
inherited CreateCopy(AMasterList, T);
TT := T as TSection;
Len := TT.Len;
BuffSize := TT.BuffSize;
BuffS := TT.BuffS;
SetLength(BuffS, Length(BuffS));
Buff := PWideChar(BuffS);
Brk := TT.Brk;
Fonts := TFontList.CreateCopy(Self, TT.Fonts);
Images := TImageObjList.CreateCopy(AMasterList, TT.Images);
FormControls := TFormControlList.Create;
for I := 0 to TT.FormControls.Count-1 do
  FormControls.Add(TT.FormControls[I]);
Lines := TFreeList.Create;
Justify := TT.Justify;
ClearAttr := TT.ClearAttr;
LineHeight := TT.LineHeight;
FirstLineIndent := TT.FirstLineIndent;
FLPercent := TT.FLPercent;
BreakWord := TT.BreakWord;
end;

{----------------TSection.Destroy}
destructor TSection.Destroy;
begin
if Assigned(XP) then
  Freemem(XP);
Fonts.Free;
Images.Free;
FormControls.Free;
SIndexList.Free;
Lines.Free;
inherited Destroy;
end;

procedure TSection.CheckFree;
var
  I, J: integer;
begin
if not Assigned(Self) then
  Exit;
if Assigned(ParentSectionList) then
  begin
  {Check to see that there isn't a TFontObj in LinkList}
  if Assigned(ParentSectionList.LinkList) then
    for I := 0 to Fonts.Count-1 do
      begin
      J := ParentSectionList.LinkList.IndexOf(Fonts[I]);
      if J >=0 then
        ParentSectionList.LinkList.Delete(J);
      end;
  {Remove Self from IDNameList if there}
  if Assigned(ParentSectionList.IDNameList) then
    with ParentSectionList.IDNameList do
      begin
      I := IndexOfObject(Self);
      if I > -1 then
        Delete(I);
      end;
  end;
end;

{----------------TSection.AddChar}
procedure TSection.AddChar(C: WideChar; Index: integer);
var
  Tok: TokenObj;
begin
Tok := TokenObj.Create;
Tok.AddUnicodeChar(C, Index);
AddTokenObj(Tok);
Tok.Free;
end;

function TSection.GetIndexObj(I: integer): IndexObj;
begin
Result := SIndexList[I];
end;

procedure TSection.AddOpBrk;
begin
if Brk <> '' then
  Brk[Length(Brk)] := 'a';
end;

{----------------TSection.AddTokenObj}
procedure TSection.AddTokenObj(T : TokenObj);
var
  L, I : integer;
  C: char;
  St, StU: WideString;
  Small: boolean;
begin
if T.Leng = 0 then Exit;

L := Len+T.Leng;
if BuffSize < L+3 then Allocate(L + 500);  {L+3 to permit additions later}
case PropStack.Last.GetTextTransform of
  txUpper:
    St := WideUpperCase1(T.S);
  txLower:
    St := WideLowerCase1(T.S);
  else
    St := T.S;
  end;
Move(T.I[1], XP^[Len], T.Leng*Sizeof(integer));
if NoBreak or (Self is TPreformated) then
  C := 'n'
else C := 'y';
for I := 1 to T.Leng do
  Brk := Brk+C;

if PropStack.Last.GetFontVariant = 'small-caps' then
  begin
  StU := WideUpperCase1(St);
  BuffS := BuffS+StU;
  Small := False;
  for I := 1 to Length(St) do
    begin
    if not (St[I] in [WideChar(' '), WideChar('0')..WideChar('9')]) then   {no font changes for these chars}
      begin
      if not Small then
        begin
        if StU[I] <> St[I] then
          begin    {St[I] was lower case}
          PushNewProp('small', '', '', '', '', Nil);  {change to smaller font}
          ChangeFont(PropStack.Last);
          Small := True;
          end;
        end
      else
        if StU[I] = St[I] then
          begin    {St[I] was uppercase and Small is set}
          PopAProp('small');
          ChangeFont(PropStack.Last);
          Small := False;
          end;
      end;
    Inc(Len);
    end;
  if Small then   {change back to regular font}
    begin
    PopAProp('small');
    ChangeFont(PropStack.Last);
    end;
  end
else
  begin
  BuffS := BuffS+St;
  Len := L;
  end;
Buff := PWideChar(BuffS);
end;

{----------------TSection.ProcessText}
Procedure TSection.ProcessText(TagIndex: integer);
const
  Shy = #173;  {soft hyphen}
var
  I: integer;
  FO: TFontObj;

  Procedure Remove(I: integer);
  begin
  Move(XP^[I], XP^[I-1], ((Length(BuffS))-I)*Sizeof(integer));
  System.Delete(BuffS, I, 1);
  System.Delete(Brk, I, 1);
  TFormControlList(FormControls).Decrement(I-1);
  TFontList(Fonts).Decrement(I-1, ParentSectionList);
  TImageObjList(Images).Decrement(I-1);
  end;

begin
while (Length(BuffS) > 0) and (BuffS[1] = ' ') do
  Remove(1);

I := WidePos(Shy, BuffS);
while I > 0 do
  begin
  Remove(I);
  if (I > 1) and (Brk[I-1] <> 'n') then
    Brk[I-1] := 's';
  I := WidePos(Shy, BuffS);
  end;

I := WidePos('  ', BuffS);
while I > 0 do
  begin
  if Brk[I] = 'n' then
    Remove(I)
  else
    Remove(I + 1);
  I := WidePos('  ', BuffS);
  end;

{After floating images at start, delete an annoying space}
for I := Length(BuffS)-1 downto 1 do
  if (BuffS[I] = ImgPan) and (Images.FindImage(I-1).ObjAlign in [ALeft, ARight])
               and (BuffS[I+1] = ' ') then
    Remove(I+1);

I := WidePos(WideString(' '+#8), BuffS);  {#8 is break char}
while I > 0 do
  begin
  Remove(I);
  I := WidePos(WideString(' '+#8), BuffS);
  end;

  I := WidePos(WideString(#8+' '), BuffS);
while I > 0 do
  begin
  Remove(I+1);
  I := WidePos(WideString(#8+' '), BuffS);
  end;

if (Length(BuffS) > 1) and (BuffS[Length(BuffS)] = #8) then
  Remove(Length(BuffS));

if (Length(BuffS) > 1) and (BuffS[Length(BuffS)] = ' ') then
  Remove(Length(BuffS));

if (BuffS <> #8) and (Length(BuffS) > 0) and (BuffS[Length(BuffS)] <> ' ') then
  Begin
  FO := TFontObj(Fonts.Items[Fonts.Count-1]);    {keep font the same for inserted space}
  if FO.Pos = Length(BuffS) then
    Inc(FO.Pos);
  BuffS := BuffS+' ';
  XP^[Length(BuffS)-1] := TagIndex;
  end;

Finish;
end;

{----------------TSection.Finish}
procedure TSection.Finish;
{complete some things after all information added}
var
  Last, I: integer;
  IO: IndexObj;
begin
Buff := PWideChar(BuffS);
Len := Length(BuffS);
if Len > 0 then
  begin
  Brk := Brk+'y';
  if Assigned(XP) then  {XP = Nil when printing}
    begin
    Last := 0;   {to prevent warning msg}
    SIndexList := TFreeList.Create;
    for I := 0 to Len-1 do
      begin
      if (I = 0) or (XP^[I] <> Last+1) then
        begin
        IO := IndexObj.Create;
        IO.Pos := I;
        IO.Index := XP^[I];
        SIndexList.Add(IO);
        end;
      Last := XP^[I];
      end;
    FreeMem(XP);
    XP := Nil;
    end;
  end;
if Len > 0 then
  begin
  Inc(ParentSectionList.SectionCount);
  SectionNumber := ParentSectionList.SectionCount;
  end;
end;

{----------------TSection.Allocate}
procedure TSection.Allocate(N : integer);
begin
if BuffSize < N then
  begin
  ReAllocMem(XP, N*Sizeof(integer));
  BuffSize := N;
  end;
end;

{----------------TSection.ChangeFont}
procedure TSection.ChangeFont(Prop: TProperties);
var
  FO: TFontObj;
  LastUrl: TUrlTarget;
  NewFont: TMyFont;
  Align: AlignmentType;
begin
FO := TFontObj(Fonts[Fonts.Count-1]);
LastUrl := FO.UrlTarget;
NewFont := Prop.GetFont;
If FO.Pos = Len then
  FO.ReplaceFont(NewFont)  {fontobj already at this position, modify it}
else
  begin
  FO := TFontObj.Create(Self, NewFont, Len);
  FO.URLTarget.Copy(LastUrl);
  Fonts.Add(FO);
  end;
FO.Title := Prop.PropTitle;
if LastUrl.Url <> '' then
  begin
  FO.CreateFIArray;
  Prop.GetFontInfo(FO.FIArray);
  FO.ConvertFont(FO.FIArray.Ar[LFont]);
  if ParentSectionList.LinkList.IndexOf(FO) = -1 then
    ParentSectionList.LinkList.Add(FO);
  end;
if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then
  FO.SScript := Align
else FO.SScript := ANone;
end;

{----------------------TSection.HRef}
procedure TSection.HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
          Attributes: TAttributeList; Prop: TProperties);
var
  FO: TFontObj;
  NewFont: TMyFont;
  Align: AlignmentType;
begin
FO := TFontObj(Fonts[Fonts.Count-1]);
NewFont := Prop.GetFont;
If FO.Pos = Len then
  FO.ReplaceFont(NewFont)  {fontobj already at this position, modify it}
else
  begin
  FO := TFontObj.Create(Self, NewFont, Len);
  Fonts.Add(FO);
  end;

if Sy = HRefSy then
  begin
  FO.CreateFIArray;
  Prop.GetFontInfo(FO.FIArray);
  FO.ConvertFont(FO.FIArray.Ar[LFont]);
  if ParentSectionList.LinkList.IndexOf(FO) = -1 then
    ParentSectionList.LinkList.Add(FO);
  {$ifndef NoTabLink}
  if not ParentSectionList.StopTab then
    FO.CreateTabControl(AnUrl.TabIndex);
  {$endif}
  end
else if Assigned(FO.FIArray) then
  begin
  FO.FIArray.Free;
  FO.FIArray := Nil;
  end;
FO.UrlTarget.Copy(AnUrl);
if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then
  FO.SScript := Align
else FO.SScript := ANone;
end;

function TSection.AddImage(L: TAttributeList; ACell: TCellBasic; Index: integer): TImageObj;
begin
Result := TImageObj.Create(ParentSectionList, Len, L);
Result.MyCell := ACell;
Images.Add(Result);
AddChar(ImgPan, Index);    {marker for image}
end;

function TSection.AddPanel(L: TAttributeList;
                     ACell: TCellBasic; Index: integer): TPanelObj;
begin
Result := TPanelObj.Create(ParentSectionList, Len, L, ACell, False);
Images.Add(Result);
AddChar(ImgPan, Index);    {marker for panel}
end;

function TSection.CreatePanel(L: TAttributeList;
                     ACell: TCellBasic): TPanelObj;
{Used by object tag}
begin
Result := TPanelObj.Create(ParentSectionList, Len, L, ACell, True);
end;

procedure TSection.AddPanel1(PO: TPanelObj; Index: integer);
{Used for Object Tag}
begin
Images.Add(PO);
AddChar(ImgPan, Index);    {marker for panel}
end;

{----------------TSection.AddFormControl}
function TSection.AddFormControl(Which: Symb; AMasterList: TSectionList;
         L: TAttributeList; ACell: TCellBasic; Index: integer;
         Prop: TProperties): TFormControlObj;
var
  T: TAttribute;
  FCO: TFormControlObj;
  S: string;
  IO: TImageObj;
  ButtonControl:  TButtonFormControlObj;

  procedure GetEditFCO;
  begin
  FCO := TEditFormControlObj.Create(AMasterList, Len, L, S, Prop);
  end;

begin
S := '';
if Which = InputSy then
  begin
  if L.Find(TypeSy, T) then
    begin
    S := LowerCase(T.Name);
    if (S = 'text') or (S = 'password') or (S = 'file') then
      GetEditFCO
    else if (S = 'submit') or (S = 'reset') or (S = 'button') then
      FCO := TButtonFormControlObj.Create(AMasterList, Len, L, S, Prop)
    else if S = 'radio' then
      FCO := TRadioButtonFormControlObj.Create(AMasterList, Len, L, ACell)
    else if S = 'checkbox' then
      FCO := TCheckBoxFormControlObj.Create(AMasterList, Len, L, Prop)
    else if S = 'hidden' then
      FCO := THiddenFormControlObj.Create(AMasterList, Len, L)
    else if S = 'image' then
      FCO := TImageFormControlObj.Create(AMasterList, Len, L)
    else
      GetEditFCO;
    end
  else
    GetEditFCO;
  end
else if Which = SelectSy then
  begin
  if L.Find(MultipleSy, T) or L.Find(SizeSy, T) and (T.Value > 1) then
    FCO := TListBoxFormControlObj.Create(AMasterList, Len, L, Prop)
  else
    FCO := TComboFormControlObj.Create(AMasterList, Len, L, Prop);
  end
else
  FCO := TTextAreaFormControlObj.Create(AMasterList, Len, L, Prop);
if S = 'image' then
  begin
  IO := AddImage(L, ACell, Index);  {leave out of FormControlList}
  IO.MyFormControl := TImageFormControlObj(FCO);
  TImageFormControlObj(FCO).MyImage := IO;
  end
else if S = 'file' then
  begin
  FormControls.Add(FCO);
  AddChar(FmCtl, Index);    {marker for FormControl}
  Brk[Len] := 'n';          {don't allow break between these two controls}
  ButtonControl := TButtonFormControlObj.Create(AMasterList, Len, L, S, Prop);
  ButtonControl.MyEdit := TEditFormControlObj(FCO);
  FormControls.Add(ButtonControl);
  {the following fixup puts the ID on the TEdit and deletes it from the Button}
  if L.TheID <> '' then
    ParentSectionList.IDNameList.AddObject(L.TheID, FCO);
  FCO.Value := '';   {value attribute should not show in TEdit}
  ThtEdit(FCO.TheControl).Text := '';
  AddChar(FmCtl, Index);
  Brk[Len] := 'n';
  end
else if S <> 'hidden' then
  begin
  FormControls.Add(FCO);
  AddChar(FmCtl, Index);    {marker for FormControl}
  end;
if Prop.GetBorderStyle <> bssNone then   {start of inline border}
  ParentSectionList.ProcessInlines(Index, Prop, True);
Result := FCO;
end;

{----------------TSection.FindCountThatFits}
function TSection.FindCountThatFits(Canvas: TCanvas; Width: integer; Start: PWideChar; Max: integer): integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, OHang, Tmp : integer;
  Picture: boolean;
  Align: AlignmentType;
  HSpcL, HSpcR: integer;
  FLObj: TFloatingObj;
  Extent: integer;
const
  OldStart: PWideChar = nil;
  OldResult: integer = 0;
  OldWidth: integer = 0;

begin
if (Width = OldWidth) and (Start = OldStart) then
  begin
  Result := OldResult;
  Exit;
  end;
OldStart := Start;
OldWidth := Width;
Cnt := 0;
XX := 0;
while True do
  begin
  Fonts.GetFontAt(Start-Buff, OHang).AssignToCanvas(Canvas);
  J1 := Fonts.GetFontCountAt(Start-Buff, Len);
  J2 := Images.GetImageCountAt(Start-Buff);
  J3 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J2 = 0 then
    begin
    Tmp:= Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    if not (Align in [ALeft, ARight]) then
      XX := XX + Tmp + HSpcL + HSpcR;
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;
    end
  else if J3 = 0 then
    begin
    XX := XX + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    XX := XX + HSpcL + HSpcR;
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;
    end
  else
    begin
    Picture := False;
    J := IntMin(J1, J2);
    J := IntMin(J, J3);
    I := FitText(Canvas.Handle, Start, J, Width-XX, Extent);
    end;
  if Cnt+I >= Max then      {I has been initialized}
    begin
    Cnt := Max;
    Break;
    end
  else Inc(Cnt, I);

  if not Picture then
    begin
    if (I < J) or (I = 0) then
      Break;
    XX := XX + Extent;
    end;

  Inc(Start, I);
  end;
Result := Cnt;
OldResult := Result;
end;

{----------------TSection.FindCountThatFits1}
function TSection.FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; Max: integer; X, Y: integer; IMgr: IndentManager;
                var ImgHt: integer; NxImages: TList) : integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, X1, X2,
    OHang, ImgWidth, Width : integer;
  Picture: boolean;
  Align: AlignmentType;
  ImageAtStart: boolean;
  FlObj: TFloatingObj;
  HSpcL, HSpcR: integer;
  BrChr, TheStart: PWideChar;
  Font, LastFont: TMyFont;
  SaveX: integer;
  FoundBreak: boolean;
  HyphenWidth: Integer;

begin
LastFont := Nil;
TheStart := Start;
ImageAtStart := True;
ImgHt := 0;

BrChr := StrScanW(TheStart, BrkCh); {see if a break char}
if Assigned(BrChr) and (BrChr-TheStart < Max) then
  begin
  Max := BrChr-TheStart;
  if Max = 0 then
    begin
    Result := 1;
    Exit;     {single character fits}
    end;
  FoundBreak := True;
  end
else FoundBreak := False;

Cnt := 0;
X1 := Imgr.LeftIndent(Y);
if Start = Buff then
  Inc(X1, FirstLineIndent);
X2 := IMgr.RightSide(Y);
Width := X2-X1;

if (Start = Buff) and (Images.Count = 0) and (FormControls.Count = 0) then
  if Max * TFontObj(Fonts[0]).tmMaxCharWidth <= Width then  {try a shortcut}
    begin  {it will all fit}
    Result := Max;
    if FoundBreak then
      Inc(Result);
    Exit;
    end;

XX := 0;
while True do
  begin
  Font := Fonts.GetFontAt(Start-Buff, OHang);
  if Font <> LastFont then   {may not have to load font}
    begin
    Font.AssignToCanvas(Canvas);
    end;
  LastFont := Font;
  J1 := IntMin(Fonts.GetFontCountAt(Start-Buff, Len), Max-Cnt);
  J2 := Images.GetImageCountAt(Start-Buff);
  J3 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J2 = 0 then
    begin   {next is an image}
    I := 1;  J := 1;
    Picture := True;
    ImgWidth := Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    if Align in [ALeft, ARight] then
      begin
      FlObj.DrawYY := Y;     {approx y position}
      if ImageAtStart then
        begin
        Inc(XX, ImgWidth + FlObj.HSpaceL + FlObj.HSpaceR);
        if XX <= Width then   {it fits}
          begin
          IMgr.Update(Y, FlObj);
          ImgHt := IntMax(ImgHt, FlObj.ImageHeight + FlObj.VSpaceT + FlObj.VSpaceB);
          end
        else if Cnt > 0 then
          Break    {One or more do fit, this one doesn't}
        else
          begin   {first image doesn't fit}
          if IMgr.GetNextWiderY(Y) > Y then
            Break;   {wider area below, it might fit there}
          {Can't move it down, might as well put it here}
          IMgr.Update(Y, FlObj);
          ImgHt := IntMax(ImgHt, FlObj.ImageHeight + FlObj.VSpaceT + FlObj.VSpaceB);
          Cnt := 1;
          Break;
          end;
        end
      else
        NxImages.Add(FlObj);    {save it for the next line}
      end
    else
      begin
      Inc(XX, ImgWidth+HSpcL+HSpcR);
      ImageAtStart := False;
      end;
    if XX > Width then break;
    end
  else if J3 = 0 then
    begin
    XX := XX + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    XX := XX + HSpcL + HSpcR;
    I := 1;  J := 1;
    Picture := True;
    ImageAtStart := False;
    if XX > Width then break;
    end
  else
    begin
    Picture := False;
    J := IntMin(J1, J2);
    J := IntMin(J, J3);
    I := FitText(Canvas.Handle, Start, J, Width-XX, SaveX);
    if (I > 0) and (Brk[TheStart-Buff+Cnt+I] = 's') then
      begin  {a hyphen could go here}
      HyphenWidth := Canvas.TextWidth('-');
      if XX + SaveX + HyphenWidth > Width then
        Dec(I);
      end;
    end;
  if Cnt+I >= Max then
    begin
    Cnt := Max;
    Break;
    end
  else Inc(Cnt, I);

  if not Picture then  {it's a text block}
    begin
    if I < J then Break;
    XX := XX + SaveX;
    ImageAtStart := False;
    end;

  Inc(Start, I);
  end;
Result := Cnt;
if FoundBreak and (Cnt = Max) then
  Inc(Result);
end;

function WrapChar(C: WideChar): boolean;
begin
Result := Ord(C) >= $3000;
end;

{----------------TSection.MinMaxWidth}
procedure TSection.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
{Min is the width the section would occupy when wrapped as tightly as possible.
 Max, the width if no wrapping were used.}
var
  I, Indx, FloatMin: integer;
  P, P1: PWideChar;
  Obj: TObject;
  SoftHyphen: Boolean;

  function FindTextWidthB(Canvas: TCanvas; Start: PWideChar; N: integer; RemoveSpaces: boolean): integer;
  begin
  Result := FindTextWidth(Canvas, Start, N, RemoveSpaces);
  if (Start = Buff) then
    if (FLPercent = 0) then   {not a percent}
      Inc(Result, FirstLineIndent)
    else
      Result := (100 * Result) div (100 - FLPercent);
  if SoftHyphen then
    Result := Result + Canvas.TextWidth('-');
  end;

begin
if (StoredMin > 0) and (Images.Count = 0) then
  begin
  Min := StoredMin;
  Max := StoredMax;
  Exit;
  end;
Min := 0;
Max := 0;
if Len = 0 then Exit;

for I := 0 to Images.Count-1 do     {call drawlogic for all the images}
  begin
  Obj := Images[I];
  with TFloatingObj(Obj) do
    begin
    DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Pos, Indx), 0, 0);
    if not PercentWidth then
      if ObjAlign in [ALeft, ARight] then
        begin
        Max := Max + ImageWidth + HSpaceL + HSpaceR;
        Brk[Pos+1] := 'y';    {allow break after floating image}
        end
      else Min := IntMax(Min, ImageWidth);
    end;
  end;
FloatMin := Max;

for I := 0 to FormControls.Count-1 do     {get Min for form controls}
  begin
  Obj := FormControls[I];
  if Obj is TFormControlObj then
    with TFormControlObj(FormControls[I]) do
      if not PercentWidth then
        Min := IntMax(Min, FControl.Width + HSpaceL + HSpaceR);
  end;

Max := 0;
P := Buff;
P1 := StrScanW(P, BrkCh); {look for break char}
while Assigned(P1) do
  begin
  Max := IntMax(Max, FindTextWidthB(Canvas, P, P1-P, False));
  P:= P1+1;
  P1 := StrScanW(P, BrkCh);
  end;
P1 := StrScanW(P, #0); {look for the end}
Max := IntMax(Max, FindTextWidthB(Canvas, P, P1-P, True)) + FloatMin;

P := Buff;
if not BreakWord then
  begin
  while P^ = ' ' do Inc(P);
  P1 := P;
  I := P1-Buff+1;
  while P^ <> #0 do
    {find the next string of chars that can't be wrapped}
    begin
    if WrapChar(P1^) and  (Brk[I]='y') then
      begin
      Inc(P1);
      Inc(I);
      end
    else
      begin
      repeat
        begin
        Inc(P1);
        Inc(I);
        end;
      until (P1^=#0) or
            ((((P1^ in [WideChar(' '), WideChar('-'), WideChar('?'), ImgPan, FmCtl, BrkCh]) or WrapChar(P1^))
                 and (Brk[I]='y')) or (Brk[I-1] in ['a', 's']));
      SoftHyphen := Brk[I-1] = 's';
      if P1^ in [WideChar('-'), WideChar('?')] then
        begin
        Inc(P1);
        Inc(I);
        end;
      end;
    Min := IntMax(Min, FindTextWidthB(Canvas, P, P1-P, True));
    while (P1^ in [WideChar(' '), ImgPan, FmCtl, BrkCh]) do
      begin
      Inc(P1);
      Inc(I);
      end;
    P := P1;
    end;
  end
else
  while P^ <> #0 do
    begin
    Min := IntMax(Min, FindTextWidthB(Canvas, P, 1, True));
    Inc(P);
    end;

Min := IntMax(FloatMin, Min);
StoredMin := Min;
StoredMax := Max;
end;

{----------------TSection.FindTextWidth}
function TSection.FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: integer; RemoveSpaces: boolean): integer;
{find actual line width of N chars starting at Start.  If RemoveSpaces set,
 don't count spaces on right end}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
begin
Result := 0;
if RemoveSpaces then
  while ((Start + N - 1)^ in [WideChar(' '), BrkCh]) do
    Dec(N);    {remove spaces on end}
while N > 0 do
  begin
  J := Images.GetImageCountAt(Start-Buff);
  J1 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J = 0 then  {it's and image}
    begin
    Wid := Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    {Here we count floating images as 1 char but do not include their width,
      This is required for the call in FindCursor}
    if not (Align in [ALeft, ARight]) then
      begin
      Result := Result + Wid + HSpcL + HSpcR;
      end;
    Dec(N);   {image counts as one char}
    Inc(Start);
    end
  else if J1 = 0 then
    begin
    Result := Result + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    Result := Result + HSpcL + HSpcR;
    Dec(N);   {control counts as one char}
    Inc(Start);
    end
  else
    begin
    Fonts.GetFontAt(Start-Buff, OHang).AssignToCanvas(Canvas);
    I := IntMin(J, J1);
    I := IntMin(I, IntMin(Fonts.GetFontCountAt(Start-Buff, Len), N));
    Assert(I > 0, 'I less than or = 0 in FindTextWidth');
    Inc(Result, GetXExtent(Canvas.Handle, Start, I) + OHang);
    if I = 0 then
      Break;
    Dec(N, I);
    Inc(Start, I);
    end;
  end;
end;

{----------------TSection.FindTextWidthA}
function TSection.FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: integer): integer;
{find actual line width of N chars starting at Start.}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
  Font: TMyFont;
begin
Result := 0;
while N > 0 do
  begin
  J := Images.GetImageCountAt(Start-Buff);
  J1 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J = 0 then  {it's an image}
    begin
    Wid := Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    {Here we count floating images as 1 char but do not include their width,
      This is required for the call in FindCursor}
    if not (Align in [ALeft, ARight]) then
      begin
      Result := Result + Wid + HSpcL + HSpcR;
      end;
    Dec(N);   {image counts as one char}
    Inc(Start);
    end
  else if J1 = 0 then
    begin
    Result := Result + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    Result := Result + HSpcL + HSpcR;
    Dec(N);   {control counts as one char}
    Inc(Start);
    end
  else
    begin
    Font := Fonts.GetFontAt(Start-Buff, OHang);
    Font.AssignToCanvas(Canvas);
    I := IntMin(J, J1);
    I := IntMin(I, IntMin(Fonts.GetFontCountAt(Start-Buff, Len), N));
    Inc(Result, GetXExtent(Canvas.Handle, Start, I) - OHang);
    if I = 0 then
      Break;
    Dec(N, I);
    Inc(Start, I);
    end;
  end;
end;

{----------------TSection.DrawLogic}
function TSection.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
{returns height of the section}
var
  PStart, P, Last: PWideChar;
  Max, N, NN, Width, I, Indx, ImgHt: integer;
  Finished: boolean;
  LR : LineRec;
  NxImages: TList;
  Tmp, Tmp1: integer;
  Obj: TFloatingObj;
  TopY, AccumImgBot, HtRef: integer;

  function GetClearSpace(ClearAttr: ClearAttrType): integer;
  var
    CL, CR: integer;
  begin
  Result := 0;
  if (ClearAttr <> clrNone) then
    begin  {may need to move down past floating image}
    IMgr.GetClearY(CL, CR);
    case ClearAttr of
      clLeft:  Result := IntMax(0, CL-Y-1);
      clRight:  Result := IntMax(0, CR-Y-1);
      clAll: Result := IntMax(CL-Y-1, IntMax(0, CR-Y-1));
      end;
    end;
  end;

  procedure LineComplete(NN : integer);
  var
    I, J, DHt, Desc, Tmp, TmpRt, Cnt, Index, H, SB, SA : integer;
    FO : TFontObj;
    Align: AlignmentType;
    FormAlign: AlignmentType;
    NoChar: boolean;
    P: PWideChar;
    FCO: TFormControlObj;
    FlObj: TFloatingObj;
    TextWidth: Integer;
    OHang: Integer;

    function FindSpaces: integer;
    var
      I: integer;
    begin
    Result := 0;
    for I := 0 to NN-2 do  {-2 so as not to count end spaces}
      if ((PStart+I)^ = ' ') or ((PStart+I)^ = #160) then
        Inc(Result);
    end;

  begin
  DHt := 0;    {for the fonts on this line get the maximum height}
  Cnt := 0;
  Desc := 0;
  P := PStart;
  if (NN = 1) and (P^ = BrkCh) then
    NoChar := False
  else
    begin
    NoChar := True;
    for I := 0 to NN-1 do
      begin
      if (not (P^ in [FmCtl, ImgPan, BrkCh])) {ignore images and space on end}
              and (not ((P = Last) and (Last^ = ' '))) then
        begin   {check for the no character case}
        NoChar := False;
        Break;
        end;
      Inc(P);
      end;
    end;

  if not NoChar then
    repeat
      FO := Fonts.GetFontObjAt(PStart-Buff+Cnt, Index);
      Tmp := FO.GetHeight(Desc);
      DHt := IntMax(DHt, Tmp);
      LR.Descent := IntMax(LR.Descent, Desc);
      J := Fonts.GetFontCountAt(PStart-Buff+Cnt, Len);
      Inc(Cnt, J);
    until Cnt >= NN;

  SB := 0;  {if there are images, then maybe they add extra space}
  SA := 0;  {space before and after}
  if LineHeight >= 0 then
    begin
    SB := (LineHeight-DHt) div 2;
    SA := (LineHeight-DHt) - SB;
    end;
  Cnt := 0;
  repeat
    Cnt := Cnt + Images.GetImageCountAt(PStart-Buff+Cnt);
    if Cnt < NN then
      begin
      H := Images.GetHeightAt(PStart-Buff+Cnt, Align, FlObj);
      FlObj.DrawYY := Y;     {approx y dimension}
      if (FLObj is TImageObj) and Assigned(TImageObj(FLObj).MyFormControl) then
         TImageObj(FLObj).MyFormControl.FYValue := Y;
      case Align of
        ATop: SA := IntMax(SA, H - DHt);
        AMiddle:
             begin
             if DHt = 0 then
               begin
               DHt := Fonts.GetFontObjAt(PStart-Buff, Index).GetHeight(Desc);
               LR.Descent := Desc;
               end;
             Tmp := (H - DHt) div 2;
             SA := IntMax(SA, Tmp);
             SB := IntMax(SB, (H - DHt - Tmp));
             end;
        ABottom, ABaseline: SB := IntMax(SB, H - (DHt - LR.Descent));
        end;
      end;
    Inc(Cnt);  {to skip by the image}
  until Cnt >= NN;

  Cnt := 0;   {now check on form controls}
  repeat
    Cnt := Cnt + TFormControlList(FormControls).GetControlCountAt(PStart-Buff+Cnt);
    if Cnt < NN then
      begin
      FCO := TFormControlList(FormControls).FindControl(PStart-Buff+Cnt);
      H := TFormControlList(FormControls).GetHeightAt(PStart-Buff+Cnt, FormAlign);
      case FormAlign of
        ATop:
          SA := IntMax(SA, H+FCO.VSpaceB+FCO.VSpaceT-Dht);
        AMiddle:
          begin
          Tmp := (H - DHt) div 2;
          SA := IntMax(SA, Tmp+FCO.VSpaceB);
          SB := IntMax(SB, (H - DHt - Tmp+FCO.VSpaceT));
          end;
        ABaseline:
          SB := IntMax(SB, H+FCO.VSpaceT+FCO.VSpaceB-(DHt-LR.Descent));
        ABottom:
          SB := IntMax(SB, H+FCO.VSpaceT+FCO.VSpaceB-DHt);
        end;
      if Assigned(FCO) and not ParentSectionList.IsCopy then
        FCO.FYValue := Y;
      end;
    Inc(Cnt);  {to skip by the control}
  until Cnt >= NN;

  {$ifndef NoTabLink}
  if not ParentSectionList.IsCopy then
    begin
    Cnt := 0;   {now check URLs}
    repeat
      FO := Fonts.GetFontObjAt(PStart-Buff+Cnt, Index);
      FO.AssignY(Y);
      Cnt := Cnt + Fonts.GetFontCountAt(PStart-Buff+Cnt, Len);
    until Cnt >= NN;
    end;
  {$endif}

  LR.Start := PStart;
  LR.LineHt := DHt;
  LR.Ln := NN;
  if Brk[PStart-Buff+NN] = 's' then     {see if there is a soft hyphen on the end}
    LR.Shy := True;
  TmpRt := IMgr.RightSide(Y);
  Tmp := IMgr.LeftIndent(Y);
  if PStart = Buff then
    Tmp := Tmp + FirstLineIndent;

  if Justify = Left then
    LR.LineIndent := Tmp-X
  else
    begin
    TextWidth:= FindTextWidth(Canvas, PStart, NN, True);
    if LR.Shy then
      begin  {take into account the width of the hyphen}
      Fonts.GetFontAt(PStart-Buff+NN-1, OHang).AssignToCanvas(Canvas);
      Inc(TextWidth, Canvas.TextWidth('-'));
      end;
    if Justify = Centered then
      LR.LineIndent := (TmpRt + Tmp - TextWidth) div 2 -X
    else if Justify = Right then
      LR.LineIndent := TmpRt - X - TextWidth
    else
      begin    {Justify = FullJustify}
      LR.LineIndent := Tmp-X;
      if not Finished then
        begin
        LR.Extra := TmpRt - Tmp - TextWidth;
        LR.Spaces := FindSpaces;
        end;
      end;
    end;
  LR.DrawWidth := TmpRt-Tmp;
  LR.SpaceBefore := LR.SpaceBefore + SB;
  LR.SpaceAfter := SA;
  Lines.Add(LR);
  Inc(PStart, NN);
  SectionHeight := SectionHeight +DHt + SA + LR.SpaceBefore;
  Tmp := DHt +SA + SB;
  Inc(Y, Tmp);
  LR.LineImgHt := IntMax(Tmp, ImgHt);
  for I := 0 to NxImages.Count-1 do
    begin
    IMgr.Update(Y, TFloatingObj(NxImages[I]));  {update Image manager and Image}
    {include images in Line height}
    with TFloatingObj(NxImages[I]) do
      Tmp1 := ImageHeight + VSpaceT + VSpaceB;
    LR.LineImgHt := IntMax(LR.LineImgHt, Tmp+Tmp1);
    AccumImgBot := IntMax(AccumImgBot, Y + Tmp1);
    end;
  NxImages.Clear;
  end;

begin         {TSection.DrawLogic}
YDraw := Y;
AccumImgBot := 0;
TopY := Y;
ContentTop := Y;
DrawTop := Y;
StartCurs := Curs;
PStart := Buff;
Last := Buff + Len - 1;
SectionHeight := 0;
Lines.Clear;
if (Len = 0) then
  begin
  Result := GetClearSpace(ClearAttr);
  DrawHeight := Result;
  SectionHeight := Result;
  ContentBot := Y+Result;
  DrawBot := ContentBot;
  MaxWidth := 0;
  DrawWidth := 0;
  Exit;
  end;
if FLPercent <> 0 then
  FirstLineIndent := (FLPercent * AWidth) div 100;  {percentage calculated}
Finished := False;
DrawWidth := IMgr.RightSide(Y) - X;
Width := IntMin(IMgr.RightSide(Y)-IMgr.LeftIndent(Y), AWidth);
MaxWidth := Width;
if AHeight = 0 then
  HtRef := BlHt
else HtRef := AHeight;
for I := 0 to Images.Count-1 do     {call drawlogic for all the images}
  begin
  Obj := TFloatingObj(Images[I]);
  Obj.DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Obj.Pos, Indx), Width, HtRef);
  MaxWidth := IntMax(MaxWidth, Obj.ImageWidth);  {HScrollBar for wide images}
  end;
for I := 0 to FormControls.Count-1 do
  with TFormControlObj(FormControls[I]) do
    if Assigned(FControl) then
      begin
      if PercentWidth then
        FControl.Width := IntMax(10, IntMin(MulDiv(FWidth, Width, 100), Width-HSpaceL-HSpaceR));
      MaxWidth := IntMax(MaxWidth, FControl.Width);
      end;
NxImages := TList.Create;
while not Finished do
  begin
  Max := Last - PStart + 1;
  if Max <= 0 then Break;
  LR := LineRec.Create(ParentSectionList);     {a new line}
  if (Lines.Count = 0) then
    begin  {may need to move down past floating image}
    Tmp := GetClearSpace(ClearAttr);
    if Tmp > 0 then
      begin
      LR.LineHt := Tmp;
      Inc(SectionHeight, Tmp);
      LR.Ln := 0;
      LR.Start := PStart;
      Inc(Y, Tmp);
      Lines.Add(LR);
      LR := LineRec.Create(ParentSectionList);
      end;
    end;

  ImgHt := 0;
  NN := 0;
  if (Self is TPreformated) and not BreakWord then
    N := Max
  else
    begin
    NN := FindCountThatFits1(Canvas, PStart, Max, X, Y, IMgr, ImgHt, NxImages);
    N := IntMax(NN, 1);   {N = at least 1}
    end;

  AccumImgBot := IntMax(AccumImgBot, Y+ImgHt);
  if NN = 0 then   {if nothing fits, see if we can move down}
    Tmp := IMgr.GetNextWiderY(Y) - Y
  else Tmp := 0;
  if Tmp > 0 then
    begin      {move down where it's wider}
    LR.LineHt := Tmp;
    Inc(SectionHeight, Tmp);
    LR.Ln := 0;
    LR.Start := PStart;
    Inc(Y, Tmp);
    Lines.Add(LR);
    end        {else can't move down or don't have to}
  else if N = Max then
    begin   {Do the remainder}
    Finished := True;
    LineComplete(N);
    end
  else
    begin
    P := PStart + N -1;   {the last char that fits}
    if ((P^ in [WideChar(' '), FmCtl, ImgPan]) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n')
           or (P^ = BrkCh) then
      begin  {move past spaces so as not to print any on next line}
      while (N < Max) and ((P+1)^ = ' ') do
        begin
        Inc(P);
        Inc(N);
        end;
      Finished := N >= Max;
      LineComplete(N);
      end
    else if (N < Max) and ((P+1)^ = ' ') and (Brk[P - Buff + 2] <> 'n') then
      begin
      repeat
        Inc(N);         {pass the space}
        Inc(p);
      until (N >= Max) or ((P+1)^ <> ' ');
      Finished := N >= Max;
      LineComplete(N);
      end
    else if (N < Max) and ((P+1)^ in [FmCtl, ImgPan]) and (Brk[PStart-Buff+N] <> 'n') then {an image or control}
      begin
      Finished := False;
      LineComplete(N);
      end
    else
      begin {non space, wrap it by backing off to previous space or image}
      while ((not ((P^ in [WideChar(' '), WideChar('-'), WideChar('?'), FmCtl, ImgPan])
                   or WrapChar(P^) or WrapChar((P+1)^)) and not (Brk[P-Buff+1] in ['a', 's']))
             or ((Brk[P-Buff+1] = 'n'))) and (P > PStart) do
        Dec(P);
      if (P = PStart) and ((not (P^ in [FmCtl, ImgPan])) or (Brk[PStart-Buff+1] = 'n')) then
        begin {no space found, forget the wrap, write the whole word and any
               spaces found after it}
        if BreakWord then
          LineComplete(N)
        else
          begin
          P := PStart+N-1;

          while (P <> Last) and not (P^ in [WideChar('-'), WideChar('?')])
                      and not (Brk[P-Buff+1] in ['a', 's'])
                      and not (((P + 1)^ in [WideChar(' '), FmCtl, ImgPan, BrkCh]) or WrapChar((P+1)^))
                      or (Brk[P - Buff + 2] = 'n') do
            begin
            Inc(P);
            end;
          while (P <> Last) and ((P+1)^ = ' ') do
            begin
            Inc(P);
            end;
          if (P <> Last) and ((P+1)^ = BrkCh) then
            Inc(P);
          {Line is too long, add spacer line to where it's clear}
          Tmp := IMgr.GetNextWiderY(Y) - Y;
          if Tmp > 0 then
            begin
            LR.LineHt := Tmp;
            Inc(SectionHeight, Tmp);
            LR.Ln := 0;
            LR.Start := PStart;
            Inc(Y, Tmp);
            Lines.Add(LR);
            end
          else
            begin   {line is too long but do it anyway}
            MaxWidth := IntMax(MaxWidth, FindTextWidth(Canvas, PStart, P-PStart+1, True));
            Finished := P = Last;
            LineComplete(P-PStart+1);
            end;
          end
        end
      else
        begin  {found space}
        while (P+1)^ = ' ' do
          begin
          if P = Last then
            begin
            Inc(P);
            Dec(P);
            end;
          Inc(P);
          end;
        LineComplete(P-PStart+1);
        end;
      end;
    end;
  end;
NxImages.Free;
Curs := StartCurs + Len;

If Assigned(ParentSectionList.FirstLineHtPtr) and (Lines.Count > 0) then       {used for List items}
  with LineRec(Lines[0]) do
    if (ParentSectionList.FirstLineHtPtr^ = 0) then
      ParentSectionList.FirstLineHtPtr^ := YDraw + LineHt - Descent + SpaceBefore;

DrawHeight := AccumImgBot - TopY;  {in case image overhangs}
if DrawHeight < SectionHeight then
  DrawHeight := SectionHeight;
Result := SectionHeight;
ContentBot := TopY+SectionHeight;
DrawBot := TopY+DrawHeight;
with ParentSectionList do
  begin
  if not IsCopy and (SectionNumber mod 50 = 0) and (ThisCycle <> CycleNumber)
          and (SectionCount > 0) then
    ThtmlViewer(TheOwner).htProgress(ProgressStart + ((100-ProgressStart)*SectionNumber) div SectionCount);
  ThisCycle := CycleNumber;  {only once per cycle}
  end;
end;

{----------------TSection.CheckForInlines}
procedure TSection.CheckForInlines(LR: Linerec);
{called before each line is drawn the first time to check if there are any
 inline borders in the line}
var
  I: integer;
  BR: BorderRec;
  StartBI, EndBI, LineStart: integer;
begin
with LR do
  begin
  FirstDraw := False;  {this will turn it off if there is no inline border action in this line}
  with TInlineList(ParentSectionList.InlineList) do
    for I := 0 to Count-1 do    {look thru the inlinelist}
      begin
      StartBI := StartB[I];
      EndBI := EndB[I];
      LineStart := StartCurs + Start-Buff;  {offset from Section start to Line start}
      if (EndBI > LineStart) and (StartBI < LineStart +Ln) then
        begin        {it's in this line}
        if not Assigned(BorderList) then
          begin
          BorderList := TFreeList.Create;
          FirstDraw := True;   {there will be more processing needed}
          end;
        BR := BorderRec.Create;
        BorderList.Add(BR);
        with BR do
          begin
          BR.MargArray := InlineRec(ParentSectionList.InlineList.Items[I]).MargArray;  {get border data}
          if StartBI < LineStart then
            begin
            OpenStart := True;  {continuation of border on line above, end is open}
            BStart := Start-Buff;  {start of this line}
            end
          else
            begin
            OpenStart := False;
            BStart := StartBI - StartCurs;  {start is in this line}
            end;
          if EndBI > LineStart + Ln  then
            begin
            OpenEnd := True;   {will continue on next line, end is open}
            BEnd := Start-Buff +Ln;
            end
          else
            begin
            OpenEnd := False;
            BEnd := EndBI - StartCurs;   {end is in this line}
            end;
          end;
        end;
      end;
  end;
end;

{----------------TSection.Draw}
function TSection.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  I: integer;
  MySelB, MySelE: integer;
  DC: HDC;
  Ctrl: TFormControlObj;
  YOffset, Y, Desc: integer;

  procedure DrawTheText(LineNo: integer);
  var
    I, J, J1, J2, J3, J4, Index, Addon, TopP, BottomP, LeftT, Tmp, K : integer;
    Obj: TFloatingObj;
    FO: TFontObj;
    ARect: TRect;
    Inverted, ImageAtStart, NewCP: boolean;
    Color: TColor;
    CP1: TPoint;
    CPx, CPy, CP1x: integer;
    SaveColor: TColor;
    BR: BorderRec;
    LR:LineRec;
    Start: PWideChar;
    Cnt, Descent: integer;
    St: WideString;

    function AddHyphen(P: PWideChar; N: integer): WideString;
    var
      I: integer;
    begin
    SetLength(Result, N+1);
    for I := 1 to N do
      Result[I] := P[I-1];
    Result[N+1] := WideChar('-');
    end;

    function ChkInversion(Start: PWideChar; var Count: Integer): boolean;
    var
      LongCount, C: integer;
    begin
    Result := False;
    C := Start-Buff;
    Count := 32000;
    if ParentSectionList.IsCopy then Exit;
    if (MySelE < MySelB) or ((MySelE = MySelB) and
                 not ParentSectionList.ShowDummyCaret) then
      Exit;
    if (MySelB <= C) and (MySelE > C) then
      begin
      Result := True;
      LongCount := MySelE - C;
      end
    else if MySelB > C then LongCount := MySelB - C
    else LongCount := 32000;
    if LongCount > 32000 then Count := 32000
      else Count := LongCount;
    end;

  begin  {Y is at bottom of line here}
  LR := LineRec(Lines[LineNo]);
  Start := LR.Start;
  Cnt := LR.Ln;
  Descent := LR.Descent;

  NewCP := True;
  ImageAtStart := True;
  CPx := X + LR.LineIndent;
  CP1x := CPx;
  LR.DrawY := Y-LR.LineHt;
  LR.DrawXX := CPx;
  while Cnt > 0 do
    begin
    I := 1;
    J1 := Fonts.GetFontCountAt(Start-Buff, Len)-1;
    J2 := Images.GetImageCountAt(Start-Buff)-1;
    J4 := TFormControlList(FormControls).GetControlCountAt(Start-Buff)-1;
    FO := Fonts.GetFontObjAt(Start-Buff, Index);

    {if an inline border, find it's boundaries}
    if LR.FirstDraw and Assigned(LR.BorderList) then
      for K := 0 to LR.BorderList.Count-1 do  {may be several inline borders}
        begin
        BR := BorderRec(LR.BorderList.Items[K]);
        if (Start-Buff = BR.BStart) then
          begin   {this is it's start}
          BR.bRect.Top := Y-FO.GetHeight(Desc)-Descent+Desc+1;
          BR.bRect.Left := CPx;
          BR.bRect.Bottom := Y-Descent+Desc;
          end
        else if (Start-Buff = BR.BEnd) and (BR.bRect.Right = 0) then
          BR.bRect.Right := CPx  {this is it's end}
        else if (Start-Buff > BR.BStart) and (Start-Buff < BR.BEnd) then
          begin {this is position within boundary, it's top or bottom may enlarge}
          BR.bRect.Top := IntMin(BR.bRect.Top, Y-FO.GetHeight(Desc)-Descent+Desc+1);
          BR.bRect.Bottom := IntMax(BR.bRect.Bottom, Y-Descent+Desc);
          end;
        end;

    FO.TheFont.AssignToCanvas(Canvas);
    if J2 = -1 then
      begin  {it's an image or panel}
      Obj := Images.FindImage(Start-Buff);
      if Obj is TImageObj then
        begin
        if Obj.ObjAlign in [ALeft, ARight] then
          begin
          if ImageAtStart then
            begin
            ParentSectionList.DrawList.AddImage(TImageObj(Obj), Canvas, IMgr.LfEdge+Obj.Indent,
                 Y-LR.LineHt-LR.SpaceBefore, Y-Descent, FO);
            end
          else
            begin {if not at start, draw on next line}
            ParentSectionList.DrawList.AddImage(TImageObj(Obj), Canvas, IMgr.LfEdge+Obj.Indent, Y, Y-Descent, FO);
            end;
          {if a boundary is on a floating image, remove it}
          if LR.FirstDraw and Assigned(LR.BorderList) then
            for K := LR.BorderList.Count-1 downto 0 do
              begin
              BR := BorderRec(LR.BorderList.Items[K]);
              if (Start-Buff = BR.BStart) and (BR.BEnd = BR.BStart+1) then
                begin
                LR.BorderList.Delete(K);
                BR.Free;
                end;
              end;
          end
        else
          begin
          SetTextJustification(Canvas.Handle, 0, 0);
          if Assigned(MyBlock) then
            TImageObj(Obj).Positioning := MyBlock.Positioning
          else TImageObj(Obj).Positioning := posStatic;
          TImageObj(Obj).Draw(Canvas, CPx+Obj.HSpaceL, Y-LR.LineHt, Y-Descent, FO);
          {see if there's an inline border for the image}
          if LR.FirstDraw and Assigned(LR.BorderList) then
            for K := 0 to LR.BorderList.Count-1 do
              begin
              BR := BorderRec(LR.BorderList.Items[K]);
              if (Start-Buff >= BR.BStart) and (Start-Buff <= BR.BEnd) then
                begin  {there is a border here, find the image dimensions}
                with TImageObj(Obj) do
                  case ObjAlign of
                    ATop:
                      begin
                      TopP := Y-LR.LineHt+VSpaceT;
                      BottomP := Y-LR.LineHT+ImageHeight+VSpaceT;
                      end;
                    AMiddle:
                      begin
                      TopP := Y-Descent+FO.Descent-(FO.tmHeight div 2)-((ImageHeight-VSpaceT+VSpaceB) div 2);
                      BottomP := Y-Descent+FO.Descent-(FO.tmHeight div 2)-((ImageHeight-VSpaceT+VSpaceB) div 2)+ImageHeight;
                      end;
                    ABottom, ABaseline:
                      begin
                      TopP := Y-Descent-Obj.ImageHeight-VSpaceB;
                      BottomP := Y-Descent-VSpaceB;
                      end;
                    else
                      begin
                      TopP := 0;   {to eliminate warning msg}
                      BottomP := 0;
                      end;
                    end;
                if (Start-Buff = BR.BStart) then
                  begin  {border starts at image}
                  BR.bRect.Top := ToPP;
                  BR.bRect.Left := CPx + TImageObj(Obj).HSpaceL;
                  if BR.BEnd = BR.BStart+1 then {border ends with image also, rt side set by image width}
                    BR.bRect.Right := BR.bRect.Left+TImageObj(Obj).ImageWidth;
                  BR.bRect.Bottom := BottomP;
                  end
                else if Start-Buff = BR.BEnd then
                else
                  begin {image is included in border and may effect the border top and bottom}
                  BR.bRect.Top := IntMin(BR.bRect.Top, ToPP);
                  BR.bRect.Bottom := IntMax(BR.bRect.Bottom, BottomP);
                  end;
                end;
              end;
          CPx := CPx + Obj.ImageWidth + Obj.HSpaceL + Obj.HSpaceR;
          NewCP := True;
          ImageAtStart := False;
          end;
        end
      else
        begin  {it's a Panel}
        with TPanelObj(Obj) do
          begin
          ShowIt := True;
          if (Obj.ObjAlign in [ALeft, ARight]) then
            begin
            LeftT := IMgr.LfEdge+Obj.Indent;
            if ImageAtStart then
              TopP := Y-LR.LineHt-LR.SpaceBefore-YOffset+VSpaceT
            else
              TopP := Y-YOffset+VSpaceT;
            {check for border.  For floating panel, remove it}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := LR.BorderList.Count-1 downto 0 do
                begin
                BR := BorderRec(LR.BorderList.Items[K]);
                if (Start-Buff = BR.BStart) and (BR.BEnd = BR.BStart+1) then
                  begin
                  LR.BorderList.Delete(K);
                  BR.Free;
                  end;
                end;
            end
          else
            begin
            LeftT := CPx+Obj.HSpaceL;
            case Obj.ObjAlign of
              ATop: TopP := Y-YOffset-LR.LineHt+Obj.VSpaceT;
              AMiddle:  TopP := Y-YOffset - FO.tmHeight div 2 - (ImageHeight-Obj.VSpaceT+Obj.VSpaceB) div 2;
              ABottom, ABaseline:  TopP := Y-YOffset-ImageHeight-Descent-Obj.VSpaceB;
              else TopP := 0;   {to eliminate warning msg}
              end;
            {Check for border on inline panel}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := 0 to LR.BorderList.Count-1 do
                begin
                BR := BorderRec(LR.BorderList.Items[K]);
                if (Start-Buff >= BR.BStart) and (Start-Buff <= BR.BEnd) then
                  begin
                  if (Start-Buff = BR.BStart) then
                    begin {border starts on panel}
                    BR.bRect.Top := ToPP+YOffSet;
                    BR.bRect.Left := CPx + HSpaceL;
                    if BR.BEnd = BR.BStart+1 then  {border also ends with panel}
                      BR.bRect.Right := BR.bRect.Left+ImageWidth;
                    BR.bRect.Bottom := TopP+YOffSet+ImageHeight;
                    end
                  else if Start-Buff = BR.BEnd then
                  else
                    begin {Panel is included in border, may effect top and bottom}
                    BR.bRect.Top := IntMin(BR.bRect.Top, ToPP+YOffSet);
                    BR.bRect.Bottom := IntMax(BR.bRect.Bottom, TopP+YOffSet+ImageHeight);
                    end;
                  end;
                end;
            Inc(CPx, ImageWidth+Obj.HSpaceL+Obj.HSpaceR);
            NewCP := True;
            ImageAtStart := False;
            end;
          if ParentSectionList.IsCopy then
            TPanelObj(Obj).Draw(Canvas, LeftT, TopP)
          else
            begin
            Panel.Top := TopP;
            Panel.Left := LeftT;
            if ThvPanel(Panel).FVisible then
              Panel.Show
            else Panel.Hide;
            end;
          DrawXX := LeftT;
          end;
        end;
      end
    else if J4 = -1 then
      begin  {it's a form control}
      Ctrl := TFormControlList(FormControls).FindControl(Start-Buff);
      if Assigned(Ctrl.FControl) then
        with Ctrl, FControl do
          begin
          ShowIt := True;
          case FormAlign of
             ATop:
               TopP := LR.DrawY+VSpaceT - YOffset;
             AMiddle:
               TopP := Y - ((LR.LineHt+Height) div 2) - YOffset;
             ABaseline:
               TopP := Y - Height-VSpaceB - Descent -YOffset; {sits on baseline}
             ABottom:
               TopP := Y-Height-VSpaceB-YOffset;
             else TopP := Y;    {never get here}
             end;
          if FControl is TRadioButton then
            Inc(Topp, 2)
          else if FControl is TCheckbox then
            Inc(Topp, 1);
          {Check for border}
          if LR.FirstDraw and Assigned(LR.BorderList) then
            for K := 0 to LR.BorderList.Count-1 do
              begin
              BR := BorderRec(LR.BorderList.Items[K]);
              if (Start-Buff >= BR.BStart) and (Start-Buff <= BR.BEnd) then
                begin
                if (Start-Buff = BR.BStart) then
                  begin {border starts with Form control}
                  BR.bRect.Top := ToPP+YOffSet;
                  BR.bRect.Left := CPx + HSpaceL;
                  if BR.BEnd = BR.BStart+1 then  {border is confined to form control}
                    BR.bRect.Right := BR.bRect.Left+Width;
                  BR.bRect.Bottom := TopP+YOffSet+Height;
                  end
                else if Start-Buff = BR.BEnd then
                else
                  begin {form control is included in border}
                  BR.bRect.Top := IntMin(BR.bRect.Top, ToPP+YOffSet);
                  BR.bRect.Bottom := IntMax(BR.bRect.Bottom, TopP+YOffSet+Height);
                  end;
                end;
              end;
          if ParentSectionList.IsCopy then
            Ctrl.Draw(Canvas, CPx+Ctrl.HSpaceL, TopP)
          else
            begin
            Show;
            Top := TopP;
            Left := CPx+Ctrl.HSpaceL;
            if Ctrl is TRadioButtonFormControlObj then
              with  TRadioButtonFormControlObj(Ctrl) do
                begin
                TRadioButtonFormControlObj(Ctrl).TheControl.Show;
                if MyCell.BkGnd then
                  (TheControl as TFormRadioButton).Color := MyCell.BkColor
                else (TheControl as TFormRadioButton).Color := ParentSectionList.Background;
                end;
            if Ctrl.Active and ((Ctrl is TRadioButtonFormControlObj) or
                   (Ctrl is TCheckBoxFormControlObj)) then
                begin
                Canvas.Brush.Color := clWhite;
                SaveColor := SetTextColor(Handle, clBlack);
                if (Ctrl is TRadioButtonFormControlObj) then
                  begin
                  if Screen.PixelsPerInch > 100 then
                    Canvas.DrawFocusRect(Rect(Left-2, Top-2, Left+18, Top+18))
                  else
                    Canvas.DrawFocusRect(Rect(Left-3, Top-2, Left+16, Top+16));
                  end
                else
                  Canvas.DrawFocusRect(Rect(Left-3, Top-3, Left+16, Top+16));
                SetTextColor(Handle, SaveColor);
                end;
            end;
          Inc(CPx, Width+Ctrl.HSpaceL+Ctrl.HSpaceR);
          NewCP := True;
          end;
      ImageAtStart := False;
      end
    else
      begin
      J := IntMin(J1, J2);
      J := IntMin(J, J4);
      Inverted := ChkInversion(Start, J3);
      J := IntMin(J, J3-1);
      I := IntMin(Cnt, J+1);

      if Inverted then
        begin
        SetBkMode(Canvas.Handle, Opaque);
        Canvas.Brush.Color := Canvas.Font.Color;
        if FO.TheFont.bgColor = clNone then
          begin
          Color := Canvas.Font.Color;
          if Color and $80000000 = $80000000 then
            Color := GetSysColor(Color and $FFFFFF)
          else Color := Color and $FFFFFF;
          Canvas.Font.Color := Color xor $FFFFFF;
          end
        else Canvas.Font.Color := FO.TheFont.bgColor;
        end
      else if FO.TheFont.BGColor = clNone then
        begin
        SetBkMode(Canvas.Handle, Transparent);
        Canvas.Brush.Style := bsClear;
        end
      else
        begin
        SetBkMode(Canvas.Handle, Opaque);
        Canvas.Brush.Style := bsClear;
        Canvas.Brush.Color := FO.TheFont.BGColor;
        end;

      if ParentSectionList.Printing then
        begin
        if ParentSectionList.PrintMonoBlack and
              (GetDeviceCaps(Canvas.Handle, NumColors) in [0..2]) then
          begin
          Color := Canvas.Font.Color;
          if Color and $80000000 = $80000000 then
            Color := GetSysColor(Color);
          if Color and $ffffff <> $ffffff then
            Canvas.Font.Color := clBlack;     {Print black}
          end;
        if not ParentSectionlist.PrintTableBackground then
          begin
          Color := Canvas.Font.Color;
          if Color and $80000000 = $80000000 then
            Color := GetSysColor(Color);
          if (Color and $E0E0 = $E0E0) then
            Canvas.Font.Color := $2A0A0A0;   {too near white or yellow, make it gray}
          end;
        end;

      SetTextAlign(Canvas.Handle, TA_BaseLine);
      {figure any offset for subscript or superscript}
      with FO do
        if SScript = ANone then Addon := 0
          else if SScript = ASuper then Addon := -(FontHeight div 3)
          else Addon := Descent div 2 +1;
      NewCP := NewCP or (Addon <> 0);
      {calc a new CP if required}
      if NewCP then
        begin
        CPy := Y - Descent + Addon - YOffset;
        NewCP := Addon <> 0;
        end;

      if not ParentSectionList.NoOutput then
        begin
        if (Cnt - I <= 0) and ((Start + I - 1)^ in [WideChar(' '), WideChar(BrkCh)]) then
          Tmp := I-1   {at end of line, don't show space or break}
        else Tmp := I;
        if (Self is TPreformated) and not MyBlock.HideOverflow then
          begin   {so will clip in Table cells}
          ARect := Rect(IMgr.LfEdge, Y-LR.LineHt-LR.SpaceBefore-YOffset, X+IMgr.ClipWidth, Y-YOffset+1);
          ExtTextOutW(Canvas.Handle, CPx, CPy, ETO_CLIPPED, @ARect, Start, Tmp, nil);
          CP1x := CPx+GetXExtent(Canvas.Handle, Start, Tmp);
          end
        else
          begin
          if LR.Spaces = 0 then
            SetTextJustification(Canvas.Handle, 0, 0)
          else
            SetTextJustification(Canvas.Handle, LR.Extra, LR.Spaces);
          if not IsWin95 then    {use TextOutW}
            begin
            if (Cnt - I <= 0) and LR.Shy then
              begin
              St := AddHyphen(Start, Tmp);
              TextOutW(Canvas.Handle, CPx, CPy, PWideChar(St), Length(St));
              CP1x := CPx+GetXExtent(Canvas.Handle, PWideChar(St), Length(St));
              end
            else
              begin
              TextOutW(Canvas.Handle, CPx, CPy, Start, Tmp);
              CP1x := CPx+GetXExtent(Canvas.Handle, Start, Tmp);
              end
            end
          else
            begin  {Win95}
            {Win95 has bug which extends text underline for proportional font in TextOutW.
             Use clipping to clip the extra underline.}
            CP1x := CPx+GetXExtent(Canvas.Handle, Start, Tmp);
            ARect := Rect(CPx, Y-LR.LineHt-LR.SpaceBefore-YOffset, CP1x, Y-YOffset+1);
            ExtTextOutW(Canvas.Handle, CPx, CPy, ETO_CLIPPED, @ARect, Start, Tmp, nil)
            end;
          end;
        {Put in a dummy caret to show character position}
        if ParentSectionList.ShowDummyCaret and not Inverted
                                    and (MySelB = Start-Buff) then
          begin
          Canvas.Pen.Color := Canvas.Font.Color;
          Tmp := Y - Descent+ FO.Descent + Addon - YOffset;
          Canvas.Brush.Color := clWhite;
          Canvas.Rectangle(CPx, Tmp, CPx+1, Tmp-FO.FontHeight);
          end;
      end;

      if FO.Active or ParentSectionList.IsCopy and Assigned(ParentSectionList.LinkDrawnEvent)
                             and (FO.UrlTarget.Url <> '') then
        begin
        Tmp := Y - Descent+ FO.Descent + Addon - YOffset;
        ARect := Rect(CPx, Tmp-FO.FontHeight, CP1x+1, Tmp);
        if FO.Active then
          begin
          Canvas.Font.Color := clBlack;  {black font needed for DrawFocusRect}
          DC := Canvas.Handle;    {Dummy call needed to make Delphi add font color change to handle}
          Canvas.DrawFocusRect(ARect);
          end;
        if Assigned(ParentSectionList.LinkDrawnEvent) then
          ParentSectionList.LinkDrawnEvent(ParentSectionList.TheOwner, ParentSectionList.LinkPage,
               FO.UrlTarget.Url, FO.UrlTarget.Target, ARect);
        end;
      CPx := CP1x;

      {the following puts a dummy caret at the very end of text if it should be there}
      if ParentSectionList.ShowDummyCaret and not Inverted
                and ((MySelB = Len) and (ParentSectionList.Selb = ParentSectionList.Len))
                and (Cnt = I) and (LineNo = Lines.Count-1) then
        begin
        Canvas.Pen.Color := Canvas.Font.Color;
        Tmp := Y - Descent+ FO.Descent + Addon - YOffset;
        Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(CPx, Tmp, CPx+1, Tmp-FO.FontHeight);
        end;

      ImageAtStart := False;
      end;
    Dec(Cnt, I);
    Inc(Start, I);
    end;
  SetTextJustification(Canvas.Handle, 0, 0);
  {at the end of this line.  see if there are open borders which need right side set}
  if LR.FirstDraw and Assigned(LR.BorderList) then
    for K := 0 to LR.BorderList.Count-1 do
      begin
      BR := BorderRec(LR.BorderList.Items[K]);
      if BR.OpenEnd or (BR.BRect.Right = 0) then
        BR.BRect.Right := CPx;
      end;
  end;

  procedure DoDraw(I: integer);
  {draw the Ith line in this section}
  var
    BR: BorderRec;
    K: integer;
    XOffset: integer;
  begin
  with LineRec(Lines[I]) do
    begin
    Inc(Y, LineHt+SpaceBefore);
    if FirstDraw then
      begin {see if any inline borders in this line}
      CheckForInlines(LineRec(Lines[I]));
      if FirstDraw then  {if there are, need a first pass to get boundaries}
        begin
        FirstX := X;
        DrawTheText(I);
        end;
      end;
    XOffset := X-FirstX;
    FirstDraw := False;
    if Assigned(BorderList) then  {draw any borders found in this line}
      for K := 0 to BorderList.Count-1 do
        begin
        BR := BorderRec(BorderList.Items[K]);
        BR.DrawTheBorder(Canvas, XOffset, YOffSet, ParentSectionList.Printing);
        end;
    DrawTheText(I);  {draw the text, etc., in this line}
    Inc(Y, SpaceAfter);
    end;
  ParentSectionList.FirstPageItem := False;
  end;

begin       {TSection.Draw}
Y := YDraw;
Result := Y + SectionHeight;
YOffset := ParentSectionList.YOff;

{Only draw if will be in display rectangle}
if (Len > 0) and (Y-YOffset+DrawHeight+40 >= ARect.Top)
        and (Y-YOffset-40 < ARect.Bottom) then
  begin
  DC := Canvas.Handle;
  SetTextAlign(DC, TA_BaseLine);

  MySelB := ParentSectionList.SelB-StartCurs;
  MySelE := ParentSectionList.SelE-StartCurs;
  for I := 0 to Lines.Count-1 do
    with ParentSectionList do
      if Printing then
          with LineRec(Lines[I]) do
            begin
            if (Y + LineImgHt <= PageBottom) then
              begin
              if(Y + LineImgHt - 1> ARect.Top+YOffSet) then
                DoDraw(I)
              else Inc(Y, SpaceBefore + LineHt + SpaceAfter);
              end
            else if (LineImgHt >= ARect.Bottom - ARect.Top) or PageShortened then
              DoDraw(I)
            else
              begin
              if Assigned(MyBlock) and (MyBlock.Positioning = PosAbsolute) then
                DoDraw(I)
              else if Y < PageBottom then
                  PageBottom := Y;  {Dont' print, don't want partial line}
              end;
            end
      else
        with LineRec(Lines[I]) do
          if ((Y-YOffset+LineImgHt+40 >= ARect.Top) and (Y-YOffset-40 < ARect.Bottom)) then
            DoDraw(I)
          else  {do not completely draw extremely long paragraphs}
            Inc(Y, SpaceBefore + LineHt + SpaceAfter);
  end;
end;

{----------------TSection.CopyToClipboard}
procedure TSection.CopyToClipboard;
var
  I, Strt, X1, X2: integer;
  MySelB, MySelE: integer;
begin
MySelB := ParentSectionList.SelB - StartCurs;
MySelE := ParentSectionList.SelE - StartCurs;
for I := 0 to Lines.Count-1 do
  with LineRec(Lines.Items[I]) do
    begin
    Strt := Start-Buff;
    if (MySelE <= Strt) or (MySelB > Strt + Ln) then Continue;
    if MySelB-Strt > 0 then X1 := MySelB-Strt
      else X1 := 0;
    if MySelE-Strt < Ln then X2 := MySelE - Strt
      else X2 := Ln;
    if (I = Lines.Count-1) and (X2 = Ln) then
      Dec(X2);
    ParentSectionList.CB.AddText(Start+X1, X2-X1);
    end;
if MySelE > Len then
  ParentSectionList.CB.AddTextCR('', 0);
end;

{----------------TSection.PtInObject}
function TSection.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
{Y is distance from start of section}
begin
Result := (Images.Count > 0) and Images.PtInObject(X, Y, Obj, IX, IY);
end;

{----------------TSection.GetURL}
function TSection.GetURL(Canvas: TCanvas; X: integer; Y: integer;
          var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
          var ATitle: string): guResultType;
 {Y is absolute}
var
  I, L, Index, Width, IX, IY, Posn: integer;
  FO : TFontObj;
  LR: LineRec;
  IMap, UMap: boolean;
  MapItem: TMapItem;
  ImageObj: TImageObj;
  Tmp: string;

  function MakeCopy(UrlTarget: TUrlTarget): TUrlTarget;
  begin
  Result := TUrlTarget.Create;
  Result.Copy(UrlTarget);
  end;

begin
Result := [];
{First, check to see if in an image}
if (Images.Count > 0) and
    Images.PtInImage(X, Y, IX, IY, Posn, IMap, UMap, MapItem, ImageObj) then
  begin
  if ImageObj.ImageTitle <> '' then
    begin
    ATitle := ImageObj.ImageTitle;
    Include(Result, guTitle);
    end
  else if ImageObj.FAlt <> '' then
    begin
    ATitle := ImageObj.FAlt;
    Include(Result, guTitle);
    end;
  ParentSectionList.ActiveImage := ImageObj;
  if Assigned(ImageObj.MyFormControl) then
    begin
    FormControl := ImageObj.MyFormControl;
    Include(Result, guControl);
    FormControl.XTmp := IX;
    FormControl.YTmp := IY;
    end
  else if UMap then
    begin
    if MapItem.GetURL(IX, IY, UrlTarg, Tmp) then
      begin
      Include(Result, guUrl);
      if Tmp <> '' then
        begin
        ATitle := Tmp;
        Include(Result, guTitle);
        end;
      end;
    end
  else
    begin
    FO := Fonts.GetFontObjAt(Posn, Index);
    if (FO.UrlTarget.Url <> '') then
      begin   {found an URL}
      Include(Result, guUrl);
      UrlTarg := MakeCopy(FO.UrlTarget);
      ParentSectionList.ActiveLink := FO;
      if IMap then
        UrlTarg.Url := UrlTarg.Url + '?'+IntToStr(IX)+','+IntToStr(IY);
      end;
    end;
  end
else
  begin
  I := 0;
  LR := Nil;
  with Lines do
    begin
    while I < Count do
      begin
      LR := LineRec(Lines[I]);
      if (Y > LR.DrawY) and (Y <= LR.DrawY+LR.LineHt) then
        Break;
      Inc(I);
      end;
    if I >= Count then Exit;
    end;
  with LR do
    begin
    if X < DrawXX then Exit;
    Width := X - DrawXX;
    if Spaces > 0 then
      SetTextJustification(Canvas.Handle, Extra, Spaces);
    L := FindCountThatFits(Canvas, Width, Start, Ln);
    if Spaces > 0 then
      SetTextJustification(Canvas.Handle, 0, 0);
    if L >= Ln then Exit;
    FO := Fonts.GetFontObjAt(L+(Start-Buff), Index);
    if (FO.UrlTarget.Url <> '') then {found an URL}
      if not ((Start+L)^ in [ImgPan]) then   {an image here would be in HSpace area}
        begin
        Include(Result, guUrl);
        UrlTarg := MakeCopy(FO.UrlTarget);
        ParentSectionList.ActiveLink := FO;
        end;
    if (FO.Title <> '') then  {found a Title}
      if not ((Start+L)^ in [ImgPan]) then   {an image here would be in HSpace area}
        begin
        ATitle := FO.Title;
        Include(Result, guTitle);
        end;
    end;
  end;
end;

{----------------TSection.FindCursor}
function TSection.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
{Given an X, Y, find the character position and the resulting XR, YR position
 for a caret along with its height, CaretHt.  Coordinates are relative to this
 section}
var
  I, H, L, Width, TotalHt, L1, W, Delta, OHang: integer;
  LR: LineRec;

begin
Result := -1;
I := 0;  H := ContentTop; L1 := 0;
LR := Nil;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      TotalHt := LineHt+SpaceBefore+SpaceAfter;
    if H+TotalHt > Y then Break;
    Inc(H, TotalHt);
    Inc(I);
    Inc(L1, LR.Ln);  {L1 accumulates char count of previous lines}
    end;
  if (I >= Count) then
    Exit;
  end;
with LR do
  begin
  if X > LR.DrawXX + LR.DrawWidth then
    Exit;
  if X < LR.DrawXX-10 then
    Exit;
  InText := True;
  CaretHt := LineHt;
  YR := H + SpaceBefore;
  if X < DrawXX then
    begin
    Result := L1+StartCurs;
    InText := False;
    Exit;
    end;
  Width := X-DrawXX;
  if (Justify = FullJustify) and (Spaces > 0) then
    SetTextJustification(Canvas.Handle, Extra, Spaces);
  L := FindCountThatFits(Canvas, Width, Start, Ln);
  W := FindTextWidth(Canvas, Start, L, False);
  XR := DrawXX + W;
  if L < Ln then
    begin   {check to see if passed 1/2 character mark}
    Fonts.GetFontAt(L1+L, OHang).AssignToCanvas(Canvas);
    Delta := FindTextWidthA(Canvas, Start+L, 1);
    if Width > W+(Delta div 2) then
      begin
      Inc(L);
      Inc(XR, Delta);
      end;
    end
  else InText := False;
  Result := L+L1+StartCurs;
  if Justify = FullJustify then
    SetTextJustification(Canvas.Handle, 0, 0);
  end;
end;

{----------------TSection.FindString}
function TSection.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
{find the first occurance of the string, ToFind, with a cursor value >= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  I: integer;
  ToSearch: WideString;

begin
Result := -1;
if (Len = 0) or (From >= StartCurs + Len) then Exit;
if From < StartCurs then I := 0
else I := From-StartCurs;

if MatchCase then
  ToSearch := BuffS
else ToSearch := WideLowerCase1(BuffS);  {ToFind already lower case}

P := StrPosW(PWideChar(ToSearch) + I, PWideChar(ToFind));
if Assigned(P) then
  Result := StartCurs+(P-PWideChar(ToSearch));
end;

{----------------TSection.FindStringR}
function TSection.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
{find the first occurance of the string, ToFind, with a cursor value <= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  ToFindLen: word;
  ToMatch, ToSearch: WideString;

begin
Result := -1;
if (Len = 0) or (From < StartCurs) then
  Exit;
ToFindLen := Length(ToFind);
if (Len < ToFindLen) or (From-StartCurs+1 < ToFindLen) then
  Exit;

if From >= StartCurs + Len then
  ToSearch := BuffS      {search all of BuffS}
else ToSearch := Copy(BuffS, 1, From-StartCurs);  {Search smaller part}
if not MatchCase then
  ToSearch := WideLowerCase1(ToSearch);  {ToFind already lower case}

{search backwards for the end char of ToFind}
P := StrRScanW(PWideChar(ToSearch), ToFind[ToFindLen]);
while Assigned(P) and (P-PWideChar(ToSearch)+1 >= ToFindLen) do
  begin
  {pick out a string of proper length from end char to see if it matches}
  SetString(ToMatch, P-ToFindLen+1, ToFindLen);
  if WideSameStr1(ToFind, ToMatch) then
    begin  {matches, return the cursor position}
    Result := StartCurs + (P - ToFindLen+1 - PWideChar(ToSearch));
    Exit;
    end;
  {doesn't match, shorten string to search for next search}
  ToSearch := Copy(ToSearch, 1, P-PWideChar(ToSearch));
  {and search backwards for end char again}
  P := StrRScanW(PWideChar(ToSearch), ToFind[ToFindLen]);
  end;
end;

{----------------TSection.FindSourcePos}
function TSection.FindSourcePos(DocPos: integer): integer;
var
  I: integer;
  IO: IndexObj;
begin
Result := -1;
if (Len = 0) or (DocPos >= StartCurs + Len) then Exit;

for I := SIndexList.Count-1 downto 0 do
  begin
  IO := PosIndex[I];
  if IO.Pos <= DocPos-StartCurs then
    begin
    Result := IO.Index + DocPos-StartCurs - IO.Pos;
    break;
    end;
  end;
end;

{----------------TSection.FindDocPos}
function TSection.FindDocPos(SourcePos: integer; Prev: boolean): integer;
{for a given Source position, find the nearest document position either Next or
 previous}
var
  I: integer;
  IO, IOPrev: IndexObj;
begin
Result := -1;
if Len = 0 then Exit;

if not Prev then
  begin
  I:= SIndexList.Count-1;
  IO := PosIndex[I];
  if SourcePos > IO.Index + (Len-1) - IO.Pos then Exit; {beyond this section}

  IOPrev := PosIndex[0];
  if SourcePos <= IOPrev.Index then
    begin  {in this section but before the start of Document text}
    Result := StartCurs;
    Exit;
    end;

  for I := 1 to SIndexList.Count-1 do
    begin
    IO := PosIndex[I];
    if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin   {between IOprev and IO}
      if SourcePos-IOPrev.Index+IOPrev.Pos < IO.Pos then
        Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index)
      else Result := StartCurs+IO.Pos;
      Exit;
      end;
    IOPrev := IO;
    end;
  {after the last IndexObj in list}
  Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index);
  end
else     {prev  -- we're iterating from the end of TSectionList}
  begin
  IOPrev := PosIndex[0];
  if SourcePos < IOPrev.Index then Exit;   {before this section}

  I:= SIndexList.Count-1;
  IO := PosIndex[I];
  if SourcePos > IO.Index + (Len-1) - IO.Pos then
    begin   {SourcePos is after the end of this section}
    Result := StartCurs + (Len-1);
    Exit;
    end;

  for I := 1 to SIndexList.Count-1 do
    begin
    IO := PosIndex[I];
    if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin {between IOprev and IO}
      if SourcePos-IOPrev.Index+IOPrev.Pos < IO.Pos then
        Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index)
      else Result := StartCurs+IO.Pos-1;
      Exit;
      end;
    IOPrev := IO;
    end;
  {after the last IndexObj in list}
  Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index);
  end;
end;

{----------------TSection.CursorToXY}
function TSection.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  I, Curs: integer;
  LR: LineRec;
begin
Result := False;
if (Len = 0) or (Cursor > StartCurs + Len) then Exit;

I := 0;
LR := Nil;
Curs := Cursor - StartCurs;
Y := ContentTop;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      begin
      if Curs < Ln then Break;
      Inc(Y, LineHt+SpaceBefore+SpaceAfter);
      Dec(Curs, Ln);
      end;
    Inc(I);
    end;
  if I >= Count then Exit;
  end;
if Assigned(Canvas) then
  begin
  if LR.Spaces > 0 then
    SetTextJustification(Canvas.Handle, LR.Extra, LR.Spaces);
  X := LR.DrawXX + FindTextWidth(Canvas, LR.Start, Curs, False);
  if LR.Spaces > 0 then
    SetTextJustification(Canvas.Handle, 0, 0);
  end
else X := LR.DrawXX;
Result := True;
end;

{----------------TSection.GetChAtPos}
function TSection.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
Result := False;
if (Len = 0) or (Pos < StartCurs) or (Pos >= StartCurs + Len) then Exit;
Ch := Buff[Pos-StartCurs];
Obj := Self;
Result := True;
end;

{----------------TPanelObj.Create}
constructor TPanelObj.Create(AMasterList: TSectionList; Position: integer;
               L: TAttributeList; ACell: TCellBasic; ObjectTag: boolean);
var
  PntPanel: TPaintPanel;
  I: integer;
  NewSpace: integer;
  S, Source, AName, AType: string;
begin
inherited Create;
fMasterList := AMasterList;
Pos := Position;
PntPanel := TPaintPanel(AMasterList.PPanel);
Panel := ThvPanel.Create(PntPanel);
Panel.Left := -4000;
Panel.Parent := PntPanel;
with ThvPanel(Panel) do
  begin
  FMyPanelObj := Self;
  Top := -4000;
  Height := 20;
  Width := 30;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  Color := clWhite;
  FVisible := True;
  Ctl3D := False;
  ParentCtl3D := False;
  ParentFont := False;
  ObjAlign := ABottom;   {default}
  NewSpace := -1;
  for I := 0 to L.Count-1 do
    with TAttribute(L[I]) do
      case Which of
        HeightSy:
          if System.Pos('%', Name) = 0 then
            begin
            SpecHeight := Intmax(1, Value); {spec ht of 0 becomes 1}
            Height := SpecHeight;   {so panels ht will be set for OnPanelCreate}
            end
          else if (Value > 0) and (Value <=100) then
            begin
            SpecHeight := Value;
            PercentHeight := True;
            end;
        WidthSy:
          if System.Pos('%', Name) = 0 then
            begin
            SpecWidth := Value;
            Width := Value;
            end
          else
            begin
            Value := IntMax(1, IntMin(Value, 100));
            SpecWidth := Value;
            PercentWidth := True;
            end;
        HSpaceSy:  NewSpace := IntMin(40, Abs(Value));
        VSpaceSy:  VSpaceT := IntMin(40, Abs(Value));
        SrcSy: Source := Name;
        NameSy:
          begin
          AName := Name;
          try
            Panel.Name := Name;
          except  {duplicate name will be ignored}
            end;
          end;
        AlignSy:
          begin
          S := UpperCase(Name);
          if S = 'TOP' then ObjAlign := ATop
          else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then ObjAlign := AMiddle
          else if S = 'LEFT' then ObjAlign := ALeft
          else if S = 'RIGHT' then ObjAlign := ARight;
          end;
        AltSy:
          begin
          FAlt := Name;
          while (Length(FAlt) > 0) and (FAlt[Length(FAlt)] in [#$D, #$A]) do
            Delete(FAlt, Length(FAlt), 1);
          ImageTitle := FAlt;    {use Alt as default Title}
          FAltW := MultibyteToWideString(CodePage, FAlt);
          end;
        TypeSy: AType := Name;
        end;
  if NewSpace >= 0 then
    HSpaceL := NewSpace
  else if ObjAlign in [ALeft, ARight] then
    HSpaceL := ImageSpace   {default}
  else HSpaceL := 0;

  HSpaceR := HSpaceL;
  VSpaceB := VSpaceT;
  Caption := '';
  if not ObjectTag and Assigned(AMasterList.PanelCreateEvent) then
    AMasterList.PanelCreateEvent(AMasterList.TheOwner, AName, AType,
        Source, ThvPanel(Panel));
  SetWidth := Width;
  SetHeight := Height;
  end;
AMasterList.PanelList.Add(Self);
end;

constructor TPanelObj.CreateCopy(AMasterList: TSectionList; T: TPanelObj);
begin
inherited CreateCopy(T);
Panel := ThvPanel.Create(Nil);
with T.Panel do
  Panel.SetBounds(Left, Top, Width, Height);
Panel.FVisible := T.Panel.FVisible;
Panel.Color := T.Panel.Color;
Panel.Parent := AMasterList.PPanel;
SpecWidth := T.SpecWidth;
PercentWidth := T.PercentWidth;
SpecHeight := T.SpecHeight;
PercentHeight := T.PercentHeight;
SetHeight := T.SetHeight;
SetWidth := T.SetWidth;
OPanel := T.Panel;   {save these for printing}
OSender := T.fMasterList.TheOwner;
PanelPrintEvent := T.fMasterList.PanelPrintEvent;
IsCopy := True;
end;

destructor TPanelObj.Destroy;
begin
if Assigned(fMasterList) and Assigned(fMasterList.PanelDestroyEvent) then
  fMasterList.PanelDestroyEvent(fMasterList.TheOwner, ThvPanel(Panel));
Panel.Free;
inherited Destroy;
end;

procedure TPanelObj.DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                            FO: TFontObj; AvailableWidth, AvailableHeight: integer);
begin
if not ImageKnown or PercentWidth or PercentHeight then
  begin
  if PercentWidth then
    begin
    ImageWidth := MulDiv(AvailableWidth, SpecWidth, 100);
    if SpecHeight <> 0 then
      if PercentHeight then
        ImageHeight := MulDiv(AvailableHeight, SpecHeight, 100)
      else ImageHeight := SpecHeight
    else ImageHeight := MulDiv(ImageWidth, SetHeight, SetWidth);
    end
  else if PercentHeight then
    begin
    ImageHeight := MulDiv(AvailableHeight, SpecHeight, 100);
    if SpecWidth <> 0 then
      ImageWidth := SpecWidth
    else ImageWidth := MulDiv(ImageHeight, SetWidth, SetHeight);
    end
  else if (SpecWidth <> 0) and (SpecHeight <> 0) then
    begin       {Both width and height specified}
    ImageHeight := SpecHeight;
    ImageWidth := SpecWidth;
    ImageKnown := True;
    end
  else if SpecHeight <> 0 then
    begin
    ImageHeight := SpecHeight;
    ImageWidth := MulDiv(SpecHeight, SetWidth, SetHeight);
    ImageKnown := True;
    end
  else if SpecWidth <> 0 then
    begin
    ImageWidth := SpecWidth;
    ImageHeight := MulDiv(SpecWidth, SetHeight, SetWidth);
    ImageKnown := True;
    end
  else
    begin       {neither height and width specified}
    ImageHeight := SetHeight;
    ImageWidth := SetWidth;
    ImageKnown := True;
    end;
  if not IsCopy then
    with Panel do
      if (ImageWidth > 0) and (ImageHeight > 0) then
        SetBounds(Left, Top, ImageWidth, ImageHeight);
  end;
end;

procedure TPanelObj.Draw(ACanvas: TCanvas; X1, Y1: integer);
var
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
  OldPenColor: TColor;
  Bitmap: TBitmap;
  OldHeight, OldWidth: integer;
  SaveFont :TFont;
begin
if Panel.FVisible then
  with ACanvas do
    if Assigned(PanelPrintEvent) then
      begin
      Bitmap := TBitmap.Create;
      OldHeight := Opanel.Height;
      OldWidth := Opanel.Width;
      try
        Bitmap.Height := ImageHeight;
        Bitmap.Width := ImageWidth;
        with Opanel do
          SetBounds(Left, Top, ImageWidth, ImageHeight);
        PanelPrintEvent(OSender, OPanel, Bitmap);
        PrintBitmap(ACanvas, X1, Y1, ImageWidth, ImageHeight, Bitmap.Handle);
      finally
        with Opanel do
          SetBounds(Left, Top, OldWidth, OldHeight);
        Bitmap.Free;
        end;
      end
    else
      begin
      OldBrushStyle := Brush.Style;   {save style first}
      OldBrushColor := Brush.Color;
      OldPenColor := Pen.Color;
      Pen.Color := clBlack;
      Brush.Color := Panel.Color;
      Brush.Style := bsSolid;

      ACanvas.Rectangle(X1, Y1, X1+ImageWidth, Y1+ImageHeight);
      SaveFont := TFont.Create;
      try
        SaveFont.Assign(ACanvas.Font);
        with ACanvas.Font do
          begin
          Size := 8;
          Name := 'Arial';
          end;
        WrapTextW(ACanvas, X1+5, Y1+5, X1+ImageWidth-5, Y1+ImageHeight-5, FAltW);
      finally
        ACanvas.Font := SaveFont;
        SaveFont.Free;
        Brush.Color := OldBrushColor;
        Brush.Style := OldBrushStyle;    {style after color as color changes style}
        Pen.Color := OldPenColor;
        end;
      end;
end;

{----------------TFloatingObj.CreateCopy}
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
ObjAlign := T.ObjAlign;
HSpaceL := T.HSpaceL;
HSpaceR := T.HSpaceR;
VSpaceT := T.VSpaceT;
VSpaceB := T.VSpaceB;
Pos := T.Pos;
end;

function TFloatingObj.GetYPosition: integer;
begin
Result := DrawYY;
end;

procedure ThvPanel.SetVisible(Value: boolean);
begin
if Value <> FVisible then
  begin
  FVisible := Value;
  if FVisible then
    Show
  else Hide;
  end;
end;

{----------------TCell.Create}
constructor TCell.Create(Master: TSectionList);
begin
inherited Create(Master);
IMgr := IndentManager.Create;
end;

{----------------TCell.CreateCopy}
constructor TCell.CreateCopy(AMasterList: TSectionList; T: TCellBasic);
begin
inherited CreateCopy(AMasterList, T);
IMgr := IndentManager.Create;
end;

destructor TCell.Destroy;
begin
IMgr.Free;
inherited Destroy;
end;

{----------------TCell.DoLogic}
function TCell.DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight, BlHt: integer;
                 var ScrollWidth: integer; var Curs: integer): integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  IB: integer;
  LIndex, RIndex: integer;
  SaveID: TObject;
begin
IMgr.Clear;
IMgr.Reset(0, Width);
IMgr.Width := Width;
SaveID := IMgr.CurrentID;
IMgr.CurrentID := Self;

LIndex := IMgr.SetLeftIndent(0, Y);
RIndex := IMgr.SetRightIndent(0+Width, Y);

Result := inherited DoLogic(Canvas, Y, Width, AHeight, BlHt, ScrollWidth, Curs);

IMgr.FreeLeftIndentRec(LIndex);
IMgr.FreeRightIndentRec(RIndex);
IB := IMgr.ImageBottom - YValue;   {check for image overhang}
IMgr.CurrentID := SaveID;
if IB > Result then
  Result := IB;
end;

{----------------TCell.Draw}
function TCell.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                            Y, XRef, YRef : integer): integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
begin
IMgr.Reset(X, X+IMgr.Width);
IMgr.ClipWidth := ClipWidth;
DrawYY := Y;   {This is overridden in TCellObj.Draw}
Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y, XRef, YRef);
end;

{----------------TCellObjCell.CreateCopy}
constructor TCellObjCell.CreateCopy(AMasterList: TSectionList; T: TCellObjCell);
begin
inherited CreateCopy(AMasterList, T);
MyRect := T.MyRect;;
end;

{----------------TCellObjCell.GetUrl}
function TCellObjCell.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType;
{Y is absolute}
begin
Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
if PtInRect(MyRect, Point(X, Y-MasterList.YOFF)) then
  begin
  if (not (guTitle in Result)) and (Title <> '') then
    begin
    ATitle := Title;
    Include(Result, guTitle);
    end;
  if (not (guUrl in Result)) and (Url <> '') then
    begin
    UrlTarg := TUrlTarget.Create;
    UrlTarg.URL := Url;
    UrlTarg.Target := Target;
    Include(Result, guUrl);
    end;
  end;
end;

{ TBlockCell }

function TBlockCell.DoLogicX(Canvas: TCanvas; X, Y, XRef, YRef, Width, AHeight, BlHt: integer;
  var ScrollWidth, Curs: Integer): integer;
{Do the entire layout of the this cell.  Return the total pixel height}
var
  I, Sw, TheCount: integer;
  H, Tmp: integer;
  SB: TSectionBase;
begin
  YValue := Y;
  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  tcContentBot := 0;
  tcDrawTop := 990000;
  tcDrawBot := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
    begin
    SB :=  TSectionBase(Items[I]);
    Tmp := SB.DrawLogic(Canvas, X, Y+H, XRef, YRef, Width, AHeight, BlHt, IMgr, Sw, Curs);
    H := H+Tmp;
    if Owner.HideOverflow then
      ScrollWidth := Width
    else ScrollWidth := IntMax(ScrollWidth, Sw);
    if (SB is TBlock) and (TBlock(SB).Positioning = posAbsolute) then
    else  tcContentBot := IntMax(tcContentBot, SB.ContentBot);
    tcDrawTop := IntMin(tcDrawTop, SB.DrawTop);
    tcDrawBot := IntMax(tcDrawBot, SB.DrawBot);
    Inc(I);
    end;
  Len := Curs - StartCurs;
  Result := H;
  CellHeight := Result;
 end;


{ TDrawList }

Type
  TImageRec = class(TObject)
    AObj: TImageObj;
    ACanvas: TCanvas;
    AX, AY:  integer;
    AYBaseline: integer;
    AFO: TFontObj;
  end;

procedure TDrawList.AddImage(Obj: TImageObj; Canvas: TCanvas; X, TopY, YBaseline: Integer;
  FO: TFontObj);
var
  IR: TImageRec;
begin
IR := TImageRec.Create;
IR.AObj := Obj;
IR.ACanvas := Canvas;
IR.AX := X;
IR.AY := TopY;
IR.AYBaseline := YBaseline;
IR.AFO := FO;
Add(IR);
end;

procedure TDrawList.DrawImages;
var
  I: integer;
  Item: TObject;
begin
I := 0;
while I < Count do  {note: Count may increase during this operation}
  begin
  Item := Items[I];
  if (Item is TImageRec) then
    with TImageRec(Item) do
      AObj.Draw(ACanvas, AX, AY, AYBaseline, AFO);
  Inc(I);
  end;
end;

{----------------TFormRadioButton.GetChecked:}
function TFormRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TFormRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
end;

procedure TFormRadioButton.SetChecked(Value: Boolean);
begin
  if GetKeyState(vk_Tab) < 0 then   {ignore if tab key down}
    Exit;
  if FChecked <> Value then
  begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
    if Value then
    begin
      inherited Changed;
      if not ClicksDisabled then Click;
    end;
  end;
end;

procedure TFormRadioButton.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {else don't get the arrow keys}
end;

{----------------TFormCheckBox.WMGetDlgCode}
procedure TFormCheckBox.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {this to eat the arrow keys}
end;

{----------------ThtTabcontrol.Destroy}
destructor ThtTabcontrol.Destroy;
begin
  inherited;
end;

procedure ThtTabcontrol.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {this to eat the arrow keys}
end;

{----------------LineRec.Create}
constructor LineRec.Create(SL: TSectionList);
begin
inherited Create;
if SL.InlineList.Count > 0 then
  FirstDraw := True;
end;

procedure LineRec.Clear;
begin
FreeAndNil(BorderList);
end;

destructor LineRec.Destroy;
begin
BorderList.Free;
inherited;
end;

{----------------BorderRec.DrawTheBorder}
procedure BorderRec.DrawTheBorder(Canvas: TCanvas; XOffset, YOffSet: integer; Printing: boolean);
var
  IRect, ORect: TRect;
begin
IRect := BRect;
Dec(IRect.Top, YOffSet);
Dec(IRect.Bottom, YOffSet);
Inc(IRect.Left, XOffset);
Inc(IRect.Right, XOffset);
if OpenStart then
  MargArray[BorderLeftStyle] := ord(bssNone);
if OpenEnd then
  MargArray[BorderRightStyle] := ord(bssNone);

if MargArray[BackgroundColor] <> clNone then
  begin
  Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(IRect);
  end;

ORect.Left := IRect.Left - MargArray[BorderLeftWidth];
ORect.Top := IRect.Top - MargArray[BorderTopWidth];
ORect.Right := IRect.Right + MargArray[BorderRightWidth];
ORect.Bottom := IRect.Bottom + MargArray[BorderBottomWidth];

DrawBorder(Canvas, ORect, IRect,
                     htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
                     htStyles(BorderStyleType(MargArray[BorderLeftStyle]), BorderStyleType(MargArray[BorderTopStyle]), BorderStyleType(MargArray[BorderRightStyle]), BorderStyleType(MargArray[BorderBottomStyle])),
                     MargArray[BackgroundColor], Printing)
end;

end.




