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

{
This module is comprised mostly of the various Section object definitions.
As the HTML document is parsed, it is divided up into sections.  Some sections
are quite simple, like TParagraphSpace.  Others are more complex such as
TSection which can hold a complete paragraph.

The HTML document is then stored as a list, TSectionList, of the various
sections.

Closely related to TSectionList is TCell.  TCell holds the list of sections for
each cell in a Table (the THtmlTable section).  In this way each table cell may
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

{-$define DO_BLOCK_INLINE}
{$ifdef DO_BLOCK_INLINE}
{$endif}

interface
uses
  Windows, Messages, Classes, Graphics, Controls, ExtCtrls,
{$ifdef LCL}Interfaces, {$endif}
  HtmlGlobals, HtmlUn2, StyleUn, HtmlGif2;

type

  THtmlStyleList = class(TStyleList)
  private
    MasterList: TObject;
  public
    constructor Create(AMasterList: TObject);
    procedure AddModifyProp(const Selector, Prop, Value: ThtString); override;
  end;

  ThvPanel = class(TPanel)
  public
    FVisible: boolean;
    procedure SetVisible(Value: boolean);
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  TLinkDrawnEvent = procedure(Sender: TObject; Page: Integer; const Url, Target: ThtString; ARect: TRect) of object;
  TFileBrowseEvent = procedure(Sender, Obj: TObject; var S: ThtString) of object;
  TGetBitmapEvent = procedure(Sender: TObject; const SRC: ThtString; var Bitmap: TBitmap; var Color: TColor) of object;
  TGetImageEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TMemoryStream) of object;
  TGottenImageEvent = TGetImageEvent;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList) of object;
  TPanelCreateEvent = procedure(Sender: TObject; const AName, AType, SRC: ThtString; Panel: ThvPanel) of object;
  TPanelDestroyEvent = procedure(Sender: TObject; Panel: ThvPanel) of object;
  TPanelPrintEvent = procedure(Sender: TObject; Panel: ThvPanel; const Bitmap: TBitmap) of object;
  TObjectTagEvent = procedure(Sender: TObject; Panel: ThvPanel; const Attributes, Params: ThtStringList; var WantPanel: boolean) of object;
  TObjectClickEvent = procedure(Sender, Obj: TObject; const OnClick: ThtString) of object;
  ThtObjectEvent = procedure(Sender, Obj: TObject; const Attribute: ThtString) of object;
  TExpandNameEvent = procedure(Sender: TObject; const SRC: ThtString; var Result: ThtString) of object;
  TCell = class;
  TBlockCell = class;
  TCellBasic = class;
  TSectionList = class;
  TSection = class;
  TBlock = class;

  ThtTabcontrol = class(TWinControl)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    property OnEnter;
    property OnExit;
    property TabStop;
    property OnKeyUp;
  end;

  TFontObj = class(TFontObjBase) {font information}
  private
    Section: TSection;
    FVisited, FHover: boolean;
    Title: ThtString;
    FYValue: Integer;
    Active: boolean;
    procedure SetVisited(Value: boolean);
    procedure SetHover(Value: boolean);
    function GetURL: ThtString;
    procedure SetAllHovers(List: TList; Value: boolean);
    procedure CreateFIArray;
{$IFNDEF NoTabLink}
    procedure EnterEvent(Sender: TObject);
    procedure ExitEvent(Sender: TObject);
    procedure CreateTabControl(TabIndex: Integer);
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AssignY(Y: Integer);
{$ENDIF}
  public
    Pos: Integer; {0..Len  Index where font takes effect}
    TheFont: TMyFont;
    FIArray: TFontInfoArray;
    FontHeight, {tmHeight+tmExternalLeading}
      tmHeight, tmMaxCharWidth,
      Overhang, Descent: Integer;
    SScript: AlignmentType;
    TabControl: ThtTabControl;
    constructor Create(ASection: TSection; F: TMyFont; Position: Integer);
    constructor CreateCopy(ASection: TSection; T: TFontObj);
    destructor Destroy; override;
    procedure ReplaceFont(F: TMyFont);
    procedure ConvertFont(FI: ThtFontInfo);
    procedure FontChanged;
    function GetOverhang: Integer;
    function GetHeight(var Desc: Integer): Integer;

    property URL: ThtString read GetURL;
    property Visited: boolean read FVisited write SetVisited;
    property Hover: boolean read FHover write SetHover;
    property YValue: Integer read FYValue;
  end;

  TFontList = class(TFreeList) {a list of TFontObj's}
  public
    constructor CreateCopy(ASection: TSection; T: TFontList);
    function GetFontAt(Posn: Integer; var OHang: Integer): TMyFont;
    function GetFontCountAt(Posn, Leng: Integer): Integer;
    function GetFontObjAt(Posn: Integer;
      var Index: Integer): TFontObj;
    procedure Decrement(N: Integer; ParentSectionList: TSectionList);
  end;

  TImageFormControlObj = class;

  TPanelObj = class(TFloatingObj)
  private
    FMasterList: TSectionList;
    SetWidth, SetHeight: Integer;
    IsCopy: boolean;
  public
    ShowIt: boolean;
    Panel, OPanel: ThvPanel;
    OSender: TObject;
    PanelPrintEvent: TPanelPrintEvent;
    FUserData: TObject;
    FMyPanelObj: TPanelObj;
    constructor Create(AMasterList: TSectionList; Position: Integer;
      L: TAttributeList; ACell: TCellBasic; ObjectTag: boolean);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TPanelObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: TSectionBaseList; Canvas: TCanvas;
      FO: TFontObjBase; AvailableWidth, AvailableHeight: Integer); override;
    procedure Draw(ACanvas: TCanvas; X1, Y1: Integer);
  end;

  HoverType = (hvOff, hvOverUp, hvOverDown);

  TImageObj = class(TFloatingObj) {inline image info}
  private
    FBitmap: TBitmap;
    FHover: HoverType;
    FHoverImage: boolean;
    AltHeight, AltWidth: Integer;
    Positioning: PositionType;
    function GetBitmap: TBitmap;
    procedure SetHover(Value: HoverType);
  public
    ObjHeight, ObjWidth: Integer; {width as drawn}
    Source: ThtString; {the src= attribute}
    Image: TgpObject; {bitmap possibly converted from GIF, Jpeg, etc or animated GIF}
    OrigImage: TgpObject; {same as above unless swapped}
    Mask: TBitmap; {Image's mask if needed for transparency}
    ParentSectionList: TSectionList;
    Transparent: Transparency; {None, Lower Left Corner, or Transp GIF}
    IsMap, UseMap: boolean;
    MapName: ThtString;
    MyFormControl: TImageFormControlObj; {if an <INPUT type=image}
    MyCell: TCellBasic;
    Swapped: boolean; {image has been replaced}
    Missing: boolean; {waiting for image to be downloaded}

    constructor Create(MasterList: TSectionBaseList; Position: Integer; L: TAttributeList);
    constructor SimpleCreate(MasterList: TSectionBaseList; const AnURL: ThtString);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TImageObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: TSectionBaseList; Canvas: TCanvas;
      FO: TFontObjBase; AvailableWidth, AvailableHeight: Integer); override;
    procedure DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: TgpObject; ddMask: TBitmap);
    procedure Draw(Canvas: TCanvas; X: Integer; TopY, YBaseline: Integer; FO: TFontObj);
    function InsertImage(const UName: ThtString; Error: boolean; var Reformat: boolean): boolean;

    property Bitmap: TBitmap read GetBitmap;
    property Hover: HoverType read FHover write SetHover;
    procedure ReplaceImage(NewImage: TStream);
  end;

  TImageObjList = class(TFreeList) {a list of TImageObj's and TPanelObj's}
  public
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TImageObjList);
    function FindImage(Posn: Integer): TFloatingObj;
    function GetHeightAt(Posn: Integer; var AAlign: AlignmentType;
      var FlObj: TFloatingObj): Integer;
    function GetWidthAt(Posn: Integer; var AAlign: AlignmentType;
      var HSpcL, HSpcR: Integer; var FlObj: TFloatingObj): Integer;
    function GetImageCountAt(Posn: Integer): Integer;
    function PtInImage(X: Integer; Y: Integer; var IX, IY, Posn: Integer;
      var AMap, UMap: boolean; var MapItem: TMapItem;
      var ImageObj: TImageObj): boolean;
    function PtInObject(X: Integer; Y: Integer; var Obj: TObject;
      var IX, IY: Integer): boolean;
    procedure Decrement(N: Integer);
  end;

  TFormControlObj = class;
  TRadioButtonFormControlObj = class;

  ThtmlForm = class(TObject)
  private
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    MasterList: TSectionList;
    Method: ThtString;
    Action, Target, EncType: ThtString;
    ControlList: TFreeList;
    NonHiddenCount: Integer;
    constructor Create(AMasterList: TSectionList; L: TAttributeList);
    destructor Destroy; override;
    procedure DoRadios(Radio: TRadioButtonFormControlObj);
    procedure InsertControl(Ctrl: TFormControlObj);
    procedure ResetControls;
    function GetFormSubmission: ThtStringList;
    procedure SubmitTheForm(const ButtonSubmission: ThtString);
    procedure SetFormData(SL: ThtStringList);
    procedure SetSizes(Canvas: TCanvas);
    procedure ControlKeyPress(Sender: TObject; var Key: Char);
  end;

  TFormControlObj = class(TIDObject)
  private
    FYValue: Integer;
    Active: boolean;
    PaintBitmap: TBitmap;
    AttributeList: ThtStringList;
    FTitle: ThtString;
    function GetControl: TWinControl; virtual;
    function GetAttribute(const AttrName: ThtString): ThtString;
  protected
    CodePage: Integer;
    procedure DoOnChange; virtual;
    procedure SaveContents; virtual;
    function GetYPosition: Integer; override;
  public
    Pos: Integer; {0..Len  index of control position}
    MasterList: TSectionList;
    MyForm: ThtmlForm;
    Value: ThtString;
    FName, FID: ThtString;
    FormAlign: AlignmentType;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB, BordT, BordB: Integer;
    FHeight, FWidth: Integer;
    PercentWidth: boolean;
    Disabled: boolean;
    Readonly: boolean;
    BkColor: TColor;
    FControl: TWinControl;
    ShowIt: boolean;
    OnClickMessage: ThtString;
    OnFocusMessage: ThtString;
    OnBlurMessage: ThtString;
    OnChangeMessage: ThtString;

    constructor Create(AMasterList: TSectionList; Position: Integer; L: TAttributeList);
    constructor CreateCopy(T: TFormControlObj);
    destructor Destroy; override;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ProcessProperties(Prop: TProperties); virtual;
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); virtual;
    procedure ResetToValue; virtual;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; virtual;
    procedure SetData(Index: Integer; const V: ThtString); virtual;
    procedure SetDataInit; virtual;
    procedure SetHeightWidth(Canvas: TCanvas); virtual;
    procedure EnterEvent(Sender: TObject); {these two would be better private}
    procedure ExitEvent(Sender: TObject);
    procedure FormControlClick(Sender: TObject);

    property TheControl: TWinControl read GetControl; {the Delphi control, TButton, TMemo, etc}
    property Name: ThtString read FName; {Name given to control}
    property ID: ThtString read FID; {ID attribute of control}
    property YValue: Integer read FYValue;
    property AttributeValue[const AttrName: ThtString]: ThtString read GetAttribute;
    property Title: ThtString read FTitle write FTitle;
  end;

  TImageFormControlObj = class(TFormControlObj)
  private
    MyImage: TImageObj;
  public
    XPos, YPos, XTmp, YTmp: Integer; {click position}
    constructor Create(AMasterList: TSectionList; Position: Integer; L: TAttributeList);
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ImageClick(Sender: TObject);
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
  end;

  THiddenFormControlObj = class(TFormControlObj)
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
  end;

  TEditFormControlObj = class(TFormControlObj)
  private
    EnterContents: ThtString;
    tmAveCharWidth: Integer;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    EditSize: Integer;
    constructor Create(AMasterList: TSectionList; Position: Integer;
      L: TAttributeList; const Typ: ThtString; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ResetToValue; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  WhichType = (Submit, ResetB, Button, Browse);

  TButtonFormControlObj = class(TFormControlObj)
  public
    Which: WhichType;
    MyEdit: TEditFormControlObj;
    constructor Create(AMasterList: TSectionList; Position: Integer;
      L: TAttributeList; const Typ: ThtString; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
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
    constructor Create(AMasterList: TSectionList; Position: Integer;
      L: TAttributeList; ACell: TCellBasic);
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure RadioClick(Sender: TObject);
    procedure ResetToValue; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
  end;

  TCheckBoxFormControlObj = class(TFormControlObj)
  private
    WasChecked: boolean;
  public
    IsChecked: boolean;
    constructor Create(AMasterList: TSectionList; Position: Integer;
      L: TAttributeList; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure ResetToValue; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure SetDataInit; override;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  end;

  LineRec = class(TObject) {holds info on a line of text}
  private
    Start: PWideChar;
    SpaceBefore, SpaceAfter,
      LineHt, {total height of line}
      LineImgHt, {top to bottom including any floating image}
      Ln, {# chars in line}
      Descent,
      LineIndent: Integer;
    DrawXX, DrawWidth: Integer;
    DrawY: Integer;
    Spaces, Extra: Integer;
    BorderList: TFreeList; {List of inline borders (BorderRec's) in this Line}
    FirstDraw: boolean; {set if border processing needs to be done when first drawn}
    FirstX: Integer; {x value at FirstDraw}
    Shy: boolean;
  public
    constructor Create(SL: TSectionList);
    procedure Clear;
    destructor Destroy; override;
  end;

  XArray = array[0..300] of Integer;
  PXArray = ^XArray;

  IndexObj = class
    Pos: Integer;
      Index: Integer;
  end;

  TSectionSubsBase = class(TSectionBase)
  private
    function getMyBlock: TBlock;
    function getParentSectionList: TSectionList;
  public
    property MyBlock: TBlock read getMyBlock;
    property ParentSectionList: TSectionList read getParentSectionList;
  end;

  TSection = class(TSectionSubsBase)
  {TSection holds <p>, <li>, many other things, and the base for lists}
  private
    SectionNumber: Integer;
    ThisCycle: Integer;
    function GetIndexObj(I: Integer): IndexObj;
    property PosIndex[I: Integer]: IndexObj read GetIndexObj;
    procedure CheckForInlines(LR: Linerec);
  public
    BuffS: WideString; {holds the text for the section}
    Buff: PWideChar; {same as above}
    Brk: string;
    XP: PXArray;
    BuffSize: Integer; {buffer may be larger}
    Fonts: TFontList; {List of FontObj's in this section}
    Images: TImageObjList; {list of TImageObj's, the images in section}
    FormControls: TList; {list of TFormControls in section}
    SIndexList: TFreeList; {list of Source index changes}
    Lines: TFreeList; {List of LineRecs,  info on all the lines in section}
    Justify: JustifyType; {Left, Centered, Right}
    ClearAttr: ClearAttrType;
    LineHeight: Integer;
    DrawWidth: Integer;
    AnchorName: boolean;
    StoredMin, StoredMax: Integer;
    FirstLineIndent: Integer;
    FLPercent: Integer;
    BreakWord: boolean;
    TextWidth: Integer;

    constructor Create(AMasterList: TSectionList; L: TAttributeList;
      Prop: TProperties; AnURL: TUrlTarget; ACell: TCellBasic; FirstItem: boolean);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    destructor Destroy; override;
    function AddFormControl(Which: Symb; AMasterList: TSectionList; L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TFormControlObj;
    function AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer): TImageObj;
    function AddPanel(L: TAttributeList; ACell: TCellBasic; Index: Integer): TPanelObj;
    function CreatePanel(L: TAttributeList; ACell: TCellBasic): TPanelObj;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function FindCountThatFits(Canvas: TCanvas; Width: Integer; Start: PWideChar; Max: Integer): Integer;
    function FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; Max: Integer; X, Y: Integer; IMgr: TIndentManager; var ImgHt: Integer; NxImages: TList): Integer;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer; var XR: Integer; var YR: Integer; var CaretHt: Integer; var Intext: boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: boolean): Integer;
    function FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer; var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
    function PtInObject(X: Integer; Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean; override;
    procedure AddChar(C: WideChar; Index: Integer); virtual;
    procedure AddOpBrk;
    procedure AddPanel1(PO: TPanelObj; Index: Integer);
    procedure AddTokenObj(T: TokenObj); virtual;
    procedure Allocate(N: Integer);
    procedure ChangeFont(Prop: TProperties);
    procedure CheckFree;
    procedure CopyToClipboard; override;
    procedure Finish;
    procedure HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget; Attributes: TAttributeList; Prop: TProperties);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
    procedure ProcessText(TagIndex: Integer); virtual;
  end;

  TBlock = class(TBlockBase)
  private
    function getParentSectionList: TSectionList;
  protected
    procedure ConvMargArray(BaseWidth, BaseHeight: Integer; var AutoCount: Integer); virtual;
    procedure DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect); virtual;
    function getBorderWidth: Integer; virtual;
//    function getDisplay: TPropDisplay; virtual;
    property BorderWidth: Integer read getBorderWidth;
    property ParentSectionList: TSectionList read getParentSectionList;
    procedure ContentMinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); virtual;
  public
    MargArray: TMarginArray;
    MyCell: TBlockCell;
    EmSize, ExSize, FGColor: Integer;
    BorderStyle: BorderStyleType;
    FloatLR: AlignmentType; {ALeft or ARight if floating}
    ClearAttr: ClearAttrType;
    IsListBlock: boolean;
    PRec: PtPositionRec;
    Positioning: PositionType; {posStatic, posAbsolute, posRelative}
    Visibility: VisibilityType;
    BottomAuto: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    //DisplayNone: Boolean;
    HideOverflow: boolean;
    Justify: JustifyType;
    Converted: boolean;

    MargArrayO: TVMarginArray;
    OwnerCell: TCellBasic;
    TagClass: ThtString; {debugging aid}
    NewWidth: Integer;
    ClearAddon: Integer;
    Indent: Integer;
    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage: TgpObject;
    TiledMask, FullBG: TBitmap;
    TopP, LeftP: Integer;
    DrawList: TList;
    NoMask: boolean;
    ClientContentBot: Integer;
    BlockTitle: ThtString;
    MyRect: TRect;
    RefIMgr: TIndentManager;

    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    destructor Destroy; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
      var XR: Integer; var YR: Integer; var CaretHt: Integer; var Intext: boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; virtual;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
    function PtInObject(X: Integer; Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean; override;
    procedure AddSectionsToList; override;
    procedure CollapseMargins;
    procedure CopyToClipboard; override;
    procedure DrawBlock(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y, XRef, YRef: Integer);
    procedure DrawSort;
    procedure DrawTheList(Canvas: TCanvas; ARect: TRect; ClipWidth, X, XRef, YRef: Integer);
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
  end;

  ListTypeType = (None, Ordered, Unordered, Definition, liAlone);

  THtmlTable = class;

  TTableBlock = class(TBlock)
  protected
    function getBorderWidth: Integer; override;
    procedure DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect); override;
  public
    Table: THtmlTable;
    WidthAttr: Integer;
    AsPercent: boolean;
    BkColor: TColor;
    BkGnd: boolean;
    HSpace, VSpace: Integer;
    HasCaption: boolean;
    TableBorder: boolean;
    Justify: JustifyType;
    TableIndent: Integer;

    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
      ATable: THtmlTable; TableAttr: TAttributeList; TableLevel: Integer);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
    function FindWidth1(Canvas: TCanvas; AWidth, ExtMarg: Integer): Integer;
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
    TableID: ThtString;
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
      Attributes: TAttributeList; ATableBlock: TTableBlock);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    procedure CancelUsage;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    property CaptionBlock: TBlock read FCaptionBlock write SetCaptionBlock;
  end;

  THRBlock = class(TBlock)
  public
    Align: JustifyType;
    MyHRule: TSectionBase;
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
  end;

  TBlockLI = class(TBlock)
  private
    FListType: ListTypeType;
    FListNumb: Integer;
    FListStyleType: ListBulletType;

    ListFont: TFont;
    Image: TImageObj;
    FirstLineHt: Integer;
  public
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
      Sy: Symb; APlain: boolean; AIndexType: ThtChar;
      AListNumb, ListLevel: Integer; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property ListNumb: Integer read FListNumb;
    property ListStyleType: ListBulletType read FListStyleType;
    property ListType: ListTypeType read FListType;
  end;

  TBodyBlock = class(TBlock)
  public
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  TFieldsetBlock = class(TBlock)
  private
    FLegend: TBlockCell;
  protected
    procedure ConvMargArray(BaseWidth: Integer; BaseHeight: Integer; var AutoCount: Integer); override;
    procedure DrawBlockBorder(Canvas: TCanvas; ORect: TRect; IRect: TRect); override;
    procedure ContentMinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
  public
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas: TCanvas; X: Integer; Y: Integer; XRef: Integer; YRef: Integer;
      AWidth: Integer; AHeight: Integer; BlHt: Integer; IMgr: TIndentManager; var MaxWidth: Integer;
      var Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X: Integer;
      XRef: Integer; YRef: Integer): Integer; override;
    property Legend: TBlockCell read FLegend;
  end;

  TCellObj = class;

  TCellList = class(TFreeList)
  {a list of TCellObj's to form a table row}
  private
    function getCellObj(Index: Integer): TCellObj;
  public
    RowHeight: Integer;
    SpecRowHeight, SpecRowHeightPercent: Integer;
    RowSpanHeight: Integer; {height of largest rowspan}
    BkGnd: boolean;
    BkColor: TColor;
    BkImage: ThtString;
    APRec: PtPositionRec;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    RowType: TRowType;

    constructor Create(Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TCellList);
    procedure InitializeRow;
    function DrawLogic1(Canvas: TCanvas; const Widths: array of Integer; Span,
      CellSpacing, AHeight, Rows: Integer; var Desired: Integer; var Spec, More: boolean): Integer;
    procedure DrawLogic2(Canvas: TCanvas; Y: Integer;
      CellSpacing: Integer; var Curs: Integer);
    function Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
      const Widths: array of Integer; X: Integer; Y, YOffset: Integer;
      CellSpacing: Integer; Border: boolean; Light, Dark: TColor;
      MyRow: Integer): Integer;
    procedure Add(CellObj: TCellObj);
    property Items[Index: Integer]: TCellObj read getCellObj;
  end;

  TColObj = class
    colWidth: Integer;
    colAsPercent: boolean;
    colAlign: ThtString;
    colVAlign: AlignmentType;
  end;

  IntArray = array of Integer;

  THtmlTable = class(TSectionSubsBase)
  private
    TablePartRec: TTablePartRec;
    HeaderHeight, HeaderRowCount, FootHeight, FootStartRow, FootOffset: Integer;
    BodyBreak: Integer;
    HeadOrFoot: Boolean;
    procedure DrawTable(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y: Integer);
    procedure DrawTableP(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y: Integer);
    procedure FindRowHeights(Canvas: TCanvas; AHeight: Integer);
  public
    Rows: TFreeList; {a list of TCellLists}
    ListsProcessed: Boolean;
    Indent: Integer; {table indent}
    BorderWidth: Integer; {width of border}
    Float: Boolean; {if floating}
    NumCols, {Number columns in table}
      TableWidth, {width of table}
      tblWidthAttr: Integer; {Width attribute as entered}
    UseAbsolute: boolean; {width entries are considered absolute}
    TableHeight: Integer; {height of table itself, not incl caption}
    CellPadding, CellSpacing: Integer;
    HSpace, VSpace: Integer; {horizontal, vertical extra space}
    BorderColor: TColor; //BG, 13.06.2010: added for Issue 5: Table border versus stylesheets
    BorderColorLight, BorderColorDark: TColor;
    EndList: boolean; {marker for copy}
    DrawX: Integer;
    DrawY: Integer;
    BkGnd: boolean;
    BkColor: TColor;
    ColInfo: TFreeList;
    Widths, {holds column widths}
      MaxWidths, MinWidths, Heights,
      Percents: IntArray; {percent widths of columns}

    constructor Create(Master: TSectionList; Attr: TAttributeList;
      Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure DoColumns(Width: Integer; AsPercent: boolean;
      VAlign: AlignmentType; const Align: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
    procedure AddDummyCells;
    procedure GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
      TotalMaxWidth: Integer);
    procedure GetWidthsAbs(Canvas: TCanvas; TablWidth: Integer; Specified: boolean);
    procedure GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: Integer;
      TheWidth: Integer);
    procedure TableSpecifiedAndWillFit(TheWidth: Integer);
    procedure TableNotSpecifiedAndWillFit(TotalMinWidth, TotalMaxWidth, TheWidth: Integer);
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj};
      var ATitle: ThtString): guResultType; override;
    function PtInObject(X: Integer; Y: Integer; var Obj: TObject;
      var IX, IY: Integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
      var XR: Integer; var YR: Integer; var CaretHt: Integer;
      var Intext: boolean): Integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer;
      var Y: Integer): boolean; override;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    procedure CopyToClipboard; override;
  end;

  TDrawList = class(TFreeList)
    procedure AddImage(Obj: TImageObj; Canvas: TCanvas; X: Integer; TopY,
      YBaseline: Integer; FO: TFontObj);
    procedure DrawImages;
  end;

  TCellBasic = class(TSectionBaseList) {a list which holds sections and blocks}
  public
    MasterList: TSectionList; {the TSectionList that holds the whole document}
    YValue: Integer; {vertical position at top of cell}
    IMgr: TIndentManager;
    StartCurs: Integer;
    Len: Integer;
    BkGnd: boolean;
    BkColor: TColor;
    tcContentBot, tcDrawTop, tcDrawBot: Integer;
    OwnersTag: ThtString;
    Owner: TBlock;

    constructor Create(Master: TSectionBaseList);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TCellBasic);
    procedure Add(Item: TSectionBase; TagIndex: Integer);
    function CheckLastBottomMargin: boolean;
    procedure CopyToClipboard;
    function DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
      var ScrollWidth: Integer; var Curs: Integer): Integer; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer;
      Y, XRef, YRef: Integer): Integer; virtual;
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer; var UrlTarg: TUrlTarget;
      var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; virtual;
    function PtInObject(X: Integer; Y: Integer; var Obj: TObject;
      var IX, IY: Integer): boolean;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
      var XR: Integer; var YR: Integer; var Ht: Integer;
      var Intext: boolean): Integer;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
    function FindSourcePos(DocPos: Integer): Integer;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
    procedure AddSectionsToList;
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
  end;

  TCell = class(TCellBasic)
    DrawYY: Integer;

    constructor Create(Master: TSectionBaseList);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TCellBasic);
    destructor Destroy; override;
    function DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
      var ScrollWidth: Integer; var Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer;
      Y, XRef, YRef: Integer): Integer; override;
  end;

  TCellObjCell = class(TCell)
    MyRect: TRect;
    Title: ThtString;
    Url, Target: ThtString;
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TCellObjCell);
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer; var UrlTarg: TUrlTarget;
      var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
  end;

  TBlockCell = class(TCellBasic)
    CellHeight: Integer;
    TextWidth: Integer;

    function DoLogicX(Canvas: TCanvas; X, Y: Integer; XRef, YRef, Width, AHeight, BlHt: Integer;
      var ScrollWidth: Integer; var Curs: Integer): Integer;
  end;

  TSectionList = class(TCell) {a list of all the sections--holds document}
  private
    procedure AdjustFormControls;
  protected
    procedure AddSectionsToPositionList(Sections: TSectionBase); override;
  public
    // copied by move() in CreateCopy()
    ShowImages: boolean; {set if showing images}
    Printing: boolean; {set if printing -- also see IsCopy}
    YOff: Integer; {marks top of window that's displayed}
    YOffChange: boolean; {when above changes}
    NoPartialLine: boolean; {set when printing if no partial line allowed at page bottom}
    SelB, SelE: Integer;
    LinkVisitedColor, LinkActiveColor, HotSpotColor: TColor;
    PrintTableBackground: boolean;
    PrintBackground: boolean;
    PrintMonoBlack: boolean;
    TheOwner: THtmlViewerBase; {the viewer that owns this document}
    PPanel: TWinControl; {the viewer's PaintPanel}
    GetBitmap: TGetBitmapEvent; {for OnBitmapRequest Event}
    GetImage: TGetImageEvent; {for OnImageRequest Event}
    GottenImage: TGottenImageEvent; {for OnImageRequest Event}
    ExpandName: TExpandNameEvent;
    ObjectClick: TObjectClickEvent;
    ObjectFocus: ThtObjectEvent;
    ObjectBlur: ThtObjectEvent;
    ObjectChange: ThtObjectEvent;
    FileBrowse: TFileBrowseEvent;
    BackGround: TColor;
    // end of copied by move() in CreateCopy()
    // don't copy strings via move()
    PreFontName: ThtString; {<pre>, <code> font for document}

    OnBackgroundChange: TNotifyEvent;
    BackgroundBitmap: TGpObject; //TBitmap;
    BackgroundMask: TBitmap;
    BackgroundAniGif: TGifImage;
    BackgroundPRec: PtPositionRec;
    BitmapName: ThtString; {name of background bitmap}
    BitmapLoaded: boolean; {if background bitmap is loaded}
    htmlFormList: TFreeList;
    AGifList: TList; {list of all animated Gifs}
    SubmitForm: TFormSubmitEvent;
    ScriptEvent: TScriptEvent;
    PanelCreateEvent: TPanelCreateEvent;
    PanelDestroyEvent: TPanelDestroyEvent;
    PanelPrintEvent: TPanelPrintEvent;
    CB: SelTextCount;
    PageBottom: Integer;
    PageShortened: boolean;
    MapList: TFreeList; {holds list of client maps, TMapItems}
    Timer: TTimer; {for animated GIFs}
    FormControlList: TList; {List of all TFormControlObj's in this SectionList}
    PanelList: TList; {List of all TPanelObj's in this SectionList}
    MissingImages: ThtStringList; {images to be supplied later}
    ControlEnterEvent: TNotifyEvent;
    LinkList: TList; {List of links (TFontObj's)}
    ActiveLink: TFontObj;
    LinksActive: boolean;
    ActiveImage: TImageObj;
    ShowDummyCaret: boolean;
    Styles: THtmlStyleList; {the stylesheet}
    DrawList: TDrawList;
    FirstLineHtPtr: PInteger;
    IDNameList: TIDNameList;
    PositionList: TList;
    BitmapList: TStringBitmapList;
    SectionCount: Integer;
    CycleNumber: Integer;
    ProgressStart: Integer;
    IsCopy: boolean; {set when printing or making bitmap/metafile}
    NoOutput: boolean;
    TabOrderList: ThtStringList;
    FirstPageItem: boolean;
    StopTab: boolean;
    InlineList: TFreeList; {actually TInlineList, a list of InlineRec's}
    TableNestLevel: Integer;
    InLogic2: boolean;
    LinkDrawnEvent: TLinkDrawnEvent;
    LinkPage: Integer;
    PrintingTable: THtmlTable;
    ScaleX, ScaleY: single;
    SkipDraw: boolean;

    constructor Create(Owner: THtmlViewerBase; APaintPanel: TWinControl);
    constructor CreateCopy(T: TSectionList);
    destructor Destroy; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; override;
    function DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
      var ScrollWidth: Integer; var Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer; Y, XRef, YRef: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSectionAtPosition(Pos: Integer; var TopPos: Integer; var Index: Integer): TSectionBase;
    function GetFormcontrolData: TFreeList;
    function GetSelLength: Integer;
    function GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
    function GetTheBitmap(const BMName: ThtString;
      var Transparent: Transparency; var AMask: TBitmap; var FromCache, Delay: boolean): TgpObject;
    function GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
    procedure CancelActives;
    procedure CheckGIFList(Sender: TObject);
    procedure Clear; override;
    procedure ClearLists;
    procedure CopyToClipboardA(Leng: Integer);
    procedure GetBackgroundBitmap;
    procedure HideControls;
    procedure InsertImage(const Src: ThtString; Stream: TMemoryStream; var Reformat: boolean);
    procedure LButtonDown(Down: boolean);
    procedure ProcessInlines(SIndex: Integer; Prop: TProperties; Start: boolean);
    procedure SetBackground(ABackground: TColor);
    procedure SetBackgroundBitmap(const Name: ThtString; const APrec: PtPositionRec);
    procedure SetFormcontrolData(T: TFreeList);
    procedure SetYOffset(Y: Integer);
    procedure SetFonts(const Name, PreName: ThtString; ASize: Integer;
      AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
      LnksActive: boolean; LinkUnderLine: boolean; ACharSet: TFontCharSet;
      MarginHeight, MarginWidth: Integer);
  end;

  TCellObj = class(TObject) {holds a TCell and some other information}
  public
    ColSpan, RowSpan, {column and row spans for this cell}
      Wd: Integer; {total width (may cover more than one column)}
    Ht, {total height (may cover more than one row)}
      VSize: Integer; {Actual vertical size of contents}
    SpecHt: Integer; {Height as specified}
    SpecHtPercent: Integer;
    YIndent: Integer; {Vertical indent}
    VAlign: AlignmentType; {Top, Middle, or Bottom}
    WidthAttr: Integer; {Width attribute (percentage or absolute)}
    AsPercent: boolean; {it's a percent}
    EmSize, ExSize: Integer;
    PRec: PtPositionRec;
    PadTop, PadRight, PadBottom, PadLeft: Integer;
    BrdTop, BrdRight, BrdBottom, BrdLeft: Integer;
    HzSpace, VrSpace: Integer;
    BorderStyle: BorderStyleType;
    ShowEmptyCells: Boolean;
    Cell: TCellObjCell;

    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage: TGpObject;
    TiledMask, FullBG: TBitmap;
    MargArray: TMarginArray;
    MargArrayO: TVMarginArray;
    NoMask: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;

    constructor Create(Master: TSectionBaseList; AVAlign: AlignmentType; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TCellObj);
    destructor Destroy; override;
  private
    procedure InitializeCell(TablePadding: Integer; const BkImageName: ThtString;
      const APRec: PtPositionRec; Border: boolean);
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: Integer;
      Border: boolean; Light, Dark: TColor);
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: Integer;
      var Curs: Integer);
  end;

  TPage = class(TSectionSubsBase)
  public
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  THorzLine = class(TSectionSubsBase) {a horizontal line, <hr>}
    VSize: Integer;
    Color: TColor;
    Align: JustifyType;
    NoShade: boolean;
    BkGnd: boolean;
    Width, Indent: Integer;

    constructor Create(AMasterList: TSectionList; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase); override;
    procedure CopyToClipboard; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  TPreFormated = class(TSection)
  {section for preformated, <pre>}
  public
    procedure ProcessText(TagIndex: Integer); override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth: Integer; var Curs: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer); override;
  end;

  THtmlPropStack = class(TPropStack)
  public
    MasterList: TSectionList;
    SIndex: Integer;
    procedure PopAProp(const Tag: ThtString);
    procedure PopProp;
    procedure PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProps: TProperties);
  end;

  function htCompareText(const T1, T2: ThtString): Integer;

var
  CurrentStyle: TFontStyles; {as set by <b>, <i>, etc.}
  CurrentForm: ThtmlForm;
{$ifdef UNICODE}
{$else}
  UnicodeControls: boolean;
{$endif}

var
  PropStack: THtmlPropStack;
  Title: UnicodeString;
  Base: ThtString;
  BaseTarget: ThtString;
  NoBreak: boolean; {set when in <NoBr>}

implementation

uses
  SysUtils, Variants, Forms, Math,
  HtmlSbs1
  {$IFNDEF NoGDIPlus}, GDIPL2A{$ENDIF};

//-- BG ---------------------------------------------------------- 10.12.2010 --
function htCompareText(const T1, T2: ThtString): Integer;
begin
{$ifdef UseUnicode}
  Result := WideCompareText(T1, T2);
{$else}
  Result := AnsiCompareText(T1, T2);
{$endif}
end;

procedure InitializeFontSizes(Size: Integer);
var
  I: Integer;
begin
  for I := 1 to 7 do
  begin
    FontConv[I] := FontConvBase[I] * Size / 12.0;
    PreFontConv[I] := PreFontConvBase[I] * Size / 12.0;
  end;
end;

var
  NLevel: Integer; {for debugging}

type
  TSectionClass = class of TSectionBase;
  EProcessError = class(Exception);

type
  TFormRadioButton = class(ThtRadioButton)
  private
    IDName: ThtString;
    FChecked: boolean;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    procedure CreateWnd; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  published
    property Checked: boolean read GetChecked write SetChecked;
  end;

  TFormCheckBox = class(ThtCheckBox)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  end;

  BorderRec = class {record for inline borders}
  private
    BStart, BEnd: Integer;
    OpenStart, OpenEnd: boolean;
    BRect: TRect;
    MargArray: TMarginArray;
    procedure DrawTheBorder(Canvas: TCanvas; XOffset, YOffSet: Integer; Printing: boolean);
  end;

  InlineRec = class
  private
    StartB, EndB, IDB, StartBDoc, EndBDoc: Integer;
    MargArray: TMarginArray;
  end;

  TInlineList = class(TFreeList) {a list of InlineRec's}
  private
    NeedsConverting: boolean;
    Owner: TSectionList;
    procedure AdjustValues;
    function GetStartB(I: Integer): Integer;
    function GetEndB(I: Integer): Integer;
  public
    constructor Create(AnOwner: TSectionList);
    procedure Clear; override;
    property StartB[I: Integer]: Integer read GetStartB;
    property EndB[I: Integer]: Integer read GetEndB;
  end;

constructor TFontObj.Create(ASection: TSection; F: TMyFont; Position: Integer);
begin
  inherited Create;
  Section := ASection;
  TheFont := F;
  Pos := Position;
  UrlTarget := TUrlTarget.Create;
  FontChanged;
end;

{$IFNDEF NoTabLink}

procedure TFontObj.EnterEvent(Sender: TObject);
var
  List: TList;
  I, J: Integer;
begin
  Active := True;
{Make adjacent fonts in this link active also}
  List := Section.ParentSectionList.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I + 1 to List.Count - 1 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        TFontObj(List[J]).Active := True
      else
        Break;
  Section.ParentSectionList.ControlEnterEvent(Self);
end;

procedure TFontObj.ExitEvent(Sender: TObject);
var
  List: TList;
  I, J: Integer;
begin
  Active := False;
{Make adjacent fonts in this link inactive also}
  List := Section.ParentSectionList.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I + 1 to List.Count - 1 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        TFontObj(List[J]).Active := False
      else
        Break;
  Section.ParentSectionList.PPanel.Invalidate;
end;

procedure TFontObj.AssignY(Y: Integer);
var
  List: TList;
  I, J: Integer;
begin
  if UrlTarget.Url = '' then
    Exit;
  if Assigned(TabControl) then
    FYValue := Y
  else
  begin {Look back for the TFontObj with the TabControl}
    List := Section.ParentSectionList.LinkList;
    I := List.IndexOf(Self);
    if I >= 0 then
      for J := I - 1 downto 0 do
        if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        begin
          if Assigned(TFontObj(List[J]).TabControl) then
          begin
            TFontObj(List[J]).FYValue := Y;
            break;
          end;
        end
        else
          Break;
  end;
end;

procedure TFontObj.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Viewer: THtmlViewer;
begin
  Viewer := THtmlViewer(Section.ParentSectionList.TheOwner);
  if (Key = vk_Return) then
  begin
    Viewer.Url := UrlTarget.Url;
    Viewer.Target := UrlTarget.Target;
    Viewer.LinkAttributes.Text := UrlTarget.Attr;
    Viewer.LinkText := Viewer.GetTextByIndices(UrlTarget.Start, UrlTarget.Last);
    Viewer.TriggerUrlAction; {call to UrlAction via message}
  end
  else {send other keys to THtmlViewer}
    Viewer.KeyDown(Key, Shift);
end;

procedure TFontObj.CreateTabControl(TabIndex: Integer);
var
  PntPanel: TWinControl; //TPaintPanel;
  I, J: Integer;
  List: TList;
begin
  if Assigned(TabControl) then
    Exit;
  {Look back for the TFontObj with the TabControl}
  List := Section.ParentSectionList.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I - 1 downto 0 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        if Assigned(TFontObj(List[J]).TabControl) then
          Exit;

  PntPanel := {TPaintPanel(}Section.ParentSectionList.PPanel{)};
  TabControl := ThtTabcontrol.Create(PntPanel);
  with ThtTabcontrol(TabControl) do
  begin
    Left := -4000; {so will be invisible until placed}
    Width := 1;
    Height := 1;
    TabStop := True;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnKeyDown := Self.AKeyDown;
  end;
  TabControl.Parent := PntPanel;

  if TabIndex > 0 then
  {Adding leading 0's to the number ThtString allows it to be sorted numerically,
   and the Count takes care of duplicates}
    with Section.ParentSectionList.TabOrderList do
      AddObject(Format('%.5d%.3d', [TabIndex, Count]), TabControl);
end;
{$ENDIF}

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
    CharSet := ICharSet;
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
    else if Hover then
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
      else
        ConvertFont(FIArray.Ar[HLFont])
    else if FVisited then
      ConvertFont(FIArray.Ar[VFont])
    else
      ConvertFont(FIArray.Ar[LFont]);
    FontChanged;
  end;
end;

procedure TFontObj.SetAllHovers(List: TList; Value: boolean);
{Set/Reset Hover on this item and all adjacent item with the same URL}
var
  I, J: Integer;
begin
  SetHover(Value);
  I := List.IndexOf(Self);
  if I >= 0 then
  begin
    J := I + 1;
    while (J < List.Count) and (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) do
    begin
      TFontObj(List[J]).Hover := Value;
      Inc(J);
    end;
    J := I - 1;
    while (J >= 0) and (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) do
    begin
      TFontObj(List[J]).Hover := Value;
      Dec(J);
    end;
  end;
end;

function TFontObj.GetURL: ThtString;
begin
  try
    Result := UrlTarget.Url;
  except
    Result := '';
{$IFDEF DebugIt}
    ShowMessage('Bad TFontObj, htmlsubs.pas, TFontObj.GetUrl');
{$ENDIF}
  end;
end;

procedure TFontObj.FontChanged;
begin
  tmHeight := TheFont.tmHeight;
  tmMaxCharWidth := TheFont.tmMaxCharWidth;
  FontHeight := TheFont.tmHeight + TheFont.tmExternalLeading;
  Descent := TheFont.tmDescent;
  if fsItalic in TheFont.Style then {estimated overhang}
    Overhang := TheFont.tmheight div 10
  else
    Overhang := 0;
  TheFont.Charset := TheFont.tmCharset;
end;

function TFontObj.GetOverhang: Integer;
begin
  Result := Overhang;
end;

function TFontObj.GetHeight(var Desc: Integer): Integer;
begin
  Desc := Descent;
  Result := FontHeight;
end;

constructor TFontList.CreateCopy(ASection: TSection; T: TFontList);
var
  I: Integer;
begin
  inherited create;
  for I := 0 to T.Count - 1 do
    Add(TFontObj.CreateCopy(ASection, TFontObj(T.Items[I])));
end;

function TFontList.GetFontAt(Posn: Integer;
  var OHang: Integer): TMyFont;
{given a character index, find the font that's effective there}
var
  I, PosX: Integer;
  F: TFontObj;
begin
  I := 0;
  PosX := 0;
  while (I < Count) do
  begin
    PosX := TFontObj(Items[I]).Pos;
    Inc(I);
    if PosX >= Posn then
      Break;
  end;
  Dec(I);
  if PosX > Posn then
    Dec(I);
  F := TFontObj(Items[I]);
  OHang := F.Overhang;
  Result := F.TheFont;
end;

function TFontList.GetFontCountAt(Posn, Leng: Integer): Integer;
{Given a position, return the number of chars before the font changes}
var
  I, PosX: Integer;
begin
  I := 0;
  PosX := 0;
  while I < Count do
  begin
    PosX := TFontObj(Items[I]).Pos;
    if PosX >= Posn then
      Break;
    Inc(I);
  end;
  if PosX = Posn then
    Inc(I);
  if I = Count then
    Result := Leng - Posn
  else
    Result := TFontObj(Items[I]).Pos - Posn;
end;

{----------------TFontList.GetFontObjAt}

function TFontList.GetFontObjAt(Posn: Integer;
  var Index: Integer): TFontObj;
{Given a position, returns the FontObj which applies there and the index of
 the FontObj in the list}
var
  PosX: Integer;
begin
  Index := 0;
  PosX := 0;
  while (Index < Count) do
  begin
    PosX := TFontObj(Items[Index]).Pos;
    Inc(Index);
    if PosX >= Posn then
      Break;
  end;
  Dec(Index);
  if PosX > Posn then
    Dec(Index);
  Result := TFontObj(Items[Index]);
end;

{----------------TFontList.Decrement}

procedure TFontList.Decrement(N: Integer; ParentSectionList: TSectionList);
{called when a character is removed to change the Position figure}
var
  I, J: Integer;
  FO, FO1: TFontObj;
begin
  I := 0;
  while I < Count do
  begin
    FO := TFontObj(Items[I]);
    if FO.Pos > N then
      Dec(FO.Pos);
    if (I > 0) and (TFontObj(Items[I - 1]).Pos = FO.Pos) then
    begin
      FO1 := TFontObj(Items[I - 1]);
      J := ParentSectionList.LinkList.IndexOf(FO1);
      if J >= 0 then
        ParentSectionList.LinkList.Delete(J);
{$IFNDEF NoTabLink}
      if Assigned(FO1.TabControl) then
        if FO.UrlTarget.Id = FO1.UrlTarget.ID then
        begin {if the same link, transfer the TabControl to the survivor}
          FO.TabControl := FO1.TabControl;
          FO.TabControl.OnEnter := FO.EnterEvent;
          FO.TabControl.OnExit := FO.ExitEvent;
          FO1.TabControl := nil;
        end
        else
        begin {remove the TabControl from the TabOrderList}
          J := ParentSectionList.TabOrderList.IndexOfObject(FO1.TabControl);
          if J >= 0 then
            ParentSectionList.TabOrderList.Delete(J);
        end;
{$ENDIF}
      Delete(I - 1);
    end
    else
      Inc(I);
  end;
end;

{----------------TImageObj.Create}

constructor TImageObj.Create(MasterList: TSectionBaseList; Position: Integer; L: TAttributeList);
var
  I: Integer;
  S: ThtString;
  NewSpace: Integer;
  T: TAttribute;
begin
  inherited Create;
  ParentSectionList := MasterList as TSectionList;
  Pos := Position;
  VertAlign := ABottom; {default}
  HorzAlign := ANone; {default}
  NewSpace := -1;
  SpecHeight := -1;
  SpecWidth := -1;
  for I := 0 to L.Count - 1 do
    with TAttribute(L[I]) do
      case Which of
        SrcSy:
          Source := Trim(Name);

        AltSy:
          begin
            SetAlt(CodePage, Name);
            ImageTitle := Alt;
          end;

        IsMapSy:
          IsMap := True;

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
            if S = 'TOP' then
              VertAlign := ATop
            else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then
              VertAlign := AMiddle
            else if S = 'LEFT' then
            begin
              VertAlign := ANone;
              HorzAlign := ALeft;
            end
            else if S = 'RIGHT' then
            begin
              VertAlign := ANone;
              HorzAlign := ARight;
            end;
          end;

        BorderSy:
          begin
            NoBorder := Value = 0;
            BorderSize := Min(Max(0, Value), 10);
          end;

        TranspSy:
          Transparent := LLCorner;

        HeightSy: if System.Pos('%', Name) > 0 then
          begin
            if (Value >= 0) and (Value <= 100) then
            begin
              SpecHeight := Value;
              PercentHeight := True;
            end;
          end
          else
            SpecHeight := Value;

        WidthSy:
          if System.Pos('%', Name) > 0 then
          begin
            if (Value >= 0) and (Value <= 100) then
            begin
              SpecWidth := Value;
              PercentWidth := True;
            end;
          end
          else
            SpecWidth := Value;

        HSpaceSy:
          NewSpace := Min(40, Abs(Value));

        VSpaceSy:
          VSpaceT := Min(40, Abs(Value));

        ActiveSy:
          FHoverImage := True;

        NameSy:
          ParentSectionList.IDNameList.AddObject(Name, Self);
      end;
  if L.Find(TitleSy, T) then
    ImageTitle := T.Name; {has higher priority than Alt loaded above}
  if L.TheID <> '' then
    ParentSectionList.IDNameList.AddObject(L.TheID, Self);

  if NewSpace >= 0 then
    HSpaceL := NewSpace
  else if HorzAlign in [ALeft, ARight] then
    HSpaceL := ImageSpace {default}
  else
    HSpaceL := 0;
  HSpaceR := HSpaceL;
  VSpaceB := VSpaceT;
end;

constructor TImageObj.SimpleCreate(MasterList: TSectionBaseList; const AnURL: ThtString);
begin
  inherited Create;
  ParentSectionList := MasterList as TSectionList;
  VertAlign := ABottom; {default}
  HorzAlign := ANone; {default}
  Source := AnURL;
  NoBorder := True;
  BorderSize := 0;
  SpecHeight := -1;
  SpecWidth := -1;
end;

constructor TImageObj.CreateCopy(AMasterList: TSectionBaseList; T: TImageObj);
begin
  inherited CreateCopy(T);
  ParentSectionList := AMasterList as TSectionList;
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
end;

destructor TImageObj.Destroy;
begin
  if not ParentSectionList.IsCopy then
  begin
    if (Source <> '') and Assigned(OrigImage) then
      ParentSectionList.BitmapList.DecUsage(Source);
    if Swapped and (Image <> OrigImage) then
    begin {not in cache}
      Image.Free;
      Mask.Free;
    end;
    if (OrigImage is TGifImage) and TGifImage(OrigImage).IsCopy then
      OrigImage.Free;
  end;
  FBitmap.Free;
  inherited Destroy;
end;

function TImageObj.GetBitmap: TBitmap;
begin
  Result := nil;
  if Image = ErrorBitmap then
    Exit;
  if (Image is TGifImage) then
    Result := TGifImage(Image).Bitmap
  else if (Image is TBitmap) {$IFNDEF NoGDIPlus}or (Image is TGpBitmap){$ENDIF} then
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
      {$IFNDEF NoGDIPlus}
      else {it's a TGpBitmap}
        FBitmap := TGpBitmap(Image).GetTBitmap
        {$ENDIF NoGDIPlus}
        ;
      Result := FBitmap;
    end;
  end
{$IFNDEF NoMetafile}
  else if (Image is ThtMetaFile) then
    Result := ThtMetaFile(Image).WhiteBGBitmap;
{$ENDIF}
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
            else
              CurrentFrame := 2;
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
  AMask: TBitmap;
  Stream: TMemoryStream;
  I: Integer;
begin
  Transparent := NotTransp;
  AMask := nil;
  if NewImage is TMemoryStream then
    TmpImage := LoadImageFromStream(TMemoryStream(NewImage), Transparent, AMask)
  else
  begin
    // TODO BG, 30.11.2010: do we need this intermediate stream?
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromStream(NewImage);
      TmpImage := LoadImageFromStream(Stream, Transparent, AMask);
    finally
      Stream.Free;
    end;
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
    else {swapped already}
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
    begin {if waiting for image, no longer want it}
      with ParentSectionList.MissingImages do
        for I := 0 to count - 1 do
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

function TImageObj.InsertImage(const UName: ThtString; Error: boolean; var Reformat: boolean): boolean;
var
  TmpImage: TgpObject;
  FromCache, Delay: boolean;
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

      if TmpImage is TGIFImage then
      begin
        if FromCache then {it would be}
          Image := TGifImage.CreateCopy(TGifImage(TmpImage)) {it's in Cache already, make copy}
        else
          Image := TmpImage;
        if not FHoverImage then
        begin
          ParentSectionList.AGifList.Add(Image);
          TGifImage(Image).Animate := True;
          if Assigned(ParentSectionList.Timer) then
            ParentSectionList.Timer.Enabled := True;
        end
        else
          TGifImage(Image).Animate := False;
      end
      else
        Image := TmpImage;
      OrigImage := Image;
    end;
    Missing := False;

    if not ImageKnown then
      Reformat := True; {need to get the dimensions}
  end;
end;

{----------------TImageObj.DrawLogic}

procedure TImageObj.DrawLogic(SectionList: TSectionBaseList; Canvas: TCanvas;
  FO: TFontObjBase; AvailableWidth, AvailableHeight: Integer);
{calculate the height and width}
var
  TmpImage: TgpObject;
  ImHeight, ImWidth: Integer;
  ViewImages, FromCache: boolean;
  Rslt: ThtString;
  ARect: TRect;
  SubstImage: Boolean;
  HasBlueBox: Boolean;

begin
  ViewImages := ParentSectionList.ShowImages;

  TmpImage := Image;
  if ViewImages and not Assigned(TmpImage) then
  begin
    if Source <> '' then
      with SectionList as TSectionList do
      begin
        if not Assigned(GetBitmap) and not Assigned(GetImage) then
          Source := TheOwner.HtmlExpandFilename(Source)
        else if Assigned(ExpandName) then
        begin
          ExpandName(TheOwner, Source, Rslt);
          Source := Rslt;
        end;
        if MissingImages.IndexOf(Uppercase(Source)) = -1 then
          TmpImage := ParentSectionList.GetTheBitmap(Source, Transparent, Mask, FromCache, Missing)
        else
          Missing := True; {already in list, don't request it again}
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
      begin {it's in Cache already, make copy}
        Image := TGifImage.CreateCopy(TGifImage(TmpImage));
        TmpImage := Image;
      end
      else
        Image := TmpImage;
      OrigImage := Image;
      if not FHoverImage then
        ParentSectionList.AGifList.Add(Image)
      else
        TGifImage(Image).Animate := False;
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
      ObjWidth := MulDiv(AvailableWidth - 2 * BorderSize, SpecWidth, 100);
      if SpecHeight >= 0 then
        if PercentHeight then
          ObjHeight := MulDiv(AvailableHeight - 2 * BorderSize, SpecHeight, 100)
        else
          ObjHeight := SpecHeight
      else
        ObjHeight := MulDiv(ObjWidth, ImHeight, ImWidth);
    end
    else if PercentHeight then
    begin
      ObjHeight := MulDiv(AvailableHeight - 2 * BorderSize, SpecHeight, 100);
      if SpecWidth >= 0 then
        ObjWidth := SpecWidth
      else
        ObjWidth := MulDiv(ObjHeight, ImWidth, ImHeight);
    end
    else if (SpecWidth >= 0) and (SpecHeight >= 0) then
    begin {Both width and height specified}
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
    begin {neither height and width specified}
      ObjHeight := ImHeight;
      ObjWidth := ImWidth;
      ImageKnown := not SubstImage;
    end;
  end;

  if (not ViewImages or SubstImage) then
  begin
    if (SpecWidth >= 0) or (SpecHeight >= 0) then
    begin {size to whatever is specified}
      AltWidth := ObjWidth;
      AltHeight := ObjHeight;
    end
    else if FAltW <> '' then {Alt text and no size specified, take as much space as necessary}
    begin
      Canvas.Font.Name := 'Arial'; {use same font as in Draw}
      Canvas.Font.Size := 8;
      ARect := Rect(0, 0, 0, 0);
      DrawTextW(Canvas.Handle, PWideChar(FAltW + CRLF), -1, ARect, DT_CALCRECT);
      with ARect do
      begin
        AltWidth := Right + 16 + 8 + 2;
        AltHeight := Max(16 + 8, Bottom);
      end;
    end
    else
    begin {no Alt text and no size specified}
      AltWidth := Max(ObjWidth, 16 + 8);
      AltHeight := Max(ObjHeight, 16 + 8);
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
    BorderSize := Max(1, BorderSize);

  if (BorderSize > 0) then
  begin
    Inc(ImageHeight, 2 * BorderSize); {extra pixels top and bottom for border}
    Inc(ImageWidth, 2 * BorderSize);
  end;
end;

{----------------TImageObj.DoDraw}

procedure TImageObj.DoDraw(Canvas: TCanvas; XX: Integer; Y: Integer;
  ddImage: TgpObject; ddMask: TBitmap);
{Y relative to top of display here}
var
  DC: HDC;
  Img: TBitmap;
  W, H: Integer;
  PrintTransparent: boolean;
begin
  DC := Canvas.Handle;

  {$IFNDEF NoGDIPlus}
  if TObject(ddImage) is TGPImage then
  begin
    if not ParentSectionList.Printing then
      StretchDrawGpImage(DC, TGPImage(ddImage), XX, Y, ObjWidth, ObjHeight)
    else if not ParentSectionList.PrintBackground and (Positioning = posStatic) then {Printing}
      StretchPrintGpImageOnColor(Canvas, TGpImage(ddImage), XX, Y, ObjWidth, ObjHeight)
    else
      StretchPrintGpImageDirect(DC, TGPImage(ddImage), XX, Y, ObjWidth, ObjHeight,
        ParentSectionList.ScaleX, ParentSectionList.ScaleY);
    Exit;
  end;
  {$ENDIF !NoGDIPlus}

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
{$IFNDEF NoMetafile}
        else if ddImage is ThtMetaFile then
          Canvas.StretchDraw(Rect(XX, Y, XX + ObjWidth, Y + ObjHeight), ThtMetaFile(ddImage));
{$ENDIF}
      end;
    end
    else
    begin {printing}
      if ddImage is TGifImage then
        with TGifImage(ddImage) do
        begin
          ddMask := Mask;
          if Assigned(ddMask) then
            Transparent := TrGif;
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
      begin {printing, not transparent}
        PrintBitmap(Canvas, XX, Y, W, H, TBitmap(ddImage));
      end
{$IFNDEF NoMetafile}
      else if ddImage is ThtMetaFile then
        Canvas.StretchDraw(Rect(XX, Y, XX + ObjWidth, Y + ObjHeight), ThtMetaFile(ddImage));
{$ENDIF}
    end;
  except
  end;
end;

{----------------TImageObj.Draw}

//-- BG ---------------------------------------------------------- 12.06.2010 --
procedure GetRaisedColors(SectionList: TSectionList; Canvas: TCanvas; out Light, Dark: TColor);
var
  White, BlackBorder: boolean;
begin
  with SectionList, Canvas do
  begin
    White := SectionList.Printing or ((Background and $FFFFFF = clWhite) or
      ((Background = clWindow) and (GetSysColor(Color_Window) = $FFFFFF)));
    BlackBorder := Printing and PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
      (GetDeviceCaps(Handle, PLANES) = 1);
  end;
  if BlackBorder then
  begin
    Light := clBlack;
    Dark := clBlack;
  end
  else
  begin
    Dark := clBtnShadow;
    if White then
      Light := clSilver
    else
      Light := clBtnHighLight;
  end;
end;

//BG, 15.10.2010: issue 28: Borland C++ Builder does not accept an array as a result of a function.
// Thus move htStyles and htColors from HtmlUn2.pas to HtmlSubs.pas the only unit where they are used

function htStyles(P0, P1, P2, P3: BorderStyleType): htBorderStyleArray;
begin
  Result[0] := P0;
  Result[1] := P1;
  Result[2] := P2;
  Result[3] := P3;
end;

function htColors(C0, C1, C2, C3: TColor): htColorArray;
begin
  Result[0] := C0;
  Result[1] := C1;
  Result[2] := C2;
  Result[3] := C3;
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
function htRaisedColors(Light, Dark: TColor; Raised: Boolean): htColorArray; overload;
begin
  if Raised then
    Result := htColors(Light, Light, Dark, Dark)
  else
    Result := htColors(Dark, Dark, Light, Light);
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
function htRaisedColors(SectionList: TSectionList; Canvas: TCanvas; Raised: Boolean): htColorArray; overload;
var
  Light, Dark: TColor;
begin
  GetRaisedColors(SectionList, Canvas, Light, Dark);
  Result := htRaisedColors(Light, Dark, Raised);
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
procedure RaisedRectColor(Canvas: TCanvas;
  const ORect, IRect: TRect;
  const Colors: htColorArray;
  Styles: htBorderStyleArray); overload;
{Draws colored raised or lowered rectangles for table borders}
begin
  DrawBorder(Canvas, ORect, IRect, Colors, Styles, clNone, False);
end;

procedure RaisedRectColor(SectionList: TSectionList; Canvas: TCanvas;
  X1, Y1, X2, Y2: Integer;
  Light, Dark: TColor;
  Raised: boolean;
  W: Integer); overload;
begin
  RaisedRectColor(Canvas,
    Rect(X1, Y1, X2, Y2),
    Rect(X1 + W, Y1 + W, X2 - W, Y2 - W),
    htRaisedColors(SectionList, Canvas, Raised),
    htStyles(bssSolid, bssSolid, bssSolid, bssSolid));
end;

procedure RaisedRect(SectionList: TSectionList; Canvas: TCanvas;
  X1, Y1, X2, Y2: Integer;
  Raised: boolean;
  W: Integer);
{Draws raised or lowered rectangles for table borders}
begin
  RaisedRectColor(Canvas,
    Rect(X1, Y1, X2, Y2),
    Rect(X1 + W, Y1 + W, X2 - W, Y2 - W),
    htRaisedColors(SectionList, Canvas, Raised),
    htStyles(bssSolid, bssSolid, bssSolid, bssSolid));
end;

procedure TImageObj.Draw(Canvas: TCanvas; X: Integer; TopY, YBaseline: Integer;
  FO: TFontObj);
var
  TmpImage: TgpObject;
  TmpMask: TBitmap;
  MiddleAlignTop: Integer;
  ViewImages: boolean;
  SubstImage: boolean;
  Ofst: Integer;
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
    else
      TmpMask := nil;
  end
  else
  begin
    TmpImage := DefBitMap;
    TmpMask := nil;
  end;
  SubstImage := not ViewImages or (TmpImage = ErrorBitmap) or (TmpImage = DefBitmap); {substitute image}

  with Canvas do
  begin
    Brush.Style := bsClear;
    Font.Size := 8;
    Font.Name := 'Arial'; {make this a property?}
    Font.Style := Font.Style - [fsBold];
    if SubstImage then
      Ofst := 4
    else
      Ofst := 0;
    if VertAlign = AMiddle then
      MiddleAlignTop := YBaseLine + FO.Descent - (FO.tmHeight div 2) - ((ImageHeight - VSpaceT + VSpaceB) div 2)
    else
      MiddleAlignTop := 0; {not used}

    DrawXX := X;
    case VertAlign of
      ATop, ANone:
        DrawYY := TopY + VSpaceT;
      AMiddle:
        DrawYY := MiddleAlignTop;
      ABottom, ABaseline:
        DrawYY := YBaseLine - ImageHeight - VSpaceB;
    end;
    if (BorderSize > 0) then
    begin
      Inc(DrawXX, BorderSize);
      Inc(DrawYY, BorderSize);
    end;

    if not SubstImage or (AltHeight >= 16 + 8) and (AltWidth >= 16 + 8) then
      DoDraw(Canvas, DrawXX + Ofst, DrawYY + Ofst, TmpImage, TmpMask);
    Inc(DrawYY, ParentSectionList.YOff);
    SetTextAlign(Canvas.Handle, TA_Top);
    if SubstImage and (BorderSize = 0) then
    begin
      Font.Color := FO.TheFont.Color;
    {calc the offset from the image's base to the alt= text baseline}
      case VertAlign of
        ATop, ANone:
          begin
            if FAltW <> '' then
              WrapTextW(Canvas, X + 24, TopY + Ofst + VSpaceT, X + AltWidth - 2, TopY + AltHeight - 1 + VSpaceT, FAltW);
            RaisedRect(ParentSectionList, Canvas, X, TopY + VSpaceT, X + AltWidth, TopY + AltHeight + VSpaceT, False, 1);
          end;
        AMiddle:
          begin {MiddleAlignTop is always initialized}
            if FAltW <> '' then
              WrapTextW(Canvas, X + 24, MiddleAlignTop + Ofst, X + AltWidth - 2, MiddleAlignTop + AltHeight - 1, FAltW);
            RaisedRect(ParentSectionList, Canvas, X, MiddleAlignTop, X + AltWidth, MiddleAlignTop + AltHeight, False, 1);
          end;
        ABottom, ABaseline:
          begin
            if FAltW <> '' then
              WrapTextW(Canvas, X + 24, YBaseLine - AltHeight + Ofst - VSpaceB, X + AltWidth - 2, YBaseLine - VSpaceB - 1, FAltW);
            RaisedRect(ParentSectionList, Canvas, X, YBaseLine - AltHeight - VSpaceB, X + AltWidth, YBaseLine - VSpaceB, False, 1);
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
        if (FAltW <> '') and SubstImage then {output Alt message}
        begin
          YY := DrawYY - ParentSectionList.YOff;
          case VertAlign of
            ATop, ANone:
              WrapTextW(Canvas, DrawXX + 24, YY + Ofst, DrawXX + AltWidth - 2, YY + AltHeight - 1, FAltW);
            AMiddle:
              WrapTextW(Canvas, DrawXX + 24, YY + Ofst, DrawXX + AltWidth - 2, YY + AltHeight - 1, FAltW);
            ABottom, ABaseline:
              WrapTextW(Canvas, DrawXX + 24, YY + Ofst, DrawXX + AltWidth - 2, YY + AltHeight - 1, FAltW);
          end;
        end;
        case VertAlign of {draw border}
          {ALeft, ARight,} ATop, ANone: Rectangle(X, TopY + VSpaceT, X + ImageWidth, TopY + VSpaceT + ImageHeight);
          AMiddle: Rectangle(X, MiddleAlignTop, X + ImageWidth, MiddleAlignTop + ImageHeight);
          ABottom, ABaseline: Rectangle(X, YBaseLine - ImageHeight - VSpaceB, X + ImageWidth, YBaseLine - VSpaceB);
        end;
      finally
        Pen.Color := SaveColor;
        Pen.Width := SaveWidth;
        Pen.Style := SaveStyle;
      end;
    end;
    if (Assigned(MyFormControl) and MyFormControl.Active or FO.Active) or
      ParentSectionList.IsCopy and Assigned(ParentSectionList.LinkDrawnEvent)
      and (FO.UrlTarget.Url <> '') then
    begin
      SaveColor := SetTextColor(Handle, clBlack);
      Brush.Color := clWhite;
      case VertAlign of
        ATop, ANone:
          ARect := Rect(X, TopY + VSpaceT, X + ImageWidth, TopY + VSpaceT + ImageHeight);
        AMiddle:
          ARect := Rect(X, MiddleAlignTop, X + ImageWidth, MiddleAlignTop + ImageHeight);
        ABottom, ABaseline:
          ARect := Rect(X, YBaseLine - ImageHeight - VSpaceB, X + ImageWidth, YBaseLine - VSpaceB);
      end;
      if not ParentSectionList.IsCopy then
      begin
        if ParentSectionList.TheOwner.ShowFocusRect then //MK20091107
          Canvas.DrawFocusRect(ARect);  {draw focus box}
      end
      else
        ParentSectionList.LinkDrawnEvent(ParentSectionList.TheOwner, ParentSectionList.LinkPage,
          FO.UrlTarget.Url, FO.UrlTarget.Target, ARect);
      SetTextColor(handle, SaveColor);
    end;
  end;
end;

{----------------TImageObjList.CreateCopy}

constructor TImageObjList.CreateCopy(AMasterList: TSectionBaseList; T: TImageObjList);
var
  I: Integer;
  Item: TObject;
begin
  inherited create;
  for I := 0 to T.Count - 1 do
  begin
    Item := T.Items[I];
    if Item is TImageObj then
      Add(TImageObj.CreateCopy(AMasterList, TImageObj(Item)))
    else
      Add(TPanelObj.CreateCopy(AMasterList, TPanelObj(Item)));
  end;
end;

function TImageObjList.FindImage(Posn: Integer): TFloatingObj;
{find the image at a given character position}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if TFloatingObj(Items[I]).Pos = Posn then
    begin
      Result := Items[I];
      Exit;
    end;
  Result := nil;
end;

function TImageObjList.GetHeightAt(Posn: Integer; var AAlign: AlignmentType;
  var FlObj: TFloatingObj): Integer;
begin
  FLObj := FindImage(Posn);
  if Assigned(FLObj) then
  begin
    Result := FLObj.ImageHeight + FLObj.VSpaceT + FLObj.VSpaceB;
    AAlign := FLObj.VertAlign;
  end
  else
    Result := -1;
end;

function TImageObjList.GetWidthAt(Posn: Integer; var AAlign: AlignmentType;
  var HSpcL, HSpcR: Integer; var FlObj: TFloatingObj): Integer;
begin
  FLObj := FindImage(Posn);
  if Assigned(FLObj) then
  begin
    Result := FLObj.ImageWidth;
    AAlign := FLObj.HorzAlign;
    HSpcL := FLObj.HSpaceL;
    HSpcR := FLObj.HSpaceR;
  end
  else
    Result := -1;
end;

function TImageObjList.GetImageCountAt(Posn: Integer): Integer;
{Return count of chars before the next image.  0 if at the image, 99999999 if no
 images after Posn}
var
  I, Pos: Integer;
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
    if Pos >= Posn then
      break;
    Inc(I);
  end;
  if I = Count then
    Result := 99999999
  else
    Result := TFloatingObj(Items[I]).Pos - Posn;
end;

{----------------TImageObjList.Decrement}

procedure TImageObjList.Decrement(N: Integer);
{called when a character is removed to change the Position figure}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with TImageObj(Items[I]) do
      if Pos > N then
        Dec(Pos);
end;

{----------------TImageObjList.PtInImage}

function TImageObjList.PtInImage(X: Integer; Y: Integer; var IX, IY, Posn: Integer;
  var AMap, UMap: boolean; var MapItem: TMapItem;
  var ImageObj: TImageObj): boolean;
var
  I, J, LimX, LimY: Integer;
  LIY: Integer;
  Obj: TObject;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Obj := Items[I];
    if Obj is TImageObj then
      with TImageObj(Obj) do
      begin
        IX := X - DrawXX; {these are actual image, box if any is outside}
        LIY := Y - DrawYY;
        LimX := ImageWidth - 2 * BorderSize;
        LimY := ImageHeight - 2 * BorderSize;
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
              for J := 0 to Count - 1 do
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

function TImageObjList.PtInObject(X: Integer; Y: Integer; var Obj: TObject;
  var IX, IY: Integer): boolean;
var
  I, LimX, LimY: Integer;
  LIY: Integer;
  Item: TObject;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item is TImageObj then
      with TImageObj(Item) do
      begin
        IX := X - DrawXX; {these are actual image, box if any is outside}
        LIY := Y - DrawYY;
        LimX := ImageWidth - 2 * BorderSize;
        LimY := ImageHeight - 2 * BorderSize;
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

constructor ThtmlForm.Create(AMasterList: TSectionList; L: TAttributeList);
var
  I: Integer;
begin
  inherited Create;
  MasterList := AMasterList;
  AMasterList.htmlFormList.Add(Self);
  Method := 'Get';
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
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
  if not (Ctrl is THiddenFormControlObj) then
    Inc(NonHiddenCount);
end;

procedure ThtmlForm.DoRadios(Radio: TRadioButtonFormControlObj);
var
  S: ThtString;
  Ctrl: TFormControlObj;
  I: Integer;
begin
  if Radio.FName <> '' then
  begin
    S := Radio.FName;
    for I := 0 to ControlList.Count - 1 do
    begin
      Ctrl := TFormControlObj(ControlList.Items[I]);
      if (Ctrl is TRadioButtonFormControlObj) and (Ctrl <> Radio) then
        if CompareText(Ctrl.FName, S) = 0 then
        begin
          TFormRadioButton(Ctrl.TheControl).Checked := False;
          TFormRadioButton(Ctrl.TheControl).TabStop := False; {first check turns off other tabstops}
          TRadioButtonFormControlObj(Ctrl).DoOnchange;
        end;
    end;
  end;
end;

procedure ThtmlForm.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  S: ThtString;
  Ctrl: TFormControlObj;
  I: Integer;
  List: TList;
begin
  if (Key in [vk_up, vk_down, vk_left, vk_right]) and (Sender is TFormRadioButton) then
  begin
    S := TFormRadioButton(Sender).IDName;
    List := TList.Create;
    try
      for I := 0 to ControlList.Count - 1 do
      begin
        Ctrl := TFormControlObj(ControlList.Items[I]);
        if (Ctrl is TRadioButtonFormControlObj) and
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
        else if I < List.Count - 1 then
          Inc(I);
        TFormRadioButton(List.Items[I]).SetFocus;
      end;
    finally
      List.Free;
    end;
  end
  else {send other keys to THtmlViewer}
    MasterList.TheOwner.KeyDown(Key, Shift);
end;

procedure ThtmlForm.ResetControls;
var
  I: Integer;
begin
  for I := 0 to ControlList.Count - 1 do
    TFormControlObj(ControlList.Items[I]).ResetToValue;
end;

procedure ThtmlForm.ControlKeyPress(Sender: TObject; var Key: Char);
begin
  if (Sender is ThtEdit) then
    if (Key = #13) then
    begin
      SubmitTheForm('');
      Key := #0;
    end;
end;

function ThtmlForm.GetFormSubmission: ThtStringList;
var
  I, J: Integer;
  S: ThtString;
begin
  Result := ThtStringList.Create;
  for I := 0 to ControlList.Count - 1 do
    with TFormControlObj(ControlList.Items[I]) do
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

procedure ThtmlForm.SubmitTheForm(const ButtonSubmission: ThtString);
var
  I, J: Integer;
  SL: ThtStringList;
  S: ThtString;
begin
  if Assigned(MasterList.SubmitForm) then
  begin
    SL := ThtStringList.Create;
    for I := 0 to ControlList.Count - 1 do
      with TFormControlObj(ControlList.Items[I]) do
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

procedure ThtmlForm.SetFormData(SL: ThtStringList);
var
  I, J, K, Index: Integer;
  Value: ThtString;
  FormControl: TFormControlObj;
begin
  for I := 0 to ControlList.Count - 1 do
  begin
    FormControl := TFormControlObj(ControlList[I]);
    FormControl.SetDataInit;
    Index := 0;
    for J := 0 to SL.Count - 1 do
      if CompareText(FormControl.FName, SL.Names[J]) = 0 then
      begin
        K := Pos('=', SL[J]);
        if K > 0 then
        begin
          Value := Copy(SL[J], K + 1, Length(SL[J]) - K);
          FormControl.SetData(Index, Value);
          Inc(Index);
        end;
      end;
  end;
end;

procedure ThtmlForm.SetSizes(Canvas: TCanvas);
var
  I: Integer;
begin
  for I := 0 to ControlList.Count - 1 do
    TFormControlObj(ControlList.Items[I]).SetHeightWidth(Canvas);
end;

{----------------TFormControlObj.Create}

constructor TFormControlObj.Create(AMasterList: TSectionList;
  Position: Integer; L: TAttributeList);
var
  I: Integer;
begin
  inherited Create;
  Pos := Position;
  MasterList := AMasterList;
  if not Assigned(CurrentForm) then {maybe someone forgot the <form> tag}
    CurrentForm := ThtmlForm.Create(AMasterList, nil);
  AMasterList.FormControlList.Add(Self);
  MyForm := CurrentForm;
  for I := 0 to L.Count - 1 do
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
            {Adding leading 0's to the number ThtString allows it to be sorted numerically,
             and the Count takes care of duplicates}
            with AMasterList.TabOrderList do
              AddObject(Format('%.5d%.3d', [Value, Count]), Self);
        TitleSy: FTitle := Name;
        DisabledSy: Disabled := (Lowercase(Name) <> 'no') and (Name <> '0');
        ReadonlySy: ReadOnly := True;
      end;

  if L.TheID <> '' then
    MasterList.IDNameList.AddObject(L.TheID, Self);
  AttributeList := L.CreateStringList;
  FormAlign := ABottom; {ABaseline set individually}
  MyForm.InsertControl(Self);
end;

constructor TFormControlObj.CreateCopy(T: TFormControlObj);
begin
  inherited Create;
  System.Move(T.Pos, Pos, DWord(@FControl) - DWord(@Pos));
end;

destructor TFormControlObj.Destroy;
begin
  if Assigned(FControl) then {hidden controls are Nil}
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  AttributeList.Free;
  PaintBitmap.Free;
  inherited Destroy;
end;

procedure TFormControlObj.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MasterList.TheOwner.ControlMouseMove(Self, Shift, X, Y);
end;

function TFormControlObj.GetYPosition: Integer;
begin
  Result := YValue;
end;

procedure TFormControlObj.ProcessProperties(Prop: TProperties);
var
  MargArrayO: TVMarginArray;
  MargArray: TMarginArray;
  Align: AlignmentType;
  EmSize, ExSize: Integer;
begin
  Prop.GetVMarginArray(MargArrayO);
  EmSize := Prop.EmSize;
  ExSize := Prop.ExSize;
  PercentWidth := (VarIsStr(MargArrayO[Width])) and (System.Pos('%', MargArrayO[Width]) > 0);
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

  if MargArray[Width] > 0 then {excludes IntNull and Auto}
    if PercentWidth then
    begin
      if MargArray[Width] <= 100 then
        FWidth := MargArray[Width]
      else
        PercentWidth := False;
    end
    else
      FWidth := MargArray[Width];
  if MargArray[Height] > 0 then
    FHeight := MargArray[Height] - BordT - BordB;
  if Prop.GetVertAlign(Align) then
    FormAlign := Align;
  BkColor := Prop.GetBackgroundColor;
end;

procedure TFormControlObj.EnterEvent(Sender: TObject);
{Once form control entered, insure all form controls are tab active}
{$IFNDEF FastRadio}
var
  I: Integer;
{$ENDIF}
begin
  if MasterList.IsCopy then
    Exit;
  Active := True;
  MasterList.PPanel.Invalidate;
  MasterList.ControlEnterEvent(Self);
{$IFNDEF FastRadio}
  with MasterList.FormControlList do
  begin
    for I := 0 to Count - 1 do
      with TFormControlObj(Items[I]) do
        if not ShowIt and Assigned(FControl) then
        begin
          FControl.Show; {makes it tab active}
          FControl.Left := -4000; {even if it can't be seen}
        end;
  end;
{$ENDIF}
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
{$IFNDEF FastRadio}
  MasterList.AdjustFormControls;
{$ENDIF}
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

procedure TFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
begin end;

procedure TFormControlObj.ResetToValue;
begin end;

function TFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := False;
end;

procedure TFormControlObj.SetDataInit;
begin
end;

procedure TFormControlObj.SetData(Index: Integer; const V: ThtString);
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

function TFormControlObj.GetAttribute(const AttrName: ThtString): ThtString;
begin
  Result := AttributeList.Values[AttrName];
end;

{----------------TImageFormControlObj.Create}

constructor TImageFormControlObj.Create(AMasterList: TSectionList;
  Position: Integer; L: TAttributeList);
var
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(AMasterList, Position, L);
  XPos := -1; {so a button press won't submit image data}

  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := ThtButton.Create(PntPanel);
  with ThtButton(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
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

function TImageFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := (Index <= 1) and (XPos >= 0);
  if Result then
  begin
    S := '';
    if FName <> '' then
      S := FName + '.';
    if Index = 0 then
      S := S + 'x=' + IntToStr(XPos)
    else
    begin {index = 1}
      S := S + 'y=' + IntToStr(YPos);
      XPos := -1;
    end;
  end;
end;

{----------------THiddenFormControlObj.GetSubmission}

function THiddenFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := Index = 0;
  if Result then
    S := FName + '=' + Value;
end;

procedure THiddenFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  Value := V;
end;

{----------------TEditFormControlObj.Create}

constructor TEditFormControlObj.Create(AMasterList: TSectionList;
  Position: Integer; L: TAttributeList; const Typ: ThtString; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  Tmp: TMyFont;
begin
  inherited Create(AMasterList, Position, L);
  CodePage := Prop.CodePage;
  EditSize := 15;
  if L.Find(SizeSy, T) then
  begin
    if T.Value > 0 then
      EditSize := T.Value
    else
    begin {see if it's comma delimited list}
      I := Min(System.Pos(',', T.Name), System.Pos(' ', T.Name));
      if I > 1 then
        EditSize := StrToIntDef(copy(T.Name, 1, I - 1), 20);
    end;
  end;
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := ThtEdit.Create(PntPanel);
  with ThtEdit(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
    Width := 120;
    if (Prop.GetBorderStyle <> bssNone) then
      BorderStyle := bsNone;
    Parent := PntPanel;
    Tmp := Prop.GetFont;
    Font.Assign(Tmp);
    FHeight := Height; {Height can change when font assigned}
    tmAveCharWidth := Tmp.tmAveCharWidth;
    Tmp.Free;
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
  ThtEdit(FControl).Text := Value;
end;

procedure TEditFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  H2, Addon: Integer;
  ARect: TRect;
begin
  if ThtEdit(FControl).BorderStyle <> bsNone then
    Addon := 4 {normal 3D border}
  else
    Addon := 2; {inline border, 3D Border removed}
  with ThtEdit(FControl) do
  begin
    Canvas.Font := Font;
    H2 := Abs(Font.Height);
    if BorderStyle <> bsNone then
      FormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, MasterList.PrintMonoBlack, False, Color)
    else
      FillRectWhite(Canvas, X1, Y1, X1 + Width, Y1 + Height, Color);
    SetTextAlign(Canvas.handle, TA_Left);
    SetBkMode(Canvas.Handle, Windows.Transparent);
    Canvas.Brush.Style := bsClear;
    ARect := Rect(X1 + Addon, Y1, X1 + Width - (Addon div 2), Y1 + Height);
    ThtCanvas(Canvas).htTextRect(ARect, ARect.Left, Y1 + (Height - H2) div 2 - 1, Text);
  end
end;

function TEditFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := Index = 0;
  if Result then
    S := FName + '=' + ThtEdit(FControl).Text;
end;

procedure TEditFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  ThtEdit(FControl).Text := V;
end;

procedure TEditFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with ThtEdit(FControl) do
  begin
    Canvas.Font := Font;
    if not PercentWidth then
      if (FWidth >= 10) then
        Width := FWidth
      else
        Width := tmAveCharWidth * EditSize + 23
    else
    begin {percent width set later}
      Left := -4000;
      Width := 10;
    end;
    Height := Max(FHeight, Max(Canvas.TextHeight('A'), 10));
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
  Position: Integer; L: TAttributeList; const Typ: ThtString;
  Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
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
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := ThtButton.Create(PntPanel);
  with ThtButton(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
    Tmp := Prop.GetFont;
    Font.Assign(Tmp);
    Tmp.Free;
    OnClick := Self.ButtonClick;
    if Which = Browse then
      Caption := 'Browse...'
    else
      Caption := Value;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
  end;
  FControl.Parent := PntPanel;
{$IFDEF UseElPack}
  ThtButton(FControl).Color := clBtnFace;
{$ENDIF}
end;

procedure TButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  H2: Integer;
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
        PrintBitmap(Canvas, X1, Y1, Width, Height, PaintBitmap);
      except end;
    end
    else
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font := Font;
      FormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, True, MasterList.PrintMonoBlack, False, clWhite);
      H2 := Canvas.TextHeight('A');
      SetTextAlign(Canvas.handle, TA_Center + TA_Top);
      ThtCanvas(Canvas).htTextRect(Rect(X1, Y1, X1 + Width, Y1 + Height), X1 + (Width div 2), Y1 + (Height - H2) div 2, Value);
    end;
  end;
end;

procedure TButtonFormControlObj.ButtonClick(Sender: TObject);
var
  S: ThtString;
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
      MyForm.SubmitTheForm(S + '=' + Value);
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
    else
      Height := Canvas.TextHeight('A') + 8;
    if (FWidth >= 10) and not PercentWidth then {percent width set later}
      Width := FWidth
    else
      Width := Canvas.TextWidth(Caption) + 20;
  end;
end;

{----------------TCheckBoxFormControlObj.Create}

constructor TCheckBoxFormControlObj.Create(AMasterList: TSectionList;
  Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(AMasterList, Position, L);
  if Value = '' then
    Value := 'on';
  FormAlign := ABaseline;
  if L.Find(CheckedSy, T) then
    IsChecked := True;
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := TFormCheckBox.Create(PntPanel);
  with TFormCheckBox(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
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
  ThtCheckBox(FControl).Checked := IsChecked;
end;

procedure TCheckBoxFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  x, y: Integer;
begin
  with ThtCheckBox(FControl) do
  begin
    FormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, MasterList.PrintMonoBlack, Disabled, clWhite);
    if Checked then
      with Canvas do
      begin
        Pen.Color := clBlack;
        x := X1 + 3; y := Y1 + Height div 2;
        MoveTo(x, y);
        LineTo(x + 2, y + 2);
        LineTo(x + 6, y - 2);
      end;
  end;
end;

function TCheckBoxFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := (Index = 0) and ThtCheckBox(FControl).Checked;
  if Result then
    S := FName + '=' + Value;
end;

procedure TCheckBoxFormControlObj.SetDataInit;
begin
  ThtCheckBox(FControl).Checked := False; {not checked unless later data says so}
end;

procedure TCheckBoxFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  if htCompareText(V, Value) = 0 then
    ThtCheckBox(FControl).Checked := True;
end;

procedure TCheckBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  WasChecked := ThtCheckBox(FControl).Checked;
end;

procedure TCheckBoxFormControlObj.DoOnChange;
begin
  if ThtCheckBox(FControl).Checked <> WasChecked then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TRadioButtonFormControlObj.Create}

constructor TRadioButtonFormControlObj.Create(AMasterList: TSectionList;
  Position: Integer; L: TAttributeList; ACell: TCellBasic);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
  Ctrl: TFormControlObj;
  I: Integer;
  SetTabStop: boolean;
begin
  inherited Create(AMasterList, Position, L);
  MyCell := ACell;
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := TFormRadioButton.Create(PntPanel);
  FormAlign := ABaseline;
  if L.Find(CheckedSy, T) then
    IsChecked := True;
  with TFormRadioButton(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
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
    Parent := PntPanel; {must precede Checked assignment}

  {The Tabstop for the first radiobutton in a group will be set in case no
   radiobuttons that follow are checked.  This insures that the tab key can
   access the group}
    SetTabStop := True;
  {Examine all other radiobuttons in this group (same FName)}
    for I := 0 to MyForm.ControlList.Count - 1 do
    begin
      Ctrl := TFormControlObj(MyForm.ControlList.Items[I]);
      if (Ctrl is TRadioButtonFormControlObj)
        and (TRadioButtonFormControlObj(Ctrl).TheControl <> FControl) then {skip the current radiobutton}
        if CompareText(Ctrl.FName, FName) = 0 then {same group}
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
    Checked := IsChecked; {must precede setting OnClick}
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

procedure TRadioButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  OldStyle: TPenStyle;
  OldWidth, XW, YH, XC, YC: Integer;
  OldColor, OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  MonoBlack: boolean;
begin
  with Canvas do
  begin
    XW := X1 + 14;
    YH := Y1 + 14;
    OldStyle := Pen.Style;
    OldWidth := Pen.Width;
    OldBrushStyle := Brush.Style;
    OldBrushColor := Brush.Color;
    MonoBlack := MasterList.PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
      (GetDeviceCaps(Handle, PLANES) = 1);
    if Disabled and not MonoBlack then
      Brush.Color := clBtnFace
    else
      Brush.Color := clWhite;
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
      XC := X1 + 7;
      YC := Y1 + 7;
      Ellipse(XC - 2, YC - 2, XC + 2, YC + 2);
      Brush.Color := OldColor;
    end;
    Pen.Width := OldWidth;
    Pen.Style := OldStyle;
    Brush.Color := OldBrushColor;
    Brush.Style := OldBrushStyle;
  end;
end;

function TRadioButtonFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := (Index = 0) and TFormRadioButton(TheControl).Checked;
  if Result then
    S := FName + '=' + Value;
end;

procedure TRadioButtonFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  if htCompareText(V, Value) = 0 then
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

constructor TCellBasic.Create(Master: TSectionBaseList);
begin
  inherited Create;
  MasterList := Master as TSectionList;
end;

{----------------TCellBasic.CreateCopy}

constructor TCellBasic.CreateCopy(AMasterList: TSectionBaseList; T: TCellBasic);
var
  I: Integer;
  Tmp, Tmp1: TSectionBase;
begin
  inherited Create;
  MasterList := AMasterList as TSectionList;
  OwnersTag := T.OwnersTag;
  for I := 0 to T.Count - 1 do
  begin
    Tmp := T.Items[I];
    Tmp1 := TSectionClass(Tmp.ClassType).CreateCopy(AMasterList, Tmp);
    Add(Tmp1, 0);
  end;
end;

{----------------TCellBasic.Add}

procedure TCellBasic.Add(Item: TSectionBase; TagIndex: Integer);
begin
  if Assigned(Item) then
  begin
    if (Item is TSection) and Assigned(TSection(Item).XP) then {XP not assigned if printing}
    begin
      TSection(Item).ProcessText(TagIndex);
      if not (Item is TPreFormated) and (TSection(Item).Len = 0)
        and not TSection(Item).AnchorName and (TSection(Item).ClearAttr = clrNone) then
      begin
        TSection(Item).CheckFree;
        Item.Free; {discard empty TSections that aren't anchors}
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
  I: Integer;
  Done: boolean;
begin
  Result := False;
  I := Count - 1; {find the preceding block that isn't absolute positioning}
  Done := False;
  while (I >= 0) and not Done do
  begin
    TB := Items[I];
    if (TB is TBlock) and (TBlock(TB).Positioning <> PosAbsolute) then
      Done := True
    else
      Dec(I);
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

function TCellBasic.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj};
  var ATitle: ThtString): guResultType;
{Y is absolute}
var
  I: Integer;
begin
  Result := [];
  FormControl := nil;
  UrlTarg := nil;
  for I := 0 to Count - 1 do
  begin
    with Items[I] do
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

function TCellBasic.PtInObject(X: Integer; Y: Integer; var Obj: TObject;
  var IX, IY: Integer): boolean;
{Y is absolute}
var
  I: Integer;
begin
  Result := False;
  Obj := nil;
  for I := 0 to Count - 1 do
    with Items[I] do
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

function TCellBasic.FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
  var XR: Integer; var YR: Integer; var Ht: Integer;
  var Intext: boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    with Items[I] do
    begin
      if (Y >= DrawTop) and (Y < DrawBot) then
        Result := Items[I].FindCursor(Canvas, X, Y, XR, YR, Ht, InText);
      if Result >= 0 then
        Break;
    end;
  end;
end;

procedure TCellBasic.AddSectionsToList;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AddSectionsToList;
end;

{----------------TCellBasic.FindString}

function TCellBasic.FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FindString(From, ToFind, MatchCase);
    if Result >= 0 then
      Break;
  end;
end;

{----------------TCellBasic.FindStringR}

function TCellBasic.FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
  begin
    Result := Items[I].FindStringR(From, ToFind, MatchCase);
    if Result >= 0 then
      Break;
  end;
end;

{----------------TCellBasic.FindSourcePos}

function TCellBasic.FindSourcePos(DocPos: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FindSourcePos(DocPos);
    if Result >= 0 then
      Break;
  end;
end;

procedure TCellBasic.FormTree(const Indent: ThtString; var Tree: ThtString);
var
  I: Integer;
  Item: TSectionBase;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item is TBlock then
      TBlock(Item).FormTree(Indent, Tree)
    else if Item is TSection then
      Tree := Tree + Indent + Copy(TSection(Item).BuffS, 1, 10) + ^M + ^J
    else
      Tree := Tree + Indent + '----'^M + ^J;
  end;
end;

{----------------TCellBasic.GetChAtPos}

function TCellBasic.GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
var
  I: Integer;
begin
  Result := False;
  if (Pos >= StartCurs) and (Pos <= StartCurs + Len) then
    for I := 0 to Count - 1 do
    begin
      Result := TSectionBase(Items[I]).GetChAtPos(Pos, Ch, Obj);
      if Result then
        Break;
    end;
end;

{----------------TCellBasic.CopyToClipboard}

procedure TCellBasic.CopyToClipboard;
var
  I: Integer;
  SLE, SLB: Integer;
begin
  if not Assigned(MasterList) then
    Exit; {dummy cell}
  SLB := MasterList.SelB;
  SLE := MasterList.SelE;
  if SLE <= SLB then
    Exit; {nothing to do}

  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      if (SLB >= StartCurs + Len) then
        Continue;
      if (SLE <= StartCurs) then
        Break;
      CopyToClipboard;
    end;
end;

{----------------TCellBasic.DoLogic}

function TCellBasic.DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
  var ScrollWidth: Integer; var Curs: Integer): Integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  I, Sw, TheCount: Integer;
  H: Integer;
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
      Inc(H, Items[I].DrawLogic(Canvas, 0, Y + H, 0, 0, Width, AHeight, BlHt, IMgr, Sw, Curs));
      ScrollWidth := Max(ScrollWidth, Sw);
      Inc(I);
    except
      on E: EProcessError do
      begin
        // Yunqa.de - Don't want message dialog for individual errors.
        // Yunqa.de MessageDlg(e.Message, mtError, [mbOK], 0);
        Delete(I);
        Dec(TheCount);
      end;
    end;
  end;
  Len := Curs - StartCurs;
  Result := H;
end;

{----------------TCellBasic.MinMaxWidth}

procedure TCellBasic.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
{Find the Width the cell would take if no wordwrap, Max, and the width if wrapped
 at largest word, Min}
var
  I, Mn, Mx: Integer;
begin
  Max := 0; Min := 0;
  for I := 0 to Count - 1 do
  begin
    Items[I].MinMaxWidth(Canvas, Mn, Mx);
    Max := Math.Max(Max, Mx);
    Min := Math.Max(Min, Mn);
  end;
end;

{----------------TCellBasic.Draw}

function TCellBasic.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer;
  Y, XRef, YRef: Integer): Integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
var
  I: Integer;
  H: Integer;
begin
  H := Y;
  for I := 0 to Count - 1 do
    H := Items[I].Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
  Result := H;
end;

{----------------TBlock.Create}

constructor TBlock.Create(Master: TSectionList; Prop: TProperties;
  AnOwnerCell: TCellBasic; Attributes: TAttributeList);
var
  Clr: ClearAttrType;
  S: ThtString;
begin
  inherited Create(Master, Prop);
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
  //DisplayNone := Prop.DisplayNone;
  BlockTitle := Prop.PropTitle;
  if not (Self is TBodyBlock) and not (Self is TTableAndCaptionBlock)
    and Prop.GetBackgroundImage(S) and (S <> '') then
  begin {body handles its own image}
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
    ZIndex := 10 * Prop.GetZIndex;
    if (Positioning = posAbsolute) and (ZIndex = 0) then
      ZIndex := 1; {abs on top unless otherwise specified}
    if Positioning = posAbsolute then
      MyCell.IMgr := TIndentManager.Create;
  end;
  if (FloatLR in [ALeft, ARight]) and (ZIndex = 0) then
    ZIndex := 1;
  TagClass := Prop.PropTag + '.' + Prop.PropClass;
  if not (Self is TTableBlock) and not (Self is TTableAndCaptionBlock) then
    CollapseMargins;
  if Assigned(Attributes) and (Attributes.TheID <> '') then
    Master.IDNameList.AddObject(Attributes.TheID, Self);
  HideOverflow := Prop.IsOverflowHidden;
  if Prop.Props[TextAlign] = 'right' then
    Justify := Right
  else if Prop.Props[TextAlign] = 'center' then
    Justify := Centered
  else
    Justify := Left;
end;

procedure TBlock.CollapseMargins;
{adjacent vertical margins need to be reduced}
var
  TopAuto, Done: boolean;
  TB: TSectionBase;
  LastMargin, Negs, I: Integer;
  Tag: ThtString;
begin
  ConvVertMargins(MargArrayO, 400, {height not known at this point}
    EmSize, ExSize, MargArray, TopAuto, BottomAuto);
  if Positioning = posAbsolute then
  begin
    if TopAuto then
      MargArray[MarginTop] := 0;
  end
  else if FloatLR in [ALeft, ARight] then {do nothing}
  else
    with OwnerCell do
    begin
      I := Count - 1; {find the preceding block that isn't absolute positioning}
      Done := False;
      while (I >= 0) and not Done do
      begin
        TB := TSectionBase(Items[I]);
        if ((TB is TBlock) and (TBlock(TB).Positioning <> PosAbsolute))
          or not (TB is TBlock) then {allow for a TSection}
          Done := True
        else
          Dec(I);
      end;
      Tag := OwnerCell.OwnersTag;
      if I < 0 then
      begin {no previous non absolute block, remove any Auto paragraph space}
        if TopAuto then
        begin
          if (Tag = 'li') then
          begin
            MargArray[MarginTop] := 0;
          end
          else
            MargArray[MarginTop] := 0;
        end
        else if (Tag = 'default') or (Tag = 'body') then
          MargArray[MarginTop] := Max(0, MargArray[MarginTop] - OwnerCell.Owner.MargArray[MarginTop]);
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
          if LastMargin >= 0 then {figure out how many are negative}
            if MargArray[MarginTop] >= 0 then
              Negs := 0
            else
              Negs := 1
          else if MargArray[MarginTop] >= 0 then
            Negs := 1
          else
            Negs := 2;
          case Negs of
            0: MargArray[MarginTop] := Max(MargArray[MarginTop], LastMargin);
            1: MargArray[MarginTop] := MargArray[MarginTop] + LastMargin;
            2: MargArray[MarginTop] := Min(MargArray[MarginTop], LastMargin);
          end;
        end
        else if (Tag = 'li') and TopAuto
          and ((Pos('ul.', TagClass) = 1) or (Pos('ol.', TagClass) = 1)) then
          MargArray[MarginTop] := 0; {removes space from nested lists}
      end;
    end;
end;

//-- BG ---------------------------------------------------------- 09.10.2010 --
procedure TBlock.ContentMinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
begin
  MyCell.MinMaxWidth(Canvas, Min, Max);
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
procedure TBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; var AutoCount: Integer);
begin
  StyleUn.ConvMargArray(MargArrayO, BaseWidth, BaseHeight, EmSize, ExSize, BorderStyle, BorderWidth, AutoCount, MargArray);
end;

{----------------TBlock.CreateCopy}

constructor TBlock.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
var
  TT: TBlock;
begin
  inherited CreateCopy(AMasterList, T);
  TT := T as TBlock;
  System.Move(TT.MargArray, MargArray, DWord(@Converted) - DWord(@MargArray) + Sizeof(Converted));
  MyCell := TBlockCell.CreateCopy(AMasterList, TT.MyCell);
  MyCell.Owner := Self;
  DrawList := TList.Create;
  TagClass := TT.TagClass;
  if Assigned(TT.BGImage) and (AMasterlist as TSectionList).PrintTableBackground then
    BGImage := TImageObj.CreateCopy(AMasterList, TT.BGImage);
  MargArrayO := TT.MargArrayO;
  if Positioning = posAbsolute then
    MyCell.IMgr := TIndentManager.Create;
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

procedure TBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
var
  MinCell, MaxCell: Integer;
  LeftSide, RightSide, AutoCount: Integer;
begin
  if (Display = pdNone) or (Positioning = PosAbsolute) then
  begin
    Min := 0;
    Max := 0;
    Exit;
  end;
{$ifdef DO_BLOCK_INLINE}
  if Display = pdInline then
  begin
    inherited;
    exit;
  end;
{$endif}

  ConvMargArray(0, 400, AutoCount);
  HideOverflow := HideOverflow and (MargArray[Width] <> Auto) and (MargArray[Width] > 20);
  if HideOverflow then
  begin
    MinCell := MargArray[Width];
    MaxCell := MinCell;
  end
  else
    ContentMinMaxWidth(Canvas, MinCell, MaxCell);
  if MargArray[MarginLeft] = Auto then
    MargArray[MarginLeft] := 0;
  if MargArray[MarginRight] = Auto then
    MargArray[MarginRight] := 0;
  if MargArray[Width] = Auto then
    MargArray[Width] := 0;
  LeftSide := MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
  RightSide := MargArray[MarginRight] + MargArray[BorderRightWidth] + MargArray[PaddingRight];
  Min := Math.Max(MinCell, MargArray[Width]) + LeftSide + RightSide;
  if MargArray[Width] > 0 then
    Max := Min
  else
    Max := Math.Max(MaxCell, MargArray[Width]) + LeftSide + RightSide;
end;

{----------------TBlock.GetURL}

function TBlock.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType;
begin
  case Display of
    pdNone: Result := [];
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
{$endif}
  else
    if (BlockTitle <> '') and PtInRect(MyRect, Point(X, Y - ParentSectionList.YOFF)) then
    begin
      ATitle := BlockTitle;
      Include(Result, guTitle);
    end;
    Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  end;
end;

{----------------TBlock.FindString}

function TBlock.FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
begin
  case Display of
    pdNone: Result := -1;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited FindString(From, ToFind, MatchCase);
{$endif}
  else
    Result := MyCell.FindString(From, ToFind, MatchCase);
  end;
end;

{----------------TBlock.FindStringR}

function TBlock.FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
begin
  case Display of
    pdNone: Result := -1;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited FindStringR(From, ToFind, MatchCase);
{$endif}
  else
    Result := MyCell.FindStringR(From, ToFind, MatchCase);
  end;
end;

{----------------TBlock.FindCursor}

function TBlock.FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
  var XR: Integer; var YR: Integer; var CaretHt: Integer;
  var Intext: boolean): Integer;
var
  I: Integer;
begin
  case Display of
    pdNone: Result := -1;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited FindCursor(Canvas, X, Y, XR, YR, CaretHt, Intext);
{$endif}
  else
    {check this in z order}
    Result := -1;
    with DrawList do
      for I := Count - 1 downto 0 do
        with TSectionBase(Items[I]) do
        begin
          if (Y >= DrawTop) and (Y < DrawBot) then
          begin
            Result := FindCursor(Canvas, X, Y, XR, YR, CaretHt, Intext);
            if Result >= 0 then
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

function TBlock.PtInObject(X: Integer; Y: Integer; var Obj: TObject;
  var IX, IY: Integer): boolean;
{Y is absolute}
var
  I: Integer;
begin
  case Display of
    pdNone: Result := False;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited PtInObject(X, Y, Obj, IX, IY);
{$endif}
  else
    {check this in z order}
    Result := False;
    Obj := nil;
    with DrawList do
      for I := Count - 1 downto 0 do
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

function TBlock.GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
  case Display of
    pdNone:   Result := False;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited GetChAtPos(Pos, Ch, Obj);
{$endif}
  else
    Result := MyCell.GetChAtPos(Pos, Ch, Obj);
  end;
end;

////-- BG ---------------------------------------------------------- 16.09.2009 --
//function TBlock.getDisplay: TPropDisplay;
//begin
//  Result := pdBlock;
//end;

function TBlock.getParentSectionList: TSectionList;
begin
  Result := TSectionList(inherited ParentSectionList);
end;

function TBlock.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean;
begin
  case Display of
    pdNone:   Result := False;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited CursorToXY(Canvas, Cursor, X, Y);
{$endif}
  else
    Result := MyCell.CursorToXY(Canvas, Cursor, X, Y);
  end;
end;

function TBlock.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
begin
  case Display of
    pdNone:   Result := -1;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited FindDocPos(SourcePos, Prev);
{$endif}
  else
    Result := MyCell.FindDocPos(SourcePos, Prev);
  end;
end;

function TBlock.FindSourcePos(DocPos: Integer): Integer;
begin
  case Display of
    pdNone:   Result := -1;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited FindSourcePos(DocPos);
{$endif}
  else
    Result := MyCell.FindSourcePos(DocPos);
  end;
end;

procedure TBlock.CopyToClipboard;
begin
  case Display of
    pdNone:;
{$ifdef DO_BLOCK_INLINE}
    pdInline: inherited;
{$endif}
  else
    MyCell.CopyToClipboard;
    if (Pos('p.', TagClass) = 1) and (ParentSectionList.SelE > MyCell.StartCurs + MyCell.Len) then
      ParentSectionList.CB.AddTextCR('', 0);
  end;
end;

{----------------TBlock.FindWidth}

function TBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer;
var
  Marg2: Integer;
  MinWidth, MaxWidth: Integer;

  function BordPad: Integer;
  begin
    Result := MargArray[BorderLeftWidth] + MargArray[BorderRightWidth] +
              MargArray[PaddingLeft] + MargArray[PaddingRight];
  end;

  function BordWidth: Integer;
  begin
    Result := MargArray[BorderLeftWidth] + MargArray[BorderRightWidth] +
              MargArray[PaddingLeft] + MargArray[PaddingRight] +
              MargArray[MarginLeft] + MargArray[MarginRight];
  end;

  procedure CalcWidth;
  begin
    if Positioning = posAbsolute then
      MargArray[Width] := Max(MinWidth, AWidth - BordWidth - LeftP)
    else if (FloatLR in [ALeft, ARight]) then
      MargArray[Width] := Min(MaxWidth, AWidth - BordWidth)
    else
      MargArray[Width] := Max(MinWidth, AWidth - BordWidth);
  end;

  procedure CalcMargRt;
  begin
    MargArray[MarginRight] := Max(0, AWidth - BordPad - MargArray[MarginLeft] - MargArray[Width]);
  end;

  procedure CalcMargLf;
  begin
    MargArray[MarginLeft] := Max(0, AWidth - BordPad - MargArray[MarginRight] - MargArray[Width]);
  end;

begin
  ContentMinMaxWidth(Canvas, MinWidth, MaxWidth);
  HideOverflow := HideOverflow and (MargArray[Width] <> Auto) and (MargArray[Width] > 20);
  case AutoCount of
    0:
      begin
        if not HideOverflow then
          MargArray[Width] := Max(MinWidth, MargArray[Width]);
        if (Justify in [centered, Right]) and (Positioning = posStatic)
          and not (FloatLR in [ALeft, ARight]) and
          (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
        begin
          Marg2 := Max(0, AWidth - MargArray[Width] - BordPad);
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
          MargArray[Width] := Max(MargArray[Width], MinWidth);
        if MargArray[MarginRight] = Auto then
          if (FloatLR in [ALeft, ARight]) then
            MargArray[MarginRight] := 0
          else
            CalcMargRt
        else
          CalcMargLf;
      end;
    2: if MargArray[Width] = Auto then
      begin
        if MargArray[MarginLeft] = Auto then
          MargArray[MarginLeft] := 0
        else
          MargArray[MarginRight] := 0;
        CalcWidth;
      end
      else
      begin
        if not HideOverflow then
          MargArray[Width] := Max(MargArray[Width], MinWidth);
        Marg2 := Max(0, AWidth - MargArray[Width] - BordPad);
        MargArray[MarginLeft] := Marg2 div 2;
        MargArray[MarginRight] := Marg2 div 2;
      end;
    3:
      begin
        MargArray[MarginLeft] := 0;
        MargArray[MarginRight] := 0;
        CalcWidth;
      end;
  end;
  Result := MargArray[Width];
end;

procedure DoImageStuff(Canvas: TCanvas; IW, IH: Integer; BGImage: TImageObj; PRec: PtPositionRec;
  var TiledImage: TgpObject; var TiledMask: TBitmap; var NoMask: boolean);
{Set up for the background image. Allow for tiling, and transparency
  BGImage is the image
  PRec describes the location and tiling
  IW, IH, the width and height of the background
}
var
  I, OW, OH, X, XX, Y, X2, Y2: Integer;
  P: array[1..2] of Integer;
  TheMask, NewBitmap, NewMask: TBitmap;
  TheGpObj: TGpObject;
  {$IFNDEF NoGDIPlus}
  g: TgpGraphics;
  {$ENDIF !NoGDIPlus}

  procedure Tile(Bitmap, Mask: TBitmap; W, H: Integer);
  begin
    repeat {tile BGImage in the various dc's}
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

  {$IFNDEF NoGDIPlus}
  procedure TileGpImage(Image: TgpImage; W, H: Integer);
  var
    ImW, ImH: Integer;
    graphics: TgpGraphics;
  begin
    ImW := Image.Width;
    ImH := Image.Height;
    try
      graphics := TGPGraphics.Create(TgpImage(TiledImage));
      try
        repeat {tile Image in the various dc's}
          XX := X;
          repeat
            graphics.DrawImage(Image, XX, Y, Image.Width, Image.Height);
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
  {$ENDIF !NoGDIPlus}

var
  IsTiledGpImage: Boolean;
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
{$IFNDEF NoMetafile}
  else if BGImage.Image is ThtMetafile then
  begin
    TheGpObj := ThtMetafile(BGImage.Image).Bitmap;
    TheMask := ThtMetafile(BGImage.Image).Mask;
  end
{$ENDIF}
  else
  begin
    TheGpObj := BGImage.Image;
    TheMask := nil;
  end;

  NoMask := not Assigned(TheMask) and PRec[1].RepeatD and PRec[2].RepeatD;

  OW := GetImageWidth(BGImage.Image);
  OH := GetImageHeight(BGImage.Image);

  {$IFNDEF NoGDIPlus}
  if (BGImage.Image is TgpImage) and not ((OW = 1) or (OH = 1)) then
  begin {TiledImage will be a TGpBitmap unless Image needs to be enlarged}
    with TgpBitmap(TiledImage) do
      if Assigned(TiledImage) and ((IW <> Width) or (IH <> Height)) then
        FreeAndNil(TiledImage);
    if not Assigned(TiledImage) then
      TiledImage := TgpBitmap.Create(IW, IH);
    g := TgpGraphics.Create(TgpBitmap(TiledImage));
    g.Clear(0); {clear to transparent black}
    g.Free;
    IsTiledGpImage := False;
  end
  else
  {$ENDIF !NoGDIPlus}
  begin {TiledImage will be a TBitmap}
    if not Assigned(TiledImage) then
      TiledImage := TBitmap.Create;
    TBitmap(TiledImage).Palette := CopyPalette(ThePalette);
    TBitmap(TiledImage).Height := IH;
    TBitmap(TiledImage).Width := IW;
    PatBlt(TBitmap(TiledImage).Canvas.Handle, 0, 0, IW, IH, Blackness);
    IsTiledGpImage := True;
  end;

  if not NoMask and ((BGImage.Image is TBitmap)
//BG, 25.08.2007:
    or (BGImage.Image is TGifImage)
//BG, 25.08.2007
{$IFNDEF NoMetafile}
    or (BGImage.Image is ThtMetafile)
{$ENDIF}
    or IsTiledGpImage
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
  P[1] := 0; P[2] := 0;
  for I := 1 to 2 do
    with PRec[I] do
    begin
      case PosType of
        pTop:
          P[I] := 0;
        pCenter:
          if I = 1 then
            P[1] := IW div 2 - OW div 2
          else
            P[2] := IH div 2 - OH div 2;
        pBottom:
          P[I] := IH - OH;
        pLeft:
          P[I] := 0;
        pRight:
          P[I] := IW - OW;
        PPercent:
          if I = 1 then
            P[1] := ((IW - OW) * Value) div 100
          else
            P[2] := ((IH - OH) * Value) div 100;
        pDim:
          P[I] := Value;
      end;
      if I = 1 then
        P[1] := Min(Max(P[1], -OW), IW)
      else
        P[2] := Min(Max(P[2], -OH), IH);
    end;

  X := P[1];
  Y := P[2];
  if PRec[2].RepeatD then
  begin
    while Y > 0 do
      Dec(Y, OH);
    Y2 := IH;
  end
  else
    Y2 := Y;
  if PRec[1].RepeatD then
  begin
    while X > 0 do
      Dec(X, OW);
    X2 := IW;
  end
  else
    X2 := X;

  if ((OW = 1) or (OH = 1)) then
  begin {in case a 1 pixel bitmap is being tiled.  EnlargeImage returns a
     TBitmap regardless of TheGpObj type.}
    NewBitmap := EnlargeImage(TheGpObj, X2 - X + 1, Y2 - Y + 1);
    try
      if Assigned(TheMask) then
        NewMask := EnlargeImage(TheMask, X2 - X + 1, Y2 - Y + 1)
      else
        NewMask := nil;
      try
        Tile(NewBitmap, NewMask, X2 - X + 1, Y2 - Y + 1);
      finally
        NewMask.Free;
      end;
    finally
      NewBitmap.Free;
    end;
  end
  else if (TheGpObj is TBitmap) then
    Tile(TBitmap(TheGpObj), TheMask, OW, OH)
    {$IFNDEF NoGDIPlus}
  else
    TileGpImage(TgpImage(TheGpObj), OW, OH)
    {$ENDIF !NoGDIPlus}
    ;
end;

{----------------TBlock.DrawLogic}

function TBlock.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
var
  ScrollWidth, YClear: Integer;
  LIndex, RIndex: Integer;
  SaveID: TObject;
  TotalWidth, LeftWidths, RightWidths, MiscWidths: Integer;
  AutoCount: Integer;
  BlockHeight: Integer;
  IB, Xin: Integer;

  function GetClearSpace(CA: ClearAttrType): Integer;
  var
    CL, CR: Integer;
  begin
    Result := 0;
    if (CA <> clrNone) then
    begin {may need to move down past floating image}
      IMgr.GetClearY(CL, CR);
      case CA of
        clLeft: Result := Max(0, CL - Y - 1);
        clRight: Result := Max(0, CR - Y - 1);
        clAll: Result := Max(CL - Y - 1, Max(0, CR - Y - 1));
      end;
    end;
  end;

  function GetClientContentBot(ClientContentBot: Integer): Integer;
  begin
    if HideOverflow and (MargArray[Height] > 3) then
      Result := ContentTop + MargArray[Height]
    else
      Result := Max(Max(ContentTop, ClientContentBot), ContentTop + MargArray[Height]);
  end;

begin
  case Display of
    pdNone:
    begin
      SectionHeight := 0;
      DrawHeight := 0;
      ContentBot := 0;
      DrawBot := 0;
      MaxWidth := 0;
      Result := 0;
    end;

{$ifdef DO_BLOCK_INLINE}
    pdInline:
      Result := inherited DrawLogic(Canvas, X, Y, XRef, YREf, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
{$endif}

  else
    YDraw := Y;
    Xin := X;
    ClearAddOn := GetClearSpace(ClearAttr);
    StartCurs := Curs;
    MaxWidth := AWidth;

    ConvMargArray(AWidth, AHeight, AutoCount);

    TopP := MargArray[TopPos];
    LeftP := MargArray[LeftPos];
    case Positioning of
      posRelative:
      begin
        if TopP = Auto then
          TopP := 0;
        if LeftP = Auto then
          LeftP := 0;
      end;

      posAbsolute:
      begin
        if TopP = Auto then
          TopP := 0;
        if (LeftP = Auto) then
          if (MargArray[RightPos] <> Auto) and (AutoCount = 0) then
            LeftP := AWidth - MargArray[RightPos] - MargArray[Width]
              - MargArray[MarginLeft] - MargArray[PaddingLeft] - MargArray[BorderLeftWidth]
              - MargArray[MarginRight] - MargArray[PaddingRight] - MargArray[BorderRightWidth]
          else
            LeftP := 0;
        X := LeftP;
        Y := TopP + YRef;
      end;
    end;

    NewWidth := FindWidth(Canvas, AWidth, AHeight, AutoCount);

    case Positioning of
      posAbsolute:
      begin
        RefIMgr := IMgr;
        IMgr := MyCell.IMgr;
        IMgr.Clear;
        IMgr.Reset(0, NewWidth);
        IMgr.Width := NewWidth;
      end
    else
      MyCell.IMgr := IMgr;
    end;

    SaveID := IMgr.CurrentID;
    IMgr.CurrentID := Self;

    LeftWidths := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
    RightWidths := MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth];
    MiscWidths := LeftWidths + RightWidths;
    TotalWidth := MiscWidths + NewWidth;

    YClear := Y + ClearAddon;
    case FloatLR of
      ALeft:
        Indent := IMgr.GetNextLeftXY(YClear, X, NewWidth, AWidth, 0) + LeftWidths - X;

      ARight:
        Indent := Min(AWidth, IMgr.RightSide(YClear)) - RightWidths - NewWidth;
    else
      Indent := LeftWidths;
    end;
    if MargArray[MarginTop] > 0 then
      DrawTop := YClear
    else
      DrawTop := YClear + MargArray[MarginTop]; {Border top}

    X := X + Indent;
    ContentTop := YClear + MargArray[MarginTop] + MargArray[PaddingTop] + MargArray[BorderTopWidth];

    LIndex := IMgr.SetLeftIndent(X, ContentTop);
    RIndex := IMgr.SetRightIndent(X + NewWidth, ContentTop);

    ContentLeft := X;

    if MargArray[Height] > 0 then
      BlockHeight := MargArray[Height]
    else if AHeight > 0 then
      BlockHeight := AHeight
    else
      BlockHeight := BlHt;
    case Positioning of
      posRelative:
      begin
        MyCell.DoLogicX(Canvas,
          X,
          ContentTop + TopP,
          XRef,
          ContentTop + TopP,
          NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs);
        MaxWidth := ScrollWidth + MiscWidths - MargArray[MarginRight] + LeftP - Xin;
        ClientContentBot := GetClientContentBot(MyCell.tcContentBot - TopP);
      end;

      posAbsolute:
      begin
        MyCell.DoLogicX(Canvas,
          X,
          ContentTop,
          XRef + LeftP + MargArray[MarginLeft] + MargArray[BorderLeftWidth],
          YRef + TopP + MargArray[MarginTop] + MargArray[BorderTopWidth],
          NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs);
        MaxWidth := ScrollWidth + MiscWidths - MargArray[MarginRight] + LeftP - Xin;
        ClientContentBot := GetClientContentBot(MyCell.tcContentBot);
        IB := IMgr.ImageBottom; {check for image overhang}
        if IB > ClientContentBot then
          ClientContentBot := IB;
      end;

    else
      MyCell.DoLogicX(Canvas,
        X,
        ContentTop,
        XRef,
        YRef,
        NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs);
      MaxWidth := ScrollWidth + RightWidths + Indent;
      ClientContentBot := GetClientContentBot(MyCell.tcContentBot);
    end;
    Len := Curs - StartCurs;
    ContentBot :=  ClientContentBot +                    MargArray[PaddingBottom] + MargArray[BorderBottomWidth] + MargArray[MarginBottom];
    DrawBot := Max(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom] + MargArray[BorderBottomWidth];

    Result := ContentBot - Y;

    if Assigned(BGImage) and ParentSectionList.ShowImages then
    begin
      BGImage.DrawLogic(ParentSectionList, Canvas, nil, 100, 0);
      if (BGImage.Image = ErrorBitmap) then
      begin
        FreeAndNil(BGImage);
        NeedDoImageStuff := False;
      end
      else
      begin
        BGImage.ImageKnown := True; {won't need reformat on InsertImage}
        NeedDoImageStuff := True;
      end;
    end;
    SectionHeight := Result;
    IMgr.FreeLeftIndentRec(LIndex);
    IMgr.FreeRightIndentRec(RIndex);
    if (FloatLR in [ALeft, ARight]) or (Positioning = posAbsolute) then
    begin
      case Positioning of
        posAbsolute:
          DrawHeight := 0
      else
        DrawHeight := SectionHeight;
      end;
      case FloatLR of
        ALeft:
          IMgr.UpdateBlock(DrawTop, X + NewWidth + RightWidths, DrawBot - DrawTop, FloatLR);
        ARight:
          IMgr.UpdateBlock(DrawTop, TotalWidth, DrawBot - DrawTop, FloatLR);
      end;
      SectionHeight := 0;
      Result := 0;
    end
    else
    begin
      DrawHeight := IMgr.ImageBottom - Y; {in case image overhangs}
      if DrawHeight < SectionHeight then
        DrawHeight := SectionHeight;
    end;
    IMgr.CurrentID := SaveID;
    if DrawList.Count = 0 then
      DrawSort;
  end;
end;

{----------------TBlock.DrawSort}

procedure TBlock.DrawSort;
var
  I, ZeroIndx, EndZeroIndx, SBZIndex: Integer;
  SB: TSectionBase;

  procedure InsertSB(I1, I2: Integer);
  var
    J: Integer;
    Inserted: boolean;
  begin
    Inserted := False;
    for J := I1 to I2 - 1 do
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
  for I := 0 to MyCell.Count - 1 do
  begin
    SB := MyCell.Items[I];
    SB.MyBlock := Self;
    SBZIndex := SB.ZIndex;
    if SBZIndex < 0 then
    begin
      InsertSB(0, ZeroIndx);
      Inc(ZeroIndx);
      Inc(EndZeroIndx);
    end
    else if SBZIndex = 0 then {most items go here}
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
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  Y, YO: Integer;
  HeightNeeded, Spacing: Integer;
begin
  case Display of
    pdNone:   Result := 0;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
{$endif}
  else
    Y := YDraw;
    YO := Y - ParentSectionList.YOff;
    Result := Y + SectionHeight;

    if ParentSectionList.SkipDraw then
    begin
      ParentSectionList.SkipDraw := False;
      Exit;
    end;

    with ParentSectionList do
      if Printing and (Positioning <> posAbsolute) then
        if BreakBefore and not FirstPageItem then
        begin
          if ARect.Top + YOff < YDraw + MargArray[MarginTop] then {page-break-before}
          begin
            if YDraw + MargArray[MarginTop] < PageBottom then
              PageBottom := YDraw + MargArray[MarginTop];
            SkipDraw := True; {prevents next block from drawing a line}
            Exit;
          end;
        end
        else if KeepIntact then
        begin
        {if we're printing and block won't fit on this page and block will fit on
         next page, then don't do block now}
          if (YO > ARect.Top) and (Y + DrawHeight > PageBottom) and
            (DrawHeight - MargArray[MarginTop] < ARect.Bottom - ARect.Top) then
          begin
            if Y + MargArray[MarginTop] < PageBottom then
              PageBottom := Y + MargArray[MarginTop];
            Exit;
          end;
        end
        else if BreakAfter then
        begin
          if ARect.Top + YOff < Result then {page-break-after}
            if Result < PageBottom then
              PageBottom := Result;
        end
        else if Self is TTableBlock and not TTableBlock(Self).Table.HeadOrFoot then {ordinary tables}
        {if we're printing and
         we're 2/3 down page and table won't fit on this page and table will fit on
         next page, then don't do table now}
        begin
          if (YO > ARect.Top + ((ARect.Bottom - ARect.Top) * 2) div 3) and
            (Y + DrawHeight > PageBottom) and
            (DrawHeight < ARect.Bottom - ARect.Top) then
          begin
            if Y + MargArray[MarginTop] < PageBottom then
              PageBottom := Y + MargArray[MarginTop];
            Exit;
          end;
        end
        else if Self is TTableBlock then {try to avoid just a header and footer at page break}
          with TTableBlock(Self).Table do
            if HeadOrFoot and (ParentSectionList.TableNestLevel = 0)
              and ((ParentSectionList.PrintingTable = nil) or
              (ParentSectionList.PrintingTable = TTableBlock(Self).Table)) then
            begin
              Spacing := CellSpacing div 2;
              HeightNeeded := HeaderHeight + FootHeight +
                TCellList(Rows.Items[HeaderRowCount]).RowHeight;
              if (YO > ARect.Top) and (Y + HeightNeeded > ParentSectionList.PageBottom) and
                (HeightNeeded < ARect.Bottom - ARect.Top) then
              begin {will go on next page}
                if Y + Spacing < ParentSectionList.PageBottom then
                begin
                  ParentSectionList.PageShortened := True;
                  ParentSectionList.PageBottom := Y + Spacing;
                end;
                Exit;
              end;
            end;

    if Visibility <> viHidden then
      if Positioning = posRelative then {for debugging}
        DrawBlock(Canvas, ARect, IMgr, X + LeftP, Y + TopP, XRef, YRef)
      else if Positioning = posAbsolute then
        DrawBlock(Canvas, ARect, IMgr, XRef + LeftP, YRef + TopP, XRef, YRef)
      else if FloatLR in [ALeft, ARight] then
        DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef)
      else
        DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef);
  end;
end;

{----------------TBlock.DrawBlock}

procedure TBlock.DrawBlock(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, Y, XRef, YRef: Integer);
var
  YOffset: Integer;
  XR, YB, BL, BT, BR, BB, PL, PT, PR, PB, RefX, TmpHt: Integer;
  SaveID: TObject;
  ImgOK, HasBackgroundColor: boolean;
  IT, IH, FT: Integer;
  Rgn, SaveRgn, SaveRgn1: HRgn;
  OpenRgn: Boolean;

  procedure InitFullBg(W, H: Integer);
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
    FullBG.Height := Max(H, 2);
    FullBG.Width := Max(W, 2);
  end;

var
  XOffset: Integer;
begin
  YOffset := ParentSectionList.YOff;
  XOffset := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];

  case FLoatLR of
    ALeft, ARight: RefX := X + Indent - XOffset;
  else
    RefX := X;
  end;

  X := X + Indent;

  XR := RefX + XOffset + NewWidth + MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth]; {current right edge}
  if Positioning = posRelative then
    YB := ContentBot - YOffset + TopP
  else if FLoatLR in [ALeft, ARight] then
    YB := DrawBot + MargArray[MarginBottom] - YOffset
  else
    YB := ContentBot - YOffset;

  BL := RefX + MargArray[MarginLeft]; {Border left and right}
  BR := XR - MargArray[MarginRight];
  PL := BL + MargArray[BorderLeftWidth]; {Padding left and right}
  PR := BR - MargArray[BorderRightWidth];

  BT := Y + ClearAddon + MargArray[MarginTop] - YOffset; {Border Top and Bottom}
  BB := YB - MargArray[MarginBottom];
  PT := BT + MargArray[BorderTopWidth]; {Padding Top and Bottom}
  PB := BB - MargArray[BorderBottomWidth];

  IT := Max(0, Arect.Top - 2 - PT);
  FT := Max(PT, ARect.Top - 2); {top of area drawn, screen coordinates}
  IH := Min(PB - FT, Arect.Bottom - FT); {height of area actually drawn}

  SaveRgn1 := 0;
  OpenRgn := (Positioning <> PosStatic) and (ParentSectionList.TableNestLevel > 0);
  if OpenRgn then
  begin
    SaveRgn1 := CreateRectRgn(0, 0, 1, 1);
    GetClipRgn(Canvas.Handle, SaveRgn1);
    SelectClipRgn(Canvas.Handle, 0);
  end;
  try
    MyRect := Rect(BL, BT, BR, BB);
    if (BT <= ARect.Bottom) and (BB >= ARect.Top) then
    begin
      HasBackgroundColor := MargArray[BackgroundColor] <> clNone;
      try
        if NeedDoImageStuff and Assigned(BGImage) and (BGImage.Image <> DefBitmap) then
        begin
          if BGImage.Image = ErrorBitmap then {Skip the background image}
            FreeAndNil(BGImage)
          else
          try
            if FloatLR in [ALeft, ARight] then
              TmpHt := DrawBot - ContentTop + MargArray[PaddingTop] + MargArray[PaddingBottom]
            else
              TmpHt := ClientContentBot - ContentTop + MargArray[PaddingTop] + MargArray[PaddingBottom];

            DoImageStuff(Canvas, MargArray[PaddingLeft] + NewWidth + MargArray[PaddingRight],
              TmpHt, BGImage, PRec, TiledImage, TiledMask, NoMask);
            if ParentSectionList.IsCopy and (TiledImage is TBitmap) then
              TBitmap(TiledImage).HandleType := bmDIB;
          except {bad image, get rid of it}
            FreeAndNil(BGImage);
            FreeAndNil(TiledImage);
            FreeAndNil(TiledMask);
          end;
          NeedDoImageStuff := False;
        end;

        ImgOK := not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
          and ParentSectionList.ShowImages;

        if HasBackgroundColor and
          (not ParentSectionList.Printing or ParentSectionList.PrintTableBackground) then
        begin {color the Padding Region}
          Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
          Canvas.Brush.Style := bsSolid;
          if ParentSectionList.IsCopy and ImgOK then
          begin
            InitFullBG(PR - PL, IH);
            FullBG.Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
            FullBG.Canvas.Brush.Style := bsSolid;
            FullBG.Canvas.FillRect(Rect(0, 0, PR - PL, IH));
          end
          else
            Canvas.FillRect(Rect(PL, FT, PR, FT + IH));
        end;

        if ImgOK then
        begin
          if not ParentSectionList.IsCopy then
            {$IFNDEF NoGDIPlus}
            if TiledImage is TgpBitmap then
            //DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PL, PT)
              DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PL, FT, 0, IT, PR - PL, IH)
            //BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcCopy)
            else
            {$ENDIF !NoGDIPlus}
            if NoMask then
              BitBlt(Canvas.Handle, PL, FT, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcCopy)
            else
            begin
              InitFullBG(PR - PL, IH);
              BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, Canvas.Handle, PL, FT, SrcCopy);
              BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
              BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
              BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
              BitBlt(Canvas.Handle, PL, FT, PR - PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
            end
          else
          {$IFNDEF NoGDIPlus}
          if TiledImage is TGpBitmap then {printing}
          begin
            if HasBackgroundColor then
            begin
              DrawGpImage(FullBg.Canvas.Handle, TgpImage(TiledImage), 0, 0);
              PrintBitmap(Canvas, PL, FT, PR - PL, IH, FullBG);
            end
            else
              PrintGpImageDirect(Canvas.Handle, TgpImage(TiledImage), PL, PT,
                ParentSectionList.ScaleX, ParentSectionList.ScaleY);
          end
          else
          {$ENDIF !NoGDIPlus}
          if NoMask then {printing}
            PrintBitmap(Canvas, PL, FT, PR - PL, IH, TBitmap(TiledImage))
          else if HasBackgroundColor then
          begin
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
            PrintBitmap(Canvas, PL, FT, PR - PL, IH, FullBG);
          end
          else
            PrintTransparentBitmap3(Canvas, PL, FT, PR - PL, IH, TBitmap(TiledImage), TiledMask, IT, IH)
        end;

      except
      end;
    end;

    if HideOverflow then
      GetClippingRgn(Canvas, Rect(PL + MargArray[PaddingLeft], PT + MargArray[PaddingTop],
        PR - MargArray[PaddingRight], PB - MargArray[PaddingBottom]),
        ParentSectionList.Printing, Rgn, SaveRgn);

    SaveID := IMgr.CurrentID;
    Imgr.CurrentID := Self;
    if Positioning = posRelative then
      DrawTheList(Canvas, ARect, NewWidth, X,
        RefX + MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft],
        Y + MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop])
    else if Positioning = posAbsolute then
      DrawTheList(Canvas, ARect, NewWidth, X,
        RefX + MargArray[MarginLeft] + MargArray[BorderLeftWidth],
        Y + MargArray[MarginTop] + MargArray[BorderTopWidth])
    else
      DrawTheList(Canvas, ARect, NewWidth, X, XRef, YRef);
    Imgr.CurrentID := SaveID;

    if HideOverflow then {restore any previous clip region}
    begin
      SelectClipRgn(Canvas.Handle, SaveRgn);
      DeleteObject(Rgn);
      if SaveRgn <> 0 then
        DeleteObject(SaveRgn);
    end;
    if (BL <> PL) or (BT <> PT) or (BR <> PR) or (BB <> PB) then
      DrawBlockBorder(Canvas, Rect(BL, BT, BR, BB), Rect(PL, PT, PR, PB));
  finally
    if OpenRgn then
    begin
      SelectClipRgn(Canvas.Handle, SaveRgn1);
      DeleteObject(SaveRgn1);
    end;
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
  XRef, YRef: Integer);
{draw the list sorted by Z order.}
var
  I: Integer;
  SaveID: TObject;
begin
  if Positioning = posAbsolute then
    with MyCell do
    begin
      SaveID := IMgr.CurrentID;
      IMgr.Reset(RefIMgr.LfEdge, RefIMgr.LfEdge + IMgr.Width);
      IMgr.ClipWidth := ClipWidth;
      IMgr.CurrentID := SaveID;
    end
  else
    MyCell.IMgr.ClipWidth := ClipWidth;
  with DrawList do
    for I := 0 to Count - 1 do
      TSectionBase(Items[I]).Draw1(Canvas, ARect, MyCell.IMgr, X, XRef, YRef);
end;

procedure TBlock.FormTree(const Indent: ThtString; var Tree: ThtString);
var
  MyIndent: ThtString;
  TM, BM: ThtString;
begin
  MyIndent := Indent + '   ';
  TM := IntToStr(MargArray[MarginTop]);
  BM := IntToStr(MargArray[MarginBottom]);
  Tree := Tree + Indent + TagClass + '  ' + TM + '  ' + BM + ^M + ^J;
  MyCell.FormTree(MyIndent, Tree);
end;

//-- BG ---------------------------------------------------------- 24.08.2010 --
function TBlock.getBorderWidth: Integer;
begin
  Result := 1;
end;

{----------------TTableAndCaptionBlock.Create}

constructor TTableAndCaptionBlock.Create(Master: TSectionList; Prop: TProperties;
  AnOwnerCell: TCellBasic; Attributes: TAttributeList; ATableBlock: TTableBlock);
var
  I: Integer;
begin
  inherited Create(Master, Prop, AnOwnerCell, Attributes);
  TableBlock := ATableBlock;
  Justify := TableBlock.Justify;

  for I := 0 to Attributes.Count - 1 do
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

constructor TTableAndCaptionBlock.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
var
  TT: TTableAndCaptionBlock;
  Item: TObject;
  I1, I2: Integer;
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

function TTableAndCaptionBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer;
var
  Mx, Mn, FWidth: Integer;
begin
  BorderStyle := bssNone; {has no border}
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
  FWidth := TableBlock.FindWidth1(Canvas, AWidth, MargArray[MarginLeft] + MargArray[MarginRight]);
  Result := Max(FWidth, Mn);
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

procedure TTableAndCaptionBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
var
  Mx, Mn, MxTable, MnTable: Integer;
begin
  TableBlock.MinMaxWidth(Canvas, MnTable, MxTable);
  FCaptionBlock.MinMaxWidth(Canvas, Mn, Mx);
  Min := Math.Max(MnTable, Mn);
  Max := Math.Max(MxTable, Mn);
end;

function TTableAndCaptionBlock.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
begin
  if not Prev then
  begin
    Result := FCaptionBlock.FindDocPos(SourcePos, Prev);
    if Result < 0 then
      Result := TableBlock.FindDocPos(SourcePos, Prev);
  end
  else {Prev, iterate backwards}
  begin
    Result := TableBlock.FindDocPos(SourcePos, Prev);
    if Result < 0 then
      Result := FCaptionBlock.FindDocPos(SourcePos, Prev);
  end;
end;

{----------------TTableBlock.Create}

constructor TTableBlock.Create(Master: TSectionList; Prop: TProperties;
  AnOwnerCell: TCellBasic; ATable: THtmlTable; TableAttr: TAttributeList;
  TableLevel: Integer);
var
  I, AutoCount, BorderColor: Integer;
  Percent: boolean;
  S,W,C: PropIndices;
begin
  inherited Create(Master, Prop, AnOwnerCell, TableAttr);
  Table := ATable;
  Justify := NoJustify;

  for I := 0 to TableAttr.Count - 1 do
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
        HSpaceSy: HSpace := Min(40, Abs(Value));
        VSpaceSy: VSpace := Min(200, Abs(Value));
        WidthSy:
          if Pos('%', Name) > 0 then
          begin
            if (Value > 0) and (Value <= 100) then
              WidthAttr := Value * 10;
            AsPercent := True;
          end
          else
            WidthAttr := Value;

        HeightSy:
          if (VarType(MargArrayO[Height]) in VarInt) and (MargArrayO[Height] = IntNull) then
            MargArrayO[Height] := Name;
      end;

  //BG, 13.06.2010: Issue 5: Table border versus stylesheets:
  if Table.BorderWidth > 0 then
  begin
    S := BorderTopStyle;
    for W := BorderTopWidth to BorderLeftWidth do
    begin
      if MargArrayO[S] = bssNone then
      begin
        if Table.BorderColor = clNone then
          MargArrayO[S] := bssOutset
        else
          MargArrayO[S] := bssSolid;
        if (VarType(MargArrayO[W]) in varInt) and (MargArrayO[W] = IntNull) then
          MargArrayO[W] := Table.BorderWidth;
      end;
      Inc(S);
    end;
    BorderStyle := bssOutset;
  end;

  BorderColor := Table.BorderColor;
  if BorderColor = clNone then
    BorderColor := clSilver;
  C := BorderTopColor;
  TableBorder := False;
  for S := BorderTopStyle to BorderLeftStyle do
  begin
    if MargArrayO[S] <> bssNone then
    begin
      TableBorder := True;
      if (VarType(MargArrayO[C]) in varInt) and (MargArrayO[C] = IntNull) then
        case BorderStyleType(MargArrayO[S]) of
          bssOutset:
            if S in [BorderLeftStyle, BorderTopStyle] then
              MargArrayO[C] := Table.BorderColorLight
            else
              MargArrayO[C] := Table.BorderColorDark;
          bssInset:
            if S in [BorderLeftStyle, BorderTopStyle] then
              MargArrayO[C] := Table.BorderColorDark
            else
              MargArrayO[C] := Table.BorderColorLight;
        else
          MargArrayO[C] := BorderColor;
        end;
    end;
    Inc(C);
  end;


{need to see if width is defined in style}
  Percent := (VarIsStr(MargArrayO[Width])) and (Pos('%', MargArrayO[Width]) > 0);
  StyleUn.ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, BorderStyle, BorderWidth, AutoCount, MargArray);
  if MargArray[Width] > 0 then
  begin
    if Percent then
    begin
      AsPercent := True;
      WidthAttr := Min(1000, MargArray[Width] * 10);
    end
    else
    begin
      WidthAttr := MargArray[Width];
    {By custom (not by specs), tables handle CSS Width property differently.  The
     Width includes the padding and border.}
      MargArray[Width] := WidthAttr - MargArray[BorderLeftWidth] - MargArray[BorderRightWidth]
        - MargArray[PaddingLeft] - MargArray[PaddingRight];
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

constructor TTableBlock.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
var
  TT: TTableBlock;
  Item: TObject;
begin
  inherited;
  TT := T as TTableBlock;
  System.Move(TT.WidthAttr, WidthAttr, DWord(@Justify) - DWord(@WidthAttr) + Sizeof(Justify));
  Item := MyCell.Items[0];
  Table := Item as THtmlTable;
end;

{----------------TTableBlock.MinMaxWidth}

procedure TTableBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
var
  TmpWidth: Integer;
begin
  TmpWidth := 0;
  if AsPercent then
    Table.tblWidthAttr := 0
  else
  begin
    TmpWidth := Math.Max(0, WidthAttr - MargArray[BorderLeftWidth] - MargArray[BorderRightWidth]
      - MargArray[PaddingLeft] - MargArray[PaddingRight]);
    Table.tblWidthAttr := TmpWidth;
  end;
  inherited MinMaxWidth(Canvas, Min, Max);
  if TmpWidth > 0 then
  begin
    Min := Math.Max(Min, TmpWidth);
    Max := Min;
  end;
end;

{----------------TTableBlock.FindWidth1}

function TTableBlock.FindWidth1(Canvas: TCanvas; AWidth, ExtMarg: Integer): Integer;
{called by TTableAndCaptionBlock to assist in it's FindWidth Calculation.
 This method is called before TTableBlockFindWidth but is called only if there
 is a caption on the table.  AWidth is the full width available to the
 TTableAndCaptionBlock.}
var
  LeftSide, RightSide: Integer;
  Min, Max, Allow: Integer;
begin
  MargArray[MarginLeft] := 0;
  MargArray[MarginRight] := 0;
  MargArray[MarginTop] := 0;
  MargArray[MarginBottom] := 0;

  LeftSide := MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  RightSide := MargArray[PaddingRight] + MargArray[BorderRightWidth];

  Table.tblWidthAttr := 0;
  if WidthAttr > 0 then
  begin
    if AsPercent then
      Result := Math.Min(MulDiv(AWidth, WidthAttr, 1000), AWidth - ExtMarg)
    else
      Result := WidthAttr;
    Result := Result - (LeftSide + RightSide);
    Table.tblWidthAttr := Result;
    Table.MinMaxWidth(Canvas, Min, Max);
    Result := Math.Max(Min, Result);
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
    else
      Result := Allow;
  end;
  Result := Result + LeftSide + RightSide;
end;

//-- BG ---------------------------------------------------------- 24.08.2010 --
function TTableBlock.getBorderWidth: Integer;
begin
  Result := Table.BorderWidth;
end;

{----------------TTableBlock.FindWidth}

function TTableBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer;
var
  LeftSide, RightSide: Integer;
  Min, Max, Allow: Integer;
begin
  if not HasCaption then
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
    MargArray[BackgroundColor] := BkColor;
  Table.BkGnd := (MargArray[BackgroundColor] <> clNone) and not Assigned(BGImage);
  Table.BkColor := MargArray[BackgroundColor]; {to be passed on to cells}

  LeftSide := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  RightSide := MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth];

  if not HasCaption then
    Table.tblWidthAttr := 0;
  if WidthAttr > 0 then
  begin
    if not HasCaption then {already done if HasCaption}
    begin
      if AsPercent then
      begin
        Result := MulDiv(AWidth, WidthAttr, 1000);
        Dec(Result, (LeftSide + RightSide));
      end
      else
        Result := WidthAttr - (MargArray[PaddingLeft] + MargArray[BorderLeftWidth]
          + MargArray[PaddingRight] + MargArray[BorderRightWidth]);
      Table.tblWidthAttr := Result;
      Table.MinMaxWidth(Canvas, Min, Max);
      Result := Math.Max(Min, Result);
      Table.tblWidthAttr := Result;
    end
    else
      Result := Table.tblWidthAttr;
  end
  else
  begin
    Table.MinMaxWidth(Canvas, Min, Max);
    Allow := AWidth - LeftSide - RightSide;
    if Max <= Allow then
      Result := Max
    else if Min >= Allow then
      Result := Min
    else
      Result := Allow;
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

function TTableBlock.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer;
  IMgr: TIndentManager; var MaxWidth: Integer; var Curs: Integer): Integer;
var
  X1, Tmp: Integer;
begin
  if not (FloatLR in [ALeft, ARight]) then
  begin
    Tmp := X;
    X := Max(Tmp, IMgr.LeftIndent(Y));
    TableIndent := X - Tmp;
    X1 := Min(Tmp + AWidth, IMgr.RightSide(Y));
    AWidth := X1 - X;
  end;
  Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
end;

function TTableBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
begin
  X := X + TableIndent;
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
end;

procedure TTableBlock.DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect);
var
  Light, Dark: TColor;
  C: PropIndices;
begin
  //BG, 13.06.2010: Issue 5: Table border versus stylesheets
  GetRaisedColors(ParentSectionList, Canvas, Light, Dark);
  for C := BorderTopColor to BorderLeftColor do
    if MargArrayO[C] = clBtnHighLight then
      MargArray[C] := Light
    else if MargArrayO[C] = clBtnShadow then
      MargArray[C] := Dark;
  inherited;
end;

procedure TTableBlock.AddSectionsToList;
begin {Sections in Table not added only table itself}
  ParentSectionList.PositionList.Add(Table);
end;

constructor THRBlock.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
begin
  inherited;
  Align := (T as THRBlock).Align;
end;

{----------------THRBlock.FindWidth}

function THRBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer;
var
  LeftSide, RightSide, SWidth: Integer;
  Diff: Integer;
begin
  if Positioning = posAbsolute then
    Align := Left;
  LeftSide := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  RightSide := MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth];
  SWidth := MargArray[Width];

  if SWidth > 0 then
    Result := Min(SWidth, AWidth - LeftSide - RightSide)
  else
    Result := Max(15, AWidth - LeftSide - RightSide);
  MargArray[Width] := Result;
{note that above could be inherited; if LeftSide and Rightside were fields
of TBlock}

  if Align <> Left then
  begin
    Diff := AWidth - Result - LeftSide - RightSide;
    if Diff > 0 then
      case Align of
        Centered: Inc(MargArray[MarginLeft], Diff div 2);
        Right: Inc(MargArray[MarginLeft], Diff);
      end;
  end;
  if not ParentSectionList.IsCopy then
    THorzline(MyHRule).VSize := MargArray[StyleUn.Height];
end;

{----------------TBlockLI.Create}

constructor TBlockLI.Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
  Sy: Symb; APlain: boolean; AIndexType: ThtChar; AListNumb,
  ListLevel: Integer; Attributes: TAttributeList);
var
  Tmp: ListBulletType;
  S: ThtString;
  TmpFont: TMyFont;
begin
  inherited Create(Master, Prop, AnOwnerCell, Attributes);
  case Sy of

    UlSy, DirSy, MenuSy:
      begin
        FListType := Unordered;
        if APlain then
          FListStyleType := lbNone
        else
          case ListLevel mod 3 of
            1: FListStyleType := lbDisc;
            2: FListStyleType := lbCircle;
            0: FListStyleType := lbSquare;
          end;
      end;

    OLSy:
      begin
        FListType := Ordered;
        case AIndexType of
          'a': FListStyleType := lbLowerAlpha;
          'A': FListStyleType := lbUpperAlpha;
          'i': FListStyleType := lbLowerRoman;
          'I': FListStyleType := lbUpperRoman;
        else
          FListStyleType := lbDecimal;
        end;
      end;

    DLSy:
      FListType := Definition;
  else
    FListType := liAlone;
    FListStyleType := lbDisc;
    if (VarType(MargArrayO[MarginLeft]) in varInt) and
      ((MargArrayO[MarginLeft] = IntNull) or (MargArrayO[MarginLeft] = 0)) then
      MargArrayO[MarginLeft] := 16;
  end;
  FListNumb := AListNumb;

  Tmp := Prop.GetListStyleType;
  if Tmp <> lbBlank then
    FListStyleType := Tmp;
  ListFont := TMyFont.Create;
  TmpFont := Prop.GetFont;
  ListFont.Assign(TmpFont);
  TmpFont.Free;

  S := Prop.GetListStyleImage;
  if S <> '' then
    Image := TImageObj.SimpleCreate(Master, S);
end;

constructor TBlockLI.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
var
  TT: TBlockLI;
begin
  inherited CreateCopy(AMasterList, T);
  TT := T as TBlockLI;
  FListType := TT.FListType;
  FListNumb := TT.FListNumb;
  FListStyleType := TT.FListStyleType;
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

function TBlockLI.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
begin
  if Assigned(Image) then
  begin
    Image.DrawLogic(ParentSectionList, Canvas, nil, 100, 0);
    if (Image.Image = ErrorBitmap) then
    begin
      Image.Free;
      Image := nil;
    end;
  end;
  ParentSectionList.FirstLineHtPtr := @FirstLineHt;
  FirstLineHt := 0;
  try
    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  finally
    ParentSectionList.FirstLineHtPtr := nil;
  end;
end;

////-- BG ---------------------------------------------------------- 16.09.2009 --
//function TBlockLI.getDisplay: TPropDisplay;
//begin
//  Result := FDisplay;
//end;

{----------------TBlockLI.Draw}

function TBlockLI.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;

const
  MaxRoman = 20;
  LowRoman: array[1..MaxRoman] of ThtString = ('i', 'ii', 'iii', 'iv', 'v', 'vi',
    'vii', 'viii', 'ix', 'x', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvi', 'xvii',
    'xviii', 'xix', 'xx');
  HighRoman: array[1..MaxRoman] of ThtString = ('I', 'II', 'III', 'IV', 'V', 'VI',
    'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 'XV', 'XVI', 'XVII',
    'XVIII', 'XIX', 'XX');
var
  NStr: ThtString;
  BkMode, TAlign: Integer;
  PenColor, BrushColor: TColor;
  PenStyle: TPenStyle;
  BrushStyle: TBrushStyle;
  YB, AlphaNumb: Integer;

  procedure Circle(X, Y, Rad: Integer);
  begin
    Canvas.Ellipse(X, Y - Rad, X + Rad, Y);
  end;

begin
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);

  X := X + Indent;

  if FirstLineHt > 0 then
  begin
    YB := FirstLineHt - ParentSectionList.YOff;
    if (YB < ARect.Top - 50) or (YB > ARect.Bottom + 50) then
      Exit;
    if Assigned(Image) and (Image.Image <> DefBitmap) and ParentSectionList.ShowImages then
    begin
      Image.DoDraw(Canvas, X - 16, YB - Image.ObjHeight, Image.Image, Image.Mask);
    end

    else if not (ListType in [None, Definition]) then
    begin
      if ListStyleType in [lbDecimal, lbLowerAlpha, lbLowerRoman, lbUpperAlpha, lbUpperRoman] then
      begin
        AlphaNumb := Min(ListNumb - 1, 25);
        case ListStyleType of
          lbLowerAlpha: NStr := chr(ord('a') + AlphaNumb);
          lbUpperAlpha: NStr := chr(ord('A') + AlphaNumb);
          lbLowerRoman: NStr := LowRoman[Min(ListNumb, MaxRoman)];
          lbUpperRoman: NStr := HighRoman[Min(ListNumb, MaxRoman)];
        else
          NStr := IntToStr(ListNumb);
        end;
        Canvas.Font := ListFont;
        NStr := NStr + '.';
        BkMode := SetBkMode(Canvas.Handle, Transparent);
        TAlign := SetTextAlign(Canvas.Handle, TA_BASELINE);
        Canvas.TextOut(X - 10 - Canvas.TextWidth(NStr), YB, NStr);
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
                Circle(X - 16, YB, 7);
              end;
            lbDisc:
              Circle(X - 15, YB - 1, 5);
            lbSquare: Rectangle(X - 15, YB - 6, X - 10, YB - 1);
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
  Image: ThtString;
  Val: TColor;
begin
  inherited;
  positioning := PosStatic; {7.28}
  Prop.GetBackgroundPos(0, 0, PRec);
  if Prop.GetBackgroundImage(Image) and (Image <> '') then
    Master.SetBackgroundBitmap(Image, PRec);
  Val := Prop.GetBackgroundColor;
  if Val <> clNone then
    Master.SetBackGround(Val or PalRelative);
end;

{----------------TBodyBlock.GetURL}

function TBodyBlock.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj};
  var ATitle: ThtString): guResultType;
begin
  if (BlockTitle <> '') then
  begin
    ATitle := BlockTitle;
    Result := [guTitle];
  end;
  Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
end;

{----------------TBodyBlock.DrawLogic}

function TBodyBlock.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
var
  ScrollWidth: Integer;
  Lindex, RIndex, AutoCount: Integer;
  SaveID: TObject;
  ClientContentBot: Integer;
begin
  YDraw := Y;
  StartCurs := Curs;
  StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderStyle, BorderWidth, AutoCount, MargArray);

  NewWidth := IMgr.Width - (MargArray[MarginLeft] + MargArray[PaddingLeft] +
    MargArray[BorderLeftWidth] + MargArray[MarginRight] +
    MargArray[PaddingRight] + MargArray[BorderRightWidth]);

  X := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  DrawTop := MargArray[MarginTop];

  MyCell.IMgr := IMgr;

  SaveID := IMgr.CurrentID;
  Imgr.CurrentID := Self;
  LIndex := IMgr.SetLeftIndent(X, Y);
  RIndex := IMgr.SetRightIndent(X + NewWidth, Y);

  ContentTop := Y + MargArray[MarginTop] + MargArray[PaddingTop] + MargArray[BorderTopWidth];
  ContentLeft := X;
  MyCell.DoLogicX(Canvas, X, ContentTop, 0, 0, NewWidth,
    AHeight - MargArray[MarginTop] - MargArray[MarginBottom], BlHt, ScrollWidth, Curs);

  Len := Curs - StartCurs;

  ClientContentBot := Max(ContentTop, MyCell.tcContentBot);
  ContentBot := ClientContentBot + MargArray[PaddingBottom] +
    MargArray[BorderBottomWidth] + MargArray[MarginBottom];
  DrawBot := Max(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom]
    + MargArray[BorderBottomWidth];

  MyCell.tcDrawTop := 0;
  MyCell.tcContentBot := 999000;

  Result := DrawBot + MargArray[MarginBottom] - Y;
  SectionHeight := Result;
  IMgr.FreeLeftIndentRec(LIndex);
  IMgr.FreeRightIndentRec(RIndex);
  DrawHeight := IMgr.ImageBottom - Y; {in case image overhangs}
  Imgr.CurrentID := SaveID;
  if DrawHeight < SectionHeight then
    DrawHeight := SectionHeight;
  MaxWidth := Max(IMgr.Width, Max(ScrollWidth, NewWidth) + MargArray[MarginLeft] + MargArray[MarginRight]);
  if DrawList.Count = 0 then
    DrawSort;
end;

{----------------TBodyBlock.Draw}

function TBodyBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  SaveID: TObject;
  Y: Integer;
begin
  Y := YDraw;
  Result := Y + SectionHeight;

  X := IMgr.LfEdge + MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
  SaveID := IMgr.CurrentID;
  Imgr.CurrentID := Self;
  DrawTheList(Canvas, ARect, NewWidth, X, IMgr.LfEdge, 0);
  Imgr.CurrentID := SaveID;
end;

{----------------TSectionList}

constructor TSectionList.Create(Owner: THtmlViewerBase; APaintPanel: TWinControl);
begin
  inherited Create(Self);
  TheOwner := Owner;
  PPanel := APaintPanel;
  IDNameList := TIDNameList.Create(Self);
  htmlFormList := TFreeList.Create;
  AGifList := TList.Create;
  MapList := TFreeList.Create;
  FormControlList := TList.Create;
  MissingImages := ThtStringList.Create;
  MissingImages.Sorted := False;
  LinkList := TList.Create;
  PanelList := TList.Create;
  Styles := THtmlStyleList.Create(Self);
  DrawList := TDrawList.Create;
  PositionList := TList.Create;
  TabOrderList := ThtStringList.Create;
  TabOrderList.Sorted := True;
  TabOrderList.Duplicates := dupAccept;
  InLineList := TInlineList.Create(Self);
  ScaleX := 1.0;
  ScaleY := 1.0;
end;

{----------------TSectionList.CreateCopy}

constructor TSectionList.CreateCopy(T: TSectionList);
begin
  PrintTableBackground := T.PrintTableBackground;
  PrintBackground := T.PrintBackground;
  BitmapList := T.BitmapList; {same list}
  InlineList := T.InlineList; {same list}
  IsCopy := True;
  inherited CreateCopy(Self, T);
// r14: Added some fixes for object copies done using Move(), re-checked Unicode Support, minor fixes for file structure
// was: System.Move(T.ShowImages, ShowImages, DWord(@Background) - DWord(@ShowImages) + Sizeof(Integer));
  //BG, 08.06.2010: Issue 9: Getting black background
  // this Move() copied 24 fields including a ThtString, not only ShowImages.
  // re-introduce Move() after moving ThtString out of the moved area between ShowImages and Background.
  //ShowImages := T.ShowImages;
  System.Move(T.ShowImages, ShowImages, DWord(@Background) - DWord(@ShowImages) + Sizeof(Integer));
  PreFontName := T.PreFontName;
  htmlFormList := TFreeList.Create; {no copy of list made}
  AGifList := TList.Create;
  MapList := TFreeList.Create;
  MissingImages := ThtStringList.Create;
  PanelList := TList.Create;
  DrawList := TDrawList.Create;
  ScaleX := 1.0;
  ScaleY := 1.0;
end;

destructor TSectionList.Destroy;
begin
  inherited Destroy; // Yunqa.de: Destroy calls Clear, so do this first.
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
end;

function TSectionList.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj};
  var ATitle: ThtString): guResultType;
var
  OldLink: TFontObj;
  OldImage: TImageObj;
begin
  OldLink := ActiveLink;
  OldImage := ActiveImage;
  ActiveLink := nil;
  ActiveImage := nil;
  Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  if LinksActive and (ActiveLink <> OldLink) then
  begin
    if OldLink <> nil then
      OldLink.SetAllHovers(LinkList, False);
    if ActiveLink <> nil then
      ActiveLink.SetAllHovers(LinkList, True);
    PPanel.Invalidate;
  end;
  if (ActiveImage <> OldImage) then
  begin
    if OldImage <> nil then
      OldImage.Hover := hvOff;
  end;
  if ActiveImage <> nil then
    if Word(GetKeyState(VK_LBUTTON)) and $8000 <> 0 then
      ActiveImage.Hover := hvOverDown
    else
      ActiveImage.Hover := hvOverUp;
end;

procedure TSectionList.LButtonDown(Down: boolean);
{called from htmlview.pas when left mouse button depressed}
begin
  if ActiveImage <> nil then
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
    ActiveLink := nil;
  end;
  if Assigned(ActiveImage) then
  begin
    ActiveImage.Hover := hvOff;
    ActiveImage := nil;
  end;
end;

procedure TSectionList.CheckGIFList(Sender: TObject);
var
  I: Integer;
  Frame: Integer;
begin
  if IsCopy then
    Exit;
  Frame := 0;
  if Assigned(BackgroundAniGif) then
    Frame := BackgroundAniGif.CurrentFrame;
  for I := 0 to AGifList.Count - 1 do
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
  I, J: Integer;
begin
  {After next Draw, hide all formcontrols that aren't to be shown}
  for I := 0 to htmlFormList.Count - 1 do
    with ThtmlForm(htmlFormList.Items[I]) do
      for J := 0 to ControlList.Count - 1 do
        with TFormControlObj(ControlList.Items[J]) do
          ShowIt := False;
  for I := 0 to PanelList.Count - 1 do
    TPanelObj(PanelList[I]).ShowIt := False; {same for panels}
end;

procedure TSectionList.SetYOffset(Y: Integer);
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
  BackgroundBitmap := nil;
  BackgroundMask := nil;
  BackgroundAniGif := nil;
  if BitmapLoaded and (BitmapName <> '') then
    BitmapList.DecUsage(BitmapName);
  BitmapName := '';
  BitmapLoaded := False;
  AGifList.Clear;
  FreeAndNil(Timer);
  SelB := 0;
  SelE := 0;
  MapList.Clear;
  MissingImages.Clear;
  if Assigned(LinkList) then
    LinkList.Clear;
  ActiveLink := nil;
  ActiveImage := nil;
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

function TSectionList.GetSelLength: Integer;
var
  I: Integer;
begin
  Result := 0;
  if SelE <= SelB then
    Exit; {nothing to do}
  CB := SelTextCount.Create;
  try
    for I := 0 to Count - 1 do
      with TSectionBase(Items[I]) do
      begin
        if (SelB >= StartCurs + Len) then
          Continue;
        if (SelE <= StartCurs) then
          Break;
        CopyToClipboard;
      end;
    Result := CB.Terminate;
  finally
    CB.Free;
  end;
end;

{----------------TSectionList.CopyToClipboard}

procedure TSectionList.CopyToClipboardA(Leng: Integer);
var
  I: Integer;
  SB: TSectionBase;
begin
  if SelE <= SelB then
    Exit; {nothing to do}
  try
    CB := ClipBuffer.Create(Leng);
    for I := 0 to Count - 1 do
    begin
      SB := TSectionBase(Items[I]);
      with SB do
      begin
        if (SelB >= StartCurs + Len) then
          Continue;
        if (SelE <= StartCurs) then
          Break;
        CopyToClipboard;
      end;
    end;
    CB.Terminate;
  finally
    CB.Free;
  end;
end;

{----------------TSectionList.GetSelTextBuf}

function TSectionList.GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;

var
  I: Integer;
begin
  if BufSize >= 1 then
  begin
    Buffer[0] := #0;
    Result := 1;
  end
  else
    Result := 0;
  if SelE <= SelB then
    Exit; {nothing to do}
  CB := SelTextBuf.Create(Buffer, BufSize);
  try
    for I := 0 to Count - 1 do
      with TSectionBase(Items[I]) do
      begin
        if (SelB >= StartCurs + Len) then
          Continue;
        if (SelE <= StartCurs) then
          Break;
        CopyToClipboard;
      end;
    Result := CB.Terminate;
  finally
    CB.Free;
  end;
end;

{----------------TSectionList.DoLogic}

function TSectionList.DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
  var ScrollWidth: Integer; var Curs: Integer): Integer;
var
  I, J: Integer;
begin
  Inc(CycleNumber);
  TableNestLevel := 0;
  NLevel := 0;
  InLogic2 := False;
  if Assigned(Timer) then
    Timer.Enabled := False;
  for I := 0 to htmlFormList.Count - 1 do
    ThtmlForm(htmlFormList.Items[I]).SetSizes(Canvas);
  SetTextJustification(Canvas.Handle, 0, 0);
  TInlineList(InlineList).NeedsConverting := True;

{set up the tab order for form controls according to the TabIndex attributes}
  if Assigned(TabOrderList) and (TabOrderList.Count > 0) then
    with TabOrderList do
    begin
      J := 0; {tab order starts with 0}
      for I := 0 to Count - 1 do {list is sorted into proper order}
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
      TabOrderList.Clear; {only need do this once}
    end;

  Result := inherited DoLogic(Canvas, Y, Width, AHeight, BlHt, ScrollWidth, Curs);

  for I := 0 to AGifList.Count - 1 do
    with TGifImage(AGifList.Items[I]) do
    begin
      Animate := False; {starts iteration count from 1}
      if not Self.IsCopy then
        Animate := True;
    end;
  if not IsCopy and not Assigned(Timer) then
  begin
    Timer := TTimer.Create(TheOwner);
    Timer.Interval := 50;
    Timer.OnTimer := CheckGIFList;
  end;
  if Assigned(Timer) then
    Timer.Enabled := AGifList.Count >= 1;
  AdjustFormControls;
  if not IsCopy and (PositionList.Count = 0) then
  begin
    AddSectionsToList;
  end;
end;

//-- BG ---------------------------------------------------------- 11.09.2010 --
procedure TSectionList.AddSectionsToPositionList(Sections: TSectionBase);
begin
  inherited;
  PositionList.Add(Sections);
end;

procedure TSectionList.AdjustFormControls;
var
  I: Integer;
  Control: TControl;
  Showing: boolean;

{$IFNDEF FastRadio}

  function ActiveInList: boolean; {see if active control is a form control}
  var
    Control: TWinControl;
    I: Integer;
  begin
    with FormControlList do
    begin
      Result := False;
      Control := Screen.ActiveControl;
      for I := 0 to Count - 1 do
        with TFormControlObj(Items[I]) do
          if FControl = Control then
          begin
            Result := True;
            Break;
          end;
    end;
  end;
{$ENDIF}

begin
  if IsCopy or (FormControlList.Count = 0) then
    Exit;
  with FormControlList do
{$IFNDEF FastRadio}
    if not ActiveInList then
    begin {if none of the formcontrols are active, turn off tabs for those off screen}
      for I := 0 to Count - 1 do
        with TFormControlObj(Items[I]) do
          if not ShowIt and Assigned(FControl) then
            FControl.Hide; {hides and turns off tabs}
    end
    else
{$ENDIF}
    begin
      Control := TheOwner; {THtmlViewer}
      repeat
        Showing := Control.Visible;
        Control := Control.Parent;
      until not Showing or not Assigned(Control);
      if Showing then
        for I := 0 to Count - 1 do
          with TFormControlObj(Items[I]) do
            if not ShowIt and Assigned(FControl) then
            begin
              FControl.Show; {turns on tabs}
              FControl.Left := -4000; {but it still can't be seen}
            end;
    end;
end;

{----------------TSectionList.Draw}

function TSectionList.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer;
  Y, XRef, YRef: Integer): Integer;
var
  OldPal: HPalette;
  I: Integer;
begin
  PageBottom := ARect.Bottom + YOff;
  PageShortened := False;
  FirstPageItem := True;
  TableNestLevel := 0;
  SkipDraw := False;

  if Assigned(Timer) then
    Timer.Enabled := False;
  for I := 0 to AGifList.Count - 1 do
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
  else
    OldPal := 0;
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
    for I := 0 to PanelList.Count - 1 do
      with TPanelObj(PanelList[I]) do
        if not ShowIt then
          Panel.Hide;
    YOffChange := False;
  end;
  if Assigned(Timer) then
    Timer.Enabled := AGifList.Count >= 1;
end;

procedure TSectionList.SetFonts(const Name, PreName: ThtString; ASize: Integer;
  AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
  LnksActive: boolean; LinkUnderLine: boolean; ACharSet: TFontCharSet;
  MarginHeight, MarginWidth: Integer);
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

procedure TSectionList.SetBackgroundBitmap(const Name: ThtString; const APrec: PtPositionRec);
begin
  BackgroundBitmap := nil;
  BackgroundAniGif := nil;
  BitmapName := Name;
  BitmapLoaded := False;
  BackgroundPRec := APrec;
end;

{----------------TSectionList.InsertImage}

procedure TSectionList.InsertImage(const Src: ThtString; Stream: TMemoryStream;
  var Reformat: boolean);
var
  UName: ThtString;
  I, J: Integer;
  Pair: TBitmapItem;
  Rformat, Error: boolean;
  Image: TgpObject;
  AMask: TBitmap;
  Tr, Transparent: Transparency;
  Obj: TObject;
begin
  Image := nil;
  AMask := nil;
  Error := False;
  Reformat := False;
  UName := Trim(Uppercase(Src));
  I := BitmapList.IndexOf(UName); {first see if the bitmap is already loaded}
  J := MissingImages.IndexOf(UName); {see if it's in missing image list}
  if (I = -1) and (J >= 0) then
  begin
    Transparent := NotTransp;
    Image := LoadImageFromStream(Stream, Transparent, AMask);
    if Assigned(Image) then {put in Cache}
    try
      if Assigned(AMask) then
        Tr := Transparent
      else
        Tr := NotTransp;
      Pair := TBitmapItem.Create(Image, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair); {put new bitmap in list}
        BitmapList.DecUsage(UName); {this does not count as being used yet}
      except
        Pair.Mask := nil;
        Pair.MImage := nil;
        Pair.Free;
      end;
    except {accept inability to create}
    end
    else
      Error := True; {bad stream or Nil}
  end;
  if (I >= 0) or Assigned(Image) or Error then {a valid image in the Cache or Bad stream}
  begin
    while J >= 0 do
    begin
      Obj := MissingImages.Objects[J];
      if (Obj = Self) and not IsCopy and not Error then
        BitmapLoaded := False {the background image, set to load}
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

function TSectionList.GetTheBitmap(const BMName: ThtString; var Transparent: Transparency;
  var AMask: TBitmap; var FromCache, Delay: boolean): TgpObject;
{Note: bitmaps and Mask returned by this routine are on "loan".  Do not destroy
 them}
{Transparent may be set to NotTransp or LLCorner on entry but may discover it's
 TGif here}

var
  UName: ThtString;
  I: Integer;
  Pair: TBitmapItem;
  Tr: Transparency;
  Stream: TMemoryStream;
  Color: TColor;
begin
  AMask := nil;
  Delay := False;
  FromCache := False;
  if BMName <> '' then
  begin
    UName := Trim(Uppercase(BMName));
    I := BitmapList.IndexOf(UName); {first see if the bitmap is already loaded}
    if I > -1 then
    begin {yes, handle the case where the image is already loaded}
      Result := BitmapList.GetImage(I);
      FromCache := True;
      if Result is TBitmap then
        with BitmapList.Objects[I] as TBitmapItem do
        begin
          if Transp = TrGif then
            Transparent := TrGif {it's a transparent GIF}
          else if Transp = TrPng then
            Transparent := TrPng
          else if Transparent = LLCorner then
          begin
            if not Assigned(Mask) then {1st bitmap may not have been marked transp}
              Mask := GetImageMask(TBitmap(MImage), False, 0);
            if Assigned(Mask) then
              Transp := LLCorner;
          end;
          AMask := Mask;
        end;
      Exit;
    end;

  {The image is not loaded yet, need to get it}
    Result := nil;
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
            Transparent := TrGif;
          end
          else if (Transparent = LLCorner) then
            AMask := GetImageMask(TBitmap(Result), False, 0);
      end;
      if Assigned(GetImage) then
      begin {the OnImageRequest}
        Stream := nil;
        GetImage(TheOwner, BMName, Stream);
        if Stream = WaitStream then
          Delay := True
        else if Assigned(Stream) then
          try
            Result := LoadImageFromStream(Stream, Transparent, AMask);
          finally
            if assigned(GottenImage) then
              GottenImage(TheOwner, BMName, Stream);
          end
        else
          Result := LoadImageFromFile(TheOwner.HtmlExpandFilename(BMName), Transparent, AMask);
      end;
    end
    else
      Result := LoadImageFromFile(BMName, Transparent, AMask);
    if Assigned(Result) then {put in Image List for use later also}
    try
      if Assigned(AMask) then
        Tr := Transparent
      else
        Tr := NotTransp;
      Pair := TBitmapItem.Create(Result, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair); {put new bitmap in list}
      except
        Pair.Mask := nil;
        Pair.MImage := nil;
        Pair.Free;
      end;
    except {accept inability to create}
    end;
  end
  else
    Result := nil;
end;

{----------------TSectionList.FindSectionAtPosition}

function TSectionList.FindSectionAtPosition(Pos: Integer;
  var TopPos: Integer; var Index: Integer): TSectionBase;
var
  I: Integer;
begin
  with PositionList do
    for I := Count - 1 downto 0 do
      if TSectionBase(Items[I]).YPosition <= Pos then
      begin
        Result := TSectionBase(Items[I]);
        TopPos := Result.YPosition;
        Index := I;
        Exit;
      end;
  Result := nil;
end;

procedure TSectionList.GetBackgroundBitmap;
var
  Mask: TBitmap;
  Dummy1: Transparency;
  TmpResult: TgpObject;
  FromCache, Delay: boolean;
  Rslt: ThtString;
begin
  if ShowImages and not BitmapLoaded and (BitmapName <> '') then
  begin
    if not Assigned(BackgroundBitmap) then
    begin
      Dummy1 := NotTransp;
      if not Assigned(GetBitmap) and not Assigned(GetImage) then
        BitmapName := TheOwner.HtmlExpandFilename(BitmapName)
      else if Assigned(ExpandName) then
      begin
        ExpandName(TheOwner, BitmapName, Rslt);
        BitmapName := Rslt;
      end;
      TmpResult := GetTheBitmap(BitmapName, Dummy1, Mask, FromCache, Delay); {might be Nil}
      if (TmpResult is TBitmap) {$IFNDEF NoGDIPlus}or (TmpResult is TGpImage){$ENDIF !NoGDIPlus} then
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
{$IFNDEF NoMetafile}
      else if TmpResult is ThtMetaFile then
      begin
        BackgroundBitmap := ThtMetaFile(TmpResult);
      end
{$ENDIF}
      else
      begin
        BackgroundBitmap := nil;
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
  I: Integer;
begin
  if htmlFormList.Count > 0 then
  begin
    Result := TFreeList.Create;
    for I := 0 to htmlFormList.Count - 1 do
      Result.Add(ThtmlForm(htmlFormList[I]).GetFormSubmission);
  end
  else
    Result := nil;
end;

procedure TSectionList.SetFormcontrolData(T: TFreeList);
var
  I: Integer;
begin
  try
    for I := 0 to T.Count - 1 do
      if htmlFormList.Count > I then
        ThtmlForm(htmlFormList[I]).SetFormData(ThtStringList(T[I]));
  except end;
end;

{----------------TSectionList.FindDocPos}

function TSectionList.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
begin
  Result := inherited FindDocPos(SourcePos, Prev);
  if Result < 0 then {if not found return 1 past last ThtChar}
    Result := Len;
end;

{----------------TSectionList.CursorToXY}

function TSectionList.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer;
  var Y: Integer): boolean;
var
  Beyond: boolean;
begin
  Beyond := Cursor >= Len;
  if Beyond then
    Cursor := Len - 1;
  Result := inherited CursorToXY(Canvas, Cursor, X, Y);
  if Beyond then
    X := X + 15;
end;

procedure TSectionList.ProcessInlines(SIndex: Integer; Prop: TProperties; Start: boolean);
{called when an inline property is found to specify a border}
var
  I, EmSize, ExSize: Integer;
  IR: InlineRec;
  MargArrayO: TVMarginArray;
  Dummy1: Integer;
begin
  with InlineList do
  begin
    if Start then
    begin {this is for border start}
      IR := InlineRec.Create;
      InlineList.Add(IR);
      with IR do
      begin
        StartBDoc := SIndex; {Source index for border start}
        IDB := Prop.ID; {property ID}
        EndB := 999999; {end isn't known yet}
        Prop.GetVMarginArray(MargArrayO);
        EmSize := Prop.EmSize;
        ExSize := Prop.ExSize;
        ConvMargArray(MargArrayO, 200, 200, EmSize, ExSize, bssNone, 0{4}, Dummy1, MargArray);
      end;
    end
    else {this call has end information}
      for I := Count - 1 downto 0 do {the record we want is probably the last one}
      begin
        IR := InlineRec(Items[I]);
        if Prop.ID = IR.IDB then {check the ID to make sure}
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
{convert all the list data from source ThtChar positions to display ThtChar positions}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with InlineRec(Items[I]) do
    begin
      StartB := Owner.FindDocPos(StartBDoc, False);
      EndB := Owner.FindDocPos(EndBDoc, False);
      if StartB = EndB then
        Dec(StartB); {this takes care of images, form controls}
    end;
  NeedsConverting := False;
end;

function TInlineList.GetStartB(I: Integer): Integer;
begin
  if NeedsConverting then
    AdjustValues;
  if (I < Count) and (I >= 0) then
    Result := InlineRec(Items[I]).StartB
  else
    Result := 99999999;
end;

function TInlineList.GetEndB(I: Integer): Integer;
begin
  if NeedsConverting then
    AdjustValues;
  if (I < Count) and (I >= 0) then
    Result := InlineRec(Items[I]).EndB
  else
    Result := 99999999;
end;

{----------------TCellObj.Create}

constructor TCellObj.Create(Master: TSectionBaseList; AVAlign: AlignmentType; Attr: TAttributeList; Prop: TProperties);
{Note: on entry Attr and Prop may be Nil when dummy cells are being created}
var
  I, AutoCount: Integer;
  Color: TColor;
  BackgroundImage: ThtString;
  Percent: boolean;
  Algn: AlignmentType;
  J: PropIndices;
  Border: Boolean;
begin
  inherited Create;
  Cell := TCellObjCell.Create(Master);
  if Assigned(Prop) then
    Cell.Title := Prop.PropTitle;
  ColSpan := 1;
  RowSpan := 1;
  VAlign := AVAlign;
  if Assigned(Attr) then
    for I := 0 to Attr.Count - 1 do
      with TAttribute(Attr[I]) do
        case Which of
          ColSpanSy:
            if Value > 1 then
              ColSpan := Value;
          RowSpanSy:
            if Value > 1 then
              RowSpan := Value;
          WidthSy:
            if Pos('%', Name) > 0 then
            begin
              if (Value > 0) and (Value <= 100) then
              begin
                WidthAttr := Value * 10;
                AsPercent := True;
              end;
            end
            else if (Value > 0) then
              WidthAttr := Value;
          HeightSy:
            if Pos('%', Name) = 0 then
              SpecHt := Value
            else
              SpecHtPercent := Max(0, Min(Value, 100));
          BGColorSy:
            Cell.BkGnd := ColorFromString(Name, False, Cell.BkColor);
          BackgroundSy: BackgroundImage := Name;
          HRefSy: Cell.Url := Name;
          TargetSy: Cell.Target := Name;
        end;

  if Assigned(Prop) then
  begin {Caption does not have Prop}
    if Prop.GetVertAlign(Algn) and (Algn in [Atop, AMiddle, ABottom]) then
      Valign := Algn;
    Prop.GetVMarginArray(MargArrayO);
    EmSize := Prop.EmSize;
    ExSize := Prop.ExSize;
    Percent := (VarIsStr(MargArrayO[Width])) and (Pos('%', MargArrayO[Width]) > 0);
    ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, bssNone, 0, AutoCount, MargArray);
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
    Prop.GetBackgroundImage(BackgroundImage); {'none' will change ThtString to empty}
    if BackgroundImage <> '' then
    begin
      BGImage := TImageObj.SimpleCreate(Master, BackgroundImage);
      Prop.GetBackgroundPos(EmSize, ExSize, PRec);
    end;

  {In the following, Padding widths in percent aren't accepted}
    ConvMargArrayForCellPadding(MargArrayO, EmSize, ExSize, MargArray);
    PadTop := MargArray[PaddingTop];
    PadRight := MargArray[PaddingRight];
    PadBottom := MargArray[PaddingBottom];
    PadLeft := MargArray[PaddingLeft];

    Border := True;
    if BorderStyleType(MargArray[BorderTopStyle]) <> bssNone then
    begin
      Border := False;
      BrdTop := MargArray[BorderTopWidth];
    end;
    if BorderStyleType(MargArray[BorderRightStyle]) <> bssNone then
    begin
      Border := False;
      BrdRight := MargArray[BorderRightWidth];
    end;
    if BorderStyleType(MargArray[BorderBottomStyle]) <> bssNone then
    begin
      Border := False;
      BrdBottom := MargArray[BorderBottomWidth];
    end;
    if BorderStyleType(MargArray[BorderLeftStyle]) <> bssNone then
    begin
      Border := False;
      BrdLeft := MargArray[BorderLeftWidth];
    end;
    if Border then
      BorderStyle := Prop.GetBorderStyle
    else
      BorderStyle := bssNone;

    for J := BorderTopColor to BorderLeftColor do
      if MargArray[J] = clNone then
        MargArray[J] := clSilver;

    Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
    ShowEmptyCells := Prop.ShowEmptyCells;
  end;
end;

constructor TCellObj.CreateCopy(AMasterList: TSectionBaseList; T: TCellObj);
begin
  inherited create;
  Cell := TCellObjCell.CreateCopy(AMasterList, T.Cell);
  Move(T.ColSpan, ColSpan, DWord(@Cell) - DWord(@ColSpan));

  if Cell.MasterList.PrintTableBackground then
  begin
    Cell.BkGnd := T.Cell.BkGnd;
    Cell.BkColor := T.Cell.BkColor;
  end
  else
    Cell.BkGnd := False;
  if Assigned(T.BGImage) and Cell.MasterList.PrintTableBackground then
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

procedure TCellObj.InitializeCell(TablePadding: Integer; const BkImageName: ThtString;
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
    BrdLeft := Max(1, BrdLeft);
    BrdRight := Max(1, BrdRight);
    BrdTop := Max(1, BrdTop);
    BrdBottom := Max(1, BrdBottom);
  end;
  HzSpace := PadLeft + BrdLeft + BrdRight + PadRight;
  VrSpace := PadTop + BrdTop + BrdBottom + PadBottom;

  if (BkImageName <> '') and not Assigned(BGImage) then
  begin
    BGImage := TImageObj.SimpleCreate(Cell.MasterList, BkImageName);
    PRec := APrec;
  end;
end;

{----------------TCellObj.DrawLogic2}

procedure TCellObj.DrawLogic2(Canvas: TCanvas; Y, CellSpacing: Integer; var Curs: Integer);
var
  Dummy: Integer;
  Tmp: Integer;
begin
  if Cell.Count > 0 then
  begin
    Tmp := Ht - VSize - (VrSpace + CellSpacing);
    case VAlign of
      ATop: YIndent := 0;
      AMiddle: YIndent := Tmp div 2;
      ABottom, ABaseline: YIndent := Tmp;
    end;
    Dummy := 0;
    Cell.DoLogic(Canvas, Y + PadTop + BrdTop + CellSpacing + YIndent, Wd - (HzSpace + CellSpacing),
      Ht - VrSpace - CellSpacing, 0, Dummy, Curs);
  end;
  if Assigned(BGImage) and Cell.MasterList.ShowImages then
  begin
    BGImage.DrawLogic(Cell.MasterList, Canvas, nil, 100, 0);
    if BGImage.Image = ErrorBitmap then
    begin
      BGImage.Free;
      BGImage := nil;
    end
    else
    begin
      BGImage.ImageKnown := True; {won't need reformat on InsertImage}
      NeedDoImageStuff := True;
    end;
  end;
end;

{----------------TCellObj.Draw}

procedure TCellObj.Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: Integer;
  Border: boolean; Light, Dark: TColor);
var
  YO: Integer;
  BL, BT, BR, BB, PL, PT, PR, PB: Integer;
  ImgOK: boolean;
  IT, IH, FT, Rslt: Integer;
  Rgn, SaveRgn: HRgn;
  Point: TPoint;
  SizeV, SizeW: TSize;
  HF, VF: double;
  BRect: TRect;
  IsVisible: Boolean;

  procedure InitFullBg(W, H: Integer);
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
    FullBG.Height := Max(H, 2);
    FullBG.Width := Max(W, 2);
  end;

begin
  YO := Y - Cell.MasterList.YOff;

  BL := X + CellSpacing; {Border left and right}
  BR := X + Wd;
  PL := BL + BrdLeft; {Padding left and right}
  PR := BR - BrdRight;

  BT := YO + CellSpacing; {Border Top and Bottom}
  BB := YO + Ht;
  PT := BT + BrdTop; {Padding Top and Bottom}
  PB := BB - BrdBottom;

  IT := Max(0, Arect.Top - 2 - PT);
  FT := Max(PT, ARect.Top - 2); {top of area drawn, screen coordinates}
  IH := Min(PB - FT, Arect.Bottom - FT); {height of area actually drawn}

  Cell.MyRect := Rect(BL, BT, BR, BB);
  if not (BT <= ARect.Bottom) and (BB >= ARect.Top) then
    Exit;

  try
    if NeedDoImageStuff then
    begin
      if BGImage = nil then
        NeedDoImageStuff := False
      else if BGImage.Image <> DefBitmap then
      begin
        if BGImage.Image = ErrorBitmap then {Skip the background image}
          FreeAndNil(BGImage)
        else
        try
          DoImageStuff(Canvas, Wd - CellSpacing, Ht - CellSpacing,
            BGImage, PRec, TiledImage, TiledMask, NoMask);
          if Cell.MasterList.IsCopy and (TiledImage is TBitmap) then
            TBitmap(TiledImage).HandleType := bmDIB;
        except {bad image, get rid of it}
          FreeAndNil(BGImage);
          FreeAndNil(TiledImage);
          FreeAndNil(TiledMask);
        end;
        NeedDoImageStuff := False;
      end;
    end;

    ImgOK := not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
      and Cell.MasterList.ShowImages;

    if Cell.BkGnd then
    begin
      Canvas.Brush.Color := Cell.BkColor or PalRelative;
      Canvas.Brush.Style := bsSolid;
      if Cell.MasterList.IsCopy and ImgOK then
      begin
        InitFullBG(PR - PL, IH);
        FullBG.Canvas.Brush.Color := Cell.BkColor or PalRelative;
        FullBG.Canvas.Brush.Style := bsSolid;
        FullBG.Canvas.FillRect(Rect(0, 0, PR - PL, IH));
      end
      else
      begin
        {slip under border to fill gap when printing}
        BRect := Rect(PL, FT, PR, FT + IH);
        if BorderStyle = bssNone then
        begin
          if MargArray[BorderLeftWidth] > 0 then
            Dec(BRect.Left);
          if MargArray[BorderTopWidth] > 0 then
            Dec(BRect.Top);
          if MargArray[BorderRightWidth] > 0 then
            Inc(BRect.Right);
          if MargArray[BorderBottomWidth] > 0 then
            Inc(BRect.Bottom);
        end
        else
          if Border then
            InflateRect(BRect, -1, -1);
        Canvas.FillRect(BRect);
      end;
    end;
    if ImgOK then
    begin
      if not Cell.MasterList.IsCopy then
        {$IFNDEF NoGDIPlus}
        if TiledImage is TgpBitmap then
          DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PL, FT, 0, IT, PR - PL, IH)
        else
        {$ENDIF !NoGDIPlus}
        if NoMask then
          BitBlt(Canvas.Handle, PL, FT, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcCopy)
        else
        begin
          InitFullBG(PR - PL, IH);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, Canvas.Handle, PL, FT, SrcCopy);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
          BitBlt(Canvas.Handle, PL, FT, PR - PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
        end
      else
      {$IFNDEF NoGDIPlus}
      if TiledImage is TGpBitmap then {printing}
      begin
        if Cell.BkGnd then
        begin
          DrawGpImage(FullBg.Canvas.Handle, TgpImage(TiledImage), 0, 0);
          PrintBitmap(Canvas, PL, FT, PR - PL, IH, FullBG);
        end
        else
          PrintGpImageDirect(Canvas.Handle, TgpImage(TiledImage), PL, PT,
            Cell.MasterList.ScaleX, Cell.MasterList.ScaleY);
      end
      else
      {$ENDIF !NoGDIPlus}
      if NoMask then
        PrintBitmap(Canvas, PL, FT, PR - PL, IH, TBitmap(TiledImage))
      else if Cell.BkGnd then
      begin
        InitFullBG(PR - PL, IH);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
        PrintBitmap(Canvas, PL, FT, PR - PL, IH, FullBG);
      end
      else
        PrintTransparentBitmap3(Canvas, PL, FT, PR - PL, IH, TBitmap(TiledImage), TiledMask, IT, IH);
    end;
  except
  end;

  IsVisible := (YO < ARect.Bottom + 200) and (YO + Ht > -200);
  try
    if IsVisible and (Cell.Count > 0) then
    begin
    {clip cell contents to prevent overflow.  First check to see if there is
     already a clip region}
      SaveRgn := CreateRectRgn(0, 0, 1, 1);
      Rslt := GetClipRgn(Canvas.Handle, SaveRgn); {Rslt = 1 for existing region, 0 for none}
    {Form the region for this cell}
      GetWindowOrgEx(Canvas.Handle, Point); {when scrolling or animated Gifs, canvas may not start at X=0, Y=0}
      if not Cell.MasterList.Printing then
        if IsWin95 then
          Rgn := CreateRectRgn(BL - Point.X, Max(BT - Point.Y, -32000), X + Wd - Point.X, Min(YO + Ht - Point.Y, 32000))
        else
          Rgn := CreateRectRgn(BL - Point.X, BT - Point.Y, X + Wd - Point.X, YO + Ht - Point.Y)
      else
      begin
        GetViewportExtEx(Canvas.Handle, SizeV);
        GetWindowExtEx(Canvas.Handle, SizeW);
        HF := (SizeV.cx / SizeW.cx); {Horizontal adjustment factor}
        VF := (SizeV.cy / SizeW.cy); {Vertical adjustment factor}
        if IsWin95 then
          Rgn := CreateRectRgn(Round(HF * (BL - Point.X) - 1), Max(Round(VF * (BT - Point.Y) - 1), -32000), Round(HF * (X + Wd - Point.X) + 1), Min(Round(VF * (YO + Ht - Point.Y)), 32000))
        else
          Rgn := CreateRectRgn(Round(HF * (BL - Point.X) - 1), Round(VF * (BT - Point.Y) - 1), Round(HF * (X + Wd - Point.X) + 1), Round(VF * (YO + Ht - Point.Y)));
      end;
      if Rslt = 1 then {if there was a region, use the intersection with this region}
        CombineRgn(Rgn, Rgn, SaveRgn, Rgn_And);
      SelectClipRgn(Canvas.Handle, Rgn);
      try
        Cell.Draw(Canvas, ARect, Wd - HzSpace - CellSpacing,
          X + PadLeft + BrdLeft + CellSpacing,
          Y + PadTop + BrdTop + YIndent, ARect.Left, 0); {possibly should be IRgn.LfEdge}
      finally
        if Rslt = 1 then {restore any previous clip region}
          SelectClipRgn(Canvas.Handle, SaveRgn)
        else
          SelectClipRgn(Canvas.Handle, 0);
        DeleteObject(Rgn);
        DeleteObject(SaveRgn);
      end;
    end;
  except
  end;

  Cell.DrawYY := Y;
  if IsVisible and ((Cell.Count > 0) or ShowEmptyCells) then
    try
      DrawBorder(Canvas, Rect(BL, BT, BR, BB), Rect(PL, PT, PR, PB),
        htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
        htStyles(BorderStyleType(MargArray[BorderLeftStyle]), BorderStyleType(MargArray[BorderTopStyle]), BorderStyleType(MargArray[BorderRightStyle]), BorderStyleType(MargArray[BorderBottomStyle])),
        MargArray[BackgroundColor], Cell.MasterList.Printing);
    except
    end;
end;

{----------------TCellList.Create}

constructor TCellList.Create(Attr: TAttributeList; Prop: TProperties);
var
  I: Integer;
  Color: TColor;
begin
  inherited Create;
  if Assigned(Attr) then
    for I := 0 to Attr.Count - 1 do
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
              SpecRowHeightPercent := Max(0, Min(Value, 100));
        end;
  if Assigned(Prop) then
  begin
    Color := Prop.GetBackgroundColor;
    if Color <> clNone then
    begin
      BkGnd := True;
      BkColor := Color;
    end;
    Prop.GetBackgroundImage(BkImage); {'none' will change ThtString to empty}
    if BkImage <> '' then
      Prop.GetBackgroundPos(Prop.EmSize, Prop.ExSize, APRec);
    Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
  end;
end;

{----------------TCellList.CreateCopy}

constructor TCellList.CreateCopy(AMasterList: TSectionBaseList; T: TCellList);
var
  I: Integer;
begin
  inherited create;
  BreakBefore := T.BreakBefore;
  BreakAfter := T.BreakAfter;
  KeepIntact := T.KeepIntact;
  RowType := T.Rowtype;
  for I := 0 to T.Count - 1 do
    if Assigned(T.Items[I]) then
      Add(TCellObj.CreateCopy(AMasterList, T.Items[I]))
    else
      Add(nil);
end;

procedure TCellList.Add(CellObj: TCellObj);
begin
  inherited Add(CellObj);
  if Assigned(CellObj) then
  begin
    BreakBefore := BreakBefore or CellObj.BreakBefore;
    BreakAfter := BreakAfter or CellObj.BreakAfter;
    KeepIntact := KeepIntact or CellObj.KeepIntact;
    if SpecRowHeight > 0 then
      CellObj.SpecHt := Max(SpecRowHeight, CellObj.SpecHt)
    else if SpecRowHeightPercent > 0 then
      CellObj.SpecHtPercent := Max(SpecRowHeightPercent, CellObj.SpecHt);
  end;
end;

{----------------TCellList.InitializeRow}

procedure TCellList.InitializeRow;
var
  I: Integer;
begin
  if BkGnd then
    for I := 0 to Count - 1 do
      with TCellObj(Items[I]).Cell do
        if not BkGnd then
        begin
          BkGnd := True;
          BkColor := Self.BkColor;
        end;
end;

{----------------TCellList.DrawLogic1}

function TCellList.DrawLogic1(Canvas: TCanvas; const Widths: array of Integer; Span,
  CellSpacing, AHeight, Rows: Integer; var Desired: Integer; var Spec, More: boolean): Integer;
{Find vertical size of each cell, Row height of this row.  But final Y position
 is not known at this time.
 Rows is number rows in table.
 AHeight is for calculating percentage heights}
var
  I, J, Dummy: Integer;
  DummyCurs, GuessHt: Integer;
  CellObj: TCellObj;
begin
  Result := 0;
  Desired := 0;
  Spec := False;
  DummyCurs := 0;
  More := False;
  for I := 0 to Count - 1 do
  begin
    CellObj := TCellObj(Items[I]);
    if Assigned(CellObj) then
      with CellObj do
        if ColSpan > 0 then {skip the dummy cells}
        begin
          Wd := 0;
          for J := I to ColSpan + I - 1 do
            Inc(Wd, Widths[J]); {accumulate column widths}
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
            VSize := Cell.DoLogic(Canvas, 0, Wd - HzSpace - CellSpacing, Max(0, GuessHt - VrSpace), 0,
              Dummy, DummyCurs);
            Result := Max(Result, VSize + VrSpace);
            if SpecHt > 0 then
            begin
              Result := Max(Result, Max(VSize, SpecHt));
              Spec := True;
            end
            else if SpecHtPercent > 0 then
            begin
              Desired := Max(Desired, GuessHt);
              Spec := True;
            end;
          end
          else if RowSpan > Span then
            More := True;
        end;
  end;
  Desired := Max(Result, Desired);
end;

{----------------TCellList.DrawLogic2}

procedure TCellList.DrawLogic2(Canvas: TCanvas; Y: Integer;
  CellSpacing: Integer; var Curs: Integer);
{Calc Y indents. Set up Y positions of all cells.}
var
  I: Integer;
  CellObj: TCellObj;
begin
  for I := 0 to Count - 1 do
  begin
    CellObj := TCellObj(Items[I]);
    if Assigned(CellObj) then
      CellObj.DrawLogic2(Canvas, Y, CellSpacing, Curs);
  end;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
function TCellList.getCellObj(Index: Integer): TCellObj;
begin
  Result := TCellObj(inherited Items[Index]);
end;

{----------------TCellList.Draw}

function TCellList.Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
  const Widths: array of Integer; X: Integer; Y, YOffset: Integer;
  CellSpacing: Integer; Border: boolean; Light, Dark: TColor;
  MyRow: Integer): Integer;
var
  I, Spacing: Integer;
  YO: Integer;
  CellObj: TCellObj;
begin
  YO := Y - YOffset;
  Result := RowHeight + Y;
  Spacing := CellSpacing div 2;

  with MasterList do {check CSS page break properties}
    if Printing then
      if BreakBefore then
      begin
        if YO > ARect.Top then {page-break-before}
        begin
          if Y + Spacing < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Y + Spacing;
          end;
          Exit;
        end;
      end
      else if KeepIntact then
      begin
      {Try to fit this RowSpan on a page by itself}
        if (YO > ARect.Top) and (Y + RowSpanHeight > PageBottom) and
          (RowSpanHeight < ARect.Bottom - ARect.Top) then
        begin
          if Y < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Y;
          end;
          Exit;
        end
        else if (YO > ARect.Top) and (Y + RowHeight > PageBottom) and
          (RowHeight < ARect.Bottom - ARect.Top) then
        begin
          if Y + Spacing < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Y + Spacing;
          end;
          Exit;
        end;
      end
      else if BreakAfter then
        if ARect.Top + YOff < Result then {page-break-after}
          if Result + Spacing < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Result + Spacing;
          end;

  with MasterList do {avoid splitting any small rows}
    if Printing and (RowSpanHeight <= 100) and
      (Y + RowSpanHeight > PageBottom) then
    begin
      if Y < PageBottom then
      begin
        PageShortened := True;
        PageBottom := Y;
      end;
      Exit;
    end;

  if (YO + RowSpanHeight >= ARect.Top) and (YO < ARect.Bottom) and
    (not MasterList.Printing or (Y < MasterList.PageBottom)) then
    for I := 0 to Count - 1 do
    begin
      CellObj := TCellObj(Items[I]);
      if Assigned(CellObj) then
        CellObj.Draw(Canvas, ARect, X, Y, CellSpacing, Border, Light, Dark);
      X := X + Widths[I];
    end;
end;

{----------------THtmlTable.Create}

constructor THtmlTable.Create(Master: TSectionList; Attr: TAttributeList;
  Prop: TProperties);
var
  I: Integer;
begin
  //BG, 08.06.2010: TODO:  Issue 5: Table border versus stylesheets:
  //  Added: BorderColor
  inherited Create(Master, Prop);
  Rows := TFreeList.Create;
  CellPadding := 1;
  CellSpacing := 2;
  BorderColor := clNone;
  BorderColorLight := clBtnHighLight;
  BorderColorDark := clBtnShadow;
  for I := 0 to Attr.Count - 1 do
    with TAttribute(Attr[I]) do
      case Which of
        BorderSy:
          //BG, 15.10.2010: issue 5: set border width only, if style does not set any border width:
          if not Prop.HasBorder then
            if Name = '' then
              BorderWidth := 1
            else
              BorderWidth := Min(100, Max(0, Value)); {Border=0 is no border}

        CellSpacingSy:
          CellSpacing := Min(40, Max(-1, Value));

        CellPaddingSy:
          CellPadding := Min(50, Max(0, Value));

        BorderColorSy:
          ColorFromString(Name, False, BorderColor);

        BorderColorLightSy:
          ColorFromString(Name, False, BorderColorLight);

        BorderColorDarkSy:
          ColorFromString(Name, False, BorderColorDark);
      end;
  if Prop.Collapse then
    Cellspacing := -1;
end;

{----------------THtmlTable.CreateCopy}

constructor THtmlTable.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
var
  I: Integer;
  HtmlTable: THtmlTable absolute T;
begin
  assert(T is THtmlTable);
  inherited CreateCopy(AMasterList, T);
  Rows := TFreeList.Create;
  for I := 0 to HtmlTable.Rows.Count - 1 do
    Rows.Add(TCellList.CreateCopy(ParentSectionList, TCellList(HtmlTable.Rows.Items[I])));

  Move(HtmlTable.ListsProcessed, ListsProcessed, DWord(@EndList) - DWord(@ListsProcessed));

  SetLength(Widths, NumCols);
  SetLength(MaxWidths, NumCols);
  SetLength(MinWidths, NumCols);
  SetLength(Percents, NumCols);

  if ParentSectionList.PrintTableBackground then
  begin
    BkGnd := HtmlTable.BkGnd;
    BkColor := HtmlTable.BkColor;
  end
  else
    BkGnd := False;
  TablePartRec := TTablePartRec.Create;
  TablePartRec.TablePart := Normal;
end;

{----------------THtmlTable.Destroy}

destructor THtmlTable.Destroy;
begin
  Rows.Free;
  TablePartRec.Free;
  FreeAndNil(ColInfo);
  inherited Destroy;
end;

{----------------THtmlTable.DoColumns}

procedure THtmlTable.DoColumns(Width: Integer; AsPercent: boolean;
  VAlign: AlignmentType; const Align: ThtString);
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

{----------------THtmlTable.AddDummyCells}

procedure THtmlTable.AddDummyCells;
var
  Cl, Rw, K, RowCount: Integer;
  AnyAbsolute: boolean;
  Row: TCellList;
  CellObj: TCellObj;
  SpanEq0: boolean;

  function DummyCell(RSpan: Integer): TCellObj;
  begin
    Result := TCellObj.Create(ParentSectionList, ATop, nil, nil);
    Result.ColSpan := 0;
    Result.RowSpan := RSpan;
  end;

begin
  RowCount := Rows.Count;
  if not ListsProcessed then
  begin {put dummy cells in rows to make up for ColSpan > 1}
    NumCols := 0;
    AnyAbsolute := False;
    for Rw := 0 to RowCount - 1 do
    begin
      with TCellList(Rows[Rw]) do
      begin
        InitializeRow;
        for Cl := Count - 1 downto 0 do
          with TCellObj(Items[Cl]) do
          begin
            InitializeCell(CellPadding, BkImage, APRec, Self.BorderWidth > 0);
            if WidthAttr > 0 then
            begin
              if not AsPercent then
                AnyAbsolute := True;
            end;
            if Self.BkGnd and not Cell.BkGnd then {transfer bgcolor to cells if no Table image}
            begin
              Cell.BkGnd := True;
              Cell.BkColor := Self.BkColor;
            end;
            RowSpan := Min(RowSpan, RowCount - Rw); {So can't extend beyond table}
            for K := 1 to ColSpan - 1 do
              if RowSpan > 1 then
                TCellList(Rows[Rw]).Insert(Cl + K, DummyCell(RowSpan)) {these could be
                Nil also except they're needed for expansion in the next section}
              else
                TCellList(Rows[Rw]).Insert(Cl + K, DummyCell(1));
          end;
      end;
      NumCols := Max(NumCols, TCellList(Rows[Rw]).Count); {temporary # cols}
    end;

  {Absolute calc only if  some absolute widths entered}
    UseAbsolute := AnyAbsolute;

  {put dummy cells in cols to make up for RowSpan > 1}
    for Cl := 0 to NumCols - 1 do
      for Rw := 0 to RowCount - 1 do
        with TCellList(Rows[Rw]) do
          if Count > Cl then
            if Assigned(Items[Cl]) then
              with TCellObj(Items[Cl]) do
              begin
                RowSpan := Min(RowSpan, RowCount - Rw); {practical limit}
                if RowSpan > 1 then
                  for K := Rw + 1 to Rw + RowSpan - 1 do
                  begin {insert dummy cells in following rows if RowSpan > 1}
                    while TCellList(Rows[K]).Count < Cl do {add padding if row is short}
                      TCellList(Rows[K]).Add(DummyCell(0));
                    TCellList(Rows[K]).Insert(Cl, DummyCell(0));
                  end;
              end;

  {look for excessive Colspans on last cells in each row.  These would be dummy
   cells added above with Colspan = 0}
    if (RowCount > 0) and (NumCols > 0) then
      repeat
        SpanEq0 := True; {assume there are some}
        for Rw := 0 to RowCount - 1 do
          with TCellList(Rows[Rw]) do
            if (Count = NumCols) and (TCellObj(Items[NumCols - 1]).ColSpan <> 0) then
              SpanEq0 := False; {at least one last cell is not a dummy}
        if SpanEq0 then
        begin {trim off the dummy cells on end and fixup the Colspan value which was to blame}
          for Rw := 0 to RowCount - 1 do
            with TCellList(Rows[Rw]) do
              if (Count = NumCols) and (TCellObj(Items[NumCols - 1]).ColSpan = 0) then
              begin
                Delete(NumCols - 1); {trim cell on end}
                K := NumCols - 2;
                while K >= 0 do {find the Colspan value}
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

    NumCols := 0; {find the number of columns}
    for Rw := 0 to RowCount - 1 do
    begin
      NumCols := Max(NumCols, TCellList(Rows[Rw]).Count);
    end;

  {add the width info from the <col> tags to the cells}
    if Assigned(colInfo) then
    begin
      AnyAbsolute := False;
      for Rw := 0 to RowCount - 1 do
      begin
        Row := TCellList(Rows[Rw]);
        for Cl := 0 to Min(Row.Count - 1, NumCols - 1) do
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
      FreeAndNil(colInfo); {no longer needed}
    end;

    SetLength(Widths, NumCols);
    SetLength(MaxWidths, NumCols);
    SetLength(MinWidths, NumCols);
    SetLength(Percents, NumCols);

    ListsProcessed := True;
  end; {if not ListsProcessed}
end;

{----------------THtmlTable.GetMinMaxAbs}

procedure THtmlTable.GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
  TotalMaxWidth: Integer);
var
  I, J, Min, Max, N, Span, Addon, D: Integer;
  More: boolean;
  CellObj: TCellObj;

label Two;

begin
  for I := 0 to NumCols - 1 do
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
    for J := 0 to Rows.Count - 1 do
      with TCellList(Rows[J]) do
      begin
        for I := 0 to Count - 1 do
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
                    Max := Math.Max(Min, WidthAttr + Addon);
                    Min := Max;
                  end;
                  MinWidths[I] := Math.Max(MinWidths[I], Min);
                  MaxWidths[I] := Math.Max(MaxWidths[I], Max);
                end
                else
                begin
                  TotalMinWidth := 0; TotalMaxWidth := 0;
                  for N := I to I + ColSpan - 1 do
                  begin {find the current totals for the span}
                    Inc(TotalMaxWidth, MaxWidths[N]);
                    Inc(TotalMinWidth, MinWidths[N]);
                  end;
                  if not AsPercent and (WidthAttr > 0) then
                  begin
                    Min := Math.Max(Min, WidthAttr {+Cellspacing});
                    Max := Math.Max(Min, WidthAttr {+Cellspacing});
                  end;
                  if (TotalMinWidth < Min) then
                    if TotalMinWidth > 0 then
                    begin
                      D := Min - TotalMinWidth;
                      for N := I to I + ColSpan - 1 do {increase the sub widths to match the span}
                        MinWidths[N] := MinWidths[N] + MulDiv(MinWidths[N], D, TotalMinWidth);
                    end
                    else
                      MinWidths[I] := Min; {this for multiple empty cols}
                  if (TotalMaxWidth < Max) then
                    if TotalMaxWidth > 0 then
                    begin {increase the sub widths to match the span}
                      D := Max - TotalMaxWidth;
                      for N := I to I + ColSpan - 1 do {increase the sub widths to match the span}
                        MaxWidths[N] := MaxWidths[N] + MulDiv(MaxWidths[N], D, TotalMaxWidth);
                    end
                    else
                      MaxWidths[I] := Max;
                end;
              end;
            end;
          end;
      end;
    Inc(Span);
  end;

{Find the total min and max width}
  Two:
  TotalMaxWidth := 0; TotalMinWidth := 0;
  for I := 0 to NumCols - 1 do
  begin
    Inc(TotalMaxWidth, MaxWidths[I]);
    Inc(TotalMinWidth, MinWidths[I]);
  end;

end;

{----------------THtmlTable.GetWidthsAbs}

procedure THtmlTable.GetWidthsAbs(Canvas: TCanvas; TablWidth: Integer;
  Specified: boolean);
var
  N, D, W, dd, TotalMinWidth, TotalMaxWidth: Integer;
  Accum: Integer;
begin
  GetMinMaxAbs(Canvas, TotalMinWidth, TotalMaxWidth);

  if TotalMinWidth > TablWidth then {use the minimum column widths, table will expand}
    Widths := Copy(MinWidths)
  else if (TotalMaxWidth <= TablWidth) and not Specified then
  {use the max column widths, table will be smaller}
    Widths := Copy(MaxWidths)
  else {make table fit}
  begin
    D := TotalMaxWidth - TotalMinWidth;
    W := TablWidth - TotalMinWidth;
    if D > 0 then {expand only those columns with some slop in them}
    begin
      Accum := 0;
      for N := 0 to NumCols - 1 do
      begin
        dd := MaxWidths[N] - MinWidths[N]; {some dd's may be 0}
        Widths[N] := MinWidths[N] + MulDiv(dd, W, D);
        Inc(Accum, Widths[N]);
      end;
      dd := Accum - TablWidth; {check for Roundoff error}
      if dd <> 0 then
      begin
        for N := 0 to NumCols - 1 do
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
    else {no adjustable columns, will have to expand them all}
      for N := 0 to NumCols - 1 do
        Widths[N] := MinWidths[N] + MulDiv(MinWidths[N], W, TotalMinWidth);
  end;
end;

{----------------THtmlTable.GetWidths}

procedure THtmlTable.GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: Integer;
  TheWidth: Integer);
var
  I, J, Min, Max, N, Span, Addon, Distributable, TotalPC, Accum,
    ExcessMin, ExcessMax, NonPC, PCWidth, NewTotalPC: Integer;
  More: boolean;

begin
{Find the max and min widths of each column}
  for I := 0 to NumCols - 1 do
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
    for J := 0 to Rows.Count - 1 do
      with TCellList(Rows[J]) do
      begin
        for I := 0 to Count - 1 do
          if Assigned(Items[I]) then
            with TCellObj(Items[I]) do
            begin
              PCWidth := 0;
              if WidthAttr > 0 then
                if AsPercent then
                  PCWidth := WidthAttr
                else if TheWidth > 0 then
                  PCWidth := Math.Min(1000, MulDiv(WidthAttr, 1000, TheWidth));
              More := More or (ColSpan > Span); {set if need another iteration}
              if ColSpan = Span then
              begin
                Cell.MinMaxWidth(Canvas, Min, Max);
                Addon := CellSpacing + HzSpace;
                Inc(Min, Addon);
                Inc(Max, Addon);
                if Span = 1 then
                begin
                  MaxWidths[I] := Math.Max(MaxWidths[I], Max);
                  MinWidths[I] := Math.Max(MinWidths[I], Min);
                  Percents[I] := Math.Max(Percents[I], PCWidth); {collect percents}
                end
                else
                begin
                  TotalMaxWidth := 0; TotalMinWidth := 0;
                  TotalPC := 0; NonPC := 0;
                  for N := I to I + ColSpan - 1 do
                  begin {Total up the pertinant column widths}
                    Inc(TotalMaxWidth, MaxWidths[N]);
                    Inc(TotalMinWidth, MinWidths[N]);
                    if Percents[N] > 0 then
                      Inc(TotalPC, Percents[N]) {total percents}
                    else
                      Inc(NonPC); {count of cell with no percent}
                  end;
                  if Colspan = NumCols then
                  begin
                    TotalMinWidth := Math.Max(TotalMinWidth, TheWidth);
                    TotalMaxWidth := Math.Max(TotalMaxWidth, TheWidth);
                  end;
                  ExcessMin := Min - TotalMinWidth;
                  ExcessMax := Max - TotalMaxWidth;
                  if (PCWidth > 0) or (TotalPC > 0) then
                  begin {manipulate for percentages}
                    if NonPC > 0 then
                    {find the extra percentages to divvy up}
                      Distributable := Math.Max(0, (PCWidth - TotalPC) div NonPC)
                    else
                      Distributable := 0;
                    if (NonPC = 0) and (PCWidth > TotalPC + 1) then
                    begin
                      for N := I to I + ColSpan - 1 do {stretch percentages to fit}
                        Percents[N] := MulDiv(Percents[N], PCWidth, TotalPC);
                    end
                    else if Distributable > 0 then {spread colspan percentage excess over the unspecified cols}
                      for N := I to I + ColSpan - 1 do
                        if Percents[N] = 0 then
                          Percents[N] := Distributable;
                    NewTotalPC := Math.Max(TotalPC, PCWidth);
                    if ExcessMin > 0 then
                    begin
                      if (NonPC > 0) and (TotalMaxWidth > 0) then {split excess over all cells}
                      begin
                      {proportion the distribution so cells with large MaxWidth get more}
                        for N := I to I + ColSpan - 1 do
                          Inc(MinWidths[N], MulDiv(ExcessMin, MaxWidths[N], TotalMaxWidth));
                      end
                      else
                        for N := I to I + ColSpan - 1 do
                          Inc(MinWidths[N], (MulDiv(ExcessMin, Percents[N], NewTotalPC)));
                    end;
                    if ExcessMax > 0 then
                      for N := I to I + ColSpan - 1 do
                        Inc(MaxWidths[N], (MulDiv(ExcessMax, Percents[N], NewTotalPC)));
                  end
                  else
                  begin {no width dimensions entered}
                    if ExcessMin > 0 then
                    begin
                      Accum := 0;
                      for N := I to I + ColSpan - 1 do
                      begin
                        if TotalMinWidth = 0 then
                          MinWidths[N] := Min div ColSpan
                        else {split up the widths in proportion to widths already there}
                          MinWidths[N] := MulDiv(Min, MinWidths[N], TotalMinWidth);
                        Inc(Accum, MinWidths[N]);
                      end;
                      if Accum < Min then {might be a roundoff pixel or two left over}
                        Inc(MinWidths[I], Min - Accum);
                    end;
                    if ExcessMax > 0 then
                    begin
                      Accum := 0;
                      for N := I to I + ColSpan - 1 do
                      begin
                        if TotalMaxWidth = 0 then
                          MaxWidths[N] := Max div ColSpan
                        else {split up the widths in proportion to widths already there}
                          MaxWidths[N] := MulDiv(Max, MaxWidths[N], TotalMaxWidth);
                        Inc(Accum, MaxWidths[N]);
                      end;
                      if Accum < Max then {might be a roundoff pixel or two left over}
                        Inc(MaxWidths[I], Max - Accum);
                    end;
                  end;
                end;
              end;
            end;
      end;
    Inc(Span);
  end;

  TotalMaxWidth := 0; TotalMinWidth := 0;
  for I := 0 to NumCols - 1 do
  begin
    Inc(TotalMaxWidth, MaxWidths[I]);
    Inc(TotalMinWidth, MinWidths[I]);
  end;
end;

{----------------THtmlTable.MinMaxWidth}

procedure THtmlTable.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
begin
  AddDummyCells; {in case it hasn't been done}
  if UseAbsolute and (tblWidthAttr = 0) then
    GetMinMaxAbs(Canvas, Min, Max)
  else
    GetWidths(Canvas, Min, Max, tblWidthAttr);

  Inc(Min, CellSpacing);
  Inc(Max, CellSpacing);
  Min := Math.Max(Min, tblWidthAttr);
  Max := Math.Max(Max, tblWidthAttr);
end;

procedure THtmlTable.TableSpecifiedAndWillFit(TheWidth: Integer);
{Divide up the table into columns.  TheWidth is the specified width of the table.
 At this point, it is known that everything will fit into TheWidth. Percents are
 being used}
var
  I, W, PCNotMinWid, TotalWid, Unsp, UnspDiff, Delta, Addon, Count, d: Integer;
  UseMin: array of boolean;
  NoChange: boolean;
  UnspCol: Integer;
begin
  if NumCols = 0 then
    Exit;
  SetLength(UseMin, NumCols);
  for I := 0 to NumCols - 1 do
    UseMin[I] := False;
  PCNotMinWid := 0; TotalWid := 0; Unsp := 0; UnspDiff := 0; UnspCol := -1;
{First calculate everything assuming the data entered is perfectly correct}
  for I := 0 to NumCols - 1 do
  begin
    if Percents[I] > 0 then
    begin
      W := MulDiv(TheWidth, Percents[I], 1000); {width based on percentage}
      if W > MinWidths[I] then
      begin
        Widths[I] := W;
        Inc(PCNotMinWid, Percents[I]);
      end
      else
      begin {percent is too small, use Min width}
        Widths[I] := MinWidths[I];
        UseMin[I] := True;
      end;
    end
    else
    begin {no percent}
      Widths[I] := MinWidths[I];
      Inc(Unsp); {an unspecified column}
      UnspCol := I; {save location of unspedified column}
      Inc(UnspDiff, Max(0, MaxWidths[I] - MinWidths[I])); {total max-min for unspecified cols}
    end;
    Inc(TotalWid, Widths[I]);
  end;

  Delta := TotalWid - TheWidth; {see what the error is}
  if Delta < 0 then {table is too small}
  begin
    if Unsp > 0 then
    begin
      if (UnspDiff > 0) and (UnspDiff >= Abs(Delta) div 4) then
      {increase the unspecified columns widths prop to Max, Min unless the difference is trivial}
      begin
        for I := 0 to NumCols - 1 do
          if (Percents[I] = 0) then
            Inc(Widths[I], MulDiv(-Delta, Max(0, MaxWidths[I] - MinWidths[I]), UnspDiff));
      end
      else
      begin {increase the unspecified columns widths uniformly}
        Addon := -Delta div Unsp;
        for I := 0 to NumCols - 1 do
          if (Percents[I] = 0) then
            Inc(Widths[I], Addon);
      end;
    end
    else
    begin {no unspecified widths, increase the specified columns which are not minimum}
      for I := 0 to NumCols - 1 do
        if (Percents[I] > 0) and not UseMin[I] then
          Inc(Widths[I], MulDiv(-Delta, Percents[I], PCNotMinWid));
    end;
  end
  else if Delta > 0 then {calculated table is too large}
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
          begin {new width is smaller than MinWidth, make adustments}
            UseMin[I] := True;
            NoChange := False;
            Dec(PCNotMinWid, Percents[I]);
            Dec(Delta, Widths[I] - MinWidths[I]);
            Widths[I] := MinWidths[I];
          end;
        end;
      Inc(Count);
    until NoChange or (Count >= 4); {count guards against endless loop}
    for I := 0 to NumCols - 1 do {now actually change the widths}
      if (Percents[I] > 0) and not UseMin[I] then
        Dec(Widths[I], MulDiv(Delta, Percents[I], PCNotMinWid));
  end;

  TotalWid := 0; {fix up any round off errors}
  for I := 0 to NumCols - 1 do
    Inc(TotalWid, Widths[I]);
  Delta := TotalWid - TheWidth; {round off error}
  if Delta > 0 then
  begin
    for I := 0 to NumCols - 1 do
      if not UseMin[I] then
      begin
        Dec(Widths[I], Delta); {remove extra from first non minimum}
        Break;
      end;
  end
  else if Length(Widths) > 0 then
    if UnspCol >= 0 then
      Inc(Widths[UnspCol], -Delta) {put it into an unspecified column}
    else
    begin
      Delta := -Delta; {Delta is now positive}
      while Delta > NumCols do
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
      for I := 0 to NumCols - 1 do
        if Delta > 0 then
        begin
          Inc(Widths[I]);
          Dec(Delta);
        end
        else
          Break;
    end;
end;

{----------------THtmlTable.TableNotSpecifiedAndWillFit}

procedure THtmlTable.TableNotSpecifiedAndWillFit(TotalMinWidth, TotalMaxWidth, TheWidth: Integer);
{Find column widths.  Table known to fit within allowed space and its width hasn't
 been specified}
var
  D, W, DS, MaxDS, MaxI, I, Addon,
    Total, TotalPC, Residual, NewResidual, W1, W2, NewTotal, LastNewTotal: Integer;
  HasPercents, UsesPercents, Done: boolean;
begin
  if NumCols = 0 then
    Exit;
  TotalPC := 0; {see if any percentage widths entered}
  for I := 0 to NumCols - 1 do
    Inc(TotalPC, Percents[I]);
  UsesPercents := (TotalPc > 0) and (TotalPc <= 1000) {ignore ridiculous values}
    or (tblWidthAttr > 0);

  if UsesPercents then
  begin {find the largest width that will accomodate the %'s}
    Residual := 0; W1 := 0;
    for I := 0 to NumCols - 1 do
      if Percents[I] > 0 then {a percent has been entered}
        W1 := Max(W1, MulDiv(MaxWidths[I], 1000, Percents[I])) {look for maximum}
      else
        Inc(Residual, MaxWidths[I]); {accumlate the cols which have no percent}
    if TotalPC < 1000 then
      W2 := MulDiv(Residual, 1000, 1000 - TotalPC)
    else if Residual > 0 then
      W2 := 30000
    else
      W2 := 0;
    Total := Max(W1, W2);
    if Total <= TheWidth then
    begin {a fit is found using percents and maxwidths}
      if tblWidthAttr > 0 then
        Total := TheWidth; {don't try to make it smaller than TheWidth}
      NewResidual := MulDiv(Total, 1000 - TotalPC, 1000);
      for I := 0 to NumCols - 1 do
        if Percents[I] > 0 then {figure widths to fit this situation}
          Widths[I] := MulDiv(Total, Percents[I], 1000)
        else if Residual > 0 then
          Widths[I] := MulDiv(MaxWidths[I], NewResidual, Residual)
        else
          Widths[I] := 0; {this is an table syntax error condition}
      Exit;
    end;

    Done := False;
    LastNewTotal := $FFFFFFF;
    repeat {with the above possibilites taken care of, we can assume the final
           width will = NewWidth}
      HasPercents := False;
      Total := 0; Residual := 0;
      for I := 0 to NumCols - 1 do
      begin
        if Percents[I] > 0 then
        begin
          W := MulDiv(TheWidth, Percents[I], 1000); {a Percent's width based on TheWidth}
          if W < MinWidths[I] then {but it must be > MinWidth}
          begin {eliminate the percentage value as not achievable}
            Percents[I] := 0;
            Inc(Residual, MinWidths[I]); {and put it in the residuals}
          end
          else
          begin
            HasPercents := True; {still valid percents}
            Inc(Total, W);
          end;
        end
        else
          Inc(Residual, MinWidths[I]);
      end;
      if not HasPercents then
        Break; {no percents are achievable}
      if Total + Residual <= TheWidth then
      begin {a solution with at least some percentages can be found}
        Done := True;
        TotalMaxWidth := 0; TotalMinWidth := 0; {recalc these}
        for I := 0 to NumCols - 1 do
        begin
          if Percents[I] > 0 then
          begin
            MinWidths[I] := MulDiv(TheWidth, Percents[I], 1000);
            MaxWidths[I] := MinWidths[I]; {this fixes the width thru later calculations}
          end;
          Inc(TotalMaxWidth, MaxWidths[I]);
          Inc(TotalMinWidth, MinWidths[I]);
        end;
      end
      else {it doesn't fit screen, reduce percentages and try again}
      begin
        NewTotal := TheWidth - Residual; {percent items must fit this}
        while LastNewTotal <= NewTotal do
          Dec(NewTotal);
        LastNewTotal := NewTotal;
        for I := 0 to NumCols - 1 do
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
    for I := 0 to NumCols - 1 do
    begin
      Widths[I] := MinWidths[I] + Addon;
      Inc(Total, Widths[I]);
    end;
  end
  else
  begin
    MaxDS := 0;
    for I := 0 to NumCols - 1 do
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
  if Total <> TheWidth then {a round off error}
    Inc(Widths[MaxI], TheWidth - Total); {adjust column with largest variation}
end;

{----------------THtmlTable.FindRowHeights}

procedure THtmlTable.FindRowHeights(Canvas: TCanvas; AHeight: Integer);
var
  I, J, K, H, Span, TotalMinHt, TotalDesHt, AddOn,
    Sum, AddedOn, Desired, UnSpec: Integer;
  More, Mr, IsSpeced: boolean;
  MinHts, DesiredHts: IntArray;
  SpecHts: array of boolean;
  F: double;
begin
  if Rows.Count = 0 then
    Exit;
  Dec(AHeight, CellSpacing); {calculated heights will include one cellspacing each,
  this removes that last odd cellspacing}
  if Length(Heights) = 0 then
    SetLength(Heights, Rows.Count);
  SetLength(DesiredHts, Rows.Count);
  SetLength(MinHts, Rows.Count);
  SetLength(SpecHts, Rows.Count);
  for I := 0 to Rows.Count - 1 do
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
    for J := 0 to Rows.Count - 1 do
      with TCellList(Rows[J]) do
      begin
        if J + Span > Rows.Count then
          Break; {otherwise will overlap}
        H := DrawLogic1(Canvas, Widths, Span, CellSpacing, Max(0, AHeight - Rows.Count * CellSpacing),
          Rows.Count, Desired, IsSpeced, Mr) + CellSpacing;
        Inc(Desired, Cellspacing);
        More := More or Mr;
        if Span = 1 then
        begin
          MinHts[J] := H;
          DesiredHts[J] := Desired;
          SpecHts[J] := SpecHts[J] or IsSpeced;
        end
        else if H > Cellspacing then {if H=Cellspacing then no rowspan for this span}
        begin
          TotalMinHt := 0; {sum up the heights so far for the rows involved}
          TotalDesHt := 0;
          for K := J to J + Span - 1 do
          begin
            Inc(TotalMinHt, MinHts[K]);
            Inc(TotalDesHt, DesiredHts[K]);
            SpecHts[K] := SpecHts[K] or IsSpeced;
          end;
          if H > TotalMinHt then {apportion the excess over the rows}
          begin
            Addon := ((H - TotalMinHt) div Span);
            AddedOn := 0;
            for K := J to J + Span - 1 do
            begin
              Inc(MinHts[K], Addon);
              Inc(AddedOn, Addon);
            end;
            Inc(MinHts[J + Span - 1], (H - TotalMinHt) - AddedOn); {make up for round off error}
          end;
          if Desired > TotalDesHt then {apportion the excess over the rows}
          begin
            Addon := ((Desired - TotalDesHt) div Span);
            AddedOn := 0;
            for K := J to J + Span - 1 do
            begin
              Inc(DesiredHts[K], Addon);
              Inc(AddedOn, Addon);
            end;
            Inc(DesiredHts[J + Span - 1], (Desired - TotalDesHt) - AddedOn); {make up for round off error}
          end;
        end;
      end;
    Inc(Span);
  end;

  TotalMinHt := 0;
  TotalDesHt := 0;
  UnSpec := 0;
  for I := 0 to Rows.Count - 1 do
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
    begin {expand the unspeced rows to fit}
      Heights := Copy(DesiredHts);
      Addon := (AHeight - TotalDesHt) div UnSpec;
      Sum := 0;
      for I := 0 to Rows.Count - 1 do
        if not SpecHts[I] then
        begin
          Dec(UnSpec);
          if UnSpec > 0 then
          begin
            Inc(Heights[I], AddOn);
            Inc(Sum, Addon);
          end
          else
          begin {last item, complete everything}
            Inc(Heights[I], AHeight - TotalDesHt - Sum);
            Break;
          end;
        end;
    end
    else
    begin {expand desired hts to fit}
      Sum := 0;
      F := AHeight / TotalDesHt;
      for I := 0 to Rows.Count - 2 do
      begin
        Heights[I] := Round(F * DesiredHts[I]);
        Inc(Sum, Heights[I]);
      end;
      Heights[Rows.Count - 1] := AHeight - Sum; {last row is the difference}
    end
  else
  begin
    Sum := 0;
    F := (AHeight - TotalMinHt) / (TotalDesHt - TotalMinHt);
    for I := 0 to Rows.Count - 2 do
    begin
      Heights[I] := MinHts[I] + Round(F * (DesiredHts[I] - MinHts[I]));
      Inc(Sum, Heights[I]);
    end;
    Heights[Rows.Count - 1] := AHeight - Sum;
  end;
end;

{----------------THtmlTable.DrawLogic}

function THtmlTable.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
var
  I, J, K,
    TotalMaxWidth, TotalMinWidth: Integer;
  NewWidth: Integer;
  OwnerWidth: Integer;
  Specified: boolean;
  TopY: Integer;
  FirstLinePtr: PInteger;
  CellObj: TCellObj;
  HasBody: Boolean;

begin
  Inc(ParentSectionList.TableNestLevel);
  Inc(NLevel);
  try
    YDraw := Y;
    TopY := Y;
    ContentTop := Y;
    DrawTop := Y;
    StartCurs := Curs;
    if Assigned(ParentSectionList.FirstLineHtPtr) and {used for List items}
      (ParentSectionList.FirstLineHtPtr^ = 0) then
      FirstLinePtr := ParentSectionList.FirstLineHtPtr {save for later}
    else
      FirstLinePtr := nil;

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
    begin {Table width not specified and at least one column has absolute width specified}
      GetWidthsAbs(Canvas, NewWidth, Specified); {fills in the Widths array}
    end
    else
    begin
      GetWidths(Canvas, TotalMinWidth, TotalMaxWidth, NewWidth);

      if (TotalMinWidth >= NewWidth) then
      begin {table won't fit, use minimun widths}
        if Assigned(MinWidths) then {Delphi 4 needs this check}
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
    for I := 0 to NumCols - 1 do
      Inc(TableWidth, Widths[I]);

    if (Length(Heights) = 0) then
      FindRowHeights(Canvas, AHeight)
    else if ParentSectionList.InLogic2 and (ParentSectionList.TableNestLevel <= 5) then
      FindRowHeights(Canvas, AHeight);

    SectionHeight := 0;
    HeaderHeight := 0;
    HeaderRowCount := 0;
    FootHeight := 0;
    FootStartRow := -1;
    HasBody := False;
    for J := 0 to Rows.Count - 1 do
      with TCellList(Rows[J]) do
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
          Inc(FootHeight, RowHeight);
        end
        else if Rowtype = TBody then
          HasBody := True;
        RowSpanHeight := 0;
        Inc(SectionHeight, Heights[J]);
        for I := 0 to Count - 1 do
          if Assigned(Items[I]) then
          begin
            CellObj := TCellObj(Items[I]);
            with CellObj do
            begin {find the actual height, Ht, of each cell}
              Ht := 0;
              for K := J to Min(J + RowSpan - 1, Rows.Count - 1) do
                Inc(Ht, Heights[K]);
              if RowSpanHeight < Ht then
                RowSpanHeight := Ht;
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

    Len := Curs - StartCurs;
    MaxWidth := TableWidth;
    Result := SectionHeight;
    DrawHeight := Result;
    ContentBot := TopY + SectionHeight;
    DrawBot := TopY + DrawHeight;
    try
      if Assigned(FirstLinePtr) then
        FirstLinePtr^ := YDraw + SectionHeight;
    except
    end;
  finally
    Dec(ParentSectionList.TableNestLevel);
    Dec(NLevel);
  end;
end;

{----------------THtmlTable.Draw}

function THtmlTable.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  YO, YOffset, Y: Integer;
begin
  Inc(ParentSectionList.TableNestLevel);
  Y := YDraw;
  Result := Y + SectionHeight;
  if Float then
    Y := Y + VSpace;
  YOffset := ParentSectionList.YOff;
  YO := Y - YOffset;

  if (YO + DrawHeight >= ARect.Top) and (YO < ARect.Bottom)
    or ParentSectionList.Printing then
    if ParentSectionList.Printing and (ParentSectionList.TableNestLevel = 1)
      and HeadOrFoot
      and (Y < ParentSectionList.PageBottom)
      and ((ParentSectionList.PrintingTable = nil) or
      (ParentSectionList.PrintingTable = Self)) then
      DrawTableP(Canvas, ARect, IMgr, X, Y)
    else
      DrawTable(Canvas, ARect, IMgr, X, Y);
  Dec(ParentSectionList.TableNestLevel);
end;

procedure THtmlTable.DrawTable(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X: Integer; Y: Integer);
var
  I, XX: Integer;
  YY, YOffset: Integer;
begin
  YOffset := ParentSectionList.YOff;
  XX := X + Indent; {for the table}
  YY := Y;
  DrawX := XX;
  DrawY := YY;
  for I := 0 to Rows.Count - 1 do
    YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
      XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
      BorderColorDark, I);
end;

procedure THtmlTable.DrawTableP(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X: Integer; Y: Integer);
{Printing table with thead and/or tfoot}
var
  I, XX, TopBorder, BottomBorder: Integer;
  YY, YOffset: Integer;
  SavePageBottom: Integer;
  Spacing, HeightNeeded: Integer;
begin
  YOffset := ParentSectionList.YOff;
  XX := X + Indent; {for the table}
  YY := Y;
  DrawX := XX;
  DrawY := YY;

  if TTableBlock(MyBlock).TableBorder then
  begin
    TopBorder := BorderWidth;
    BottomBorder := BorderWidth;
  end
  else
  begin
    TopBorder := TBlock(MyBlock).MargArray[BorderTopWidth];
    BottomBorder := TBlock(MyBlock).MargArray[BorderBottomWidth];
  end;

  case TablePartRec.TablePart of
  {.$Region 'Normal'}
    Normal:
      begin
        ParentSectionList.PrintingTable := Self;
        if ParentSectionList.PageBottom - Y >= TableHeight + BottomBorder then
        begin
          for I := 0 to Rows.Count - 1 do {do whole table now}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          ParentSectionList.PrintingTable := nil;
        end
        else
        begin {see if enough room on this page for header, 1 row, footer}
          if HeadOrFoot then
          begin
            Spacing := CellSpacing div 2;
            HeightNeeded := HeaderHeight + FootHeight +
              TCellList(Rows.Items[HeaderRowCount]).RowHeight;
            if (Y - YOffset > ARect.Top) and (Y + HeightNeeded > ParentSectionList.PageBottom) and
              (HeightNeeded < ARect.Bottom - ARect.Top) then
            begin {not enough room, start table on next page}
              if Y + Spacing < ParentSectionList.PageBottom then
              begin
                ParentSectionList.PageShortened := True;
                ParentSectionList.PageBottom := Y + Spacing;
              end;
              exit;
            end;
          end;
          {start table. it will not be complete and will go to next page}
          SavePageBottom := ParentSectionList.PageBottom;
          ParentSectionList.PageBottom := SavePageBottom - FootHeight - Cellspacing - BottomBorder - 5; {a little to spare}
          for I := 0 to Rows.Count - 1 do {do part of table}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          BodyBreak := ParentSectionList.PageBottom;
          if FootStartRow >= 0 then
          begin
            TablePartRec.TablePart := DoFoot;
            TablePartRec.PartStart := Y + FootOffset;
            TablePartRec.PartHeight := FootHeight + Max(2 * Cellspacing, Cellspacing + 1) + BottomBorder;
            ParentSectionList.TheOwner.TablePartRec := TablePartRec;
          end
          else if HeaderHeight > 0 then
          begin {will do header next}
            //ParentSectionList.PageBottom := SavePageBottom;
            TablePartRec.TablePart := DoHead;
            TablePartRec.PartStart := DrawY - TopBorder;
            TablePartRec.PartHeight := HeaderHeight + TopBorder;
            ParentSectionList.TheOwner.TablePartRec := TablePartRec;
          end;
          ParentSectionList.TheOwner.TablePartRec := TablePartRec;
        end;
      end;
  {.$EndRegion}
  {.$Region 'DoBody1'}
    DoBody1:
      begin
        if ParentSectionList.PageBottom > Y + TableHeight + BottomBorder then
        begin {can complete table now}
          for I := 0 to Rows.Count - 1 do {do remainder of table now}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          ParentSectionList.TheOwner.TablePartRec.TablePart := Normal;
        end
        else
        begin {will do part of the table now}
      {Leave room for foot later}
          ParentSectionList.PageBottom := ParentSectionList.PageBottom
            - FootHeight + Max(Cellspacing, 1) - BottomBorder;
          for I := 0 to Rows.Count - 1 do
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          BodyBreak := ParentSectionList.PageBottom;
          if FootStartRow >= 0 then
          begin
            TablePartRec.TablePart := DoFoot;
            TablePartRec.PartStart := Y + FootOffset;
            TablePartRec.PartHeight := FootHeight + Max(2 * Cellspacing, Cellspacing + 1) + BottomBorder; //FootHeight+Max(CellSpacing, 1);
            ParentSectionList.TheOwner.TablePartRec := TablePartRec;
          end
          else if HeaderHeight > 0 then
          begin
            TablePartRec.TablePart := DoHead;
            TablePartRec.PartStart := DrawY - TopBorder;
            TablePartRec.PartHeight := HeaderHeight + TopBorder;
            ParentSectionList.TheOwner.TablePartRec := TablePartRec;
          end;
          ParentSectionList.TheOwner.TablePartRec := TablePartRec;
        end;
      end;
  {.$EndRegion}
  {.$Region 'DoBody2'}
    DoBody2:
      begin
        if ParentSectionList.PageBottom > Y + TableHeight + BottomBorder then
        begin
          for I := 0 to Rows.Count - 1 do {do remainder of table now}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          ParentSectionList.TheOwner.TablePartRec.TablePart := Normal;
          ParentSectionList.PrintingTable := nil;
        end
        else
        begin
          SavePageBottom := ParentSectionList.PageBottom;
          for I := 0 to Rows.Count - 1 do {do part of table}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          BodyBreak := ParentSectionList.PageBottom;
          if FootStartRow >= 0 then
          begin
            TablePartRec.TablePart := DoFoot;
            TablePartRec.PartStart := Y + FootOffset;
            TablePartRec.PartHeight := FootHeight + Max(2 * Cellspacing, Cellspacing + 1) + BottomBorder; //FootHeight+Max(CellSpacing, 1);
            ParentSectionList.TheOwner.TablePartRec := TablePartRec;
          end
          else if HeaderHeight > 0 then
          begin
            ParentSectionList.PageBottom := SavePageBottom;
            TablePartRec.TablePart := DoHead;
            TablePartRec.PartStart := DrawY - TopBorder;
            TablePartRec.PartHeight := HeaderHeight + TopBorder;
            ParentSectionList.TheOwner.TablePartRec := TablePartRec;
          end;
          ParentSectionList.TheOwner.TablePartRec := TablePartRec;
        end;
      end;
  {.$EndRegion}
  {.$Region 'DoFoot'}
    DoFoot:
      begin
        YY := TablePartRec.PartStart;
        if FootStartRow >= 0 then
          for I := FootStartRow to Rows.Count - 1 do
            YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
        if HeaderHeight > 0 then
        begin
          TablePartRec.TablePart := DoHead;
          TablePartRec.PartStart := DrawY - TopBorder;
          TablePartRec.PartHeight := HeaderHeight + TopBorder;
        end
        else
        begin {No THead}
          TablePartRec.TablePart := DoBody3;
          TablePartRec.PartStart := BodyBreak - 1;
          TablePartRec.FootHeight := FootHeight + Max(Cellspacing, 1);
        end;
        ParentSectionList.TheOwner.TablePartRec := TablePartRec;
      end;
  {.$EndRegion}
  {.$Region 'DoHead'}
    DoHead:
      begin
        for I := 0 to HeaderRowCount - 1 do
          YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        TablePartRec.TablePart := DoBody1;
        TablePartRec.PartStart := BodyBreak - 1;
        TablePartRec.FootHeight := FootHeight + Max(Cellspacing, 1) + BottomBorder;
        ParentSectionList.TheOwner.TablePartRec := TablePartRec;
      end;
  {.$$EndRegion}
  end;
end;

{----------------THtmlTable.GetURL}

function THtmlTable.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj};
  var ATitle: ThtString): guResultType;
var
  TableOK: boolean;

  function GetTableURL(X: Integer; Y: Integer): guResultType;
  var
    I, J, XX: Integer;
    CellObj: TCellObj;
  begin
    for J := 0 to Rows.Count - 1 do
    begin
      XX := DrawX;
      with TCellList(Rows[J]) do
      begin
        for I := 0 to Count - 1 do
        begin
          CellObj := TCellObj(Items[I]);
          if Assigned(CellObj) then
            with CellObj do
            begin
              if (X >= XX) and (X < XX + Wd)
                and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY + Ht) then
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
    TableOK := (X >= DrawX) and (X <= TableWidth + DrawX);
    if TableOK then
      Result := GetTableURL(X, Y);
  end;
end;

{----------------THtmlTable.PtInObject}

function THtmlTable.PtInObject(X: Integer; Y: Integer; var Obj: TObject;
  var IX, IY: Integer): boolean;
var
  TableOK: boolean;

  function GetTableObj(X: Integer; Y: Integer): boolean;
  var
    I, J, XX: Integer;
  begin
    for J := 0 to Rows.Count - 1 do
    begin
      XX := DrawX;
      with TCellList(Rows[J]) do
      begin
        for I := 0 to Count - 1 do
        begin
          if Assigned(Items[I]) then
            with TCellObj(Items[I]) do
            begin
              if (X >= XX) and (X < XX + Wd)
                and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY + Ht) then
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
    TableOK := (X >= DrawX) and (X <= TableWidth + DrawX);
    if TableOK then
      Result := GetTableObj(X, Y);
  end;
end;

{----------------THtmlTable.FindCursor}

function THtmlTable.FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
  var XR: Integer; var YR: Integer; var CaretHt: Integer;
  var Intext: boolean): Integer;
var
  TableOK: boolean;

  function GetTableCursor(X: Integer; Y: Integer; var XR: Integer;
    var YR: Integer; var CaretHt: Integer; var Intext: boolean): Integer;
  var
    I, J, XX: Integer;
  begin
    for J := 0 to Rows.Count - 1 do
    begin
      XX := DrawX;
      with TCellList(Rows[J]) do
      begin
        for I := 0 to Count - 1 do
        begin
          if Assigned(Items[I]) then
            with TCellObj(Items[I]) do
            begin
              if (X >= XX) and (X < XX + Wd)
                and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY + Ht) then
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
  if (Y >= ContentTop) and (Y < ContentBot) then
  begin
    TableOK := (X >= DrawX) and (X <= TableWidth + DrawX);
    if TableOK then
      Result := GetTableCursor(X, Y, XR, YR, CaretHt, InText);
  end;
end;

{----------------THtmlTable.CursorToXY}

function THtmlTable.CursorToXY(Canvas: TCanvas; Cursor: Integer;
  var X: Integer; var Y: Integer): boolean;
{note: returned X value is not correct here but it isn't used}
var
  I, J: Integer;
begin
  Result := False;
  if (Len = 0) or (Cursor > StartCurs + Len) then
    Exit;
  for J := 0 to Rows.Count - 1 do
    with TCellList(Rows[J]) do
      for I := 0 to Count - 1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
          begin
            Result := Cell.CursorToXy(Canvas, Cursor, X, Y);
            if Result then
              Exit;
          end;
end;

{----------------THtmlTable.GetChAtPos}

function THtmlTable.GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
var
  I, J: Integer;
begin
  Result := False;
  if (Len = 0) or (Pos < StartCurs) or (Pos > StartCurs + Len) then
    Exit;

  for J := 0 to Rows.Count - 1 do
    with TCellList(Rows[J]) do
      for I := 0 to Count - 1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
          begin
            Result := Cell.GetChAtPos(Pos, Ch, Obj);
            if Result then
              Exit;
          end;
end;

{----------------THtmlTable.FindString}

function THtmlTable.FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;

var
  I, J: Integer;
begin
  Result := -1;
  for J := 0 to Rows.Count - 1 do
    with TCellList(Rows[J]) do
      for I := 0 to Count - 1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
          begin
            Result := Cell.FindString(From, ToFind, MatchCase);
            if Result >= 0 then
              Exit;
          end;
end;

{----------------THtmlTable.FindStringR}

function THtmlTable.FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;

var
  I, J: Integer;
begin
  Result := -1;
  for J := Rows.Count - 1 downto 0 do
    with TCellList(Rows[J]) do
      for I := Count - 1 downto 0 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
          begin
            Result := Cell.FindStringR(From, ToFind, MatchCase);
            if Result >= 0 then
              Exit;
          end;
end;

{----------------THtmlTable.FindSourcePos}

function THtmlTable.FindSourcePos(DocPos: Integer): Integer;
var
  I, J: Integer;
begin
  Result := -1;
  for J := 0 to Rows.Count - 1 do
    with TCellList(Rows[J]) do
      for I := 0 to Count - 1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
          begin
            Result := Cell.FindSourcePos(DocPos);
            if Result >= 0 then
              Exit;
          end;
end;

{----------------THtmlTable.FindDocPos}

function THtmlTable.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
var
  I, J: Integer;
  TC: TCellObj;
begin
  Result := -1;
  if not Prev then
  begin
    for J := 0 to Rows.Count - 1 do
      if Assigned(Rows.Items[J]) then
        with TCellList(Rows[J]) do
          for I := 0 to Count - 1 do
          begin
            TC := TCellObj(Items[I]);
            if Assigned(TC) then
            begin
              Result := TC.Cell.FindDocPos(SourcePos, Prev);
              if Result >= 0 then
                Exit;
            end;
          end;
  end
  else {Prev , iterate in reverse}
  begin
    for J := Rows.Count - 1 downto 0 do
      with TCellList(Rows[J]) do
        for I := Count - 1 downto 0 do
          if Assigned(Items[I]) then
          begin
            TC := TCellObj(Items[I]);
            if Assigned(TC) then
            begin
              Result := TC.Cell.FindDocPos(SourcePos, Prev);
              if Result >= 0 then
                Exit;
            end;
          end;
  end;
end;

{----------------THtmlTable.CopyToClipboard}

procedure THtmlTable.CopyToClipboard;
var
  I, J: Integer;
begin
  for J := 0 to Rows.Count - 1 do
    with TCellList(Rows[J]) do
      for I := 0 to Count - 1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            Cell.CopyToClipboard;
end;

{----------------TSection.Create}

constructor TSection.Create(AMasterList: TSectionList; L: TAttributeList;
  Prop: TProperties; AnURL: TUrlTarget; ACell: TCellBasic; FirstItem: boolean);
var
  FO: TFontObj;
  T: TAttribute;
  S: ThtString;
  Clr: ClearAttrType;
  Percent: boolean;
begin
  inherited Create(AMasterList, Prop);
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
{$IFNDEF NoTabLink}
    if not AMasterList.StopTab then
      FO.CreateTabControl(AnUrl.TabIndex);
{$ENDIF}
  end;

  Fonts.Add(FO);

  LineHeight := Prop.GetLineHeight(Abs(FO.TheFont.Height));
  if FirstItem then
  begin
    FirstLineIndent := Prop.GetTextIndent(Percent);
    if Percent then
      FLPercent := Min(FirstLineIndent, 90);
  end;

  Images := TImageObjList.Create;
  FormControls := TFormControlList.Create;

  if Assigned(L) then
  begin
    if L.Find(ClearSy, T) then
    begin
      S := LowerCase(T.Name);
      if (S = 'left') then
        ClearAttr := clLeft
      else if (S = 'right') then
        ClearAttr := clRight
      else
        ClearAttr := clAll;
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
  else
    Justify := Left;
  BreakWord := Prop.Props[WordWrap] = 'break-word';
end;

{----------------TSection.CreateCopy}

constructor TSection.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
var
  TT: TSection;
  I: Integer;
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
  for I := 0 to TT.FormControls.Count - 1 do
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
var
  i: Integer;
begin
  if XP <> nil then
    Freemem(XP);

  { Yunqa.de: Do not leave references to deleted font objects in the
    HtmlViewer's link list. Otherwise TURLTarget.SetLast might see an access
    violation. }
  if ParentSectionList <> nil then
    if ParentSectionList.LinkList <> nil then
      for i := 0 to Fonts.Count - 1 do
        ParentSectionList.LinkList.Remove(Fonts[i]);

  Fonts.Free;
  Images.Free;
  FormControls.Free;
  SIndexList.Free;
  Lines.Free;
  inherited Destroy;
end;

procedure TSection.CheckFree;
var
  I, J: Integer;
begin
  if not Assigned(Self) then
    Exit;
  if Assigned(ParentSectionList) then
  begin
  {Check to see that there isn't a TFontObj in LinkList}
    if Assigned(ParentSectionList.LinkList) then
      for I := 0 to Fonts.Count - 1 do
      begin
        J := ParentSectionList.LinkList.IndexOf(Fonts[I]);
        if J >= 0 then
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

procedure TSection.AddChar(C: WideChar; Index: Integer);
var
  Tok: TokenObj;
begin
  Tok := TokenObj.Create;
  Tok.AddUnicodeChar(C, Index);
  AddTokenObj(Tok);
  Tok.Free;
end;

function TSection.GetIndexObj(I: Integer): IndexObj;
begin
  Result := SIndexList[I];
end;

procedure TSection.AddOpBrk;
begin
  if Brk <> '' then
    Brk[Length(Brk)] := 'a';
end;

{----------------TSection.AddTokenObj}

procedure TSection.AddTokenObj(T: TokenObj);
var
  L, I: Integer;
  C: char;
  St, StU: WideString;
  Small: boolean;
begin
  if T.Leng = 0 then
    Exit;
  { Yunqa.de: Simple hack to support <span style="display:none"> }
  if PropStack.Last.Display = pdNone then Exit;

  L := Len + T.Leng;
  if BuffSize < L + 3 then
    Allocate(L + 500); {L+3 to permit additions later}
  case PropStack.Last.GetTextTransform of
    txUpper:
      St := WideUpperCase1(T.S);
    txLower:
      St := WideLowerCase1(T.S);
  else
    St := T.S;
  end;
  Move(T.I[1], XP^[Len], T.Leng * Sizeof(Integer));
  if NoBreak or (Self is TPreformated) then
    C := 'n'
  else
    C := 'y';
  for I := 1 to T.Leng do
    Brk := Brk + C;

  if PropStack.Last.GetFontVariant = 'small-caps' then
  begin
    StU := WideUpperCase1(St);
    BuffS := BuffS + StU;
    Small := False;
    for I := 1 to Length(St) do
    begin
      if not (St[I] in [WideChar(' '), WideChar('0')..WideChar('9')]) then {no font changes for these chars}
      begin
        if not Small then
        begin
          if StU[I] <> St[I] then
          begin {St[I] was lower case}
            PropStack.PushNewProp('small', '', '', '', '', nil); {change to smaller font}
            ChangeFont(PropStack.Last);
            Small := True;
          end;
        end
        else if StU[I] = St[I] then
        begin {St[I] was uppercase and Small is set}
          PropStack.PopAProp('small');
          ChangeFont(PropStack.Last);
          Small := False;
        end;
      end;
      Inc(Len);
    end;
    if Small then {change back to regular font}
    begin
      PropStack.PopAProp('small');
      ChangeFont(PropStack.Last);
    end;
  end
  else
  begin
    BuffS := BuffS + St;
    Len := L;
  end;
  Buff := PWideChar(BuffS);
end;

{----------------TSection.ProcessText}

procedure TSection.ProcessText(TagIndex: Integer);
const
  Shy = #173; {soft hyphen}
var
  I: Integer;
  FO: TFontObj;

  procedure Remove(I: Integer);
  begin
    Move(XP^[I], XP^[I - 1], ((Length(BuffS)) - I) * Sizeof(Integer));
    System.Delete(BuffS, I, 1);
    System.Delete(Brk, I, 1);
    TFormControlList(FormControls).Decrement(I - 1);
    TFontList(Fonts).Decrement(I - 1, ParentSectionList);
    TImageObjList(Images).Decrement(I - 1);
  end;

begin
  while (Length(BuffS) > 0) and (BuffS[1] = ' ') do
    Remove(1);

  I := WidePos(Shy, BuffS);
  while I > 0 do
  begin
    Remove(I);
    if (I > 1) and (Brk[I - 1] <> 'n') then
      Brk[I - 1] := 's';
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
  for I := Length(BuffS) - 1 downto 1 do
    if (BuffS[I] = ImgPan) and (Images.FindImage(I - 1).HorzAlign in [ALeft, ARight])
      and (BuffS[I + 1] = ' ') then
      Remove(I + 1);

  I := WidePos(WideString(' ' + #8), BuffS); {#8 is break ThtChar}
  while I > 0 do
  begin
    Remove(I);
    I := WidePos(WideString(' ' + #8), BuffS);
  end;

  I := WidePos(WideString(#8 + ' '), BuffS);
  while I > 0 do
  begin
    Remove(I + 1);
    I := WidePos(WideString(#8 + ' '), BuffS);
  end;

  if (Length(BuffS) > 1) and (BuffS[Length(BuffS)] = #8) then
    Remove(Length(BuffS));

  if (Length(BuffS) > 1) and (BuffS[Length(BuffS)] = ' ') then
    Remove(Length(BuffS));

  if (BuffS <> #8) and (Length(BuffS) > 0) and (BuffS[Length(BuffS)] <> ' ') then
  begin
    FO := TFontObj(Fonts.Items[Fonts.Count - 1]); {keep font the same for inserted space}
    if FO.Pos = Length(BuffS) then
      Inc(FO.Pos);
    BuffS := BuffS + ' ';
    XP^[Length(BuffS) - 1] := TagIndex;
  end;

  Finish;
end;

{----------------TSection.Finish}

procedure TSection.Finish;
{complete some things after all information added}
var
  Last, I: Integer;
  IO: IndexObj;
begin
  Buff := PWideChar(BuffS);
  Len := Length(BuffS);
  if Len > 0 then
  begin
    Brk := Brk + 'y';
    if Assigned(XP) then {XP = Nil when printing}
    begin
      Last := 0; {to prevent warning msg}
      SIndexList := TFreeList.Create;
      for I := 0 to Len - 1 do
      begin
        if (I = 0) or (XP^[I] <> Last + 1) then
        begin
          IO := IndexObj.Create;
          IO.Pos := I;
          IO.Index := XP^[I];
          SIndexList.Add(IO);
        end;
        Last := XP^[I];
      end;
      FreeMem(XP);
      XP := nil;
    end;
  end;
  if Len > 0 then
  begin
    Inc(ParentSectionList.SectionCount);
    SectionNumber := ParentSectionList.SectionCount;
  end;
end;

{----------------TSection.Allocate}

procedure TSection.Allocate(N: Integer);
begin
  if BuffSize < N then
  begin
    ReAllocMem(XP, N * Sizeof(Integer));
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
  FO := TFontObj(Fonts[Fonts.Count - 1]);
  LastUrl := FO.UrlTarget;
  NewFont := Prop.GetFont;
  if FO.Pos = Len then
    FO.ReplaceFont(NewFont) {fontobj already at this position, modify it}
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
  else
    FO.SScript := ANone;
end;

{----------------------TSection.HRef}

procedure TSection.HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
  Attributes: TAttributeList; Prop: TProperties);
var
  FO: TFontObj;
  NewFont: TMyFont;
  Align: AlignmentType;
begin
  FO := TFontObj(Fonts[Fonts.Count - 1]);
  NewFont := Prop.GetFont;
  if FO.Pos = Len then
    FO.ReplaceFont(NewFont) {fontobj already at this position, modify it}
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
{$IFNDEF NoTabLink}
    if not ParentSectionList.StopTab then
      FO.CreateTabControl(AnUrl.TabIndex);
{$ENDIF}
  end
  else if Assigned(FO.FIArray) then
  begin
    FO.FIArray.Free;
    FO.FIArray := nil;
  end;
  FO.UrlTarget.Copy(AnUrl);
  if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then
    FO.SScript := Align
  else
    FO.SScript := ANone;
end;

function TSection.AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer): TImageObj;
begin
  Result := TImageObj.Create(ParentSectionList, Len, L);
  Result.MyCell := ACell;
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for image}
end;

function TSection.AddPanel(L: TAttributeList;
  ACell: TCellBasic; Index: Integer): TPanelObj;
begin
  Result := TPanelObj.Create(ParentSectionList, Len, L, ACell, False);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for panel}
end;

function TSection.CreatePanel(L: TAttributeList;
  ACell: TCellBasic): TPanelObj;
{Used by object tag}
begin
  Result := TPanelObj.Create(ParentSectionList, Len, L, ACell, True);
end;

procedure TSection.AddPanel1(PO: TPanelObj; Index: Integer);
{Used for Object Tag}
begin
  Images.Add(PO);
  AddChar(ImgPan, Index); {marker for panel}
end;

{----------------TSection.AddFormControl}

function TSection.AddFormControl(Which: Symb; AMasterList: TSectionList;
  L: TAttributeList; ACell: TCellBasic; Index: Integer;
  Prop: TProperties): TFormControlObj;
var
  T: TAttribute;
  FCO: TFormControlObj;
  S: ThtString;
  IO: TImageObj;
  ButtonControl: TButtonFormControlObj;

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
    IO := AddImage(L, ACell, Index); {leave out of FormControlList}
    IO.MyFormControl := TImageFormControlObj(FCO);
    TImageFormControlObj(FCO).MyImage := IO;
  end
  else if S = 'file' then
  begin
    FormControls.Add(FCO);
    AddChar(FmCtl, Index); {marker for FormControl}
    Brk[Len] := 'n'; {don't allow break between these two controls}
    ButtonControl := TButtonFormControlObj.Create(AMasterList, Len, L, S, Prop);
    ButtonControl.MyEdit := TEditFormControlObj(FCO);
    FormControls.Add(ButtonControl);
  {the following fixup puts the ID on the TEdit and deletes it from the Button}
    if L.TheID <> '' then
      ParentSectionList.IDNameList.AddObject(L.TheID, FCO);
    FCO.Value := ''; {value attribute should not show in TEdit}
    ThtEdit(FCO.TheControl).Text := '';
    AddChar(FmCtl, Index);
    Brk[Len] := 'n';
  end
  else if S <> 'hidden' then
  begin
    FormControls.Add(FCO);
    AddChar(FmCtl, Index); {marker for FormControl}
  end;
  if Prop.GetBorderStyle <> bssNone then {start of inline border}
    ParentSectionList.ProcessInlines(Index, Prop, True);
  Result := FCO;
end;

{----------------TSection.FindCountThatFits}

function TSection.FindCountThatFits(Canvas: TCanvas; Width: Integer; Start: PWideChar; Max: Integer): Integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, OHang, Tmp: Integer;
  Picture: boolean;
  Align: AlignmentType;
  HSpcL, HSpcR: Integer;
  FLObj: TFloatingObj;
  Extent: Integer;
const
  OldStart: PWideChar = nil;
  OldResult: Integer = 0;
  OldWidth: Integer = 0;

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
    Fonts.GetFontAt(Start - Buff, OHang).AssignToCanvas(Canvas);
    J1 := Fonts.GetFontCountAt(Start - Buff, Len);
    J2 := Images.GetImageCountAt(Start - Buff);
    J3 := TFormControlList(FormControls).GetControlCountAt(Start - Buff);
    if J2 = 0 then
    begin
      Tmp := Images.GetWidthAt(Start - Buff, Align, HSpcL, HSpcR, FlObj);
      if not (Align in [ALeft, ARight]) then
        XX := XX + Tmp + HSpcL + HSpcR;
      I := 1; J := 1;
      Picture := True;
      if XX > Width then
        break;
    end
    else if J3 = 0 then
    begin
      XX := XX + TFormControlList(FormControls).GetWidthAt(Start - Buff, HSpcL, HSpcR);
      XX := XX + HSpcL + HSpcR;
      I := 1; J := 1;
      Picture := True;
      if XX > Width then
        break;
    end
    else
    begin
      Picture := False;
      J := Min(J1, J2);
      J := Min(J, J3);
      I := FitText(Canvas.Handle, Start, J, Width - XX, Extent);
    end;
    if Cnt + I >= Max then {I has been initialized}
    begin
      Cnt := Max;
      Break;
    end
    else
      Inc(Cnt, I);

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

function TSection.FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; Max: Integer; X, Y: Integer; IMgr: TIndentManager;
  var ImgHt: Integer; NxImages: TList): Integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, X1, X2,
    OHang, ImgWidth, Width: Integer;
  Picture: boolean;
  Align: AlignmentType;
  ImageAtStart: boolean;
  FlObj: TFloatingObj;
  HSpcL, HSpcR: Integer;
  BrChr, TheStart: PWideChar;
  Font, LastFont: TMyFont;
  SaveX: Integer;
  FoundBreak: boolean;
  HyphenWidth: Integer;

begin
  LastFont := nil;
  TheStart := Start;
  ImageAtStart := True;
  ImgHt := 0;

  BrChr := StrScanW(TheStart, BrkCh); {see if a break ThtChar}
  if Assigned(BrChr) and (BrChr - TheStart < Max) then
  begin
    Max := BrChr - TheStart;
    if Max = 0 then
    begin
      Result := 1;
      Exit; {single character fits}
    end;
    FoundBreak := True;
  end
  else
    FoundBreak := False;

  Cnt := 0;
  X1 := IMgr.LeftIndent(Y);
  if Start = Buff then
    Inc(X1, FirstLineIndent);
  X2 := IMgr.RightSide(Y);
  Width := X2 - X1;

  if (Start = Buff) and (Images.Count = 0) and (FormControls.Count = 0) then
    if Fonts.GetFontCountAt(0, Len) = Len then
      if Max * TFontObj(Fonts[0]).tmMaxCharWidth <= Width then {try a shortcut}
      begin {it will all fit}
        Result := Max;
        if FoundBreak then
          Inc(Result);
        Exit;
      end;

  XX := 0;
  while True do
  begin
    Font := Fonts.GetFontAt(Start - Buff, OHang);
    if Font <> LastFont then {may not have to load font}
    begin
      Font.AssignToCanvas(Canvas);
      LastFont := Font;
    end;
    J1 := Min(Fonts.GetFontCountAt(Start - Buff, Len), Max - Cnt);
    J2 := Images.GetImageCountAt(Start - Buff);
    J3 := TFormControlList(FormControls).GetControlCountAt(Start - Buff);
    if J2 = 0 then
    begin {next is an image}
      I := 1; J := 1;
      Picture := True;
      ImgWidth := Images.GetWidthAt(Start - Buff, Align, HSpcL, HSpcR, FlObj);
      if Align in [ALeft, ARight] then
      begin
        FlObj.DrawYY := Y; {approx y position}
        if ImageAtStart then
        begin
          Inc(XX, ImgWidth + FlObj.HSpaceL + FlObj.HSpaceR);
          if XX <= Width then {it fits}
          begin
            IMgr.Update(Y, FlObj);
            ImgHt := Math.Max(ImgHt, FlObj.ImageHeight + FlObj.VSpaceT + FlObj.VSpaceB);
          end
          else if Cnt > 0 then
            Break {One or more do fit, this one doesn't}
          else
          begin {first image doesn't fit}
//BG, 24.01.2010: only moving down looks very strange. 
//            if IMgr.GetNextWiderY(Y) > Y then
//              Break; {wider area below, it might fit there}
          {Can't move it down, might as well put it here}
            IMgr.Update(Y, FlObj);
            ImgHt := Math.Max(ImgHt, FlObj.ImageHeight + FlObj.VSpaceT + FlObj.VSpaceB);
            Cnt := 1;
            Break;
          end;
        end
        else
          NxImages.Add(FlObj); {save it for the next line}
      end
      else
      begin
        Inc(XX, ImgWidth + HSpcL + HSpcR);
        ImageAtStart := False;
      end;
      if XX > Width then
        break;
    end
    else if J3 = 0 then
    begin
      XX := XX + TFormControlList(FormControls).GetWidthAt(Start - Buff, HSpcL, HSpcR);
      XX := XX + HSpcL + HSpcR;
      I := 1; J := 1;
      Picture := True;
      ImageAtStart := False;
      if XX > Width then
        break;
    end
    else
    begin
      Picture := False;
      J := Min(J1, J2);
      J := Min(J, J3);
      I := FitText(Canvas.Handle, Start, J, Width - XX, SaveX);
      if (I > 0) and (Brk[TheStart - Buff + Cnt + I] = 's') then
      begin {a hyphen could go here}
        HyphenWidth := Canvas.TextWidth('-');
        if XX + SaveX + HyphenWidth > Width then
          Dec(I);
      end;
    end;
    if Cnt + I >= Max then
    begin
      Cnt := Max;
      Break;
    end
    else
      Inc(Cnt, I);

    if not Picture then {it's a text block}
    begin
      if I < J then
        Break;
      XX := XX + SaveX;
      ImageAtStart := False;
    end;

    Inc(Start, I);
  end;
  Result := Cnt;
  if FoundBreak and (Cnt = Max) then
    Inc(Result);
end;

function WrapChar(C: WideChar): Boolean;
begin
  Result := Ord(C) >= $3000;
end;

//BG, 20.09.2010:
function CanWrap(C: WideChar): Boolean;
begin
  if C in [WideChar(' '), WideChar('-'), WideChar('?'), ImgPan, FmCtl, BrkCh] then
    Result := True
  else
    Result := WrapChar(C);
end;

{----------------TSection.MinMaxWidth}

procedure TSection.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
{Min is the width the section would occupy when wrapped as tightly as possible.
 Max, the width if no wrapping were used.}
var
  I, Indx, FloatMin: Integer;
  P, P1: PWideChar;
  Obj: TObject;
  SoftHyphen: Boolean;

  function FindTextWidthB(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: boolean): Integer;
  begin
    Result := FindTextWidth(Canvas, Start, N, RemoveSpaces);
    if Start = Buff then
      if FLPercent = 0 then {not a percent}
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
  if Len = 0 then
    Exit;

  for I := 0 to Images.Count - 1 do {call drawlogic for all the images}
  begin
    Obj := Images[I];
    with TFloatingObj(Obj) do
    begin
      DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Pos, Indx), 0, 0);
      if not PercentWidth then
        if HorzAlign in [ALeft, ARight] then
        begin
          Max := Max + ImageWidth + HSpaceL + HSpaceR;
          Brk[Pos + 1] := 'y'; {allow break after floating image}
        end
        else
          Min := Math.Max(Min, ImageWidth);
    end;
  end;
  FloatMin := Max;

  for I := 0 to FormControls.Count - 1 do {get Min for form controls}
  begin
    Obj := FormControls[I];
    if Obj is TFormControlObj then
      with TFormControlObj(FormControls[I]) do
        if not PercentWidth then
          Min := Math.Max(Min, FControl.Width + HSpaceL + HSpaceR);
  end;

  Max := 0;
  P := Buff;
  P1 := StrScanW(P, BrkCh); {look for break ThtChar}
  while Assigned(P1) do
  begin
    Max := Math.Max(Max, FindTextWidthB(Canvas, P, P1 - P, False));
    P := P1 + 1;
    P1 := StrScanW(P, BrkCh);
  end;
  P1 := StrScanW(P, #0); {look for the end}
  Max := Math.Max(Max, FindTextWidthB(Canvas, P, P1 - P, True)) + FloatMin;

  P := Buff;
  if not BreakWord then
  begin
    while P^ = ' ' do
      Inc(P);
    P1 := P;
    I := P1 - Buff + 1;
    while P^ <> #0 do
    {find the next ThtString of chars that can't be wrapped}
    begin
      if CanWrap(P1^) and (Brk[I] = 'y') then
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
        until (P1^ = #0) or (CanWrap(P1^) and (Brk[I] = 'y')) or (Brk[I - 1] in ['a', 's']);
        SoftHyphen := Brk[I - 1] = 's';
        if P1^ in [WideChar('-'), WideChar('?')] then
        begin
          Inc(P1);
          Inc(I);
        end;
      end;
      Min := Math.Max(Min, FindTextWidthB(Canvas, P, P1 - P, True));
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
      Min := Math.Max(Min, FindTextWidthB(Canvas, P, 1, True));
      Inc(P);
    end;

  Min := Math.Max(FloatMin, Min);
  StoredMin := Min;
  StoredMax := Max;
end;

{----------------TSection.FindTextWidth}

function TSection.FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: boolean): Integer;
{find actual line width of N chars starting at Start.  If RemoveSpaces set,
 don't count spaces on right end}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: Integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
begin
  Result := 0;
  if RemoveSpaces then
    while ((Start + N - 1)^ in [WideChar(' '), BrkCh]) do
      Dec(N); {remove spaces on end}
  while N > 0 do
  begin
    J := Images.GetImageCountAt(Start - Buff);
    J1 := TFormControlList(FormControls).GetControlCountAt(Start - Buff);
    if J = 0 then {it's and image}
    begin
      Wid := Images.GetWidthAt(Start - Buff, Align, HSpcL, HSpcR, FlObj);
    {Here we count floating images as 1 ThtChar but do not include their width,
      This is required for the call in FindCursor}
      if not (Align in [ALeft, ARight]) then
        Inc(Result, Wid + HSpcL + HSpcR);
      Dec(N); {image counts as one ThtChar}
      Inc(Start);
    end
    else if J1 = 0 then
    begin
      Inc(Result, TFormControlList(FormControls).GetWidthAt(Start - Buff, HSpcL, HSpcR) + HSpcL + HSpcR);
      Dec(N); {control counts as one ThtChar}
      Inc(Start);
    end
    else
    begin
      Fonts.GetFontAt(Start - Buff, OHang).AssignToCanvas(Canvas);
      I := Min(J, J1);
      I := Min(I, Min(Fonts.GetFontCountAt(Start - Buff, Len), N));
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

function TSection.FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
{find actual line width of N chars starting at Start.
 BG: The only difference to FindTextWidth is the '- OHang' when incrementing the result.}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: Integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
  Font: TMyFont;
begin
  Result := 0;
  while N > 0 do
  begin
    J := Images.GetImageCountAt(Start - Buff);
    J1 := TFormControlList(FormControls).GetControlCountAt(Start - Buff);
    if J = 0 then {it's an image}
    begin
      Wid := Images.GetWidthAt(Start - Buff, Align, HSpcL, HSpcR, FlObj);
    {Here we count floating images as 1 ThtChar but do not include their width,
      This is required for the call in FindCursor}
      if not (Align in [ALeft, ARight]) then
      begin
        Result := Result + Wid + HSpcL + HSpcR;
      end;
      Dec(N); {image counts as one ThtChar}
      Inc(Start);
    end
    else if J1 = 0 then
    begin
      Result := Result + TFormControlList(FormControls).GetWidthAt(Start - Buff, HSpcL, HSpcR);
      Result := Result + HSpcL + HSpcR;
      Dec(N); {control counts as one ThtChar}
      Inc(Start);
    end
    else
    begin
      Font := Fonts.GetFontAt(Start - Buff, OHang);
      Font.AssignToCanvas(Canvas);
      I := Min(J, J1);
      I := Min(I, Min(Fonts.GetFontCountAt(Start - Buff, Len), N));
      Inc(Result, GetXExtent(Canvas.Handle, Start, I) - OHang);
      if I = 0 then
        Break;
      Dec(N, I);
      Inc(Start, I);
    end;
  end;
end;

{----------------TSection.DrawLogic}

function TSection.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
{returns height of the section}
var
  PStart, P, Last: PWideChar;
  Max, N, NN, Width, I, Indx, ImgHt: Integer;
  Finished: boolean;
  LR: LineRec;
  NxImages: TList;
  Tmp, Tmp1: Integer;
  Obj: TFloatingObj;
  TopY, AccumImgBot, HtRef: Integer;

  function GetClearSpace(ClearAttr: ClearAttrType): Integer;
  var
    CL, CR: Integer;
  begin
    Result := 0;
    if (ClearAttr <> clrNone) then
    begin {may need to move down past floating image}
      IMgr.GetClearY(CL, CR);
      case ClearAttr of
        clLeft: Result := Math.Max(0, CL - Y - 1);
        clRight: Result := Math.Max(0, CR - Y - 1);
        clAll: Result := Math.Max(CL - Y - 1, Math.Max(0, CR - Y - 1));
      end;
    end;
  end;

  procedure LineComplete(NN: Integer);
  var
    I, J, DHt, Desc, Tmp, TmpRt, Cnt, Index, H, SB, SA: Integer;
    FO: TFontObj;
    Align: AlignmentType;
    FormAlign: AlignmentType;
    NoChar: boolean;
    P: PWideChar;
    FCO: TFormControlObj;
    FlObj: TFloatingObj;
    LRTextWidth: Integer;
    OHang: Integer;

    function FindSpaces: Integer;
    var
      I: Integer;
    begin
      Result := 0;
      for I := 0 to NN - 2 do {-2 so as not to count end spaces}
        if ((PStart + I)^ = ' ') or ((PStart + I)^ = #160) then
          Inc(Result);
    end;

  begin
    DHt := 0; {for the fonts on this line get the maximum height}
    Cnt := 0;
    Desc := 0;
    P := PStart;
    if (NN = 1) and (P^ = BrkCh) then
      NoChar := False
    else
    begin
      NoChar := True;
      for I := 0 to NN - 1 do
      begin
        if (not (P^ in [FmCtl, ImgPan, BrkCh])) {ignore images and space on end}
          and (not ((P = Last) and (Last^ = ' '))) then
        begin {check for the no character case}
          NoChar := False;
          Break;
        end;
        Inc(P);
      end;
    end;

    if not NoChar then
      repeat
        FO := Fonts.GetFontObjAt(PStart - Buff + Cnt, Index);
        Tmp := FO.GetHeight(Desc);
        DHt := Math.Max(DHt, Tmp);
        LR.Descent := Math.Max(LR.Descent, Desc);
        J := Fonts.GetFontCountAt(PStart - Buff + Cnt, Len);
        Inc(Cnt, J);
      until Cnt >= NN;

    SB := 0; {if there are images, then maybe they add extra space}
    SA := 0; {space before and after}
    if LineHeight >= 0 then
    begin
      SB := (LineHeight - DHt) div 2;
      SA := (LineHeight - DHt) - SB;
    end;
    Cnt := 0;
    repeat
      Cnt := Cnt + Images.GetImageCountAt(PStart - Buff + Cnt);
      if Cnt < NN then
      begin
        H := Images.GetHeightAt(PStart - Buff + Cnt, Align, FlObj);
        FlObj.DrawYY := Y; {approx y dimension}
        if (FLObj is TImageObj) and Assigned(TImageObj(FLObj).MyFormControl) then
          TImageObj(FLObj).MyFormControl.FYValue := Y;
        case Align of
          ATop: SA := Math.Max(SA, H - DHt);
          AMiddle:
            begin
              if DHt = 0 then
              begin
                DHt := Fonts.GetFontObjAt(PStart - Buff, Index).GetHeight(Desc);
                LR.Descent := Desc;
              end;
              Tmp := (H - DHt) div 2;
              SA := Math.Max(SA, Tmp);
              SB := Math.Max(SB, (H - DHt - Tmp));
            end;
          ABottom, ABaseline: SB := Math.Max(SB, H - (DHt - LR.Descent));
        end;
      end;
      Inc(Cnt); {to skip by the image}
    until Cnt >= NN;

    Cnt := 0; {now check on form controls}
    repeat
      Cnt := Cnt + TFormControlList(FormControls).GetControlCountAt(PStart - Buff + Cnt);
      if Cnt < NN then
      begin
        FCO := TFormControlList(FormControls).FindControl(PStart - Buff + Cnt);
        H := TFormControlList(FormControls).GetHeightAt(PStart - Buff + Cnt, FormAlign);
        case FormAlign of
          ATop:
            SA := Math.Max(SA, H + FCO.VSpaceB + FCO.VSpaceT - Dht);
          AMiddle:
            begin
              Tmp := (H - DHt) div 2;
              SA := Math.Max(SA, Tmp + FCO.VSpaceB);
              SB := Math.Max(SB, (H - DHt - Tmp + FCO.VSpaceT));
            end;
          ABaseline:
            SB := Math.Max(SB, H + FCO.VSpaceT + FCO.VSpaceB - (DHt - LR.Descent));
          ABottom:
            SB := Math.Max(SB, H + FCO.VSpaceT + FCO.VSpaceB - DHt);
        end;
        if Assigned(FCO) and not ParentSectionList.IsCopy then
          FCO.FYValue := Y;
      end;
      Inc(Cnt); {to skip by the control}
    until Cnt >= NN;

{$IFNDEF NoTabLink}
    if not ParentSectionList.IsCopy then
    begin
      Cnt := 0; {now check URLs}
      repeat
        FO := Fonts.GetFontObjAt(PStart - Buff + Cnt, Index);
        FO.AssignY(Y);
        Cnt := Cnt + Fonts.GetFontCountAt(PStart - Buff + Cnt, Len);
      until Cnt >= NN;
    end;
{$ENDIF}

    LR.Start := PStart;
    LR.LineHt := DHt;
    LR.Ln := NN;
    if Brk[PStart - Buff + NN] = 's' then {see if there is a soft hyphen on the end}
      LR.Shy := True;
    TmpRt := IMgr.RightSide(Y);
    Tmp := IMgr.LeftIndent(Y);
    if PStart = Buff then
      Tmp := Tmp + FirstLineIndent;

    LRTextWidth := FindTextWidth(Canvas, PStart, NN, True);
    if LR.Shy then
    begin {take into account the width of the hyphen}
      Fonts.GetFontAt(PStart - Buff + NN - 1, OHang).AssignToCanvas(Canvas);
      Inc(LRTextWidth, Canvas.TextWidth('-'));
    end;
    TextWidth := Math.Max(TextWidth, LRTextWidth);
    case Justify of
      Left:     LR.LineIndent := Tmp - X;
      Centered: LR.LineIndent := (TmpRt + Tmp - LRTextWidth) div 2 - X;
      Right:    LR.LineIndent := TmpRt - X - LRTextWidth;
    else
      {Justify = FullJustify}
      LR.LineIndent := Tmp - X;
      if not Finished then
      begin
        LR.Extra := TmpRt - Tmp - LRTextWidth;
        LR.Spaces := FindSpaces;
      end;
    end;
    LR.DrawWidth := TmpRt - Tmp;
    LR.SpaceBefore := LR.SpaceBefore + SB;
    LR.SpaceAfter := SA;
    Lines.Add(LR);
    Inc(PStart, NN);
    SectionHeight := SectionHeight + DHt + SA + LR.SpaceBefore;
    Tmp := DHt + SA + SB;
    Inc(Y, Tmp);
    LR.LineImgHt := Math.Max(Tmp, ImgHt);
    for I := 0 to NxImages.Count - 1 do
    begin
      IMgr.Update(Y, TFloatingObj(NxImages[I])); {update Image manager and Image}
    {include images in Line height}
      with TFloatingObj(NxImages[I]) do
        Tmp1 := ImageHeight + VSpaceT + VSpaceB;
      LR.LineImgHt := Math.Max(LR.LineImgHt, Tmp + Tmp1);
      AccumImgBot := Math.Max(AccumImgBot, Y + Tmp1);
    end;
    NxImages.Clear;
  end;

begin {TSection.DrawLogic}
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
  TextWidth := 0;
  if Len = 0 then
  begin
    Result := GetClearSpace(ClearAttr);
    DrawHeight := Result;
    SectionHeight := Result;
    ContentBot := Y + Result;
    DrawBot := ContentBot;
    MaxWidth := 0;
    DrawWidth := 0;
    Exit;
  end;
  if FLPercent <> 0 then
    FirstLineIndent := (FLPercent * AWidth) div 100; {percentage calculated}
  Finished := False;
  DrawWidth := IMgr.RightSide(Y) - X;
  Width := Min(IMgr.RightSide(Y) - IMgr.LeftIndent(Y), AWidth);
  MaxWidth := Width;
  if AHeight = 0 then
    HtRef := BlHt
  else
    HtRef := AHeight;
  for I := 0 to Images.Count - 1 do {call drawlogic for all the images}
  begin
    Obj := TFloatingObj(Images[I]);
    Obj.DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Obj.Pos, Indx), Width, HtRef);
    MaxWidth := Math.Max(MaxWidth, Obj.ImageWidth); {HScrollBar for wide images}
  end;
  for I := 0 to FormControls.Count - 1 do
    with TFormControlObj(FormControls[I]) do
      if Assigned(FControl) then
      begin
        if PercentWidth then
          FControl.Width := Math.Max(10, Min(MulDiv(FWidth, Width, 100), Width - HSpaceL - HSpaceR));
        MaxWidth := Math.Max(MaxWidth, FControl.Width);
      end;
  NxImages := TList.Create;
  while not Finished do
  begin
    Max := Last - PStart + 1;
    if Max <= 0 then
      Break;
    LR := LineRec.Create(ParentSectionList); {a new line}
    if Lines.Count = 0 then
    begin {may need to move down past floating image}
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
      N := Math.Max(NN, 1); {N = at least 1}
    end;

    AccumImgBot := Math.Max(AccumImgBot, Y + ImgHt);
    if NN = 0 then {if nothing fits, see if we can move down}
      Tmp := IMgr.GetNextWiderY(Y) - Y
    else
      Tmp := 0;
    if Tmp > 0 then
    begin
      //BG, 24.01.2010: do not move down images or trailing spaces.
      P := PStart + N - 1; {the last ThtChar that fits}
      if ((P^ in [WideChar(' '), {FmCtl,} ImgPan]) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n')
      or (P^ = BrkCh) then
      begin {move past spaces so as not to print any on next line}
        while (N < Max) and ((P + 1)^ = ' ') do
        begin
          Inc(P);
          Inc(N);
        end;
        Finished := N >= Max;
        LineComplete(N);
      end
      else
      begin {move down where it's wider}
        LR.LineHt := Tmp;
        Inc(SectionHeight, Tmp);
        LR.Ln := 0;
        LR.Start := PStart;
        Inc(Y, Tmp);
        Lines.Add(LR);
      end
    end {else can't move down or don't have to}
    else if N = Max then
    begin {Do the remainder}
      Finished := True;
      LineComplete(N);
    end
    else
    begin
      P := PStart + N - 1; {the last ThtChar that fits}
      if ((P^ in [WideChar(' '), FmCtl, ImgPan]) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n')
      or (P^ = BrkCh) then
      begin {move past spaces so as not to print any on next line}
        while (N < Max) and ((P + 1)^ = ' ') do
        begin
          Inc(P);
          Inc(N);
        end;
        Finished := N >= Max;
        LineComplete(N);
      end
      else if (N < Max) and ((P + 1)^ = ' ') and (Brk[P - Buff + 2] <> 'n') then
      begin
        repeat
          Inc(N); {pass the space}
          Inc(p);
        until (N >= Max) or ((P + 1)^ <> ' ');
        Finished := N >= Max;
        LineComplete(N);
      end
      else if (N < Max) and ((P + 1)^ in [FmCtl, ImgPan]) and (Brk[PStart - Buff + N] <> 'n') then {an image or control}
      begin
        Finished := False;
        LineComplete(N);
      end
      else
      begin {non space, wrap it by backing off to previous space or image}
        while ((not ((P^ in [WideChar(' '), WideChar('-'), WideChar('?'), FmCtl, ImgPan])
          or WrapChar(P^) or WrapChar((P + 1)^)) and not (Brk[P - Buff + 1] in ['a', 's']))
          or ((Brk[P - Buff + 1] = 'n'))) and (P > PStart) do
          Dec(P);
        if (P = PStart) and ((not (P^ in [FmCtl, ImgPan])) or (Brk[PStart - Buff + 1] = 'n')) then
        begin {no space found, forget the wrap, write the whole word and any
               spaces found after it}
          if BreakWord then
            LineComplete(N)
          else
          begin
            P := PStart + N - 1;

            while (P <> Last) and not (P^ in [WideChar('-'), WideChar('?')])
            and not (Brk[P - Buff + 1] in ['a', 's'])
              and not (((P + 1)^ in [WideChar(' '), FmCtl, ImgPan, BrkCh]) or WrapChar((P + 1)^))
            or (Brk[P - Buff + 2] = 'n') do
            begin
              Inc(P);
            end;
            while (P <> Last) and ((P + 1)^ = ' ') do
            begin
              Inc(P);
            end;
            if (P <> Last) and ((P + 1)^ = BrkCh) then
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
            begin {line is too long but do it anyway}
              MaxWidth := Math.Max(MaxWidth, FindTextWidth(Canvas, PStart, P - PStart + 1, True));
              Finished := P = Last;
              LineComplete(P - PStart + 1);
            end;
          end
        end
        else
        begin {found space}
          while (P + 1)^ = ' ' do
          begin
            if P = Last then
            begin
              Inc(P);
              Dec(P);
            end;
            Inc(P);
          end;
          LineComplete(P - PStart + 1);
        end;
      end;
    end;
  end;
  NxImages.Free;
  Curs := StartCurs + Len;

  if Assigned(ParentSectionList.FirstLineHtPtr) and (Lines.Count > 0) then {used for List items}
    with LineRec(Lines[0]) do
      if (ParentSectionList.FirstLineHtPtr^ = 0) then
        ParentSectionList.FirstLineHtPtr^ := YDraw + LineHt - Descent + SpaceBefore;

  DrawHeight := AccumImgBot - TopY; {in case image overhangs}
  if DrawHeight < SectionHeight then
    DrawHeight := SectionHeight;
  Result := SectionHeight;
  ContentBot := TopY + SectionHeight;
  DrawBot := TopY + DrawHeight;
  with ParentSectionList do
  begin
    if not IsCopy and (SectionNumber mod 50 = 0) and (ThisCycle <> CycleNumber)
      and (SectionCount > 0) then
      TheOwner.htProgress(ProgressStart + ((100 - ProgressStart) * SectionNumber) div SectionCount);
    ThisCycle := CycleNumber; {only once per cycle}
  end;
end;

{----------------TSection.CheckForInlines}

procedure TSection.CheckForInlines(LR: Linerec);
{called before each line is drawn the first time to check if there are any
 inline borders in the line}
var
  I: Integer;
  BR: BorderRec;
  StartBI, EndBI, LineStart: Integer;
begin
  with LR do
  begin
    FirstDraw := False; {this will turn it off if there is no inline border action in this line}
    with TInlineList(ParentSectionList.InlineList) do
      for I := 0 to Count - 1 do {look thru the inlinelist}
      begin
        StartBI := StartB[I];
        EndBI := EndB[I];
        LineStart := StartCurs + Start - Buff; {offset from Section start to Line start}
        if (EndBI > LineStart) and (StartBI < LineStart + Ln) then
        begin {it's in this line}
          if not Assigned(BorderList) then
          begin
            BorderList := TFreeList.Create;
            FirstDraw := True; {there will be more processing needed}
          end;
          BR := BorderRec.Create;
          BorderList.Add(BR);
          with BR do
          begin
            BR.MargArray := InlineRec(ParentSectionList.InlineList.Items[I]).MargArray; {get border data}
            if StartBI < LineStart then
            begin
              OpenStart := True; {continuation of border on line above, end is open}
              BStart := Start - Buff; {start of this line}
            end
            else
            begin
              OpenStart := False;
              BStart := StartBI - StartCurs; {start is in this line}
            end;
            if EndBI > LineStart + Ln then
            begin
              OpenEnd := True; {will continue on next line, end is open}
              BEnd := Start - Buff + Ln;
            end
            else
            begin
              OpenEnd := False;
              BEnd := EndBI - StartCurs; {end is in this line}
            end;
          end;
        end;
      end;
  end;
end;

{----------------TSection.Draw}

function TSection.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  I: Integer;
  MySelB, MySelE: Integer;
  DC: HDC;
  Ctrl: TFormControlObj;
  YOffset, Y, Desc: Integer;

  procedure DrawTheText(LineNo: Integer);
  var
    I, J, J1, J2, J3, J4, Index, Addon, TopP, BottomP, LeftT, Tmp, K: Integer;
    Obj: TFloatingObj;
    FO: TFontObj;
    ARect: TRect;
    Inverted, ImageAtStart, NewCP: boolean;
    Color: TColor;
    CP1: TPoint;
    CPx, CPy, CP1x: Integer;
    SaveColor: TColor;
    BR: BorderRec;
    LR: LineRec;
    Start: PWideChar;
    Cnt, Descent: Integer;
    St: WideString;

    function AddHyphen(P: PWideChar; N: Integer): WideString;
    var
      I: Integer;
    begin
      SetLength(Result, N + 1);
      for I := 1 to N do
        Result[I] := P[I - 1];
      Result[N + 1] := WideChar('-');
    end;

    function ChkInversion(Start: PWideChar; var Count: Integer): boolean;
    var
      LongCount, C: Integer;
    begin
      Result := False;
      C := Start - Buff;
      Count := 32000;
      if ParentSectionList.IsCopy then
        Exit;
      if (MySelE < MySelB) or ((MySelE = MySelB) and
        not ParentSectionList.ShowDummyCaret) then
        Exit;
      if (MySelB <= C) and (MySelE > C) then
      begin
        Result := True;
        LongCount := MySelE - C;
      end
      else if MySelB > C then
        LongCount := MySelB - C
      else
        LongCount := 32000;
      if LongCount > 32000 then
        Count := 32000
      else
        Count := LongCount;
    end;

  begin {Y is at bottom of line here}
    LR := LineRec(Lines[LineNo]);
    Start := LR.Start;
    Cnt := LR.Ln;
    Descent := LR.Descent;

    NewCP := True;
    ImageAtStart := True;
    CPx := X + LR.LineIndent;
    CP1x := CPx;
    LR.DrawY := Y - LR.LineHt;
    LR.DrawXX := CPx;
    while Cnt > 0 do
    begin
      I := 1;
      J1 := Fonts.GetFontCountAt(Start - Buff, Len) - 1;
      J2 := Images.GetImageCountAt(Start - Buff) - 1;
      J4 := TFormControlList(FormControls).GetControlCountAt(Start - Buff) - 1;
      FO := Fonts.GetFontObjAt(Start - Buff, Index);

    {if an inline border, find it's boundaries}
      if LR.FirstDraw and Assigned(LR.BorderList) then
        for K := 0 to LR.BorderList.Count - 1 do {may be several inline borders}
        begin
          BR := BorderRec(LR.BorderList.Items[K]);
          if (Start - Buff = BR.BStart) then
          begin {this is it's start}
            BR.bRect.Top := Y - FO.GetHeight(Desc) - Descent + Desc + 1;
            BR.bRect.Left := CPx;
            BR.bRect.Bottom := Y - Descent + Desc;
          end
          else if (Start - Buff = BR.BEnd) and (BR.bRect.Right = 0) then
            BR.bRect.Right := CPx {this is it's end}
          else if (Start - Buff > BR.BStart) and (Start - Buff < BR.BEnd) then
          begin {this is position within boundary, it's top or bottom may enlarge}
            BR.bRect.Top := Min(BR.bRect.Top, Y - FO.GetHeight(Desc) - Descent + Desc + 1);
            BR.bRect.Bottom := Max(BR.bRect.Bottom, Y - Descent + Desc);
          end;
        end;

      FO.TheFont.AssignToCanvas(Canvas);
      if J2 = -1 then
      begin {it's an image or panel}
        Obj := Images.FindImage(Start - Buff);
        if Obj is TImageObj then
        begin
          if Obj.HorzAlign in [ALeft, ARight] then
          begin
            if ImageAtStart then
            begin
              ParentSectionList.DrawList.AddImage(TImageObj(Obj), Canvas, IMgr.LfEdge + Obj.Indent,
                Y - LR.LineHt - LR.SpaceBefore, Y - Descent, FO);
            end
            else
            begin {if not at start, draw on next line}
              ParentSectionList.DrawList.AddImage(TImageObj(Obj), Canvas, IMgr.LfEdge + Obj.Indent, Y, Y - Descent, FO);
            end;
          {if a boundary is on a floating image, remove it}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := LR.BorderList.Count - 1 downto 0 do
              begin
                BR := BorderRec(LR.BorderList.Items[K]);
                if (Start - Buff = BR.BStart) and (BR.BEnd = BR.BStart + 1) then
                  LR.BorderList.Delete(K);
              end;
          end
          else
          begin
            SetTextJustification(Canvas.Handle, 0, 0);
            if Assigned(MyBlock) then
              TImageObj(Obj).Positioning := MyBlock.Positioning
            else
              TImageObj(Obj).Positioning := posStatic;
            TImageObj(Obj).Draw(Canvas, CPx + Obj.HSpaceL, Y - LR.LineHt, Y - Descent, FO);
          {see if there's an inline border for the image}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := 0 to LR.BorderList.Count - 1 do
              begin
                BR := BorderRec(LR.BorderList.Items[K]);
                if (Start - Buff >= BR.BStart) and (Start - Buff <= BR.BEnd) then
                begin {there is a border here, find the image dimensions}
                  with TImageObj(Obj) do
                    case VertAlign of
                      ATop, ANone:
                        begin
                          TopP := Y - LR.LineHt + VSpaceT;
                          BottomP := Y - LR.LineHT + ImageHeight + VSpaceT;
                        end;
                      AMiddle:
                        begin
                          TopP := Y - Descent + FO.Descent - (FO.tmHeight div 2) - ((ImageHeight - VSpaceT + VSpaceB) div 2);
                          BottomP := Y - Descent + FO.Descent - (FO.tmHeight div 2) - ((ImageHeight - VSpaceT + VSpaceB) div 2) + ImageHeight;
                        end;
                      ABottom, ABaseline:
                        begin
                          TopP := Y - Descent - Obj.ImageHeight - VSpaceB;
                          BottomP := Y - Descent - VSpaceB;
                        end;
                    else
                      begin
                        TopP := 0; {to eliminate warning msg}
                        BottomP := 0;
                      end;
                    end;
                  if (Start - Buff = BR.BStart) then
                  begin {border starts at image}
                    BR.bRect.Top := ToPP;
                    BR.bRect.Left := CPx + TImageObj(Obj).HSpaceL;
                    if BR.BEnd = BR.BStart + 1 then {border ends with image also, rt side set by image width}
                      BR.bRect.Right := BR.bRect.Left + TImageObj(Obj).ImageWidth;
                    BR.bRect.Bottom := BottomP;
                  end
                  else if Start - Buff = BR.BEnd then
                  else
                  begin {image is included in border and may effect the border top and bottom}
                    BR.bRect.Top := Min(BR.bRect.Top, ToPP);
                    BR.bRect.Bottom := Max(BR.bRect.Bottom, BottomP);
                  end;
                end;
              end;
            CPx := CPx + Obj.ImageWidth + Obj.HSpaceL + Obj.HSpaceR;
            NewCP := True;
            ImageAtStart := False;
          end;
        end
        else
        begin {it's a Panel}
          with TPanelObj(Obj) do
          begin
            ShowIt := True;
            if (Obj.HorzAlign in [ALeft, ARight]) then
            begin
              LeftT := IMgr.LfEdge + Obj.Indent;
              if ImageAtStart then
                TopP := Y - LR.LineHt - LR.SpaceBefore - YOffset + VSpaceT
              else
                TopP := Y - YOffset + VSpaceT;
            {check for border.  For floating panel, remove it}
              if LR.FirstDraw and Assigned(LR.BorderList) then
                for K := LR.BorderList.Count - 1 downto 0 do
                begin
                  BR := BorderRec(LR.BorderList.Items[K]);
                  if (Start - Buff = BR.BStart) and (BR.BEnd = BR.BStart + 1) then
                    LR.BorderList.Delete(K);
                end;
            end
            else
            begin
              LeftT := CPx + Obj.HSpaceL;
              case Obj.VertAlign of
                ATop, ANone: TopP := Y - YOffset - LR.LineHt + Obj.VSpaceT;
                AMiddle: TopP := Y - YOffset - FO.tmHeight div 2 - (ImageHeight - Obj.VSpaceT + Obj.VSpaceB) div 2;
                ABottom, ABaseline: TopP := Y - YOffset - ImageHeight - Descent - Obj.VSpaceB;
              else
                TopP := 0; {to eliminate warning msg}
              end;
            {Check for border on inline panel}
              if LR.FirstDraw and Assigned(LR.BorderList) then
                for K := 0 to LR.BorderList.Count - 1 do
                begin
                  BR := BorderRec(LR.BorderList.Items[K]);
                  if (Start - Buff >= BR.BStart) and (Start - Buff <= BR.BEnd) then
                  begin
                    if (Start - Buff = BR.BStart) then
                    begin {border starts on panel}
                      BR.bRect.Top := ToPP + YOffSet;
                      BR.bRect.Left := CPx + HSpaceL;
                      if BR.BEnd = BR.BStart + 1 then {border also ends with panel}
                        BR.bRect.Right := BR.bRect.Left + ImageWidth;
                      BR.bRect.Bottom := TopP + YOffSet + ImageHeight;
                    end
                    else if Start - Buff = BR.BEnd then
                    else
                    begin {Panel is included in border, may effect top and bottom}
                      BR.bRect.Top := Min(BR.bRect.Top, ToPP + YOffSet);
                      BR.bRect.Bottom := Max(BR.bRect.Bottom, TopP + YOffSet + ImageHeight);
                    end;
                  end;
                end;
              Inc(CPx, ImageWidth + Obj.HSpaceL + Obj.HSpaceR);
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
              else
                Panel.Hide;
            end;
            DrawXX := LeftT;
          end;
        end;
      end
      else if J4 = -1 then
      begin {it's a form control}
        Ctrl := TFormControlList(FormControls).FindControl(Start - Buff);
        if Assigned(Ctrl.FControl) then
          with Ctrl, FControl do
          begin
            ShowIt := True;
            case FormAlign of
              ATop:
                TopP := LR.DrawY + VSpaceT - YOffset;
              AMiddle:
                TopP := Y - ((LR.LineHt + Height) div 2) - YOffset;
              ABaseline:
                TopP := Y - Height - VSpaceB - Descent - YOffset; {sits on baseline}
              ABottom:
                TopP := Y - Height - VSpaceB - YOffset;
            else
              TopP := Y; {never get here}
            end;
            if FControl is ThtRadioButton then
              Inc(Topp, 2)
            else if FControl is ThtCheckBox then
              Inc(Topp, 1);
          {Check for border}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := 0 to LR.BorderList.Count - 1 do
              begin
                BR := BorderRec(LR.BorderList.Items[K]);
                if (Start - Buff >= BR.BStart) and (Start - Buff <= BR.BEnd) then
                begin
                  if (Start - Buff = BR.BStart) then
                  begin {border starts with Form control}
                    BR.bRect.Top := ToPP + YOffSet;
                    BR.bRect.Left := CPx + HSpaceL;
                    if BR.BEnd = BR.BStart + 1 then {border is confined to form control}
                      BR.bRect.Right := BR.bRect.Left + Width;
                    BR.bRect.Bottom := TopP + YOffSet + Height;
                  end
                  else if Start - Buff = BR.BEnd then
                  else
                  begin {form control is included in border}
                    BR.bRect.Top := Min(BR.bRect.Top, ToPP + YOffSet);
                    BR.bRect.Bottom := Max(BR.bRect.Bottom, TopP + YOffSet + Height);
                  end;
                end;
              end;
            if ParentSectionList.IsCopy then
              Ctrl.Draw(Canvas, CPx + Ctrl.HSpaceL, TopP)
            else
            begin
              Show;
              Top := TopP;
              Left := CPx + Ctrl.HSpaceL;
              if Ctrl is TRadioButtonFormControlObj then
                with TRadioButtonFormControlObj(Ctrl) do
                begin
                  TRadioButtonFormControlObj(Ctrl).TheControl.Show;
                  if MyCell.BkGnd then
                    (TheControl as TFormRadioButton).Color := MyCell.BkColor
                  else
                    (TheControl as TFormRadioButton).Color := ParentSectionList.Background;
                end;
              if Ctrl.Active and ((Ctrl is TRadioButtonFormControlObj) or
                (Ctrl is TCheckBoxFormControlObj)) then
              begin
                Canvas.Brush.Color := clWhite;
                SaveColor := SetTextColor(Handle, clBlack);
                if ParentSectionList.TheOwner.ShowFocusRect then //MK20091107
                begin
                  if (Ctrl is TRadioButtonFormControlObj) then
                  begin
                    if Screen.PixelsPerInch > 100 then
                      Canvas.DrawFocusRect(Rect(Left - 2, Top - 2, Left + 18, Top + 18))
                    else
                      Canvas.DrawFocusRect(Rect(Left - 3, Top - 2, Left + 16, Top + 16));
                  end
                  else
                    Canvas.DrawFocusRect(Rect(Left - 3, Top - 3, Left + 16, Top + 16));
                end;
                SetTextColor(Handle, SaveColor);
              end;
            end;
            Inc(CPx, Width + Ctrl.HSpaceL + Ctrl.HSpaceR);
            NewCP := True;
          end;
        ImageAtStart := False;
      end
      else
      begin
        J := Min(J1, J2);
        J := Min(J, J4);
        Inverted := ChkInversion(Start, J3);
        J := Min(J, J3 - 1);
        I := Min(Cnt, J + 1);

        if Inverted then
        begin
          SetBkMode(Canvas.Handle, Opaque);
          Canvas.Brush.Color := Canvas.Font.Color;
          if FO.TheFont.bgColor = clNone then
          begin
            Color := Canvas.Font.Color;
            if Color < 0 then
              Color := GetSysColor(Color and $FFFFFF)
            else
              Color := Color and $FFFFFF;
            Canvas.Font.Color := Color xor $FFFFFF;
          end
          else
            Canvas.Font.Color := FO.TheFont.bgColor;
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
            if Color < 0 then
              Color := GetSysColor(Color);
            if Color < 0 then
              Canvas.Font.Color := clBlack; {Print black}
          end;
          if not ParentSectionlist.PrintTableBackground then
          begin
            Color := Canvas.Font.Color;
            if Color < 0 then
              Color := GetSysColor(Color);
            if (Color and $E0E0) = $E0E0 then
              Canvas.Font.Color := $2A0A0A0; {too near white or yellow, make it gray}
          end;
        end;

        SetTextAlign(Canvas.Handle, TA_BaseLine);
      {figure any offset for subscript or superscript}
        with FO do
          if SScript = ANone then
            Addon := 0
          else if SScript = ASuper then
            Addon := -(FontHeight div 3)
          else
            Addon := Descent div 2 + 1;
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
            Tmp := I - 1 {at end of line, don't show space or break}
          else
            Tmp := I;
          if (Self is TPreformated) and not MyBlock.HideOverflow then
          begin {so will clip in Table cells}
            ARect := Rect(IMgr.LfEdge, Y - LR.LineHt - LR.SpaceBefore - YOffset, X + IMgr.ClipWidth, Y - YOffset + 1);
            ExtTextOutW(Canvas.Handle, CPx, CPy, ETO_CLIPPED, @ARect, Start, Tmp, nil);
            CP1x := CPx + GetXExtent(Canvas.Handle, Start, Tmp);
          end
          else
          begin
            if LR.Spaces = 0 then
              SetTextJustification(Canvas.Handle, 0, 0)
            else
              SetTextJustification(Canvas.Handle, LR.Extra, LR.Spaces);
            if not IsWin95 then {use TextOutW}
            begin
              if (Cnt - I <= 0) and LR.Shy then
              begin
                St := AddHyphen(Start, Tmp);
                TextOutW(Canvas.Handle, CPx, CPy, PWideChar(St), Length(St));
                CP1x := CPx + GetXExtent(Canvas.Handle, PWideChar(St), Length(St));
              end
              else
              begin
                TextOutW(Canvas.Handle, CPx, CPy, Start, Tmp);
                CP1x := CPx + GetXExtent(Canvas.Handle, Start, Tmp);
              end
            end
            else
            begin {Win95}
            {Win95 has bug which extends text underline for proportional font in TextOutW.
             Use clipping to clip the extra underline.}
              CP1x := CPx + GetXExtent(Canvas.Handle, Start, Tmp);
              ARect := Rect(CPx, Y - LR.LineHt - LR.SpaceBefore - YOffset, CP1x, Y - YOffset + 1);
              ExtTextOutW(Canvas.Handle, CPx, CPy, ETO_CLIPPED, @ARect, Start, Tmp, nil)
            end;
          end;
        {Put in a dummy caret to show character position}
          if ParentSectionList.ShowDummyCaret and not Inverted
            and (MySelB = Start - Buff) then
          begin
            Canvas.Pen.Color := Canvas.Font.Color;
            Tmp := Y - Descent + FO.Descent + Addon - YOffset;
            Canvas.Brush.Color := clWhite;
            Canvas.Rectangle(CPx, Tmp, CPx + 1, Tmp - FO.FontHeight);
          end;
        end;

        if FO.Active or ParentSectionList.IsCopy and Assigned(ParentSectionList.LinkDrawnEvent)
          and (FO.UrlTarget.Url <> '') then
        begin
          Tmp := Y - Descent + FO.Descent + Addon - YOffset;
          ARect := Rect(CPx, Tmp - FO.FontHeight, CP1x + 1, Tmp);
          if FO.Active then
          begin
            Canvas.Font.Color := clBlack; {black font needed for DrawFocusRect}
            DC := Canvas.Handle; {Dummy call needed to make Delphi add font color change to handle}
            if ParentSectionList.TheOwner.ShowFocusRect then //MK20091107
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
          and (Cnt = I) and (LineNo = Lines.Count - 1) then
        begin
          Canvas.Pen.Color := Canvas.Font.Color;
          Tmp := Y - Descent + FO.Descent + Addon - YOffset;
          Canvas.Brush.Color := clWhite;
          Canvas.Rectangle(CPx, Tmp, CPx + 1, Tmp - FO.FontHeight);
        end;

        ImageAtStart := False;
      end;
      Dec(Cnt, I);
      Inc(Start, I);
    end;
    SetTextJustification(Canvas.Handle, 0, 0);
  {at the end of this line.  see if there are open borders which need right side set}
    if LR.FirstDraw and Assigned(LR.BorderList) then
      for K := 0 to LR.BorderList.Count - 1 do
      begin
        BR := BorderRec(LR.BorderList.Items[K]);
        if BR.OpenEnd or (BR.BRect.Right = 0) then
          BR.BRect.Right := CPx;
      end;
  end;

  procedure DoDraw(I: Integer);
  {draw the Ith line in this section}
  var
    BR: BorderRec;
    K: Integer;
    XOffset: Integer;
  begin
    with LineRec(Lines[I]) do
    begin
      Inc(Y, LineHt + SpaceBefore);
      if FirstDraw then
      begin {see if any inline borders in this line}
        CheckForInlines(LineRec(Lines[I]));
        if FirstDraw then {if there are, need a first pass to get boundaries}
        begin
          FirstX := X;
          DrawTheText(I);
        end;
      end;
      XOffset := X - FirstX;
      FirstDraw := False;
      if Assigned(BorderList) then {draw any borders found in this line}
        for K := 0 to BorderList.Count - 1 do
        begin
          BR := BorderRec(BorderList.Items[K]);
          BR.DrawTheBorder(Canvas, XOffset, YOffSet, ParentSectionList.Printing);
        end;
      DrawTheText(I); {draw the text, etc., in this line}
      Inc(Y, SpaceAfter);
    end;
    ParentSectionList.FirstPageItem := False;
  end;

begin {TSection.Draw}
  Y := YDraw;
  Result := Y + SectionHeight;
  YOffset := ParentSectionList.YOff;

{Only draw if will be in display rectangle}
  if (Len > 0) and (Y - YOffset + DrawHeight + 40 >= ARect.Top)
    and (Y - YOffset - 40 < ARect.Bottom) then
  begin
    DC := Canvas.Handle;
    SetTextAlign(DC, TA_BaseLine);

    MySelB := ParentSectionList.SelB - StartCurs;
    MySelE := ParentSectionList.SelE - StartCurs;
    for I := 0 to Lines.Count - 1 do
      with ParentSectionList do
        if Printing then
          with LineRec(Lines[I]) do
          begin
            if (Y + LineImgHt <= PageBottom) then
            begin
              if (Y + LineImgHt - 1 > ARect.Top + YOffSet) then
                DoDraw(I)
              else
                Inc(Y, SpaceBefore + LineHt + SpaceAfter);
            end
            else if (LineImgHt >= ARect.Bottom - ARect.Top) or PageShortened then
              DoDraw(I)
            else
            begin
              if Assigned(MyBlock) and (MyBlock.Positioning = PosAbsolute) then
                DoDraw(I)
              else if Y < PageBottom then
                PageBottom := Y; {Dont' print, don't want partial line}
            end;
          end
        else
          with LineRec(Lines[I]) do
            if ((Y - YOffset + LineImgHt + 40 >= ARect.Top) and (Y - YOffset - 40 < ARect.Bottom)) then
              DoDraw(I)
            else {do not completely draw extremely long paragraphs}
              Inc(Y, SpaceBefore + LineHt + SpaceAfter);
  end;
end;

{----------------TSection.CopyToClipboard}

procedure TSection.CopyToClipboard;
var
  I, Strt, X1, X2: Integer;
  MySelB, MySelE: Integer;
begin
  MySelB := ParentSectionList.SelB - StartCurs;
  MySelE := ParentSectionList.SelE - StartCurs;
  for I := 0 to Lines.Count - 1 do
    with LineRec(Lines.Items[I]) do
    begin
      Strt := Start - Buff;
      if (MySelE <= Strt) or (MySelB > Strt + Ln) then
        Continue;
      if MySelB - Strt > 0 then
        X1 := MySelB - Strt
      else
        X1 := 0;
      if MySelE - Strt < Ln then
        X2 := MySelE - Strt
      else
        X2 := Ln;
      if (I = Lines.Count - 1) and (X2 = Ln) then
        Dec(X2);
      ParentSectionList.CB.AddText(Start + X1, X2 - X1);
    end;
  if MySelE > Len then
    ParentSectionList.CB.AddTextCR('', 0);
end;

{----------------TSection.PtInObject}

function TSection.PtInObject(X: Integer; Y: Integer; var Obj: TObject;
  var IX, IY: Integer): boolean;
{Y is distance from start of section}
begin
  Result := (Images.Count > 0) and Images.PtInObject(X, Y, Obj, IX, IY);
end;

{----------------TSection.GetURL}

function TSection.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj};
  var ATitle: ThtString): guResultType;
 {Y is absolute}
var
  I, L, Index, Width, IX, IY, Posn: Integer;
  FO: TFontObj;
  LR: LineRec;
  IMap, UMap: boolean;
  MapItem: TMapItem;
  ImageObj: TImageObj;
  Tmp: ThtString;

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
    else if ImageObj.Alt <> '' then
    begin
      ATitle := ImageObj.Alt;
      Include(Result, guTitle);
    end;
    ParentSectionList.ActiveImage := ImageObj;
    if Assigned(ImageObj.MyFormControl) then
    begin
      FormControl := ImageObj.MyFormControl;
      Include(Result, guControl);
      TImageFormControlObj(FormControl).XTmp := IX;
      TImageFormControlObj(FormControl).YTmp := IY;
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
      begin {found an URL}
        Include(Result, guUrl);
        UrlTarg := MakeCopy(FO.UrlTarget);
        ParentSectionList.ActiveLink := FO;
        if IMap then
          UrlTarg.Url := UrlTarg.Url + '?' + IntToStr(IX) + ',' + IntToStr(IY);
      end;
    end;
  end
  else
  begin
    I := 0;
    LR := nil;
    with Lines do
    begin
      while I < Count do
      begin
        LR := LineRec(Lines[I]);
        if (Y > LR.DrawY) and (Y <= LR.DrawY + LR.LineHt) then
          Break;
        Inc(I);
      end;
      if I >= Count then
        Exit;
    end;
    with LR do
    begin
      if X < DrawXX then
        Exit;
      Width := X - DrawXX;
      if Spaces > 0 then
        SetTextJustification(Canvas.Handle, Extra, Spaces);
      L := FindCountThatFits(Canvas, Width, Start, Ln);
      if Spaces > 0 then
        SetTextJustification(Canvas.Handle, 0, 0);
      if L >= Ln then
        Exit;
      FO := Fonts.GetFontObjAt(L + (Start - Buff), Index);
      if (FO.UrlTarget.Url <> '') then {found an URL}
        if not ((Start + L)^ in [ImgPan]) then {an image here would be in HSpace area}
        begin
          Include(Result, guUrl);
          UrlTarg := MakeCopy(FO.UrlTarget);
          ParentSectionList.ActiveLink := FO;
        end;
      if (FO.Title <> '') then {found a Title}
        if not ((Start + L)^ in [ImgPan]) then {an image here would be in HSpace area}
        begin
          ATitle := FO.Title;
          Include(Result, guTitle);
        end;
    end;
  end;
end;

{----------------TSection.FindCursor}

function TSection.FindCursor(Canvas: TCanvas; X: Integer; Y: Integer;
  var XR: Integer; var YR: Integer; var CaretHt: Integer;
  var Intext: boolean): Integer;
{Given an X, Y, find the character position and the resulting XR, YR position
 for a caret along with its height, CaretHt.  Coordinates are relative to this
 section}
var
  I, H, L, Width, TotalHt, L1, W, Delta, OHang: Integer;
  LR: LineRec;

begin
  Result := -1;
  I := 0; H := ContentTop; L1 := 0;
  LR := nil;
  with Lines do
  begin
    while I < Count do
    begin
      LR := LineRec(Lines[I]);
      with LR do
        TotalHt := LineHt + SpaceBefore + SpaceAfter;
      if H + TotalHt > Y then
        Break;
      Inc(H, TotalHt);
      Inc(I);
      Inc(L1, LR.Ln); {L1 accumulates ThtChar count of previous lines}
    end;
    if (I >= Count) then
      Exit;
  end;
  with LR do
  begin
    if X > LR.DrawXX + LR.DrawWidth then
      Exit;
    if X < LR.DrawXX - 10 then
      Exit;
    InText := True;
    CaretHt := LineHt;
    YR := H + SpaceBefore;
    if X < DrawXX then
    begin
      Result := L1 + StartCurs;
      InText := False;
      Exit;
    end;
    Width := X - DrawXX;
    if (Justify = FullJustify) and (Spaces > 0) then
      SetTextJustification(Canvas.Handle, Extra, Spaces);
    L := FindCountThatFits(Canvas, Width, Start, Ln);
    W := FindTextWidth(Canvas, Start, L, False);
    XR := DrawXX + W;
    if L < Ln then
    begin {check to see if passed 1/2 character mark}
      Fonts.GetFontAt(L1 + L, OHang).AssignToCanvas(Canvas);
      Delta := FindTextWidthA(Canvas, Start + L, 1);
      if Width > W + (Delta div 2) then
      begin
        Inc(L);
        Inc(XR, Delta);
      end;
    end
    else
      InText := False;
    Result := L + L1 + StartCurs;
    if Justify = FullJustify then
      SetTextJustification(Canvas.Handle, 0, 0);
  end;
end;

{----------------TSection.FindString}

function TSection.FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
{find the first occurance of the ThtString, ToFind, with a cursor value >= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  I: Integer;
  ToSearch: WideString;

begin
  Result := -1;
  if (Len = 0) or (From >= StartCurs + Len) then
    Exit;
  if From < StartCurs then
    I := 0
  else
    I := From - StartCurs;

  if MatchCase then
    ToSearch := BuffS
  else
    ToSearch := WideLowerCase1(BuffS); {ToFind already lower case}

  P := StrPosW(PWideChar(ToSearch) + I, PWideChar(ToFind));
  if Assigned(P) then
    Result := StartCurs + (P - PWideChar(ToSearch));
end;

{----------------TSection.FindStringR}

function TSection.FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
{find the first occurance of the ThtString, ToFind, with a cursor value <= to From.
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
  if (Len < ToFindLen) or (From - StartCurs + 1 < ToFindLen) then
    Exit;

  if From >= StartCurs + Len then
    ToSearch := BuffS {search all of BuffS}
  else
    ToSearch := Copy(BuffS, 1, From - StartCurs); {Search smaller part}
  if not MatchCase then
    ToSearch := WideLowerCase1(ToSearch); {ToFind already lower case}

{search backwards for the end ThtChar of ToFind}
  P := StrRScanW(PWideChar(ToSearch), ToFind[ToFindLen]);
  while Assigned(P) and (P - PWideChar(ToSearch) + 1 >= ToFindLen) do
  begin
  {pick out a ThtString of proper length from end ThtChar to see if it matches}
    SetString(ToMatch, P - ToFindLen + 1, ToFindLen);
    if WideSameStr1(ToFind, ToMatch) then
    begin {matches, return the cursor position}
      Result := StartCurs + (P - ToFindLen + 1 - PWideChar(ToSearch));
      Exit;
    end;
  {doesn't match, shorten ThtString to search for next search}
    ToSearch := Copy(ToSearch, 1, P - PWideChar(ToSearch));
  {and search backwards for end ThtChar again}
    P := StrRScanW(PWideChar(ToSearch), ToFind[ToFindLen]);
  end;
end;

{----------------TSection.FindSourcePos}

function TSection.FindSourcePos(DocPos: Integer): Integer;
var
  I: Integer;
  IO: IndexObj;
begin
  Result := -1;
  if (Len = 0) or (DocPos >= StartCurs + Len) then
    Exit;

  for I := SIndexList.Count - 1 downto 0 do
  begin
    IO := PosIndex[I];
    if IO.Pos <= DocPos - StartCurs then
    begin
      Result := IO.Index + DocPos - StartCurs - IO.Pos;
      break;
    end;
  end;
end;

{----------------TSection.FindDocPos}

function TSection.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
{for a given Source position, find the nearest document position either Next or
 previous}
var
  I: Integer;
  IO, IOPrev: IndexObj;
begin
  Result := -1;
  if Len = 0 then
    Exit;

  if not Prev then
  begin
    I := SIndexList.Count - 1;
    IO := PosIndex[I];
    if SourcePos > IO.Index + (Len - 1) - IO.Pos then
      Exit; {beyond this section}

    IOPrev := PosIndex[0];
    if SourcePos <= IOPrev.Index then
    begin {in this section but before the start of Document text}
      Result := StartCurs;
      Exit;
    end;

    for I := 1 to SIndexList.Count - 1 do
    begin
      IO := PosIndex[I];
      if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin {between IOprev and IO}
        if SourcePos - IOPrev.Index + IOPrev.Pos < IO.Pos then
          Result := StartCurs + IOPrev.Pos + (SourcePos - IOPrev.Index)
        else
          Result := StartCurs + IO.Pos;
        Exit;
      end;
      IOPrev := IO;
    end;
  {after the last IndexObj in list}
    Result := StartCurs + IOPrev.Pos + (SourcePos - IOPrev.Index);
  end
  else {prev  -- we're iterating from the end of TSectionList}
  begin
    IOPrev := PosIndex[0];
    if SourcePos < IOPrev.Index then
      Exit; {before this section}

    I := SIndexList.Count - 1;
    IO := PosIndex[I];
    if SourcePos > IO.Index + (Len - 1) - IO.Pos then
    begin {SourcePos is after the end of this section}
      Result := StartCurs + (Len - 1);
      Exit;
    end;

    for I := 1 to SIndexList.Count - 1 do
    begin
      IO := PosIndex[I];
      if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin {between IOprev and IO}
        if SourcePos - IOPrev.Index + IOPrev.Pos < IO.Pos then
          Result := StartCurs + IOPrev.Pos + (SourcePos - IOPrev.Index)
        else
          Result := StartCurs + IO.Pos - 1;
        Exit;
      end;
      IOPrev := IO;
    end;
  {after the last IndexObj in list}
    Result := StartCurs + IOPrev.Pos + (SourcePos - IOPrev.Index);
  end;
end;

{----------------TSection.CursorToXY}

function TSection.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer;
  var Y: Integer): boolean;
var
  I, Curs: Integer;
  LR: LineRec;
begin
  Result := False;
  if (Len = 0) or (Cursor > StartCurs + Len) then
    Exit;

  I := 0;
  LR := nil;
  Curs := Cursor - StartCurs;
  Y := ContentTop;
  with Lines do
  begin
    while I < Count do
    begin
      LR := LineRec(Lines[I]);
      with LR do
      begin
        if Curs < Ln then
          Break;
        Inc(Y, LineHt + SpaceBefore + SpaceAfter);
        Dec(Curs, Ln);
      end;
      Inc(I);
    end;
    if I >= Count then
      Exit;
  end;
  if Assigned(Canvas) then
  begin
    if LR.Spaces > 0 then
      SetTextJustification(Canvas.Handle, LR.Extra, LR.Spaces);
    X := LR.DrawXX + FindTextWidth(Canvas, LR.Start, Curs, False);
    if LR.Spaces > 0 then
      SetTextJustification(Canvas.Handle, 0, 0);
  end
  else
    X := LR.DrawXX;
  Result := True;
end;

{----------------TSection.GetChAtPos}

function TSection.GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
  Result := False;
  if (Len = 0) or (Pos < StartCurs) or (Pos >= StartCurs + Len) then
    Exit;
  Ch := Buff[Pos - StartCurs];
  Obj := Self;
  Result := True;
end;

{----------------TPanelObj.Create}

constructor TPanelObj.Create(AMasterList: TSectionList; Position: Integer;
  L: TAttributeList; ACell: TCellBasic; ObjectTag: boolean);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  NewSpace: Integer;
  S, Source, AName, AType: ThtString;
begin
  inherited Create;
  fMasterList := AMasterList;
  Pos := Position;
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
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
{$ifndef LCL}
    Ctl3D := False;
    ParentCtl3D := False;
{$endif}
    ParentFont := False;
    VertAlign := ABottom; {default}
    HorzAlign := ANone;
    NewSpace := -1;
    for I := 0 to L.Count - 1 do
      with TAttribute(L[I]) do
        case Which of
          HeightSy:
            if System.Pos('%', Name) = 0 then
            begin
              SpecHeight := Max(1, Value); {spec ht of 0 becomes 1}
              Height := SpecHeight; {so panels ht will be set for OnPanelCreate}
            end
            else if (Value > 0) and (Value <= 100) then
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
              Value := Max(1, Min(Value, 100));
              SpecWidth := Value;
              PercentWidth := True;
            end;
          HSpaceSy: NewSpace := Min(40, Abs(Value));
          VSpaceSy: VSpaceT := Min(40, Abs(Value));
          SrcSy: Source := Name;
          NameSy:
            begin
              AName := Name;
              try
                Panel.Name := Name;
              except {duplicate name will be ignored}
              end;
            end;
          AlignSy:
            begin
              S := UpperCase(Name);
              if S = 'TOP' then
                VertAlign := ATop
              else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then
                VertAlign := AMiddle
              else if S = 'LEFT' then
                HorzAlign := ALeft
              else if S = 'RIGHT' then
                HorzAlign := ARight;
            end;
          AltSy:
            begin
              SetAlt(CodePage, Name);
              ImageTitle := Alt; {use Alt as default Title}
            end;
          TypeSy: AType := Name;
        end;
    if NewSpace >= 0 then
      HSpaceL := NewSpace
    else if HorzAlign in [ALeft, ARight] then
      HSpaceL := ImageSpace {default}
    else
      HSpaceL := 0;

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

constructor TPanelObj.CreateCopy(AMasterList: TSectionBaseList; T: TPanelObj);
begin
  assert(AMasterList is TSectionList);
  inherited CreateCopy(T);
  Panel := ThvPanel.Create(nil);
  with T.Panel do
    Panel.SetBounds(Left, Top, Width, Height);
  Panel.FVisible := T.Panel.FVisible;
  Panel.Color := T.Panel.Color;
  Panel.Parent := (AMasterList as TSectionList).PPanel;
  SpecWidth := T.SpecWidth;
  PercentWidth := T.PercentWidth;
  SpecHeight := T.SpecHeight;
  PercentHeight := T.PercentHeight;
  SetHeight := T.SetHeight;
  SetWidth := T.SetWidth;
  OPanel := T.Panel; {save these for printing}
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

procedure TPanelObj.DrawLogic(SectionList: TSectionBaseList; Canvas: TCanvas;
  FO: TFontObjBase; AvailableWidth, AvailableHeight: Integer);
begin
  if not ImageKnown or PercentWidth or PercentHeight then
  begin
    if PercentWidth then
    begin
      ImageWidth := MulDiv(AvailableWidth, SpecWidth, 100);
      if SpecHeight <> 0 then
        if PercentHeight then
          ImageHeight := MulDiv(AvailableHeight, SpecHeight, 100)
        else
          ImageHeight := SpecHeight
      else
        ImageHeight := MulDiv(ImageWidth, SetHeight, SetWidth);
    end
    else if PercentHeight then
    begin
      ImageHeight := MulDiv(AvailableHeight, SpecHeight, 100);
      if SpecWidth <> 0 then
        ImageWidth := SpecWidth
      else
        ImageWidth := MulDiv(ImageHeight, SetWidth, SetHeight);
    end
    else if (SpecWidth <> 0) and (SpecHeight <> 0) then
    begin {Both width and height specified}
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
    begin {neither height and width specified}
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

procedure TPanelObj.Draw(ACanvas: TCanvas; X1, Y1: Integer);
var
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
  OldPenColor: TColor;
  Bitmap: TBitmap;
  OldHeight, OldWidth: Integer;
  SaveFont: TFont;
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
          PrintBitmap(ACanvas, X1, Y1, ImageWidth, ImageHeight, Bitmap);
        finally
          with Opanel do
            SetBounds(Left, Top, OldWidth, OldHeight);
          Bitmap.Free;
        end;
      end
      else
      begin
        OldBrushStyle := Brush.Style; {save style first}
        OldBrushColor := Brush.Color;
        OldPenColor := Pen.Color;
        Pen.Color := clBlack;
        Brush.Color := Panel.Color;
        Brush.Style := bsSolid;

        ACanvas.Rectangle(X1, Y1, X1 + ImageWidth, Y1 + ImageHeight);
        SaveFont := TFont.Create;
        try
          SaveFont.Assign(ACanvas.Font);
          with ACanvas.Font do
          begin
            Size := 8;
            Name := 'Arial';
          end;
          WrapTextW(ACanvas, X1 + 5, Y1 + 5, X1 + ImageWidth - 5, Y1 + ImageHeight - 5, FAltW);
        finally
          ACanvas.Font := SaveFont;
          SaveFont.Free;
          Brush.Color := OldBrushColor;
          Brush.Style := OldBrushStyle; {style after color as color changes style}
          Pen.Color := OldPenColor;
        end;
      end;
end;

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

{----------------TCell.Create}

constructor TCell.Create(Master: TSectionBaseList);
begin
  inherited Create(Master);
  IMgr := TIndentManager.Create;
end;

{----------------TCell.CreateCopy}

constructor TCell.CreateCopy(AMasterList: TSectionBaseList; T: TCellBasic);
begin
  inherited CreateCopy(AMasterList, T);
  IMgr := TIndentManager.Create;
end;

destructor TCell.Destroy;
begin
  IMgr.Free;
  inherited Destroy;
end;

{----------------TCell.DoLogic}

function TCell.DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
  var ScrollWidth: Integer; var Curs: Integer): Integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  IB: Integer;
  LIndex, RIndex: Integer;
  SaveID: TObject;
begin
  IMgr.Clear;
  IMgr.Reset(0, Width);
  IMgr.Width := Width;
  SaveID := IMgr.CurrentID;
  IMgr.CurrentID := Self;

  LIndex := IMgr.SetLeftIndent(0, Y);
  RIndex := IMgr.SetRightIndent(0 + Width, Y);

  Result := inherited DoLogic(Canvas, Y, Width, AHeight, BlHt, ScrollWidth, Curs);

  IMgr.FreeLeftIndentRec(LIndex);
  IMgr.FreeRightIndentRec(RIndex);
  IB := IMgr.ImageBottom - YValue; {check for image overhang}
  IMgr.CurrentID := SaveID;
  if IB > Result then
    Result := IB;
end;

{----------------TCell.Draw}

function TCell.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer;
  Y, XRef, YRef: Integer): Integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
begin
  IMgr.Reset(X, X + IMgr.Width);
  IMgr.ClipWidth := ClipWidth;
  DrawYY := Y; {This is overridden in TCellObj.Draw}
  Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y, XRef, YRef);
end;

{----------------TCellObjCell.CreateCopy}

constructor TCellObjCell.CreateCopy(AMasterList: TSectionBaseList; T: TCellObjCell);
begin
  inherited CreateCopy(AMasterList, T);
  MyRect := T.MyRect; ;
end;

{----------------TCellObjCell.GetUrl}

function TCellObjCell.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj};
  var ATitle: ThtString): guResultType;
{Y is absolute}
begin
  Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  if PtInRect(MyRect, Point(X, Y - MasterList.YOFF)) then
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

function TBlockCell.DoLogicX(Canvas: TCanvas; X, Y, XRef, YRef, Width, AHeight, BlHt: Integer;
  var ScrollWidth, Curs: Integer): Integer;
{Do the entire layout of the this cell.  Return the total pixel height}
var
  I, Sw, TheCount: Integer;
  H, Tmp: Integer;
  SB: TSectionBase;
begin
  YValue := Y;
  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  TextWidth := 0;
  tcContentBot := 0;
  tcDrawTop := 990000;
  tcDrawBot := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
  begin
    SB := Items[I];
    Tmp := SB.DrawLogic(Canvas, X, Y + H, XRef, YRef, Width, AHeight, BlHt, IMgr, Sw, Curs);
    Inc(H, Tmp);
    if Owner.HideOverflow then
      ScrollWidth := Width
    else
      ScrollWidth := Max(ScrollWidth, Sw);
    if SB is TSection then
      TextWidth := Max(TextWidth, TSection(SB).TextWidth);
    if (SB is TBlock) and (TBlock(SB).Positioning = posAbsolute) then
    else
      tcContentBot := Max(tcContentBot, SB.ContentBot);
    tcDrawTop := Min(tcDrawTop, SB.DrawTop);
    tcDrawBot := Max(tcDrawBot, SB.DrawBot);
    Inc(I);
  end;
  Len := Curs - StartCurs;
  Result := H;
  CellHeight := Result;
end;


{ TDrawList }

type
  TImageRec = class(TObject)
    AObj: TImageObj;
    ACanvas: TCanvas;
    AX, AY: Integer;
    AYBaseline: Integer;
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
  I: Integer;
  Item: TObject;
begin
  I := 0;
  while I < Count do {note: Count may increase during this operation}
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
  if GetKeyState(vk_Tab) < 0 then {ignore if tab key down}
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
      if not ClicksDisabled then
        Click;
    end;
  end;
end;

procedure TFormRadioButton.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {else don't get the arrow keys}
end;

{----------------TFormCheckBox.WMGetDlgCode}

procedure TFormCheckBox.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {this to eat the arrow keys}
end;

{----------------ThtTabcontrol.Destroy}

procedure ThtTabcontrol.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {this to eat the arrow keys}
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

procedure BorderRec.DrawTheBorder(Canvas: TCanvas; XOffset, YOffSet: Integer; Printing: boolean);
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

{----------------TPage.Draw1}

function TPage.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  YOffset, Y: Integer;
begin
  Result := inherited Draw1(Canvas, ARect, Imgr, X, XRef, YRef);
  if ParentSectionList.Printing then
  begin
    Y := YDraw;
    YOffset := ParentSectionList.YOff;
    if (Y - YOffset > ARect.Top + 5) and (Y - YOffset < ARect.Bottom) and (Y + ARect.Top - 1 < ParentSectionList.PageBottom) then
      ParentSectionList.PageBottom := Y + ARect.Top - 1;
  end;
end;

{----------------THorzLine.Create}

constructor THorzLine.Create(AMasterList: TSectionList; L: TAttributeList;
  Prop: TProperties);
var
  LwName: ThtString;
  I: Integer;
  TmpColor: TColor;
begin
  inherited Create(AMasterList, Prop.GetDisplay);
  VSize := 2;
  Align := Centered;
  Color := clNone;
  for I := 0 to L.Count - 1 do
    with TAttribute(L[I]) do
      case Which of
        SizeSy: if (Value > 0) and (Value <= 20) then
          begin
            VSize := Value;
          end;
        WidthSy:
          if Value > 0 then
            if Pos('%', Name) > 0 then
            begin
              if (Value <= 100) then
                Prop.Assign(IntToStr(Value) + '%', StyleUn.Width);
            end
            else
              Prop.Assign(Value, StyleUn.Width);
        ColorSy: if ColorFromString(Name, False, Color) then
            Prop.Assign(Color, StyleUn.Color);
        AlignSy:
          begin
            LwName := Lowercase(Name);
            if LwName = 'left' then
              Align := Left
            else if LwName = 'right' then
              Align := Right;
          end;
        NoShadeSy: NoShade := True;
      end;

  Prop.Assign(VSize, StyleUn.Height); {assigns if no property exists yet}
  TmpColor := Prop.GetOriginalForegroundColor;
  if TmpColor <> clNone then
    Color := TmpColor;
  with Prop do
    if (VarIsStr(Props[TextAlign])) and Originals[TextAlign] then
      if Props[TextAlign] = 'left' then
        Align := Left
      else if Props[TextAlign] = 'right' then
        Align := Right
      else if Props[TextAlign] = 'center' then
        Align := Centered;
end;

constructor THorzLine.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
begin
  inherited Create(AMasterList, T.Display);
  System.Move((T as THorzline).VSize, VSize, DWord(@BkGnd) - DWord(@VSize) + Sizeof(BkGnd));
end;

procedure THorzLine.CopyToClipboard;
begin
  ParentSectionList.CB.AddTextCR('', 0);
end;

function THorzLine.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
begin
  YDraw := Y;
  StartCurs := Curs;
{Note: VSize gets updated in THRBlock.FindWidth}
  ContentTop := Y;
  DrawTop := Y;
  Indent := Max(X, IMgr.LeftIndent(Y));
  Width := Min(X + AWidth - Indent, IMgr.RightSide(Y) - Indent);
  MaxWidth := Width;
  SectionHeight := VSize;
  DrawHeight := SectionHeight;
  ContentBot := Y + SectionHeight;
  DrawBot := Y + DrawHeight;
  Result := SectionHeight;
end;

{----------------THorzLine.Draw}

function THorzLine.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  XR: Integer;
  YT, YO, Y: Integer;
  White, BlackBorder: boolean;
begin
  Y := YDraw;
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
  YO := Y - ParentSectionList.YOff;
  if (YO + SectionHeight >= ARect.Top) and (YO < ARect.Bottom) and
    (not ParentSectionList.Printing or (Y < ParentSectionList.PageBottom)) then
    with Canvas do
    begin
      YT := YO;
      XR := X + Width - 1;
      if Color <> clNone then
      begin
        Brush.Color := Color or $2000000;
        Brush.Style := bsSolid;
        FillRect(Rect(X, YT, XR + 1, YT + VSize));
      end
      else
      begin
        with ParentSectionList do
        begin
          White := Printing or ((Background and $FFFFFF = clWhite) or
            ((Background = clWindow) and (GetSysColor(Color_Window) = $FFFFFF)));
          BlackBorder := NoShade or (Printing and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
            (GetDeviceCaps(Handle, PLANES) = 1));
        end;
        if BlackBorder then
          Pen.Color := clBlack
        else if White then
          Pen.Color := clSilver
        else
          Pen.Color := clBtnHighLight;
        MoveTo(XR, YT);
        LineTo(XR, YT + VSize - 1);
        LineTo(X, YT + VSize - 1);
        if BlackBorder then
          Pen.Color := clBlack
        else
          Pen.Color := clBtnShadow;
        LineTo(X, YT);
        LineTo(XR, YT);
      end;
      ParentSectionList.FirstPageItem := False; {items after this will not be first on page}
    end;
end;

procedure TPreformated.ProcessText(TagIndex: Integer);
var
  FO: TFontObj;
begin
  FO := TFontObj(Fonts.Items[Fonts.Count - 1]); {keep font the same for inserted space}
  if FO.Pos = Length(BuffS) then
    Inc(FO.Pos);
  BuffS := BuffS + ' ';
  XP^[Length(BuffS) - 1] := XP^[Length(BuffS) - 2] + 1;
  Finish;
end;

procedure TPreformated.MinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
begin
  if BreakWord then
  begin
    inherited;
    Exit;
  end;
  if Len = 0 then
  begin
    Max := 0;
    Min := 0;
  end
  else
  begin
    if StoredMax = 0 then
    begin
      Max := FindTextWidth(Canvas, Buff, Len - 1, False);
      StoredMax := Max;
    end
    else
      Max := StoredMax;
    Min := Math.Min(MaxHScroll, Max);
  end;
end;

function TPreFormated.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
var
  Dummy: Integer;
  Save: Integer;
begin
  if Len = 0 then
  begin
    ContentTop := Y;
    Result := Fonts.GetFontObjAt(0, Dummy).FontHeight;
    SectionHeight := Result;
    MaxWidth := 0;
    YDraw := Y;
    DrawHeight := Result;
    ContentBot := Y + Result;
    DrawBot := ContentBot;
  end
  else if not BreakWord then
  begin
  {call with large width to prevent wrapping}
    Save := IMgr.Width;
    IMgr.Width := 32000;
    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, Dummy, Curs);
    IMgr.Width := Save;
    MinMaxWidth(Canvas, Dummy, MaxWidth); {return MaxWidth}
  end
  else
  begin
    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  end;
end;

{ TSectionSubsBase }

//-- BG ---------------------------------------------------------- 12.09.2010 --
function TSectionSubsBase.getMyBlock: TBlock;
begin
  Result := TBlock(inherited MyBlock);
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
function TSectionSubsBase.getParentSectionList: TSectionList;
begin
  Result := TSectionList(inherited ParentSectionList);
end;

{ THtmlPropStack }

{ Add a TProperties to the PropStack. }
procedure THtmlPropStack.PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProps: TProperties);
var
  NewProp: TProperties;
begin
  NewProp := TProperties.Create(self);
  NewProp.Inherit(Tag, Last);
  Add(NewProp);
  NewProp.Combine(MasterList.Styles, Tag, AClass, AnID, APseudo, ATitle, AProps, Count - 1);
end;

procedure THtmlPropStack.PopProp;
{pop and free a TProperties from the Prop stack}
var
  TopIndex: Integer;
begin
  TopIndex := Count - 1;
  if TopIndex > 0 then
    Delete(TopIndex);
end;

procedure THtmlPropStack.PopAProp(const Tag: ThtString);
{pop and free a TProperties from the Prop stack.  It should be on top but in
 case of a nesting error, find it anyway}
var
  I, J: Integer;
begin
  for I := Count - 1 downto 1 do
    if Items[I].Proptag = Tag then
    begin
      if Items[I].GetBorderStyle <> bssNone then
      {this would be the end of an inline border}
        MasterList.ProcessInlines(SIndex, Items[I], False);
      Delete(I);
      if I > 1 then {update any stack items which follow the deleted one}
        for J := I to Count - 1 do
          Items[J].Update(Items[J - 1], MasterList.Styles, J);
      Break;
    end;
end;

{ THtmlStyleList }

procedure THtmlStyleList.AddModifyProp(const Selector, Prop, Value: ThtString);
{strings are all lowercase here}
var
  I: Integer;
  PropIndex: PropIndices;
  Propty: TProperties;
  NewColor: TColor;
  NewProp: boolean;
begin
  if FindPropIndex(Prop, PropIndex) then
  begin
    if not Find(Selector, I) then
    begin
      NewProp := True;
      Propty := TProperties.Create(); {newly created property}
    end
    else
    begin
      Propty := TProperties(Objects[I]); {modify existing property}
      NewProp := False;
    end;
    case PropIndex of
      Color:
        if ColorFromString(Value, False, NewColor) then
        begin
          if Selector = ':link' then
          begin {changed the defaults to be the same as link}
            ModifyLinkColor('hover', NewColor);
            ModifyLinkColor('visited', NewColor);
          end
          else if Selector = ':visited' then
            ModifyLinkColor('hover', NewColor);
          Propty.Props[PropIndex] := NewColor;
        end;
      BorderColor:
        if ColorFromString(Value, False, NewColor) then
        begin
          Propty.Props[BorderColor] := NewColor;
          Propty.Props[BorderLeftColor] := NewColor;
          Propty.Props[BorderTopColor] := NewColor;
          Propty.Props[BorderRightColor] := NewColor;
          Propty.Props[BorderBottomColor] := NewColor;
        end;
      BorderTopColor..BorderLeftColor:
        if ColorFromString(Value, False, NewColor) then
          Propty.Props[PropIndex] := NewColor;
      BackgroundColor:
        if ColorFromString(Value, False, NewColor) then
          Propty.Props[PropIndex] := NewColor
        else
          Propty.Props[PropIndex] := clNone;
      Visibility:
        begin
          if Value = 'visible' then
            Propty.Props[Visibility] := viVisible
          else if Value = 'hidden' then
            Propty.Props[Visibility] := viHidden;
        end;
      TextTransform:
        begin
          if Value = 'uppercase' then
            Propty.Props[TextTransform] := txUpper
          else if Value = 'lowercase' then
            Propty.Props[TextTransform] := txLower
          else
            Propty.Props[TextTransform] := txNone;
        end;
      WordWrap:
        if Value = 'break-word' then
          Propty.Props[WordWrap] := Value
        else
          Propty.Props[WordWrap] := 'normal';
      FontVariant:
        if Value = 'small-caps' then
          Propty.Props[FontVariant] := Value
        else if Value = 'normal' then
          Propty.Props[FontVariant] := 'normal';
      BorderTopStyle..BorderLeftStyle:
        begin
          if Value <> 'none' then
            Propty.Props[BorderStyle] := Value;
          Propty.Props[PropIndex] := Value;
        end;
      BorderStyle:
        begin
          Propty.Props[BorderStyle] := Value;
          Propty.Props[BorderTopStyle] := Value;
          Propty.Props[BorderRightStyle] := Value;
          Propty.Props[BorderBottomStyle] := Value;
          Propty.Props[BorderLeftStyle] := Value;
        end;
      LineHeight:
        Propty.Props[PropIndex] := Value;
    else
      Propty.Props[PropIndex] := Value;
    end;
    if NewProp then
      AddObject(Selector, Propty); {it's a newly created property}
    if Pos(':hover', Selector) > 0 then
      TSectionList(MasterList).LinksActive := True;
    if Selector = 'a' then
    begin
      AddModifyProp('::link', Prop, Value); {also applies to ::link}
    end;
{$IFDEF Quirk}
    if (Selector = 'body') and (PropIndex = Color) then
      FixupTableColor(Propty);
{$ENDIF}
  end;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
constructor THtmlStyleList.Create(AMasterList: TObject);
begin
  inherited Create;
  MasterList := AMasterList;
end;

{ TFieldsetBlock }

//-- BG ---------------------------------------------------------- 09.10.2010 --
procedure TFieldsetBlock.ContentMinMaxWidth(Canvas: TCanvas; var Min, Max: Integer);
var
  LegendMin, LegendMax: Integer;
  ContentMin, ContentMax: Integer;
begin
  Legend.MinMaxWidth(Canvas, LegendMin, LegendMax);
  inherited ContentMinMaxWidth(Canvas, ContentMin, ContentMax);
  Min := Math.Max(ContentMin, LegendMin);
  Max := Math.Max(ContentMax, LegendMax);
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
procedure TFieldsetBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; var AutoCount: Integer);
var
  PaddTop, Delta: Integer;
begin
  inherited;
  MargArray[MarginTop] := VMargToMarg(MargArrayO[MarginTop], False, BaseHeight, EmSize, ExSize, 10);
  Delta := Legend.CellHeight - (MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop]);
  if Delta > 0 then
  begin
    PaddTop := Delta div 2;
    MargArray[MarginTop] := MargArray[MarginTop] + Delta - PaddTop;
    MargArray[PaddingTop] := MargArray[PaddingTop] + PaddTop;
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
constructor TFieldsetBlock.Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
  Attributes: TAttributeList);
var
  Index: PropIndices;
begin
  inherited;
  BorderStyle := bssSolid;
  for Index := BorderTopStyle to BorderLeftStyle do
    if VarIsIntNull(MargArrayO[Index]) or VarIsEmpty(MargArrayO[Index]) then
      MargArrayO[Index] := BorderStyle;
  for Index := BorderTopColor to BorderLeftColor do
    if VarIsIntNull(MargArrayO[Index]) or VarIsEmpty(MargArrayO[Index]) then
      MargArrayO[Index] := RGB(165, 172, 178);
  for Index := BorderTopWidth to BorderLeftWidth do
    if VarIsIntNull(MargArrayO[Index]) or VarIsEmpty(MargArrayO[Index]) then
      MargArrayO[Index] := 1;
//  for Index := MarginTop to MarginLeft do
//    if VarIsIntNull(MargArrayO[Index]) or VarIsEmpty(MargArrayO[Index]) then
//      MargArrayO[Index] := 10;
  for Index := PaddingTop to PaddingLeft do
    if VarIsIntNull(MargArrayO[Index]) or VarIsEmpty(MargArrayO[Index]) then
      MargArrayO[Index] := 10;
  FLegend := TBlockCell.Create(Master);
  FLegend.Owner := Self;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
constructor TFieldsetBlock.CreateCopy(AMasterList: TSectionBaseList; T: TSectionBase);
begin
  inherited;
  FLegend := TBlockCell.CreateCopy(AMasterList, (T as TFieldsetBlock).FLegend);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
destructor TFieldsetBlock.Destroy;
begin
  FLegend.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
function TFieldsetBlock.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef,
  YRef: Integer): Integer;
var
  Rect: TRect;
begin
  case Display of
    pdNone:   Result := 0;
  else
    Rect.Left := X + MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft] - 2;
    Rect.Right := Rect.Left + Legend.TextWidth + 4;
    Rect.Top := YDraw - ParentSectionList.YOff;
    Rect.Bottom := Rect.Top + Legend.CellHeight;
    Legend.Draw(Canvas, ARect, NewWidth, Rect.Left + 2, YDraw, XRef, YRef);
    Rect := CalcClipRect(Canvas, Rect, ParentSectionList.Printing);
    ExcludeClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
procedure TFieldsetBlock.DrawBlockBorder(Canvas: TCanvas; ORect, IRect: TRect);
begin
  inherited;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function TFieldsetBlock.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer;
  IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer;
var
  BorderWidth: TRect;
  AutoCount, BlockHeight, ScrollWidth, L, LI, RI: Integer;
  SaveID: TObject;
begin
  case Display of

    pdNone:
    begin
      SectionHeight := 0;
      DrawHeight := 0;
      ContentBot := 0;
      DrawBot := 0;
      MaxWidth := 0;
      Result := 0;
    end;

  else
    StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderStyle, self.BorderWidth, AutoCount, MargArray);
    BorderWidth.Left := MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
    BorderWidth.Right := MargArray[MarginRight] + MargArray[BorderRightWidth] + MargArray[PaddingRight];
    BorderWidth.Top  := MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop];
    if MargArray[Height] > 0 then
      BlockHeight := MargArray[Height]
    else if AHeight > 0 then
      BlockHeight := AHeight
    else
      BlockHeight := BlHt;

    L := X + BorderWidth.Left;
    NewWidth := AWidth - BorderWidth.Left - BorderWidth.Right;

    SaveID := IMgr.CurrentID;
    IMgr.CurrentID := Self;
    Legend.IMgr := IMgr;
    LI := IMgr.SetLeftIndent(L, Y);
    RI := IMgr.SetRightIndent(L + NewWidth, Y);
    Legend.DoLogicX(Canvas, X + BorderWidth.Left, Y, XRef, YRef, NewWidth, MargArray[Height], BlockHeight, ScrollWidth, Curs);
    IMgr.FreeLeftIndentRec(LI);
    IMgr.FreeRightIndentRec(RI);
    IMgr.CurrentID := SaveID;

    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  end;
end;


initialization
{$ifdef UNICODE}
{$else}
  {$IFDEF UseElPack}
    UnicodeControls := True;
  {$ENDIF}

  {$IFDEF UseTNT}
    UnicodeControls := not IsWin32Platform;
  {$ENDIF}
{$endif}
end.
