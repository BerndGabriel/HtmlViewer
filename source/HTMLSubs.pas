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

{
This module is comprised mostly of the various Section object definitions.
As the HTML document is parsed, it is divided up into sections.  Some sections
are quite simple, like TParagraphSpace.  Others are more complex such as
TSection which can hold a complete paragraph.

The HTML document is then stored as a list of type ThtDocument, of the various sections.

Closely related to ThtDocument is TCell.  TCell holds the list of sections for
each cell in a Table (the THtmlTable section).  In this way each table cell may
contain a document of it's own.

The Section objects each store relevant data for the section such as the text,
fonts, images, and other info needed for formating.

Each Section object is responsible for its own formated layout.  The layout is
done in the DrawLogic method.  Layout for the whole document is done in the
ThtDocument.DoLogic method which essentially just calls all the Section
DrawLogic's.  It's only necessary to call ThtDocument.DoLogic when a new
layout is required (when the document is loaded or when its width changes).

Each Section is also responsible for drawing itself (its Draw method).  The
whole document is drawn with the ThtDocument.Draw method.
}

unit HTMLSubs;

{-$define DO_BLOCK_INLINE}
{$ifdef DO_BLOCK_INLINE}
{$endif}

interface

uses
{$ifdef VCL}
  Windows,
  EncdDecd,
{$endif}
  Messages, Graphics, Controls, ExtCtrls, Classes, SysUtils, Variants, Forms, Math, Contnrs,
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc, types,
{$endif}
  HtmlGlobals,
  HTMLUn2,
  StyleUn,
  HTMLGif2;

type

//------------------------------------------------------------------------------
// TSectionBase, the abstract base class for all document sections
//------------------------------------------------------------------------------
// Each block is a section (see TBlock and its derivates) and a series of text
// and non block building "inline" tags is held in a section (see TSection)
//
// Base class for TSection, TBlock, THtmlTable, TPage and THorzLine
//------------------------------------------------------------------------------

  ThtDocument = class;
  TBlock = class;
  THtmlPropStack = class;

  TSectionBase = class(TIDObject)
  private
    FDocument: ThtDocument; // the document it belongs to
    FOwner: TBlock;         // the parental block it is placed in
    FDisplay: TPropDisplay; // how it is displayed
  protected
    function GetYPosition: Integer; override;
    procedure SetDocument(List: ThtDocument);
  public
    // source buffer reference
    StartCurs: Integer;     // where the section starts in the source buffer.
    Len: Integer;           // number of bytes in source buffer the section represents.
    // Z coordinates are calculated in Create()
    ZIndex: Integer;
    // Y coordinates calculated in DrawLogic() are still valid in Draw1()
    YDraw: Integer;         // where the section starts.
    DrawTop: Integer;       // where the border starts.  In case of a block this is YDraw + MarginTop
    ContentTop: Integer;    // where the content starts. In case of a block this is YDraw + MarginTop + PaddingTop + BorderTopWidth
    ContentBot: Integer;    // where the section ends.   In case of a block this is Block.ClientContentBot + PaddingBottom + BorderBottomWidth + MarginBottom
    DrawBot: Integer;       // where the border ends.    In case of a block this is Max(Block.ClientContentBot, MyCell.tcDrawBot) + PaddingBottom + BorderBottomWidth
    SectionHeight: Integer; // pixel height of section. = ContentBot - YDraw
    DrawHeight: Integer;    // floating image may overhang. = Max(ContentBot, DrawBot) - YDraw
    // X coordinates calculated in DrawLogic() may be shifted in Draw1(), if section is centered or right aligned

    constructor Create(AMasterList: ThtDocument; ADisplay: TPropDisplay); overload;
    constructor Create(AMasterList: ThtDocument; AProp: TProperties); overload;
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); virtual;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean; virtual;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; virtual;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: boolean): Integer; virtual;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; virtual;
    function FindSourcePos(DocPos: Integer): Integer; virtual;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; virtual;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; virtual;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean; virtual;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj}; out ATitle: ThtString): guResultType; virtual;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean; virtual;
    procedure AddSectionsToList; virtual;
    procedure CopyToClipboard; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); virtual;
    property Display: TPropDisplay read FDisplay write FDisplay;
    property Owner: TBlock read FOwner; //BG, 07.02.2011: public for reading document structure (see issue 24). Renamed from MyBlock to Owner to clarify the relation.
    property Document: ThtDocument read FDocument;
  end;

  TSectionBaseList = class(TFreeList)
  private
    function getItem(Index: Integer): TSectionBase;
  public
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean; virtual;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; virtual;
    property Items[Index: Integer]: TSectionBase read getItem; default;
  end;

//------------------------------------------------------------------------------
// TCellBasic, the base class for content
//------------------------------------------------------------------------------
// Base class for table cells, block content and the entire document
//------------------------------------------------------------------------------

  TCellBasic = class(TSectionBaseList) {a list of sections and blocks}
  private
    FDocument: ThtDocument; // the document it belongs to
    FOwner: TBlock;         // the parental block it is placed in
  public
    // source buffer reference
    StartCurs: Integer;     // where the section starts in the source buffer.
    Len: Integer;           // number of bytes in source buffer the section represents.
    //
    IMgr: TIndentManager;   // Each tag displayed as a block needs an indent manager.
    BkGnd: boolean;
    BkColor: TColor;
    OwnersTag: ThtString;   // TODO -oBG, 10.03.2011: move to Owner
    // Y coordinates calculated in DrawLogic() are still valid in Draw1()
    YValue: Integer;        // vertical position at top of cell. As this is a simple container, YValue is same as Self[0].YDraw.
    tcDrawTop: Integer;
    tcContentBot: Integer;
    tcDrawBot: Integer;

    constructor Create(Master: ThtDocument);
    constructor CreateCopy(AMasterList: ThtDocument; T: TCellBasic);
    function CheckLastBottomMargin: boolean;
    function DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X, Y, XRef, YRef: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer; out XR, YR, Ht: Integer; out Intext: boolean): Integer;
    function FindSourcePos(DocPos: Integer): Integer;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): guResultType; virtual;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;
    procedure Add(Item: TSectionBase; TagIndex: Integer);
    procedure AddSectionsToList;
    procedure CopyToClipboard;
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); virtual;
    property MasterList: ThtDocument read FDocument; {the ThtDocument that holds the whole document}
    property Owner: TBlock read FOwner;
  end;

  TCell = class(TCellBasic)
  private
    DrawYY: Integer;
  public
    constructor Create(Master: ThtDocument);
    constructor CreateCopy(AMasterList: ThtDocument; T: TCellBasic);
    destructor Destroy; override;
    function DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X, Y, XRef, YRef: Integer): Integer; override;
  end;

//------------------------------------------------------------------------------
// TFontObj, contains an atomic font info for a part of a section.
// TFontList, contains all font infos of a section
//------------------------------------------------------------------------------

  TSection = class;

  ThtTabControl = class(TWinControl)
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
    FSection: TSection; // only used if NoTabLink is not defined.
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
    function GetFontInfoIndex: FIIndex;
    property FontInfoIndex: FIIndex read GetFontInfoIndex;
  public
    Pos: Integer; {0..Len  Index where font takes effect}
    TheFont: TMyFont;
    FIArray: TFontInfoArray;
    FontHeight, {tmHeight+tmExternalLeading}
      tmHeight,
      tmMaxCharWidth,
      Overhang,
      Descent: Integer;
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
    function GetFontAt(Posn: Integer; out OHang: Integer): TMyFont;
    function GetFontCountAt(Posn, Leng: Integer): Integer;
    function GetFontObjAt(Posn: Integer; out Index: Integer): TFontObj;
    procedure Decrement(N: Integer; Document: ThtDocument);
  end;

//------------------------------------------------------------------------------
// TFloatingObj, an inline block for floating images and panels resp. iframes.
//------------------------------------------------------------------------------
// TFloatingObj is base class for floating objects like TImageObj and TPanelObj.
//
// These objects may appear in text flow or attribute ALIGN or style FLOAT may
// push them out of the flow floating to the left or right side in the
// containing block.
//
// BG, 06.03.2011: PANEL is not HTML standard, but its implementation could be
//     the base for HTML tag IFRAME, which is not yet supported by THtmlViewer.
//
// Base class for TImageObj and TPanelObj
//------------------------------------------------------------------------------

  TFloatingObj = class(TIDObject)
  public
    Pos: Integer; {0..Len  index of image position}
    ImageHeight, {does not include VSpace}
    ImageWidth: Integer;
    Floating: AlignmentType;
    VertAlign: AlignmentType;
    Indent: Integer;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB: Integer; {horizontal, vertical extra space}
    SpecWidth: Integer; {as specified by <img or panel> tag}
    SpecHeight: Integer; {as specified by <img or panel> tag}
    PercentWidth: boolean; {if width is percent}
    PercentHeight: boolean; {if height is percent}
    ImageTitle: ThtString;
    FAlt: ThtString; {the alt= attribute}

    function GetYPosition: Integer; override;
  public
    ImageKnown: boolean; {know size of image}
    DrawYY: Integer;
    DrawXX: Integer;
    NoBorder: boolean; {set if don't want blue border}
    BorderSize: Integer;
    constructor CreateCopy(T: TFloatingObj);
    function TotalHeight: Integer; {$ifdef UseInline} inline; {$endif}
    function TotalWidth: Integer; {$ifdef UseInline} inline; {$endif}
    procedure SetAlt(CodePage: Integer; const Value: ThtString);
    procedure DrawLogic(SectionList: ThtDocument; Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); virtual; abstract;
    procedure ProcessProperties(Prop: TProperties);
    property Alt: ThtString read FAlt;
  end;


  ThvPanel = class(TPanel)
  public
    FVisible: boolean;
    procedure SetVisible(Value: boolean); {$ifdef LCL} override; {$endif}
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  TPanelCreateEvent = procedure(Sender: TObject; const AName, AType, SRC: ThtString; Panel: ThvPanel) of object;
  TPanelDestroyEvent = procedure(Sender: TObject; Panel: ThvPanel) of object;
  TPanelPrintEvent = procedure(Sender: TObject; Panel: ThvPanel; const Bitmap: TBitmap) of object;

  TPanelObj = class(TFloatingObj)
  private
    FMasterList: ThtDocument;
    SetWidth, SetHeight: Integer;
    IsCopy: boolean;
  public
    ShowIt: boolean;
    Panel, OPanel: ThvPanel;
    OSender: TObject;
    PanelPrintEvent: TPanelPrintEvent;
    FUserData: TObject;
    FMyPanelObj: TPanelObj;
    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList; ACell: TCellBasic; ObjectTag: boolean);
    constructor CreateCopy(AMasterList: ThtDocument; T: TPanelObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: ThtDocument; Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure Draw(ACanvas: TCanvas; X1, Y1: Integer);
  end;


  HoverType = (hvOff, hvOverUp, hvOverDown);

  TImageFormControlObj = class;

  TImageObj = class(TFloatingObj) {inline image info}
  private
    FImage: TgpObject; {bitmap possibly converted from GIF, Jpeg, etc or animated GIF}
    FBitmap: TBitmap;
    FHover: HoverType;
    FHoverImage: boolean;
    AltHeight, AltWidth: Integer;
    Positioning: PositionType;
    function GetBitmap: TBitmap;
    procedure SetHover(Value: HoverType);
    procedure setImage(const AValue: TgpObject);
  public
    ObjHeight, ObjWidth: Integer; {width as drawn}
    Source: ThtString; {the src= attribute}
    OrigImage: TgpObject; {same as above unless swapped}
    Mask: TBitmap; {Image's mask if needed for transparency}
    Document: ThtDocument;
    Transparent: Transparency; {None, Lower Left Corner, or Transp GIF}
    IsMap, UseMap: boolean;
    MapName: ThtString;
    MyFormControl: TImageFormControlObj; {if an <INPUT type=image}
    MyCell: TCellBasic;
    Swapped: boolean; {image has been replaced}
    Missing: boolean; {waiting for image to be downloaded}

    constructor Create(MasterList: ThtDocument; Position: Integer; L: TAttributeList);
    constructor SimpleCreate(MasterList: ThtDocument; const AnURL: ThtString);
    constructor CreateCopy(AMasterList: ThtDocument; T: TImageObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: ThtDocument; Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: TgpObject; ddMask: TBitmap);
    procedure Draw(Canvas: TCanvas; X: Integer; TopY, YBaseline: Integer; FO: TFontObj);
    function InsertImage(const UName: ThtString; Error: boolean; out Reformat: boolean): boolean;

    property Bitmap: TBitmap read GetBitmap;
    property Hover: HoverType read FHover write SetHover;
    property Image: TgpObject read FImage write setImage;
    procedure ReplaceImage(NewImage: TStream);
  end;

  TDrawList = class(TFreeList)
    procedure AddImage(Obj: TImageObj; Canvas: TCanvas; X, TopY, YBaseline: Integer; FO: TFontObj);
    procedure DrawImages;
  end;


  TFloatingObjList = class(TFreeList) {a list of TImageObj's and TPanelObj's}
  public
    constructor CreateCopy(AMasterList: ThtDocument; T: TFloatingObjList);
    function FindImage(Posn: Integer): TFloatingObj;
    function GetHeightAt(Posn: Integer; out AAlign: AlignmentType; out FlObj: TFloatingObj): Integer;
    function GetImageCountAt(Posn: Integer): Integer;
    function GetWidthAt(Posn: Integer; out AAlign: AlignmentType; out HSpcL, HSpcR: Integer; out FlObj: TFloatingObj): Integer;
    function PtInImage(X, Y: Integer; out IX, IY, Posn: Integer; out AMap, UMap: boolean; out MapItem: TMapItem; out ImageObj: TImageObj): boolean;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;
    procedure Decrement(N: Integer);
  end;

//------------------------------------------------------------------------------
// TSection holds a series of text and inline tags like images and panels.
//------------------------------------------------------------------------------
// Holds tags like A, B, I, FONT, SPAN, IMG, ...
//------------------------------------------------------------------------------

  TFormControlObj = class;
  TFormControlObjList = class;

  LineRec = class(TObject) {holds info on a line of text}
  private
    Start: PWideChar;
    SpaceBefore, SpaceAfter,
      LineHt, {total height of line}
      LineImgHt, {top to bottom including any floating image}
      Ln, {# chars in line}
      Descent,
      LineIndent: Integer; // relative to section's left edge
    DrawXX, DrawWidth: Integer;
    DrawY: Integer;
    Spaces, Extra: Integer;
    BorderList: TFreeList; {List of inline borders (BorderRec's) in this Line}
    FirstDraw: boolean; {set if border processing needs to be done when first drawn}
    FirstX: Integer; {x value at FirstDraw}
    Shy: boolean;
  public
    constructor Create(SL: ThtDocument);
    procedure Clear;
    destructor Destroy; override;
  end;

  XArray = array[0..300] of Integer;
  PXArray = ^XArray;

  IndexObj = class
    Pos: Integer;
    Index: Integer;
  end;

  TSection = class(TSectionBase)
  {TSection holds <p>, <li>, many other things, and the base for lists}
  private
    SectionNumber: Integer;
    ThisCycle: Integer;
    function GetIndexObj(I: Integer): IndexObj;
    property PosIndex[I: Integer]: IndexObj read GetIndexObj;
    procedure CheckForInlines(LR: Linerec);
  public
    BuffS: WideString;  {holds the text or one of #2 (Form), #4 (Image/Panel), #8 (break char) for the section}
    Buff: PWideChar;    {same as above}
    Brk: AnsiString;    // Brk[n]: Can I wrap to new line after BuffS[n]? One of 'a' (optional), 'n' (no), 's' (soft), 'y' (yes) per character in BuffS
    XP: PXArray;
    BuffSize: Integer; {buffer may be larger}
    Fonts: TFontList; {List of FontObj's in this section}
    Images: TFloatingObjList; {list of TImageObj's, the images in section}
    FormControls: TFormControlObjList; {list of TFormControls in section}
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

    constructor Create(AMasterList: ThtDocument; L: TAttributeList; Prop: TProperties; AnURL: TUrlTarget; OwnerCell: TCellBasic; FirstItem: boolean);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    destructor Destroy; override;
    function AddFormControl(Which: Symb; AMasterList: ThtDocument; L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TFormControlObj;
    function AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer): TImageObj;
    function AddPanel(L: TAttributeList; ACell: TCellBasic; Index: Integer): TPanelObj;
    function CreatePanel(L: TAttributeList; ACell: TCellBasic): TPanelObj;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function FindCountThatFits(Canvas: TCanvas; Width: Integer; Start: PWideChar; Max: Integer): Integer;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: boolean): Integer;
    function FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj}; out ATitle: ThtString): guResultType; override;
    function PtInObject(X: Integer; Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean; override;
    procedure AddChar(C: WideChar; Index: Integer); virtual;
    procedure AddOpBrk;
    procedure AddPanel1(PO: TPanelObj; Index: Integer);
    procedure AddTokenObj(T: TokenObj); virtual;
    procedure Allocate(N: Integer);
    procedure ChangeFont(Prop: TProperties);
    procedure CheckFree;
    procedure CopyToClipboard; override;
    procedure Finish;
    procedure HRef(Sy: Symb; List: ThtDocument; AnURL: TUrlTarget; Attributes: TAttributeList; Prop: TProperties);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
    procedure ProcessText(TagIndex: Integer); virtual;
  end;

//------------------------------------------------------------------------------
// TBlock represents block tags.
//------------------------------------------------------------------------------
// A block is a rectangular area which may have a border
// with margins outside and padding inside the border. It contains a
// cell, which itself contains any kind of the html document content.
//
// Holds tags like DIV, FORM, PRE, P, H1..H6, UL, OL, DL, DIR, MENU, ...
//------------------------------------------------------------------------------

  TBlockCell = class(TCellBasic)
  private
    CellHeight: Integer;
    TextWidth: Integer;

    function DoLogicX(Canvas: TCanvas; X, Y, XRef, YRef, Width, AHeight, BlHt: Integer;
      out ScrollWidth: Integer; var Curs: Integer): Integer;
  end;

  TBlock = class(TSectionBase)
  protected
    function getBorderWidth: Integer; virtual;
    procedure ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); virtual;
    procedure ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer); virtual;
    procedure DrawBlockBorder(Canvas: TCanvas; const ORect, IRect: TRect); virtual;
    property BorderWidth: Integer read getBorderWidth;
  public
    MyCell: TBlockCell; // the block content
    MargArrayO: TVMarginArray;
    OwnerCell: TCellBasic; //BG, 10.03.2011: should be in TSectionBase instead of FDocument and FOwner
    BGImage: TImageObj;    //BG, 10.03.2011: see also bkGnd and bkColor in TCellBasic one background should be enough.
    BlockTitle: ThtString;
    TagClass: ThtString; {debugging aid} //BG, 10.03.2011: see also TCellBasic.OwnersTag

    // Notice: styling tag attributes are deprecated by W3C an must be translated
    //         to the corresponding style properties with a very low priority.

    // BEGIN: this area is copied by move() in CreateCopy() - NO string types allowed!
    MargArray: TMarginArray;
    EmSize, ExSize, FGColor: Integer;
    HasBorderStyle: Boolean;
    FloatLR: AlignmentType; {ALeft or ARight if floating}
    ClearAttr: ClearAttrType;
// BG, 25.03.2012: unused:    IsListBlock: boolean;
    PRec: PtPositionRec;
    Positioning: PositionType;
    Visibility: VisibilityType;
    BottomAuto: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    HideOverflow: boolean;
    Justify: JustifyType;
    Converted: boolean;
    // END: this area is copied by move() in CreateCopy()

    NewWidth: Integer;
    ClearAddon: Integer;
    Indent: Integer;
    NeedDoImageStuff: boolean;
    TiledImage: TgpObject;
    TiledMask, FullBG: TBitmap;
    TopP, LeftP: Integer;
    DrawList: TList;
    NoMask: boolean;
    ClientContentBot: Integer;
    MyRect: TRect;
    MyIMgr: TIndentManager;
    RefIMgr: TIndentManager;

    constructor Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    destructor Destroy; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; virtual;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): guResultType; override;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean; override;
    procedure AddSectionsToList; override;
    procedure CollapseMargins;
    procedure CopyToClipboard; override;
    procedure DrawBlock(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y, XRef, YRef: Integer);
    procedure DrawSort;
    procedure DrawTheList(Canvas: TCanvas; const ARect: TRect; ClipWidth, X, XRef, YRef: Integer);
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
  end;

//------------------------------------------------------------------------------
// THtmlForm, an object containing a form for user input
//------------------------------------------------------------------------------

  TRadioButtonFormControlObj = class;

  ThtmlForm = class(TObject)
  private
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    MasterList: ThtDocument;
    Method: ThtString;
    Action, Target, EncType: ThtString;
    ControlList: TFormControlObjList;
    NonHiddenCount: Integer;
    constructor Create(AMasterList: ThtDocument; L: TAttributeList);
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
    FName: ThtString;
    FID: ThtString;
    FTitle: ThtString;
    FValue: ThtString;
    function GetAttribute(const AttrName: ThtString): ThtString;
    procedure SetValue(const Value: ThtString);
  protected
    CodePage: Integer;
    function GetControl: TWinControl; virtual; abstract;
    function GetHeight: Integer; virtual;
    function GetLeft: Integer; virtual;
    function GetTabOrder: Integer; virtual;
    function GetTabStop: Boolean; virtual;
    function GetTop: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetYPosition: Integer; override;
    function IsHidden: Boolean; virtual;
    procedure DoOnChange; virtual;
    procedure SaveContents; virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetLeft(Value: Integer); virtual;
    procedure SetTabOrder(Value: Integer); virtual;
    procedure SetTabStop(Value: Boolean); virtual;
    procedure SetTop(Value: Integer); virtual;
    procedure SetWidth(Value: Integer); virtual;
  public
    Pos: Integer; {0..Len  index of control position}
    MasterList: ThtDocument;
    MyForm: ThtmlForm;
    FormAlign: AlignmentType;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB, BordT, BordB: Integer;
    FHeight, FWidth: Integer;
    PercentWidth: boolean;
    Disabled: boolean;
    Readonly: boolean;
    BkColor: TColor;
    ShowIt: boolean;
    OnBlurMessage: ThtString;
    OnChangeMessage: ThtString;
    OnClickMessage: ThtString;
    OnFocusMessage: ThtString;

    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList);
    constructor CreateCopy(T: TFormControlObj);
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): boolean; virtual;
    function TotalHeight: Integer; {$ifdef UseInline} inline; {$endif}
    function TotalWidth: Integer; {$ifdef UseInline} inline; {$endif}
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); virtual;
    procedure EnterEvent(Sender: TObject); {these two would be better private}
    procedure ExitEvent(Sender: TObject);
    procedure FormControlClick(Sender: TObject);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Hide; virtual;
    procedure ProcessProperties(Prop: TProperties); virtual;
    procedure ResetToValue; virtual;
    procedure SetData(Index: Integer; const V: ThtString); virtual;
    procedure SetDataInit; virtual;
    procedure SetHeightWidth(Canvas: TCanvas); virtual;
    procedure Show; virtual;

    property AttributeValue[const AttrName: ThtString]: ThtString read GetAttribute;
    property Height: Integer read GetHeight write SetHeight;
    property Hidden: Boolean read IsHidden;
    property ID: ThtString read FID; {ID attribute of control}
    property Left: Integer read GetLeft write SetLeft;
    property Name: ThtString read FName; {Name given to control}
    property TabOrder: Integer read GetTabOrder write SetTabOrder;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property TheControl: TWinControl read GetControl; {the Delphi control, TButton, TMemo, etc}
    property Title: ThtString read FTitle write FTitle;
    property Top: Integer read GetTop write SetTop;
    property Value: ThtString read FValue write SetValue;
    property Width: Integer read GetWidth write SetWidth;
    property YValue: Integer read FYValue;
  end;

  //BG, 15.01.2011:
  TFormControlObjList = class(TObjectList)
  private
    function GetItem(Index: Integer): TFormControlObj;
  public
    function FindControl(Posn: Integer): TFormControlObj;
    function GetHeightAt(Posn: Integer; out FormAlign: AlignmentType): Integer;
    function GetWidthAt(Posn: Integer; out HSpcL, HSpcR: Integer): Integer;
    function GetControlCountAt(Posn: Integer): Integer;
    procedure Decrement(N: Integer);

    procedure ActivateTabbing;
    procedure DeactivateTabbing;
    property Items[Index: Integer]: TFormControlObj read GetItem; default;
  end;

  THiddenFormControlObj = class(TFormControlObj)
  protected
    function GetControl: TWinControl; override;
    function GetHeight: Integer; override;
    function GetLeft: Integer; override;
    function GetTabOrder: Integer; override;
    function GetTabStop: Boolean; override;
    function GetTop: Integer; override;
    function GetWidth: Integer; override;
    function IsHidden: Boolean; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetLeft(Value: Integer); override;
    procedure SetTabOrder(Value: Integer); override;
    procedure SetTabStop(Value: Boolean); override;
    procedure SetTop(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
    function GetSubmission(Index: Integer; out S: ThtString): boolean; override;
    procedure Hide; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure Show; override;
  end;

  TImageFormControlObj = class(TFormControlObj)
  private
    FControl: ThtButton;
    MyImage: TImageObj;
  protected
    function GetControl: TWinControl; override;
  public
    XPos, YPos, XTmp, YTmp: Integer; {click position}
    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList);
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): boolean; override;
    procedure ImageClick(Sender: TObject);
    procedure ProcessProperties(Prop: TProperties); override;
  end;

  TEditFormControlObj = class(TFormControlObj)
  private
    FControl: ThtEdit;
    EnterContents: ThtString;
    tmAveCharWidth: Integer;
    function getText: ThtString;
    procedure setText(const Value: ThtString);
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    EditSize: Integer;
    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList; const Typ: ThtString; Prop: TProperties);
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): boolean; override;
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ResetToValue; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    property Text: ThtString read getText write setText;
  end;

  WhichType = (Submit, ResetB, Button, Browse);

  TButtonFormControlObj = class(TFormControlObj)
  private
    FControl: ThtButton;
  protected
    function GetControl: TWinControl; override;
  public
    Which: WhichType;
    MyEdit: TEditFormControlObj;
    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList; const Typ: ThtString; Prop: TProperties);
    destructor Destroy; override;
    procedure ButtonClick(Sender: TObject);
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  TFormRadioButton = class(ThtRadioButton)
  private
    IDName: ThtString;
    FChecked: boolean;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    function GetChecked: Boolean; override;
    procedure CreateWnd; override;
    procedure SetChecked(Value: Boolean); override;
  published
    property Checked: boolean read GetChecked write SetChecked;
  end;

  TRadioButtonFormControlObj = class(TFormControlObj)
  private
    FControl: TFormRadioButton;
    WasChecked: boolean;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function GetColor: TColor; //override;
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
    procedure SetColor(const Value: TColor); //override;
  public
    IsChecked: boolean;
    MyCell: TCellBasic;
    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList; ACell: TCellBasic);
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): boolean; override;
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure RadioClick(Sender: TObject);
    procedure ResetToValue; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    property Checked: Boolean read GetChecked write SetChecked;
    property Color: TColor read GetColor write SetColor;
  end;

  TFormCheckBox = class(ThtCheckBox)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  end;

  TCheckBoxFormControlObj = class(TFormControlObj)
  private
    FControl: TFormCheckBox;
    WasChecked: boolean;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    IsChecked: boolean;
    constructor Create(AMasterList: ThtDocument; Position: Integer; L: TAttributeList; Prop: TProperties);
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): boolean; override;
    procedure Draw(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure ResetToValue; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure SetDataInit; override;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  ListTypeType = (None, Ordered, Unordered, Definition, liAlone);

//------------------------------------------------------------------------------
// some blocks
//------------------------------------------------------------------------------

  THRBlock = class(TBlock)
  public
    Align: JustifyType;
    MyHRule: TSectionBase;
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
  end;

  TBlockLI = class(TBlock)
  private
    FListType: ListTypeType;
    FListNumb: Integer;
    FListStyleType: ListBulletType;
    FListFont: TFont;
    Image: TImageObj;
    FirstLineHt: Integer;
    procedure SetListFont(const Value: TFont);
  public
    constructor Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic;
      Sy: Symb; APlain: boolean; AIndexType: ThtChar;
      AListNumb, ListLevel: Integer; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property ListNumb: Integer read FListNumb write FListNumb;
    property ListStyleType: ListBulletType read FListStyleType write FListStyleType;
    property ListType: ListTypeType read FListType write FListType;
    property ListFont: TFont read FListFont write SetListFont;
  end;

  TFieldsetBlock = class(TBlock)
  private
    FLegend: TBlockCell;
  protected
    procedure ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer); override;
    procedure ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
  public
    constructor Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property Legend: TBlockCell read FLegend;
  end;

  TBodyBlock = class(TBlock)
  public
    constructor Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): guResultType; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

//------------------------------------------------------------------------------
// THtmlTable, a block that represents a html table
//------------------------------------------------------------------------------

  TCellObjCell = class(TCell)
  private
    MyRect: TRect;
    Title: ThtString;
    Url, Target: ThtString;
  public
    constructor CreateCopy(AMasterList: ThtDocument; T: TCellObjCell);
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget;
      out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): guResultType; override;
  end;

  IntArray = array of Integer;
  TWidthTypeArray = array of TWidthType;
  TIntegerPerWidthType = array [TWidthType] of Integer;

  TCellObj = class(TObject)
  {holds one cell of the table and some other information}
  private
    // BEGIN: this area is copied by move() in CreateCopy() - NO string types allowed!
    FColSpan, FRowSpan: Integer; {column and row spans for this cell}
    FWd: Integer; {total width (may cover more than one column)}
    FHt: Integer; {total height (may cover more than one row)}
    FVSize: Integer; {Actual vertical size of contents}
    FSpecWd: TSpecWidth; {Width attribute (percentage or absolute)}
    FSpecHt: TSpecWidth; {Height as specified}
    FYIndent: Integer; {Vertical indent}
    FVAlign: AlignmentType; {Top, Middle, or Bottom}
    FEmSize, FExSize: Integer;
    FPRec: PtPositionRec; // background image position info
    FPad: TRect;
    FBrd: TRect;
    FHzSpace, FVrSpace: Integer;
    FHasBorderStyle: Boolean;
    FShowEmptyCells: Boolean;
    // END: this area is copied by move() in CreateCopy()
    FCell: TCellObjCell;
    procedure Initialize(TablePadding: Integer; const BkImageName: ThtString; const APRec: PtPositionRec; Border: Boolean);
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: Integer; Border: Boolean; Light, Dark: TColor);
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: Integer; var Curs: Integer);

    // BG, 08.01.2012: Issue 109: C++Builder cannot handle properties that reference record members.
    // - added for legacy support only, will be removed in a near future release.
    //   Please use properties Border and Padding instead.
    function getBorderBottom: Integer;
    function getBorderLeft: Integer;
    function getBorderRight: Integer;
    function getBorderTop: Integer;
    function getPaddingBottom: Integer;
    function getPaddingLeft: Integer;
    function getPaddingRight: Integer;
    function getPaddingTop: Integer;
    procedure setBorderBottom(const Value: Integer);
    procedure setBorderLeft(const Value: Integer);
    procedure setBorderRight(const Value: Integer);
    procedure setBorderTop(const Value: Integer);
    procedure setPaddingBottom(const Value: Integer);
    procedure setPaddingLeft(const Value: Integer);
    procedure setPaddingRight(const Value: Integer);
    procedure setPaddingTop(const Value: Integer);
  public

    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage: TGpObject;
    TiledMask, FullBG: TBitmap;
    MargArray: TMarginArray;
    MargArrayO: TVMarginArray;
    NoMask: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;

    constructor Create(Master: ThtDocument; AVAlign: AlignmentType; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: ThtDocument; T: TCellObj);
    destructor Destroy; override;

    property Border: TRect read FBrd write FBrd; //was: BrdTop, BrdRight, BrdBottom, BrdLeft: Integer;
    property BrdBottom: Integer read getBorderBottom write setBorderBottom;
    property BrdLeft: Integer read getBorderLeft write setBorderLeft;
    property BrdRight: Integer read getBorderRight write setBorderRight;
    property BrdTop: Integer read getBorderTop write setBorderTop;
    property Cell: TCellObjCell read FCell;
    property ColSpan: Integer read FColSpan write FColSpan; {column and row spans for this cell}
    property EmSize: Integer read FEmSize write FEmSize;
    property ExSize: Integer read FExSize write FExSize;
    property HasBorderStyle: Boolean read FHasBorderStyle write FHasBorderStyle;
    property Ht: Integer read FHt write FHt; {total height (may cover more than one row)}
    property HzSpace: Integer read FHzSpace write FHzSpace;
    property Padding: TRect read FPad write FPad; //was: PadTop, PadRight, PadBottom, PadLeft: Integer;
    property PadBottom: Integer read getPaddingBottom write setPaddingBottom;
    property PadLeft: Integer read getPaddingLeft write setPaddingLeft;
    property PadRight: Integer read getPaddingRight write setPaddingRight;
    property PadTop: Integer read getPaddingTop write setPaddingTop;
    property PRec: PtPositionRec read FPRec write FPRec;
    property RowSpan: Integer read FRowSpan write FRowSpan; {column and row spans for this cell}
    property ShowEmptyCells: Boolean read FShowEmptyCells write FShowEmptyCells;
    property SpecHt: TSpecWidth read FSpecHt write FSpecHt; {Height as specified}
// BG, 12.01.2012: not C++-Builder compatible
//    property SpecHtType: TWidthType read FSpecHt.VType write FSpecHt.VType; {Height as specified}
//    property SpecHtValue: Double read FSpecHt.Value write FSpecHt.Value; {Height as specified}
    property SpecWd: TSpecWidth read FSpecWd write FSpecWd; {Width as specified}
// BG, 12.01.2012: not C++-Builder compatible
//    property SpecWdType: TWidthType read FSpecWd.VType write FSpecWd.VType; {Height as specified}
//    property SpecWdValue: Double read FSpecWd.Value write FSpecWd.Value; {Height as specified}
    property VAlign: AlignmentType read FVAlign write FVAlign; {Top, Middle, or Bottom}
    property VrSpace: Integer read FVrSpace write FVrSpace;
    property VSize: Integer read FVSize write FVSize; {Actual vertical size of contents}
    property Wd: Integer read FWd write FWd; {total width (may cover more than one column)}
    property YIndent: Integer read FYIndent write FYIndent; {Vertical indent}
  end;

  TCellList = class(TFreeList)
  {holds one row of the html table, a list of TCellObj}
  private
    function getCellObj(Index: Integer): TCellObj;
  public
    RowHeight: Integer;
    SpecRowHeight: TSpecWidth;
    RowSpanHeight: Integer; {height of largest rowspan}
    BkGnd: boolean;
    BkColor: TColor;
    BkImage: ThtString;
    APRec: PtPositionRec;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    RowType: TRowType;

    constructor Create(Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: ThtDocument; T: TCellList);
//    constructor CreateCopy(AMasterList: ThtDocument; Parent: TBlock; T: TCellList);
    procedure Initialize;
    function DrawLogic1(Canvas: TCanvas; const Widths: IntArray; Span, CellSpacing, AHeight, Rows: Integer;
      out Desired: Integer; out Spec, More: boolean): Integer;
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: Integer; var Curs: Integer);
    function Draw(Canvas: TCanvas; MasterList: ThtDocument; const ARect: TRect; const Widths: IntArray;
      X, Y, YOffset, CellSpacing: Integer; Border: boolean; Light, Dark: TColor; MyRow: Integer): Integer;
    procedure Add(CellObj: TCellObj);
    property Items[Index: Integer]: TCellObj read getCellObj; default;
  end;

  // BG, 26.12.2011:
  TRowList = class(TFreeList)
  private
    function GetItem(Index: Integer): TCellList;
  public
    property Items[Index: Integer]: TCellList read GetItem; default;
  end;

  TColSpec = class
  private
    FWidth: TSpecWidth;
    FAlign: ThtString;
    FVAlign: AlignmentType;
  public
    constructor Create(const Width: TSpecWidth; Align: ThtString; VAlign: AlignmentType);
    constructor CreateCopy(const ColSpec: TColSpec);
    property ColWidth: TSpecWidth read FWidth;
    property ColAlign: ThtString read FAlign;
    property ColVAlign: AlignmentType read FVAlign;
  end;

  // BG, 26.12.2011:
  TColSpecList = class(TFreeList)
  private
    function GetItem(Index: Integer): TColSpec;
  public
    property Items[Index: Integer]: TColSpec read GetItem; default;
  end;

  THtmlTable = class;

  TTableBlock = class(TBlock)
  protected
    function getBorderWidth: Integer; override;
    procedure DrawBlockBorder(Canvas: TCanvas; const ORect, IRect: TRect); override;
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

    constructor Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic;
      ATable: THtmlTable; TableAttr: TAttributeList; TableLevel: Integer);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
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
    constructor Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic;
      Attributes: TAttributeList; ATableBlock: TTableBlock);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    procedure CancelUsage;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    property CaptionBlock: TBlock read FCaptionBlock write SetCaptionBlock;
  end;

  THtmlTable = class(TSectionBase)
  private
    TablePartRec: TTablePartRec;
    HeaderHeight, HeaderRowCount, FootHeight, FootStartRow, FootOffset: Integer;
    BodyBreak: Integer;
    HeadOrFoot: Boolean;
    FColSpecs: TColSpecList; // Column width specifications
    // calculated in GetMinMaxWidths
    Percents: IntArray;     {percent widths of columns}
    Multis: IntArray;       {multi widths of columns}
    MaxWidths: IntArray;
    MinWidths: IntArray;
    ColumnCounts: TIntegerPerWidthType;
    ColumnSpecs: TWidthTypeArray;
    //
    procedure IncreaseWidthsByWidth(WidthType: TWidthType; var Widths: IntArray; StartIndex, EndIndex, Required, Spanned, Count: Integer);
    procedure IncreaseWidthsByPercentage(var Widths: IntArray; StartIndex, EndIndex, Required, Spanned, Percent, Count: Integer);
    procedure IncreaseWidthsByMinMaxDelta(WidthType: TWidthType; var Widths: IntArray; StartIndex, EndIndex, Excess, DeltaWidth, Count: Integer; const Deltas: IntArray);
    procedure IncreaseWidthsRelatively(var Widths: IntArray; StartIndex, EndIndex, Required, SpannedMultis: Integer; ExactRelation: Boolean);
    procedure IncreaseWidthsEvenly(WidthType: TWidthType; var Widths: IntArray; StartIndex, EndIndex, Required, Spanned, Count: Integer);
    procedure Initialize; // add dummy cells, initialize cells, prepare arrays
    procedure GetMinMaxWidths(Canvas: TCanvas; TheWidth: Integer);
  public
    Rows: TRowList;        {a list of TCellLists}
    // these fields are copied via Move() in CreateCopy. Don't add reference counted data like strings and arrays.
    Initialized: Boolean;
    //Indent: Integer;        {table indent}
    BorderWidth: Integer;   {width of border}
    brdWidthAttr: Integer;  {Width attribute as entered}
    HasBorderWidth: Boolean; {width of border has been set by attr or prop}
    Float: Boolean;         {if floating}
    NumCols: Integer;       {Number columns in table}
    TableWidth: Integer;    {width of table}
    tblWidthAttr: Integer;  {Width attribute as entered}
    CellPadding: Integer;
    CellSpacing: Integer;
    HSpace, VSpace: Integer; {horizontal, vertical extra space}
    BorderColor: TColor;      //BG, 13.06.2010: added for Issue 5: Table border versus stylesheets
    BorderColorLight: TColor;
    BorderColorDark: TColor;
    EndList: boolean;        {marker for copy}
    // end of Move()d fields
    DrawX: Integer;
    //DrawY: Integer;
    BkGnd: Boolean;
    BkColor: TColor;
    Widths: IntArray;       {holds calculated column widths}
    Heights: IntArray;      {holds calculated row heights}

    constructor Create(Master: ThtDocument; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    destructor Destroy; override;
    procedure DoColumns(Count: Integer; const SpecWidth: TSpecWidth; VAlign: AlignmentType; const Align: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
      out ATitle: ThtString): guResultType; override;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: boolean): Integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean; override;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean; override;
    function FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    procedure CopyToClipboard; override;
    property ColSpecs: TColSpecList read FColSpecs;
    property TableHeight: Integer read SectionHeight write SectionHeight;   {height of table itself, not incl caption}
  end;

//------------------------------------------------------------------------------
// TChPosObj, a pseudo object for ID attributes.
//------------------------------------------------------------------------------
// It is a general purpose ID marker, that finds its position by byte
// position in the document buffer. This object is deprecated.
// The corresponding tag object has to be added to the IDNameList instead.
//------------------------------------------------------------------------------

  // deprecated
  TChPosObj = class(TIDObject)
  private
    FDocument: ThtDocument;
    FChPos: Integer;
  protected
    function GetYPosition: Integer; override;
    function FreeMe: Boolean; override;
  public
    constructor Create(MasterList: ThtDocument; Pos: Integer);
    property ChPos: Integer read FChPos write FChPos;
    property MasterList: ThtDocument read FDocument;
  end;

//------------------------------------------------------------------------------
// ThtDocument, a complete html document, that can draw itself on a canvas.
//------------------------------------------------------------------------------

  TLinkDrawnEvent = procedure(Sender: TObject; Page: Integer; const Url, Target: ThtString; ARect: TRect) of object;
  TFileBrowseEvent = procedure(Sender, Obj: TObject; var S: ThtString) of object;
  TGetBitmapEvent = procedure(Sender: TObject; const SRC: ThtString; var Bitmap: TBitmap; var Color: TColor) of object;
  TGetImageEvent = procedure(Sender: TObject; const SRC: ThtString; var Stream: TStream) of object;
  TGottenImageEvent = TGetImageEvent;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList) of object;
  TObjectTagEvent = procedure(Sender: TObject; Panel: ThvPanel; const Attributes, Params: ThtStringList; var WantPanel: boolean) of object;
  TObjectClickEvent = procedure(Sender, Obj: TObject; const OnClick: ThtString) of object;
  ThtObjectEvent = procedure(Sender, Obj: TObject; const Attribute: ThtString) of object;
  TExpandNameEvent = procedure(Sender: TObject; const SRC: ThtString; var Result: ThtString) of object;

  THtmlStyleList = class(TStyleList) {a list of all the styles -- the stylesheet}
  private
    MasterList: ThtDocument;
  protected
    procedure setLinksActive(Value: Boolean); override;
  public
    constructor Create(AMasterList: ThtDocument);
  end;

  ThtDocument = class(TCell) {a list of all the sections -- the html document}
  private
    FUseQuirksMode : Boolean;
    FPropStack: THtmlPropStack;
    procedure AdjustFormControls;
    procedure AddSectionsToPositionList(Sections: TSectionBase);
    function CopyToBuffer(Buffer: SelTextCount): Integer;
  protected
    CB: SelTextCount;
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
    PageBottom: Integer;
    PageShortened: boolean;
    MapList: TFreeList; {holds list of client maps, TMapItems}
    Timer: TTimer; {for animated GIFs}
    FormControlList: TFormControlObjList; {List of all TFormControlObj's in this SectionList}
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
    IDNameList: TIDObjectList;
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
    FNoBreak : Boolean;
    constructor Create(Owner: THtmlViewerBase; APaintPanel: TWinControl);
    constructor CreateCopy(T: ThtDocument);
    destructor Destroy; override;
    function AddChPosObjectToIDNameList(const S: ThtString; Pos: Integer): Integer; {$ifdef UseInline} inline; {$endif}
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean; override;
    function DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer; Y, XRef, YRef: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSectionAtPosition(Pos: Integer; out TopPos, Index: Integer): TSectionBase;
    function GetFormcontrolData: TFreeList;
    function GetSelLength: Integer;
    function GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
    function GetTheImage(const BMName: ThtString;
      var Transparent: Transparency; out AMask: TBitmap; out FromCache, Delay: boolean): TgpObject;
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): guResultType; override;
    procedure CancelActives;
    procedure CheckGIFList(Sender: TObject);
    procedure Clear; override;
    procedure ClearLists;
    procedure CopyToClipboardA(Leng: Integer);
    procedure GetBackgroundBitmap;
    procedure HideControls;
    procedure InsertImage(const Src: ThtString; Stream: TStream; out Reformat: boolean);
    procedure LButtonDown(Down: boolean);
    procedure ProcessInlines(SIndex: Integer; Prop: TProperties; Start: boolean);
    procedure SetBackground(ABackground: TColor);
    procedure SetBackgroundBitmap(const Name: ThtString; const APrec: PtPositionRec);
    procedure SetFormcontrolData(T: TFreeList);
    procedure SetYOffset(Y: Integer);
    procedure SetFonts(const Name, PreName: ThtString; ASize: Integer;
      AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
      LnksActive, LinkUnderLine: boolean; ACharSet: TFontCharSet;
      MarginHeight, MarginWidth: Integer);
    property UseQuirksMode : Boolean read FUseQuirksMode write FUseQuirksMode;
    property PropStack : THtmlPropStack read FPropStack write FPropStack;

    property NoBreak : Boolean read FNoBreak write FNoBreak;  {set when in <NoBr>}

    property ImageCache: TStringBitmapList read BitmapList;
  end;

//------------------------------------------------------------------------------
// some more base sections
//------------------------------------------------------------------------------

  TPage = class(TSectionBase)
  public
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  THorzLine = class(TSectionBase) {a horizontal line, <hr>}
    VSize: Integer;
    Color: TColor;
    Align: JustifyType;
    NoShade: boolean;
    BkGnd: boolean;
    Width, Indent: Integer;

    constructor Create(AMasterList: ThtDocument; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: ThtDocument; T: TSectionBase); override;
    procedure CopyToClipboard; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  TPreFormated = class(TSection)
  {section for preformated, <pre>}
  public
    procedure ProcessText(TagIndex: Integer); override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
  end;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

  THtmlPropStack = class(TPropStack)
  public
    MasterList: ThtDocument;
    SIndex: Integer; //BG, 26.12.2010: seems, this is the current position in the original html-file.
    procedure PopAProp(const Tag: ThtString);
    procedure PopProp;
    procedure PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProps: TProperties);
  end;

function htCompareText(const T1, T2: ThtString): Integer; {$ifdef UseInline} inline; {$endif}

var
  CurrentStyle: TFontStyles; {as set by <b>, <i>, etc.}
  CurrentForm: ThtmlForm;
{$ifdef UNICODE}
{$else}
  UnicodeControls: boolean;
{$endif}

//var
  // TODO: must be part of the THtmlParser or the ThtDocument.
  //PropStack: THtmlPropStack;
  //Title: ThtString;
  //Base: ThtString;
  //BaseTarget: ThtString;
  //NoBreak: boolean; {set when in <NoBr>}

implementation

uses
   {$IFDEF JPM_DEBUGGING}
 CodeSiteLogging,
   {$ENDIF}
{$IFNDEF NoGDIPlus}
  GDIPL2A,
{$ENDIF}
{$IFNDEF NoTabLink}
  HtmlView,
{$endif}
  HtmlSbs1;


//-- BG ---------------------------------------------------------- 14.01.2012 --
function Sum(const Arr: IntArray; StartIndex, EndIndex: Integer): Integer; overload;
// Return sum of array elements from StartIndex to EndIndex.
var
  I: Integer;
begin
  Result := 0;
  for I := StartIndex to EndIndex do
    Inc(Result, Arr[I]);
end;

//-- BG ---------------------------------------------------------- 14.01.2012 --
function Sum(const Arr: IntArray): Integer; overload;
 {$ifdef UseInline} inline; {$endif}
// Return sum of all array elements.
begin
  Result := Sum(Arr, Low(Arr), High(Arr));
end;

//-- BG ---------------------------------------------------------- 14.01.2012 --
function Sum(const Arr: TIntegerPerWidthType; StartIndex, EndIndex: TWidthType): Integer; overload;
 {$ifdef UseInline} inline; {$endif}
// Return sum of array elements from StartIndex to EndIndex.
var
  I: TWidthType;
begin
  Result := 0;
  for I := StartIndex to EndIndex do
    Inc(Result, Arr[I]);
end;

//-- BG ---------------------------------------------------------- 14.01.2012 --
function Sum(const Arr: TIntegerPerWidthType): Integer; overload;
 {$ifdef UseInline} inline; {$endif}
// Return sum of all array elements.
begin
  Result := Sum(Arr, Low(Arr), High(Arr));
end;

//-- BG ---------------------------------------------------------- 17.01.2012 --
function SubArray(const Arr, Minus: IntArray): IntArray; overload;
 {$ifdef UseInline} inline; {$endif}
// Return array with differences per index.
var
  I: Integer;
begin
  Result := Copy(Arr);
  for I := 0 to Min(High(Result), High(Minus)) do
    Dec(Result[I], Minus[I]);
end;

//-- BG ---------------------------------------------------------- 16.01.2012 --
procedure SetArray(var Arr: IntArray; Value, StartIndex, EndIndex: Integer); overload;
 {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  for I := StartIndex to EndIndex do
    Arr[I] := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2012 --
procedure SetArray(var Arr: IntArray; Value: Integer); overload;
 {$ifdef UseInline} inline; {$endif}
begin
  SetArray(Arr, Value, Low(Arr), High(Arr));
end;

//-- BG ---------------------------------------------------------- 16.01.2012 --
procedure SetArray(var Arr: TIntegerPerWidthType; Value: Integer); overload;
 {$ifdef UseInline} inline; {$endif}
var
  I: TWidthType;
begin
  for I := Low(TWidthType) to High(TWidthType) do
    Arr[I] := Value;
end;

//-- BG ---------------------------------------------------------- 18.01.2012 --
procedure CountsPerType(
  var CountsPerType: TIntegerPerWidthType;
  const ColumnSpecs: TWidthTypeArray;
  StartIndex, EndIndex: Integer);
 {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  SetArray(CountsPerType, 0);
  for I := StartIndex to EndIndex do
    Inc(CountsPerType[ColumnSpecs[I]]);
end;

//-- BG ---------------------------------------------------------- 19.01.2012 --
function SumOfType(
  WidthType: TWidthType;
  const ColumnSpecs: TWidthTypeArray;
  const Widths: IntArray;
  StartIndex, EndIndex: Integer): Integer;
  {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  Result := 0;
  for I := StartIndex to EndIndex do
    if ColumnSpecs[I] = WidthType then
      Inc(Result, Widths[I]);
end;

//-- BG ---------------------------------------------------------- 17.06.2012 --
function SumOfNotType(
  WidthType: TWidthType;
  const ColumnSpecs: TWidthTypeArray;
  const Widths: IntArray;
  StartIndex, EndIndex: Integer): Integer;
  {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  Result := 0;
  for I := StartIndex to EndIndex do
    if ColumnSpecs[I] <> WidthType then
      Inc(Result, Widths[I]);
end;

//-- BG ---------------------------------------------------------- 10.12.2010 --
function htCompareText(const T1, T2: ThtString): Integer;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := WideCompareText(T1, T2);
end;

procedure InitializeFontSizes(Size: Integer);
   {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  for I := 1 to 7 do
  begin
    FontConv[I] := FontConvBase[I] * Size / 12.0;
    PreFontConv[I] := PreFontConvBase[I] * Size / 12.0;
  end;
end;

type
  TSectionClass = class of TSectionBase;
  EProcessError = class(Exception);

type
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
    Owner: ThtDocument;
    procedure AdjustValues;
    function GetStartB(I: Integer): Integer;
    function GetEndB(I: Integer): Integer;
  public
    constructor Create(AnOwner: ThtDocument);
    procedure Clear; override;
    property StartB[I: Integer]: Integer read GetStartB;
    property EndB[I: Integer]: Integer read GetEndB;
  end;

constructor TFontObj.Create(ASection: TSection; F: TMyFont; Position: Integer);
begin
  inherited Create;
{$ifndef NoTabLink}
  FSection := ASection;
{$endif}
  TheFont := F;
  Pos := Position;
  UrlTarget := TUrlTarget.Create;
  FontChanged;
end;

{$ifndef NoTabLink}

procedure TFontObj.EnterEvent(Sender: TObject);
var
  List: TList;
  I, J: Integer;
begin
  Active := True;
{Make adjacent fonts in this link active also}
  List := FSection.Document.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I + 1 to List.Count - 1 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        TFontObj(List[J]).Active := True
      else
        Break;
  FSection.Document.ControlEnterEvent(Self);
end;

procedure TFontObj.ExitEvent(Sender: TObject);
var
  List: TList;
  I, J: Integer;
begin
  Active := False;
{Make adjacent fonts in this link inactive also}
  List := FSection.Document.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I + 1 to List.Count - 1 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        TFontObj(List[J]).Active := False
      else
        Break;
  FSection.Document.PPanel.Invalidate;
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
    List := FSection.Document.LinkList;
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
  Viewer := THtmlViewer(FSection.Document.TheOwner);
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
  List := FSection.Document.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I - 1 downto 0 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        if Assigned(TFontObj(List[J]).TabControl) then
          Exit;

  PntPanel := FSection.Document.PPanel;
  TabControl := ThtTabcontrol.Create(PntPanel);
  TabControl.Left := -4000; {so will be invisible until placed}
  TabControl.Width := 1;
  TabControl.Height := 1;
  TabControl.TabStop := True;
  TabControl.OnEnter := EnterEvent;
  TabControl.OnExit := ExitEvent;
  TabControl.OnKeyDown := AKeyDown;
  TabControl.Parent := PntPanel;

  if TabIndex > 0 then
  {Adding leading 0's to the number ThtString allows it to be sorted numerically,
   and the Count takes care of duplicates}
    with FSection.Document.TabOrderList do
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
  FSection := ASection;
  Pos := T.Pos;
  SScript := T.SScript;
  TheFont := TMyFont.Create;
  TheFont.Assign(T.TheFont);
  if Assigned(T.FIArray) then
    ConvertFont(T.FIArray.Ar[LFont]);
  UrlTarget := TUrlTarget.Create;
  UrlTarget.Assign(T.UrlTarget);
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
    ConvertFont(FIArray.Ar[FontInfoIndex]);
    FontChanged;
  end;
end;

procedure TFontObj.SetHover(Value: boolean);
begin
  if Value <> FHover then
  begin
    FHover := Value;
    ConvertFont(FIArray.Ar[FontInfoIndex]);
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
    //ShowMessage('Bad TFontObj, htmlsubs.pas, TFontObj.GetUrl');
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

//-- BG ---------------------------------------------------------- 17.06.2012 --
function TFontObj.GetFontInfoIndex: FIIndex;
begin
  if Visited then
    if Hover then
      Result := HVFont
    else
      Result := VFont
  else
    if Hover then
      Result := HLFont
    else
      Result := LFont;
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

function TFontList.GetFontAt(Posn: Integer; out OHang: Integer): TMyFont;
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

function TFontList.GetFontObjAt(Posn: Integer; out Index: Integer): TFontObj;
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

procedure TFontList.Decrement(N: Integer; Document: ThtDocument);
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
      J := Document.LinkList.IndexOf(FO1);
      if J >= 0 then
        Document.LinkList.Delete(J);
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
          J := Document.TabOrderList.IndexOfObject(FO1.TabControl);
          if J >= 0 then
            Document.TabOrderList.Delete(J);
        end;
{$ENDIF}
      Delete(I - 1);
    end
    else
      Inc(I);
  end;
end;

{----------------TImageObj.Create}

constructor TImageObj.Create(MasterList: ThtDocument; Position: Integer; L: TAttributeList);
var
  I: Integer;
  S: ThtString;
  NewSpace: Integer;
  T: TAttribute;
begin
  inherited Create;
  Document := MasterList;
  Pos := Position;
  VertAlign := ABottom; {default}
  Floating := ANone; {default}
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
              Floating := ALeft;
            end
            else if S = 'RIGHT' then
            begin
              VertAlign := ANone;
              Floating := ARight;
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
          Document.IDNameList.AddObject(Name, Self);
      end;
  if L.Find(TitleSy, T) then
    ImageTitle := T.Name; {has higher priority than Alt loaded above}
  if L.TheID <> '' then
    Document.IDNameList.AddObject(L.TheID, Self);

  if NewSpace >= 0 then
    HSpaceL := NewSpace
  else
    if Self.Document.UseQuirksMode then begin
      case Floating of
       ARight :
          begin
            HSpaceR := 0;
            HSpaceL := ImageSpace;
          end;
       ALeft :
          begin
            HSpaceR := ImageSpace;
            HSpaceL := 0;
          end
        else
          HSpaceR := 0;
          HSpaceL := 0;
        end;
    end else begin
      HSpaceR := 0;
      VSpaceB := 0;
    end;
end;

constructor TImageObj.SimpleCreate(MasterList: ThtDocument; const AnURL: ThtString);
begin
  inherited Create;
  Document := MasterList;
  VertAlign := ABottom; {default}
  Floating := ANone; {default}
  Source := AnURL;
  NoBorder := True;
  BorderSize := 0;
  SpecHeight := -1;
  SpecWidth := -1;
end;

constructor TImageObj.CreateCopy(AMasterList: ThtDocument; T: TImageObj);
begin
  inherited CreateCopy(T);
  Document := AMasterList;
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
  if not Document.IsCopy then
  begin
    if (Source <> '') and Assigned(OrigImage) then
      Document.BitmapList.DecUsage(Source);
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
  if Image is TGifImage then
    Result := TGifImage(Image).Bitmap
  else if (Image is TBitmap) {$IFNDEF NoGDIPlus}or (Image is TGpBitmap){$ENDIF} then
  begin
    if FBitmap = nil then
    begin
      if Image is TBitmap then
      begin
        FBitmap := TBitmap.Create;
        FBitmap.Assign(TBitmap(Image));
        if ColorBits = 8 then
          FBitmap.Palette := CopyPalette(ThePalette);
      end
{$IFNDEF NoGDIPlus}
      else {it's a TGpBitmap}
        FBitmap := TGpBitmap(Image).GetBitmap
{$ENDIF NoGDIPlus}
        ;
    end;
    Result := FBitmap;
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
            Document.AGifList.Add(Image);
          end;
        end
      else
      begin
        Animate := False;
        if NumFrames <= 3 then
          CurrentFrame := 1;
        Document.AGifList.Remove(Image);
      end;
      FHover := Value;
      Document.PPanel.Invalidate;
    end;
end;

procedure TImageObj.setImage(const AValue: TgpObject);
begin
  if FImage = AValue then exit;
  FImage := AValue;
end;

{----------------TImageObj.ReplaceImage}

procedure TImageObj.ReplaceImage(NewImage: TStream);
var
  TmpImage: TGpObject;
  AMask: TBitmap;
  Stream: TMemoryStream;
  I: Integer;
begin
  try
    Transparent := NotTransp;
    AMask := nil;
    if NewImage is TMemoryStream then
      TmpImage := LoadImageFromStream(NewImage, Transparent, AMask)
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

    // terminated old image
    if not Swapped then
    begin
    {OrigImage is left in cache and kept}
      if Image is TGifImage then
        Document.AGifList.Remove(Image);
      Swapped := True;
    end
    else {swapped already}
    begin
      if Image is TGifImage then
        Document.AGifList.Remove(Image);
      Image.Free;
      FreeAndNil(Mask);
    end;
    FreeAndNil(FBitmap);

    // initialize new image
    Image := TmpImage;
    if Image is TGifImage then
      if not FHoverImage then
      begin
        TGifImage(Image).Animate := True;
        Document.AGifList.Add(Image);
      end
      else
      begin
        TGifImage(Image).Animate := False;
        SetHover(hvOff);
      end;
    Mask := AMask;

    if Missing then
    begin
      // image is not missing any more
      with Document.MissingImages do
        for I := 0 to count - 1 do
          if Objects[I] = Self then
          begin
            Delete(I);
            break;
          end;
      Missing := False;
    end;

    Document.PPanel.Invalidate;
  except
    // replacing image failed
  end;
end;

{----------------TImageObj.InsertImage}

function TImageObj.InsertImage(const UName: ThtString; Error: boolean; out Reformat: boolean): boolean;
var
  TmpImage: TgpObject;
  FromCache, DelayDummy: Boolean;
begin
  Result := False;
  Reformat := False;
  if Image = DefBitmap then
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
      TmpImage := Document.GetTheImage(UName, Transparent, Mask, FromCache, DelayDummy);
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
          Document.AGifList.Add(Image);
          TGifImage(Image).Animate := True;
          if Assigned(Document.Timer) then
            Document.Timer.Enabled := True;
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

procedure TImageObj.DrawLogic(SectionList: ThtDocument; Canvas: TCanvas;
  FO: TFontObj; AvailableWidth, AvailableHeight: Integer);
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
  ViewImages := Document.ShowImages;

  TmpImage := Image;
  if ViewImages and not Assigned(TmpImage) then
  begin
    if Source <> '' then
      with SectionList do
      begin
        if not Assigned(GetBitmap) and not Assigned(GetImage) then
          Source := TheOwner.HtmlExpandFilename(Source)
        else if Assigned(ExpandName) then
        begin
          Rslt := '';
          ExpandName(TheOwner, Source, Rslt);
          Source := Rslt;
        end;
        if MissingImages.IndexOf(Uppercase(Source)) = -1 then
          TmpImage := Document.GetTheImage(Source, Transparent, Mask, FromCache, Missing)
        else
          Missing := True; {already in list, don't request it again}
      end;
    if not Assigned(TmpImage) then
    begin
      if Missing then
      begin
        Image := DefBitmap;
        TmpImage := DefBitmap;
        Document.MissingImages.AddObject(Source, Self); {add it even if it's there already}
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
        Document.AGifList.Add(Image)
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
    else if FAlt <> '' then {Alt text and no size specified, take as much space as necessary}
    begin
      Canvas.Font.Name := 'Arial'; {use same font as in Draw}
      Canvas.Font.Size := 8;
      ARect := Rect(0, 0, 0, 0);
      DrawTextW(Canvas.Handle, PWideChar(FAlt + CRLF), -1, ARect, DT_CALCRECT);
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

procedure TImageObj.DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: TgpObject; ddMask: TBitmap);
{Y relative to top of display here}
var
  DC: HDC;
  Img: TBitmap absolute ddImage;
  Gif: TGifImage absolute ddImage;
  W, H: Integer;
  PrintTransparent: boolean;
begin
  DC := Canvas.Handle;

  {$IFNDEF NoGDIPlus}
  if TObject(ddImage) is TGPImage then
  begin
    if not Document.Printing then
      StretchDrawGpImage(DC, TGPImage(ddImage), XX, Y, ObjWidth, ObjHeight)
    else if not Document.PrintBackground and (Positioning = posStatic) then {Printing}
      StretchPrintGpImageOnColor(Canvas, TGpImage(ddImage), XX, Y, ObjWidth, ObjHeight)
    else
      StretchPrintGpImageDirect(DC, TGPImage(ddImage), XX, Y, ObjWidth, ObjHeight,
        Document.ScaleX, Document.ScaleY);
    Exit;
  end;
  {$ENDIF !NoGDIPlus}

  try
    if not Document.IsCopy then
    begin
      if ddImage is TGifImage then
      begin
        Gif.ShowIt := True;
        Gif.Visible := True;
        Gif.Draw(Canvas, XX, Y, ObjWidth, ObjHeight);
      end
      else if ((Transparent <> NotTransp) or (ddImage = ErrorBitmap)) and Assigned(ddMask) then
        if ddImage = ErrorBitmap then
          FinishTransparentBitmap(DC, Img, Mask, XX, Y, Img.Width, Img.Height)
        else
          FinishTransparentBitmap(DC, Img, Mask, XX, Y, ObjWidth, ObjHeight)
      else
      begin
        if (ddImage = DefBitMap) or (ddImage = ErrorBitmap) then
          BitBlt(DC, XX, Y, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0, SRCCOPY)
        else if ddImage is TBitmap then
        begin
{$IFDEF HalfToneStretching}
          SetStretchBltMode(DC, HALFTONE);
{$ELSE}
          SetStretchBltMode(DC, COLORONCOLOR);
{$ENDIF}
          SetBrushOrgEx(DC, 0, 0, nil);
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
      begin
        ddMask := Gif.Mask;
        if Assigned(ddMask) then
          Transparent := TrGif;
        Img := Gif.MaskedBitmap;
        Img.Palette := CopyPalette(ThePalette);
        Img.HandleType := bmDIB;
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
procedure GetRaisedColors(SectionList: ThtDocument; Canvas: TCanvas; out Light, Dark: TColor);  {$ifdef UseInline} inline; {$endif}
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
    Dark := ThemedColor(clBtnShadow);
    if White then
      Light := clSilver
    else
      Light := ThemedColor(clBtnHighLight);
  end;
end;

//BG, 15.10.2010: issue 28: Borland C++ Builder does not accept an array as a result of a function.
// Thus move htStyles and htColors from HtmlUn2.pas to HtmlSubs.pas the only unit where they are used

function htStyles(P0, P1, P2, P3: BorderStyleType): htBorderStyleArray;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[0] := P0;
  Result[1] := P1;
  Result[2] := P2;
  Result[3] := P3;
end;

function htColors(C0, C1, C2, C3: TColor): htColorArray;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[0] := C0;
  Result[1] := C1;
  Result[2] := C2;
  Result[3] := C3;
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
function htRaisedColors(Light, Dark: TColor; Raised: Boolean): htColorArray; overload;
  {$ifdef UseInline} inline; {$endif}
begin
  if Raised then
    Result := htColors(Light, Light, Dark, Dark)
  else
    Result := htColors(Dark, Dark, Light, Light);
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
function htRaisedColors(SectionList: ThtDocument; Canvas: TCanvas; Raised: Boolean): htColorArray; overload;
  {$ifdef UseInline} inline; {$endif}
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
  {$ifdef UseInline} inline; {$endif}
{Draws colored raised or lowered rectangles for table borders}
begin
  DrawBorder(Canvas, ORect, IRect, Colors, Styles, clNone, False);
end;

procedure RaisedRect(SectionList: ThtDocument; Canvas: TCanvas;
  X1, Y1, X2, Y2: Integer;
  Raised: boolean;
  W: Integer);
  {$ifdef UseInline} inline; {$endif}
{Draws raised or lowered rectangles for table borders}
begin
  RaisedRectColor(Canvas,
    Rect(X1, Y1, X2, Y2),
    Rect(X1 + W, Y1 + W, X2 - W, Y2 - W),
    htRaisedColors(SectionList, Canvas, Raised),
    htStyles(bssSolid, bssSolid, bssSolid, bssSolid));
end;

procedure TImageObj.Draw(Canvas: TCanvas; X: Integer; TopY, YBaseline: Integer; FO: TFontObj);
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
  with Document do
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
  end;

  if SubstImage then
    Ofst := 4
  else
    Ofst := 0;
  if VertAlign = AMiddle then
    MiddleAlignTop := YBaseLine + FO.Descent - (FO.tmHeight div 2) - ((ImageHeight - VSpaceT + VSpaceB) div 2)
  else
    MiddleAlignTop := 0; {not used}

  if Floating = ANone then
  begin
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
  end
  else
  begin
    DrawXX := X;
    DrawYY := TopY;
  end;

  if not SubstImage or (AltHeight >= 16 + 8) and (AltWidth >= 16 + 8) then
    Self.DoDraw(Canvas, DrawXX + Ofst, DrawYY + Ofst, TmpImage, TmpMask);
  Inc(DrawYY, Document.YOff);
  SetTextAlign(Canvas.Handle, TA_Top);
  if SubstImage and (BorderSize = 0) then
  begin
    Canvas.Font.Color := ThemedColor(FO.TheFont.Color);
  {calc the offset from the image's base to the alt= text baseline}
    case VertAlign of
      ATop, ANone:
        begin
          if FAlt <> '' then
            WrapTextW(Canvas, X + 24, TopY + Ofst, X + AltWidth - 2, TopY + AltHeight - 1, FAlt);
          RaisedRect(Document, Canvas, X, TopY, X + AltWidth, TopY + AltHeight, False, 1);
        end;
      AMiddle:
        begin {MiddleAlignTop is always initialized}
          if FAlt <> '' then
            WrapTextW(Canvas, X + 24, MiddleAlignTop + Ofst, X + AltWidth - 2, MiddleAlignTop + AltHeight - 1, FAlt);
          RaisedRect(Document, Canvas, X, MiddleAlignTop, X + AltWidth, MiddleAlignTop + AltHeight, False, 1);
        end;
      ABottom, ABaseline:
        begin
          if FAlt <> '' then
            WrapTextW(Canvas, X + 24, YBaseLine - AltHeight + Ofst - VSpaceB, X + AltWidth - 2, YBaseLine - VSpaceB - 1, FAlt);
          RaisedRect(Document, Canvas, X, YBaseLine - AltHeight - VSpaceB, X + AltWidth, YBaseLine - VSpaceB, False, 1);
        end;
    end;
  end;

  if BorderSize > 0 then
    with Canvas do
    begin
      SaveColor := Pen.Color;
      SaveWidth := Pen.Width;
      SaveStyle := Pen.Style;
      Pen.Color := ThemedColor(FO.TheFont.Color);
      Pen.Width := BorderSize;
      Pen.Style := psInsideFrame;
      Font.Color := Pen.Color;
      try
        if (FAlt <> '') and SubstImage then
        begin
          {output Alt message}
          YY := DrawYY - Document.YOff;
          case VertAlign of
            ATop, ANone:
              WrapTextW(Canvas, DrawXX + 24, YY + Ofst, DrawXX + AltWidth - 2, YY + AltHeight - 1, FAlt);
            AMiddle:
              WrapTextW(Canvas, DrawXX + 24, YY + Ofst, DrawXX + AltWidth - 2, YY + AltHeight - 1, FAlt);
            ABottom, ABaseline:
              WrapTextW(Canvas, DrawXX + 24, YY + Ofst, DrawXX + AltWidth - 2, YY + AltHeight - 1, FAlt);
          end;
        end;

        {draw border}
        case VertAlign of

          {ALeft, ARight,} ATop, ANone:
            Rectangle(X, TopY + VSpaceT, X + ImageWidth, TopY + VSpaceT + ImageHeight);
          AMiddle:
            Rectangle(X, MiddleAlignTop, X + ImageWidth, MiddleAlignTop + ImageHeight);
          ABottom, ABaseline:
            Rectangle(X, YBaseLine - ImageHeight - VSpaceB, X + ImageWidth, YBaseLine - VSpaceB);
        end;
      finally
        Pen.Color := SaveColor;
        Pen.Width := SaveWidth;
        Pen.Style := SaveStyle;
      end;
    end;

  if (Assigned(MyFormControl) and MyFormControl.Active or FO.Active) or
    Document.IsCopy and Assigned(Document.LinkDrawnEvent)
    and (FO.UrlTarget.Url <> '')
  then
    with Canvas do
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
      if not Document.IsCopy then
      begin
        if Document.TheOwner.ShowFocusRect then //MK20091107
          Canvas.DrawFocusRect(ARect);  {draw focus box}
      end
      else
        Document.LinkDrawnEvent(Document.TheOwner, Document.LinkPage,
          FO.UrlTarget.Url, FO.UrlTarget.Target, ARect);
      SetTextColor(handle, SaveColor);
    end;
end;

{----------------TFloatingObjList.CreateCopy}

constructor TFloatingObjList.CreateCopy(AMasterList: ThtDocument; T: TFloatingObjList);
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

function TFloatingObjList.FindImage(Posn: Integer): TFloatingObj;
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

function TFloatingObjList.GetHeightAt(Posn: Integer; out AAlign: AlignmentType;
  out FlObj: TFloatingObj): Integer;
begin
  FLObj := FindImage(Posn);
  if Assigned(FLObj) then
  begin
    Result := FLObj.ImageHeight + FLObj.VSpaceT + FLObj.VSpaceB;
    AAlign := FLObj.VertAlign
  end
  else
    Result := -1;
end;

function TFloatingObjList.GetWidthAt(Posn: Integer; out AAlign: AlignmentType;
  out HSpcL, HSpcR: Integer; out FlObj: TFloatingObj): Integer;
begin
  FLObj := FindImage(Posn);
  if Assigned(FLObj) then
  begin
    Result := FLObj.ImageWidth;
    AAlign := FLObj.Floating;
    HSpcL := FLObj.HSpaceL;
    HSpcR := FLObj.HSpaceR;
  end
  else
    Result := -1;
end;

function TFloatingObjList.GetImageCountAt(Posn: Integer): Integer;
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

{----------------TFloatingObjList.Decrement}

procedure TFloatingObjList.Decrement(N: Integer);
{called when a character is removed to change the Position figure}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with TImageObj(Items[I]) do
      if Pos > N then
        Dec(Pos);
end;

{----------------TFloatingObjList.PtInImage}

function TFloatingObjList.PtInImage(X, Y: Integer; out IX, IY, Posn: Integer;
  out AMap, UMap: boolean; out MapItem: TMapItem; out ImageObj: TImageObj): boolean;
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
            with Document.MapList do
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

function TFloatingObjList.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;
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

constructor ThtmlForm.Create(AMasterList: ThtDocument; L: TAttributeList);
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
  ControlList := TFormControlObjList.Create(True);
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
  RadioButton: TRadioButtonFormControlObj absolute Ctrl;
  I: Integer;
begin
  if Radio.FName <> '' then
  begin
    S := Radio.FName;
    for I := 0 to ControlList.Count - 1 do
    begin
      Ctrl := ControlList[I];
      if (Ctrl is TRadioButtonFormControlObj) and (Ctrl <> Radio) then
        if CompareText(RadioButton.FName, S) = 0 then
        begin
          RadioButton.Checked := False;
          RadioButton.TabStop := False; {first check turns off other tabstops}
          RadioButton.DoOnChange;
        end;
    end;
  end;
end;

procedure ThtmlForm.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  S: ThtString;
  Ctrl: TFormControlObj;
  I: Integer;
  B: Boolean;
begin
  if (Key in [vk_up, vk_down, vk_left, vk_right]) and (Sender is TFormRadioButton) then
  begin
    S := TFormRadioButton(Sender).IDName;
    B := False;
    if Key in [vk_up, vk_left] then
      for I := ControlList.Count - 1 downto 0 do
      begin
        Ctrl := ControlList[I];
        if (Ctrl is TRadioButtonFormControlObj) and SameText(Ctrl.FName, S) then
        begin
          if B then
          begin
            ControlList[I].TheControl.SetFocus;
            break;
          end;
          if Ctrl.TheControl = Sender then
            B := True
        end;
      end
    else
      for I := 0 to ControlList.Count - 1 do
      begin
        Ctrl := ControlList[I];
        if (Ctrl is TRadioButtonFormControlObj) and SameText(Ctrl.FName, S) then
        begin
          if B then
          begin
            ControlList[I].TheControl.SetFocus;
            break;
          end;
          if Ctrl.TheControl = Sender then
            B := True
        end;
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

constructor TFormControlObj.Create(AMasterList: ThtDocument;
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
    with L[I] do
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
  System.Move(T.Pos, Pos, PtrSub(@ShowIt, @Pos));
  FId := T.FID;
  FName := T.FName;
end;

destructor TFormControlObj.Destroy;
begin
  AttributeList.Free;
  PaintBitmap.Free;
  inherited Destroy;
end;

procedure TFormControlObj.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MasterList.TheOwner.ControlMouseMove(Self, Shift, X, Y);
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TFormControlObj.Hide;
begin
  TheControl.Hide;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.IsHidden: Boolean;
begin
  Result := False;
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
  PercentWidth := (VarIsStr(MargArrayO[piWidth])) and (System.Pos('%', MargArrayO[piWidth]) > 0);
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
  if Prop.HasBorderStyle then
  begin
    Inc(HSpaceL, MargArray[BorderLeftWidth]);
    Inc(HSpaceR, MargArray[BorderRightWidth]);
    BordT := MargArray[BorderTopWidth];
    BordB := MargArray[BorderBottomWidth];
    Inc(VSpaceT, BordT);
    Inc(VSpaceB, BordB);
  end;

  if MargArray[piWidth] > 0 then {excludes IntNull and Auto}
    if PercentWidth then
    begin
      if MargArray[piWidth] <= 100 then
        FWidth := MargArray[piWidth]
      else
        PercentWidth := False;
    end
    else
      FWidth := MargArray[piWidth];
  if MargArray[piHeight] > 0 then
    FHeight := MargArray[piHeight] - BordT - BordB;
  if Prop.GetVertAlign(Align) then
    FormAlign := Align;
  BkColor := Prop.GetBackgroundColor;
end;

procedure TFormControlObj.EnterEvent(Sender: TObject);
{Once form control entered, insure all form controls are tab active}
begin
  if MasterList.IsCopy then
    Exit;
  Active := True;
  MasterList.PPanel.Invalidate;
  MasterList.ControlEnterEvent(Self);
{$IFNDEF FastRadio}
  MasterList.FormControlList.ActivateTabbing;
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

procedure TFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
begin end;

procedure TFormControlObj.ResetToValue;
begin end;

function TFormControlObj.GetSubmission(Index: Integer; out S: ThtString): boolean;
begin
  Result := False;
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

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetHeight: Integer;
begin
  Result := TheControl.Height;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetLeft: Integer;
begin
  Result := TheControl.Left;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TFormControlObj.GetTabOrder: Integer;
begin
  Result := TheControl.TabOrder
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TFormControlObj.GetTabStop: Boolean;
begin
  Result := TheControl.TabStop
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetTop: Integer;
begin
  Result := TheControl.Top;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetWidth: Integer;
begin
  Result := TheControl.Width;
end;

procedure TFormControlObj.SetDataInit;
begin
end;

procedure TFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.SetHeight(Value: Integer);
begin
  TheControl.Height := Value;
end;

procedure TFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.SetLeft(Value: Integer);
begin
  TheControl.Left := Value;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TFormControlObj.SetTabOrder(Value: Integer);
begin
  TheControl.TabOrder := Value;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TFormControlObj.SetTabStop(Value: Boolean);
begin
  TheControl.TabStop := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.SetTop(Value: Integer);
begin
  TheControl.Top := Value;
end;

{----------------TImageFormControlObj.Create}

constructor TImageFormControlObj.Create(AMasterList: ThtDocument;
  Position: Integer; L: TAttributeList);
var
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(AMasterList, Position, L);
  XPos := -1; {so a button press won't submit image data}

  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := ThtButton.Create(PntPanel);
  with FControl do
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

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TImageFormControlObj.Destroy;
begin
  FControl.Parent := nil;
  FControl.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TImageFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TImageFormControlObj.GetSubmission(Index: Integer; out S: ThtString): boolean;
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

//-- BG ---------------------------------------------------------- 15.01.2011 --
function THiddenFormControlObj.GetControl: TWinControl;
begin
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetHeight: Integer;
begin
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetLeft: Integer;
begin
  Result := -4000;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetSubmission(Index: Integer; out S: ThtString): boolean;
begin
  Result := Index = 0;
  if Result then
    S := FName + '=' + Value;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function THiddenFormControlObj.GetTabOrder: Integer;
begin
  Result := -1;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function THiddenFormControlObj.GetTabStop: Boolean;
begin
  Result := False;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetTop: Integer;
begin
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetWidth: Integer;
begin
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.Hide;
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.IsHidden: Boolean;
begin
  Result := True;
end;

procedure THiddenFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  Value := V;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.SetHeight(Value: Integer);
begin
// do nothing
end;

procedure THiddenFormControlObj.SetLeft(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure THiddenFormControlObj.SetTabOrder(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure THiddenFormControlObj.SetTabStop(Value: Boolean);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.SetTop(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.SetWidth(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.Show;
begin
// do nothing
end;

{----------------TEditFormControlObj.Create}

constructor TEditFormControlObj.Create(AMasterList: ThtDocument;
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
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Width := 120;
    if Prop.HasBorderStyle then
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
    FControl.Color := BkColor;
end;

procedure TEditFormControlObj.ResetToValue;
begin
  Text := Value;
end;

procedure TEditFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  H2, Addon: Integer;
  ARect: TRect;
begin
  if FControl.BorderStyle <> bsNone then
    Addon := 4 {normal 3D border}
  else
    Addon := 2; {inline border, 3D Border removed}
  with FControl do
  begin
    Canvas.Font := Font;
    H2 := Abs(Font.Height);
    if BorderStyle <> bsNone then
      DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, MasterList.PrintMonoBlack, False, Color)
    else
      FillRectWhite(Canvas, X1, Y1, X1 + Width, Y1 + Height, Color);
    SetTextAlign(Canvas.handle, TA_Left);
    SetBkMode(Canvas.Handle, {$ifdef FPC}Lcltype.{$endif}Transparent);
    Canvas.Brush.Style := bsClear;
    ARect := Rect(X1 + Addon, Y1, X1 + Width - (Addon div 2), Y1 + Height);
    ThtCanvas(Canvas).htTextRect(ARect, ARect.Left, Y1 + (Height - H2) div 2 - 1, Text);
  end
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TEditFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TEditFormControlObj.GetSubmission(Index: Integer; out S: ThtString): boolean;
begin
  Result := Index = 0;
  if Result then
    S := FName + '=' + Text;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TEditFormControlObj.getText: ThtString;
begin
  Result := {$ifdef LCL}UTF8Decode{$endif}(FControl.Text);
end;

procedure TEditFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  Text := V;
end;

procedure TEditFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with FControl do
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

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TEditFormControlObj.setText(const Value: ThtString);
begin
  FControl.Text := {$ifdef LCL}Utf8Encode{$endif}(Value);
end;

procedure TEditFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  EnterContents := Text;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TEditFormControlObj.Destroy;
begin
  FControl.Parent := nil;
  FControl.Free;
  inherited;
end;

procedure TEditFormControlObj.DoOnChange;
begin
  if Text <> EnterContents then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TButtonFormControlObj.Create}

constructor TButtonFormControlObj.Create(AMasterList: ThtDocument;
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
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Tmp := Prop.GetFont;
    Font.Assign(Tmp);
    Tmp.Free;
    OnClick := Self.ButtonClick;
    if Which = Browse then
      Caption := 'Browse...'
    else
      Caption := {$ifdef LCL}Utf8Encode{$endif}(Value);
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
  end;
  FControl.Parent := PntPanel;
{$IFDEF UseElPack}
  FControl.Color := clBtnFace;
{$ENDIF}
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TButtonFormControlObj.Destroy;
begin
  FControl.Parent := nil;
  FControl.Free;
  inherited;
end;

procedure TButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  H2: Integer;
  MonoBlack: boolean;
begin
  with FControl do
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
      DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, True, MasterList.PrintMonoBlack, False, clWhite);
      H2 := Canvas.TextHeight('A');
      SetTextAlign(Canvas.handle, TA_Center + TA_Top);
      ThtCanvas(Canvas).htTextRect(Rect(X1, Y1, X1 + Width, Y1 + Height), X1 + (Width div 2), Y1 + (Height - H2) div 2, Value);
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TButtonFormControlObj.GetControl: TWinControl;
begin
  Result := FControl; 
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
    if Assigned(MasterList.FileBrowse) and (MyEdit is TEditFormControlObj) then
    begin
      S := MyEdit.Text;
      MasterList.FileBrowse(MasterList.TheOwner, MyEdit, S);
      MyEdit.Text := S;
    end;
end;

procedure TButtonFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with FControl do
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

constructor TCheckBoxFormControlObj.Create(AMasterList: ThtDocument;
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
  with FControl do
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
  FControl.Checked := IsChecked;
end;

procedure TCheckBoxFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
var
  x, y: Integer;
begin
  with FControl do
  begin
    DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, MasterList.PrintMonoBlack, Disabled, clWhite);
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

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TCheckBoxFormControlObj.GetChecked: Boolean;
begin
  Result := FControl.Checked;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TCheckBoxFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TCheckBoxFormControlObj.GetSubmission(Index: Integer; out S: ThtString): boolean;
begin
  Result := (Index = 0) and FControl.Checked;
  if Result then
    S := FName + '=' + Value;
end;

procedure TCheckBoxFormControlObj.SetDataInit;
begin
  FControl.Checked := False; {not checked unless later data says so}
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TCheckBoxFormControlObj.SetChecked(Value: Boolean);
begin
  FControl.Checked := Value;
end;

procedure TCheckBoxFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  if htCompareText(V, Value) = 0 then
    FControl.Checked := True;
end;

procedure TCheckBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  WasChecked := FControl.Checked;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TCheckBoxFormControlObj.Destroy;
begin
  FControl.Parent := nil;
  FControl.Free;
  inherited;
end;

procedure TCheckBoxFormControlObj.DoOnChange;
begin
  if FControl.Checked <> WasChecked then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TRadioButtonFormControlObj.Create}

constructor TRadioButtonFormControlObj.Create(AMasterList: ThtDocument;
  Position: Integer; L: TAttributeList; ACell: TCellBasic);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
  Ctrl: TFormControlObj;
  RadioButtonFormControlObj: TRadioButtonFormControlObj absolute Ctrl;
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
  with FControl do
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
      if (Ctrl is TRadioButtonFormControlObj) and (RadioButtonFormControlObj.FControl <> FControl) then {skip the current radiobutton}
        if CompareText(Ctrl.Name, Name) = 0 then {same group}
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
            RadioButtonFormControlObj.IsChecked := False;
            RadioButtonFormControlObj.Checked := False;
            RadioButtonFormControlObj.TabStop := False;
          end;
    end;
    if not IsChecked then
      TabStop := SetTabStop;
    Checked := IsChecked; {must precede setting OnClick}
    OnClick := RadioClick;
  end;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TRadioButtonFormControlObj.GetColor: TColor;
begin
  Result := FControl.Color;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
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
  Checked := IsChecked;
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
      Brush.Color := ThemedColor(clBtnFace)
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
      Pen.Color := ThemedColor(clBtnShadow);
    end;
    Arc(X1, Y1, XW, YH, XW, Y1, X1, YH);
    if not MonoBlack then
      Pen.Color := clSilver;
    Arc(X1, Y1, XW, YH, X1, YH, XW, Y1);
    if Checked then
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

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TRadioButtonFormControlObj.getChecked: Boolean;
begin
  Result := FControl.Checked;
end;

function TRadioButtonFormControlObj.GetSubmission(Index: Integer; out S: ThtString): boolean;
begin
  Result := (Index = 0) and Checked;
  if Result then
    S := FName + '=' + Value;
end;

procedure TRadioButtonFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  if htCompareText(V, Value) = 0 then
    Checked := True;
end;

procedure TRadioButtonFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  WasChecked := Checked;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TRadioButtonFormControlObj.SetChecked(Value: Boolean);
begin
  FControl.Checked := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TRadioButtonFormControlObj.SetColor(const Value: TColor);
begin
  FControl.Color := Value;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TRadioButtonFormControlObj.Destroy;
begin
  FControl.Parent := nil;
  FControl.Free;
  inherited;
end;

procedure TRadioButtonFormControlObj.DoOnChange;
begin
  if Checked <> WasChecked then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TCellBasic.Create}

constructor TCellBasic.Create(Master: ThtDocument);
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellBasic.Create');
   {$ENDIF}
  inherited Create;
  FDocument := Master;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TCellBasic.Create');
   {$ENDIF}
end;

{----------------TCellBasic.CreateCopy}

constructor TCellBasic.CreateCopy(AMasterList: ThtDocument; T: TCellBasic);
var
  I: Integer;
  Tmp, Tmp1: TSectionBase;
begin
  inherited Create;
  FDocument := AMasterList;
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
    Item.SetDocument(MasterList);
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

function TCellBasic.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): guResultType;
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

function TCellBasic.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;
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

function TCellBasic.FindCursor(Canvas: TCanvas; X, Y: Integer;
  out XR, YR, Ht: Integer; out Intext: boolean): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    with Items[I] do
    begin
      if (Y >= DrawTop) and (Y < DrawBot) then
      begin
        Result := Items[I].FindCursor(Canvas, X, Y, XR, YR, Ht, InText);
        if Result >= 0 then
          Exit;
      end;
    end;
  end;
  Result := -1;
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

function TCellBasic.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean;
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

function TCellBasic.DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer;
  var ScrollWidth: Integer; var Curs: Integer): Integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  I, Sw, TheCount: Integer;
  H: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellBasic.DoLogic');
  CodeSite.SendFmtMsg('Y = [%d]',[Y]);
  CodeSite.SendFmtMsg('Width = [%d]',[Width]);
  CodeSite.SendFmtMsg('AHeight = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt = [%d]',[BlHt]);
  CodeSite.SendFmtMsg('ScrollWidth = [%d]',[ScrollWidth]);
  CodeSite.SendFmtMsg('Curs = [%d]',[Curs]);
  CodeSite.AddSeparator;
   {$ENDIF}
  YValue := Y;
  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
  begin
    try
      //TODO -oBG, 24.06.2012: merge sections with display=inline etc.  
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('ScrollWidth = [%d]',[ScrollWidth]);
  CodeSite.SendFmtMsg('Curs = [%d]',[Curs]);
   CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TCellBasic.DoLogic');
   {$ENDIF}
end;

{----------------TCellBasic.MinMaxWidth}

procedure TCellBasic.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
{Find the Width the cell would take if no wordwrap, Max, and the width if wrapped
 at largest word, Min}
var
  I, Mn, Mx: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellBasic.MinMaxWidth');
   {$ENDIF}
  Max := 0; Min := 0;
  for I := 0 to Count - 1 do
  begin
    Items[I].MinMaxWidth(Canvas, Mn, Mx);
    Max := Math.Max(Max, Mx);
    Min := Math.Max(Min, Mn);
  end;
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('min = [%d]',[Min]);
   CodeSite.SendFmtMsg('max = [%d]',[Max]);
  CodeSite.ExitMethod(Self,'TCellBasic.MinMaxWidth');
   {$ENDIF}
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

constructor TBlock.Create(Master: ThtDocument; Prop: TProperties;
  AnOwnerCell: TCellBasic; Attributes: TAttributeList);
var
  Clr: ClearAttrType;
  S: ThtString;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.Create');
  StyleUn.LogProperties(Prop,'Prop');
  CodeSite.AddSeparator;
  {$ENDIF}
  inherited Create(Master, Prop);
  OwnerCell := AnOwnerCell;

  MyCell := TBlockCell.Create(Master);
  MyCell.OwnersTag := Prop.PropTag;
  MyCell.FOwner := Self;
  DrawList := TList.Create;

  Prop.GetVMarginArray(MargArrayO, not (Self is TTableBlock));
  if Prop.GetClear(Clr) then
    ClearAttr := Clr;
  if not Prop.GetFloat(FloatLR) then
    FloatLR := ANone;
  HasBorderStyle := Prop.HasBorderStyle;
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
  end;
  if (Positioning in [posAbsolute, posFixed]) or (FloatLR in [ALeft, ARight]) then
  begin
    MyIMgr := TIndentManager.Create;
    MyCell.IMgr := MyIMgr;
  end;
  if (FloatLR in [ALeft, ARight]) and (ZIndex = 0) then
    ZIndex := 1;
  TagClass := Prop.PropTag + '.' + Prop.PropClass + '#' + Prop.PropID;
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TBlock.Create');
  {$ENDIF}
end;

procedure TBlock.CollapseMargins;
{adjacent vertical margins need to be reduced}
var
  TopAuto: boolean;
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
  else if Display = pdNone then {do nothing}
  else
    with OwnerCell do
    begin
      I := OwnerCell.Count - 1; {find the preceding block that isn't absolute positioning}
      while I >= 0 do
      begin
        TB := OwnerCell[I];
        if TB.Display <> pdNone then
          if not (TB is TBlock) or (TBlock(TB).Positioning <> PosAbsolute) then
            break;
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
        TB := OwnerCell[I];
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
procedure TBlock.ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.ContentMinMaxWidth');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);

  CodeSite.AddSeparator;
   {$ENDIF}
  MyCell.MinMaxWidth(Canvas, Min, Max);
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);
  CodeSite.ExitMethod(Self,'TBlock.ContentMinMaxWidth');
   {$ENDIF}
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
procedure TBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer);
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.ConvMargArray');
   CodeSite.SendFmtMsg('BaseWidth       = [%d]',[BaseWidth]);
   CodeSite.SendFmtMsg('BaseHeight      = [%d]',[BaseHeight]);
  CodeSite.SendFmtMsg('Self.EmSize      = [%d]',[EmSize]);
  CodeSite.SendFmtMsg('Self.ExSize      = [%d]',[ExSize]);
  CodeSite.SendFmtMsg('Self.BorderWidth = [%d]',[BorderWidth]);
  CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);
   CodeSite.AddSeparator;
   {$ENDIF}
  StyleUn.ConvMargArray(MargArrayO, BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth, AutoCount, MargArray);

  {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('AutoCount = [%d]',[AutoCount]);
  CodeSite.ExitMethod(Self,'TBlock.ConvMargArray');
  {$ENDIF}
end;

{----------------TBlock.CreateCopy}

constructor TBlock.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
var
  TT: TBlock;
begin
  inherited CreateCopy(AMasterList, T);
  TT := T as TBlock;
  System.Move(TT.MargArray, MargArray, PtrSub(@Converted, @MargArray) + Sizeof(Converted));
  MyCell := TBlockCell.CreateCopy(AMasterList, TT.MyCell);
  MyCell.FOwner := Self;
  DrawList := TList.Create;
  if Assigned(TT.BGImage) and AMasterlist.PrintTableBackground then
    BGImage := TImageObj.CreateCopy(AMasterList, TT.BGImage);
  MargArrayO := TT.MargArrayO;
  if (Positioning in [posAbsolute, posFixed]) or (FloatLR in [ALeft, ARight]) then
    MyCell.IMgr := TIndentManager.Create;
end;

destructor TBlock.Destroy;
begin
  BGImage.Free;
  TiledImage.Free;
  TiledMask.Free;
  FullBG.Free;
  if MyIMgr <> nil then
  begin
    MyCell.IMgr := nil;
    FreeAndNil(MyIMgr);
  end;
  FreeAndNil(MyCell);
  DrawList.Free;
  inherited;
end;

procedure TBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
var
  MinCell, MaxCell: Integer;
  LeftSide, RightSide, AutoCount: Integer;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.MinMaxWidth');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);

  CodeSite.AddSeparator;
  {$ENDIF}
  if (Display = pdNone) or (Positioning = PosAbsolute) then
  begin
    Min := 0;
    Max := 0;
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);

  CodeSite.ExitMethod(Self,'TBlock.MinMaxWidth');
   {$ENDIF}
    Exit;
  end;
{$ifdef DO_BLOCK_INLINE}
  if Display = pdInline then
  begin
    inherited;
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);

  CodeSite.ExitMethod(Self,'TBlock.MinMaxWidth');
   {$ENDIF}

    exit;
  end;
{$endif}

  ConvMargArray(0, 400, AutoCount);
  HideOverflow := HideOverflow and (MargArray[piWidth] <> Auto) and (MargArray[piWidth] > 20);
  if HideOverflow then
  begin
    MinCell := MargArray[piWidth];
    MaxCell := MinCell;
  end
  else
    ContentMinMaxWidth(Canvas, MinCell, MaxCell);
  if MargArray[MarginLeft] = Auto then
    MargArray[MarginLeft] := 0;
  if MargArray[MarginRight] = Auto then
    MargArray[MarginRight] := 0;
  if MargArray[piWidth] = Auto then
    MargArray[piWidth] := 0;
  LeftSide := MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
  RightSide := MargArray[MarginRight] + MargArray[BorderRightWidth] + MargArray[PaddingRight];
  if MargArray[piWidth] > 0 then
  begin
    Min := MargArray[piWidth] + LeftSide + RightSide;
    Max := Min;
  end
  else
  begin
    Min := Math.Max(MinCell, MargArray[piWidth]) + LeftSide + RightSide;
    Max := Math.Max(MaxCell, MargArray[piWidth]) + LeftSide + RightSide;
  end;
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);

  CodeSite.ExitMethod(Self,'TBlock.MinMaxWidth');
   {$ENDIF}
end;

{----------------TBlock.GetURL}

function TBlock.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): guResultType;
begin
  UrlTarg := nil;
  FormControl := nil;
  case Display of
    pdNone: Result := [];
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
{$endif}
  else
    Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
    if (BlockTitle <> '') and PtInRect(MyRect, Point(X, Y - Document.YOFF)) then
    begin
      ATitle := BlockTitle;
      Include(Result, guTitle);
    end;
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

function TBlock.FindCursor(Canvas: TCanvas; X, Y: Integer;
  out XR, YR, CaretHt: Integer; out Intext: boolean): Integer;
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

function TBlock.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;
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

function TBlock.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean;
begin
  Obj := nil;
  case Display of
    pdNone:   Result := False;
{$ifdef DO_BLOCK_INLINE}
    pdInline: Result := inherited GetChAtPos(Pos, Ch, Obj);
{$endif}
  else
    Result := MyCell.GetChAtPos(Pos, Ch, Obj);
  end;
end;

function TBlock.CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean;
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
    if (Pos('p.', TagClass) = 1) and (Document.SelE > MyCell.StartCurs + MyCell.Len) then
      Document.CB.AddTextCR('', 0);
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
      MargArray[piWidth] := Max(MinWidth, AWidth - BordWidth - LeftP)
    else if (FloatLR in [ALeft, ARight]) then
      MargArray[piWidth] := Min(MaxWidth, AWidth - BordWidth)
    else begin
      MargArray[piWidth] := Max(MinWidth, AWidth - BordWidth);
    end;
  end;

  procedure CalcMargRt;
  begin
    MargArray[MarginRight] := Max(0, AWidth - BordPad - MargArray[MarginLeft] - MargArray[piWidth]);
  end;

  procedure CalcMargLf;
  begin
    MargArray[MarginLeft] := Max(0, AWidth - BordPad - MargArray[MarginRight] - MargArray[piWidth]);
  end;

begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.FindWidth');
  CodeSite.SendFmtMsg('AWidth    = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight   = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('AutoCount = [%d]',[AutoCount]);
  CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);

  CodeSite.AddSeparator;
  {$ENDIF}
  ContentMinMaxWidth(Canvas, MinWidth, MaxWidth);
  HideOverflow := HideOverflow and (MargArray[piWidth] <> Auto) and (MargArray[piWidth] > 20);
  case AutoCount of
    0:
      begin
        if (Justify in [centered, Right]) and (Positioning = posStatic)
          and not (FloatLR in [ALeft, ARight]) and
          (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
        begin
          ApplyBoxWidthSettings(MargArray,MinWidth,MaxWidth,Document.UseQuirksMode);
          Marg2 := Max(0, AWidth - MargArray[piWidth] - BordPad);
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

    1:
      if MargArray[piWidth] = Auto then begin
        ApplyBoxWidthSettings(MargArray,MinWidth,MaxWidth,Document.UseQuirksMode);
        CalcWidth;
      end
      else
      begin
        if MargArray[MarginRight] = Auto then
          if (FloatLR in [ALeft, ARight]) then
            MargArray[MarginRight] := 0
          else
            CalcMargRt
        else
          CalcMargLf;
      end;

    2:
      if MargArray[piWidth] = Auto then
      begin
        if MargArray[MarginLeft] = Auto then
          MargArray[MarginLeft] := 0
        else
          MargArray[MarginRight] := 0;
        ApplyBoxWidthSettings(MargArray,MinWidth,MaxWidth,Document.UseQuirksMode);
        CalcWidth;
      end
      else
      begin
        Marg2 := Max(0, AWidth - MargArray[piWidth] - BordPad);
        MargArray[MarginLeft] := Marg2 div 2;
        MargArray[MarginRight] := Marg2 div 2;
      end;

    3:
      begin
        MargArray[MarginLeft] := 0;
        MargArray[MarginRight] := 0;
        ApplyBoxWidthSettings(MargArray,MinWidth,MaxWidth,Document.UseQuirksMode);
        CalcWidth;
      end;
  end;
  Result := MargArray[piWidth];
  {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlock.FindWidth');
  {$ENDIF}
end;

procedure DoImageStuff(Canvas: TCanvas; IW, IH: Integer; BGImage: TImageObj; PRec: PtPositionRec;
  var TiledImage: TgpObject; var TiledMask: TBitmap; var NoMask: boolean);
{Set up for the background image. Allow for tiling, and transparency
  BGImage is the image
  PRec describes the location and tiling
  IW, IH, the width and height of the background
}
var
  OW, OH, X, XX, Y, X2, Y2: Integer;
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
  if (IW = 0) or (IH = 0) then
    exit;

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

  NoMask := not Assigned(TheMask) and PRec.X.RepeatD and PRec.Y.RepeatD;

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

  with PRec.X do
  begin
    X := GetPositionInRange(PosType, Value, IW - OW);
    AdjustForTiling(RepeatD, 0, IW, OW, X, X2);
  end;
  with PRec.Y do
  begin
    Y := GetPositionInRange(PosType, Value, IH - OH);
    AdjustForTiling(RepeatD, 0, IH, OH, Y, Y2);
  end;

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

  function GetClearSpace(ClearAttr: ClearAttrType): Integer;
  var
    CL, CR: Integer;
  begin
    Result := 0;
    if ClearAttr <> clrNone then
    begin {may need to move down past floating image}
      IMgr.GetClearY(CL, CR);
      case ClearAttr of
        clLeft: Result := Max(0, CL - Y - 1);
        clRight: Result := Max(0, CR - Y - 1);
        clAll: Result := Max(CL - Y - 1, Max(0, CR - Y - 1));
      end;
    end;
  end;

  function GetClientContentBot(ClientContentBot: Integer): Integer;
  begin
    if HideOverflow and (MargArray[piHeight] > 3) then
      Result := ContentTop + MargArray[piHeight]
    else
      Result := Max(Max(ContentTop, ClientContentBot), ContentTop + MargArray[piHeight]);
  end;

var
  LIndent, RIndent: Integer;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.DrawLogic');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
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
    ApplyBoxSettings(MargArray,Document.UseQuirksMode);
    NewWidth := FindWidth(Canvas, AWidth, AHeight, AutoCount);
    LeftWidths  := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
    RightWidths := MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth];
    MiscWidths  := LeftWidths + RightWidths;
    TotalWidth  := MiscWidths + NewWidth;

    Indent := LeftWidths;
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
            LeftP := AWidth - MargArray[RightPos] - MargArray[piWidth] - LeftWidths - RightWidths
          else
            LeftP := 0;
        X := LeftP;
        Y := TopP + YRef;
      end;
    end;

    YClear := Y + ClearAddon;
    if not (Positioning in [posAbsolute, PosFixed]) then
      case FloatLR of
        ALeft:
        begin
          YClear := Y;
          LIndent := IMgr.AlignLeft(YClear, TotalWidth);
          Indent := LIndent + LeftWidths - X;
        end;

        ARight:
        begin
          YClear := Y;
          RIndent := IMgr.AlignRight(YClear, TotalWidth);
          Indent := RIndent + LeftWidths - X;
        end;
      end;
    Inc(X, Indent);

    if MargArray[MarginTop] <= 0 then
      DrawTop := YClear
    else
      DrawTop := YClear + MargArray[MarginTop]; {Border top}

    ContentTop := YClear + MargArray[MarginTop] + MargArray[PaddingTop] + MargArray[BorderTopWidth];

    if (Positioning in [posAbsolute, posFixed]) or (FloatLR in [ALeft, ARight]) then
    begin
      RefIMgr := IMgr;
      if MyCell.IMgr = nil then
      begin
        MyIMgr := TIndentManager.Create;
        MyCell.IMgr := MyIMgr;
      end;
      IMgr := MyCell.IMgr;
      IMgr.Init(0, NewWidth);
    end
    else
    begin
      MyCell.IMgr := IMgr;
    end;

    SaveID := IMgr.CurrentID;
    IMgr.CurrentID := Self;

    LIndex := IMgr.SetLeftIndent(X, YClear);
    RIndex := IMgr.SetRightIndent(X + NewWidth, YClear);

    if MargArray[piHeight] > 0 then begin
      BlockHeight := MargArray[piHeight];
    end else if AHeight > 0 then
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
          NewWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
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
          NewWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
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
        NewWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
      MaxWidth := Indent + ScrollWidth + RightWidths;
      ClientContentBot := GetClientContentBot(MyCell.tcContentBot);
    end;
    Len := Curs - StartCurs;
    ContentBot :=  ClientContentBot + MargArray[PaddingBottom] + MargArray[BorderBottomWidth] + MargArray[MarginBottom];
    DrawBot := Max(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom] + MargArray[BorderBottomWidth];

    Result := ContentBot - Y;

    if Assigned(BGImage) and Document.ShowImages then
    begin
      BGImage.DrawLogic(Document, Canvas, nil, 100, 0);
      if BGImage.Image = ErrorBitmap then
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
    if (Positioning in [posAbsolute, posFixed]) or (FloatLR in [ALeft, ARight]) then
    begin
      case Positioning of
        posAbsolute,
        posFixed:
          DrawHeight := 0
      else
        DrawHeight := 0; //SectionHeight;
        case FloatLR of
          ALeft:  RefIMgr.AddLeft(YClear, ContentBot, TotalWidth);
          ARight: RefIMgr.AddRight(YClear, ContentBot, TotalWidth);
        end;
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
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlock.DrawLogic');
   {$ENDIF}
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
    SB.FOwner := Self;
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
    case Visibility of
      viHidden:
        Result := 0;
    else
      Y := YDraw;
      YO := Y - Document.YOff;
      Result := Y + SectionHeight;

      if Document.SkipDraw then
      begin
        Document.SkipDraw := False;
        Exit;
      end;

      if Document.Printing and (Positioning <> posAbsolute) then
        if BreakBefore and not Document.FirstPageItem then
        begin
          if ARect.Top + Document.YOff < YDraw + MargArray[MarginTop] then {page-break-before}
          begin
            if YDraw + MargArray[MarginTop] < Document.PageBottom then
              Document.PageBottom := YDraw + MargArray[MarginTop];
            Document.SkipDraw := True; {prevents next block from drawing a line}
            Exit;
          end;
        end
        else if KeepIntact then
        begin
        {if we're printing and block won't fit on this page and block will fit on
         next page, then don't do block now}
          if (YO > ARect.Top) and (Y + DrawHeight > Document.PageBottom) and
            (DrawHeight - MargArray[MarginTop] < ARect.Bottom - ARect.Top) then
          begin
            if Y + MargArray[MarginTop] < Document.PageBottom then
              Document.PageBottom := Y + MargArray[MarginTop];
            Exit;
          end;
        end
        else if BreakAfter then
        begin
          if ARect.Top + Document.YOff < Result then {page-break-after}
            if Result < Document.PageBottom then
              Document.PageBottom := Result;
        end
        else if Self is TTableBlock and not TTableBlock(Self).Table.HeadOrFoot then {ordinary tables}
        {if we're printing and
         we're 2/3 down page and table won't fit on this page and table will fit on
         next page, then don't do table now}
        begin
          if (YO > ARect.Top + ((ARect.Bottom - ARect.Top) * 2) div 3) and
            (Y + DrawHeight > Document.PageBottom) and
            (DrawHeight < ARect.Bottom - ARect.Top) then
          begin
            if Y + MargArray[MarginTop] < Document.PageBottom then
              Document.PageBottom := Y + MargArray[MarginTop];
            Exit;
          end;
        end
        else if Self is TTableBlock then {try to avoid just a header and footer at page break}
          with TTableBlock(Self).Table do
            if HeadOrFoot and (Document.TableNestLevel = 0)
              and ((Document.PrintingTable = nil) or
              (Document.PrintingTable = TTableBlock(Self).Table)) then
            begin
              Spacing := CellSpacing div 2;
              HeightNeeded := HeaderHeight + FootHeight + Rows.Items[HeaderRowCount].RowHeight;
              if (YO > ARect.Top) and (Y + HeightNeeded > Document.PageBottom) and
                (HeightNeeded < ARect.Bottom - ARect.Top) then
              begin {will go on next page}
                if Y + Spacing < Document.PageBottom then
                begin
                  Document.PageShortened := True;
                  Document.PageBottom := Y + Spacing;
                end;
                Exit;
              end;
            end;

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
end;

{----------------TBlock.DrawBlock}

procedure TBlock.DrawBlock(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, Y, XRef, YRef: Integer);

var
  YOffset: Integer;
  XR, YB, RefX, RefY, TmpHt: Integer;
  SaveID: TObject;
  ImgOK, HasBackgroundColor: boolean;
  IT, IH, FT, IW: Integer;
  Rgn, SaveRgn, SaveRgn1: HRgn;
  OpenRgn: Boolean;
  PdRect: TRect;
begin
  if Document.Printing and not Document.PrintBackground then
    NeedDoImageStuff := False;
  YOffset := Document.YOff;

  case FLoatLR of
    ALeft, ARight:
    begin
      //X := IMgr.LfEdge + Indent;
      X := X + Indent;
      RefX := X - MargArray[PaddingLeft] - MargArray[BorderLeftWidth];
      XR := X + NewWidth + MargArray[PaddingRight] + MargArray[BorderRightWidth];
      RefY := DrawTop;
      YB := ContentBot - MargArray[MarginBottom];
    end;
  else
    RefX := X + MargArray[MarginLeft];
    X := X + Indent;
    XR := X + NewWidth + MargArray[PaddingRight] + MargArray[BorderRightWidth]; {current right edge}
    RefY := Y + ClearAddon + MargArray[MarginTop];
    YB := ContentBot - MargArray[MarginBottom];
    case Positioning of
      posRelative:
        Inc(YB, TopP);
    end;
  end;

  // MyRect is the outmost rectangle of this block incl. border and padding but without margins in screen coordinates.
  MyRect := Rect(RefX, RefY - YOffset, XR, YB - YOffset);

  // PdRect is the border rectangle of this block incl. padding in screen coordinates
  PdRect.Left   := MyRect.Left   + MargArray[BorderLeftWidth];
  PdRect.Top    := MyRect.Top    + MargArray[BorderTopWidth];
  PdRect.Right  := MyRect.Right  - MargArray[BorderRightWidth];
  PdRect.Bottom := MyRect.Bottom - MargArray[BorderBottomWidth];

  IT := Max(0, ARect.Top - 2 - PdRect.Top);
  FT := Max(PdRect.Top, ARect.Top - 2); {top of area drawn, screen coordinates}
  IH := Min(PdRect.Bottom, ARect.Bottom) - FT; {height of area actually drawn}
  IW := PdRect.Right - PdRect.Left;

  SaveRgn1 := 0;
  OpenRgn := (Positioning <> PosStatic) and (Document.TableNestLevel > 0);
  if OpenRgn then
  begin
    SaveRgn1 := CreateRectRgn(0, 0, 1, 1);
    GetClipRgn(Canvas.Handle, SaveRgn1);
    SelectClipRgn(Canvas.Handle, 0);
  end;
  try
    if (MyRect.Top <= ARect.Bottom) and (MyRect.Bottom >= ARect.Top) then
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
            if Document.IsCopy and (TiledImage is TBitmap) then
              TBitmap(TiledImage).HandleType := bmDIB;
          except {bad image, get rid of it}
            FreeAndNil(BGImage);
            FreeAndNil(TiledImage);
            FreeAndNil(TiledMask);
          end;
          NeedDoImageStuff := False;
        end;

        if Document.NoOutput then
          exit;

        ImgOK := not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
          and Document.ShowImages;

        if HasBackgroundColor and
          (not Document.Printing or Document.PrintTableBackground) then
        begin {color the Padding Region}
          Canvas.Brush.Color := ThemedColor(MargArray[BackgroundColor]) or PalRelative;
          Canvas.Brush.Style := bsSolid;
          if Document.IsCopy and ImgOK then
          begin
            InitFullBG(FullBG,IW, IH,Document.IsCopy);
            FullBG.Canvas.Brush.Color := ThemedColor(MargArray[BackgroundColor]) or PalRelative;
            FullBG.Canvas.Brush.Style := bsSolid;
            FullBG.Canvas.FillRect(Rect(0, 0, IW, IH));
          end
          else
            if (not Document.Printing or Document.PrintBackground) then
              Canvas.FillRect(Rect(PdRect.Left, FT, PdRect.Right, FT + IH));
        end;

        if ImgOK and (TiledImage <> nil) then
        begin
          if not Document.IsCopy then
            {$IFNDEF NoGDIPlus}
            if TiledImage is TgpBitmap then
            //DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PdRect.Left, PT)
              DrawGpImage(Canvas.Handle, TgpImage(TiledImage), PdRect.Left, FT, 0, IT, IW, IH)
            //BitBlt(Canvas.Handle, PdRect.Left, FT, PdRect.Right-PdRect.Left, IH, TiledImage.Canvas.Handle, 0, IT, SrcCopy)
            else
            {$ENDIF !NoGDIPlus}
            if NoMask then
              BitBlt(Canvas.Handle, PdRect.Left, FT, PdRect.Right - PdRect.Left, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcCopy)
            else
            begin
              InitFullBG(FullBG,PdRect.Right - PdRect.Left, IH,Document.IsCopy);
              BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, Canvas.Handle, PdRect.Left, FT, SrcCopy);
              BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
              BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
              BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
              BitBlt(Canvas.Handle, PdRect.Left, FT, IW, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
            end
          else
          {$IFNDEF NoGDIPlus}
          if TiledImage is TGpBitmap then {printing}
          begin
            if HasBackgroundColor then
            begin
              DrawGpImage(FullBg.Canvas.Handle, TgpImage(TiledImage), 0, 0);
              PrintBitmap(Canvas, PdRect.Left, FT, IW, IH, FullBG);
            end
            else
              PrintGpImageDirect(Canvas.Handle, TgpImage(TiledImage), PdRect.Left, PdRect.Top,
                Document.ScaleX, Document.ScaleY);
          end
          else
          {$ENDIF !NoGDIPlus}
          if Assigned(TiledImage) then begin

            if NoMask then {printing}
                PrintBitmap(Canvas, PdRect.Left, FT, PdRect.Right - PdRect.Left, IH, TBitmap(TiledImage))
              else if HasBackgroundColor then
              begin
                BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
                BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
                BitBlt(FullBG.Canvas.Handle, 0, 0, IW, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
                PrintBitmap(Canvas, PdRect.Left, FT, IW, IH, FullBG);
              end
              else
              PrintTransparentBitmap3(Canvas, PdRect.Left, FT, IW, IH, TBitmap(TiledImage), TiledMask, IT, IH)
            end;
          end;
      except
      end;
    end;

    if HideOverflow then
      if FloatLR = ANone then
        GetClippingRgn(Canvas,
          Rect(
            PdRect.Left   + MargArray[PaddingLeft],
            PdRect.Top    + MargArray[PaddingTop],
            PdRect.Right  - MargArray[PaddingRight],
            PdRect.Bottom - MargArray[PaddingBottom]),
          Document.Printing, Rgn, SaveRgn)
      else
        GetClippingRgn(Canvas, Rect(PdRect.Left, PdRect.Top, PdRect.Right, PdRect.Bottom), Document.Printing, Rgn, SaveRgn);

    SaveID := IMgr.CurrentID;
    Imgr.CurrentID := Self;
    if Positioning = posRelative then
      DrawTheList(Canvas, ARect, NewWidth, X,
        RefX + MargArray[BorderLeftWidth] + MargArray[PaddingLeft],
        Y + MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop])
    else if Positioning = posAbsolute then
      DrawTheList(Canvas, ARect, NewWidth, X,
        RefX + MargArray[BorderLeftWidth],
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
    DrawBlockBorder(Canvas, MyRect, PdRect);
  finally
    if OpenRgn then
    begin
      SelectClipRgn(Canvas.Handle, SaveRgn1);
      DeleteObject(SaveRgn1);
    end;
  end;
end;

procedure TBlock.DrawBlockBorder(Canvas: TCanvas; const ORect, IRect: TRect);
begin
  if HasBorderStyle then
    if (ORect.Left <> IRect.Left) or (ORect.Top <> IRect.Top) or (ORect.Right <> IRect.Right) or (ORect.Bottom <> IRect.Bottom) then
      DrawBorder(Canvas, ORect, IRect,
        htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
        htStyles(BorderStyleType(MargArray[BorderLeftStyle]), BorderStyleType(MargArray[BorderTopStyle]), BorderStyleType(MargArray[BorderRightStyle]), BorderStyleType(MargArray[BorderBottomStyle])),
        MargArray[BackgroundColor], Document.Printing)
end;

procedure TBlock.DrawTheList(Canvas: TCanvas; const ARect: TRect; ClipWidth, X, XRef, YRef: Integer);
{draw the list sorted by Z order.}
var
  I: Integer;
  SaveID: TObject;
begin
  if (Positioning in [posAbsolute, posFixed]) or (FloatLR in [ALeft, ARight]) then
    with MyCell do
    begin
      SaveID := IMgr.CurrentID;
      IMgr.Reset(X{RefIMgr.LfEdge});
      IMgr.ClipWidth := ClipWidth;
      IMgr.CurrentID := SaveID;
    end
  else
    MyCell.IMgr.ClipWidth := ClipWidth;
  for I := 0 to DrawList.Count - 1 do
    TSectionBase(DrawList[I]).Draw1(Canvas, ARect, MyCell.IMgr, X, XRef, YRef);
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

constructor TTableAndCaptionBlock.Create(Master: ThtDocument; Prop: TProperties;
  AnOwnerCell: TCellBasic; Attributes: TAttributeList; ATableBlock: TTableBlock);
var
  I: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableAndCaptionBlock.Create');
   {$ENDIF}
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TTableAndCaptionBlock.Create');
   {$ENDIF}
end;

{----------------TTableAndCaptionBlock.CancelUsage}

procedure TTableAndCaptionBlock.CancelUsage;
{called when it's found that this block isn't needed (no caption)}
begin
{assign the ID back to the Table}
  if TableID <> '' then
    Document.IDNameList.AddObject(TableID, TableBlock);
end;

{----------------TTableAndCaptionBlock.CreateCopy}

constructor TTableAndCaptionBlock.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableAndCaptionBlock.FindWidth');
  CodeSite.SendFmtMsg('AWidth = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('AutoCount = [%d]',[AutoCount]);
  CodeSite.AddSeparator;
   {$ENDIF}
  HasBorderStyle := False; //bssNone; {has no border}
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TTableAndCaptionBlock.FindWidth');
   {$ENDIF}
end;

{----------------TTableAndCaptionBlock.MinMaxWidth}

procedure TTableAndCaptionBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
var
  Mx, Mn, MxTable, MnTable: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableAndCaptionBlock.MinMaxWidth');
   {$ENDIF}
  TableBlock.MinMaxWidth(Canvas, MnTable, MxTable);
  FCaptionBlock.MinMaxWidth(Canvas, Mn, Mx);
  Min := Math.Max(MnTable, Mn);
  Max := Math.Max(MxTable, Mn);
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);
  CodeSite.ExitMethod(Self,'TTableAndCaptionBlock.MinMaxWidth');
   {$ENDIF}
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

constructor TTableBlock.Create(Master: ThtDocument; Prop: TProperties;
  AnOwnerCell: TCellBasic; ATable: THtmlTable; TableAttr: TAttributeList;
  TableLevel: Integer);
var
  I, AutoCount, BorderColor, BorderWidth: Integer;
  Percent: boolean;
  S,W,C: PropIndices;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableBlock.Create');
   {$ENDIF}
  inherited Create(Master, Prop, AnOwnerCell, TableAttr);
  Table := ATable;
  Justify := NoJustify;

  for I := 0 to TableAttr.Count - 1 do
    with TableAttr[I] do
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
            PRec.X.PosType := bpDim;
            PRec.X.Value := 0;
            PRec.X.RepeatD := True;
            PRec.Y := PRec.X;
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
          if (VarType(MargArrayO[piHeight]) in VarInt) and (MargArrayO[piHeight] = IntNull) then
            MargArrayO[piHeight] := Name;
      end;

  //BG, 13.06.2010: Issue 5: Table border versus stylesheets:
  //BG, 02.02.2012: Issue 121: Cell border:
  TableBorder := False;
  BorderColor := Table.BorderColor;
  if BorderColor = clNone then
    BorderColor := clSilver;
  if Table.HasBorderWidth then
    BorderWidth := Table.BorderWidth
  else
    BorderWidth := 3;
  C := BorderTopColor;
  W := BorderTopWidth;
  for S := BorderTopStyle to BorderLeftStyle do
  begin
    if (MargArrayO[S] = Unassigned) or (MargArrayO[S] = bssNone) then
    begin
      // no style specified
      if Table.BorderWidth > 0 then
      begin
        if (VarType(MargArrayO[C]) in varInt) and (MargArrayO[C] = IntNull) then
          // set default style
          if Table.BorderColor = clNone then
            MargArrayO[S] := bssOutset
          else
            MargArrayO[S] := bssSolid;

        if (VarType(MargArrayO[W]) in varInt) and (MargArrayO[W] = IntNull) then
          // set default width
          MargArrayO[W] := Table.BorderWidth;

        HasBorderStyle := True;
        TableBorder := True;
      end;
    end
    else
    begin
      // style has been specified
      TableBorder := not Table.HasBorderWidth or (Table.BorderWidth > 0);
      if (VarType(MargArrayO[W]) in varInt) and (MargArrayO[W] = IntNull) then
        // set default width
        MargArrayO[W] := BorderWidth;
    end;

    if (VarType(MargArrayO[C]) in varInt) and (MargArrayO[C] = IntNull) then
      // no color specified, set default color:
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

    Inc(C);
    Inc(W);
  end;

{need to see if width is defined in style}
  Percent := (VarIsStr(MargArrayO[piWidth])) and (Pos('%', MargArrayO[piWidth]) > 0);
  StyleUn.ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, BorderWidth, AutoCount, MargArray);
  if MargArray[piWidth] > 0 then
  begin
    if Percent then
    begin
      AsPercent := True;
      WidthAttr := Min(1000, MargArray[piWidth] * 10);
    end
    else
    begin
      WidthAttr := MargArray[piWidth];
    {By custom (not by specs), tables handle CSS Width property differently.  The
     Width includes the padding and border.}
      MargArray[piWidth] := WidthAttr - MargArray[BorderLeftWidth] - MargArray[BorderRightWidth]
        - MargArray[PaddingLeft] - MargArray[PaddingRight];
      MargArrayO[piWidth] := MargArray[piWidth];
      AsPercent := False;
    end;
  end;

  CollapseMargins;
  Table.Float := FloatLR in [ALeft, ARight];
  if Table.Float and (ZIndex = 0) then
    ZIndex := 1;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TTableBlock.Create');
   {$ENDIF}
end;

{----------------TTableBlock.CreateCopy}

constructor TTableBlock.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
var
  TT: TTableBlock;
  Item: TObject;
begin
  inherited;
  TT := T as TTableBlock;
  System.Move(TT.WidthAttr, WidthAttr, PtrSub(@Justify, @WidthAttr) + Sizeof(Justify));
  Item := MyCell.Items[0];
  Table := Item as THtmlTable;
end;

{----------------TTableBlock.MinMaxWidth}

procedure TTableBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
var
  TmpWidth: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableBlock.MinMaxWidth');
    CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);
  CodeSite.AddSeparator;
   {$ENDIF}
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
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);
  CodeSite.ExitMethod(Self,'TTableBlock.MinMaxWidth');
   {$ENDIF}
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
  Min, Max, M, P: Integer;
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
        Result := MulDiv(AWidth, WidthAttr, 1000) - LeftSide - RightSide
      else
        Result := WidthAttr - (MargArray[PaddingLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingRight] + MargArray[BorderRightWidth]);
      Table.tblWidthAttr := Result;
      Table.MinMaxWidth(Canvas, Min, Max);
      Table.tblWidthAttr := Math.Max(Min, Result);
    end;
    Result := Table.tblWidthAttr;
  end
  else
  begin
    Result := AWidth - LeftSide - RightSide;
    Table.MinMaxWidth(Canvas, Min, Max);
    P := Math.Min(Sum(Table.Percents), 1000);
    if P > 0 then
    begin
      P := MulDiv(Result, P, 1000);
      Min := Math.Max(Min, P);
      Max := Math.Max(Max, P);
    end;
    if Result > Max then
      Result := Max
    else if Result < Min then
      Result := Min;
  end;
  MargArray[piWidth] := Result;

  if (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) and (Result + LeftSide + RightSide < AWidth) then
    case Justify of
      Centered:
      begin
        M := AWidth - LeftSide - Result - RightSide;
        MargArray[MarginLeft]  := M div 2;
        MargArray[MarginRight] := M - MargArray[MarginLeft];
      end;
      Right:
        MargArray[MarginLeft] := AWidth - (Result + LeftSide + RightSide);
    end;
end;

function TTableBlock.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer;
  IMgr: TIndentManager; var MaxWidth: Integer; var Curs: Integer): Integer;
var
  X1, Tmp: Integer;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableBlock.DrawLogic');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );

  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
  if not (FloatLR in [ALeft, ARight]) then
  begin
    Tmp := X;
    X := Max(Tmp, IMgr.LeftIndent(Y));
    TableIndent := X - Tmp;
    X1 := Min(Tmp + AWidth, IMgr.RightSide(Y));
    AWidth := X1 - X;
  end;
  Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TTableBlock.DrawLogic');
   {$ENDIF}
end;

function TTableBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
begin
  X := X + TableIndent;
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
end;

procedure TTableBlock.DrawBlockBorder(Canvas: TCanvas; const ORect, IRect: TRect);
var
  Light, Dark: TColor;
  C: PropIndices;
begin
  //BG, 13.06.2010: Issue 5: Table border versus stylesheets
  GetRaisedColors(Document, Canvas, Light, Dark);
  for C := BorderTopColor to BorderLeftColor do
    if MargArrayO[C] = clBtnHighLight then
      MargArray[C] := Light
    else if MargArrayO[C] = clBtnShadow then
      MargArray[C] := Dark;
  inherited;
end;

procedure TTableBlock.AddSectionsToList;
begin {Sections in Table not added only table itself}
  Document.PositionList.Add(Table);
end;

constructor THRBlock.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'THRBlock.FindWidth');
    CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);

  CodeSite.SendFmtMsg('AWidth = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('AutoCount = [%d]',[AutoCount]);
  CodeSite.AddSeparator;
   {$ENDIF}
  if Positioning = posAbsolute then
    Align := Left;
  LeftSide := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  RightSide := MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth];
  SWidth := MargArray[piWidth];

  if SWidth > 0 then
    Result := Min(SWidth, AWidth - LeftSide - RightSide)
  else
    Result := Max(15, AWidth - LeftSide - RightSide);
  MargArray[piWidth] := Result;
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
  if not Document.IsCopy then
    THorzline(MyHRule).VSize := MargArray[piHeight];
   {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'THRBlock.FindWidth');
   {$ENDIF}
end;

{----------------TBlockLI.Create}

constructor TBlockLI.Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic;
  Sy: Symb; APlain: boolean; AIndexType: ThtChar; AListNumb,
  ListLevel: Integer; Attributes: TAttributeList);
var
  Tmp: ListBulletType;
  S: ThtString;
  TmpFont: TMyFont;
begin
  inherited Create(Master, Prop, AnOwnerCell, Attributes);

  Tmp := Prop.GetListStyleType;
  if Tmp <> lbBlank then
    FListStyleType := Tmp;
  case Sy of

    UlSy, DirSy, MenuSy:
      begin
        FListType := Unordered;
        if APlain or (Display = pdInline) then
          FListStyleType := lbNone
        else
          if Tmp = lbBlank then
            case ListLevel mod 3 of
              1: FListStyleType := lbDisc;
              2: FListStyleType := lbCircle;
              0: FListStyleType := lbSquare;
            end;
      end;

    OLSy:
      begin
        FListType := Ordered;
        if Tmp = lbBlank then
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
    if Tmp = lbBlank then
      FListStyleType := lbDisc;
    if (VarType(MargArrayO[MarginLeft]) in varInt) and
      ((MargArrayO[MarginLeft] = IntNull) or (MargArrayO[MarginLeft] = 0)) then
      MargArrayO[MarginLeft] := 16;
  end;

  if (VarType(MargArrayO[PaddingLeft]) in varInt) and (MargArrayO[PaddingLeft] = IntNull) then
    case Sy of

      OLSy, ULSy, DirSy, MenuSy:
        if APlain then
          MargArrayO[PaddingLeft] := 0
        else
          MargArrayO[PaddingLeft] := ListIndent;

      DLSy:
        MargArrayO[PaddingLeft] := 0;
    else
      MargArrayO[PaddingLeft] := ListIndent;
    end;

  FListNumb := AListNumb;
  FListFont := TMyFont.Create;
  TmpFont := Prop.GetFont;
  FListFont.Assign(TmpFont);
  TmpFont.Free;

  S := Prop.GetListStyleImage;
  if S <> '' then
    Image := TImageObj.SimpleCreate(Master, S);
end;

constructor TBlockLI.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
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
  FListFont := TMyFont.Create;
  FListFont.Assign(TT.ListFont);
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlockLI.DrawLogic');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );

  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
  if Assigned(Image) then
  begin
    Image.DrawLogic(Document, Canvas, nil, 100, 0);
    if (Image.Image = ErrorBitmap) then
    begin
      Image.Free;
      Image := nil;
    end;
  end;
  Document.FirstLineHtPtr := @FirstLineHt;
  FirstLineHt := 0;
  try
    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  finally
    Document.FirstLineHtPtr := nil;
  end;
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlockLI.DrawLogic');
   {$ENDIF}
end;

//-- BG ---------------------------------------------------------- 31.01.2012 --
procedure TBlockLI.SetListFont(const Value: TFont);
begin
  FListFont.Assign(Value);
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

begin
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);

  X := X + Indent;

  if FirstLineHt > 0 then
  begin
    YB := FirstLineHt - Document.YOff;
    if (YB < ARect.Top - 50) or (YB > ARect.Bottom + 50) then
      Exit;
    if Assigned(Image) and (Image.Image <> DefBitmap) and Document.ShowImages then
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
          Pen.Color := ThemedColor(ListFont.Color);
          Pen.Style := psSolid;
          BrushStyle := Brush.Style;
          BrushColor := Brush.Color;
          Brush.Style := bsSolid;
          Brush.Color := ListFont.Color;
          case ListStyleType of
            lbCircle:
              begin
                Brush.Style := bsClear;
                Circle(Canvas,X - 16, YB, 7);
              end;
            lbDisc:
              Circle(Canvas,X - 15, YB - 1, 5);
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

constructor TBodyBlock.Create(Master: ThtDocument; Prop: TProperties;
  AnOwnerCell: TCellBasic; Attributes: TAttributeList);
var
  PRec: PtPositionRec;
  Image: ThtString;
  Val: TColor;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBodyBlock.Create');
  StyleUn.LogProperties(Prop,'Prop');
  CodeSite.AddSeparator;
  {$ENDIF}
  inherited Create(Master,Prop,AnOwnerCell,Attributes);
  positioning := PosStatic; {7.28}
  Prop.GetBackgroundPos(0, 0, PRec);
  if Prop.GetBackgroundImage(Image) and (Image <> '') then
    Master.SetBackgroundBitmap(Image, PRec);
  Val := Prop.GetBackgroundColor;
  if Val <> clNone then
    Master.SetBackGround(Val or PalRelative);
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TBodyBlock.Create');
  {$ENDIF}
end;

{----------------TBodyBlock.GetURL}

function TBodyBlock.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): guResultType;
begin
  Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  if (BlockTitle <> '') then
  begin
    ATitle := BlockTitle;
    Include(Result, guTitle);
  end;
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBodyBlock.DrawLogic');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
  YDraw := Y;
  StartCurs := Curs;
  StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderWidth, AutoCount, MargArray);
  if IsAuto(MargArray[MarginLeft]) then MargArray[MarginLeft] := 0;
  if IsAuto(MargArray[MarginRight]) then MargArray[MarginRight] := 0;
  ApplyBoxSettings(MargArray,Document.UseQuirksMode);

  X := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  NewWidth := IMgr.Width - (X + MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth]);

  DrawTop := MargArray[MarginTop];

  MyCell.IMgr := IMgr;

  SaveID := IMgr.CurrentID;
  Imgr.CurrentID := Self;
  LIndex := IMgr.SetLeftIndent(X, Y);
  RIndex := IMgr.SetRightIndent(X + NewWidth, Y);

  ContentTop := Y + MargArray[MarginTop] + MargArray[PaddingTop] + MargArray[BorderTopWidth];
  MyCell.DoLogicX(Canvas, X, ContentTop, 0, 0, NewWidth,
    AHeight - MargArray[MarginTop] - MargArray[MarginBottom], BlHt, ScrollWidth, Curs);

  Len := Curs - StartCurs;

  ClientContentBot := Max(ContentTop, MyCell.tcContentBot);
  ContentBot := ClientContentBot + MargArray[PaddingBottom] + MargArray[BorderBottomWidth] + MargArray[MarginBottom];
  DrawBot := Max(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom] + MargArray[BorderBottomWidth];

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
  {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBodyBlock.DrawLogic');
  {$ENDIF}
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

{ ThtDocument }

//-- BG ---------------------------------------------------------- 04.03.2011 --
// moving from TIDObjectList to ThtDocument removed field OwnerList from TIDObjectList
function ThtDocument.AddChPosObjectToIDNameList(const S: ThtString; Pos: Integer): Integer;
begin
  Result := IDNameList.AddObject(S, TChPosObj.Create(Self, Pos));
end;

constructor ThtDocument.Create(Owner: THtmlViewerBase; APaintPanel: TWinControl);
begin
  inherited Create(Self);
  FPropStack := THtmlPropStack.Create;
  UseQuirksMode := Owner.UseQuirksMode;
  TheOwner := Owner;
  PPanel := APaintPanel;
  IDNameList := TIDObjectList.Create; //(Self);
  htmlFormList := TFreeList.Create;
  AGifList := TList.Create;
  MapList := TFreeList.Create;
  FormControlList := TFormControlObjList.Create(False);
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

//------------------------------------------------------------------------------
constructor ThtDocument.CreateCopy(T: ThtDocument);
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
  System.Move(T.ShowImages, ShowImages, PtrSub(@Background, @ShowImages) + Sizeof(Integer));
  PreFontName := T.PreFontName;
  htmlFormList := TFreeList.Create; {no copy of list made}
  AGifList := TList.Create;
  MapList := TFreeList.Create;
  MissingImages := ThtStringList.Create;
  PanelList := TList.Create;
  DrawList := TDrawList.Create;
  ScaleX := 1.0;
  ScaleY := 1.0;
  UseQuirksMode := T.UseQuirksMode;
end;

destructor ThtDocument.Destroy;
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
  FPropStack.Free;
end;

function ThtDocument.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): guResultType;
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

procedure ThtDocument.LButtonDown(Down: boolean);
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

procedure ThtDocument.CancelActives;
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

procedure ThtDocument.CheckGIFList(Sender: TObject);
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

procedure ThtDocument.HideControls;
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

procedure ThtDocument.SetYOffset(Y: Integer);
begin
  YOff := Y;
  YOffChange := True;
  HideControls;
end;

procedure ThtDocument.Clear;
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
  if not IsCopy then begin
    Styles.Clear;
    Styles.UseQuirksMode := Self.UseQuirksMode;
  end;
  if Assigned(TabOrderList) then
    TabOrderList.Clear;
  inherited Clear;
  htmlFormList.Clear;
  if Assigned(FormControlList) then
    FormControlList.Clear;
end;

procedure ThtDocument.ClearLists;
{called from DoBody to clear some things when starting over}
begin
  PanelList.Clear;
  if Assigned(FormControlList) then
    FormControlList.Clear;
end;

//-- BG ---------------------------------------------------------- 03.06.2012 --
// extracted from CopyToClipboardA(), GetSelLength() and GetSelTextBuf().
function ThtDocument.CopyToBuffer(Buffer: SelTextCount): Integer;
var
  I: Integer;
begin
  CB := Buffer;
  try
    for I := 0 to Count - 1 do
    begin
      with Items[I] do
      begin
        if SelB >= StartCurs + Len then
          Continue;
        if SelE <= StartCurs then
          Break;
        CopyToClipboard;
      end;
    end;
    Result := CB.Terminate;
  finally
    FreeAndNil(CB);
  end;
end;

procedure ThtDocument.CopyToClipboardA(Leng: Integer);
begin
  if SelE > SelB then
    CopyToBuffer(ClipBuffer.Create(Leng));
end;

function ThtDocument.GetSelLength: Integer;
begin
  if SelE > SelB then
    Result := CopyToBuffer(SelTextCount.Create)
  else
    Result := 0; {nothing to do}
end;

//------------------------------------------------------------------------------
function ThtDocument.GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
begin
  if SelE > SelB then
    Result := CopyToBuffer(SelTextBuf.Create(Buffer, BufSize))
  else
  begin
    if BufSize >= 1 then
    begin
      Buffer[0] := #0;
      Result := 1;
    end
    else
      Result := 0;
  end;
end;

//------------------------------------------------------------------------------
function ThtDocument.DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
  var ScrollWidth: Integer; var Curs: Integer): Integer;
var
  I, J: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'ThtDocument.DoLogic');
   {$ENDIF}
  Inc(CycleNumber);
  TableNestLevel := 0;
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
          TFormControlObj(Objects[I]).TabOrder := J;
          Inc(J);
        end
        else if Objects[I] is ThtTabControl then
        begin
          ThtTabControl(Objects[I]).TabOrder := J;
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'ThtDocument.DoLogic');
  {$ENDIF}
end;

//-- BG ---------------------------------------------------------- 11.09.2010 --
procedure ThtDocument.AddSectionsToPositionList(Sections: TSectionBase);
begin
  inherited;
  PositionList.Add(Sections);
end;

procedure ThtDocument.AdjustFormControls;
var
  Control: TControl;
  Showing: boolean;

{$IFNDEF FastRadio}
  function ActiveInList: boolean; {see if active control is a form control}
  var
    Control: TWinControl;
    I: Integer;
  begin
    Result := False;
    Control := Screen.ActiveControl;
    for I := 0 to FormControlList.Count - 1 do
      if FormControlList.Items[I].TheControl = Control then
      begin
        Result := True;
        Break;
      end;
  end;
{$ENDIF}

begin
  if IsCopy or (FormControlList.Count = 0) then
    Exit;
  with FormControlList do
{$IFNDEF FastRadio}
    if not ActiveInList then
      DeactivateTabbing
    else
{$ENDIF}
    begin
      Control := TheOwner; {THtmlViewer}
      repeat
        Showing := Control.Visible;
        Control := Control.Parent;
      until not Showing or not Assigned(Control);
      if Showing then
        ActivateTabbing;
    end;
end;

//------------------------------------------------------------------------------
function ThtDocument.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer;
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

procedure ThtDocument.SetFonts(const Name, PreName: ThtString; ASize: Integer;
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

procedure ThtDocument.SetBackground(ABackground: TColor);
begin
  Background := ABackground;
  if Assigned(OnBackGroundChange) then
    OnBackgroundChange(Self);
end;

procedure ThtDocument.SetBackgroundBitmap(const Name: ThtString; const APrec: PtPositionRec);
begin
  BackgroundBitmap := nil;
  BackgroundAniGif := nil;
  BitmapName := Name;
  BitmapLoaded := False;
  BackgroundPRec := APrec;
end;

//------------------------------------------------------------------------------
procedure ThtDocument.InsertImage(const Src: ThtString; Stream: TStream; out Reformat: boolean);
var
  UName: ThtString;
  J: Integer;
  Pair: TBitmapItem;
  Rformat, Error: boolean;
  Image: TgpObject;
  AMask: TBitmap;
  Tr, Transparent: Transparency;
  Obj: TObject;
begin
  Reformat := False;
  UName := Trim(Uppercase(Src));
  J := MissingImages.IndexOf(UName);
  if J >= 0 then
  begin
    Error := False;
    if BitmapList.IndexOf(UName) = -1 then
      // Bitmap not yet loaded: Add to BitmmapList.
      try
        AMask := nil;
        Transparent := NotTransp;
        Image := LoadImageFromStream(Stream, Transparent, AMask);
        if AMask <> nil then
          Tr := Transparent
        else
          Tr := NotTransp;
        Pair := TBitmapItem.Create(Image, AMask, Tr);
        try
          BitmapList.AddObject(UName, Pair); {put new bitmap in list}
          BitmapList.DecUsage(UName); {this does not count as being used yet}
        except {accept inability to create}
          Pair.Mask := nil;
          Pair.MImage := nil;
          Pair.Free;
        end;
      except
        Error := True;
      end;

    // Insert image into all TImageObj that are missing it.
    repeat
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
    until J = -1;
  end;
end;

//------------------------------------------------------------------------------
function ThtDocument.GetTheImage(const BMName: ThtString; var Transparent: Transparency;
  out AMask: TBitmap; out FromCache, Delay: boolean): TgpObject;
{Note: Bitmaps and Mask returned by this routine are on "loan".  Do not destroy them}
{Transparent may be set to NotTransp or LLCorner on entry but may discover it's TrGif or TrPng here}

  procedure GetTheBitmap;
  var
    Color: TColor;
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
  end;

  procedure GetTheStream;
  var
    Stream: TStream;
  begin
    if Assigned(GetImage) then
    begin {the OnImageRequest}
      Stream := nil;
      GetImage(TheOwner, BMName, Stream);
      if Stream = WaitStream then
        Delay := True
      else if Stream = ErrorStream then
        Result := nil
      else if Stream <> nil then
        try
          Result := LoadImageFromStream(Stream, Transparent, AMask);
        finally
          if Assigned(GottenImage) then
            GottenImage(TheOwner, BMName, Stream);
        end
      else
        Result := LoadImageFromFile(TheOwner.HtmlExpandFilename(BMName), Transparent, AMask);
    end;
  end;

  procedure GetTheBase64(Name: ThtString);
  var
    I: Integer;
    Source: TStream;
    Stream: TStream;
  begin
    I := Pos(';base64,', Name);
    if I >= 11 then
    begin
      // Firefox 11 saves multiline inline images by writing %0A for the linefeeds.
      // BTW: Internet Explorer 9 shows but does not save inline images at all.
      // Using StringReplace() here is a quick and dirty hack.
      // Better decode %encoded attribute values while reading the attributes.
      Name := StringReplace(Name, '%0A', #$0A, [rfReplaceAll]);
      Source := TStringStream.Create(Copy(Name, I + 8, MaxInt));
      try
        Stream := TMemoryStream.Create;
        try
          DecodeStream(Source, Stream);
          Result := LoadImageFromStream(Stream, Transparent, AMask);
        finally
          Stream.Free;
        end;
      finally
        Source.Free;
      end;
    end;
  end;

var
  UName, Name: ThtString;
  I: Integer;
  Pair: TBitmapItem;
  Tr: Transparency;
begin
  Result := nil;
  AMask := nil;
  Delay := False;
  FromCache := False;

  try
    if BMName <> '' then
    begin
      Name := htTrim(BMName);
      UName := htUpperCase(Name);
      I := BitmapList.IndexOf(UName); {first see if image is already loaded}
      if I >= 0 then
      begin {yes, image is already loaded}
        Result := BitmapList.GetImage(I);
        FromCache := True;
        if Result is TBitmap then
        begin
          Pair := BitmapList.Objects[I];
          case Pair.Transp of
            TrGif, TrPng:
              Transparent := Pair.Transp;
          else
            if Transparent = LLCorner then
            begin
              if not Assigned(Pair.Mask) then {1st bitmap may not have been marked transp}
                Pair.Mask := GetImageMask(TBitmap(Result), False, 0);
              if Assigned(Pair.Mask) then
                Pair.Transp := LLCorner;
            end;
          end;
          AMask := Pair.Mask;
        end;
        Exit;
      end;

    {The image is not loaded yet, need to get it}
      if Copy(Name, 1, 11) = 'data:image/' then
        GetTheBase64(Name)
      else if Assigned(GetBitmap) or Assigned(GetImage) then
      begin
        GetTheBitmap;
        GetTheStream;
      end
      else
        Result := LoadImageFromFile(BMName, Transparent, AMask);

      if Assigned(Result) then {put in Image List for use later also}
      begin
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
      end;
    end;
  except
    Result := nil;
  end;
end;

//------------------------------------------------------------------------------
function ThtDocument.FindSectionAtPosition(Pos: Integer; out TopPos, Index: Integer): TSectionBase;
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

procedure ThtDocument.GetBackgroundBitmap;
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
        Rslt := '';
        ExpandName(TheOwner, BitmapName, Rslt);
        BitmapName := Rslt;
      end;
      TmpResult := GetTheImage(BitmapName, Dummy1, Mask, FromCache, Delay); {might be Nil}
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

//------------------------------------------------------------------------------
function ThtDocument.GetFormcontrolData: TFreeList;
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

procedure ThtDocument.SetFormcontrolData(T: TFreeList);
var
  I: Integer;
begin
  try
    for I := 0 to T.Count - 1 do
      if htmlFormList.Count > I then
        ThtmlForm(htmlFormList[I]).SetFormData(ThtStringList(T[I]));
  except
  end;
end;

//------------------------------------------------------------------------------
function ThtDocument.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
begin
  Result := inherited FindDocPos(SourcePos, Prev);
  if Result < 0 then {if not found return 1 past last ThtChar}
    Result := Len;
end;

//------------------------------------------------------------------------------
function ThtDocument.CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean;
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

procedure ThtDocument.ProcessInlines(SIndex: Integer; Prop: TProperties; Start: boolean);
{called when an inline property is found to specify a border}
var
  I, EmSize, ExSize: Integer;
  Result: InlineRec;
  MargArrayO: TVMarginArray;
  Dummy1: Integer;
begin
 {$ifdef JPM_DEBUGGING}
 CodeSite.EnterMethod(Self,'ThtDocument.ProcessInlines');
 {$endif}
  with InlineList do
  begin
    if Start then
    begin {this is for border start}
      Result := InlineRec.Create;
      InlineList.Add(Result);
      with Result do
      begin
        StartBDoc := SIndex; {Source index for border start}
        IDB := Prop.ID; {property ID}
        EndB := 999999; {end isn't known yet}
        Prop.GetVMarginArray(MargArrayO);
        EmSize := Prop.EmSize;
        ExSize := Prop.ExSize;
        ConvMargArray(MargArrayO, 200, 200, EmSize, ExSize, 0{4}, Dummy1, MargArray);
      end;
    end
    else {this call has end information}
      for I := Count - 1 downto 0 do {the record we want is probably the last one}
      begin
        Result := InlineRec(Items[I]);
        if Prop.ID = Result.IDB then {check the ID to make sure}
        begin
          Result.EndBDoc := SIndex; {the source position of the border end}
          Break;
        end;
      end;
  end;
 {$ifdef JPM_DEBUGGING}
 CodeSite.ExitMethod(Self,'ThtDocument.ProcessInlines');
 {$endif}
end;

{----------------TInlineList.Create}

constructor TInlineList.Create(AnOwner: ThtDocument);
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

constructor TCellObj.Create(Master: ThtDocument; AVAlign: AlignmentType; Attr: TAttributeList; Prop: TProperties);
{Note: on entry Attr and Prop may be Nil when dummy cells are being created}
var
  I, AutoCount: Integer;
  Color: TColor;
  BackgroundImage: ThtString;
  Algn: AlignmentType;
  J: PropIndices;
begin
 {$ifdef JPM_DEBUGGING}
 CodeSite.EnterMethod(Self,'TCellObj.Create');
 {$endif}
  inherited Create;
  FCell := TCellObjCell.Create(Master);
  if Assigned(Prop) then
    Cell.Title := Prop.PropTitle;
  ColSpan := 1;
  RowSpan := 1;
  VAlign := AVAlign;
  if Assigned(Attr) then
    for I := 0 to Attr.Count - 1 do
      with Attr[I] do
        case Which of
          ColSpanSy:
            if Value > 1 then
              ColSpan := Value;

          RowSpanSy:
            if Value > 1 then
              RowSpan := Value;

          WidthSy:
            if Value >= 0 then
              FSpecWd := ToSpecWidth(Value, Name);

          HeightSy:
            if Value >= 0 then
              FSpecHt := ToSpecWidth(Value, Name);

          BGColorSy:
            Cell.BkGnd := ColorFromString(Name, False, Cell.BkColor);

          BackgroundSy:
            BackgroundImage := Name;

          HRefSy:
            Cell.Url := Name;

          TargetSy:
            Cell.Target := Name;

        end;

  if Assigned(Prop) then
  begin {Caption does not have Prop}
    if Prop.GetVertAlign(Algn) and (Algn in [Atop, AMiddle, ABottom]) then
      Valign := Algn;
    Prop.GetVMarginArray(MargArrayO, False);
    EmSize := Prop.EmSize;
    ExSize := Prop.ExSize;
    ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, 0, AutoCount, MargArray);
    if VarIsStr(MargArrayO[piWidth]) and (MargArray[piWidth] >= 0) then
      FSpecWd := ToSpecWidth(MargArray[piWidth], MargArrayO[piWidth]);
    if VarIsStr(MargArrayO[piHeight]) and (MargArray[piHeight] >= 0) then
      FSpecHt := ToSpecWidth(MargArray[piHeight], MargArrayO[piHeight]);

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
      Prop.GetBackgroundPos(EmSize, ExSize, FPRec);
    end;

  {In the following, Padding widths in percent aren't accepted}
    ConvMargArrayForCellPadding(MargArrayO, EmSize, ExSize, MargArray);
    FPad.Top := MargArray[PaddingTop];
    FPad.Right := MargArray[PaddingRight];
    FPad.Bottom := MargArray[PaddingBottom];
    FPad.Left := MargArray[PaddingLeft];

    HasBorderStyle := False;
    if BorderStyleType(MargArray[BorderTopStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Top := MargArray[BorderTopWidth];
    end;
    if BorderStyleType(MargArray[BorderRightStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Right := MargArray[BorderRightWidth];
    end;
    if BorderStyleType(MargArray[BorderBottomStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Bottom := MargArray[BorderBottomWidth];
    end;
    if BorderStyleType(MargArray[BorderLeftStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Left := MargArray[BorderLeftWidth];
    end;

    for J := BorderTopColor to BorderLeftColor do
      if MargArray[J] = IntNull {was: clNone} then
        MargArray[J] := clSilver;

    Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
    ShowEmptyCells := Prop.ShowEmptyCells;
  end;
 {$ifdef JPM_DEBUGGING}
 CodeSite.ExitMethod(Self,'TCellObj.Create');
 {$endif}
end;

constructor TCellObj.CreateCopy(AMasterList: ThtDocument; T: TCellObj);
begin
  inherited create;
  FCell := TCellObjCell.CreateCopy(AMasterList, T.Cell);
  Move(T.ColSpan, FColSpan, PtrSub(@Cell, @FColSpan));

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

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getBorderBottom: Integer;
begin
  Result := FBrd.Bottom;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getBorderLeft: Integer;
begin
  Result := FBrd.Left;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getBorderRight: Integer;
begin
  Result := FBrd.Right;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getBorderTop: Integer;
begin
  Result := FBrd.Top;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getPaddingBottom: Integer;
begin
  Result := FPad.Bottom;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getPaddingLeft: Integer;
begin
  Result := FPad.Left;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getPaddingRight: Integer;
begin
  Result := FPad.Right;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.getPaddingTop: Integer;
begin
  Result := FPad.Top;
end;

{----------------TCellObj.InitializeCell}

procedure TCellObj.Initialize(TablePadding: Integer; const BkImageName: ThtString;
  const APRec: PtPositionRec; Border: boolean);
begin
  if FPad.Top < 0 then
    FPad.Top := TablePadding;
  if FPad.Right < 0 then
    FPad.Right := TablePadding;
  if FPad.Bottom < 0 then
    FPad.Bottom := TablePadding;
  if FPad.Left < 0 then
    FPad.Left := TablePadding;
  if Border and not HasBorderStyle then // (BorderStyle = bssNone) then
  begin
    FBrd.Left := Max(1, FBrd.Left);
    FBrd.Right := Max(1, FBrd.Right);
    FBrd.Top := Max(1, FBrd.Top);
    FBrd.Bottom := Max(1, FBrd.Bottom);
  end;
  HzSpace := FPad.Left + FBrd.Left + FBrd.Right + FPad.Right;
  VrSpace := FPad.Top + FBrd.Top + FBrd.Bottom + FPad.Bottom;

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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellObj.DrawLogic2');
  CodeSite.SendFmtMsg('Y           = [%d]',[Y]);
  CodeSite.SendFmtMsg('CellSpacing = [%d]',[CellSpacing]);
  CodeSite.SendFmtMsg('Curs         = [%d]',[Curs]);
  CodeSite.AddSeparator;
   {$ENDIF}
  if Cell.Count > 0 then
  begin
    Tmp := Ht - VSize - (VrSpace + CellSpacing);
    case VAlign of
      ATop: YIndent := 0;
      AMiddle: YIndent := Tmp div 2;
      ABottom, ABaseline: YIndent := Tmp;
    end;
    Dummy := 0;
    Cell.DoLogic(Canvas, Y + FPad.Top + FBrd.Top + CellSpacing + YIndent, Wd - (HzSpace + CellSpacing),
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TCellObj.DrawLogic2');
   {$ENDIF}
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

begin
  YO := Y - Cell.MasterList.YOff;

  BL := X + CellSpacing; {Border left and right}
  BR := X + Wd;
  PL := BL + FBrd.Left; {Padding left and right}
  PR := BR - FBrd.Right;

  BT := YO + CellSpacing; {Border Top and Bottom}
  BB := YO + Ht;
  PT := BT + FBrd.Top; {Padding Top and Bottom}
  PB := BB - FBrd.Bottom;

  IT := Max(0, ARect.Top - 2 - PT);
  FT := Max(PT, ARect.Top - 2); {top of area drawn, screen coordinates}
  IH := Min(PB - FT, ARect.Bottom - FT); {height of area actually drawn}

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
      Canvas.Brush.Color := ThemedColor(Cell.BkColor) or PalRelative;
      Canvas.Brush.Style := bsSolid;
      if Cell.MasterList.IsCopy and ImgOK then
      begin
        InitFullBG(FullBG, PR - PL, IH,Cell.MasterList.IsCopy);
        FullBG.Canvas.Brush.Color := Cell.BkColor or PalRelative;
        FullBG.Canvas.Brush.Style := bsSolid;
        FullBG.Canvas.FillRect(Rect(0, 0, PR - PL, IH));
      end
      else
      begin
        {slip under border to fill gap when printing}
        BRect := Rect(PL, FT, PR, FT + IH);
        if not HasBorderStyle then // BorderStyle = bssNone then
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
            InflateRect(BRect, 1, 1);
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
          InitFullBG(FullBG,PR - PL, IH,Cell.MasterList.IsCopy);
          BitBlt(FullBG.Canvas.Handle,  0,  0, PR - PL, IH, Canvas.Handle, PL, FT, SrcCopy);
          BitBlt(FullBG.Canvas.Handle,  0,  0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SrcInvert);
          BitBlt(FullBG.Canvas.Handle,  0,  0, PR - PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
          BitBlt(FullBG.Canvas.Handle,  0,  0, PR - PL, IH, TBitmap(TiledImage).Canvas.Handle, 0, IT, SRCPaint);
          BitBlt(       Canvas.Handle, PL, FT, PR - PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
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
        InitFullBG(FullBG,PR - PL, IH,Cell.MasterList.IsCopy);
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
          Rgn := CreateRectRgn(BL - Point.X, Max(BT - Point.Y, -32000), BR - Point.X, Min(BB - Point.Y, 32000))
        else
          Rgn := CreateRectRgn(BL - Point.X, BT - Point.Y, BR - Point.X, BB - Point.Y)
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
          X + FPad.Left + FBrd.Left + CellSpacing,
          Y + FPad.Top + FBrd.Top + YIndent, ARect.Left, 0); {possibly should be IRgn.LfEdge}
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

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setBorderBottom(const Value: Integer);
begin
  FBrd.Bottom := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setBorderLeft(const Value: Integer);
begin
  FBrd.Left := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setBorderRight(const Value: Integer);
begin
  FBrd.Right := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setBorderTop(const Value: Integer);
begin
  FBrd.Top := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setPaddingBottom(const Value: Integer);
begin
  FPad.Bottom := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setPaddingLeft(const Value: Integer);
begin
  FPad.Left := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setPaddingRight(const Value: Integer);
begin
  FPad.Right := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.setPaddingTop(const Value: Integer);
begin
  FPad.Top := Value;
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
            SpecRowHeight := ToSpecWidth(Max(0, Min(Value, 100)), Name);
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

constructor TCellList.CreateCopy(AMasterList: ThtDocument; T: TCellList);
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
    case SpecRowHeight.VType of
      wtPercent:
        case CellObj.FSpecHt.VType of
          wtPercent:
            if CellObj.FSpecHt.Value < SpecRowHeight.Value then
              CellObj.FSpecHt.Value := SpecRowHeight.Value;

          wtNone,
          wtRelative: // percentage is stronger
            CellObj.FSpecHt := SpecRowHeight;

        else
          // keep specified absolute value
        end;

      wtRelative:
        case CellObj.FSpecHt.VType of
          wtPercent: ; // percentage is stronger

          wtNone:
            CellObj.FSpecHt := SpecRowHeight;

          wtRelative:
            if CellObj.FSpecHt.Value < SpecRowHeight.Value then
              CellObj.FSpecHt.Value := SpecRowHeight.Value;
        else
          // keep specified absolute value
        end;

      wtAbsolute:
        case CellObj.FSpecHt.VType of
          wtAbsolute:
            if CellObj.FSpecHt.Value < SpecRowHeight.Value then
              CellObj.FSpecHt.Value := SpecRowHeight.Value;
        else
          // absolute value is stronger
          CellObj.FSpecHt := SpecRowHeight;
        end;
    end;
  end;
end;

{----------------TCellList.Initialize}

procedure TCellList.Initialize;
var
  I: Integer;
begin
  if BkGnd then
    for I := 0 to Count - 1 do
      with Items[I].Cell do
        if not BkGnd then
        begin
          BkGnd := True;
          BkColor := Self.BkColor;
        end;
end;

{----------------TCellList.DrawLogic1}

function TCellList.DrawLogic1(Canvas: TCanvas; const Widths: IntArray; Span,
  CellSpacing, AHeight, Rows: Integer; out Desired: Integer; out Spec, More: boolean): Integer;
{Find vertical size of each cell, Row height of this row.  But final Y position
 is not known at this time.
 Rows is number rows in table.
 AHeight is for calculating percentage heights}
var
  I, Dummy: Integer;
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
          Wd := Sum(Widths, I, I + ColSpan - 1); {accumulate column widths}
          if Span = RowSpan then
          begin
            Dummy := 0;
            case SpecHt.VType of
              wtAbsolute:
                GuessHt := Trunc(SpecHt.Value);

              wtPercent:
                GuessHt := Trunc(SpecHt.Value * AHeight / 1000.0);
            else
              GuessHt := 0;
            end;

            if (GuessHt = 0) and (Rows = 1) then
              GuessHt := AHeight;

            VSize := Cell.DoLogic(Canvas, 0, Wd - HzSpace - CellSpacing, Max(0, GuessHt - VrSpace), 0, Dummy, DummyCurs);
            Result := Max(Result, VSize + VrSpace);

            case SpecHt.VType of
              wtAbsolute:
              begin
                Result := Max(Result, Max(VSize, Trunc(SpecHt.Value)));
                Spec := True;
              end;

              wtPercent:
              begin
                Desired := Max(Desired, GuessHt);
                Spec := True;
              end;
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellObj.DrawLogic2');
  CodeSite.SendFmtMsg('Y           = [%d]',[Y]);
  CodeSite.SendFmtMsg('CellSpacing = [%d]',[CellSpacing]);
  CodeSite.SendFmtMsg('Curs         = [%d]',[Curs]);
  CodeSite.AddSeparator;
   {$ENDIF}
  for I := 0 to Count - 1 do
  begin
    CellObj := TCellObj(Items[I]);
    if Assigned(CellObj) then
      CellObj.DrawLogic2(Canvas, Y, CellSpacing, Curs);
  end;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Curs         = [%d]',[Curs]);
  CodeSite.ExitMethod(Self,'TCellObj.DrawLogic2');
   {$ENDIF}
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
function TCellList.getCellObj(Index: Integer): TCellObj;
begin
  Result := TCellObj(inherited Items[Index]);
end;

{----------------TCellList.Draw}

function TCellList.Draw(Canvas: TCanvas; MasterList: ThtDocument; const ARect: TRect; const Widths: IntArray;
  X, Y, YOffset, CellSpacing: Integer; Border: boolean; Light, Dark: TColor; MyRow: Integer): Integer;
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
      CellObj := Items[I];
      if (CellObj <> nil) and (CellObj.ColSpan > 0) and (CellObj.RowSpan > 0) then
        CellObj.Draw(Canvas, ARect, X, Y, CellSpacing, Border, Light, Dark);
      X := X + Widths[I];
    end;
end;

{----------------THtmlTable.Create}

constructor THtmlTable.Create(Master: ThtDocument; Attr: TAttributeList;
  Prop: TProperties);
var
  I: Integer;
begin
  //BG, 08.06.2010: TODO:  Issue 5: Table border versus stylesheets:
  //  Added: BorderColor
  inherited Create(Master, Prop);
  Rows := TRowList.Create;
  CellPadding := 1;
  CellSpacing := 2;
  HasBorderWidth := Prop.HasBorderWidth;
  BorderColor := clNone;
  BorderColorLight := clBtnHighLight;
  BorderColorDark := clBtnShadow;
  for I := 0 to Attr.Count - 1 do
    with Attr[I] do
      case Which of
        BorderSy:
          //BG, 15.10.2010: issue 5: set border width only, if style does not set any border width:
          if not HasBorderWidth then
          begin
            if Name = '' then
              BorderWidth := 1
            else
              BorderWidth := Min(100, Max(0, Value)); {Border=0 is no border}
            HasBorderWidth := True;
            brdWidthAttr := BorderWidth;
          end;

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

constructor THtmlTable.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
var
  I: Integer;
  HtmlTable: THtmlTable absolute T;
begin
  assert(T is THtmlTable);
  inherited;
  Rows := TRowList.Create;
  for I := 0 to HtmlTable.Rows.Count - 1 do
    Rows.Add(TCellList.CreateCopy(Document, {OwnerCell.OwnerBlock,} HtmlTable.Rows[I]));

  Move(HtmlTable.Initialized, Initialized, PtrSub(@EndList, @Initialized));

  SetLength(Widths, NumCols);
  SetLength(MaxWidths, NumCols);
  SetLength(MinWidths, NumCols);
  SetLength(Percents, NumCols);
  SetLength(Multis, NumCols);
  SetLength(ColumnSpecs, NumCols);

  if HtmlTable.FColSpecs <> nil then
  begin
    FColSpecs := TColSpecList.Create;
    for I := 0 to HtmlTable.FColSpecs.Count - 1 do
      FColSpecs.Add(TColSpec.CreateCopy(HtmlTable.FColSpecs[I]));
  end;

  if Document.PrintTableBackground then
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
  FreeAndNil(FColSpecs);
  inherited Destroy;
end;

{----------------THtmlTable.DoColumns}

procedure THtmlTable.DoColumns(Count: Integer; const SpecWidth: TSpecWidth; VAlign: AlignmentType; const Align: ThtString);
{add the <col> / <colgroup> info to the Cols list}
var
  I: Integer;
begin
  if FColSpecs = nil then
    FColSpecs := TColSpecList.Create;
  Count := Min(Count, 10000);
  for I := 0 to Count - 1 do
    FColSpecs.Add(TColSpec.Create(SpecWidth, Align, VAlign));
end;

{----------------THtmlTable.AddDummyCells}

procedure THtmlTable.Initialize;

  function DummyCell(RSpan: Integer): TCellObj;
  begin
    Result := TCellObj.Create(Document, ATop, nil, nil);
    Result.ColSpan := 0;
    Result.RowSpan := RSpan;
    if BkGnd then {transfer bgcolor to cell if no Table image}
    begin
      Result.Cell.BkGnd := True;
      Result.Cell.BkColor := BkColor;
    end;
  end;

  procedure AddDummyCellsForColSpansAndInitializeCells;
  var
    Cl, Rw, RowCount, K: Integer;
    Row: TCellList;
    CellObj: TCellObj;
  begin
    {initialize cells and put dummy cells in rows to make up for ColSpan > 1}
    NumCols := 0;
    RowCount := Rows.Count;
    for Rw := 0 to RowCount - 1 do
    begin
      Row := Rows[Rw];
      Row.Initialize;
      for Cl := Row.Count - 1 downto 0 do
      begin
        CellObj := Row[Cl];
        CellObj.Initialize(CellPadding, Row.BkImage, Row.APRec, Self.BorderWidth > 0);
        if BkGnd and not CellObj.Cell.BkGnd then {transfer bgcolor to cells if no Table image}
        begin
          CellObj.Cell.BkGnd := True;
          CellObj.Cell.BkColor := BkColor;
        end;
        CellObj.RowSpan := Min(CellObj.RowSpan, RowCount - Rw); {So can't extend beyond table}
        for K := Cl + 1 to Cl + CellObj.ColSpan - 1 do
          if CellObj.RowSpan > 1 then
            Row.Insert(K, DummyCell(CellObj.RowSpan)) {these could be
            Nil also except they're needed for expansion in the next section}
          else
            Row.Insert(K, DummyCell(1));
      end;
      NumCols := Max(NumCols, Row.Count); {temporary # cols}
    end;
  end;

  procedure AddDummyCellsForRowSpans;
  var
    Cl, Rw, RowCount, K: Integer;
    Row: TCellList;
    CellObj: TCellObj;
  begin
    RowCount := Rows.Count;
    for Cl := 0 to NumCols - 1 do
      for Rw := 0 to RowCount - 1 do
      begin
        Row := Rows[Rw];
        if Row.Count > Cl then
        begin
          CellObj := Row[Cl];
          if CellObj <> nil then
          begin
            CellObj.RowSpan := Min(CellObj.RowSpan, RowCount - Rw); {practical limit}
            if CellObj.RowSpan > 1 then
              for K := Rw + 1 to Rw + CellObj.RowSpan - 1 do
              begin {insert dummy cells in following rows if RowSpan > 1}
                while Rows[K].Count < Cl do {add padding if row is short}
                  Rows[K].Add(DummyCell(0));
                Rows[K].Insert(Cl, DummyCell(0));
              end;
          end;
        end;
      end;

    NumCols := 0;
    for Rw := 0 to Rows.Count - 1 do
      NumCols := Max(NumCols, Rows[Rw].Count);
  end;

  procedure AddDummyCellsForUnequalRowLengths;
  var
    Cl: Integer;
    CellObj: TCellObj;
    Row: TCellList;

    function IsLastCellOfRow(): Boolean;
    begin
      // Is Row[Cl] resp. CellObj a cell in this row? (Cl >= 0)
      // With respect to rowspans from previous rows is it the last one? (Cl + CellObj.ColSpan >= NumCols)
      Result := (Cl >= 0) and (Cl + CellObj.ColSpan >= Row.Count);
    end;

  var
    Rw, I, K: Integer;
  begin
    Rw := 0;
    while Rw < Rows.Count do
    begin
      Row := Rows[Rw];
      Cl := -1;
      if Row.Count < NumCols then
      begin
        // this row is too short

        // find the spanning column
        Cl := Row.Count - 1;
        while Cl >= 0 do
        begin
          CellObj := Row[Cl];
          if CellObj.ColSpan > 0 then
            break;
          Dec(Cl);
        end;

        if IsLastCellOfRow then
        begin
          // widen this cell
          for I := Row.Count to NumCols - 1 do
          begin
            Row.Add(DummyCell(CellObj.RowSpan));
            for K := Rw + 1 to Rw + CellObj.RowSpan - 1 do
              Rows[K].Add(DummyCell(0));
          end;
          CellObj.ColSpan := NumCols - Cl;
        end;
      end;
      
      // continue with next row not spanned by this cell.
      if (Cl >= 0) and (CellObj.RowSpan > 0) then
        Inc(Rw, CellObj.RowSpan)
      else
        Inc(Rw);
    end;
  end;

var
  Cl, Rw, MaxColSpan, MaxRowSpan: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  if not Initialized then
  begin
    AddDummyCellsForColSpansAndInitializeCells;
    AddDummyCellsForRowSpans;
    AddDummyCellsForUnequalRowLengths;

    for Rw := 0 to Rows.Count - 1 do
    begin
      MaxRowSpan := Rows.Count - Rw;
      Row := Rows[Rw];
      for Cl := 0 to Row.Count - 1 do
      begin
        MaxColSpan := NumCols - Cl;
        CellObj := Row[Cl];

        // Reduce excessive colspans.
        if CellObj.ColSpan > MaxColSpan then
          CellObj.ColSpan := MaxColSpan;

        // Reduce excessive rowspans.
        if CellObj.RowSpan > MaxRowSpan then
          CellObj.RowSpan := MaxRowSpan;
      end;
    end;

    SetLength(Widths, NumCols);
    SetLength(MaxWidths, NumCols);
    SetLength(MinWidths, NumCols);
    SetLength(Percents, NumCols);
    SetLength(Multis, NumCols);
    SetLength(ColumnSpecs, NumCols);

    Initialized := True;
  end; {if not ListsProcessed}
end;

procedure THtmlTable.IncreaseWidthsByWidth(WidthType: TWidthType; var Widths: IntArray;
  StartIndex, EndIndex, Required, Spanned, Count: Integer);
// Increases width of spanned columns relative to given widths.
var
  I, OldWidth, NewWidth: Integer;
begin
  OldWidth := 0;
  NewWidth := 0;
  for I := EndIndex downto StartIndex do
    if ColumnSpecs[I] = WidthType then
      if Count > 1 then
      begin
        // building sum of all processed columns avoids rounding errors.
        Inc(OldWidth, Widths[I]);
        Widths[I] := MulDiv(OldWidth, Required, Spanned) - NewWidth;
        Inc(NewWidth, Widths[I]);
        Dec(Count);
      end
      else
      begin
        // The remaining pixels are the new first column's width.
        Widths[I] := Required - NewWidth;
        break;
      end;
end;

procedure THtmlTable.IncreaseWidthsByPercentage(var Widths: IntArray;
  StartIndex, EndIndex, Required, Spanned, Percent, Count: Integer);
// Increases width of spanned columns relative to given percentage.
var
  Excess, AddedExcess, AddedPercent, I, Add: Integer;
begin
  Excess := Required - Spanned;
  AddedExcess := 0;
  AddedPercent := 0;
  for I := EndIndex downto StartIndex do
    if ColumnSpecs[I] = wtPercent then
      if Count > 1 then
      begin
        Inc(AddedPercent, Percents[I]);
        Add := MulDiv(Excess, AddedPercent, Percent) - AddedExcess;
        Inc(Widths[I], Add);
        Inc(AddedExcess, Add);
        Dec(Count);
      end
      else
      begin
        // add the remaining pixels to the first column's width.
        Inc(Widths[I], Excess - AddedExcess);
        break;
      end;
end;

procedure THtmlTable.IncreaseWidthsByMinMaxDelta(WidthType: TWidthType; var Widths: IntArray;
  StartIndex, EndIndex, Excess, DeltaWidth, Count: Integer; const Deltas: IntArray);
// Increases width of spanned columns relative to difference between min and max widths.
var
  AddedExcess, AddedDelta, I, Add: Integer;
begin
  AddedExcess := 0;
  AddedDelta := 0;
  for I := EndIndex downto StartIndex do
    if ColumnSpecs[I] = WidthType then
      if Count > 1 then
      begin
        Inc(AddedDelta, Deltas[I]);
        Add := MulDiv(Excess, AddedDelta, DeltaWidth) - AddedExcess;
        Inc(Widths[I], Add);
        Inc(AddedExcess, Add);
        Dec(Count);
      end
      else
      begin
        // add the remaining pixels to the first column's width.
        Inc(Widths[I], Excess - AddedExcess);
        break;
      end;
end;

procedure THtmlTable.IncreaseWidthsRelatively(
  var Widths: IntArray;
  StartIndex, EndIndex, Required, SpannedMultis: Integer; ExactRelation: Boolean);
// Increases width of spanned columns according to relative columns specification.
// Does not touch columns specified by percentage or absolutely.
var
  RequiredWidthFactor: Double;
  Count, I, AddedWidth, AddedMulti: Integer;
begin
  // Some columns might have Multi=0. Don't widen these. Thus remove their width from Required.
  // Some columns might be wider than required. Widen all columns to preserve the relations.
  RequiredWidthFactor := 0;
  Count := 0;
  for I := EndIndex downto StartIndex do
    if ColumnSpecs[I] = wtRelative then
      if Multis[I] > 0 then
      begin
        Inc(Count);
        if ExactRelation then
          RequiredWidthFactor := Max(RequiredWidthFactor, Widths[I] / Multis[I]);
      end
      else
      begin
        Dec(Required, Widths[I]);
      end;

  RequiredWidthFactor := Max(RequiredWidthFactor, Required / SpannedMultis); // 100 times width of 1*.
  Required := Min(Required, Trunc(RequiredWidthFactor * SpannedMultis)); // don't exceed given requirement.
  // building sum of all processed columns to reduce rounding errors.
  AddedWidth := 0;
  AddedMulti := 0;
  for I := EndIndex downto StartIndex do
    if (ColumnSpecs[I] = wtRelative) and (Multis[I] > 0) then
      if Count > 1 then
      begin
        Inc(AddedMulti, Multis[I]);
        Widths[I] := Trunc(AddedMulti * RequiredWidthFactor) - AddedWidth;
        Inc(AddedWidth, Widths[I]);
        Dec(Count);
      end
      else
      begin
        // The remaining pixels are the new first column's width.
        Widths[I] := Required - AddedWidth;
        break;
      end;
end;

procedure THtmlTable.IncreaseWidthsEvenly(WidthType: TWidthType; var Widths: IntArray;
  StartIndex, EndIndex, Required, Spanned, Count: Integer);
// Increases width of spanned columns of given type evenly.
var
  Divisor, RemainingWidth, I: Integer;
begin
  Divisor := Count;
  RemainingWidth := Required;
  for I := EndIndex downto StartIndex do
    if ColumnSpecs[I] = WidthType then
      if Count > 1 then
      begin
        Dec(Count);
        // MulDiv for each column instead of 1 precalculated width for all columns avoids round off errors.
        Widths[I] := RemainingWidth - MulDiv(Required, Count, Divisor);
        Dec(RemainingWidth, Widths[I]);
      end
      else
      begin
        // add the remaining pixels to the first column's width.
        Widths[I] := RemainingWidth;
        break;
      end;
end;

{----------------THtmlTable.GetWidths}

procedure THtmlTable.GetMinMaxWidths(Canvas: TCanvas; TheWidth: Integer);
// calculate MaxWidths and MinWidths of all columns.

  procedure UpdateColumnSpec(var Counts: TIntegerPerWidthType; var OldType: TWidthType; NewType: TWidthType);
  begin
    // update to stonger spec only:
    case NewType of
      wtAbsolute: if OldType in [wtAbsolute] then Exit;
      wtPercent:  if OldType in [wtAbsolute, wtPercent] then Exit;
      wtRelative: if OldType in [wtAbsolute, wtPercent, wtRelative] then Exit;
    else
      // wtNone:
      Exit;
    end;

    // at this point: NewType is stronger than OldType
    Dec(Counts[OldType]);
    Inc(Counts[NewType]);
    OldType := NewType;
  end;

  procedure UpdateRelativeWidths(var Widths: IntArray; const ColumnSpecs: TWidthTypeArray; StartIndex, EndIndex: Integer);
  // Increases width of spanned columns according to relative columns specification.
  // Does not touch columns specified by percentage or absolutely.
  var
    RequiredWidthFactor, Count, Multi, I, Required, AddedWidth, AddedMulti: Integer;
  begin
    // Some columns might be wider than required. Widen all columns to preserve the relations.
    RequiredWidthFactor := 100;
    Count := 0;
    Multi := 0;
    for I := EndIndex downto StartIndex do
      if (ColumnSpecs[I] = wtRelative) and (Multis[I] > 0) then
      begin
        Inc(Count);
        Inc(Multi, Multis[I]);
        RequiredWidthFactor := Max(RequiredWidthFactor, MulDiv(Widths[I], 100, Multis[I]));
      end;

    Required := MulDiv(RequiredWidthFactor, Multi, 100);
    // building sum of all processed columns to reduce rounding errors.
    AddedWidth := 0;
    AddedMulti := 0;
    for I := EndIndex downto StartIndex do
      if (ColumnSpecs[I] = wtRelative) and (Multis[I] > 0) then
        if Count > 1 then
        begin
          Inc(AddedMulti, Multis[I]);
          Widths[I] := MulDiv(AddedMulti, RequiredWidthFactor, 100) - AddedWidth;
          Inc(AddedWidth, Widths[I]);
          Dec(Count);
        end
        else
        begin
          // add the remaining pixels to the first column's width.
          Widths[I] := Required - AddedWidth;
          break;
        end;
  end;

var
  // calculated values:
  CellSpec: TWidthType;
  CellMin, CellMax, CellPercent, CellRel: Integer;
  SpannedMin, SpannedMax, SpannedMultis, SpannedPercents: Integer;
  SpannedCounts: TIntegerPerWidthType;

  procedure IncreaseMinMaxWidthsEvenly(WidthType: TWidthType; StartIndex, EndIndex: Integer);
  var
    Untouched: Integer;
  begin
    if CellMin > SpannedMin then
    begin
      Untouched := SumOfNotType(WidthType, ColumnSpecs, MinWidths, StartIndex, EndIndex);
      IncreaseWidthsEvenly(WidthType, MinWidths, StartIndex, EndIndex, CellMin - Untouched, SpannedMin - Untouched, SpannedCounts[WidthType]);
    end;
    if CellMax > SpannedMax then
    begin
      Untouched := SumOfNotType(WidthType, ColumnSpecs, MinWidths, StartIndex, EndIndex);
      IncreaseWidthsEvenly(WidthType, MaxWidths, StartIndex, EndIndex, CellMax - Untouched, SpannedMax - Untouched, SpannedCounts[WidthType]);
    end;
  end;

  procedure IncreaseMinMaxWidthsByMinMaxDelta(WidthType: TWidthType; StartIndex, EndIndex: Integer);
  var
    Deltas: IntArray;
  begin
    Deltas := SubArray(MaxWidths, MinWidths);
    if CellMin > SpannedMin then
      IncreaseWidthsByMinMaxDelta(WidthType, MinWidths, StartIndex, EndIndex, CellMin - SpannedMin, SpannedMax - SpannedMin, SpannedCounts[WidthType], Deltas);
    if CellMax > SpannedMax then
      IncreaseWidthsByMinMaxDelta(WidthType, MaxWidths, StartIndex, EndIndex, CellMax - SpannedMax, SpannedMax - SpannedMin, SpannedCounts[WidthType], Deltas);
  end;

var
  //
  I, J, K, Span, EndIndex: Integer;
  Cells: TCellList;
  CellObj: TCellObj;
  MaxSpans: IntArray;
  MaxSpan: Integer;
  MultiCount: Integer;
begin
  // initialize default widths
  SetArray(ColumnCounts, 0);
  if FColSpecs <> nil then
    J := FColSpecs.Count
  else
    J := 0;
  MultiCount := 0;
  for I := 0 to NumCols - 1 do
  begin
    MinWidths[I] := 0;
    MaxWidths[I] := 0;
    Percents[I] := 0;
    Multis[I] := 0;
    if I < J then
      with FColSpecs[I].FWidth do
      begin
        ColumnSpecs[I] := VType;
        Inc(ColumnCounts[VType]);
        case VType of
          wtAbsolute:
          begin
            MinWidths[I] := Value;
            MaxWidths[I] := Value;
          end;

          wtPercent:
            Percents[I] := Value;

          wtRelative:
          begin
            Multis[I] := Value;
            if Value > 0 then
              Inc(MultiCount);
          end;
        end;
      end;
  end;

  SetLength(Heights, 0);
  Span := 1;

  //BG, 29.01.2011: data for loop termination and to speed up looping through
  //  very large tables with large spans.
  //  A table with 77 rows and 265 columns and a MaxSpan of 265 in 2 rows
  //  was processed in 3 seconds before the tuning and 80ms afterwards.
  MaxSpan := 1;
  SetLength(MaxSpans, Rows.Count);
  SetArray(MaxSpans, MaxSpan);
  repeat
    for J := 0 to Rows.Count - 1 do
    begin
      //BG, 29.01.2011: tuning: process rows only, if there is at least 1 cell to process left.
      if Span > MaxSpans[J] then
        continue;

      Cells := Rows[J];
      //BG, 29.01.2011: tuning: process up to cells only, if there is at least 1 cell to process left.
      for I := 0 to Cells.Count - Span do
      begin
        CellObj := Cells[I];
        if CellObj = nil then
          continue;

        if CellObj.ColSpan = Span then
        begin
          // get min and max width of this cell:
          CellObj.Cell.MinMaxWidth(Canvas, CellMin, CellMax);
          CellPercent := 0;
          CellRel := 0;
          with CellObj.SpecWd do
          begin
            CellSpec := VType;
            case VType of
              wtPercent:
                CellPercent := Value;

              wtAbsolute:
              begin
                CellMin := Max(CellMin, Value);
                CellMax := Max(CellMax, Value);
              end;

              wtRelative:
                CellRel := Value;
            end;
          end;
          Inc(CellMin, CellSpacing + CellObj.HzSpace);
          Inc(CellMax, CellSpacing + CellObj.HzSpace);

          if Span = 1 then
          begin
            MinWidths[I] := Max(MinWidths[I], CellMin);
            MaxWidths[I] := Max(MaxWidths[I], CellMax);
            Percents[I] := Max(Percents[I], CellPercent); {collect percents}
            Multis[I] := Max(Multis[I], CellRel);
            UpdateColumnSpec(ColumnCounts, ColumnSpecs[I], CellSpec);
          end
          else
          begin
            EndIndex := I + Span - 1;

            // Get current min and max width of spanned columns.
            SpannedMin := Sum(MinWidths, I, EndIndex);
            SpannedMax := Sum(MaxWidths, I, EndIndex);

            if (CellMin > SpannedMin) or (CellMax > SpannedMax) then
            begin
              { As spanning cell is wider than sum of spanned columns, we must widen the spanned columns.

                How to add the excessive width:
                a) If cell spans columns without any width specifications, then spread excessive width evenly to these columns.
                b) If cell spans columns with relative specifications, then spread excessive width according
                                                               to relative width values to these columns.
                c) If cell spans columns with precentage specifications, then spread excessive width relative
                                                               to percentages to these columns.
                d) If cell spans columns with absolute specifications only, then spread excessive width relative
                                                               to difference between MinWidth and MaxWidth to all columns.

                see also:
                - http://www.w3.org/TR/html401/struct/tables.html
                - http://www.w3.org/TR/html401/appendix/notes.html#h-B.5.2
                Notice:
                - Fixed Layout: experiments showed that IExplore and Firefox *do* respect width attributes of <td> and <th>
                  even if there was a <colgroup> definition although W3C specified differently.
              }
              CountsPerType(SpannedCounts, ColumnSpecs, I, EndIndex);

              if CellPercent > 0 then
              begin
                SpannedPercents := SumOfType(wtPercent, ColumnSpecs, Percents, I, EndIndex);
                if SpannedPercents > CellPercent then
                  continue;

                // BG, 05.02.2012: spread excessive percentage over unspecified columns:
                if SpannedCounts[wtNone] > 0 then
                begin
                  // a) There is at least 1 column without any width constraint: Widen this/these.
                  IncreaseWidthsEvenly(wtNone, Percents, I, EndIndex, CellPercent - SpannedPercents, 0, SpannedCounts[wtNone]);
                  for K := I to EndIndex do
                    ColumnSpecs[K] := wtPercent;
                  continue;
                end
              end;

              if SpannedCounts[wtNone] > 0 then
              begin
                // a) There is at least 1 column without any width constraint: Widen this/these.
                IncreaseMinMaxWidthsEvenly(wtNone, I, EndIndex);
              end
              else if SpannedCounts[wtRelative] > 0 then
              begin
                // b) There is at least 1 column with relative width: Widen this/these.
                SpannedMultis := SumOfType(wtRelative, ColumnSpecs, Multis, I, EndIndex);
                if SpannedMultis > 0 then
                begin
                  if CellMin > SpannedMin then
                    IncreaseWidthsRelatively(MinWidths, I, EndIndex, CellMin, SpannedMultis, True);
                  if CellMax > SpannedMax then
                    IncreaseWidthsRelatively(MaxWidths, I, EndIndex, CellMax, SpannedMultis, True);
                end
                else if SpannedMax > SpannedMin then
                begin
                  // All spanned columns are at 0*.
                  // Widen columns proportional to difference between yet evaluated min and max width.
                  // This ought to fill the table with least height requirements.
                  IncreaseMinMaxWidthsByMinMaxDelta(wtRelative, I, EndIndex);
                end
                else
                begin
                  // All spanned columns are at 0* and minimum = maximum. Spread excess evenly.
                  IncreaseMinMaxWidthsEvenly(wtRelative, I, EndIndex);
                end;
              end
              else if SpannedCounts[wtPercent] > 0 then
              begin
                // c) There is at least 1 column with percentage width: Widen this/these.
                if SpannedMax > SpannedMin then
                begin
                  // Widen columns proportional to difference between yet evaluated min and max width.
                  // This ought to fill the table with least height requirements.
                  IncreaseMinMaxWidthsByMinMaxDelta(wtPercent, I, EndIndex);
                end
                else
                begin
                  SpannedPercents := SumOfType(wtPercent, ColumnSpecs, Percents, I, EndIndex);
                  if SpannedPercents > 0 then
                  begin
                    // Spread excess to columns proportionally to their percentages.
                    // This ought to keep smaller columns small.
                    if CellMin > SpannedMin then
                      IncreaseWidthsByPercentage(MinWidths, I, EndIndex, CellMin, SpannedMin, SpannedPercents, SpannedCounts[wtPercent]);
                    if CellMax > SpannedMax then
                      IncreaseWidthsByPercentage(MaxWidths, I, EndIndex, CellMax, SpannedMax, SpannedPercents, SpannedCounts[wtPercent]);
                  end
                  else
                  begin
                    // All spanned columns are at 0% and minimum = maximum. Spread excess evenly.
                    IncreaseMinMaxWidthsEvenly(wtPercent, I, EndIndex);
                  end;
                end;
              end
              else
              begin
                // d) All columns have absolute widths: Widen these.
                IncreaseMinMaxWidthsEvenly(wtAbsolute, I, EndIndex);
              end;
            end;
          end;
        end
        else
        begin
          //BG, 29.01.2011: at this point: CellObj.ColSpan <> Span
          if Span = 1 then
          begin
            //BG, 29.01.2011: at this point: in the first loop with a CellObj.ColSpan > 1.

            // Collect data for termination and tuning.
            if MaxSpans[J] < CellObj.ColSpan then
            begin
              MaxSpans[J] := CellObj.ColSpan; // data for tuning
              if MaxSpan < MaxSpans[J] then
                MaxSpan := MaxSpans[J]; // data for termination
            end;
          end;
        end;
      end;
    end;
    Inc(Span);
  until Span > MaxSpan;

  if MultiCount > 0 then
  begin
    UpdateRelativeWidths(MinWidths, ColumnSpecs, 0, NumCols - 1);
    UpdateRelativeWidths(MaxWidths, ColumnSpecs, 0, NumCols - 1);
  end;
end;

{----------------THtmlTable.MinMaxWidth}

procedure THtmlTable.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
begin
  Initialize; {in case it hasn't been done}
  GetMinMaxWidths(Canvas, tblWidthAttr);
  Min := Math.Max(Sum(MinWidths) + CellSpacing, tblWidthAttr);
  Max := Math.Max(Sum(MaxWidths) + CellSpacing, tblWidthAttr);
end;

{----------------THtmlTable.DrawLogic}

function THtmlTable.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;

  function FindTableWidth: Integer;

    procedure IncreaseWidths(WidthType: TWidthType; MinWidth, NewWidth, Count: Integer);
    var
      Deltas: IntArray;
      D, W: Integer;
    begin
      Deltas := SubArray(MaxWidths, MinWidths);
      D := SumOfType(WidthType, ColumnSpecs, Deltas, 0, NumCols - 1);
      if D <> 0 then
        IncreaseWidthsByMinMaxDelta(WidthType, Widths, 0, NumCols - 1, NewWidth - MinWidth, D, Count, Deltas)
      else
      begin
        W := SumOfType(WidthType, ColumnSpecs, Widths, 0, NumCols -1);
        IncreaseWidthsByWidth(WidthType, Widths, 0, NumCols - 1, NewWidth - MinWidth + W, W, Count);
      end;
    end;

    procedure CalcPercentDeltas(var PercentDeltas: IntArray; NewWidth: Integer);
    var
      I: Integer;
      Percent, PercentDelta: Integer;
    begin
      Percent := Max(1000, Sum(Percents));
      for I := NumCols - 1 downto 0 do
        if ColumnSpecs[I] = wtPercent then
        begin
          PercentDelta := Trunc(1000 * (Percents[I] / Percent - MinWidths[I] / NewWidth));
          if PercentDelta > 0 then
            PercentDeltas[I] := PercentDelta;
        end;
    end;

    procedure IncreaseWidthsByPercentage(var Widths: IntArray;
      const PercentDeltas: IntArray;
      StartIndex, EndIndex, Required, Spanned, Percent, Count: Integer);
    // Increases width of columns relative to given percentages.
    var
      Excess, AddedExcess, AddedPercent, I, Add, MaxColWidth, SpecPercent: Integer;
    begin
      SpecPercent := Max(1000, Sum(Percents));
      Excess := Required - Spanned;
      AddedExcess := 0;
      AddedPercent := 0;
      for I := EndIndex downto StartIndex do
        if (ColumnSpecs[I] = wtPercent) and (PercentDeltas[I] > 0) then
          if Count > 1 then
          begin
            Inc(AddedPercent, PercentDeltas[I]);
            Add := MulDiv(Excess, AddedPercent, Percent) - AddedExcess;
            MaxColWidth := MulDiv(Required, Percents[I], SpecPercent);
            Widths[I] := Min(Widths[I] + Add, MaxColWidth);
            Inc(AddedExcess, Add);
            Dec(Count);
          end
          else
          begin
            // add the remaining pixels to the first column's width.
            Add := Excess - AddedExcess;
            MaxColWidth := MulDiv(Required, Percents[I], SpecPercent);
            Widths[I] := Min(Widths[I] + Add, MaxColWidth);
            break;
          end;
    end;

  var
    Specified: boolean;
    NewWidth, MaxWidth, MinWidth, D, W, I: Integer;
    Counts: TIntegerPerWidthType;
    PercentDeltas: IntArray;
    PercentAbove0Count: Integer;
    PercentDeltaAbove0Count: Integer;
  begin
    Specified := tblWidthAttr > 0;
    if Specified then
      NewWidth := tblWidthAttr
    else
      NewWidth := IMgr.RightSide(Y) - IMgr.LeftIndent(Y);
    Dec(NewWidth, CellSpacing);

    Initialize;

    {Figure the width of each column}
    GetMinMaxWidths(Canvas, NewWidth);
    MinWidth := Sum(MinWidths);
    MaxWidth := Sum(MaxWidths);

    {fill in the Widths array}
    if MinWidth >= NewWidth then
      // The minimum table width fits exactly or is too wide. Thus use minimum widths, table might expand.
      Widths := Copy(MinWidths)
    else
    begin
      // Table fits into NewWidth.

      Counts[wtPercent] := 0;
      PercentAbove0Count := 0;
      for I := 0 to NumCols - 1 do
        if ColumnSpecs[I] = wtPercent then
        begin
          Inc(Counts[wtPercent]);
          if Percents[I] > 0 then
            Inc(PercentAbove0Count);
        end;

      Widths := Copy(MinWidths);
      if (PercentAbove0Count > 0) and (NewWidth > 0) then
      begin
        // Calculate widths with respect to percentage specifications.
        // Don't shrink Column i below MinWidth[i]! Therefor spread exessive space
        // trying to fit the percentage demands.
        // If there are more than 100% percent reduce linearly to 100% (including
        // the corresponding percentages of the MinWidth of all other columns).
        SetLength(PercentDeltas, NumCols);
        CalcPercentDeltas(PercentDeltas, NewWidth);

        PercentDeltaAbove0Count := 0;
        for I := 0 to NumCols - 1 do
          if PercentDeltas[I] > 0 then
            Inc(PercentDeltaAbove0Count);

        IncreaseWidthsByPercentage(Widths, PercentDeltas, 0, NumCols - 1, NewWidth, MinWidth, Sum(PercentDeltas), PercentDeltaAbove0Count);
      end;
      MinWidth := Sum(Widths);

      if MinWidth > NewWidth then
        // Table is too small for given percentage specifications.
        // Shrink percentage columns to fit exactly into NewWidth. All other columns are at minimum.
        IncreaseWidths(wtPercent, MinWidth, NewWidth, Counts[wtPercent])
      else if not Specified and (MaxWidth <= NewWidth) then
        // Table width not specified and maximum widths fits into available width, table might be smaller than NewWidth
        Widths := Copy(MaxWidths)
      else if MinWidth < NewWidth then
      begin
        // Expand columns to fit exactly into NewWidth.
        // Prefer widening columns without or with relative specification.
        CountsPerType(Counts, ColumnSpecs, 0, NumCols - 1);
        if Counts[wtNone] > 0 then
        begin
          // a) There is at least 1 column without any width constraint: modify this/these.
          IncreaseWidths(wtNone, MinWidth, NewWidth, Counts[wtNone]);
        end
        else if Counts[wtRelative] > 0 then
        begin
          // b) There is at least 1 column with relative width: modify this/these.
          W := NewWidth - MinWidth;
          D := SumOfType(wtRelative, ColumnSpecs, Widths, 0, NumCols - 1);
          IncreaseWidthsRelatively(Widths, 0, NumCols - 1, D + W, Sum(Multis), False);
        end
        else if Counts[wtPercent] > 0 then
        begin
          // c) There is at least 1 column with percentage width: modify this/these.
          IncreaseWidths(wtPercent, MinWidth, NewWidth, Counts[wtPercent]);
        end
        else
        begin
          // d) All columns have absolute widths: modify relative to current width.
          IncreaseWidths(wtAbsolute, MinWidth, NewWidth, Counts[wtAbsolute]);
        end;
      end;
    end;

    {Return Table Width}
    Result := CellSpacing + Sum(Widths);
  end;

  function FindTableHeight: Integer;

    procedure FindRowHeights(Canvas: TCanvas; AHeight: Integer);
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
          with Rows[J] do
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

  var
    I, J, K: Integer;
    CellObj: TCellObj;
    HasBody: Boolean;
  begin
  // Find Row Heights
    if Length(Heights) = 0 then
      FindRowHeights(Canvas, AHeight)
    else if Document.InLogic2 and (Document.TableNestLevel <= 5) then
      FindRowHeights(Canvas, AHeight);

    Result := 0;
    HeaderHeight := 0;
    HeaderRowCount := 0;
    FootHeight := 0;
    FootStartRow := -1;
    HasBody := False;
    for J := 0 to Rows.Count - 1 do
      with Rows[J] do
      begin
        RowHeight := Heights[J];
        case RowType of
          THead:
          begin
            Inc(HeaderRowCount);
            Inc(HeaderHeight, RowHeight);
          end;

          TFoot:
          begin
            if FootStartRow = -1 then
            begin
              FootStartRow := J;
              FootOffset := Result;
            end;
            Inc(FootHeight, RowHeight);
          end;

          TBody:
            HasBody := True;
        end;
        RowSpanHeight := 0;
        Inc(Result, RowHeight);
        for I := 0 to Count - 1 do
          if Assigned(Items[I]) then
          begin
            CellObj := TCellObj(Items[I]);
            with CellObj do
            begin {find the actual height, Ht, of each cell}
              Ht := 0;
              for K := J to Min(J + RowSpan - 1, Rows.Count - 1) do
                Inc(FHt, Heights[K]);
              if RowSpanHeight < Ht then
                RowSpanHeight := Ht;
            end;
          end;
      {DrawLogic2 is only called in nested tables if the outer table is calling DrawLogic2}
        if Document.TableNestLevel = 1 then
          Document.InLogic2 := True;
        try
          if Document.InLogic2 then
            DrawLogic2(Canvas, Y, CellSpacing, Curs);
        finally
          if Document.TableNestLevel = 1 then
            Document.InLogic2 := False;
        end;
        Inc(Y, RowHeight);
      end;
    HeadOrFoot := ((HeaderHeight > 0) or (FootHeight > 0)) and HasBody;
    Inc(Result, CellSpacing);
  end;

var
  TopY: Integer;
  FirstLinePtr: PInteger;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'THtmlTable.DrawLogic');

  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
  Inc(Document.TableNestLevel);
  try
    YDraw := Y;
    TopY := Y;
    ContentTop := Y;
    DrawTop := Y;
    StartCurs := Curs;
    if Assigned(Document.FirstLineHtPtr) and {used for List items}
      (Document.FirstLineHtPtr^ = 0) then
      FirstLinePtr := Document.FirstLineHtPtr {save for later}
    else
      FirstLinePtr := nil;

    TableWidth := FindTableWidth;
    TableHeight := FindTableHeight;
    // Notice: SectionHeight = TableHeight
    Len := Curs - StartCurs;
    MaxWidth := TableWidth;
    Result := TableHeight;
    DrawHeight := Result;
    ContentBot := TopY + TableHeight;
    DrawBot := TopY + DrawHeight;

    try
      if Assigned(FirstLinePtr) then
        FirstLinePtr^ := YDraw + SectionHeight;
    except
    end;
  finally
    Dec(Document.TableNestLevel);
  end;
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'THtmlTable.DrawLogic');
   {$ENDIF}
end;

{----------------THtmlTable.Draw}

function THtmlTable.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;

  procedure DrawTable(XX, YY, YOffset: Integer);
  var
    I: Integer;
  begin
    for I := 0 to Rows.Count - 1 do
      YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
        XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
        BorderColorDark, I);
  end;

  procedure DrawTableP(XX, YY, YOffset: Integer);
  {Printing table with thead and/or tfoot}
  var
    TopBorder, BottomBorder: Integer;
    SavePageBottom: Integer;
    Spacing, HeightNeeded: Integer;

    procedure DrawNormal;
    var
      Y, I: Integer;
    begin
      Y := YY;
      Document.PrintingTable := Self;
      if Document.PageBottom - Y >= TableHeight + BottomBorder then
      begin
        for I := 0 to Rows.Count - 1 do {do whole table now}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        Document.PrintingTable := nil;
      end
      else
      begin {see if enough room on this page for header, 1 row, footer}
        if HeadOrFoot then
        begin
          Spacing := CellSpacing div 2;
          HeightNeeded := HeaderHeight + FootHeight + Rows[HeaderRowCount].RowHeight;
          if (Y - YOffset > ARect.Top) and (Y + HeightNeeded > Document.PageBottom) and
            (HeightNeeded < ARect.Bottom - ARect.Top) then
          begin {not enough room, start table on next page}
            if YY + Spacing < Document.PageBottom then
            begin
              Document.PageShortened := True;
              Document.PageBottom := YY + Spacing;
            end;
            exit;
          end;
        end;
        {start table. it will not be complete and will go to next page}
        SavePageBottom := Document.PageBottom;
        Document.PageBottom := SavePageBottom - FootHeight - Cellspacing - BottomBorder - 5; {a little to spare}
        for I := 0 to Rows.Count - 1 do {do part of table}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        BodyBreak := Document.PageBottom;
        if FootStartRow >= 0 then
        begin
          TablePartRec.TablePart := DoFoot;
          TablePartRec.PartStart := Y + FootOffset;
          TablePartRec.PartHeight := FootHeight + Max(2 * Cellspacing, Cellspacing + 1) + BottomBorder;
          Document.TheOwner.TablePartRec := TablePartRec;
        end
        else if HeaderHeight > 0 then
        begin {will do header next}
          //Document.PageBottom := SavePageBottom;
          TablePartRec.TablePart := DoHead;
          TablePartRec.PartStart := Y - TopBorder;
          TablePartRec.PartHeight := HeaderHeight + TopBorder;
          Document.TheOwner.TablePartRec := TablePartRec;
        end;
        Document.TheOwner.TablePartRec := TablePartRec;
      end;
    end;

    procedure DrawBody1;
    var
      Y, I: Integer;
    begin
      Y := YY;
      if Document.PageBottom > Y + TableHeight + BottomBorder then
      begin {can complete table now}
        for I := 0 to Rows.Count - 1 do {do remainder of table now}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        Document.TheOwner.TablePartRec.TablePart := Normal;
      end
      else
      begin {will do part of the table now}
    {Leave room for foot later}
        Document.PageBottom := Document.PageBottom - FootHeight + Max(Cellspacing, 1) - BottomBorder;
        for I := 0 to Rows.Count - 1 do
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        BodyBreak := Document.PageBottom;
        if FootStartRow >= 0 then
        begin
          TablePartRec.TablePart := DoFoot;
          TablePartRec.PartStart := Y + FootOffset;
          TablePartRec.PartHeight := FootHeight + Max(2 * Cellspacing, Cellspacing + 1) + BottomBorder; //FootHeight+Max(CellSpacing, 1);
          Document.TheOwner.TablePartRec := TablePartRec;
        end
        else if HeaderHeight > 0 then
        begin
          TablePartRec.TablePart := DoHead;
          TablePartRec.PartStart := Y - TopBorder;
          TablePartRec.PartHeight := HeaderHeight + TopBorder;
          Document.TheOwner.TablePartRec := TablePartRec;
        end;
        Document.TheOwner.TablePartRec := TablePartRec;
      end;
    end;

    procedure DrawBody2;
    var
      Y, I: Integer;
    begin
      Y := YY;
      if Document.PageBottom > Y + TableHeight + BottomBorder then
      begin
        for I := 0 to Rows.Count - 1 do {do remainder of table now}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        Document.TheOwner.TablePartRec.TablePart := Normal;
        Document.PrintingTable := nil;
      end
      else
      begin
        SavePageBottom := Document.PageBottom;
        for I := 0 to Rows.Count - 1 do {do part of table}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        BodyBreak := Document.PageBottom;
        if FootStartRow >= 0 then
        begin
          TablePartRec.TablePart := DoFoot;
          TablePartRec.PartStart := Y + FootOffset;
          TablePartRec.PartHeight := FootHeight + Max(2 * Cellspacing, Cellspacing + 1) + BottomBorder; //FootHeight+Max(CellSpacing, 1);
          Document.TheOwner.TablePartRec := TablePartRec;
        end
        else if HeaderHeight > 0 then
        begin
          Document.PageBottom := SavePageBottom;
          TablePartRec.TablePart := DoHead;
          TablePartRec.PartStart := Y - TopBorder;
          TablePartRec.PartHeight := HeaderHeight + TopBorder;
          Document.TheOwner.TablePartRec := TablePartRec;
        end;
        Document.TheOwner.TablePartRec := TablePartRec;
      end;
    end;

    procedure DrawFoot;
    var
      Y, I: Integer;
    begin
      Y := YY;
      YY := TablePartRec.PartStart;
      if FootStartRow >= 0 then
        for I := FootStartRow to Rows.Count - 1 do
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
      if HeaderHeight > 0 then
      begin
        TablePartRec.TablePart := DoHead;
        TablePartRec.PartStart := Y - TopBorder;
        TablePartRec.PartHeight := HeaderHeight + TopBorder;
      end
      else
      begin {No THead}
        TablePartRec.TablePart := DoBody3;
        TablePartRec.PartStart := BodyBreak - 1;
        TablePartRec.FootHeight := FootHeight + Max(Cellspacing, 1);
      end;
      Document.TheOwner.TablePartRec := TablePartRec;
    end;

    procedure DrawHead;
    var
      I: Integer;
    begin
      for I := 0 to HeaderRowCount - 1 do
        YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
          XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
          BorderColorDark, I);
      TablePartRec.TablePart := DoBody1;
      TablePartRec.PartStart := BodyBreak - 1;
      TablePartRec.FootHeight := FootHeight + Max(Cellspacing, 1) + BottomBorder;
      Document.TheOwner.TablePartRec := TablePartRec;
    end;

  begin
    if TTableBlock(Owner).TableBorder then
    begin
      TopBorder := BorderWidth;
      BottomBorder := BorderWidth;
    end
    else
    begin
      TopBorder := Owner.MargArray[BorderTopWidth];
      BottomBorder := Owner.MargArray[BorderBottomWidth];
    end;

    case TablePartRec.TablePart of
      Normal:   DrawNormal;
      DoBody1:  DrawBody1;
      DoBody2:  DrawBody2;
      DoFoot:   DrawFoot;
      DoHead:   DrawHead;
    end;
  end;

var
  Y, YO, YOffset: Integer;
begin
  Inc(Document.TableNestLevel);
  try
    Y := YDraw;
    Result := Y + SectionHeight;
    if Float then
      Y := Y + VSpace;
    YOffset := Document.YOff;
    YO := Y - YOffset;

    DrawX := X;
    //DrawY := Y;

    if (YO + DrawHeight >= ARect.Top) and (YO < ARect.Bottom) or Document.Printing then
      if Document.Printing and (Document.TableNestLevel = 1)
        and HeadOrFoot and (Y < Document.PageBottom)
        and ((Document.PrintingTable = nil) or (Document.PrintingTable = Self))
      then
        DrawTableP(X, Y, YOffset)
      else
        DrawTable(X, Y, YOffset);
  finally
    Dec(Document.TableNestLevel);
  end;
end;

{----------------THtmlTable.GetURL}

function THtmlTable.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): guResultType;

  function GetTableURL(X: Integer; Y: Integer): guResultType;
  var
    I, J, XX: Integer;
    Row: TCellList;
    CellObj: TCellObj;
  begin
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      XX := DrawX;
      for I := 0 to Row.Count - 1 do
      begin
        CellObj := Row[I];
        if CellObj <> nil then
        begin
          if (X >= XX) and (X < XX + CellObj.Wd) and (Y >= CellObj.Cell.DrawYY) and (Y < CellObj.Cell.DrawYY + CellObj.Ht) then
          begin
            Result := CellObj.Cell.GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
            Exit;
          end;
        end;
        Inc(XX, Widths[I]);
      end;
    end;
    Result := [];
  end;

begin
  UrlTarg := nil;
  FormControl := nil;
  if (Y >= ContentTop) and (Y < ContentBot) and (X >= DrawX) and (X <= TableWidth + DrawX) then
    Result := GetTableURL(X, Y)
  else
    Result := [];
end;

{----------------THtmlTable.PtInObject}

function THtmlTable.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;

  function GetTableObj(X: Integer; Y: Integer): boolean;
  var
    I, J, XX: Integer;
    Row: TCellList;
    CellObj: TCellObj;
  begin
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      XX := DrawX;
      for I := 0 to Row.Count - 1 do
      begin
        CellObj := Row[I];
        if CellObj <> nil then
        begin
          if (X >= XX) and (X < XX + CellObj.Wd) and (Y >= CellObj.Cell.DrawYY) and (Y < CellObj.Cell.DrawYY + CellObj.Ht) then
          begin
            Result := CellObj.Cell.PtInObject(X, Y, Obj, IX, IY);
            Exit;
          end;
        end;
        Inc(XX, Widths[I]);
      end;
    end;
    Result := False;
  end;

begin
  if (Y >= ContentTop) and (Y < ContentBot) and (X >= DrawX) and (X <= TableWidth + DrawX) then
    Result := GetTableObj(X, Y)
  else
    Result := False;
end;

{----------------THtmlTable.FindCursor}

function THtmlTable.FindCursor(Canvas: TCanvas; X, Y: Integer;
  out XR, YR, CaretHt: Integer; out Intext: boolean): Integer;

  function GetTableCursor(X, Y: Integer;
    out XR, YR, CaretHt: Integer; out Intext: boolean): Integer;
  var
    I, J, XX: Integer;
    Row: TCellList;
    CellObj: TCellObj;
  begin
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      XX := DrawX;
      for I := 0 to Row.Count - 1 do
      begin
        CellObj := Row[I];
        if CellObj <> nil then
        begin
          if (X >= XX) and (X < XX + CellObj.Wd) and (Y >= CellObj.Cell.DrawYY) and (Y < CellObj.Cell.DrawYY + CellObj.Ht) then
          begin
            Result := CellObj.Cell.FindCursor(Canvas, X, Y, XR, YR, CaretHt, InText);
            if Result >= 0 then
              Exit;
          end;
          Inc(XX, Widths[I]);
        end;
      end;
    end;
    Result := -1;
  end;

begin
  if (Y >= ContentTop) and (Y < ContentBot) and (X >= DrawX) and (X <= TableWidth + DrawX) then
    Result := GetTableCursor(X, Y, XR, YR, CaretHt, InText)
  else
    Result := -1;
end;

{----------------THtmlTable.CursorToXY}

function THtmlTable.CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean;
{note: returned X value is not correct here but it isn't used}
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  if (len > 0) and (Cursor >= StartCurs) and (Cursor < StartCurs + Len) then
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      for I := 0 to Row.Count - 1 do
      begin
        CellObj := Row[I];
        if CellObj <> nil then
        begin
          Result := CellObj.Cell.CursorToXy(Canvas, Cursor, X, Y);
          if Result then
            Exit;
        end;
      end;
    end;
    
  Result := False;
end;

{----------------THtmlTable.GetChAtPos}

function THtmlTable.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean;
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  Obj := nil;
  if (len > 0) and (Pos >= StartCurs) and (Pos < StartCurs + Len) then
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      for I := 0 to Row.Count - 1 do
      begin
        CellObj := Row[I];
        if CellObj <> nil then
        begin
          Result := CellObj.Cell.GetChAtPos(Pos, Ch, Obj);
          if Result then
            Exit;
        end;
      end;
    end;

  Result := False;
end;

{----------------THtmlTable.FindString}

function THtmlTable.FindString(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  for J := 0 to Rows.Count - 1 do
  begin
    Row := Rows[J];
    for I := 0 to Row.Count - 1 do
    begin
      CellObj := Row[I];
      if CellObj <> nil then
      begin
        Result := CellObj.Cell.FindString(From, ToFind, MatchCase);
        if Result >= 0 then
          Exit;
      end;
    end;
  end;
  Result := -1;
end;

{----------------THtmlTable.FindStringR}

function THtmlTable.FindStringR(From: Integer; const ToFind: WideString; MatchCase: boolean): Integer;
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  for J := Rows.Count - 1 downto 0 do
  begin
    Row := Rows[J];
    for I := Row.Count - 1 downto 0 do
    begin
      CellObj := Row[I];
      if CellObj <> nil then
      begin
        Result := CellObj.Cell.FindStringR(From, ToFind, MatchCase);
        if Result >= 0 then
          Exit;
      end;
    end;
  end;
  Result := -1;
end;

{----------------THtmlTable.FindSourcePos}

function THtmlTable.FindSourcePos(DocPos: Integer): Integer;
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  for J := 0 to Rows.Count - 1 do
  begin
    Row := Rows[J];
    for I := 0 to Row.Count - 1 do
    begin
      CellObj := Row[I];
      if CellObj <> nil then
      begin
        Result := CellObj.Cell.FindSourcePos(DocPos);
        if Result >= 0 then
          Exit;
      end;
    end;
  end;
  Result := -1;
end;

{----------------THtmlTable.FindDocPos}

function THtmlTable.FindDocPos(SourcePos: Integer; Prev: boolean): Integer;
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  if not Prev then
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      if Row <> nil then
        for I := 0 to Row.Count - 1 do
        begin
          CellObj := Row[I];
          if CellObj <> nil then
          begin
            Result := CellObj.Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then
              Exit;
          end;
        end;
    end
  else {Prev , iterate in reverse}
    for J := Rows.Count - 1 downto 0 do
    begin
      Row := Rows[J];
      if Row <> nil then
        for I := Row.Count - 1 downto 0 do
        begin
          CellObj := Row[I];
          if CellObj <> nil then
          begin
            Result := CellObj.Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then
              Exit;
          end;
        end;
    end;
  Result := -1;
end;

{----------------THtmlTable.CopyToClipboard}

procedure THtmlTable.CopyToClipboard;
var
  I, J: Integer;
  Row: TCellList;
  CellObj: TCellObj;
begin
  for J := 0 to Rows.Count - 1 do
  begin
    Row := Rows[J];
    for I := 0 to Row.Count - 1 do
    begin
      CellObj := Row[I];
      if CellObj <> nil then
        CellObj.Cell.CopyToClipboard;
    end;
  end;
end;

{----------------TSection.Create}

constructor TSection.Create(AMasterList: ThtDocument; L: TAttributeList; Prop: TProperties; AnURL: TUrlTarget; OwnerCell: TCellBasic; FirstItem: boolean);
var
  FO: TFontObj;
  T: TAttribute;
  S: ThtString;
  Clr: ClearAttrType;
  Percent: boolean;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSection.Create');
  StyleUn.LogProperties(Prop,'Prop');
  CodeSite.AddSeparator;
  {$ENDIF}
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
    FO.UrlTarget.Assign(AnUrl);
    Document.LinkList.Add(FO);
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

  Images := TFloatingObjList.Create;
  FormControls := TFormControlObjList.Create(False);

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
      Document.IDNameList.AddObject(L.TheID, Self);
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TSection.Create');
  {$ENDIF}
end;

{----------------TSection.CreateCopy}

constructor TSection.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
var
  TT: TSection;
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
  Images := TFloatingObjList.CreateCopy(AMasterList, TT.Images);
  FormControls := TFormControlObjList.Create(False);
  FormControls.Assign(TT.FormControls);
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
  if Document <> nil then
    if Document.LinkList <> nil then
      for i := 0 to Fonts.Count - 1 do
        Document.LinkList.Remove(Fonts[i]);

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
  if Assigned(Document) then
  begin
  {Check to see that there isn't a TFontObj in LinkList}
    if Assigned(Document.LinkList) then
      for I := 0 to Fonts.Count - 1 do
      begin
        J := Document.LinkList.IndexOf(Fonts[I]);
        if J >= 0 then
          Document.LinkList.Delete(J);
      end;
  {Remove Self from IDNameList if there}
    if Assigned(Document.IDNameList) then
      with Document.IDNameList do
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
  C: AnsiChar;
  St, StU: WideString;
  Small: boolean;
begin
  if T.Count = 0 then
    Exit;
  { Yunqa.de: Simple hack to support <span style="display:none"> }
  if Document.PropStack.Last.Display = pdNone then
    Exit;

  L := Len + T.Count;
  if BuffSize < L + 3 then
    Allocate(L + 500); {L+3 to permit additions later}
  case Document.PropStack.Last.GetTextTransform of
    txUpper:
      St := WideUpperCase1(T.S);
    txLower:
      St := WideLowerCase1(T.S);
  else
    St := T.S;
  end;
  Move(T.I[1], XP^[Len], T.Count * Sizeof(Integer));
  if Document.NoBreak or (Self is TPreformated) then
    C := 'n'
  else
    C := 'y';
  for I := 1 to T.Count do
    Brk := Brk + C;

  if Document.PropStack.Last.GetFontVariant = 'small-caps' then
  begin
    StU := WideUpperCase1(St);
    BuffS := BuffS + StU;
    Small := False;
    for I := 1 to Length(St) do
    begin
      case St[I] of
        WideChar(' '), WideChar('0')..WideChar('9'):
          {no font changes for these chars}
          ;
      else
        if not Small then
        begin
          if StU[I] <> St[I] then
          begin {St[I] was lower case}
            Document.PropStack.PushNewProp('small', '', '', '', '', nil); {change to smaller font}
            ChangeFont(Document.PropStack.Last);
            Small := True;
          end;
        end
        else if StU[I] = St[I] then
        begin {St[I] was uppercase and Small is set}
          Document.PropStack.PopAProp('small');
          ChangeFont(Document.PropStack.Last);
          Small := False;
        end;
      end;
      Inc(Len);
    end;
    if Small then {change back to regular font}
    begin
      Document.PropStack.PopAProp('small');
      ChangeFont(Document.PropStack.Last);
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
    FormControls.Decrement(I - 1);
    Fonts.Decrement(I - 1, Document);
    Images.Decrement(I - 1);
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
    if (BuffS[I] = ImgPan) and (Images.FindImage(I - 1).Floating in [ALeft, ARight])
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
      BuffSize := 0;
    end;
  end;
  if Len > 0 then
  begin
    Inc(Document.SectionCount);
    SectionNumber := Document.SectionCount;
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
    FO.URLTarget.Assign(LastUrl);
    Fonts.Add(FO);
  end;
  FO.Title := Prop.PropTitle;
  if LastUrl.Url <> '' then
  begin
    FO.CreateFIArray;
    Prop.GetFontInfo(FO.FIArray);
    FO.ConvertFont(FO.FIArray.Ar[LFont]);
    if Document.LinkList.IndexOf(FO) = -1 then
      Document.LinkList.Add(FO);
  end;
  if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then
    FO.SScript := Align
  else
    FO.SScript := ANone;
end;

{----------------------TSection.HRef}

procedure TSection.HRef(Sy: Symb; List: ThtDocument; AnURL: TUrlTarget;
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
    if Document.LinkList.IndexOf(FO) = -1 then
      Document.LinkList.Add(FO);
{$IFNDEF NoTabLink}
    if not Document.StopTab then
      FO.CreateTabControl(AnUrl.TabIndex);
{$ENDIF}
  end
  else if Assigned(FO.FIArray) then
  begin
    FO.FIArray.Free;
    FO.FIArray := nil;
  end;
  FO.UrlTarget.Assign(AnUrl);
  if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then
    FO.SScript := Align
  else
    FO.SScript := ANone;
end;

function TSection.AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer): TImageObj;
begin
  Result := TImageObj.Create(Document, Len, L);
  Result.MyCell := ACell;
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for image}
end;

function TSection.AddPanel(L: TAttributeList;
  ACell: TCellBasic; Index: Integer): TPanelObj;
begin
  Result := TPanelObj.Create(Document, Len, L, ACell, False);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for panel}
end;

function TSection.CreatePanel(L: TAttributeList;
  ACell: TCellBasic): TPanelObj;
{Used by object tag}
begin
  Result := TPanelObj.Create(Document, Len, L, ACell, True);
end;

procedure TSection.AddPanel1(PO: TPanelObj; Index: Integer);
{Used for Object Tag}
begin
  Images.Add(PO);
  AddChar(ImgPan, Index); {marker for panel}
end;

{----------------TSection.AddFormControl}

function TSection.AddFormControl(Which: Symb; AMasterList: ThtDocument;
  L: TAttributeList; ACell: TCellBasic; Index: Integer;
  Prop: TProperties): TFormControlObj;
var
  T: TAttribute;
  FCO: TFormControlObj;
  S: ThtString;
  IO: TImageObj;
  ButtonControl: TButtonFormControlObj;

  function CreateEditFCO: TEditFormControlObj;
  begin
    Result := TEditFormControlObj.Create(AMasterList, Len, L, S, Prop);
  end;

begin
  S := '';
  case Which of
    InputSy:
    begin
      if L.Find(TypeSy, T) then
      begin
        S := LowerCase(T.Name);
        if (S = 'text') or (S = 'password') or (S = 'file') then
          FCO := CreateEditFCO
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
          FCO := CreateEditFCO;
      end
      else
        FCO := CreateEditFCO;

    end;

    SelectSy:
    begin
      if L.Find(MultipleSy, T) or L.Find(SizeSy, T) and (T.Value > 1) then
        FCO := TListBoxFormControlObj.Create(AMasterList, Len, L, Prop)
      else
        FCO := TComboFormControlObj.Create(AMasterList, Len, L, Prop);
    end;
  else
    FCO := TTextAreaFormControlObj.Create(AMasterList, Len, L, Prop);
  end;

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
      Document.IDNameList.AddObject(L.TheID, FCO);
    FCO.Value := ''; {value attribute should not show in TEdit}
    TEditFormControlObj(FCO).Text := '';
    AddChar(FmCtl, Index);
    Brk[Len] := 'n';
  end
  else if S <> 'hidden' then
  begin
    FormControls.Add(FCO);
    AddChar(FmCtl, Index); {marker for FormControl}
  end;
  if Prop.HasBorderStyle then {start of inline border}
    Document.ProcessInlines(Index, Prop, True);
  Result := FCO;
end;

{----------------TSection.FindCountThatFits}

function TSection.FindCountThatFits(Canvas: TCanvas; Width: Integer; Start: PWideChar; Max: Integer): Integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
//BG, 06.02.2011: Why are there 2 methods and why can't GetURL and FindCursor use the formatting results of DrawLogic?
//  TSection.FindCountThatFits1() is used in TSection.DrawLogic().
//  TSection.FindCountThatFits() is used in TSection.GetURL() and TSection.FindCursor().
var
  Cnt, XX, YY, I, J, J1, J2, J3, OHang, Tmp: Integer;
  Picture: boolean;
  Align: AlignmentType;
  HSpcL, HSpcR: Integer;
  FLObj: TFloatingObj;
  Extent: TSize;
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
  YY := 0;
  while True do
  begin
    Fonts.GetFontAt(Start - Buff, OHang).AssignToCanvas(Canvas);
    J1 := Fonts.GetFontCountAt(Start - Buff, Len);
    J2 := Images.GetImageCountAt(Start - Buff);
    J3 := FormControls.GetControlCountAt(Start - Buff);
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
      XX := XX + FormControls.GetWidthAt(Start - Buff, HSpcL, HSpcR);
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
      XX := XX + Extent.cx;
      YY := Math.Max(YY, Extent.cy);
    end;

    Inc(Start, I);
  end;
  Result := Cnt;
  OldResult := Result;
end;

function WrapChar(C: WideChar): Boolean;
begin
  Result := Ord(C) >= $3000;
end;

//-- BG ---------------------------------------------------------- 27.01.2012 --
function CanWrapAfter(C: WideChar): Boolean;
begin
  case C of
    WideChar('-'), WideChar('/'), WideChar('?'):
      Result := True
  else
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 20.09.2010 --
function CanWrap(C: WideChar): Boolean;
begin
  case C of
    WideChar(' '), WideChar('-'), WideChar('/'), WideChar('?'), ImgPan, FmCtl, BrkCh:
      Result := True
  else
    Result := WrapChar(C);
  end;
end;

{----------------TSection.MinMaxWidth}

procedure TSection.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSection.MinMaxWidth');
  CodeSite.AddSeparator;
   {$ENDIF}
  if (StoredMin > 0) and (Images.Count = 0) then
  begin
    Min := StoredMin;
    Max := StoredMax;
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TSection.MinMaxWidth');
   {$ENDIF}
    Exit;
  end;
  Min := 0;
  Max := 0;
  if Len = 0 then begin
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);
  CodeSiteLogging.CodeSite.ExitMethod(Self,'TSection.MinMaxWidth');
   {$ENDIF}
    Exit;
  end;
  for I := 0 to Images.Count - 1 do {call drawlogic for all the images}
  begin
    Obj := Images[I];
    with TFloatingObj(Obj) do
    begin
      DrawLogic(Self.Document, Canvas, Fonts.GetFontObjAt(Pos, Indx), 0, 0);
      if not PercentWidth then
        if Floating in [ALeft, ARight] then
        begin
          Max := Max + ImageWidth + HSpaceL + HSpaceR;
          Brk[Pos + 1] := 'y'; {allow break after floating image}
          Min := Math.Max(Min, ImageWidth + HSpaceL + HSpaceR);
        end
        else
          Min := Math.Max(Min, ImageWidth);
    end;
  end;
  FloatMin := Min;

  for I := 0 to FormControls.Count - 1 do {get Min for form controls}
  begin
    Obj := FormControls[I];
    if Obj is TFormControlObj then
      with TFormControlObj(FormControls[I]) do
        if not PercentWidth then
          Min := Math.Max(Min, Width + HSpaceL + HSpaceR);
  end;

  //Max := 0;
  P := Buff;
  P1 := StrScanW(P, BrkCh); {look for break ThtChar}
  while Assigned(P1) do
  begin
    Max := Math.Max(Max, FindTextWidthB(Canvas, P, P1 - P, False));
    P := P1 + 1;
    P1 := StrScanW(P, BrkCh);
  end;
  P1 := StrScanW(P, #0); {look for the end}
  Max := Math.Max(Max, FindTextWidthB(Canvas, P, P1 - P, True)); // + FloatMin;

  P := Buff;
  if not BreakWord then
  begin
    while P^ = ' ' do
      Inc(P);
    P1 := P;
    I := P1 - Buff + 1;
    while P^ <> #0 do
    {find the next string of chars that can't be wrapped}
    begin
      if CanWrap(P1^) and (Brk[I] = 'y') then
      begin
        Inc(P1);
        Inc(I);
      end
      else
      begin
        repeat
          Inc(P1);
          Inc(I);
          case Brk[I - 1] of
            's', 'a':
              break;
          end;
        until (P1^ = #0) or (CanWrap(P1^) and (Brk[I] = 'y'));
        SoftHyphen := Brk[I - 1] = 's';
        if CanWrapAfter(P1^) then
        begin
          Inc(P1);
          Inc(I);
        end;
      end;
      Min := Math.Max(Min, FindTextWidthB(Canvas, P, P1 - P, True));
      while True do
        case P1^ of
          WideChar(' '), ImgPan, FmCtl, BrkCh:
          begin
            Inc(P1);
            Inc(I);
          end;
        else
          break;
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
   {$IFDEF JPM_DEBUGGING}
   CodeSite.SendFmtMsg('Min = [%d]',[Min]);
   CodeSite.SendFmtMsg('Max = [%d]',[Max]);
  CodeSite.ExitMethod(Self,'TSection.MinMaxWidth');
   {$ENDIF}
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSection.FindTextWidth');
  CodeSite.AddSeparator;
   {$ENDIF}
  Result := 0;
  if RemoveSpaces then
    while True do
      case (Start + N - 1)^ of
        SpcChar,
        BrkCh:
          Dec(N); {remove spaces on end}
      else
        break;
      end;
  while N > 0 do
  begin
    J := Images.GetImageCountAt(Start - Buff);
    J1 := FormControls.GetControlCountAt(Start - Buff);
    if J = 0 then {it's an image}
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
      Inc(Result, FormControls.GetWidthAt(Start - Buff, HSpcL, HSpcR) + HSpcL + HSpcR);
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TSection.FindTextWidth');
  {$ENDIF}
end;

{----------------TSection.FindTextWidthA}

function TSection.FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
{find actual line width of N chars starting at Start.
 BG: The only difference to FindTextWidth is the '- OHang' when incrementing the result.}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: Integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
begin
  Result := 0;
  while N > 0 do
  begin
    J := Images.GetImageCountAt(Start - Buff);
    J1 := FormControls.GetControlCountAt(Start - Buff);
    if J = 0 then {it's an image}
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
      Inc(Result, FormControls.GetWidthAt(Start - Buff, HSpcL, HSpcR) + HSpcL + HSpcR);
      Dec(N); {control counts as one ThtChar}
      Inc(Start);
    end
    else
    begin
      Fonts.GetFontAt(Start - Buff, OHang).AssignToCanvas(Canvas);
      I := Min(J, J1);
      I := Min(I, Min(Fonts.GetFontCountAt(Start - Buff, Len), N));
      Assert(I > 0, 'I less than or = 0 in FindTextWidthA');
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

  function FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; MaxChars, X, Y: Integer;
    IMgr: TIndentManager; var ImgY, ImgHt: Integer; var DoneFlObjPos: PWideChar): Integer;
  {Given a width, find the count of chars (<= Max) which will fit allowing for font changes.
    Line wrapping will be done later}
  //BG, 06.02.2011: Why are there 2 methods and why can't GetURL and FindCursor use the formatting results of DrawLogic?
  //  FindCountThatFits1() is part of TSection.DrawLogic() and fills IMgr with the embedded floating images.
  //  TSection.FindCountThatFits() is used in TSection.GetURL() and TSection.FindCursor().
  var
    Cnt, XX, YY, I, J, J1, J2, J3, X1, X2, W, H, OHang, Width: Integer;
    Picture: boolean;
    FlObj: TFloatingObj;
    FcObj: TFormControlObj;
    BrChr, TheStart: PWideChar;
    Font, LastFont: TMyFont;
    Save: TSize;
    FoundBreak: boolean;
    HyphenWidth: Integer;
    FloatingImageCount: Integer;
  begin
    LastFont := nil;
    TheStart := Start;
    ImgHt := 0;

    BrChr := StrScanW(TheStart, BrkCh); {see if a break char}
    if Assigned(BrChr) and (BrChr - TheStart < MaxChars) then
    begin
      MaxChars := BrChr - TheStart;
      if MaxChars = 0 then
      begin
        Result := 1;
        Exit; {single character fits}
      end;
      FoundBreak := True;
    end
    else
      FoundBreak := False;

    X1 := IMgr.LeftIndent(Y);
    if Start = Buff then
      Inc(X1, FirstLineIndent);
    X2 := IMgr.RightSide(Y);
    Width := X2 - X1;

    if (Start = Buff) and (Images.Count = 0) and (FormControls.Count = 0) then
      if Fonts.GetFontCountAt(0, Len) = Len then
        if MaxChars * TFontObj(Fonts[0]).tmMaxCharWidth <= Width then {try a shortcut}
        begin {it will all fit}
          Result := MaxChars;
          if FoundBreak then
            Inc(Result);
          Exit;
        end;

    FloatingImageCount := -1;
    Cnt := 0;
    XX := 0;
    YY := 0;
    while True do
    begin
      Font := Fonts.GetFontAt(Start - Buff, OHang);
      if Font <> LastFont then {may not have to load font}
      begin
        Font.AssignToCanvas(Canvas);
        LastFont := Font;
      end;
      J1 := Min(Fonts.GetFontCountAt(Start - Buff, Len), MaxChars - Cnt);
      J2 := Images.GetImageCountAt(Start - Buff);
      J3 := FormControls.GetControlCountAt(Start - Buff);
      if J2 = 0 then
      begin {next is an image}
        I := 1;
        J := 1;
        Picture := True;
        FlObj := Images.FindImage(Start - Buff);
        if FlObj.Floating in [ALeft, ARight] then
        begin
          if Start > DoneFlObjPos then
          begin
            ImgY := Max(Y, ImgY);
            W := FlObj.TotalWidth;
            H := FlObj.TotalHeight;
            case FlObj.Floating of
              ALeft:
              begin
                IMgr.AlignLeft(ImgY, W, XX, YY);
                FlObj.Indent := IMgr.AddLeft(ImgY, ImgY + H, W).X - W + FlObj.HSpaceL;
              end;

              ARight:
              begin
                IMgr.AlignRight(ImgY, W, XX, YY);
                FlObj.Indent := IMgr.AddRight(ImgY, ImgY + H, W).X + FlObj.HSpaceL;
              end;
            end;
            FlObj.DrawYY := ImgY + FlObj.VSpaceT;
            ImgHt := Max(ImgHt, H);
            DoneFlObjPos := Start;

            // go on with the line:
            X1 := IMgr.LeftIndent(Y);
            X2 := IMgr.RightSide(Y);
            Width := X2 - X1;
            Inc(FloatingImageCount);
            if Cnt >= FloatingImageCount then
            begin
              Start := TheStart;
              Cnt := 0;
              XX := 0;
              continue;
            end;
          end;
        end
        else
        begin
          ImgHt := Max(ImgHt, FlObj.TotalHeight);
          Inc(XX, FlObj.TotalWidth);
          if XX > Width then
            break;
        end;
      end
      else if J3 = 0 then
      begin
        FcObj := FormControls.FindControl(Start - Buff);
        Inc(XX, FcObj.TotalWidth);
        I := 1;
        J := 1;
        Picture := True;
        if XX > Width then
          break;
      end
      else
      begin
        Picture := False;
        J := Min(J1, J2);
        J := Min(J, J3);
        I := FitText(Canvas.Handle, Start, J, Width - XX, Save);
        if (I > 0) and (Brk[TheStart - Buff + Cnt + I] = 's') then
        begin {a hyphen could go here}
          HyphenWidth := Canvas.TextWidth('-');
          if XX + Save.cx + HyphenWidth > Width then
            Dec(I);
        end;
      end;

      if Cnt + I >= MaxChars then
      begin
        Cnt := MaxChars;
        Break;
      end
      else
        Inc(Cnt, I);

      if not Picture then {it's a text block}
      begin
        if I < J then
          Break;
        XX := XX + Save.cx;
        YY := Math.Max(YY, Save.cy);
      end;

      Inc(Start, I);
    end;
    Result := Cnt;
    if FoundBreak and (Cnt = MaxChars) then
      Inc(Result);
  end;

var
  PStart, Last: PWideChar;
  ImgHt: Integer;
  Finished: boolean;
  LR: LineRec;
  AccumImgBot: Integer;

  function GetClearSpace(ClearAttr: ClearAttrType): Integer;
  var
    CL, CR: Integer;
  begin
    Result := 0;
    if (ClearAttr <> clrNone) then
    begin {may need to move down past floating image}
      IMgr.GetClearY(CL, CR);
      case ClearAttr of
        clLeft: Result := Max(0, CL - Y - 1);
        clRight: Result := Max(0, CR - Y - 1);
        clAll: Result := Max(CL - Y - 1, Max(0, CR - Y - 1));
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
        case P^ of
          FmCtl, ImgPan, BrkCh:;
        else
          if not ((P = Last) and (Last^ = ' ')) then
          begin {check for the no character case}
            NoChar := False;
            Break;
          end;
        end;
        Inc(P);
      end;
    end;

    if not NoChar then
      repeat
        FO := Fonts.GetFontObjAt(PStart - Buff + Cnt, Index);
        Tmp := FO.GetHeight(Desc);
        DHt := Max(DHt, Tmp);
        LR.Descent := Max(LR.Descent, Desc);
        J := Fonts.GetFontCountAt(PStart - Buff + Cnt, Len);
        Inc(Cnt, J);
      until Cnt >= NN;

    {if there are images or line-height, then maybe they add extra space}
    SB := 0; // vertical space before DHt / Text
    SA := 0; // vertical space after DHt / Text
    if not NoChar then
    begin
      if LineHeight > DHt then
      begin
        // BG, 28.08.2011: too much space below an image: SA and SB depend on Align:
        case Align of
          aTop:
            SA := LineHeight - DHt;

          aMiddle:
            begin
              SB := (LineHeight - DHt) div 2;
              SA := (LineHeight - DHt) - SB;
            end;
        else
//          aNone:
//          aBaseline,
//          aBottom:
            SB := LineHeight - DHt;
        end;
      end
      else if LineHeight >= 0 then
      begin
        SB := (LineHeight - DHt) div 2;
        SA := (LineHeight - DHt) - SB;
      end;
    end;

    Cnt := 0;
    repeat
      Cnt := Cnt + Images.GetImageCountAt(PStart - Buff + Cnt);
      if Cnt < NN then
      begin
        H := Images.GetHeightAt(PStart - Buff + Cnt, Align, FlObj);
        if FlObj.Floating = ANone then
        begin
          FlObj.DrawYY := Y; {approx y dimension}
          if (FLObj is TImageObj) and Assigned(TImageObj(FLObj).MyFormControl) then
            TImageObj(FLObj).MyFormControl.FYValue := Y;
          case Align of
            aTop: 
              SA := Max(SA, H - DHt);

            aMiddle:
              begin
                if DHt = 0 then
                begin
                  DHt := Fonts.GetFontObjAt(PStart - Buff, Index).GetHeight(Desc);
                  LR.Descent := Desc;
                end;
                Tmp := (H - DHt) div 2;
                SA := Max(SA, Tmp);
                SB := Max(SB, (H - DHt - Tmp));
              end;

              aBaseline,
              aBottom:
                SB := Max(SB, H - (DHt - LR.Descent));
          end;
        end;
      end;
      Inc(Cnt); {to skip by the image}
    until Cnt >= NN;

    Cnt := 0; {now check on form controls}
    repeat
      Inc(Cnt, FormControls.GetControlCountAt(PStart - Buff + Cnt));
      if Cnt < NN then
      begin
        FCO := FormControls.FindControl(PStart - Buff + Cnt);
        H := FormControls.GetHeightAt(PStart - Buff + Cnt, FormAlign);
        case FormAlign of
          ATop:
            SA := Max(SA, H + FCO.VSpaceB + FCO.VSpaceT - Dht);
          AMiddle:
            begin
              Tmp := (H - DHt) div 2;
              SA := Max(SA, Tmp + FCO.VSpaceB);
              SB := Max(SB, (H - DHt - Tmp + FCO.VSpaceT));
            end;
          ABaseline:
            SB := Max(SB, H + FCO.VSpaceT + FCO.VSpaceB - (DHt - LR.Descent));
          ABottom:
            SB := Max(SB, H + FCO.VSpaceT + FCO.VSpaceB - DHt);
        end;
        if Assigned(FCO) and not Document.IsCopy then
          FCO.FYValue := Y;
      end;
      Inc(Cnt); {to skip by the control}
    until Cnt >= NN;

{$IFNDEF NoTabLink}
    if not Document.IsCopy then
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
      TextWidth := Max(TextWidth, LRTextWidth);
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
          LR.Spaces := FindSpaces(PStart,NN);
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
      LR.LineImgHt := Max(Tmp, ImgHt);
    end;

var
  P: PWideChar;
  MaxChars: Integer;
  N, NN, Width, I, Indx: Integer;
  Tmp: Integer;
  Obj: TFloatingObj;
  TopY, HtRef: Integer;
  Ctrl: TFormControlObj;
  //BG, 06.02.2011: floating objects:
  PDoneFlObj: PWideChar;
  YDoneFlObj: Integer;
begin {TSection.DrawLogic}
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSection.DrawLogic');
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
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
    Obj.DrawLogic(Self.Document, Canvas, Fonts.GetFontObjAt(Obj.Pos, Indx), Width, HtRef);
    MaxWidth := Max(MaxWidth, Obj.ImageWidth); {HScrollBar for wide images}
  end;
  for I := 0 to FormControls.Count - 1 do
  begin
    Ctrl := FormControls[I];
    if Ctrl.PercentWidth then
      Ctrl.Width := Max(10, Min(MulDiv(Ctrl.FWidth, Width, 100), Width - Ctrl.HSpaceL - Ctrl.HSpaceR));
  	MaxWidth := Max(MaxWidth, Ctrl.Width);
  end;

  YDoneFlObj := Y;
  PDoneFlObj := PStart - 1;
  while not Finished do
  begin
    MaxChars := Last - PStart + 1;
    if MaxChars <= 0 then
      Break;
    LR := LineRec.Create(Document); {a new line}
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
        LR := LineRec.Create(Document);
      end;
    end;

    ImgHt := 0;
    NN := 0;
    if (Self is TPreformated) and not BreakWord then
      N := MaxChars
    else
    begin
      NN := FindCountThatFits1(Canvas, PStart, MaxChars, X, Y, IMgr, YDoneFlObj, ImgHt, PDoneFlObj);
      N := Max(NN, 1); {N = at least 1}
    end;

    AccumImgBot := Max(AccumImgBot, Y + ImgHt);
    if NN = 0 then {if nothing fits, see if we can move down}
      Tmp := IMgr.GetNextWiderY(Y) - Y
    else
      Tmp := 0;
    if Tmp > 0 then
    begin
      //BG, 24.01.2010: do not move down images or trailing spaces.
      P := PStart + N - 1; {the last ThtChar that fits}
      if ((P^ = ThtChar(' ')) or {(P^ = FmCtl) or} (P^ = ImgPan) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n') or (P^ = BrkCh) then
      begin {move past spaces so as not to print any on next line}
        while (N < MaxChars) and ((P + 1)^ = ' ') do
        begin
          Inc(P);
          Inc(N);
        end;
        Finished := N >= MaxChars;
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
    else if N = MaxChars then
    begin {Do the remainder}
      Finished := True;
      LineComplete(N);
    end
    else
    begin
      P := PStart + N - 1; {the last ThtChar that fits}
      if ((P^ = ThtChar(' ')) or (P^ = FmCtl) or (P^ = ImgPan) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n') or (P^ = BrkCh) then
      begin {move past spaces so as not to print any on next line}
        while (N < MaxChars) and ((P + 1)^ = ' ') do
        begin
          Inc(P);
          Inc(N);
        end;
        Finished := N >= MaxChars;
        LineComplete(N);
      end
      else if (N < MaxChars) and ((P + 1)^ = ' ') and (Brk[P - Buff + 2] <> 'n') then
      begin
        repeat
          Inc(N); {pass the space}
          Inc(p);
        until (N >= MaxChars) or ((P + 1)^ <> ' ');
        Finished := N >= MaxChars;
        LineComplete(N);
      end
      else if (N < MaxChars) and (((P + 1)^ = FmCtl) or ((P + 1)^ = ImgPan)) and (Brk[PStart - Buff + N] <> 'n') then {an image or control}
      begin
        Finished := False;
        LineComplete(N);
      end
      else
      begin {non space, wrap it by backing off to previous space or image}
        while P > PStart do
        begin
          case Brk[P - Buff + 1] of
            'n': ; // must not wrap after this char.

            'a', 's': break; // can wrap after this char. BG, 16.12.2011: Why is 'y' missing here?
          else
            if CanWrap(P^) or WrapChar((P + 1)^) then
              break; // can wrap after this or before next char.
          end;
          Dec(P);
        end;

        if (P = PStart) and (not ((P^ = FmCtl) or (P^ = ImgPan)) or (Brk[PStart - Buff + 1] = 'n')) then
        begin {no space found, forget the wrap, write the whole word and any spaces found after it}
          if BreakWord then
            LineComplete(N)
          else
          begin
            P := PStart + N - 1;

            while (P <> Last) and not CanWrapAfter(P^) and not (Brk[P - Buff + 1] in ['a', 's'])
            do
            begin
              case Brk[P - Buff + 2] of
                'n': ; // must not wrap after this char.
              else
                case (P + 1)^ of
                  ' ', FmCtl, ImgPan, BrkCh: break; // can wrap before this char.
                else
                  if WrapChar((P + 1)^) then
                    break; // can wrap before this char.
                end;
              end;
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
              MaxWidth := Max(MaxWidth, FindTextWidth(Canvas, PStart, P - PStart + 1, True));
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
  Curs := StartCurs + Len;

  if Assigned(Document.FirstLineHtPtr) and (Lines.Count > 0) then {used for List items}
    with LineRec(Lines[0]) do
      if (Document.FirstLineHtPtr^ = 0) then
        Document.FirstLineHtPtr^ := YDraw + LineHt - Descent + SpaceBefore;

  DrawHeight := AccumImgBot - TopY; {in case image overhangs}
  if DrawHeight < SectionHeight then
    DrawHeight := SectionHeight;
  Result := SectionHeight;
  ContentBot := TopY + SectionHeight;
  DrawBot := TopY + DrawHeight;
  with Document do
  begin
    if not IsCopy and (SectionNumber mod 50 = 0) and (ThisCycle <> CycleNumber)
      and (SectionCount > 0) then
      TheOwner.htProgress(ProgressStart + ((100 - ProgressStart) * SectionNumber) div SectionCount);
    ThisCycle := CycleNumber; {only once per cycle}
  end;
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TSection.DrawLogic');
   {$ENDIF}
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
    with TInlineList(Document.InlineList) do
      for I := 0 to Count - 1 do {look thru the inlinelist}
      begin
        StartBI := StartB[I];
        EndBI := EndB[I];
        LineStart := StartCurs + (Start - Buff); {offset from Section start to Line start}
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
            BR.MargArray := InlineRec(Document.InlineList.Items[I]).MargArray; {get border data}
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
    Inverted, NewCP: boolean;
    Color: TColor;
    CPx, CPy, CP1x: Integer;
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

    function ChkInversion(Start: PWideChar; out Count: Integer): boolean;
    var
      LongCount, C: Integer;
    begin
      Result := False;
      C := Start - Buff;
      Count := 32000;
      if Document.IsCopy then
        Exit;
      if (MySelE < MySelB) or ((MySelE = MySelB) and
        not Document.ShowDummyCaret) then
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
    CPx := X + LR.LineIndent;
    CP1x := CPx;
    LR.DrawY := Y - LR.LineHt;
    LR.DrawXX := CPx;
    while Cnt > 0 do
    begin
      I := 1;
      J1 := Fonts.GetFontCountAt(Start - Buff, Len) - 1;
      J2 := Images.GetImageCountAt(Start - Buff) - 1;
      J4 := FormControls.GetControlCountAt(Start - Buff) - 1;
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
      Canvas.Font.Color := ThemedColor(Canvas.Font.Color);
      if J2 = -1 then
      begin {it's an image or panel}
        Obj := Images.FindImage(Start - Buff);
        if Obj is TImageObj then
        begin
          if Obj.Floating in [ALeft, ARight] then
          begin
            //BG, 02.03.2011: Document is the Document, thus we must
            //  feed it with document coordinates: X,Y is in document coordinates,
            //  but might not be the coordinates of the upper left corner of the
            //  containing block, the origin of the Obj's coordinates. If each block
            //  had its own IMgr and nested blocks had nested IMgrs with coordinates
            //  relative to the containing block, the document coordinates of an inner
            //  block were the sum of all LfEdges of the containing blocks.
            Document.DrawList.AddImage(TImageObj(Obj), Canvas,
              IMgr.LfEdge + Obj.Indent, Obj.DrawYY, Y - Descent, FO);

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
            if Owner <> nil then
              TImageObj(Obj).Positioning := Owner.Positioning
            else
              TImageObj(Obj).Positioning := posStatic;
            TImageObj(Obj).Draw(Canvas, CPx + Obj.HSpaceL, LR.DrawY, Y - Descent, FO);
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
          end;
        end
        else
        begin {it's a Panel}
          with TPanelObj(Obj) do
          begin
            ShowIt := True;
            if Obj.Floating in [ALeft, ARight] then
            begin
              LeftT := IMgr.LfEdge + Obj.Indent;
              TopP := Obj.DrawYY - YOffset;
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
            end;
            if Document.IsCopy then
              TPanelObj(Obj).Draw(Canvas, LeftT, TopP)
            else
            begin
              Panel.Top := TopP;
              Panel.Left := LeftT;
              if Panel.FVisible then
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
        Ctrl := FormControls.FindControl(Start - Buff);
        if not Ctrl.Hidden then
          with Ctrl do
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
            if Ctrl is TRadioButtonFormControlObj then
              Inc(Topp, 2)
            else if Ctrl is TCheckBoxFormControlObj then
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
            if Document.IsCopy then
              Ctrl.Draw(Canvas, CPx + Ctrl.HSpaceL, TopP)
            else
            begin
              Show;
              Top := TopP;
              Left := CPx + Ctrl.HSpaceL;
              if Ctrl is TRadioButtonFormControlObj then
                with TRadioButtonFormControlObj(Ctrl) do
                begin
                  Show;
                  if MyCell.BkGnd then
                    Color := MyCell.BkColor
                  else
                    Color := Document.Background;
                end;
              if Ctrl.Active and ((Ctrl is TRadioButtonFormControlObj) or
                (Ctrl is TCheckBoxFormControlObj)) then
              begin
                Canvas.Brush.Color := clWhite;
                //SaveColor := SetTextColor(Handle, clBlack);
                if Document.TheOwner.ShowFocusRect then //MK20091107
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
                //SetTextColor(Handle, SaveColor);
              end;
            end;
            Inc(CPx, Width + Ctrl.HSpaceL + Ctrl.HSpaceR);
            NewCP := True;
          end;
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
            Color := ThemedColor(Color);
//            if Color < 0 then
//              Color := GetSysColor(Color and $FFFFFF)
//            else
//              Color := Color and $FFFFFF;
            Canvas.Font.Color := Color xor $FFFFFF;
          end
          else
            Canvas.Font.Color := ThemedColor(FO.TheFont.bgColor);
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
          Canvas.Brush.Color := ThemedColor(FO.TheFont.BGColor);
        end;

        if Document.Printing then
        begin
          if Document.PrintMonoBlack and
            (GetDeviceCaps(Canvas.Handle, NumColors) in [0..2]) then
          begin
            Color := Canvas.Font.Color;
            if Color < 0 then
              Color := GetSysColor(Color);
            if Color < 0 then
              Canvas.Font.Color := clBlack; {Print black}
          end;
          if not Document.PrintTableBackground then
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

        if not Document.NoOutput then
        begin
          Tmp := I;
          if Cnt - I <= 0 then
            case (Start + I - 1)^ of
              ' ', BrkCh:
                Dec(Tmp); {at end of line, don't show space or break}
            end;
          if (Self is TPreformated) and not Owner.HideOverflow then
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
          if Document.ShowDummyCaret and not Inverted
            and (MySelB = Start - Buff) then
          begin
            Canvas.Pen.Color := Canvas.Font.Color;
            Tmp := Y - Descent + FO.Descent + Addon - YOffset;
            Canvas.Brush.Color := clWhite;
            Canvas.Rectangle(CPx, Tmp, CPx + 1, Tmp - FO.FontHeight);
          end;
        end;

        if FO.Active or Document.IsCopy and Assigned(Document.LinkDrawnEvent)
          and (FO.UrlTarget.Url <> '') then
        begin
          Tmp := Y - Descent + FO.Descent + Addon - YOffset;
          ARect := Rect(CPx, Tmp - FO.FontHeight, CP1x + 1, Tmp);
          if FO.Active then
          begin
            Canvas.Font.Color := clBlack; {black font needed for DrawFocusRect}
            DC := Canvas.Handle; {Dummy call needed to make Delphi add font color change to handle}
            if Document.TheOwner.ShowFocusRect then //MK20091107
              Canvas.DrawFocusRect(ARect);
          end;
          if Assigned(Document.LinkDrawnEvent) then
            Document.LinkDrawnEvent(Document.TheOwner, Document.LinkPage,
              FO.UrlTarget.Url, FO.UrlTarget.Target, ARect);
        end;
        CPx := CP1x;

      {the following puts a dummy caret at the very end of text if it should be there}
        if Document.ShowDummyCaret and not Inverted
          and ((MySelB = Len) and (Document.Selb = Document.Len))
          and (Cnt = I) and (LineNo = Lines.Count - 1) then
        begin
          Canvas.Pen.Color := Canvas.Font.Color;
          Tmp := Y - Descent + FO.Descent + Addon - YOffset;
          Canvas.Brush.Color := clWhite;
          Canvas.Rectangle(CPx, Tmp, CPx + 1, Tmp - FO.FontHeight);
        end;

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
          BR.DrawTheBorder(Canvas, XOffset, YOffSet, Document.Printing);
        end;
      DrawTheText(I); {draw the text, etc., in this line}
      Inc(Y, SpaceAfter);
    end;
    Document.FirstPageItem := False;
  end;

begin {TSection.Draw}
  Y := YDraw;
  Result := Y + SectionHeight;
  YOffset := Document.YOff;

{Only draw if will be in display rectangle}
  if (Len > 0) and (Y - YOffset + DrawHeight + 40 >= ARect.Top) and (Y - YOffset - 40 < ARect.Bottom) then
  begin
    DC := Canvas.Handle;
    SetTextAlign(DC, TA_BaseLine);

    MySelB := Document.SelB - StartCurs;
    MySelE := Document.SelE - StartCurs;
    for I := 0 to Lines.Count - 1 do
      with Document do
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
              if (Owner <> nil) and (Owner.Positioning = PosAbsolute) then
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
  MySelB := Document.SelB - StartCurs;
  MySelE := Document.SelE - StartCurs;
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
      Document.CB.AddText(Start + X1, X2 - X1);
    end;
  if MySelE > Len then
    Document.CB.AddTextCR('', 0);
end;

{----------------TSection.PtInObject}

function TSection.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): boolean;
{Y is distance from start of section}
begin
  Result := (Images.Count > 0) and Images.PtInObject(X, Y, Obj, IX, IY);
end;

{----------------TSection.GetURL}

function TSection.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj};
  out ATitle: ThtString): guResultType;
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
    Result.Assign(UrlTarget);
  end;

begin
  Result := [];
  UrlTarg := nil;
  FormControl := nil;
{First, check to see if in an image}
  if (Images.Count > 0) and Images.PtInImage(X, Y, IX, IY, Posn, IMap, UMap, MapItem, ImageObj) then
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
    Document.ActiveImage := ImageObj;
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
        Document.ActiveLink := FO;
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
        if not ((Start + L)^ = ImgPan) then {an image here would be in HSpace area}
        begin
          Include(Result, guUrl);
          UrlTarg := MakeCopy(FO.UrlTarget);
          Document.ActiveLink := FO;
        end;
      if (FO.Title <> '') then {found a Title}
        if not ((Start + L)^ = ImgPan) then {an image here would be in HSpace area}
        begin
          ATitle := FO.Title;
          Include(Result, guTitle);
        end;
    end;
  end;
end;

{----------------TSection.FindCursor}

function TSection.FindCursor(Canvas: TCanvas; X, Y: Integer;
  out XR, YR, CaretHt: Integer; out Intext: boolean): Integer;
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
  else {prev  -- we're iterating from the end of ThtDocument}
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

function TSection.CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean;
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

function TSection.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean;
begin
  Result := False;
  if (Len = 0) or (Pos < StartCurs) or (Pos >= StartCurs + Len) then
    Exit;
  Ch := Buff[Pos - StartCurs];
  Obj := Self;
  Result := True;
end;

{----------------TPanelObj.Create}

constructor TPanelObj.Create(AMasterList: ThtDocument; Position: Integer;
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
  VertAlign := ABottom; {default}
  Floating := ANone;
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  Panel := ThvPanel.Create(PntPanel);
  Panel.Left := -4000;
  Panel.Parent := PntPanel;
  with Panel do
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
  end;
  NewSpace := -1;
  for I := 0 to L.Count - 1 do
    with TAttribute(L[I]) do
      case Which of
        HeightSy:
          if System.Pos('%', Name) = 0 then
          begin
            SpecHeight := Max(1, Value); {spec ht of 0 becomes 1}
            Panel.Height := SpecHeight; {so panels ht will be set for OnPanelCreate}
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
            Panel.Width := Value;
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
              Floating := ALeft
            else if S = 'RIGHT' then
              Floating := ARight;
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
  else if Floating in [ALeft, ARight] then
    HSpaceL := ImageSpace {default}
  else
    HSpaceL := 0;

  HSpaceR := HSpaceL;
  VSpaceB := VSpaceT;
  with Panel do
  begin
    Caption := '';
    if not ObjectTag and Assigned(AMasterList.PanelCreateEvent) then
      AMasterList.PanelCreateEvent(AMasterList.TheOwner, AName, AType, Source, Panel);
    SetWidth := Width;
    SetHeight := Height;
  end;
  AMasterList.PanelList.Add(Self);
end;

constructor TPanelObj.CreateCopy(AMasterList: ThtDocument; T: TPanelObj);
begin
  assert(AMasterList is ThtDocument);
  inherited CreateCopy(T);
  Panel := ThvPanel.Create(nil);
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
  OPanel := T.Panel; {save these for printing}
  OSender := T.fMasterList.TheOwner;
  PanelPrintEvent := T.fMasterList.PanelPrintEvent;
  IsCopy := True;
end;

destructor TPanelObj.Destroy;
begin
  if Assigned(fMasterList) and Assigned(fMasterList.PanelDestroyEvent) then
    fMasterList.PanelDestroyEvent(fMasterList.TheOwner, Panel);
  Panel.Free;
  inherited Destroy;
end;

procedure TPanelObj.DrawLogic(SectionList: ThtDocument; Canvas: TCanvas;
  FO: TFontObj; AvailableWidth, AvailableHeight: Integer);
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
        Brush.Color := ThemedColor(Panel.Color);
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
          if FAlt <> '' then
            WrapTextW(ACanvas, X1 + 5, Y1 + 5, X1 + ImageWidth - 5, Y1 + ImageHeight - 5, FAlt);
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

constructor TCell.Create(Master: ThtDocument);
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCell.Create');
   {$ENDIF}
  inherited Create(Master);
  IMgr := TIndentManager.Create;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TCell.Create');
   {$ENDIF}
end;

{----------------TCell.CreateCopy}

constructor TCell.CreateCopy(AMasterList: ThtDocument; T: TCellBasic);
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
{Do the entire layout of the cell or document. Return the total document pixel height}
var
  IB: Integer;
  LIndex, RIndex: Integer;
  SaveID: TObject;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCell.DoLogic');
  CodeSite.SendFmtMsg('Y           = [%d]',[Y]);
  CodeSite.SendFmtMsg('Width       = [%d]',[Width]);
  CodeSite.SendFmtMsg('AHeight     = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt        = [%d]',[BlHt]);
  CodeSite.SendFmtMsg('Cur         = [%d]',[Curs]);
  CodeSite.SendFmtMsg('ScrollWidth = [%d]',[ScrollWidth]);
  CodeSite.AddSeparator;
   {$ENDIF}
  IMgr.Init(0, Width);
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TCell.DoLogic');
  {$ENDIF}
end;

{----------------TCell.Draw}

function TCell.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X, Y, XRef, YRef: Integer): Integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother drawing}
begin
  IMgr.Reset(X);
  IMgr.ClipWidth := ClipWidth;
  DrawYY := Y; {This is overridden in TCellObj.Draw}
  Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y, XRef, YRef);
end;

{----------------TCellObjCell.CreateCopy}

constructor TCellObjCell.CreateCopy(AMasterList: ThtDocument; T: TCellObjCell);
begin
  inherited CreateCopy(AMasterList, T);
  MyRect := T.MyRect; ;
end;

{----------------TCellObjCell.GetUrl}

function TCellObjCell.GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget;
  out FormControl: TIDObject{TImageFormControlObj}; out ATitle: ThtString): guResultType;
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
  out ScrollWidth: Integer; var Curs: Integer): Integer;
{Do the entire layout of the this cell.  Return the total pixel height}
var
  I, Sw, TheCount: Integer;
  H, Tmp: Integer;
  SB: TSectionBase;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlockCell.DoLogicX');
  CodeSite.SendFmtMsg('Y           = [%d]',[Y]);
  CodeSite.SendFmtMsg('Width       = [%d]',[Width]);
  CodeSite.SendFmtMsg('AHeight     = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt        = [%d]',[BlHt]);
  CodeSite.SendFmtMsg('Cur         = [%d]',[Curs]);
  CodeSite.SendFmtMsg('ScrollWidth = [%d]',[ScrollWidth]);
  CodeSite.AddSeparator;
   {$ENDIF}
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
  {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Curs = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlockCell.DoLogicX');
  {$ENDIF}
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

procedure TDrawList.AddImage(Obj: TImageObj; Canvas: TCanvas; X, TopY, YBaseline: Integer; FO: TFontObj);
var
  Result: TImageRec;
begin
  Result := TImageRec.Create;
  Result.AObj := Obj;
  Result.ACanvas := Canvas;
  Result.AX := X;
  Result.AY := TopY;
  Result.AYBaseline := YBaseline;
  Result.AFO := FO;
  Add(Result);
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

constructor LineRec.Create(SL: ThtDocument);
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
    Canvas.Brush.Color := ThemedColor(MargArray[BackgroundColor]) or PalRelative;
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
  if Document.Printing then
  begin
    Y := YDraw;
    YOffset := Document.YOff;
    if (Y - YOffset > ARect.Top + 5) and (Y - YOffset < ARect.Bottom) and (Y < Document.PageBottom) then
      Document.PageBottom := Y;
  end;
end;

{----------------THorzLine.Create}

constructor THorzLine.Create(AMasterList: ThtDocument; L: TAttributeList;
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
                Prop.Assign(IntToStr(Value) + '%', piWidth);
            end
            else
              Prop.Assign(Value, piWidth);
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

  Prop.Assign(VSize, piHeight); {assigns if no property exists yet}
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

constructor THorzLine.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
begin
  inherited Create(AMasterList, T.Display);
  System.Move((T as THorzline).VSize, VSize, PtrSub(@BkGnd, @VSize) + Sizeof(BkGnd));
end;

procedure THorzLine.CopyToClipboard;
begin
  Document.CB.AddTextCR('', 0);
end;

function THorzLine.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'THorzLine.DrawLogic');
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
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
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'THorzLine.DrawLogic');
   {$ENDIF}
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
  YO := Y - Document.YOff;
  if (YO + SectionHeight >= ARect.Top) and (YO < ARect.Bottom) and
    (not Document.Printing or (Y < Document.PageBottom)) then
    with Canvas do
    begin
      YT := YO;
      XR := X + Width - 1;
      if Color <> clNone then
      begin
        Brush.Color := ThemedColor(Color) or $2000000;
        Brush.Style := bsSolid;
        FillRect(Rect(X, YT, XR + 1, YT + VSize));
      end
      else
      begin
        with Document do
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
          Pen.Color := ThemedColor(clBtnHighLight);
        MoveTo(XR, YT);
        LineTo(XR, YT + VSize - 1);
        LineTo(X, YT + VSize - 1);
        if BlackBorder then
          Pen.Color := clBlack
        else
          Pen.Color := ThemedColor(clBtnShadow);
        LineTo(X, YT);
        LineTo(XR, YT);
      end;
      Document.FirstPageItem := False; {items after this will not be first on page}
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

procedure TPreformated.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
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
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TPreFormated.DrawLogic');
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}

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
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TPreFormated.DrawLogic');
  {$ENDIF}
end;

{ THtmlPropStack }

{ Add a TProperties to the PropStack. }
procedure THtmlPropStack.PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProps: TProperties);
var
  NewProp: TProperties;
begin
  NewProp := TProperties.Create(self, Self.MasterList.UseQuirksMode);
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
      if Items[I].HasBorderStyle then
      {this would be the end of an inline border}
        MasterList.ProcessInlines(SIndex, Items[I], False);
      Delete(I);
      if I > 1 then {update any stack items which follow the deleted one}
        for J := I to Count - 1 do
          Items[J].Update(Items[J - 1], MasterList.Styles, J);
      Break;
    end;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
constructor THtmlStyleList.Create(AMasterList: ThtDocument);
begin
  inherited Create;
  MasterList := AMasterList;
  Self.FUseQuirksMode := AMasterList.UseQuirksMode;
end;

//-- BG ---------------------------------------------------------- 08.03.2011 --
procedure THtmlStyleList.setLinksActive(Value: Boolean);
begin
  inherited;
  MasterList.LinksActive := Value;
end;

{ TFieldsetBlock }

//-- BG ---------------------------------------------------------- 09.10.2010 --
procedure TFieldsetBlock.ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
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
procedure TFieldsetBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer);
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
constructor TFieldsetBlock.Create(Master: ThtDocument; Prop: TProperties; AnOwnerCell: TCellBasic;
  Attributes: TAttributeList);
var
  Index: PropIndices;
begin
  inherited;
  HasBorderStyle := True;
  for Index := BorderTopStyle to BorderLeftStyle do
    if VarIsIntNull(MargArrayO[Index]) or VarIsEmpty(MargArrayO[Index]) then
      MargArrayO[Index] := bssSolid;
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
  FLegend.FOwner := Self;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
constructor TFieldsetBlock.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
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
    Rect.Top := YDraw - Document.YOff;
    Rect.Bottom := Rect.Top + Legend.CellHeight;
    Legend.Draw(Canvas, ARect, NewWidth, Rect.Left + 2, YDraw, XRef, YRef);
    Rect := CalcClipRect(Canvas, Rect, Document.Printing);
    ExcludeClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function TFieldsetBlock.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer;
  IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer;
var
  BorderWidth: TRect;
  AutoCount, BlockHeight, ScrollWidth, L, LI, RI: Integer;
  SaveID: TObject;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.DrawLogic');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
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
    StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, self.BorderWidth, AutoCount, MargArray);
    StyleUn.ApplyBoxSettings(MargArray,Document.UseQuirksMode);
    BorderWidth.Left := MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
    BorderWidth.Right := MargArray[MarginRight] + MargArray[BorderRightWidth] + MargArray[PaddingRight];
    BorderWidth.Top  := MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop];
    if MargArray[piHeight] > 0 then
      BlockHeight := MargArray[piHeight]
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
    Legend.DoLogicX(Canvas, X + BorderWidth.Left, Y, XRef, YRef, NewWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
    IMgr.FreeLeftIndentRec(LI);
    IMgr.FreeRightIndentRec(RI);
    IMgr.CurrentID := SaveID;

    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  end;
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlock.DrawLogic');
   {$ENDIF}
end;


procedure TFormControlObj.setValue(const Value: ThtString);
begin
  FValue := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.SetWidth(Value: Integer);
begin
  TheControl.Width := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.Show;
begin
  TheControl.Show;
end;

//-- BG ---------------------------------------------------------- 03.03.2011 --
function TFormControlObj.TotalHeight: Integer;
begin
  Result := VSpaceT + Height + VSpaceB;
end;

//-- BG ---------------------------------------------------------- 03.03.2011 --
function TFormControlObj.TotalWidth: Integer;
begin
  Result := HSpaceL + Width + HSpaceR;
end;

{ TFormControlObjList }

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TFormControlObjList.ActivateTabbing;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if not ShowIt and (TheControl <> nil) then
      begin
        TheControl.Show; {makes it tab active}
        TheControl.Left := -4000; {even if it can't be seen}
      end;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TFormControlObjList.DeactivateTabbing;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if not ShowIt and (TheControl <> nil) then
        TheControl.Hide; {hides and turns off tabs}
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TFormControlObjList.GetItem(Index: Integer): TFormControlObj;
begin
  Result := Get(Index);
end;


function TFormControlObjList.FindControl(Posn: integer): TFormControlObj;
{find the control at a given character position}
var
  I: integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Pos = Posn then
      Exit;
  end;
  Result := nil;
end;

function TFormControlObjList.GetHeightAt(Posn: integer; out FormAlign: AlignmentType): Integer;
var
  Ctrl: TFormControlObj;
begin
  Ctrl := FindControl(Posn);
  if Assigned(Ctrl) then
  begin
    Result := Ctrl.Height;
    FormAlign := Ctrl.FormAlign;
  end
  else
    Result := -1;
end;

function TFormControlObjList.GetWidthAt(Posn: integer; out HSpcL, HSpcR: integer): integer;
var
  Ctrl: TFormControlObj;
begin
  Ctrl := FindControl(Posn);
  if Assigned(Ctrl) then
  begin
    Result := Ctrl.Width;
    HSpcL := Ctrl.HSpaceL;
    HSpcR := Ctrl.HSpaceR;
  end
  else
    Result := -1;
end;

function TFormControlObjList.GetControlCountAt(Posn: integer): integer;
{Return count of chars before the next form control.  0 if at the control,
 9999 if no controls after Posn}
var
  I, Pos: integer;
begin
  if Count = 0 then
  begin
    Result := 9999;
    Exit;
  end;
  I := 0;
  while I < count do
  begin
    Pos := Items[I].Pos;
    if Pos >= Posn then
      break;
    Inc(I);
  end;
  if I = Count then
    Result := 9999
  else
    Result := Items[I].Pos - Posn;
end;

procedure TFormControlObjList.Decrement(N: integer);
{called when a character is removed to change the Position figure}
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if Pos > N then
        Dec(Pos);
end;

{ TFloatingObj }

constructor TFloatingObj.CreateCopy(T: TFloatingObj);
begin
  inherited Create;
  FAlt := T.FAlt;
  ImageWidth := T.ImageWidth;
  ImageHeight := T.ImageHeight;
  NoBorder := T.NoBorder;
  BorderSize := T.BorderSize;
  Indent := T.Indent;
  VertAlign := T.VertAlign;
  Floating := T.Floating;
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
    Floating := Align;
    VertAlign := ANone;
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

  if MargArray[piWidth] <> IntNull then
  begin
    PercentWidth := False;
    if MargArray[piWidth] = Auto then
      SpecWidth := -1
    else if (VarIsStr(MargArrayO[piWidth]))
      and (System.Pos('%', MargArrayO[piWidth]) > 0) then
    begin
      PercentWidth := True;
      SpecWidth := MulDiv(MargArray[piWidth], 100, DummyHtWd);
    end
    else
      SpecWidth := MargArray[piWidth];
  end;
  if MargArray[piHeight] <> IntNull then
  begin
    PercentHeight := False;
    if MargArray[piHeight] = Auto then
      SpecHeight := -1
    else if (VarIsStr(MargArrayO[piHeight]))
      and (System.Pos('%', MargArrayO[piHeight]) > 0) then
    begin
      PercentHeight := True;
      SpecHeight := MulDiv(MargArray[piHeight], 100, DummyHtWd);
    end
    else
      SpecHeight := MargArray[piHeight];
  end;

  if Prop.GetVertAlign(Align) then
    VertAlign := Align;
  if Prop.GetFloat(Align) and (Align <> ANone) then
    Floating := Align;

  if Prop.BorderStyleNotBlank then
  begin
    NoBorder := True; {will have inline border instead}
    BorderSize := 0;
  end
  else if Prop.HasBorderStyle then
  begin
    Inc(HSpaceL, MargArray[BorderLeftWidth]);
    Inc(HSpaceR, MargArray[BorderRightWidth]);
    Inc(VSpaceT, MargArray[BorderTopWidth]);
    Inc(VSpaceB, MargArray[BorderBottomWidth]);
  end;
end;

//-- BG ---------------------------------------------------------- 30.11.2010 --
procedure TFloatingObj.SetAlt(CodePage: Integer; const Value: ThtString);
begin
  FAlt := Value;
  while Length(FAlt) > 0 do
    case FAlt[Length(FAlt)] of
      CrChar, LfChar:
        Delete(FAlt, Length(FAlt), 1);
    else
      break;
    end;
end;

//-- BG ---------------------------------------------------------- 02.03.2011 --
function TFloatingObj.TotalHeight: Integer;
begin
  Result := VSpaceT + ImageHeight + VSpaceB;
end;

//-- BG ---------------------------------------------------------- 02.03.2011 --
function TFloatingObj.TotalWidth: Integer;
begin
  Result := HSpaceL + ImageWidth + HSpaceR;
end;

{----------------TSectionBase.Create}

constructor TSectionBase.Create(AMasterList: ThtDocument; ADisplay: TPropDisplay);
begin
  inherited Create;
  FDocument := AMasterList;
  FDisplay := ADisplay;
  ContentTop := 999999999; {large number in case it has Display: none; }
end;

//-- BG ---------------------------------------------------------- 20.09.2009 --
constructor TSectionBase.Create(AMasterList: ThtDocument; AProp: TProperties);
begin
  create(AMasterList, AProp.Display);
end;

constructor TSectionBase.CreateCopy(AMasterList: ThtDocument; T: TSectionBase);
begin
  inherited Create;
  FDocument := AMasterList;
  FDisplay := T.Display; //BG, 30.12.2010: issue-43: Invisible section is printed 
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
// Computes all coordinates of the section.
//
// Normal sections, absolutely positioned blocks and floating blocks start at given (X,Y) relative to document origin.
// Table cells start at given (X,Y) coordinates relative to the outmost containing block.
//
// Returns the nominal height of the section (without overhanging floating blocks)
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSectionBase.DrawLogic');
  CodeSite.SendFmtMsg('X        = [%d]',[X]);
  CodeSite.SendFmtMsg('Y        = [%d]',[Y]);
  CodeSite.SendFmtMsg('XRef     = [%d]',[XRef]);
  CodeSite.SendFmtMsg('YRef     = [%d]',[YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
  {$ENDIF}
  StartCurs := Curs;
  Result := SectionHeight;
  DrawHeight := SectionHeight;
  MaxWidth := 0;
  ContentTop := Y;
  DrawTop := Y;
  YDraw := Y;
  ContentBot := Y + SectionHeight;
  DrawBot := Y + DrawHeight;
   {$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TSectionBase.DrawLogic');
   {$ENDIF}
end;

function TSectionBase.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
// returns the pixel row, where the section ends.
begin
  Result := YDraw + SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj};
  out ATitle: ThtString): guResultType;
begin
  Result := [];
  UrlTarg := nil;
  FormControl := nil;
end;

function TSectionBase.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;
begin
  Result := False;
  Obj := nil;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: boolean): Integer;
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

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean;
begin
  Result := False;
end;

function TSectionBase.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TObject): boolean;
begin
  Result := False;
  Obj := nil;
end;

procedure TSectionBase.SetDocument(List: ThtDocument);
begin
  FDocument := List;
end;

procedure TSectionBase.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
begin
  Min := 0;
  Max := 0;
end;

procedure TSectionBase.AddSectionsToList;
begin
  Document.addSectionsToPositionList(Self);
end;

{ TSectionBaseList }

function TSectionBaseList.CursorToXY(Canvas: TCanvas; Cursor: Integer; out X, Y: Integer): boolean;
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

{ TChPosObj }

//-- BG ---------------------------------------------------------- 04.03.2011 --
constructor TChPosObj.Create(MasterList: ThtDocument; Pos: Integer);
begin
  inherited Create;
  FChPos := Pos;
  FDocument := MasterList;
end;

//-- BG ---------------------------------------------------------- 06.03.2011 --
function TChPosObj.FreeMe: Boolean;
begin
  Result := True;
end;

function TChPosObj.GetYPosition: Integer;
var
  Pos, X, Y: Integer;
begin
  Pos := MasterList.FindDocPos(ChPos, False);
  if MasterList.CursorToXY(nil, Pos, X, Y) then
    Result := Y
  else
    Result := 0;
end;

{ TRowList }

//-- BG ---------------------------------------------------------- 26.12.2011 --
function TRowList.GetItem(Index: Integer): TCellList;
begin
  Result := Get(Index);
end;

{ TColSpecList }

//-- BG ---------------------------------------------------------- 26.12.2011 --
function TColSpecList.GetItem(Index: Integer): TColSpec;
begin
  Result := Get(Index);
end;

{ TColSpec }

//-- BG ---------------------------------------------------------- 12.01.2012 --
constructor TColSpec.Create(const Width: TSpecWidth; Align: ThtString; VAlign: AlignmentType);
begin
  inherited Create;
  FWidth := Width;
  FAlign := Align;
  FVAlign := VAlign;
end;

//-- BG ---------------------------------------------------------- 27.01.2012 --
constructor TColSpec.CreateCopy(const ColSpec: TColSpec);
begin
  Create(ColSpec.FWidth, ColSpec.FAlign, ColSpec.FVAlign);
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
