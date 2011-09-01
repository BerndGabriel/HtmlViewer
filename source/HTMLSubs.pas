{
Version   11
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011 by Bernd Gabriel

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
{$endif}
  Messages, Graphics, Controls, ExtCtrls, Classes, SysUtils, Variants, Forms, Math, Contnrs,
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc, types,
{$endif}
  HtmlGlobals,
  HtmlFonts,
  StyleTypes,
  Parser,
  HTMLUn2,
  StyleUn,
  HTMLGif2,
  HtmlImages;

type

  TSymbol = Symb;

  ThtDocument = class;
  TBlock = class;
  TCellBasic = class;

//------------------------------------------------------------------------------
// THtmlNode is base class for all objects in the HTML document tree.
//------------------------------------------------------------------------------

  THtmlNode = class(TIDObject)
  private
    FDocument: ThtDocument; // the document it belongs to
    FOwnerBlock: TBlock;    // the parental block it is placed in
    FOwnerCell: TCellBasic; // the parent's child list it is placed in
    //FTag: TSymbol;
    //FIds: ThtStringArray;
    //FClasses: ThtStringArray;
    FAttributes: TAttributeList;
    FProperties: TProperties;
  protected
    function FindAttribute(NameSy: Symb; out Attribute: TAttribute): Boolean; overload; virtual;
    function FindAttribute(Name: ThtString; out Attribute: TAttribute): Boolean; overload; virtual;
    function GetChild(Index: Integer): THtmlNode; virtual;
    function GetParent: TBlock; virtual;
//    function GetPseudos: TPseudos; virtual;
  public
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Node: THtmlNode);
    //constructor Create(Parent: THtmlNode; Tag: TSymbol; Attributes: TAttributeList; const Properties: TResultingProperties);
    function IndexOf(Child: THtmlNode): Integer; virtual;
    //function IsMatching(Selector: TSelector): Boolean;
    property Parent: TBlock read GetParent;
    property Children[Index: Integer]: THtmlNode read GetChild; default;
    property OwnerBlock: TBlock read FOwnerBlock; //BG, 07.02.2011: public for reading document structure (see issue 24). Renamed from MyBlock to Owner to clarify the relation.
    property OwnerCell: TCellBasic read FOwnerCell write FOwnerCell;
    property Document: ThtDocument read FDocument;
  end;

//------------------------------------------------------------------------------
// TSectionBase is base class for all HTML document sections
//------------------------------------------------------------------------------
// Each block is a section (see TBlock and its derivates) and a series of text
// and non block building "inline" tags is held in a section (see TSection)
//
// Base class for TSection, TBlock, THtmlTable, TPage and THorzLine
//------------------------------------------------------------------------------

  TSectionBase = class(THtmlNode)
  private
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
    // X coordinates calulated in DrawLogic() may be shifted in Draw1(), if section is centered or right aligned

    constructor Create(OwnerCell: TCellBasic; Attributes: TAttributeList; AProp: TProperties);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); virtual;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): boolean; virtual;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; virtual;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; var XR, YR, CaretHt: Integer; var Intext: boolean): Integer; virtual;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; virtual;
    function FindSourcePos(DocPos: Integer): Integer; virtual;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; virtual;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; virtual;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; virtual;
    function GetURL(Canvas: TCanvas; X, Y: Integer; var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj}; var ATitle: ThtString): guResultType; virtual;
    function PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean; virtual;
    procedure AddSectionsToList; virtual;
    procedure CopyToClipboard; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); virtual;
    property Display: TPropDisplay read FDisplay write FDisplay;
  end;

  TSectionBaseList = class(TFreeList)
  private
    function getItem(Index: Integer): TSectionBase;
  public
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; virtual;
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
    FOwnerBlock: TBlock;         // the parental block it is placed in
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

    constructor Create(MasterList: ThtDocument; OwnerBlock: TBlock);
    constructor CreateCopy(MasterList: ThtDocument; OwnerBlock: TBlock; T: TCellBasic);
    function CheckLastBottomMargin: boolean;
    function DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X, Y, XRef, YRef: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer; var XR, YR, Ht: Integer; var Intext: boolean): Integer;
    function FindSourcePos(DocPos: Integer): Integer;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
    function GetURL(Canvas: TCanvas; X, Y: Integer; var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; virtual;
    function PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean;
    procedure Add(Item: TSectionBase; TagIndex: Integer);
    procedure AddSectionsToList;
    procedure CopyToClipboard;
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); virtual;
    property Document: ThtDocument read FDocument; {the ThtDocument that holds the whole document}
    property OwnerBlock: TBlock read FOwnerBlock;
  end;

  TCell = class(TCellBasic)
  private
    DrawYY: Integer;
  public
    constructor Create(Master: ThtDocument; OwnerBlock: TBlock);
    constructor CreateCopy(AMasterList: ThtDocument; OwnerBlock: TBlock; T: TCellBasic);
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
  public
    Pos: Integer; {0..Len  Index where font takes effect}
    TheFont: ThtFont;
    FIArray: TFontInfoArray;
    FontHeight, {tmHeight+tmExternalLeading}
      tmHeight,
      tmMaxCharWidth,
      Overhang,
      Descent: Integer;
    SScript: AlignmentType;
    TabControl: ThtTabControl;
    constructor Create(ASection: TSection; F: ThtFont; Position: Integer);
    constructor CreateCopy(ASection: TSection; T: TFontObj);
    destructor Destroy; override;
    procedure ReplaceFont(F: ThtFont);
    procedure ConvertFont(const FI: ThtFontInfo);
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
    function GetFontAt(Posn: Integer; var OHang: Integer): ThtFont;
    function GetFontCountAt(Posn, Leng: Integer): Integer;
    function GetFontObjAt(Posn: Integer; var Index: Integer): TFontObj;
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

  TFloatingObj = class(THtmlNode)
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
    //constructor Create(Document: ThtDocument; Parent: TCellBasic);
    constructor CreateCopy(Document: ThtDocument; Parent: TCellBasic; T: TFloatingObj);
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
    procedure SetVisible(Value: boolean);
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  TPanelCreateEvent = procedure(Sender: TObject; const AName, AType, SRC: ThtString; Panel: ThvPanel) of object;
  TPanelDestroyEvent = procedure(Sender: TObject; Panel: ThvPanel) of object;
  TPanelPrintEvent = procedure(Sender: TObject; Panel: ThvPanel; const Bitmap: TBitmap) of object;

  TPanelObj = class(TFloatingObj)
  private
    SetWidth, SetHeight: Integer;
    IsCopy: boolean;
  public
    ShowIt: boolean;
    Panel, OPanel: ThvPanel;
    OSender: TObject;
    PanelPrintEvent: TPanelPrintEvent;
    FUserData: TObject;
    FMyPanelObj: TPanelObj;
    constructor Create(MasterList: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; ObjectTag: boolean);
    constructor CreateCopy(MasterList: ThtDocument; Parent: TCellBasic; T: TPanelObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: ThtDocument; Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure Draw(ACanvas: TCanvas; X1, Y1: Integer);
  end;


  HoverType = (hvOff, hvOverUp, hvOverDown);

  TImageFormControlObj = class;

  TImageObj = class(TFloatingObj) {inline image info}
  private
    FSource: ThtString;
    FImage: ThtImage;
    OrigImage: ThtImage; {same as above unless swapped}
    Transparent: TTransparency; {None, Lower Left Corner, or Transp GIF}
    FHover: HoverType;
    FHoverImage: boolean;
    AltHeight, AltWidth: Integer;
    Positioning: PositionType;
    function GetBitmap: TBitmap;
    procedure SetHover(Value: HoverType);
//    procedure SetImage(const AValue: ThtImage);
  public
    ObjHeight, ObjWidth: Integer; {width as drawn}
    IsMap, UseMap: boolean;
    MapName: ThtString;
    MyFormControl: TImageFormControlObj; {if an <INPUT type=image}
    //unused: MyCell: TCellBasic;
    Swapped: boolean; {image has been replaced}
    Missing: boolean; {waiting for image to be downloaded}
  public
    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList);
    constructor SimpleCreate(Document: ThtDocument; Parent: TCellBasic; const AnURL: ThtString);
    constructor CreateCopy(AMasterList: ThtDocument; Parent: TCellBasic; T: TImageObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: ThtDocument; Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: ThtImage);
    procedure Draw(Canvas: TCanvas; X: Integer; TopY, YBaseline: Integer; FO: TFontObj);
    function InsertImage(const UName: ThtString; Error: boolean; var Reformat: boolean): boolean;

    property Bitmap: TBitmap read GetBitmap;
    property Hover: HoverType read FHover write SetHover;
    property Image: ThtImage read FImage ; //write SetImage;
    property Source: ThtString read FSource; {the src= attribute}
    procedure ReplaceImage(NewImage: TStream);
  end;

  TDrawList = class(TFreeList)
    procedure AddImage(Obj: TImageObj; Canvas: TCanvas; X, TopY, YBaseline: Integer; FO: TFontObj);
    procedure DrawImages;
  end;


  TFloatingObjList = class(TFreeList) {a list of TImageObj's and TPanelObj's}
  public
    constructor CreateCopy(AMasterList: ThtDocument; Parent: TCellBasic; T: TFloatingObjList);
    function FindImage(Posn: Integer): TFloatingObj;
    function GetHeightAt(Posn: Integer; var AAlign: AlignmentType; var FlObj: TFloatingObj): Integer;
    function GetImageCountAt(Posn: Integer): Integer;
    function GetWidthAt(Posn: Integer; var AAlign: AlignmentType; var HSpcL, HSpcR: Integer; var FlObj: TFloatingObj): Integer;
    function PtInImage(X, Y: Integer; var IX, IY, Posn: Integer; var AMap, UMap: boolean; var MapItem: TMapItem; var ImageObj: TImageObj): boolean;
    function PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean;
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
    BuffS: UnicodeString;  {holds the text or one of #2 (Form), #4 (Image/Panel), #8 (break char) for the section}
    Buff: PWideChar;    {same as above}
    Brk: string;        // Brk[n]: Can I wrap to new line after BuffS[n]? One of 'a' (optional), 'n' (no), 's' (soft), 'y' (yes) per character in BuffS
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
    BreakWord: Boolean;
    TextWidth: Integer;
    WhiteSpaceStyle: TWhiteSpaceStyle;

    constructor Create(OwnerCell: TCellBasic; Attr: TAttributeList; Prop: TProperties; AnURL: TUrlTarget; FirstItem: boolean);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
    destructor Destroy; override;
    function AddFormControl(Which: Symb; AMasterList: ThtDocument; L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TFormControlObj;
    function AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer): TImageObj;
    function AddPanel(L: TAttributeList; ACell: TCellBasic; Index: Integer): TPanelObj;
    function CreatePanel(L: TAttributeList; ACell: TCellBasic): TPanelObj;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function FindCountThatFits(Canvas: TCanvas; Width: Integer; Start: PWideChar; Max: Integer): Integer;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; var XR, YR, CaretHt: Integer; var Intext: boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; override;
    function FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: boolean): Integer;
    function FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer; var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
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
    //OwnerCell: TCellBasic; //TODO -oBG, 10.03.2011: should be in TSectionBase instead of FDocument and FOwnerBlock
    BGImage: TImageObj;    //TODO -oBG, 10.03.2011: see also bkGnd and bkColor in TCellBasic one background should be enough.
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
    IsListBlock: boolean;
    PRec: PtPositionRec;
    Positioning: PositionType;
    Visibility: VisibilityType;
    BottomAuto: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    HideOverflow: Boolean;
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

    constructor Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
    destructor Destroy; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; var XR, YR, CaretHt: Integer; var Intext: boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; virtual;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
    function PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean; override;
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
    Document: ThtDocument;
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

  TFormControlObj = class(THtmlNode)
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

    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); virtual;
    constructor CreateCopy(Parent: TCellBasic; T: TFormControlObj); virtual;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; virtual;
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
    function GetHeightAt(Posn: Integer; var FormAlign: AlignmentType): Integer;
    function GetWidthAt(Posn: Integer; var HSpcL, HSpcR: Integer): Integer;
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
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
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
    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
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
    //TODO -oBG, 24.03.2011: remove param Typ and activate override
    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; const Typ: ThtString; L: TAttributeList; Prop: TProperties); reintroduce;//override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
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
    //TODO -oBG, 24.03.2011: remove param Typ and activate override
    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; const Typ: ThtString; L: TAttributeList; Prop: TProperties); reintroduce;//override;
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
    //xMyCell: TCellBasic;
    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
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
    constructor Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; var S: ThtString): boolean; override;
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
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
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
  public
    constructor Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties;
      Sy: Symb; APlain: boolean; AIndexType: ThtChar;
      AListNumb, ListLevel: Integer);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property ListNumb: Integer read FListNumb;
    property ListStyleType: ListBulletType read FListStyleType;
    property ListType: ListTypeType read FListType;
    property ListFont: TFont read FListFont;
  end;

  TFieldsetBlock = class(TBlock)
  private
    FLegend: TBlockCell;
  protected
    procedure ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer); override;
    procedure ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
  public
    constructor Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
    destructor Destroy; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property Legend: TBlockCell read FLegend;
  end;

  TBodyBlock = class(TBlock)
  public
    constructor Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
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
    constructor CreateCopy(Document: ThtDocument; Parent: TBlock; T: TCellObjCell);
    function GetURL(Canvas: TCanvas; X, Y: Integer; var UrlTarg: TUrlTarget;
      var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
  end;

  IntArray = array of Integer;

  TCellObj = class(TObject)
  {holds one cell of the table and some other information}
  //BG, 25.03.2011: this is the <td> resp.<th> block, isn't it?
  //  If so, why is it not a "TCellBlock" derived from TBlock
  public
    // BEGIN: this area is copied by move() in CreateCopy() - NO string types allowed!
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
    HasBorderStyle: Boolean;
    ShowEmptyCells: Boolean;
    // END: this area is copied by move() in CreateCopy()
    Cell: TCellObjCell;

    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage: TGpObject;
    TiledMask, FullBG: TBitmap;
    MargArray: TMarginArray;
    MargArrayO: TVMarginArray;
    NoMask: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;

    constructor Create(Master: ThtDocument; Parent: TBlock; AVAlign: AlignmentType; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: ThtDocument; Parent: TBlock; T: TCellObj);
    destructor Destroy; override;
  private
    procedure InitializeCell(TablePadding: Integer; const BkImageName: ThtString;
      const APRec: PtPositionRec; Border: boolean);
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: Integer;
      Border: boolean; Light, Dark: TColor);
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: Integer; var Curs: Integer);
  end;

  TCellList = class(TFreeList)
  {holds one row of the html table, a list of TCellObj}
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
    constructor CreateCopy(AMasterList: ThtDocument; T: TCellList);
    procedure InitializeRow;
    function DrawLogic1(Canvas: TCanvas; const Widths: IntArray; Span, CellSpacing, AHeight, Rows: Integer;
      var Desired: Integer; var Spec, More: boolean): Integer;
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: Integer; var Curs: Integer);
    function Draw(Canvas: TCanvas; Document: ThtDocument; const ARect: TRect; const Widths: IntArray;
      X, Y, YOffset, CellSpacing: Integer; Border: boolean; Light, Dark: TColor; MyRow: Integer): Integer;
    procedure Add(CellObj: TCellObj);
    property Items[Index: Integer]: TCellObj read getCellObj;
  end;

  TColObj = class
    colWidth: Integer;
    colAsPercent: boolean;
    colAlign: ThtString;
    colVAlign: AlignmentType;
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

    constructor Create(OwnerCell: TCellBasic; Attr: TAttributeList; Prop: TProperties; ATable: THtmlTable; TableLevel: Integer);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
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
    constructor Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties; ATableBlock: TTableBlock);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
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
    procedure DrawTable(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y: Integer);
    procedure DrawTableP(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y: Integer);
    procedure FindRowHeights(Canvas: TCanvas; AHeight: Integer);
  public
    Rows: TFreeList;        {a list of TCellLists}
    ListsProcessed: Boolean;
    Indent: Integer;        {table indent}
    BorderWidth: Integer;   {width of border}
    Float: Boolean;         {if floating}
    NumCols: Integer;       {Number columns in table}
    TableWidth: Integer;    {width of table}
    tblWidthAttr: Integer;  {Width attribute as entered}
    UseAbsolute: boolean;   {width entries are considered absolute}
    TableHeight: Integer;   {height of table itself, not incl caption}
    CellPadding: Integer;
    CellSpacing: Integer;
    HSpace, VSpace: Integer; {horizontal, vertical extra space}
    BorderColor: TColor;      //BG, 13.06.2010: added for Issue 5: Table border versus stylesheets
    BorderColorLight: TColor;
    BorderColorDark: TColor;
    EndList: boolean;        {marker for copy}
    DrawX: Integer;
    DrawY: Integer;
    BkGnd: boolean;
    BkColor: TColor;
    ColInfo: TFreeList;
    Percents: IntArray;     {percent widths of columns}
    Widths: IntArray;       {holds column widths}
    MaxWidths: IntArray;
    MinWidths: IntArray;
    Heights: IntArray;

    constructor Create(OwnerCell: TCellBasic; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
    destructor Destroy; override;
    procedure DoColumns(Width: Integer; AsPercent: boolean; VAlign: AlignmentType; const Align: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
    procedure AddDummyCells;
    procedure GetMinMaxAbs(Canvas: TCanvas; out TotalMinWidth, TotalMaxWidth: Integer);
    procedure GetWidthsAbs(Canvas: TCanvas; TablWidth: Integer; Specified: boolean);
    procedure GetWidths(Canvas: TCanvas; out TotalMinWidth, TotalMaxWidth: Integer; TheWidth: Integer);
    procedure TableSpecifiedAndWillFit(TheWidth: Integer);
    procedure TableNotSpecifiedAndWillFit(TotalMinWidth, TotalMaxWidth, TheWidth: Integer);
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj};
      var ATitle: ThtString): guResultType; override;
    function PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; var XR, YR, CaretHt: Integer; var Intext: boolean): Integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): boolean; override;
    function GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    procedure CopyToClipboard; override;
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
    constructor Create(Document: ThtDocument; Pos: Integer);
    property ChPos: Integer read FChPos;
    property Document: ThtDocument read FDocument;
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
    Document: ThtDocument;
  protected
    procedure setLinksActive(Value: Boolean); override;
  public
    constructor Create(AMasterList: ThtDocument);
  end;

  ThtDocument = class(TCell) {a list of all the sections -- the html document}
  private
    procedure AdjustFormControls;
    procedure AddSectionsToPositionList(Sections: TSectionBase);
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
//    BackgroundBitmap: TGpObject; //TBitmap;
//    BackgroundMask: TBitmap;
//    BackgroundAniGif: TGifImage;
    BackgroundImage: ThtImage;
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
    ImageCache: ThtImageCache;
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
    constructor CreateCopy(T: ThtDocument);
    destructor Destroy; override;
    function AddChPosObjectToIDNameList(const S: ThtString; Pos: Integer): Integer; {$ifdef UseInline} inline; {$endif}
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer; var Y: Integer): boolean; override;
    function DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer; Y, XRef, YRef: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: boolean): Integer; override;
    function FindSectionAtPosition(Pos: Integer; var TopPos, Index: Integer): TSectionBase;
    function GetFormcontrolData: TFreeList;
    function GetSelLength: Integer;
    function GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
    function GetTheImage(const BMName: ThtString; var Transparent: TTransparency; out FromCache, Delay: boolean): ThtImage;
    function GetURL(Canvas: TCanvas; X, Y: Integer;
      var UrlTarg: TUrlTarget; var FormControl: TIDObject {TImageFormControlObj}; var ATitle: ThtString): guResultType; override;
    procedure CancelActives;
    procedure CheckGIFList(Sender: TObject);
    procedure Clear; override;
    procedure ClearLists;
    procedure CopyToClipboardA(Leng: Integer);
    procedure GetBackgroundBitmap;
    procedure HideControls;
    procedure InsertImage(const Src: ThtString; Stream: TStream; var Reformat: boolean);
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

    constructor Create(OwnerCell: TCellBasic; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(OwnerCell: TCellBasic; T: TSectionBase); override;
    procedure CopyToClipboard; override;
    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
      var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  TPreFormated = class(TSection)
  {section for preformated, <pre>}
//  public
//    procedure ProcessText(TagIndex: Integer); override;
//    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
//      var MaxWidth, Curs: Integer): Integer; override;
//    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
  end;

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

  THtmlPropStack = class(TPropStack)
  public
    Document: ThtDocument;
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

var
  // TODO: must be part of the THtmlParser or the ThtDocument.
  PropStack: THtmlPropStack;
  NoBreak: boolean; {set when in <NoBr>}
  WaitStream: TMemoryStream;

implementation

uses
{$IFNDEF NoGDIPlus}
  GDIPL2A,
{$ENDIF}
{$IFNDEF NoTabLink}
  HtmlView,
{$endif}
  HtmlSbs1,
  ReadHtml;

//-- BG ---------------------------------------------------------- 10.12.2010 --
function htCompareText(const T1, T2: ThtString): Integer;
begin
  Result := WideCompareText(T1, T2);
end;

procedure InitializeFontSizes(Size: Integer);
var
  I: Integer;
begin
  //BG, 01.05.2011: FontConvBase[1] relates to css font size 'x-small'.
  FontConv[1] := FontConvBase[0] * Size;
  PreFontConv[1] := PreFontConvBase[0] * Size;
  for I := 2 to 7 do
  begin
    FontConv[I] := FontConvBase[I] * Size;
    PreFontConv[I] := PreFontConvBase[I] * Size;
  end;
end;

{ THtmlNode }

//-- BG ---------------------------------------------------------- 24.03.2011 --
constructor THtmlNode.Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties);
begin
  inherited Create;
  FOwnerCell := Parent;
  FOwnerBlock := Parent.OwnerBlock;
  FDocument := Parent.Document;
  FAttributes := Attributes;
  FProperties := Properties;
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
constructor THtmlNode.CreateCopy(Parent: TCellBasic; Node: THtmlNode);
begin
  inherited Create;
  FOwnerCell := Parent;
  FOwnerBlock := Parent.OwnerBlock;
  FDocument := Parent.Document;
  FAttributes := Node.FAttributes;
  FProperties := Node.FProperties;
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlNode.FindAttribute(NameSy: Symb; out Attribute: TAttribute): Boolean;
begin
  Attribute := nil;
  Result := (FAttributes <> nil) and FAttributes.Find(NameSy, Attribute);
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlNode.FindAttribute(Name: ThtString; out Attribute: TAttribute): Boolean;
begin
  Attribute := nil;
  Result := (FAttributes <> nil) and FAttributes.Find(Name, Attribute);
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlNode.GetChild(Index: Integer): THtmlNode;
begin
  Result := nil; //TODO -oBG, 24.03.2011
end;

//-- BG ---------------------------------------------------------- 23.03.2011 --
function THtmlNode.GetParent: TBlock;
begin
  Result := FOwnerBlock;
end;

//function THtmlNode.GetPseudos: TPseudos;
//begin
//  Result := []; //TODO -oBG, 24.03.2011
//end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlNode.IndexOf(Child: THtmlNode): Integer;
begin
  Result := -1; //TODO -oBG, 24.03.2011
end;

////-- BG ---------------------------------------------------------- 23.03.2011 --
//function THtmlNode.IsMatching(Selector: TSelector): Boolean;
//
//  function IsMatchingSimple: Boolean;
//
//    function IncludesStringArray(S, F: ThtStringArray): Boolean;
//    var
//      I: Integer;
//    begin
//      Result := Length(S) <= Length(F);
//      if not Result then
//        exit;
//      for I := Low(S) to High(S) do
//        if IndexOfString(F, S[I]) < 0 then
//          exit;
//      Result := True;
//    end;
//
//  var
//    Index: Integer;
//    Attribute: TAttribute;
//    Match: TAttributeMatch;
//    S: TSymbol;
//  begin
//    Result := False;
//
//    // http://www.w3.org/TR/2010/WD-CSS2-20101207/selector.html
//    // If all conditions in the selector are true for a certain element, the selector matches the element.
//
//    if Selector.Pseudos <> [] then
//      if not (Selector.Pseudos >= GetPseudos) then
//        exit;
//
//    // a loop about tags? there is one or none tag in the selector.
//    for Index := Low(Selector.Tags) to High(Selector.Tags) do
//      if TryStrToReservedWord(Selector.Tags[Index], S) then
//        if S <> FTag then
//          exit;
//
//    // a loop about ids? CSS 2.1 allows more than 1 ID, but most browsers do not support them.
//    if not IncludesStringArray(Selector.Ids, FIds) then
//      exit;
//
//    if not IncludesStringArray(Selector.Classes, FClasses) then
//      exit;
//
//    for Index := 0 to Selector.AttributeMatchesCount - 1 do
//    begin
//      Match := Selector.AttributeMatches[Index];
//      if not FindAttribute(Match.Name, Attribute) then
//        exit;
//      case Match.Oper of
//        //no more checks here. Attribute it set! amoSet: ;       // [name] : matches, if attr is set and has any value.
//
//        amoEquals:     // [name=value] : matches, if attr equals value.
//          if htCompareString(Match.Value, Attribute.AsString) <> 0 then
//            break;
//
//        amoContains:   // [name~=value] : matches, if attr is a white space separated list of values and value is one of these values.
//          if PosX(Match.Value + ' ', Attribute.AsString + ' ', 1) = 0 then
//            break;
//
//        amoStartsWith: // [name|=value] : matches, if attr equals value or starts with value immediately followed by a hyphen.
//          if PosX(Match.Value + '-', Attribute.AsString + '-', 1) <> 1 then
//            break;
//        end;
//      end;
//
//    Result := True;
//  end;
//
//  function IsChild(Selector: TSelector): Boolean;
//  var
//    P: THtmlNode;
//  begin
//    P := Parent;
//    Result := (P <> nil) and P.IsMatching(Selector);
//  end;
//
//  function IsDescendant(Selector: TSelector): Boolean;
//  var
//    Node: THtmlNode;
//  begin
//    Result := False;
//    Node := Parent;
//    while Node <> nil do
//    begin
//      Result := Node.IsMatching(Selector);
//      if Result then
//        break;
//      Node := Node.Parent;
//    end;
//  end;
//
//  function IsFollower(Selector: TSelector): Boolean;
//  var
//    P: THtmlNode;
//    I: Integer;
//  begin
//    P := Parent;
//    Result := P <> nil;
//    if Result then
//    begin
//      I := P.IndexOf(Self);
//      if I > 0 then
//        Result := P[I - 1].IsMatching(Selector);
//    end;
//  end;
//
//begin
//  Result := IsMatchingSimple;
//  if Result then
//    if Selector is TCombinedSelector then
//      case TCombinedSelector(Selector).Combinator of
//        scChild:      Result := IsChild(TCombinedSelector(Selector).LeftHand);
//        scDescendant: Result := IsDescendant(TCombinedSelector(Selector).LeftHand);
//        scFollower:   Result := IsFollower(TCombinedSelector(Selector).LeftHand);
//      end;
//end;


{ TFontObj }

var
  NLevel: Integer; {for debugging}

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

constructor TFontObj.Create(ASection: TSection; F: ThtFont; Position: Integer);
begin
  inherited Create;
  FSection := ASection;
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

procedure TFontObj.ReplaceFont(F: ThtFont);
begin
  TheFont.Free;
  TheFont := F;
  FontChanged;
end;

procedure TFontObj.ConvertFont(const FI: ThtFontInfo);
begin
  TheFont.Assign(FI);
  FontChanged;
end;

constructor TFontObj.CreateCopy(ASection: TSection; T: TFontObj);
begin
  inherited Create;
  FSection := ASection;
  Pos := T.Pos;
  SScript := T.SScript;
  TheFont := ThtFont.Create;
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
  var OHang: Integer): ThtFont;
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

function TFontList.GetFontObjAt(Posn: Integer; var Index: Integer): TFontObj;
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
var ImgageCount: Integer;
constructor TImageObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList);
var
  I: Integer;
  S: ThtString;
  NewSpace: Integer;
  T: TAttribute;
begin
  inc(ImgageCount);  
  inherited Create(Parent, L, nil);
  Pos := Position;
  VertAlign := ABottom; {default}
  Floating := ANone; {default}
  NewSpace := -1;
  SpecHeight := -1;
  SpecWidth := -1;
  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        SrcSy:
          FSource := htTrim(Name);

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
            S := htUpperCase(htTrim(Name));
            if (Length(S) > 1) and (S[1] = '#') then
              System.Delete(S, 1, 1);
            MapName := S;
          end;

        AlignSy:
          begin
            S := htUpperCase(htTrim(Name));
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
  else if Floating in [ALeft, ARight] then
    HSpaceL := ImageSpace {default}
  else
    HSpaceL := 0;
  HSpaceR := HSpaceL;
  VSpaceB := VSpaceT;
end;

constructor TImageObj.SimpleCreate(Document: ThtDocument; Parent: TCellBasic; const AnURL: ThtString);
begin
  inherited Create(Parent, nil, nil);
  VertAlign := ABottom; {default}
  Floating := ANone; {default}
  FSource := AnURL;
  NoBorder := True;
  BorderSize := 0;
  SpecHeight := -1;
  SpecWidth := -1;
end;

constructor TImageObj.CreateCopy(AMasterList: ThtDocument; Parent: TCellBasic; T: TImageObj);
begin
  inherited CreateCopy(AMasterList, Parent, T); //TODO -oBG, 24.03.2011: add parent
  ImageKnown := T.ImageKnown;
  ObjHeight := T.ObjHeight;
  ObjWidth := T.ObjWidth;
  SpecHeight := T.SpecHeight;
  SpecWidth := T.SpecWidth;
  PercentWidth := T.PercentWidth;
  PercentHeight := T.PercentHeight;
  FImage := T.Image;
  IsMap := T.IsMap;
  Transparent := T.Transparent;
  FSource := T.FSource;
end;

destructor TImageObj.Destroy;
begin
  if not Document.IsCopy then
  begin
    if (Source <> '') and Assigned(OrigImage) then
      Document.ImageCache.DecUsage(htUpperCase(htTrim(Source)));
    if Swapped and (Image <> OrigImage) then
    begin {not in cache}
      Image.Free;
    end;
    if (OrigImage is ThtGifImage) and ThtGifImage(OrigImage).Gif.IsCopy then
      OrigImage.Free;
  end;
//  FBitmap.Free;
  inherited Destroy;
end;

function TImageObj.GetBitmap: TBitmap;
begin
  if Image is ThtBitmapImage then
  begin
    if Image.Bitmap = ErrorBitmap then
      Result := nil
    else
    begin
//      if FBitmap = nil then
//      begin
//        FBitmap := TBitmap.Create;
//        FBitmap.Assign(TBitmap(Image));
//        if ColorBits = 8 then
//          FBitmap.Palette := CopyPalette(ThePalette);
//      end;
//      Result := FBitmap;
      Result := Image.Bitmap;
    end
  end
  else if Image <> nil  then
    Result := Image.Bitmap
  else
    Result := nil;
end;

procedure TImageObj.SetHover(Value: HoverType);
begin
  if (Value <> FHover) and FHoverImage and (Image is ThtGifImage) then
    with ThtGifImage(Image).Gif do
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

//procedure TImageObj.SetImage(const AValue: ThtImage);
//begin
//  if FImage = AValue then exit;
//  FImage := AValue;
//end;

{----------------TImageObj.ReplaceImage}

procedure TImageObj.ReplaceImage(NewImage: TStream);
var
  TmpImage: ThtImage;
  I: Integer;
begin
  Transparent := NotTransp;
  TmpImage := LoadImageFromStream(NewImage, Transparent);
  if Assigned(TmpImage) then
  begin
    // remove current image
    if not Swapped then
    begin
    {OrigImage is left in cache and kept}
      if Image is ThtGifImage then
        Document.AGifList.Remove(ThtGifImage(Image).Gif);
      Swapped := True;
    end
    else {swapped already}
    begin
      if Image is ThtGifImage then
        Document.AGifList.Remove(ThtGifImage(Image).Gif);
      FImage.Free;
    end;

    // set new image
    FImage := TmpImage;
    if Image is ThtGifImage then
    begin
      if not FHoverImage then
      begin
        ThtGifImage(Image).Gif.Animate := True;
        Document.AGifList.Add(ThtGifImage(Image).Gif);
      end
      else
      begin
        ThtGifImage(Image).Gif.Animate := False;
        SetHover(hvOff);
      end;
    end;
    if Missing then
    begin {if waiting for image, no longer want it}
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
  end;
end;

{----------------TImageObj.InsertImage}

function TImageObj.InsertImage(const UName: ThtString; Error: boolean; var Reformat: boolean): boolean;
var
  TmpImage: ThtImage;
  FromCache, Delay: boolean;
begin
  Result := False;
  Reformat := False;
  if FImage = DefImage then
  begin
    Result := True;
    if Error then
      FImage := ErrorImage
    else
    begin
      TmpImage := Document.GetTheImage(UName, Transparent, FromCache, Delay);
      if not Assigned(TmpImage) then
        Exit;

      if TmpImage is ThtGifImage then
      begin
        if FromCache then {it would be}
          FImage := ThtGifImage(TmpImage).Clone {it's in Cache already, make copy}
        else
          FImage := TmpImage;
        if not FHoverImage then
        begin
          ThtGifImage(Image).Gif.Animate := True;
          Document.AGifList.Add(ThtGifImage(Image).Gif);
          if Assigned(Document.Timer) then
            Document.Timer.Enabled := True;
        end
        else
          ThtGifImage(Image).Gif.Animate := False;
      end
      else
        FImage := TmpImage;
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
  TempImage: ThtImage;
  ImHeight, ImWidth: Integer;
  ViewImages, FromCache: boolean;
  Rslt: ThtString;
  ARect: TRect;
  SubstImage: Boolean;
  HasBlueBox: Boolean;
  UName: ThtString;
begin
  ViewImages := Document.ShowImages;
  if ViewImages then
  begin
    if FImage = nil then
    begin
      TempImage := nil;
      UName := htUpperCase(htTrim(Source));                               
      if UName <> '' then
      begin
        if not Assigned(Document.GetBitmap) and not Assigned(Document.GetImage) then
          FSource := Document.TheOwner.HtmlExpandFilename(Source)
        else if Assigned(Document.ExpandName) then
        begin
          Document.ExpandName(Document.TheOwner, Source, Rslt);
          FSource := Rslt;
        end;
        UName := htUpperCase(htTrim(Source));
        if Document.MissingImages.IndexOf(UName) = -1 then
          TempImage := Document.GetTheImage(Source, Transparent, FromCache, Missing)
        else
          Missing := True; {already in list, don't request it again}
      end;

      if TempImage = nil then
      begin
        if Missing then
        begin
          FImage := DefImage;
          Document.MissingImages.AddObject(UName, Self); {add it even if it's there already}
        end
        else
        begin
          FImage := ErrorImage;
        end;
      end
      else if TempImage is ThtGifImage then
      begin
        if FromCache then
          FImage := ThtGifImage(TempImage).Clone {it's in Cache already, make copy}
        else
          FImage := TempImage;
        OrigImage := FImage;
        if not FHoverImage then
        begin
          ThtGifImage(Image).Gif.Animate := True;
          Document.AGifList.Add(ThtGifImage(Image).Gif);
          if Assigned(Document.Timer) then
            Document.Timer.Enabled := True;
        end
        else
          ThtGifImage(Image).Gif.Animate := False;
      end
      else
      begin
        FImage := TempImage; //TBitmap(TmpImage);
        OrigImage := FImage;
      end;
    end;
  end
  else
    FImage := DefImage;

  ImHeight := Image.Height;
  ImWidth := Image.Width;

  SubstImage := (Image = ErrorImage) or (Image = DefImage);

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
var
  LastExceptionMessage: String;
  LastDdImage: ThtImage;
procedure TImageObj.DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: ThtImage);
{Y relative to top of display here}
var
  W, H: Integer;
begin
  if (ddImage = ErrorImage) or (ddImage = DefImage) then
  begin
    W := ddImage.Width;
    H := ddImage.Height;
  end
  else if ddImage = nil then
    exit
  else
  begin
    W := ObjWidth;
    H := ObjHeight;
  end;
  try
    if Document.IsCopy then
      ddImage.Print(Canvas, XX, Y, W, H, clWhite)
    else
      ddImage.Draw(Canvas, XX, Y, W, H);
  except
    on E: Exception do
    begin
      LastExceptionMessage := E.Message;
      LastDdImage := ddImage;
    end;
  end;
end;

{----------------TImageObj.Draw}

//-- BG ---------------------------------------------------------- 12.06.2010 --
procedure GetRaisedColors(SectionList: ThtDocument; Canvas: TCanvas; out Light, Dark: TColor);
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
function htRaisedColors(SectionList: ThtDocument; Canvas: TCanvas; Raised: Boolean): htColorArray; overload;
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

procedure RaisedRect(SectionList: ThtDocument; Canvas: TCanvas;
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

procedure TImageObj.Draw(Canvas: TCanvas; X: Integer; TopY, YBaseline: Integer; FO: TFontObj);
var
  TmpImage: ThtImage;
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
  ViewImages := Document.ShowImages;
  Dec(TopY, Document.YOff);
  Dec(YBaseLine, Document.YOff);

  if ViewImages then
    TmpImage := Image
  else
    TmpImage := DefImage;
  SubstImage := not ViewImages or (TmpImage = ErrorImage) or (TmpImage = DefImage); {substitute image}

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
    Self.DoDraw(Canvas, DrawXX + Ofst, DrawYY + Ofst, TmpImage);
  Inc(DrawYY, Document.YOff);
  SetTextAlign(Canvas.Handle, TA_Top);
  if SubstImage and (BorderSize = 0) then
  begin
    Canvas.Font.Color := FO.TheFont.Color;
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
      Pen.Color := FO.TheFont.Color;
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

constructor TFloatingObjList.CreateCopy(AMasterList: ThtDocument; Parent: TCellBasic; T: TFloatingObjList);
var
  I: Integer;
  Item: TObject;
begin
  inherited create;
  for I := 0 to T.Count - 1 do
  begin
    Item := T.Items[I];
    if Item is TImageObj then
      Add(TImageObj.CreateCopy(AMasterList, Parent, TImageObj(Item)))
    else
      Add(TPanelObj.CreateCopy(AMasterList, Parent, TPanelObj(Item)));
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

function TFloatingObjList.GetHeightAt(Posn: Integer; var AAlign: AlignmentType;
  var FlObj: TFloatingObj): Integer;
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

function TFloatingObjList.GetWidthAt(Posn: Integer; var AAlign: AlignmentType;
  var HSpcL, HSpcR: Integer; var FlObj: TFloatingObj): Integer;
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

function TFloatingObjList.PtInImage(X, Y: Integer; var IX, IY, Posn: Integer;
  var AMap, UMap: boolean; var MapItem: TMapItem; var ImageObj: TImageObj): boolean;
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

function TFloatingObjList.PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean;
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
  Document := AMasterList;
  AMasterList.htmlFormList.Add(Self);
  Method := 'Get';
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
      with L[I] do
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
    Document.TheOwner.KeyDown(Key, Shift);
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
  if Assigned(Document.SubmitForm) then
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
    Document.SubmitForm(Document.TheOwner, Action, Target, EncType, Method, SL);
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

constructor TFormControlObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  I: Integer;
begin
  inherited Create(Parent, L, Prop);
  Pos := Position;
  if not Assigned(CurrentForm) then {maybe someone forgot the <form> tag}
    CurrentForm := ThtmlForm.Create(Document, nil);
  Document.FormControlList.Add(Self);
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
            with Document.TabOrderList do
              AddObject(Format('%.5d%.3d', [Value, Count]), Self);
        TitleSy: FTitle := Name;
        DisabledSy: Disabled := (Lowercase(Name) <> 'no') and (Name <> '0');
        ReadonlySy: ReadOnly := True;
      end;

  if L.TheID <> '' then
    Document.IDNameList.AddObject(L.TheID, Self);
  AttributeList := L.CreateStringList;
  FormAlign := ABottom; {ABaseline set individually}
  MyForm.InsertControl(Self);
end;

constructor TFormControlObj.CreateCopy(Parent: TCellBasic; T: TFormControlObj);
begin
  inherited CreateCopy(Parent, T);
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
  Document.TheOwner.ControlMouseMove(Self, Shift, X, Y);
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
  if Document.IsCopy then
    Exit;
  Active := True;
  Document.PPanel.Invalidate;
  Document.ControlEnterEvent(Self);
{$IFNDEF FastRadio}
  Document.FormControlList.ActivateTabbing;
{$ENDIF}
  if Assigned(Document.ObjectFocus) and (OnFocusMessage <> '') then
    Document.ObjectFocus(Document.TheOwner, Self, OnFocusMessage);
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
  Document.AdjustFormControls;
{$ENDIF}
  Active := False;
  if OnChangeMessage <> '' then
    DoOnChange;
  if Assigned(Document.ObjectBlur) and (OnBlurMessage <> '') then
    Document.ObjectBlur(Document.TheOwner, Self, OnBlurMessage);
  Document.PPanel.Invalidate;
end;

procedure TFormControlObj.DoOnChange;
begin
end;

procedure TFormControlObj.Draw(Canvas: TCanvas; X1, Y1: Integer);
begin end;

procedure TFormControlObj.ResetToValue;
begin end;

function TFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
begin
  Result := False;
end;

procedure TFormControlObj.FormControlClick(Sender: TObject);
begin
  if Assigned(Document.ObjectClick) then
    Document.ObjectClick(Document.TheOwner, Self, OnClickMessage);
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

constructor TImageFormControlObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); 
var
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited;
  XPos := -1; {so a button press won't submit image data}

  PntPanel := Document.PPanel;
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
function THiddenFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
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

constructor TEditFormControlObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer;
  const Typ: ThtString; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  Tmp: ThtFont;
begin
  inherited Create(Document, Parent, Position, L, Prop);
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
  PntPanel := Document.PPanel;
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
      DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, Document.PrintMonoBlack, False, Color)
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

function TEditFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
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
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

{----------------TButtonFormControlObj.Create}

constructor TButtonFormControlObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer;
  const Typ: ThtString; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
  Tmp: ThtFont;
begin
  inherited Create(Document, Parent, Position, L, Prop);
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
  PntPanel := Document.PPanel;
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
    MonoBlack := Document.PrintMonoBlack and (GetDeviceCaps(Canvas.Handle, BITSPIXEL) = 1) and
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
      DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, True, Document.PrintMonoBlack, False, clWhite);
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
    if Assigned(Document.FileBrowse) and (MyEdit is TEditFormControlObj) then
    begin
      S := MyEdit.Text;
      Document.FileBrowse(Document.TheOwner, MyEdit, S);
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

constructor TCheckBoxFormControlObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited;
  if Value = '' then
    Value := 'on';
  FormAlign := ABaseline;
  if L.Find(CheckedSy, T) then
    IsChecked := True;
  PntPanel := Document.PPanel;
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
    DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, Document.PrintMonoBlack, Disabled, clWhite);
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

function TCheckBoxFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
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
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

{----------------TRadioButtonFormControlObj.Create}

constructor TRadioButtonFormControlObj.Create(Document: ThtDocument; Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
  Ctrl: TFormControlObj;
  RadioButtonFormControlObj: TRadioButtonFormControlObj absolute Ctrl;
  I: Integer;
  SetTabStop: boolean;
begin
  //TODO -oBG, 24.03.2011: what is ACell? Child or Parent?
  // I think it is parent and is used to address the co-radiobuttons
  inherited;
  //xMyCell := ACell;
  PntPanel := Document.PPanel;
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
    MonoBlack := Document.PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
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

function TRadioButtonFormControlObj.GetSubmission(Index: Integer; var S: ThtString): boolean;
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
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

{----------------TCellBasic.Create}

constructor TCellBasic.Create(MasterList: ThtDocument; OwnerBlock: TBlock);
begin
  inherited Create;
  FOwnerBlock := OwnerBlock;
  FDocument := MasterList;
end;

{----------------TCellBasic.CreateCopy}

constructor TCellBasic.CreateCopy(MasterList: ThtDocument; OwnerBlock: TBlock; T: TCellBasic);
var
  I: Integer;
  Tmp, Tmp1: TSectionBase;
begin
  inherited Create;
  FOwnerBlock := OwnerBlock;
  FDocument := MasterList;
  OwnersTag := T.OwnersTag;
  for I := 0 to T.Count - 1 do
  begin
    Tmp := T.Items[I];
    Tmp1 := TSectionClass(Tmp.ClassType).CreateCopy(Self, Tmp);
    Add(Tmp1, 0);
  end;
end;

{----------------TCellBasic.Add}

procedure TCellBasic.Add(Item: TSectionBase; TagIndex: Integer);
var
  Section: TSection absolute Item;
begin
  if Assigned(Item) then
  begin
    if Item is TSection then
      if Assigned(Section.XP) then {XP not assigned if printing}
      begin
        Section.ProcessText(TagIndex);
        if not (Section.WhiteSpaceStyle in [wsPre, wsPreWrap, wsPreLine]) and (Section.Len = 0)
          and not Section.AnchorName and (Section.ClearAttr = clrNone) then
        begin
          Section.CheckFree;
          Item.Free; {discard empty TSections that aren't anchors}
          Exit;
        end;
      end;
    inherited Add(Item);
    Item.SetDocument(Document);
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

function TCellBasic.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
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

function TCellBasic.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
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
  if not Assigned(Document) then
    Exit; {dummy cell}
  SLB := Document.SelB;
  SLE := Document.SelE;
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

procedure TCellBasic.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
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

constructor TBlock.Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
var
  Clr: ClearAttrType;
  S: ThtString;
begin
  inherited Create(OwnerCell, Attributes, Prop);

  MyCell := TBlockCell.Create(Document, Self);
  MyCell.OwnersTag := Prop.PropTag;
  DrawList := TList.Create;

  Prop.GetVMarginArray(MargArrayO);
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
    BGImage := TImageObj.SimpleCreate(Document, MyCell, S);
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
    Document.IDNameList.AddObject(Attributes.TheID, Self);
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
          MargArray[MarginTop] := Max(0, MargArray[MarginTop] - OwnerBlock.MargArray[MarginTop]);
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
procedure TBlock.ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
begin
  MyCell.MinMaxWidth(Canvas, Min, Max);
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
procedure TBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer);
begin
  StyleUn.ConvMargArray(MargArrayO, BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth, AutoCount, MargArray);
end;

{----------------TBlock.CreateCopy}

constructor TBlock.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
var
  TT: TBlock;
begin
  inherited;
  TT := T as TBlock;
  System.Move(TT.MargArray, MargArray, PtrSub(@Converted, @MargArray) + Sizeof(Converted));
  MyCell := TBlockCell.CreateCopy(Document, Self, TT.MyCell);
  DrawList := TList.Create;
  if Assigned(TT.BGImage) and Document.PrintTableBackground then
    BGImage := TImageObj.CreateCopy(Document, MyCell, TT.BGImage);
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
  Min := Math.Max(MinCell, MargArray[piWidth]) + LeftSide + RightSide;
  if MargArray[piWidth] > 0 then
    Max := Min
  else
    Max := Math.Max(MaxCell, MargArray[piWidth]) + LeftSide + RightSide;
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
    if (BlockTitle <> '') and PtInRect(MyRect, Point(X, Y - Document.YOFF)) then
    begin
      ATitle := BlockTitle;
      Include(Result, guTitle);
    end;
    Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  end;
end;

{----------------TBlock.FindString}

function TBlock.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
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

function TBlock.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
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
    else
      MargArray[piWidth] := Max(MinWidth, AWidth - BordWidth);
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
  ContentMinMaxWidth(Canvas, MinWidth, MaxWidth);
  HideOverflow := HideOverflow and (MargArray[piWidth] <> Auto) and (MargArray[piWidth] > 20);
  case AutoCount of
    0:
      begin
        if not HideOverflow then
          MargArray[piWidth] := Max(MinWidth, MargArray[piWidth]);
        if (Justify in [centered, Right]) and (Positioning = posStatic)
          and not (FloatLR in [ALeft, ARight]) and
          (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
        begin
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
    1: if MargArray[piWidth] = Auto then
        CalcWidth
      else
      begin
        if not HideOverflow then
          MargArray[piWidth] := Max(MargArray[piWidth], MinWidth);
        if MargArray[MarginRight] = Auto then
          if (FloatLR in [ALeft, ARight]) then
            MargArray[MarginRight] := 0
          else
            CalcMargRt
        else
          CalcMargLf;
      end;
    2: if MargArray[piWidth] = Auto then
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
          MargArray[piWidth] := Max(MargArray[piWidth], MinWidth);
        Marg2 := Max(0, AWidth - MargArray[piWidth] - BordPad);
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
  Result := MargArray[piWidth];
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

    if MargArray[piHeight] > 0 then
      BlockHeight := MargArray[piHeight]
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
      if BGImage.Image = ErrorImage then
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
    SB.FOwnerBlock := Self;
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

      //with Document do
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
                HeightNeeded := HeaderHeight + FootHeight +
                  TCellList(Rows.Items[HeaderRowCount]).RowHeight;
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

  procedure InitFullBg(W, H: Integer);
  begin
    if not Assigned(FullBG) then
    begin
      FullBG := TBitmap.Create;
      if Document.IsCopy then
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
  YOffset: Integer;
  XR, YB, RefX, RefY, TmpHt: Integer;
  SaveID: TObject;
  ImgOK, HasBackgroundColor: boolean;
  IT, IH, FT, IW: Integer;
  Rgn, SaveRgn, SaveRgn1: HRgn;
  OpenRgn: Boolean;
  PdRect, CnRect: TRect; // padding rect, content rect
begin
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

  // CnRect is the content rectangle of this block in screen coordinates
  CnRect.Left   := PdRect.Left   + MargArray[PaddingLeft];
  CnRect.Top    := PdRect.Top    + MargArray[PaddingTop];
  CnRect.Right  := PdRect.Right  - MargArray[PaddingRight];
  CnRect.Bottom := PdRect.Bottom - MargArray[PaddingBottom];

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
        if NeedDoImageStuff and Assigned(BGImage) and (BGImage.Image <> DefImage) then
        begin
          if BGImage.Image = ErrorImage then {Skip the background image}
            FreeAndNil(BGImage)
          else
          try
            if FloatLR in [ALeft, ARight] then
              TmpHt := DrawBot - ContentTop + MargArray[PaddingTop] + MargArray[PaddingBottom]
            else
              TmpHt := ClientContentBot - ContentTop + MargArray[PaddingTop] + MargArray[PaddingBottom];

            DoImageStuff(Canvas, MargArray[PaddingLeft] + NewWidth + MargArray[PaddingRight],
              TmpHt, BGImage.Image, PRec, TiledImage, TiledMask, NoMask);
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
          Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
          Canvas.Brush.Style := bsSolid;
          if Document.IsCopy and ImgOK then
          begin
            InitFullBG(IW, IH);
            FullBG.Canvas.Brush.Color := MargArray[BackgroundColor] or PalRelative;
            FullBG.Canvas.Brush.Style := bsSolid;
            FullBG.Canvas.FillRect(Rect(0, 0, IW, IH));
          end
          else
            Canvas.FillRect(Rect(PdRect.Left, FT, PdRect.Right, FT + IH));
        end;

        if ImgOK then
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
              InitFullBG(PdRect.Right - PdRect.Left, IH);
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

      except
      end;
    end;

    if HideOverflow then
    begin
// BG, 28.08.2011: always clip to content rect:    
//      if FloatLR = ANone then
        GetClippingRgn(Canvas, Rect(CnRect.Left, CnRect.Top, CnRect.Right, CnRect.Bottom), Document.Printing, Rgn, SaveRgn)
//      else
//        GetClippingRgn(Canvas, Rect(PdRect.Left, PdRect.Top, PdRect.Right, PdRect.Bottom), Document.Printing, Rgn, SaveRgn)
      ;
      SelectClipRgn(Canvas.Handle, Rgn);
    end;
    try
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
    finally
      if HideOverflow then {restore any previous clip region}
      begin
        SelectClipRgn(Canvas.Handle, SaveRgn);
        DeleteObject(Rgn);
        if SaveRgn <> 0 then
          DeleteObject(SaveRgn);
      end;
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

constructor TTableAndCaptionBlock.Create(
  OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties; ATableBlock: TTableBlock);
var
  I: Integer;
begin
  inherited Create(OwnerCell, Attributes, Prop);
  TableBlock := ATableBlock;
  Justify := TableBlock.Justify;

  for I := 0 to Attributes.Count - 1 do
    with Attributes[I] do
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
    Document.IDNameList.AddObject(TableID, TableBlock);
end;

{----------------TTableAndCaptionBlock.CreateCopy}

constructor TTableAndCaptionBlock.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
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
end;

{----------------TTableAndCaptionBlock.MinMaxWidth}

procedure TTableAndCaptionBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
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

constructor TTableBlock.Create(
  OwnerCell: TCellBasic; Attr: TAttributeList; Prop: TProperties; ATable: THtmlTable; TableLevel: Integer);
var
  I, AutoCount, BorderColor: Integer;
  Percent: boolean;
  S,W,C: PropIndices;
begin
  inherited Create(OwnerCell, Attr, Prop);
  Table := ATable;
  Justify := NoJustify;

  for I := 0 to Attr.Count - 1 do
    with Attr[I] do
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
          BkGnd := TryStrToColor(Name, False, BkColor);
        BackgroundSy:
          if not Assigned(BGImage) then
          begin
            BGImage := TImageObj.SimpleCreate(Document, MyCell, Name);
            PRec[1].PosType := bpDim;
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
          if (VarType(MargArrayO[piHeight]) in VarInt) and (MargArrayO[piHeight] = IntNull) then
            MargArrayO[piHeight] := Name;
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
    HasBorderStyle := True; //bssOutset;
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
end;

{----------------TTableBlock.CreateCopy}

constructor TTableBlock.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
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
  Min, Max, M: Integer;
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
    Result := AWidth - LeftSide - RightSide;
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

constructor THRBlock.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
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
end;

{----------------TBlockLI.Create}

constructor TBlockLI.Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties;
  Sy: Symb; APlain: boolean; AIndexType: ThtChar; AListNumb, ListLevel: Integer);
var
  Tmp: ListBulletType;
  S: ThtString;
  TmpFont: ThtFont;
begin
  inherited Create(OwnerCell, Attributes, Prop);

  Tmp := Prop.GetListStyleType;
  if Tmp <> lbBlank then
    FListStyleType := Tmp;
  case Sy of

    UlSy, DirSy, MenuSy:
      begin
        FListType := Unordered;
        if APlain then
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
        if FListStyleType = lbNone then
          MargArrayO[PaddingLeft] := 0
        else
          MargArrayO[PaddingLeft] := ListIndent;

      DLSy:
        MargArrayO[PaddingLeft] := 0;
    else
      MargArrayO[PaddingLeft] := ListIndent;
    end;

  FListNumb := AListNumb;
  FListFont := ThtFont.Create;
  TmpFont := Prop.GetFont;
  FListFont.Assign(TmpFont);
  TmpFont.Free;

  S := Prop.GetListStyleImage;
  if S <> '' then
    Image := TImageObj.SimpleCreate(Document, MyCell, S);
end;

constructor TBlockLI.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
var
  TT: TBlockLI;
begin
  inherited;
  TT := T as TBlockLI;
  FListType := TT.FListType;
  FListNumb := TT.FListNumb;
  FListStyleType := TT.FListStyleType;
  if Assigned(TT.Image) then
    Image := TImageObj.CreateCopy(Document, MyCell, TT.Image);
  FListFont := ThtFont.Create;
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
  if Assigned(Image) then
  begin
    Image.DrawLogic(Document, Canvas, nil, 100, 0);
    if Image.Image = ErrorImage then
      FreeAndNil(Image);
  end;
  Document.FirstLineHtPtr := @FirstLineHt;
  FirstLineHt := 0;
  try
    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  finally
    Document.FirstLineHtPtr := nil;
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
    YB := FirstLineHt - Document.YOff;
    if (YB < ARect.Top - 50) or (YB > ARect.Bottom + 50) then
      Exit;
    if Assigned(Image) and (Image.Image <> DefImage) and Document.ShowImages then
      Image.DoDraw(Canvas, X - 16, YB - Image.ObjHeight, Image.Image)
    else
      if not (ListType in [None, Definition]) then
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

constructor TBodyBlock.Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
var
  PRec: PtPositionRec;
  Image: ThtString;
  Val: TColor;
begin
  inherited;
  positioning := PosStatic; {7.28}
  Prop.GetBackgroundPos(0, 0, PRec);
  if Prop.GetBackgroundImage(Image) and (Image <> '') then
    Document.SetBackgroundBitmap(Image, PRec);
  Val := Prop.GetBackgroundColor;
  if Val <> clNone then
    Document.SetBackGround(Val or PalRelative);
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
  StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderWidth, AutoCount, MargArray);

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
  inherited Create(Self, nil);
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
  ImageCache := T.ImageCache; {same list}
  InlineList := T.InlineList; {same list}
  IsCopy := True;
  inherited CreateCopy(Self, nil, T);
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
end;

function ThtDocument.GetURL(Canvas: TCanvas; X: Integer; Y: Integer;
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
  IsAniGifBackground: Boolean;
  I: Integer;
  Frame: Integer;
begin
  if IsCopy then
    Exit;
  Frame := 0;
  IsAniGifBackground := BackgroundImage is ThtGifImage;
  if IsAniGifBackground then
  begin
    IsAniGifBackground := ThtGifImage(BackgroundImage).Gif.Animate;
    if IsAniGifBackground then
      Frame := ThtGifImage(BackgroundImage).Gif.CurrentFrame;
  end;
  for I := 0 to AGifList.Count - 1 do
    with TGifImage(AGifList.Items[I]) do
      if ShowIt then
        CheckTime(PPanel);
  if IsAniGifBackground then
    if Frame <> ThtGifImage(BackgroundImage).Gif.CurrentFrame then
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
  BackgroundImage := nil;
  if BitmapLoaded and (BitmapName <> '') then
    ImageCache.DecUsage(BitmapName);
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

procedure ThtDocument.ClearLists;
{called from DoBody to clear some things when starting over}
begin
  PanelList.Clear;
  if Assigned(FormControlList) then
    FormControlList.Clear;
end;

function ThtDocument.GetSelLength: Integer;
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

//------------------------------------------------------------------------------
procedure ThtDocument.CopyToClipboardA(Leng: Integer);
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

//------------------------------------------------------------------------------
function ThtDocument.GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;

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

//------------------------------------------------------------------------------
function ThtDocument.DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
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
  if (BackgroundImage is ThtGifImage) and not IsCopy then
    ThtGifImage(BackgroundImage).Gif.ShowIt := True;
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
  BackgroundImage := nil;
  BitmapName := Name;
  BitmapLoaded := False;
  BackgroundPRec := APrec;
end;

//------------------------------------------------------------------------------
procedure ThtDocument.InsertImage(const Src: ThtString; Stream: TStream; var Reformat: boolean);
var
  UName: ThtString;
  I, J: Integer;
  Image: ThtImage;
  Rformat, Error: boolean;
  Transparent: TTransparency;
  Obj: TObject;
begin
  Image := nil;
  Error := False;
  Reformat := False;
  UName := htUpperCase(htTrim(Src));
  I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
  J := MissingImages.IndexOf(UName); {see if it's in missing image list}
  if (I = -1) and (J >= 0) then
  begin
    Transparent := NotTransp;
    Image := LoadImageFromStream(Stream, Transparent);
    if Image <> nil then {put in Cache}
    begin
      ImageCache.AddObject(UName, Image); {put new bitmap in list}
      ImageCache.DecUsage(UName); {this does not count as being used yet}
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

//------------------------------------------------------------------------------
function ThtDocument.GetTheImage(
  const BMName: ThtString; var Transparent: TTransparency; out FromCache, Delay: boolean): ThtImage;
{Note: bitmaps and Mask returned by this routine are on "loan".  Do not destroy them}
{Transparent may be set to NotTransp or LLCorner on entry but may discover it's TGif here}

  procedure GetTheBitmap;
  var
    Color: TColor;
    Bitmap: TBitmap;
  begin
    if Assigned(GetBitmap) then
    begin {the OnBitmapRequest event}
      Bitmap := nil;
      Color := -1;
      GetBitmap(TheOwner, BMName, Bitmap, Color);
      if Bitmap <> nil then
        if Color <> -1 then
          Result := ThtBitmapImage.Create(Bitmap, GetImageMask(TBitmap(Result), True, Color), TrGif)
        else if Transparent = LLCorner then
          Result := ThtBitmapImage.Create(Bitmap, GetImageMask(TBitmap(Result), False, 0), LLCorner)
        else
          Result := ThtBitmapImage.Create(Bitmap, nil, NotTransp);
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
      else if Stream <> nil then
        try
          Result := LoadImageFromStream(Stream, Transparent);
        finally
          if assigned(GottenImage) then
            GottenImage(TheOwner, BMName, Stream);
        end
      else
        Result := LoadImageFromFile(TheOwner.HtmlExpandFilename(BMName), Transparent);
    end;
  end;

var
  UName: ThtString;
  I: Integer;
begin
  Result := nil;
  Delay := False;
  FromCache := False;
  if BMName <> '' then
  begin
    UName := htUpperCase(htTrim(BMName));
    I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
    if I >= 0 then
    begin {yes, handle the case where the image is already loaded}
      Result := ImageCache.GetImage(I);
      FromCache := True;
    end
    else
    begin
    {The image is not loaded yet, need to get it}
      if Assigned(GetBitmap) or Assigned(GetImage) then
      begin
        GetTheBitmap;
        GetTheStream;
      end
      else
        Result := LoadImageFromFile(BMName, Transparent);

      if Result <> nil then {put in Image List for use later also}
        ImageCache.AddObject(UName, Result); {put new bitmap in list}
    end;
  end;
end;

//------------------------------------------------------------------------------
function ThtDocument.FindSectionAtPosition(Pos: Integer;
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

procedure ThtDocument.GetBackgroundBitmap;
var
  Dummy1: TTransparency;
  FromCache, Delay: boolean;
  Rslt: ThtString;
  I: Integer;
  UName: ThtString;
begin
  UName := htUpperCase(htTrim(BitmapName));
  if ShowImages and (UName <> '') then
    if BackgroundImage = nil then
    begin
      if BitmapLoaded then
      begin
        I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
        if I >= 0 then
          BackgroundImage := ImageCache.GetImage(I);
      end
      else
      begin
        Dummy1 := NotTransp;
        if not Assigned(GetBitmap) and not Assigned(GetImage) then
          BitmapName := TheOwner.HtmlExpandFilename(BitmapName)
        else if Assigned(ExpandName) then
        begin
          ExpandName(TheOwner, BitmapName, Rslt);
          BitmapName := Rslt;
        end;
        BackgroundImage := GetTheImage(BitmapName, Dummy1, FromCache, Delay); {might be Nil}
        if Delay then
          MissingImages.AddObject(htUpperCase(htTrim(BitmapName)), Self);
        BitmapLoaded := True;
      end;
      if BackgroundImage is ThtGifImage then
        if ThtGifImage(BackgroundImage).Gif.IsAnimated and not IsCopy then
        begin
          AGifList.Add(ThtGifImage(BackgroundImage).Gif);
          ThtGifImage(BackgroundImage).Gif.Animate := True;
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
function ThtDocument.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X: Integer;
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

procedure ThtDocument.ProcessInlines(SIndex: Integer; Prop: TProperties; Start: boolean);
{called when an inline property is found to specify a border}
var
  I, EmSize, ExSize: Integer;
  Result: InlineRec;
  MargArrayO: TVMarginArray;
  Dummy1: Integer;
begin
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

constructor TCellObj.Create(Master: ThtDocument; Parent: TBlock; AVAlign: AlignmentType; Attr: TAttributeList; Prop: TProperties);
{Note: on entry Attr and Prop may be Nil when dummy cells are being created}
var
  I, AutoCount: Integer;
  Color: TColor;
  BackgroundImage: ThtString;
  Percent: boolean;
  Algn: AlignmentType;
  J: PropIndices;
//  Border: Boolean;
begin
  inherited Create;
  Cell := TCellObjCell.Create(Master, Parent);
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
            Cell.BkGnd := TryStrToColor(Name, False, Cell.BkColor);
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
    Percent := (VarIsStr(MargArrayO[piWidth])) and (Pos('%', MargArrayO[piWidth]) > 0);
    ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, 0, AutoCount, MargArray);
    if MargArray[piWidth] > 0 then
      if Percent then
      begin
        if MargArray[piWidth] < 100 then
        begin
          AsPercent := True;
          WidthAttr := MargArray[piWidth] * 10;
        end;
      end
      else
      begin
        WidthAttr := MargArray[piWidth];
        AsPercent := False;
      end;
    if MargArray[piHeight] > 0 then
      SpecHt := MargArray[piHeight];
    Color := Prop.GetBackgroundColor;
    if Color <> clNone then
    begin
      Cell.BkGnd := True;
      Cell.BkColor := Color;
    end;
    Prop.GetBackgroundImage(BackgroundImage); {'none' will change ThtString to empty}
    if BackgroundImage <> '' then
    begin
      BGImage := TImageObj.SimpleCreate(Master, Cell, BackgroundImage);
      Prop.GetBackgroundPos(EmSize, ExSize, PRec);
    end;

  {In the following, Padding widths in percent aren't accepted}
    ConvMargArrayForCellPadding(MargArrayO, EmSize, ExSize, MargArray);
    PadTop := MargArray[PaddingTop];
    PadRight := MargArray[PaddingRight];
    PadBottom := MargArray[PaddingBottom];
    PadLeft := MargArray[PaddingLeft];

    HasBorderStyle := False;
    if BorderStyleType(MargArray[BorderTopStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      BrdTop := MargArray[BorderTopWidth];
    end;
    if BorderStyleType(MargArray[BorderRightStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      BrdRight := MargArray[BorderRightWidth];
    end;
    if BorderStyleType(MargArray[BorderBottomStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      BrdBottom := MargArray[BorderBottomWidth];
    end;
    if BorderStyleType(MargArray[BorderLeftStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      BrdLeft := MargArray[BorderLeftWidth];
    end;
//    if Border then
//      BorderStyle := Prop.GetBorderStyle
//    else
//      BorderStyle := bssNone;

    for J := BorderTopColor to BorderLeftColor do
      if MargArray[J] = clNone then
        MargArray[J] := clSilver;

    Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
    ShowEmptyCells := Prop.ShowEmptyCells;
  end;
end;

constructor TCellObj.CreateCopy(AMasterList: ThtDocument; Parent: TBlock; T: TCellObj);
begin
  inherited create;
  Cell := TCellObjCell.CreateCopy(AMasterList, Parent, T.Cell);
  Move(T.ColSpan, ColSpan, PtrSub(@Cell, @ColSpan));

  if Cell.Document.PrintTableBackground then
  begin
    Cell.BkGnd := T.Cell.BkGnd;
    Cell.BkColor := T.Cell.BkColor;
  end
  else
    Cell.BkGnd := False;
  if Assigned(T.BGImage) and Cell.Document.PrintTableBackground then
    BGImage := TImageObj.CreateCopy(AMasterList, Cell, T.BGImage);
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
  if Border and not HasBorderStyle then // (BorderStyle = bssNone) then
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
    BGImage := TImageObj.SimpleCreate(Cell.Document, Cell, BkImageName);
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
  if Assigned(BGImage) and Cell.Document.ShowImages then
  begin
    BGImage.DrawLogic(Cell.Document, Canvas, nil, 100, 0);
    if BGImage.Image = ErrorImage then
      FreeAndNil(BGImage)
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
      if Cell.Document.IsCopy then
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
  YO := Y - Cell.Document.YOff;

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
      else if BGImage.Image <> DefImage then
      begin
        if BGImage.Image = ErrorImage then {Skip the background image}
          FreeAndNil(BGImage)
        else
        try
          DoImageStuff(Canvas, Wd - CellSpacing, Ht - CellSpacing,
            BGImage.Image, PRec, TiledImage, TiledMask, NoMask);
          if Cell.Document.IsCopy and (TiledImage is TBitmap) then
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
      and Cell.Document.ShowImages;

    if Cell.BkGnd then
    begin
      Canvas.Brush.Color := Cell.BkColor or PalRelative;
      Canvas.Brush.Style := bsSolid;
      if Cell.Document.IsCopy and ImgOK then
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
            InflateRect(BRect, -1, -1);
        Canvas.FillRect(BRect);
      end;
    end;
    if ImgOK then
    begin
      if not Cell.Document.IsCopy then
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
            Cell.Document.ScaleX, Cell.Document.ScaleY);
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
      if not Cell.Document.Printing then
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
        MargArray[BackgroundColor], Cell.Document.Printing);
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
      with Attr[I] do
        case Which of
          BGColorSy:
            BkGnd := TryStrToColor(Name, False, BkColor);
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
      Add(TCellObj.CreateCopy(AMasterList, nil, T.Items[I]))
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

function TCellList.DrawLogic1(Canvas: TCanvas; const Widths: IntArray; Span,
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

function TCellList.Draw(Canvas: TCanvas; Document: ThtDocument; const ARect: TRect; const Widths: IntArray;
  X, Y, YOffset, CellSpacing: Integer; Border: boolean; Light, Dark: TColor; MyRow: Integer): Integer;
var
  I, Spacing: Integer;
  YO: Integer;
  CellObj: TCellObj;
begin
  YO := Y - YOffset;
  Result := RowHeight + Y;
  Spacing := CellSpacing div 2;

  with Document do {check CSS page break properties}
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

  with Document do {avoid splitting any small rows}
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
    (not Document.Printing or (Y < Document.PageBottom)) then
    for I := 0 to Count - 1 do
    begin
      CellObj := TCellObj(Items[I]);
      if Assigned(CellObj) then
        CellObj.Draw(Canvas, ARect, X, Y, CellSpacing, Border, Light, Dark);
      X := X + Widths[I];
    end;
end;

{----------------THtmlTable.Create}

constructor THtmlTable.Create(OwnerCell: TCellBasic; Attr: TAttributeList; Prop: TProperties);
var
  I: Integer;
begin
  //TODO -oBG, 08.06.2010: Issue 5: Table border versus stylesheets:
  //  Added: BorderColor
  inherited Create(OwnerCell, Attr, Prop);
  Rows := TFreeList.Create;
  CellPadding := 1;
  CellSpacing := 2;
  BorderColor := clNone;
  BorderColorLight := clBtnHighLight;
  BorderColorDark := clBtnShadow;
  for I := 0 to Attr.Count - 1 do
    with Attr[I] do
      case Which of
        BorderSy:
          //BG, 15.10.2010: issue 5: set border width only, if style does not set any border width:
          if not Prop.HasBorderWidth then
            if Name = '' then
              BorderWidth := 1
            else
              BorderWidth := Min(100, Max(0, Value)); {Border=0 is no border}

        CellSpacingSy:
          CellSpacing := Min(40, Max(-1, Value));

        CellPaddingSy:
          CellPadding := Min(50, Max(0, Value));

        BorderColorSy:
          TryStrToColor(Name, False, BorderColor);

        BorderColorLightSy:
          TryStrToColor(Name, False, BorderColorLight);

        BorderColorDarkSy:
          TryStrToColor(Name, False, BorderColorDark);
      end;
  if Prop.Collapse then
    Cellspacing := -1;
end;

{----------------THtmlTable.CreateCopy}

constructor THtmlTable.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
var
  I: Integer;
  HtmlTable: THtmlTable absolute T;
begin
  assert(T is THtmlTable);
  inherited;
  Rows := TFreeList.Create;
  for I := 0 to HtmlTable.Rows.Count - 1 do
    Rows.Add(TCellList.CreateCopy(Document, TCellList(HtmlTable.Rows.Items[I])));

  Move(HtmlTable.ListsProcessed, ListsProcessed, PtrSub(@EndList, @ListsProcessed));

  SetLength(Widths, NumCols);
  SetLength(MaxWidths, NumCols);
  SetLength(MinWidths, NumCols);
  SetLength(Percents, NumCols);

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
    Result := TCellObj.Create(Document, nil, ATop, nil, nil);
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

procedure THtmlTable.GetMinMaxAbs(Canvas: TCanvas; out TotalMinWidth,
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

procedure THtmlTable.GetWidths(Canvas: TCanvas; out TotalMinWidth, TotalMaxWidth: Integer;
  TheWidth: Integer);
var
  I, J, Min, Max, N, Span, Addon, Distributable, TotalPC, Accum,
    ExcessMin, ExcessMax, NonPC, PCWidth, NewTotalPC: Integer;
  Cells: TCellList;
  CellObj: TCellObj;
  MaxSpans: array of Integer;
  MaxSpan: Integer;
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

  //BG, 29.01.2011: data for loop termination and to speed up looping through
  //  very large tables with large spans.
  //  A table with 77 rows and 265 columns and a MaxSpan of 265 in 2 rows
  //  was processed in 3 seconds before the tuning and 80ms afterwards.
  MaxSpan := 1;
  SetLength(MaxSpans, Rows.Count);
  for J := 0 to Rows.Count - 1 do
    MaxSpans[J] := 1;
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
        if CellObj <> nil then
          if CellObj.ColSpan = Span then
            with CellObj do
            begin
              PCWidth := 0;
              if CellObj.WidthAttr > 0 then
                if CellObj.AsPercent then
                  PCWidth := CellObj.WidthAttr
                else if TheWidth > 0 then
                  PCWidth := Math.Min(1000, MulDiv(WidthAttr, 1000, TheWidth));

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
            end
          else
            //BG, 29.01.2011: at this point: CellObj.ColSpan <> Span
            if Span = 1 then
            begin
              //BG, 29.01.2011: at this point: in the first loop with a CellObj.ColSpan > 1.

              // Reduce too large ColSpans, or the column loop tuning will not process all cells.
              if CellObj.ColSpan > NumCols - I then
                CellObj.ColSpan := NumCols - I;

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
    Inc(Span);
  until Span > MaxSpan;

  TotalMaxWidth := 0;
  TotalMinWidth := 0;
  for I := 0 to NumCols - 1 do
  begin
    Inc(TotalMaxWidth, MaxWidths[I]);
    Inc(TotalMinWidth, MinWidths[I]);
  end;
end;

{----------------THtmlTable.MinMaxWidth}

procedure THtmlTable.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
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
  var MaxWidth, Curs: Integer): Integer;
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
  Inc(Document.TableNestLevel);
  Inc(NLevel);
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
    else if Document.InLogic2 and (Document.TableNestLevel <= 5) then
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
    Dec(Document.TableNestLevel);
    Dec(NLevel);
  end;
end;

{----------------THtmlTable.Draw}

function THtmlTable.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  YO, YOffset, Y: Integer;
begin
  Inc(Document.TableNestLevel);
  try
    Y := YDraw;
    Result := Y + SectionHeight;
    if Float then
      Y := Y + VSpace;
    YOffset := Document.YOff;
    YO := Y - YOffset;

    if (YO + DrawHeight >= ARect.Top) and (YO < ARect.Bottom) or Document.Printing then
      if Document.Printing and (Document.TableNestLevel = 1)
        and HeadOrFoot and (Y < Document.PageBottom)
        and ((Document.PrintingTable = nil) or (Document.PrintingTable = Self))
      then
        DrawTableP(Canvas, ARect, IMgr, X, Y)
      else
        DrawTable(Canvas, ARect, IMgr, X, Y);
  finally
    Dec(Document.TableNestLevel);
  end;
end;

procedure THtmlTable.DrawTable(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y: Integer);
var
  I, XX: Integer;
  YY, YOffset: Integer;
begin
  YOffset := Document.YOff;
  XX := X + Indent; {for the table}
  YY := Y;
  DrawX := XX;
  DrawY := YY;
  for I := 0 to Rows.Count - 1 do
    YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
      XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
      BorderColorDark, I);
end;

procedure THtmlTable.DrawTableP(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y: Integer);
{Printing table with thead and/or tfoot}
var
  I, XX, TopBorder, BottomBorder: Integer;
  YY, YOffset: Integer;
  SavePageBottom: Integer;
  Spacing, HeightNeeded: Integer;
begin
  YOffset := Document.YOff;
  XX := X + Indent; {for the table}
  YY := Y;
  DrawX := XX;
  DrawY := YY;

  if TTableBlock(OwnerBlock).TableBorder then
  begin
    TopBorder := BorderWidth;
    BottomBorder := BorderWidth;
  end
  else
  begin
    TopBorder := OwnerBlock.MargArray[BorderTopWidth];
    BottomBorder := OwnerBlock.MargArray[BorderBottomWidth];
  end;

  case TablePartRec.TablePart of
  {.$Region 'Normal'}
    Normal:
      begin
        Document.PrintingTable := Self;
        if Document.PageBottom - Y >= TableHeight + BottomBorder then
        begin
          for I := 0 to Rows.Count - 1 do {do whole table now}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          Document.PrintingTable := nil;
        end
        else
        begin {see if enough room on this page for header, 1 row, footer}
          if HeadOrFoot then
          begin
            Spacing := CellSpacing div 2;
            HeightNeeded := HeaderHeight + FootHeight +
              TCellList(Rows.Items[HeaderRowCount]).RowHeight;
            if (Y - YOffset > ARect.Top) and (Y + HeightNeeded > Document.PageBottom) and
              (HeightNeeded < ARect.Bottom - ARect.Top) then
            begin {not enough room, start table on next page}
              if Y + Spacing < Document.PageBottom then
              begin
                Document.PageShortened := True;
                Document.PageBottom := Y + Spacing;
              end;
              exit;
            end;
          end;
          {start table. it will not be complete and will go to next page}
          SavePageBottom := Document.PageBottom;
          Document.PageBottom := SavePageBottom - FootHeight - Cellspacing - BottomBorder - 5; {a little to spare}
          for I := 0 to Rows.Count - 1 do {do part of table}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
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
            TablePartRec.PartStart := DrawY - TopBorder;
            TablePartRec.PartHeight := HeaderHeight + TopBorder;
            Document.TheOwner.TablePartRec := TablePartRec;
          end;
          Document.TheOwner.TablePartRec := TablePartRec;
        end;
      end;
  {.$EndRegion}
  {.$Region 'DoBody1'}
    DoBody1:
      begin
        if Document.PageBottom > Y + TableHeight + BottomBorder then
        begin {can complete table now}
          for I := 0 to Rows.Count - 1 do {do remainder of table now}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          Document.TheOwner.TablePartRec.TablePart := Normal;
        end
        else
        begin {will do part of the table now}
      {Leave room for foot later}
          Document.PageBottom := Document.PageBottom
            - FootHeight + Max(Cellspacing, 1) - BottomBorder;
          for I := 0 to Rows.Count - 1 do
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
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
            TablePartRec.PartStart := DrawY - TopBorder;
            TablePartRec.PartHeight := HeaderHeight + TopBorder;
            Document.TheOwner.TablePartRec := TablePartRec;
          end;
          Document.TheOwner.TablePartRec := TablePartRec;
        end;
      end;
  {.$EndRegion}
  {.$Region 'DoBody2'}
    DoBody2:
      begin
        if Document.PageBottom > Y + TableHeight + BottomBorder then
        begin
          for I := 0 to Rows.Count - 1 do {do remainder of table now}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
              XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
              BorderColorDark, I);
          Document.TheOwner.TablePartRec.TablePart := Normal;
          Document.PrintingTable := nil;
        end
        else
        begin
          SavePageBottom := Document.PageBottom;
          for I := 0 to Rows.Count - 1 do {do part of table}
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
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
            TablePartRec.PartStart := DrawY - TopBorder;
            TablePartRec.PartHeight := HeaderHeight + TopBorder;
            Document.TheOwner.TablePartRec := TablePartRec;
          end;
          Document.TheOwner.TablePartRec := TablePartRec;
        end;
      end;
  {.$EndRegion}
  {.$Region 'DoFoot'}
    DoFoot:
      begin
        YY := TablePartRec.PartStart;
        if FootStartRow >= 0 then
          for I := FootStartRow to Rows.Count - 1 do
            YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
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
        Document.TheOwner.TablePartRec := TablePartRec;
      end;
  {.$EndRegion}
  {.$Region 'DoHead'}
    DoHead:
      begin
        for I := 0 to HeaderRowCount - 1 do
          YY := TCellList(Rows.Items[I]).Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacing, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        TablePartRec.TablePart := DoBody1;
        TablePartRec.PartStart := BodyBreak - 1;
        TablePartRec.FootHeight := FootHeight + Max(Cellspacing, 1) + BottomBorder;
        Document.TheOwner.TablePartRec := TablePartRec;
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

function THtmlTable.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;

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

function THtmlTable.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;

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

constructor TSection.Create(OwnerCell: TCellBasic; Attr: TAttributeList; Prop: TProperties; AnURL: TUrlTarget; FirstItem: boolean);
var
  FO: TFontObj;
  T: TAttribute;
  S: ThtString;
  Clr: ClearAttrType;
  Percent: boolean;
begin
  inherited Create(OwnerCell, Attr, Prop);
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
    if not Document.StopTab then
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

  if Assigned(Attr) then
  begin
    if Attr.Find(ClearSy, T) then
    begin
      S := LowerCase(T.Name);
      if (S = 'left') then
        ClearAttr := clLeft
      else if (S = 'right') then
        ClearAttr := clRight
      else
        ClearAttr := clAll;
    end;
    if Attr.TheID <> '' then
      Document.IDNameList.AddObject(Attr.TheID, Self);
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

  if Self is TPreFormated then
    WhiteSpaceStyle := wsPre
  else if NoBreak then
    WhiteSpaceStyle := wsNoWrap
  else
    WhiteSpaceStyle := wsNormal;
  if VarIsOrdinal(Prop.Props[piWhiteSpace]) then
    WhiteSpaceStyle := TWhiteSpaceStyle(Prop.Props[piWhiteSpace])
  else if VarIsStr(Prop.Props[piWhiteSpace]) then
  begin
    if Prop.Props[piWhiteSpace] = 'pre' then
      WhiteSpaceStyle := wsPre
    else if Prop.Props[piWhiteSpace] = 'nowrap' then
      WhiteSpaceStyle := wsNoWrap
    else if Prop.Props[piWhiteSpace] = 'pre-wrap' then
      WhiteSpaceStyle := wsPreWrap
    else if Prop.Props[piWhiteSpace] = 'pre-line' then
      WhiteSpaceStyle := wsPreLine
    else if Prop.Props[piWhiteSpace] = 'normal' then
      WhiteSpaceStyle := wsNormal;
  end;
end;

{----------------TSection.CreateCopy}

constructor TSection.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
var
  TT: TSection;
begin
  inherited;
  TT := T as TSection;
  Len := TT.Len;
  BuffSize := TT.BuffSize;
  BuffS := TT.BuffS;
  SetLength(BuffS, Length(BuffS));
  Buff := PWideChar(BuffS);
  Brk := TT.Brk;
  Fonts := TFontList.CreateCopy(Self, TT.Fonts);
  //TODO -oBG, 24.03.2011: TSection has no Cell, but owns images. Thus Parent must be a THtmlNode.
  //  and ThtDocument should become a TBodyBlock instead of a SectionList.
  Images := TFloatingObjList.CreateCopy(Document, OwnerCell {must be Self}, TT.Images);
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
  C: char;
  St, StU: UnicodeString;
  Small: boolean;
begin
  if T.Count = 0 then
    Exit;
  { Yunqa.de: Simple hack to support <span style="display:none"> }
  if PropStack.Last.Display = pdNone then
    Exit;

  L := Len + T.Count;
  if BuffSize < L + 3 then
    Allocate(L + 500); {L+3 to permit additions later}
  case PropStack.Last.GetTextTransform of
    txUpper:
      St := htUpperCase(T.S);
    txLower:
      St := htLowerCase(T.S);
  else
    St := T.S;
  end;
  Move(T.I[1], XP^[Len], T.Count * Sizeof(Integer));
  // BG, 31.08.2011: added: WhiteSpaceStyle
  if NoBreak or (WhiteSpaceStyle in [wsPre, wsPreLine, wsNoWrap]) then
    C := 'n'
  else
    C := 'y';
  for I := 1 to T.Count do
    Brk := Brk + C;

  if PropStack.Last.GetFontVariant = 'small-caps' then
  begin
    StU := htUpperCase(St);
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
    FormControls.Decrement(I - 1);
    Fonts.Decrement(I - 1, Document);
    Images.Decrement(I - 1);
  end;

begin
  if WhiteSpaceStyle in [wsPre] then
  begin
    FO := TFontObj(Fonts.Items[Fonts.Count - 1]); {keep font the same for inserted space}
    if FO.Pos = Length(BuffS) then
      Inc(FO.Pos);
    BuffS := BuffS + ' ';
    XP^[Length(BuffS) - 1] := XP^[Length(BuffS) - 2] + 1;
  end
  else
  begin
    I := WidePos(Shy, BuffS);
    while I > 0 do
    begin
      Remove(I);
      if (I > 1) and (Brk[I - 1] <> 'n') then
        Brk[I - 1] := 's';
      I := WidePos(Shy, BuffS);
    end;

    if WhiteSpaceStyle in [wsNormal, wsNoWrap, wsPreLine] then
    begin
      while (Length(BuffS) > 0) and (BuffS[1] = ' ') do
        Remove(1);

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

      I := WidePos(UnicodeString(' ' + #8), BuffS); {#8 is break char}
      while I > 0 do
      begin
        Remove(I);
        I := WidePos(UnicodeString(' ' + #8), BuffS);
      end;

      I := WidePos(UnicodeString(#8 + ' '), BuffS);
      while I > 0 do
      begin
        Remove(I + 1);
        I := WidePos(UnicodeString(#8 + ' '), BuffS);
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
    end;
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
  NewFont: ThtFont;
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
  NewFont: ThtFont;
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
  Result := TImageObj.Create(Document, ACell, Len, L);
  //Result.MyCell := ACell;
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for image}
end;

function TSection.AddPanel(L: TAttributeList; ACell: TCellBasic; Index: Integer): TPanelObj;
begin
  Result := TPanelObj.Create(Document, ACell, Len, L, False);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for panel}
end;

function TSection.CreatePanel(L: TAttributeList; ACell: TCellBasic): TPanelObj;
{Used by object tag}
begin
  Result := TPanelObj.Create(Document, ACell, Len, L, True);
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
    Result := TEditFormControlObj.Create(AMasterList, ACell, Len, S, L, Prop);
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
          FCO := TButtonFormControlObj.Create(AMasterList, ACell, Len, S, L, Prop)
        else if S = 'radio' then
          FCO := TRadioButtonFormControlObj.Create(AMasterList, ACell, Len, L, Prop)
        else if S = 'checkbox' then
          FCO := TCheckBoxFormControlObj.Create(AMasterList, ACell, Len, L, Prop)
        else if S = 'hidden' then
          FCO := THiddenFormControlObj.Create(AMasterList, ACell, Len, L, Prop)
        else if S = 'image' then
          FCO := TImageFormControlObj.Create(AMasterList, ACell, Len, L, Prop)
        else
          FCO := CreateEditFCO;
      end
      else
        FCO := CreateEditFCO;

    end;

    SelectSy:
    begin
      if L.Find(MultipleSy, T) or L.Find(SizeSy, T) and (T.Value > 1) then
        FCO := TListBoxFormControlObj.Create(AMasterList, ACell, Len, L, Prop)
      else
        FCO := TComboFormControlObj.Create(AMasterList, ACell, Len, L, Prop);
    end;
  else
    FCO := TTextAreaFormControlObj.Create(AMasterList, ACell, Len, L, Prop);
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
    ButtonControl := TButtonFormControlObj.Create(AMasterList, ACell, Len, S, L, Prop);
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

//BG, 20.09.2010:
function CanWrap(C: WideChar): Boolean;
begin
  if C in [WideChar(' '), WideChar('-'), WideChar('?'), ImgPan, FmCtl, BrkCh] then
    Result := True
  else
    Result := WrapChar(C);
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
  if Len = 0 then
  begin
    Min := 0;
    Max := 0;
    Exit;
  end;

  if not BreakWord and (WhiteSpaceStyle in [wsPre, wsNoWrap]) then
  begin
    if StoredMax = 0 then
    begin
      Max := FindTextWidth(Canvas, Buff, Len - 1, False);
      StoredMax := Max;
    end
    else
      Max := StoredMax;
    Min := Math.Min(MaxHScroll, Max);
    Exit;
  end;

  if (StoredMin > 0) and (Images.Count = 0) then
  begin
    Min := StoredMin;
    Max := StoredMax;
    Exit;
  end;

  Min := 0;
  Max := 0;
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
    J1 := FormControls.GetControlCountAt(Start - Buff);
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
end;

{----------------TSection.FindTextWidthA}

function TSection.FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
{find actual line width of N chars starting at Start.
 BG: The only difference to FindTextWidth is the '- OHang' when incrementing the result.}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: Integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
  Font: ThtFont;
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
      begin
        Result := Result + Wid + HSpcL + HSpcR;
      end;
      Dec(N); {image counts as one ThtChar}
      Inc(Start);
    end
    else if J1 = 0 then
    begin
      Result := Result + FormControls.GetWidthAt(Start - Buff, HSpcL, HSpcR);
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

  function FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; MaxChars, X, Y: Integer;
    IMgr: TIndentManager; var ImgY, ImgHt: Integer; var DoneFlObjPos: PWideChar): Integer;
  {Given a width, find the count of chars (<= Max) which will fit allowing for font changes.
    Line wrapping will be done later}
  //BG, 06.02.2011: Why are there 2 methods and why can't GetURL and FindCursor use the formatting results of DrawLogic?
  //  FindCountThatFits1() is part of TSection.DrawLogic() and fills IMgr with the embedded floating images.
  //  TSection.FindCountThatFits() is used in TSection.GetURL() and TSection.FindCursor().
  var
    Cnt, XX, YY, I, J, J1, J2, J3, X1, X2, W, H, ImgX, OHang, Width: Integer;
    Picture: boolean;
    FlObj: TFloatingObj;
    FcObj: TFormControlObj;
    BrChr, TheStart: PWideChar;
    Font, LastFont: ThtFont;
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

  procedure DoDrawLogic;
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
          DHt := Max(DHt, Tmp);
          LR.Descent := Max(LR.Descent, Desc);
          J := Fonts.GetFontCountAt(PStart - Buff + Cnt, Len);
          Inc(Cnt, J);
        until Cnt >= NN;

      SB := 0; {if there are images, then maybe they add extra space}
      SA := 0; {space before and after}
      if LineHeight > 0 then
      begin
        // BG, 28.08.2011: too much space below an image: SA and SB depend on Align:
        case Align of
          aTop:
            SA := LineHeight;

          aMiddle:
            begin
              SB := (LineHeight - DHt) div 2;
              SA := (LineHeight - DHt) - SB;
            end;

          aBaseline,
          aBottom:
            SB := LineHeight;
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
  begin
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
      // BG, 28.08.2011:
      if OwnerBlock.HideOverflow then
      begin
        if Obj.ImageWidth > Width then
          Obj.ImageWidth := Width;
      end
      else
        MaxWidth := Max(MaxWidth, Obj.ImageWidth); {HScrollBar for wide images}
    end;
    for I := 0 to FormControls.Count - 1 do
    begin
      Ctrl := FormControls[I];
      if Ctrl.PercentWidth then
        Ctrl.Width := Max(10, Min(MulDiv(Ctrl.FWidth, Width, 100), Width - Ctrl.HSpaceL - Ctrl.HSpaceR));
      // BG, 28.08.2011:
      if OwnerBlock.HideOverflow then
      begin
        if Ctrl.Width > Width then
          Ctrl.Width := Width;
      end
      else
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
      if (WhiteSpaceStyle in [wsPre, wsPreLine, wsNoWrap]) and not BreakWord then
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
        if ((P^ in [WideChar(' '), {FmCtl,} ImgPan]) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n') or (P^ = BrkCh) then
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
        begin {move down to where it's wider}
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
        if ((P^ in [WideChar(' '), FmCtl, ImgPan]) or WrapChar(P^)) and (Brk[P - Buff + 1] <> 'n')
        or (P^ = BrkCh) then
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
        else if (N < MaxChars) and ((P + 1)^ in [FmCtl, ImgPan]) and (Brk[PStart - Buff + N] <> 'n') then {an image or control}
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

    // BG, 28.08.2011:
    if OwnerBlock.HideOverflow then
      if MaxWidth > Width then
        MaxWidth := Width;
  end;

var
  Dummy: Integer;
  Save: Integer;
begin {TSection.DrawLogic}
  if WhiteSpaceStyle in [wsPre, wsNoWrap] then
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
      exit;
    end;

    if not BreakWord then
    begin
    {call with large width to prevent wrapping}
      Save := IMgr.Width;
      IMgr.Width := 32000;
      DoDrawLogic;
      IMgr.Width := Save;
      MinMaxWidth(Canvas, Dummy, MaxWidth); {return MaxWidth}
      exit;
    end;
  end;
  DoDrawLogic;
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
    St: UnicodeString;

    function AddHyphen(P: PWideChar; N: Integer): UnicodeString;
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
              {IMgr.LfEdge +} Obj.Indent, Obj.DrawYY, Y - Descent, FO);

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
            if OwnerBlock <> nil then
              TImageObj(Obj).Positioning := OwnerBlock.Positioning
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
                  if OwnerCell.BkGnd then
                    Color := OwnerCell.BkColor
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
          if (Cnt - I <= 0) and ((Start + I - 1)^ in [WideChar(' '), WideChar(BrkCh)]) then
            Tmp := I - 1 {at end of line, don't show space or break}
          else
            Tmp := I;
          if (WhiteSpaceStyle in [wsPre, wsPreLine, wsNoWrap]) and not OwnerBlock.HideOverflow then
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
  if (Len > 0) and (Y - YOffset + DrawHeight + 40 >= ARect.Top)
    and (Y - YOffset - 40 < ARect.Bottom) then
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
              if (OwnerBlock <> nil) and (OwnerBlock.Positioning = PosAbsolute) then
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
    Result.Assign(UrlTarget);
  end;

begin
  Result := [];
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
        if not ((Start + L)^ in [ImgPan]) then {an image here would be in HSpace area}
        begin
          Include(Result, guUrl);
          UrlTarg := MakeCopy(FO.UrlTarget);
          Document.ActiveLink := FO;
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

function TSection.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
{find the first occurance of the ThtString, ToFind, with a cursor value >= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  I: Integer;
  ToSearch: UnicodeString;

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
    ToSearch := htLowerCase(BuffS); {ToFind already lower case}

  P := StrPosW(PWideChar(ToSearch) + I, PWideChar(ToFind));
  if Assigned(P) then
    Result := StartCurs + (P - PWideChar(ToSearch));
end;

{----------------TSection.FindStringR}

function TSection.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
{find the first occurance of the ThtString, ToFind, with a cursor value <= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  ToFindLen: word;
  ToMatch, ToSearch: UnicodeString;

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
    ToSearch := htLowerCase(ToSearch); {ToFind already lower case}

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

constructor TPanelObj.Create(MasterList: ThtDocument; Parent: TCellBasic; Position: Integer;
  L: TAttributeList; ObjectTag: boolean);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  NewSpace: Integer;
  S, Source, AName, AType: ThtString;
begin
  inherited Create(Parent, L, nil);
  Pos := Position;
  VertAlign := ABottom; {default}
  Floating := ANone;
  PntPanel := {TPaintPanel(}MasterList.PPanel{)};
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
    with L[I] do
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
    if not ObjectTag and Assigned(MasterList.PanelCreateEvent) then
      MasterList.PanelCreateEvent(MasterList.TheOwner, AName, AType, Source, Panel);
    SetWidth := Width;
    SetHeight := Height;
  end;
  MasterList.PanelList.Add(Self);
end;

constructor TPanelObj.CreateCopy(MasterList: ThtDocument; Parent: TCellBasic; T: TPanelObj);
begin
  inherited CreateCopy(MasterList, Parent, T); //TODO -oBG, 24.03.2011: add parent
  Panel := ThvPanel.Create(nil);
  with T.Panel do
    Panel.SetBounds(Left, Top, Width, Height);
  Panel.FVisible := T.Panel.FVisible;
  Panel.Color := T.Panel.Color;
  Panel.Parent := MasterList.PPanel;
  SpecWidth := T.SpecWidth;
  PercentWidth := T.PercentWidth;
  SpecHeight := T.SpecHeight;
  PercentHeight := T.PercentHeight;
  SetHeight := T.SetHeight;
  SetWidth := T.SetWidth;
  OPanel := T.Panel; {save these for printing}
  OSender := T.Document.TheOwner;
  PanelPrintEvent := T.Document.PanelPrintEvent;
  IsCopy := True;
end;

destructor TPanelObj.Destroy;
begin
  if Assigned(Document) and Assigned(Document.PanelDestroyEvent) then
    Document.PanelDestroyEvent(Document.TheOwner, Panel);
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

constructor TCell.Create(Master: ThtDocument; OwnerBlock: TBlock);
begin
  inherited;
  IMgr := TIndentManager.Create;
end;

{----------------TCell.CreateCopy}

constructor TCell.CreateCopy(AMasterList: ThtDocument; OwnerBlock: TBlock; T: TCellBasic);
begin
  inherited;
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

constructor TCellObjCell.CreateCopy(Document: ThtDocument; Parent: TBlock; T: TCellObjCell);
begin
  inherited CreateCopy(Document, Parent, T);
  MyRect := T.MyRect;
end;

{----------------TCellObjCell.GetUrl}

function TCellObjCell.GetURL(Canvas: TCanvas; X, Y: Integer; var UrlTarg: TUrlTarget;
  var FormControl: TIDObject{TImageFormControlObj}; var ATitle: ThtString): guResultType;
{Y is absolute}
begin
  Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  if PtInRect(MyRect, Point(X, Y - Document.YOFF)) then
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
    if OwnerBlock.HideOverflow then
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
  if Document.Printing then
  begin
    Y := YDraw;
    YOffset := Document.YOff;
    if (Y - YOffset > ARect.Top + 5) and (Y - YOffset < ARect.Bottom) and (Y + ARect.Top - 1 < Document.PageBottom) then
      Document.PageBottom := Y + ARect.Top - 1;
  end;
end;

{----------------THorzLine.Create}

constructor THorzLine.Create(OwnerCell: TCellBasic; L: TAttributeList; Prop: TProperties);
var
  LwName: ThtString;
  I: Integer;
  TmpColor: TColor;
begin
  inherited Create(OwnerCell, L, Prop);
  VSize := 2;
  Align := Centered;
  Color := clNone;
  for I := 0 to L.Count - 1 do
    with L[I] do
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
        ColorSy: if TryStrToColor(Name, False, Color) then
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

constructor THorzLine.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
begin
  inherited;
  System.Move((T as THorzline).VSize, VSize, PtrSub(@BkGnd, @VSize) + Sizeof(BkGnd));
end;

procedure THorzLine.CopyToClipboard;
begin
  Document.CB.AddTextCR('', 0);
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
  YO := Y - Document.YOff;
  if (YO + SectionHeight >= ARect.Top) and (YO < ARect.Bottom) and
    (not Document.Printing or (Y < Document.PageBottom)) then
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
      Document.FirstPageItem := False; {items after this will not be first on page}
    end;
end;

//procedure TPreformated.ProcessText(TagIndex: Integer);
//var
//  FO: TFontObj;
//begin
//  FO := TFontObj(Fonts.Items[Fonts.Count - 1]); {keep font the same for inserted space}
//  if FO.Pos = Length(BuffS) then
//    Inc(FO.Pos);
//  BuffS := BuffS + ' ';
//  XP^[Length(BuffS) - 1] := XP^[Length(BuffS) - 2] + 1;
//  Finish;
//end;
//
//procedure TPreformated.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer);
//begin
//  if BreakWord then
//  begin
//    inherited;
//    Exit;
//  end;
//  if Len = 0 then
//  begin
//    Max := 0;
//    Min := 0;
//  end
//  else
//  begin
//    if StoredMax = 0 then
//    begin
//      Max := FindTextWidth(Canvas, Buff, Len - 1, False);
//      StoredMax := Max;
//    end
//    else
//      Max := StoredMax;
//    Min := Math.Min(MaxHScroll, Max);
//  end;
//end;
//
//function TPreFormated.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
//  var MaxWidth: Integer; var Curs: Integer): Integer;
//var
//  Dummy: Integer;
//  Save: Integer;
//begin
//  if Len = 0 then
//  begin
//    ContentTop := Y;
//    Result := Fonts.GetFontObjAt(0, Dummy).FontHeight;
//    SectionHeight := Result;
//    MaxWidth := 0;
//    YDraw := Y;
//    DrawHeight := Result;
//    ContentBot := Y + Result;
//    DrawBot := ContentBot;
//  end
//  else if not BreakWord then
//  begin
//  {call with large width to prevent wrapping}
//    Save := IMgr.Width;
//    IMgr.Width := 32000;
//    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, Dummy, Curs);
//    IMgr.Width := Save;
//    MinMaxWidth(Canvas, Dummy, MaxWidth); {return MaxWidth}
//  end
//  else
//  begin
//    Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
//  end;
//end;

{ THtmlPropStack }

{ Add a TProperties to the PropStack. }
procedure THtmlPropStack.PushNewProp(const Tag, AClass, AnID, APseudo, ATitle: ThtString; AProps: TProperties);
var
  NewProp: TProperties;
begin
  NewProp := TProperties.Create(self);
  NewProp.Inherit(Tag, Last);
  Add(NewProp);
  NewProp.Combine(Document.Styles, Tag, AClass, AnID, APseudo, ATitle, AProps, Count - 1);
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
        Document.ProcessInlines(SIndex, Items[I], False);
      Delete(I);
      if I > 1 then {update any stack items which follow the deleted one}
        for J := I to Count - 1 do
          Items[J].Update(Items[J - 1], Document.Styles, J);
      Break;
    end;
end;

//-- BG ---------------------------------------------------------- 12.09.2010 --
constructor THtmlStyleList.Create(AMasterList: ThtDocument);
begin
  inherited Create;
  Document := AMasterList;
end;

//-- BG ---------------------------------------------------------- 08.03.2011 --
procedure THtmlStyleList.setLinksActive(Value: Boolean);
begin
  inherited;
  Document.LinksActive := Value;
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
constructor TFieldsetBlock.Create(OwnerCell: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
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
  FLegend := TBlockCell.Create(Document, Self);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
constructor TFieldsetBlock.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
begin
  inherited;
  FLegend := TBlockCell.CreateCopy(Document, Self, (T as TFieldsetBlock).FLegend);
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

function TFormControlObjList.GetHeightAt(Posn: integer; var FormAlign: AlignmentType): Integer;
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

function TFormControlObjList.GetWidthAt(Posn: integer; var HSpcL, HSpcR: integer): integer;
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

constructor TFloatingObj.CreateCopy(Document: ThtDocument; Parent: TCellBasic; T: TFloatingObj);
begin
  inherited CreateCopy(Parent, T);
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

{ TSectionBase }

//-- BG ---------------------------------------------------------- 20.09.2009 --
constructor TSectionBase.Create(OwnerCell: TCellBasic; Attributes: TAttributeList; AProp: TProperties);
begin
  inherited;
  FDisplay := AProp.Display;
  ContentTop := 999999999; {large number in case it has Display: none; }
end;

constructor TSectionBase.CreateCopy(OwnerCell: TCellBasic; T: TSectionBase);
begin
  inherited CreateCopy(OwnerCell, T);;
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

function TSectionBase.DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
// Computes all coordinates of the section.
//
// Normal sections, absolutely positioned blocks and floating blocks start at given (X,Y) relative to document origin.
// Table cells start at given (X,Y) coordinates relative to the outmost containing block.
//
// Returns the nominal height of the section (without overhanging floating blocks)
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

function TSectionBase.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
// returns the pixel row, where the section ends.
begin
  Result := YDraw + SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X, Y: Integer;
  var UrlTarg: TUrlTarget; var FormControl: TIDObject{TImageFormControlObj};
  var ATitle: ThtString): guResultType;
begin
  Result := [];
end;

function TSectionBase.PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): boolean;
begin
  Result := False;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X, Y: Integer; var XR, YR, CaretHt: Integer; var Intext: boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: boolean): Integer;
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

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): boolean;
begin
  Result := False;
end;

function TSectionBase.GetChAtPos(Pos: Integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
  Result := False;
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

{ TChPosObj }

//-- BG ---------------------------------------------------------- 04.03.2011 --
constructor TChPosObj.Create(Document: ThtDocument; Pos: Integer);
begin
  inherited Create;
  FChPos := Pos;
  FDocument := Document;
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
  Pos := Document.FindDocPos(ChPos, False);
  if Document.CursorToXY(nil, Pos, X, Y) then
    Result := Y
  else
    Result := 0;
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
  WaitStream := TMemoryStream.Create;
finalization
  WaitStream.Free;
end.
