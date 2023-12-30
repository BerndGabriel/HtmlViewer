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

//TODO -oBG, 15.03.2014: support display:inline
{-$define DO_PD_INLINE}
//TODO -oBG, 15.03.2014: support display:inline
{$ifdef DO_PD_INLINE}
{$endif}

interface

uses
{$ifdef UseInline}
  HtmlCaches,
{$endif}
{$ifdef VCL}
  Windows,
  {$ifdef Compiler20_Plus}
    CommCtrl,
  {$endif}
  {$ifndef Compiler28_Plus}
    EncdDecd,
  {$endif}
{$endif}
  Messages, Graphics, Controls, ExtCtrls, Classes, SysUtils, Variants, Forms, Math, Contnrs, ComCtrls,
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
{$ifdef LCL}
  LclIntf, LclType, Types, HtmlMisc,
{$endif}
  HtmlGlobals,
  HtmlFonts,
  StyleTypes,
  HtmlImages, // use before HTMLUn2, as both define a TGetImageEvent, but we need the one of HTMLUn2 here.
  HTMLUn2,
  HtmlBuffer,
  HtmlSymb,
  StyleUn,
  UrlSubs,
  HTMLGif2;

type
  TBlock = class;
  TSection = class;
  TCellBasic = class;
  ThtDocument = class;

//------------------------------------------------------------------------------
// TFontObj, contains an atomic font info for a part of a section.
// TFontList, contains and owns all font infos of a section
// TLinkList, references but does not own all link infos of a document. The objects are still owned by their sections.
//------------------------------------------------------------------------------

  ThtTabControl = class(TWinControl)
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

  TFontList = class;

  TFontObj = class(TFontObjBase) {font information}
  // BG, 10.08.2013: deprecated
  // Is used to handle the link states, but the full range of CSS
  // properties can be applied to :link, :hover, :visited, etc.
  //
  private
    FSection: TSection; 
    FVisited, FHover: Boolean;
    Title: ThtString;
    FYValue: Integer;
    Active: Boolean;
    procedure SetVisited(Value: Boolean);
    procedure SetHover(Value: Boolean);
    function GetURL: ThtString;
    procedure SetAllHovers(List: TFontList; Value: Boolean);
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
    TheFont: ThtFont;
    FIArray: TFontInfoArray;
    FontHeight, {tmHeight+tmExternalLeading}
      tmHeight,
      tmMaxCharWidth,
      Overhang,
      Descent: Integer;
    SScript: ThtAlignmentStyle;
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
    property Visited: Boolean read FVisited write SetVisited;
    property Hover: Boolean read FHover write SetHover;
    property DrawYY: Integer read FYValue;
  end;

  // BG, 10.02.2013: owns its objects.
  TFontList = class(TFontObjBaseList) {a list of TFontObj's}
  private
    function GetFont(Index: Integer): TFontObj; {$ifdef UseInline} inline; {$endif}
  public
    constructor CreateCopy(ASection: TSection; T: TFontList);
    function GetFontAt(Posn: Integer; out OHang: Integer): ThtFont;
//    function GetFontCountAt(Posn, Leng: Integer): Integer;
    function GetFontObjAt(Posn: Integer): TFontObj; overload;
    function GetFontObjAt(Posn, Leng: Integer; out Obj: TFontObj): Integer; overload;
    procedure Decrement(N: Integer; Document: ThtDocument);
    property Items[Index: Integer]: TFontObj read GetFont; default;
  end;

  // BG, 10.02.2013: does not own its font objects.
  TLinkList = class(TFontList)
  public
    constructor Create;
  end;

  // BG, 06.02.2022: moved from implementation section
  ThtInlineRec = class
  private
    StartB, EndB, IDB, StartBDoc, EndBDoc: Integer;
    MargArray: ThtMarginArray;
  end;

{$ifdef UseGenerics}
  TInlineList = class(TObjectList<ThtInlineRec>)
{$else}
  TInlineList = class(TObjectList) {a list of ThtInlineRec's}
{$endif}
  private
    NeedsConverting: Boolean;
    Owner: ThtDocument;
    procedure AdjustValues;
{$ifdef UseGenerics}
{$else}
    function Get(Index: Integer): ThtInlineRec; {$ifdef UseInline} inline; {$endif}
{$endif}
    function GetStartB(I: Integer): Integer;
    function GetEndB(I: Integer): Integer;
  public
    constructor Create(AnOwner: ThtDocument);
{$ifdef UseGenerics}
    procedure Clear; virtual;
{$else}
    procedure Clear; override;
    property Items[Index: Integer]: ThtInlineRec read Get; default;
{$endif}
    property StartB[I: Integer]: Integer read GetStartB;
    property EndB[I: Integer]: Integer read GetEndB;
  end;

//------------------------------------------------------------------------------
// THtmlNode is base class for all objects in the HTML document tree.
//------------------------------------------------------------------------------

  THtmlNode = class(TIDObject)
  private
    FDocument: ThtDocument; // the document it belongs to
    FOwnerBlock: TBlock;    // the parental block it is placed in
    FOwnerCell: TCellBasic; // the parent's child list it is placed in
    //FIds: ThtStringArray;
    //FClasses: ThtStringArray;
    FAttributes: TAttributeList;
    FProperties: TProperties;
    FDisplay: ThtDisplayStyle; // how it is displayed
    FEmSize, FExSize: Integer;

    function GetSymbol(): TElemSymb;
    function GetAttribute(const Name: ThtString): ThtString;
    function GetContainingBlock: TBlock; virtual;
    function GetProperty(PropIndex: ThtPropIndices): Variant;
    function GetContainingBox: TRect;

    function GetClearSpace(IMgr: TIndentManager; Y: Integer): Integer;

  protected
    FPositioning: ThtBoxPositionStyle;
    FPositions: ThtRectIntegers; // in Pixels
    FFloating: ThtAlignmentStyle;
    FClearing: ThtClearStyle;
    FIndent: Integer;           {Indentation of floating object relative to Draw-X}

    procedure GetVMarginArray(var MArray: ThtVMarginArray);

    function TryGetClear(var Clear: ThtClearStyle): Boolean; virtual;
    function CalcDisplayExtern: ThtDisplayStyle; virtual;
    function CalcDisplayIntern: ThtDisplayStyle; virtual;
    function FindAttribute(const Name: ThtString; out Attribute: TAttribute): Boolean; overload;
//    function FindAttribute(NameSy: TAttrSymb; out Attribute: TAttribute): Boolean; overload;
//    function GetChild(Index: Integer): THtmlNode; virtual;
//    function GetParent: TBlock; virtual;
//    function GetPseudos: TPseudos; virtual;
    function IsCopy: Boolean; virtual;
    property ContainingBlock: TBlock read GetContainingBlock;
    property ContainingBox: TRect read GetContainingBox;
  public
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties; const ID: ThtString);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); virtual;
    destructor Destroy; override;

    //constructor Create(Parent: THtmlNode; Tag: TElemSymb; Attributes: TAttributeList; const Properties: TResultingProperties);
    function IsInFlow: Boolean; {$ifdef UseInline} inline; {$endif}
    function IndexOf(Child: THtmlNode): Integer; virtual;
    procedure AfterConstruction; override;
    //function IsMatching(Selector: TSelector): Boolean;
    property Symbol: TElemSymb read GetSymbol;
//    property Parent: TBlock read GetParent;
//    property Children[Index: Integer]: THtmlNode read GetChild; default;
    property OwnerBlock: TBlock read FOwnerBlock; //BG, 07.02.2011: public for reading document structure (see issue 24). Renamed from MyBlock to Owner to clarify the relation.
    property OwnerCell: TCellBasic read FOwnerCell write FOwnerCell;
    property Document: ThtDocument read FDocument;
    property Display: ThtDisplayStyle read FDisplay write FDisplay;
    property EmSize: Integer read FEmSize write FEmSize;
    property ExSize: Integer read FExSize write FExSize;
    property AttributeValue[const AttrName: ThtString]: ThtString read GetAttribute;
    property PropertyValue[PropIndex: ThtPropIndices]: Variant read GetProperty;
  end;

//------------------------------------------------------------------------------
// TSectionBase, the abstract base class for all document sections
//------------------------------------------------------------------------------
// Each block is a section (see TBlock and its derivates) and a series of text
// and non block building "inline" tags is held in a section (see TSection)
//
// Base class for TSection, TBlock, THtmlTable, TPage and THorzLine
//------------------------------------------------------------------------------

  TSectionBase = class(THtmlNode)
  protected
    function GetYPosition: Integer; override;
    procedure SetDocument(List: ThtDocument);
  public
    // source buffer reference
    StartCurs: Integer;     // where the section starts in the source buffer.
    Len: Integer;           // number of bytes in source buffer the section represents.
    // Z coordinates are calculated in Create()
    ZIndex: Integer;
    // Y coordinates calculated in DrawLogic1() are still valid in Draw1()
    YDraw: Integer;         // where the section starts.
    DrawTop: Integer;       // where the border starts.  In case of a block this is YDraw + MarginTop
    ContentTop: Integer;    // where the content starts. In case of a block this is YDraw + MarginTop + BorderTopWidth + PaddingTop
    ContentBot: Integer;    // where the section ends.   In case of a block this is Block.ClientContentBot + PaddingBottom + BorderBottomWidth + MarginBottom
    DrawBot: Integer;       // where the border ends.    In case of a block this is Max(Block.ClientContentBot, MyCell.tcDrawBot) + PaddingBottom + BorderBottomWidth
    SectionHeight: Integer; // pixel height of section. = ContentBot - YDraw
    DrawHeight: Integer;    // floating image may overhang. = Max(ContentBot, DrawBot) - YDraw
    // X coordinates calculated in DrawLogic1() may be shifted in Draw1(), if section is centered or right aligned
    DrawRect: TRect;        // where the section border starts and the content ends relative to document origin (upper left corner)
                            // DrawRect is calculated in DrawLogic1 or Draw1 and used in PtInDrawRect()
    TagClass: ThtString; {debugging aid}

    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties; const TheId, TheClass: ThtString); overload;
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties); overload;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean; virtual;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; virtual;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer; virtual;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; virtual;
    function FindSourcePos(DocPos: Integer): Integer; virtual;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; virtual;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; virtual;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean; virtual;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; virtual;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean; virtual;
    function PtInDrawRect(X, Y: Integer; var IX, IY: Integer): Boolean; virtual;
    procedure AddSectionsToList; virtual;
    procedure CopySelectedText; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); virtual;
  end;

{$ifdef UseGenerics}
  TSectionBaseList = class(TObjectList<TSectionBase>)
{$else}
  TSectionBaseList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSectionBase; {$ifdef UseInline} inline; {$endif}
{$endif}
  public
    function PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): Boolean;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean; virtual;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; virtual;
{$ifdef UseGenerics}
{$else}
    property Items[Index: Integer]: TSectionBase read GetItem; default;
{$endif}
  end;

//------------------------------------------------------------------------------
// TFloatingObj, an inline block for floating blocks.
//------------------------------------------------------------------------------

  TBlockBase = class(TSectionBase)
  public

    constructor Create(Parent: TCellBasic; Position: Integer; Attributes: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;

    property Positioning: ThtBoxPositionStyle read FPositioning;
    property Positions: ThtRectIntegers read FPositions; // in Pixels
    property Floating: ThtAlignmentStyle read FFloating;
    property Indent: Integer read FIndent;           {Indentation of floated object}
  end;

  TFloatingObj = class(TBlockBase)
  protected
    // begin copy by move()
    // source buffer reference
    VertAlign: ThtAlignmentStyle;
    HSpaceL, HSpaceR: Integer; {horizontal extra space}
    VSpaceT, VSpaceB: Integer; {vertical extra space}
    PercentWidth: Boolean; {if width is percent}
    PercentHeight: Boolean; {if height is percent}
    // end copy by move()

    function Clone(Parent: TCellBasic): TFloatingObj;
    function GetClientHeight: Integer; virtual; abstract;
    function GetClientWidth: Integer; virtual; abstract;
    procedure SetClientHeight(Value: Integer); virtual; abstract;
    procedure SetClientWidth(Value: Integer); virtual; abstract;
  public
    DrawYY: Integer; // where the object starts.
    DrawXX: Integer; // where the object starts.
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function GetYPosition: Integer; override;
    procedure DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); virtual; abstract;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); virtual; abstract;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    function TotalHeight: Integer; {$ifdef UseInline} inline; {$endif}
    function TotalWidth: Integer; {$ifdef UseInline} inline; {$endif}
  end;
  TFloatingObjClass = class of TFloatingObj;

  TImageObj = class;

{$ifdef UseGenerics}
  TFloatingObjList = class(TObjectList<TFloatingObj>)   {a list of TFloatingObj's}
{$else}
  TFloatingObjList = class(TObjectList)   {a list of TFloatingObj's}
  private
    function GetItem(Index: Integer): TFloatingObj; {$ifdef UseInline} inline; {$endif}
    procedure SetItem(Index: Integer; const Item: TFloatingObj); {$ifdef UseInline} inline; {$endif}
{$endif}
  public
    constructor CreateCopy(Parent: TCellBasic; T: TFloatingObjList);
    procedure Decrement(N: Integer); {$ifdef UseInline} inline; {$endif}
    function FindObject(Posn: Integer): TFloatingObj; {$ifdef UseInline} inline; {$endif}
    // GetObjectAt() returns number of positions from Posn to next object. If it returns 0, Obj is at Posn.
    function GetObjectAt(Posn: Integer; out Obj): Integer;
    function PtInImage(X, Y: Integer; out IX, IY, Posn: Integer; out AMap, UMap: Boolean; out MapItem: TMapItem; out ImageObj: TImageObj): Boolean;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;
{$ifdef UseGenerics}
{$else}
    property Items[Index: Integer]: TFloatingObj read GetItem write SetItem; default;
{$endif}
  end;

//------------------------------------------------------------------------------
// TCellBasic, the base class for content
//------------------------------------------------------------------------------
// Base class for table cells, block content and the entire document
//------------------------------------------------------------------------------

//BG, 02.12.2013: handling display:inline is too complex in this class structure, where TSection is both home of a couple
// of HTML elements, the renderer and the rendering results (in the ThtLineRecs).
//
// A straight forward rendering would require overall Lines collecting the text of all consecutive inline level elements
// no matter where it is defined in the document tree.
//
//  //BG, 01.12.2013: handle display:inline
//  //This list mixes up block elements and autogenerated anonymous blocks for consecutive inline level elements based on
//  // TInlineSection. All objects based on TInlineSection are owned (and therefore freed) by this list all other kind of
//  // objects are assumed to be referenced and thus are not freed.
//  TRenderSectionBaseList = class(TSectionBaseList)
//  protected
//    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
//  end;

  TCellBasic = class(TSectionBaseList) {a list of sections and blocks}
  private
    FDocument: ThtDocument; // the document it belongs to
    FOwnerBlock: TBlock;    // the parental block it is placed in
//    FRenderList: TRenderSectionBaseList; //BG, 01.12.2013: The list of block level elements to render. Filled in DoLogic()
  protected
    function CalcDisplayExtern: ThtDisplayStyle; // returns either pdInline or pdBlock
  public
    // source buffer reference
    StartCurs: Integer;     // where the section starts in the source buffer.
    Len: Integer;           // number of bytes in source buffer the section represents.
    //
    IMgr: TIndentManager;   // Each element displayed as a block needs an indent manager.
    BkGnd: Boolean;
    BkColor: TColor;
    // Y coordinates calculated in DrawLogic() are still valid in Draw1()
    //YValue: Integer;        // vertical position at top of cell. As this is a simple container, YValue is same as Self[0].YDraw.
    tcDrawTop: Integer;
    tcContentBot: Integer;
    tcDrawBot: Integer;

    constructor Create(Parent: TBlock);
    constructor CreateCopy(Parent: TBlock; T: TCellBasic);
    destructor Destroy; override;

    function CheckLastBottomMargin: Boolean;
    function DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X, Y, XRef, YRef: Integer): Integer; virtual;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: Integer; out XR, YR, Ht: Integer; out Intext: Boolean): Integer;
    function FindSourcePos(DocPos: Integer): Integer;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; virtual;
    function IsCopy: Boolean;
    procedure Add(Item: TSectionBase; TagIndex: Integer); overload;
    procedure Add(var Section: TSection; TagIndex: Integer); overload;
    procedure AddSectionsToList;
    procedure CopySelectedText;
{$ifdef UseFormTree}
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
{$endif UseFormTree}
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); virtual;
    property Document: ThtDocument read FDocument; {the ThtDocument that holds the whole document}
    property OwnerBlock: TBlock read FOwnerBlock;
  end;

  TCell = class(TCellBasic)
  private
    DrawYY: Integer;
  public
    constructor Create(Parent: TBlock);
    constructor CreateCopy(Parent: TBlock; T: TCellBasic);
    destructor Destroy; override;
    function DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X, Y, XRef, YRef: Integer): Integer; override;
  end;

//------------------------------------------------------------------------------
// TSizeableObj is base class for floating objects TImageObj, TFrameOBj and TPanelObj.
//
// These objects may appear in text flow or attribute ALIGN or style FLOAT or
// style POSITION may push them out of the flow floating to the left or right
// side or anywhere else in the containing block or even fixed in the viewport
// resp. on printed pages.
//------------------------------------------------------------------------------

  TSizeableObj = class(TFloatingObj)
  private
    FAlt: ThtString; {the alt= attribute}
    FClientHeight: Integer; {does not include VSpace}
    FClientWidth: Integer; {does not include HSpace}
  public
    ClientSizeKnown: Boolean; {know size of image}
    SpecWidth: Integer; {as specified by <img, applet, panel, object, or iframe> tag}
    SpecHeight: Integer; {as specified by <img, applet, panel, object, or iframe> tag}
    Title: ThtString;
  protected
    procedure CalcSize(AvailableWidth, AvailableHeight, SetWidth, SetHeight: Integer; IsClientSizeSpecified: Boolean);
    function GetClientHeight: Integer; override;
    function GetClientWidth: Integer; override;
    procedure SetClientHeight(Value: Integer); override;
    procedure SetClientWidth(Value: Integer); override;
  public
    NoBorder: Boolean; {set if don't want blue border}
    BorderWidth: Integer;
    MargArray: ThtMarginArray;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    constructor SimpleCreate(Parent: TCellBasic);
    function PtInDrawRect(X, Y: Integer; var IX, IY: Integer): Boolean; override;
    procedure DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); override;
    procedure ProcessProperties(Prop: TProperties);
    procedure SetAlt(CodePage: Integer; const Value: ThtString);
    property Alt: ThtString read FAlt;
  end;

  TSizeableObjList = class(TFloatingObjList) {a list of TImageObj's, TPanelObj's , and TFrameObj's}
  end;


  // base class for inline panel, frame and progress node
  TControlObj = class(TSizeableObj)
  protected
    SetWidth, SetHeight: Integer;
    function GetBackgroundColor: TColor; virtual;
    function GetControl: TWinControl; virtual; abstract;
    property ClientControl: TWinControl read GetControl;
    property BackgroundColor: TColor read GetBackgroundColor;
  public
    ShowIt: Boolean;
    procedure DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); override;
  end;


  // inline panel (object) node
  TPanelObj = class(TControlObj)
  protected
    function GetBackgroundColor: TColor; override;
    function GetControl: TWinControl; override;
  public
    Panel, OPanel: ThvPanel;
    OSender: TObject;
    PanelPrintEvent: TPanelPrintEvent;
    FUserData: TObject;
    FMyPanelObj: TPanelObj;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties; ObjectTag: Boolean);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); override;
  end;

{$ifdef UseGenerics}
  TPanelObjList = class(TObjectList<TPanelObj>)
{$else}
  TPanelObjList = class(TObjectList)
  private
    function Get(Index: Integer): TPanelObj; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TPanelObj read Get; default;
{$endif}
  end;

  // inline frame node
  TFrameObj = class(TControlObj)
  private
    FViewer: TViewerBase;
    FSource, FUrl: ThtString;
    frMarginWidth: Integer;
    frMarginHeight: Integer;
    NoScroll: Boolean;
    procedure CreateFrame;
    procedure UpdateFrame;
  protected
    function GetBackgroundColor: TColor; override;
    function GetControl: TWinControl; override;
  public
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure DrawInline(Canvas: TCanvas; X, Y: Integer; YBaseline: Integer; FO: TFontObj); override;
  end;

  // inline progress (object) node
  TProgressObj = class(TControlObj)
  protected
    function GetControl: TWinControl; override;
  public
    Progress: ThvProgressBar;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties; ObjectTag: Boolean);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); override;
  end;

  // inline meter (object) node
  TMeterObj = class(TControlObj)
  protected
    function GetControl: TWinControl; override;
  public
    Meter: ThvMeter;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties; ObjectTag: Boolean);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); override;
  end;


  ThtHover = (hvOff, hvOverUp, hvOverDown);

  TImageFormControlObj = class;

  // inline image node
  TImageObj = class(TSizeableObj)
  private
    FSource: ThtString;
    FImage: ThtImage;
    OrigImage: ThtImage; {same as above unless swapped}
    Transparent: ThtImageTransparency; {None, Lower Left Corner, or Transp GIF}
    FHover: ThtHover;
    FHoverImage: Boolean;
    AltHeight, AltWidth: Integer;
    function GetGraphic: TGraphic;
    procedure SetHover(Value: ThtHover);
    procedure DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: ThtImage);
  public
    ObjHeight, ObjWidth: Integer; {width as drawn}
    IsMap, UseMap: Boolean;
    MapName: ThtString;
    MyFormControl: TImageFormControlObj; {if an <INPUT type=image}
    Swapped: Boolean; {image has been replaced}
    Missing: Boolean; {waiting for image to be downloaded}

    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
    constructor SimpleCreate(Parent: TCellBasic; const AnURL: ThtString);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure DrawInline(Canvas: TCanvas; X: Integer; Y, YBaseline: Integer; FO: TFontObj); override;
    function InsertImage(const UName: ThtString; Error: Boolean; out Reformat: Boolean): Boolean;

    property Graphic: TGraphic read GetGraphic;
    property Hover: ThtHover read FHover write SetHover;
    property Image: ThtImage read FImage ; //write SetImage;
    property Source: ThtString read FSource; {the src= attribute}
    procedure ReplaceImage(NewImage: TStream);
  end;

  TImageRec = class(TObject)
  public
    AObj: TImageObj;
    ACanvas: TCanvas;
    AX, AY: Integer;
    AYBaseline: Integer;
    AFO: TFontObj;
  end;

{$ifdef UseGenerics}
  TDrawList = class(TObjectList<TImageRec>)
{$else}
  TDrawList = class(TObjectList)
{$endif}
    procedure AddImage(Obj: TImageObj; Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
    procedure DrawImages;
  end;

//------------------------------------------------------------------------------
// TSection holds a series of text and inline tags like images and panels.
//------------------------------------------------------------------------------
// Holds tags like A, B, I, FONT, SPAN, IMG, ...
//------------------------------------------------------------------------------

  TFormControlObj =  class;
  TFormControlObjList = class;

  ThtBorderRec = class {record for inline borders}
  private
    BStart, BEnd: Integer;
    OpenStart, OpenEnd: Boolean;
    BRect: TRect;
    MargArray: ThtMarginArray;
    procedure DrawTheBorder(Canvas: TCanvas; XOffset, YOffSet: Integer; Printing: Boolean; ThemedColorToRGB: ThtThemedColor );
  end;

{$ifdef UseGenerics}
  ThtBorderRecList = class(TObjectList<ThtBorderRec>);
{$else}
  ThtBorderRecList = class(TObjectList)
  private
    function Get(Index: Integer): ThtBorderRec; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: ThtBorderRec read Get; default;
  end;
{$endif}

  ThtLineRec = class {holds info on a line of text}
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
    BorderList: ThtBorderRecList; {List of inline borders (ThtBorderRec's) in this Line}
    FirstDraw: Boolean; {set if border processing needs to be done when first drawn}
    Drawn: Boolean; // set if drawn by section and thus actual position has been calculated
    FirstX: Integer; {x value at FirstDraw}
    Shy: Boolean;
  public
    constructor Create(SL: ThtDocument);
    procedure Clear;
    destructor Destroy; override;
  end;

{$ifdef UseGenerics}
  TLineRecList = class(TObjectList<ThtLineRec>);
{$else}
  TLineRecList = class(TObjectList)
  private
    function Get(Index: Integer): ThtLineRec; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: ThtLineRec read Get; default;
  end;
{$endif}

  PXArray = array of Integer;

  ThtIndexObj = class
  public
    Pos: Integer;
    Index: Integer;
  end;

{$ifdef UseGenerics}
  ThtIndexObjList = class(TObjectList<ThtIndexObj>);
{$else}
  ThtIndexObjList = class(TObjectList)
  private
    function Get(Index: Integer): ThtIndexObj; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: ThtIndexObj read Get; default;
  end;
{$endif}

  ThtTextWrap = (
    twNo,      // 'n'
    twYes,     // 'y'
    twSoft,    // 's'
    twOptional // 'a'
    );

  ThtTextWrapArray = array of ThtTextWrap;

  // TODO: stop creating TSections, which mix up several inline elements into one instance.
  // Therefore we cannot control individual properties/attributes of single elements in it.
  // TSection has to be reduced to a simple inline block for text of a single inline element
  // resp. to an anonymous block for text of a block element.
  // Also remove rendering code as soon as rendering is done by TInlineSection.
  // As a document is a TSectionList, TInlineSection will do it.
  TSection = class(TSectionBase)
  {TSection holds and renders inline content. Mainly text and floating images, panel, frames, and form controls.}
  private
    BreakWord: Boolean;
    DrawWidth: Integer;
    FLPercent: Integer;
    LineHeight: Integer;
    StoredMin, StoredMax: TSize;

    SectionNumber: Integer;
    ThisCycle: Integer;

    BuffS: ThtString;  {holds the text or one of #2 (Form), #4 (Image/Panel), #8 (break char) for the section} //TODO -oBG, 30.11.2013: #1 (Table for display:inline-table)
    FBuff: PWideChar;    {same as above}
    Brk: ThtTextWrapArray; //string;        // Brk[n]: Can I wrap to new line after BuffS[n]? One entry per character in BuffS
    SIndexList: ThtIndexObjList; {list of Source index changes}
    Lines: TLineRecList; {List of ThtLineRecs,  info on all the lines in section}

    function GetIndexObj(I: Integer): ThtIndexObj;
    property PosIndex[I: Integer]: ThtIndexObj read GetIndexObj;
    procedure CheckForInlines(LR: ThtLineRec);
  public
    Images: TSizeableObjList; {list of TSizeableObj's, the images, panels and iframes in section}
    FormControls: TFormControlObjList; {list of TFormControls in section}
    XP: PXArray;
    AnchorName: Boolean;

    Fonts: TFontList; {List of FontObj's in this section}
    Justify: ThtJustify; {Left, Centered, Right}
    TextWidth: Integer;
    WhiteSpaceStyle: ThtWhiteSpaceStyle;
    FirstLineIndent: Integer;             // Issue 365: public for TRichView
    property Buff: PWideChar read FBuff;  // Issue 365: public for TRichView
  protected
    constructor Create(Parent: TCellBasic); overload;
    function TryGetClear(var Clear: ThtClearStyle): Boolean; override;
  public
    constructor Create(Parent: TCellBasic; Attr: TAttributeList; Prop: TProperties; AnURL: TUrlTarget; FirstItem: Boolean); overload;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function AddFormControl(Which: TElemSymb; AMasterList: ThtDocument; L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TFormControlObj;
    function AddFrame(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TFrameObj;
    function AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TImageObj;
    function AddPanel(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TPanelObj;
    function AddProgress(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TProgressObj;
    function AddMeter(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TMeterObj;
    function CreatePanel(L: TAttributeList; ACell: TCellBasic; Prop: TProperties): TPanelObj;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function FindCountThatFits(Canvas: TCanvas; Width: Integer; Start: PWideChar; Max: Integer): Integer;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; override;
    function FindTextSize(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: Boolean): TSize;
    function FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; override;
    function PtInObject(X: Integer; Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean; override;
    procedure AddChar(C: WideChar; Index: Integer); virtual;
    procedure AddOpBrk;
    procedure AddPanel1(PO: TPanelObj; Index: Integer);
    procedure AddTokenObj(T: TTokenObj); virtual;
    procedure Allocate(N: Integer);
    procedure ChangeFont(Prop: TProperties);
    procedure CheckFree;
    procedure CopySelectedText; override;
    procedure Finish;
    procedure HRef(IsHRef: Boolean; List: ThtDocument; AnURL: TUrlTarget; Attributes: TAttributeList; Prop: TProperties);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); override;
    procedure ProcessText(TagIndex: Integer); virtual;
  end;

//  //BG, 01.12.2013: handle display:inline
//  // anonymous block for consecutive inline level elements.
//  TInlineSection = class(TSection)
//  public
//    constructor Create(Parent: TCellBasic);
//    procedure AddElement(Section: TSection);
//  end;

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

  TBlock = class(TBlockBase)
  protected
    function GetBorderWidth: Integer; virtual;
    function CalcDisplayExtern: ThtDisplayStyle; override;
    function CalcDisplayIntern: ThtDisplayStyle; override;
    procedure ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); virtual;
    procedure ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer); virtual;
    procedure DrawBlockBorder(Canvas: TCanvas; const ORect, IRect: TRect); virtual;
    property BorderWidth: Integer read GetBorderWidth;
  public
    MyCell: TBlockCell; // the block content
    MargArrayO: ThtVMarginArray;
    BGImage: TImageObj;    //TODO -oBG, 10.03.2011: see also bkGnd and bkColor in TCellBasic one background should be enough.
    BlockTitle: ThtString;

    // Notice: styling tag attributes are deprecated by W3C and must be translated
    //         to the corresponding style properties with a very low priority.

    // BEGIN: this area is copied by move() in CreateCopy() - NO string types allowed!
    MargArray: ThtMarginArray;
    FGColor: Integer;
    HasBorderStyle: Boolean;

    PRec: PtPositionRec; // background image position
    Visibility: ThtVisibilityStyle;
    TopAuto: Boolean;
    BottomAuto: Boolean;
    BreakBefore, BreakAfter, KeepIntact: Boolean;
    HideOverflow: Boolean;
    Justify: ThtJustify;
    Converted: Boolean;
    // END: this area is copied by move() in CreateCopy()

    ContentWidth: Integer;
    ClearAddon: Integer;
    NeedDoImageStuff: Boolean;
    TiledImage: ThtImage;
    TopP, LeftP: Integer;
    DrawList: TSectionBaseList;
    NoMask: Boolean;
    ClientContentBot: Integer;
    MyRect: TRect; // calculated by DrawBlock(): the outmost rectangle of this block incl. border and padding but without margins in screen coordinates.
    MyIMgr: TIndentManager;
    RefIMgr: TIndentManager;

    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; virtual;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; override;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean; override;
    procedure AddSectionsToList; override;
    procedure CollapseAdjoiningMargins;
    procedure CollapseNestedMargins;
    procedure CopySelectedText; override;
    procedure DrawBlock(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, Y, XRef, YRef: Integer);
    procedure DrawSort;
    procedure DrawTheList(Canvas: TCanvas; const ARect: TRect; ClipWidth, X, XRef, YRef: Integer);
{$ifdef UseFormTree}
    procedure FormTree(const Indent: ThtString; var Tree: ThtString);
{$endif UseFormTree}
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); override;
  end;

//------------------------------------------------------------------------------
// THtmlForm, an object containing a form for user input
//------------------------------------------------------------------------------

  TRadioButtonFormControlObj = class;

  THtmlForm = class(TObject)
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
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

{$ifdef UseGenerics}
  THtmlFormList = class(TObjectList<THtmlForm>);
{$else}
  THtmlFormList = class(TObjectList)
  private
    function Get(Index: Integer): THtmlForm; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: THtmlForm read Get; default;
  end;
{$endif}

  TFormControlObj = class(TFloatingObj)
  private
    FName: ThtString;
    FID: ThtString;
    FTitle: ThtString;
    FValue: ThtString;
    procedure SetValue(const Value: ThtString);
  protected
    Active: Boolean;
    CodePage: Integer;
    PaintBitmap: TBitmap;
    function GetClientHeight: Integer; override;
    function GetClientLeft: Integer; virtual;
    function GetClientTop: Integer; virtual;
    function GetClientWidth: Integer; override;
    function GetControl: TWinControl; virtual; abstract;
    function GetTabOrder: Integer; virtual;
    function GetTabStop: Boolean; virtual;
    function IsHidden: Boolean; virtual;
    procedure DoOnChange; virtual;
    procedure SaveContents; virtual;
    procedure SetClientHeight(Value: Integer); override;
    procedure SetClientLeft(Value: Integer); virtual;
    procedure SetClientTop(Value: Integer); virtual;
    procedure SetClientWidth(Value: Integer); override;
    procedure SetTabOrder(Value: Integer); virtual;
    procedure SetTabStop(Value: Boolean); virtual;
  public
    // begin copy by move()
    MyForm: THtmlForm;
    BordT, BordB: Integer;
    FHeight, FWidth: Integer;
    Disabled: Boolean;
    Readonly: Boolean;
    BkColor: TColor;
    // end copy by move()
    ShowIt: Boolean;
    OnBlurMessage: ThtString;
    OnChangeMessage: ThtString;
    OnClickMessage: ThtString;
    OnFocusMessage: ThtString;

    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); virtual;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): Boolean; virtual;
    procedure DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer); override;
    procedure DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj); override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: Integer); virtual;
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

//    property Height: Integer read GetClientHeight write SetClientHeight;
    property Hidden: Boolean read IsHidden;
    property ID: ThtString read FID write FID; {ID attribute of control}
    property Left: Integer read GetClientLeft write SetClientLeft;
    property Name: ThtString read FName write FName; {Name given to control}
    property TabOrder: Integer read GetTabOrder write SetTabOrder;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property TheControl: TWinControl read GetControl; {the Delphi control, TButton, TMemo, etc}
    property Title: ThtString read FTitle write FTitle;
    property Top: Integer read GetClientTop write SetClientTop;
    property Value: ThtString read FValue write SetValue;
//    property Width: Integer read GetClientWidth write SetClientWidth;
//    property YValue: Integer read FYValue;
  end;

  //BG, 15.01.2011:
  TFormControlObjList = class(TFloatingObjList)
  private
    function GetItem(Index: Integer): TFormControlObj; {$ifdef UseInline} inline; {$endif}
  public
    procedure ActivateTabbing;
    procedure DeactivateTabbing;
    property Items[Index: Integer]: TFormControlObj read GetItem; default;
  end;

  TImageFormControlObj = class(TFormControlObj)
  private
    FControl: ThtButton;
    MyImage: TImageObj;
  protected
    function GetControl: TWinControl; override;
  public
    XPos, YPos, XTmp, YTmp: Integer; {click position}
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): Boolean; override;
    procedure ImageClick(Sender: TObject);
    procedure ProcessProperties(Prop: TProperties); override;
  end;

  TFormRadioButton = class(ThtRadioButton)
  private
    IDName: ThtString;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  end;

  // To isolate radio buttons from each other. THtmlForm.DoRadios() unchecks grouped buttons.
  TFormRadioPanel = class(TWinControl)
  end;

  TRadioButtonFormControlObj = class(TFormControlObj)
  private
    FPanel: TFormRadioPanel;
    FControl: TFormRadioButton;
    WasChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function GetClientLeft: Integer; override;
    function GetClientTop: Integer; override;
    function GetColor: TColor; //override;
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
    procedure SetClientHeight(Value: Integer); override;
    procedure SetClientLeft(Value: Integer); override;
    procedure SetClientTop(Value: Integer); override;
    procedure SetClientWidth(Value: Integer); override;
    procedure SetColor(const Value: TColor); //override;
    procedure SetTabOrder(Value: Integer); override;
    procedure SetTabStop(Value: Boolean); override;
  public
    IsChecked: Boolean;
    //xMyCell: TCellBasic;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): Boolean; override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure RadioClick(Sender: TObject);
    procedure ResetToValue; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    property Checked: Boolean read GetChecked write SetChecked;
    property Color: TColor read GetColor write SetColor;
  end;

  ThtListType = (None, Ordered, Unordered, Definition, liAlone);

//------------------------------------------------------------------------------
// some blocks
//------------------------------------------------------------------------------

  { THRBlock }

  THRBlock = class(TBlock)
  public
    Align: ThtJustify;
    MyHRule: TSectionBase;
    constructor CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
  end;

  TListBlock = class(TBlock)
  end;

  TBlockLI = class(TBlock)
  private
    FListType: ThtListType;
    FListNumb: Integer;
    FListStyleType: ThtBulletStyle;
    FListFont: TFont;
    Image: TImageObj;
    FirstLineHt: Integer;
    procedure SetListFont(const Value: TFont);
  public
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties;
      Sy: TElemSymb; APlain: Boolean; AIndexType: ThtChar;
      AListNumb, ListLevel: Integer);
    constructor CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property ListNumb: Integer read FListNumb write FListNumb;
    property ListStyleType: ThtBulletStyle read FListStyleType write FListStyleType;
    property ListType: ThtListType read FListType write FListType;
    property ListFont: TFont read FListFont write SetListFont;
  end;

  TFieldsetBlock = class(TBlock)
  private
    FLegend: TBlockCell;
  protected
    procedure ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer); override;
    procedure ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); override;
  public
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    property Legend: TBlockCell read FLegend;
  end;

  TBodyBlock = class(TBlock)
  public
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

//------------------------------------------------------------------------------
// THtmlTable, a block that represents a html table
//------------------------------------------------------------------------------

  TTableFrame = (tfVoid, tfAbove, tfBelow, tfHSides, tfLhs, tfRhs, tfVSides, tfBox, tfBorder);
  TTableRules = (trNone, trGroups, trRows, trCols, trAll);
  TIntArray = array of Integer;
  TWidthTypeArray = array of TWidthType;
  TIntegerPerWidthType = array [TWidthType] of Integer;

  TTableBlock = class;

  TCellObjCell = class(TCell)
  private
    MyRect: TRect;
    Title: ThtString;
    Url, Target: ThtString;
  public
    constructor CreateCopy(Parent: TBlock; T: TCellObjCell);
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; override;
  end;

  TCellObjBase = class(TObject)
  protected
    // BEGIN: this area is copied by move() in AssignTo() - NO string types or any other references like objects allowed!
    FColSpan: Integer; {column spans for this cell}
    FRowSpan: Integer; {row spans for this cell}
    FHzSpace: Integer;
    FVrSpace: Integer;
    FSpecWd: TSpecWidth; {Width attribute (percentage or absolute)}
    FSpecWdMin: TSpecWidth;
    FSpecWdMax: TSpecWidth;
    FSpecHt: TSpecWidth; {Height as specified}
    // END: this area is copied by move() in AssignTo()
    function GetCell: TCellObjCell; virtual; abstract;
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacingHorz, CellSpacingVert: Integer; Border: Boolean; Light, Dark: TColor); virtual; abstract;
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacingHorz, CellSpacingVert : Integer; var Curs: Integer); virtual; abstract;
  public
    function Clone(Parent: TBlock): TCellObjBase; virtual; abstract;
    procedure AssignTo(Destin: TCellObjBase); virtual;
    property Cell: TCellObjCell read GetCell;
    property ColSpan: Integer read FColSpan write FColSpan; {column and row spans for this cell}
    property RowSpan: Integer read FRowSpan write FRowSpan; {column and row spans for this cell}
    property HzSpace: Integer read FHzSpace write FHzSpace;
    property VrSpace: Integer read FVrSpace write FVrSpace;
    property SpecHt: TSpecWidth read FSpecHt write FSpecHt; {Height as specified}
// BG, 12.01.2012: not C++-Builder compatible
//    property SpecHtType: TWidthType read FSpecHt.VType write FSpecHt.VType; {Height as specified}
//    property SpecHtValue: Double read FSpecHt.Value write FSpecHt.Value; {Height as specified}
    property SpecWd: TSpecWidth read FSpecWd write FSpecWd; {Width as specified}
    property SpecWdMin: TSpecWidth read FSpecWdMin write FSpecWdMin;
    property SpecWdMax: TSpecWidth read FSpecWdMax write FSpecWdMax;
// BG, 12.01.2012: not C++-Builder compatible
//    property SpecWdType: TWidthType read FSpecWd.VType write FSpecWd.VType; {Height as specified}
//    property SpecWdValue: Double read FSpecWd.Value write FSpecWd.Value; {Height as specified}
  end;

  TDummyCellObj = class(TCellObjBase)
  {holds one dummy cell of the table}
  protected
    function GetCell: TCellObjCell; override;
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacingHorz, CellSpacingVert: Integer; Border: Boolean; Light, Dark: TColor); override;
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacingHorz, CellSpacingVert : Integer; var Curs: Integer); override;
  public
    constructor Create(RSpan: Integer);
    function Clone(Parent: TBlock): TCellObjBase; override;
  end;

  TCellObj = class(TCellObjBase)
  {holds one cell of the table and some other information}
  private
    // BEGIN: this area is copied by move() in CreateCopy() - NO string types allowed!
    FWd: Integer; {total width (may cover more than one column)}
    FHt: Integer; {total height (may cover more than one row)}
    FVSize: Integer; {Actual vertical size of contents}
    FYIndent: Integer; {Vertical indent}
    FVAlign: ThtAlignmentStyle; {Top, Middle, or Bottom}
    FEmSize, FExSize: Integer;
    FPRec: PtPositionRec; // background image position info
    FPad: TRect;
    FBrd: TRect;
    FHideOverflow: Boolean;
    FHasBorderStyle: Boolean;
    FShowEmptyCells: Boolean;
    // END: this area is copied by move() in CreateCopy()
    FCell: TCellObjCell;
    procedure Initialize(TablePadding: Integer; const BkImageName: ThtString; const APRec: PtPositionRec; Border: Boolean);
  protected
    function GetCell: TCellObjCell; override;
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacingHorz, CellSpacingVert : Integer; Border: Boolean; Light, Dark: TColor); override;
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacingHorz, CellSpacingVert: Integer; var Curs: Integer); override;
  private

    // BG, 08.01.2012: Issue 109: C++Builder cannot handle properties that reference record members.
    // - added for legacy support only, will be removed in a near future release.
    //   Please use properties Border and Padding instead.
    function GetBorderBottom: Integer;
    function GetBorderLeft: Integer;
    function GetBorderRight: Integer;
    function GetBorderTop: Integer;
    function GetPaddingBottom: Integer;
    function GetPaddingLeft: Integer;
    function GetPaddingRight: Integer;
    function GetPaddingTop: Integer;
    procedure SetBorderBottom(const Value: Integer);
    procedure SetBorderLeft(const Value: Integer);
    procedure SetBorderRight(const Value: Integer);
    procedure SetBorderTop(const Value: Integer);
    procedure SetPaddingBottom(const Value: Integer);
    procedure SetPaddingLeft(const Value: Integer);
    procedure SetPaddingRight(const Value: Integer);
    procedure SetPaddingTop(const Value: Integer);
  public

    NeedDoImageStuff: Boolean;
    BGImage: TImageObj;
    TiledImage: ThtImage;
    MargArray: ThtMarginArray;
    MargArrayO: ThtVMarginArray;
    NoMask: Boolean;
    BreakBefore, BreakAfter, KeepIntact: Boolean;

    constructor Create(Parent: TTableBlock; AVAlign: ThtAlignmentStyle; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TBlock; T: TCellObj);
    destructor Destroy; override;
    function Clone(Parent: TBlock): TCellObjBase; override;
    procedure AssignTo(Destin: TCellObjBase); override;

    property Border: TRect read FBrd write FBrd; //was: BrdTop, BrdRight, BrdBottom, BrdLeft: Integer;
    property BrdBottom: Integer read getBorderBottom write setBorderBottom;
    property BrdLeft: Integer read getBorderLeft write setBorderLeft;
    property BrdRight: Integer read getBorderRight write setBorderRight;
    property BrdTop: Integer read getBorderTop write setBorderTop;
    property Cell: TCellObjCell read FCell;
    property EmSize: Integer read FEmSize write FEmSize;
    property ExSize: Integer read FExSize write FExSize;
    property HasBorderStyle: Boolean read FHasBorderStyle write FHasBorderStyle;
    property Ht: Integer read FHt {write FHt}; {total height (may cover more than one row)}
    property Padding: TRect read FPad write FPad; //was: PadTop, PadRight, PadBottom, PadLeft: Integer;
    property PadBottom: Integer read getPaddingBottom write setPaddingBottom;
    property PadLeft: Integer read getPaddingLeft write setPaddingLeft;
    property PadRight: Integer read getPaddingRight write setPaddingRight;
    property PadTop: Integer read getPaddingTop write setPaddingTop;
    property PRec: PtPositionRec read FPRec write FPRec;
    property ShowEmptyCells: Boolean read FShowEmptyCells write FShowEmptyCells;
    property HideOverflow: Boolean read FHideOverflow write FHideOverflow;
    property VAlign: ThtAlignmentStyle read FVAlign write FVAlign; {Top, Middle, or Bottom}
    property VSize: Integer read FVSize write FVSize; {Actual vertical size of contents}
    property Wd: Integer read FWd write FWd; {total width (may cover more than one column)}
    property YIndent: Integer read FYIndent write FYIndent; {Vertical indent}
  end;

{$ifdef UseGenerics}
  TCellList = class(TObjectList<TCellObjBase>)
{$else}
  TCellList = class(TObjectList)
  {holds one row of the html table, a list of TCellObj}
  private
    function GetCellObj(Index: Integer): TCellObjBase; {$ifdef UseInline} inline; {$endif}
{$endif}
  public
    RowHeight: Integer;
    SpecRowHeight: TSpecWidth;
    RowSpanHeight: Integer; {height of largest rowspan}
    BkGnd: Boolean;
    BkColor: TColor;
    BkImage: ThtString;
    APRec: PtPositionRec;
    BreakBefore, BreakAfter, KeepIntact: Boolean;
    RowType: TRowType;

    constructor Create(Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TBlock; T: TCellList);
    procedure Initialize;
    function DrawLogicA(Canvas: TCanvas; const Widths: TIntArray; Span, CellSpacingHorz,CellSpacingVert, AHeight, Rows: Integer;
      out Desired: Integer; out Spec, More: Boolean): Integer;
    procedure DrawLogicB(Canvas: TCanvas; Y, CellSpacingHorz, CellSpacingVert: Integer; var Curs: Integer);
    function Draw(Canvas: TCanvas; Document: ThtDocument; const ARect: TRect; const Widths: TIntArray;
      X, Y, YOffset, CellSpacingHorz,CellSpacingVert: Integer; Border: Boolean; Light, Dark: TColor; MyRow: Integer): Integer;
    procedure Add(CellObjBase: TCellObjBase);
{$ifdef UseGenerics}
{$else}
    property Items[Index: Integer]: TCellObjBase read GetCellObj; default;
{$endif}
  end;

  // BG, 26.12.2011:
{$ifdef UseGenerics}
  TRowList = class(TObjectList<TCellList>);
{$else}
  TRowList = class(TObjectList)
  private
    function GetItem(Index: Integer): TCellList; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TCellList read GetItem; default;
  end;
{$endif}

  TColSpec = class
  private
    FWidth: TSpecWidth;
    FAlign: ThtString;
    FVAlign: ThtAlignmentStyle;
  public
    constructor Create(const Width: TSpecWidth; Align: ThtString; VAlign: ThtAlignmentStyle);
    constructor CreateCopy(const ColSpec: TColSpec);
    property ColWidth: TSpecWidth read FWidth;
    property ColAlign: ThtString read FAlign;
    property ColVAlign: ThtAlignmentStyle read FVAlign;
  end;

  // BG, 26.12.2011:
{$ifdef UseGenerics}
  TColSpecList = class(TObjectList<TColSpec>);
{$else}
  TColSpecList = class(TObjectList)
  private
    function GetItem(Index: Integer): TColSpec; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: TColSpec read GetItem; default;
  end;
{$endif}

  THtmlTable = class;

  TTableBlock = class(TBlock)
  protected
    function GetBorderWidth: Integer; override;
  public
    Table: THtmlTable;
    WidthAttr: Integer;
    AsPercent: Boolean;
    BkColor: TColor;
    BkGnd: Boolean;
    HSpace, VSpace: Integer;
    HasCaption: Boolean;
    TableBorder: Boolean;
    Justify: ThtJustify;
    TableIndent: Integer;

    constructor Create(Parent: TCellBasic; Attr: TAttributeList; Prop: TProperties; TableLevel: Integer);
    constructor CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode); override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
    function FindWidth1(Canvas: TCanvas; AWidth, AHeight, ExtMarg: Integer): Integer;
    procedure AddSectionsToList; override;
  end;

  TTableAndCaptionBlock = class(TBlock)
  private
    procedure SetCaptionBlock(Value: TBlock);
  public
    TopCaption: Boolean;
    TableBlock: TTableBlock;
    FCaptionBlock: TBlock;
    Justify: ThtJustify;
    TableID: ThtString;
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties; ATableBlock: TTableBlock);
    constructor CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode); override;
    procedure CancelUsage;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); override;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; override;
    property CaptionBlock: TBlock read FCaptionBlock write SetCaptionBlock;
  end;

  THtmlTable = class(TSectionBase)
  private
    FOwnerTableBlock: TTableBlock;
    TablePartRec: TTablePartRec;
    HeaderHeight, HeaderRowCount, FootHeight, FootStartRow, FootOffset: Integer;
    BodyBreak: Integer;
    HeadOrFoot: Boolean;
    FColSpecs: TColSpecList; // Column width specifications
    // calculated in GetMinMaxWidths
    Percents: TIntArray;     {percent widths of columns}
    Multis: TIntArray;       {multi widths of columns}
    MaxWidths: TIntArray;
    MinWidths: TIntArray;
    ColumnCounts: TIntegerPerWidthType;
    ColumnSpecs: TWidthTypeArray;
    CycleNumber: Integer;
    // these fields are copied via Move() in CreateCopy. Don't add reference counted data like strings and arrays.
    Initialized: Boolean;
    brdWidthAttr: Integer;  {Width attribute as entered}
    HasBorderWidthAttr: Boolean; {width of border has been set by attr}
    Frame: TTableFrame;
    Float: Boolean;         {if floating}
    NumCols: Integer;       {Number columns in table}
    TableWidth: Integer;    {width of table}
    tblWidthAttr: Integer;  {Width attribute as entered}
    FCellPadding: Integer;
    FCellSpacingHorz, FCellSpacingVert: Integer;
    BorderColorLight: TColor;
    BorderColorDark: TColor;
    EndList: Boolean;        {marker for copy}
    // end of Move()d fields
    DrawX: Integer;
    //DrawY: Integer;
    FBkGnd: Boolean;
    FBkColor: TColor;
    Widths: TIntArray;       {holds calculated column widths}
    Heights: TIntArray;      {holds calculated row heights}
    //
    procedure IncreaseWidthsByWidth(WidthType: TWidthType; var Widths: TIntArray; StartIndex, EndIndex, Required, Spanned, Count: Integer);
    procedure IncreaseWidthsByPercentage(var Widths: TIntArray; StartIndex, EndIndex, Required, Spanned, Percent, Count: Integer);
    procedure IncreaseWidthsByMinMaxDelta(WidthType: TWidthType; var Widths: TIntArray; StartIndex, EndIndex, Excess, DeltaWidth, Count: Integer; const Deltas: TIntArray);
    procedure IncreaseWidthsRelatively(var Widths: TIntArray; StartIndex, EndIndex, Required, SpannedMultis: Integer; ExactRelation: Boolean);
    procedure IncreaseWidthsEvenly(WidthType: TWidthType; var Widths: TIntArray; StartIndex, EndIndex, Required, Spanned, Count: Integer);
    procedure Initialize; // add dummy cells, initialize cells, prepare arrays
    procedure GetMinMaxWidths(Canvas: TCanvas; AvailableWidth, AvailableHeight: Integer);
  public
    Rows: TRowList;        {a list of TCellLists}
    Rules: TTableRules;
    BorderWidth: Integer;   {width of border}
    BorderColor: TColor;      //BG, 13.06.2010: added for Issue 5: Table border versus stylesheets

    constructor Create(OwnerTableBlock: TTableBlock; Parent: TCellBasic; Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure DoColumns(Count: Integer; const SpecWidth: TSpecWidth; VAlign: ThtAlignmentStyle; const Align: ThtString);
    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer); override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; override;
    function PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean; override;
    function FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean; override;
    function GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean; override;
    function FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; override;
    function FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer; override;
    function FindSourcePos(DocPos: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; override;
    procedure CopySelectedText; override;
    property ColSpecs: TColSpecList read FColSpecs;
    property TableHeight: Integer read SectionHeight write SectionHeight;   {height of table itself, not incl caption}
    property CellPadding: Integer read FCellPadding;
    property CellSpacingHorz: Integer read FCellSpacingHorz;
    property CellSpacingVert: Integer read FCellSpacingVert;
    property BkGnd: Boolean read FBkGnd;
    property BkColor: TColor read FBkColor;
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
    property ChPos: Integer read FChPos write FChPos;
    property Document: ThtDocument read FDocument;
  end;

//------------------------------------------------------------------------------
// ThtDocument, a complete html document, that can draw itself on a canvas.
//------------------------------------------------------------------------------

  TExpandNameEvent = procedure(Sender: TObject; const SRC: ThtString; var Result: ThtString) of object;

  THtmlStyleList = class(TStyleList) {a list of all the styles -- the stylesheet}
  private
    Document: ThtDocument;
  protected
    procedure SetLinksActive(Value: Boolean); override;
  public
    constructor Create(AMasterList: ThtDocument);
  end;

  THtmlPropStack = class(TPropStack)
  public
    Document: ThtDocument;
    SIndex: Integer; //BG, 26.12.2010: seems, this is the current position in the original html-file.
    procedure PopAProp(Sym: TElemSymb);
    procedure PopProp;
    procedure PushNewProp(Sym: TElemSymb; Properties: TProperties; Attributes: TAttributeList; const APseudo: ThtString = '');
  end;

{$ifdef UseGenerics}
  TFormData = class(TObjectList<ThtStringList>);
{$else}
  TFormData = class(TObjectList)
  private
    function Get(Index: Integer): ThtStringList; {$ifdef UseInline} inline; {$endif}
  public
    property Items[Index: Integer]: ThtStringList read Get; default;
  end;
{$endif}

  ThtDocument = class(TCell) {a list of all the sections -- the html document}
  private
    FUseQuirksMode : Boolean;
    FPropStack: THtmlPropStack;
    FPageArea: TRect;
    FPrinted: Boolean; {set if actually printed anything else but background}
    FPixelsPerInch: Integer;
    procedure AdjustFormControls;
    procedure AddSectionsToPositionList(Sections: TSectionBase);
    function CopyToBuffer(Buffer: TSelTextCount): Integer;
    procedure InsertMissingImage(const UName: ThtString; J: Integer; Error: Boolean; out Reformat: Boolean);
    procedure SetPageArea(const Value: TRect);
    function GetViewPort: TRect;
    procedure SetPrinted(const Value: Boolean);
    {$ifdef has_StyleElements}
    procedure SetStyleElements(const AValue : TStyleElements);
    {$endif}
  protected
    CB: TSelTextCount;
    {$ifdef has_StyleElements}
    FStyleElements : TStyleElements;
    procedure UpdateStyleElements; virtual;
    {$endif}
  public

    // copied by move() in CreateCopy()
    ShowImages: Boolean; {set if showing images}
    Printing: Boolean; {set if printing -- also see IsCopy}
    YOff: Integer; {marks top of window that's displayed}
    YOffChange: Boolean; {when above changes}
    XOffChange: Boolean; {when x offset changes}
    NoPartialLine: Boolean; {set when printing if no partial line allowed at page bottom}
    SelB, SelE: Integer;
    LinkVisitedColor, LinkActiveColor, HotSpotColor: TColor;
    PrintTableBackground: Boolean;
    PrintBackground: Boolean;
    PrintMonoBlack: Boolean;
    TheOwner: THtmlViewerBase; {the viewer that owns this document}
    PPanel: TWinControl; {the viewer's PaintPanel}
    GetBitmap: TGetBitmapEvent; {for OnBitmapRequest Event}
    GottenBitmap: TGottenBitmapEvent; {for OnBitmapRequest Event}
    GetImage: TGetImageEvent; {for OnImageRequest Event}
    GottenImage: TGottenImageEvent; {for OnImageRequest Event}
    OnExpandName: TExpandNameEvent;
    ObjectClick: TObjectClickEvent;
    ObjectFocus: ThtObjectEvent;
    ObjectBlur: ThtObjectEvent;
    ObjectChange: ThtObjectEvent;
    FileBrowse: TFileBrowseEvent;
    BackGround: TColor;
    // end of copied by move() in CreateCopy()
    // don't copy strings via move()
    PreFontName: TFontName; {<pre>, <code> font for document}

    OnBackgroundChange: TNotifyEvent;
    BackgroundImage: ThtImage;
    BackgroundImagePosition: PtPositionRec;
    BackgroundImageName: ThtString; {name of background bitmap}
    BackgroundImageLoaded: Boolean; {if background bitmap is loaded}
    HtmlFormList: THtmlFormList;
    AniList: ThtImageList; {list of all animated Images}
    SubmitForm: TFormSubmitEvent;
    ScriptEvent: TScriptEvent;
    PanelCreateEvent: TPanelCreateEvent;
    PanelDestroyEvent: TPanelDestroyEvent;
    PanelPrintEvent: TPanelPrintEvent;
    PageBottom: Integer;
    PageShortened: Boolean;
    MapList: ThtMap; {holds list of client maps, TMapItems}
    Timer: TTimer; {for animated GIFs}
    FormControlList: TFormControlObjList; {List of all TFormControlObj's in this SectionList}
    PanelList: TPanelObjList; {List of all TPanelObj's in this SectionList}
    MissingImages: ThtStringList; {images to be supplied later}
    ControlEnterEvent: TNotifyEvent;
    LinkList: TLinkList; {List of links (TFontObj's)}
    ActiveLink: TFontObj;
    LinksActive: Boolean;
    ActiveImage: TImageObj;
    ShowDummyCaret: Boolean;
    Styles: THtmlStyleList; {the stylesheet}
    DrawList: TDrawList;
    FirstLineHtPtr: PInteger;
    IDNameList: TIDObjectList;
    PositionList: TSectionBaseList;
    ImageCache: ThtImageCache;
    SectionCount: Integer;
    CycleNumber: Integer;
    ProgressStart: Integer;
    IsCopy: Boolean; {set when printing or making bitmap/metafile}
    NoOutput: Boolean;
    TabOrderList: TStringList;
    FirstPageItem: Boolean;
    StopTab: Boolean;
    InlineList: TInlineList; {actually TInlineList, a list of ThtInlineRec's}
    TableNestLevel: Integer;
    InLogic2: Boolean;
    LinkDrawnEvent: TLinkDrawnEvent;
    LinkPage: Integer;
    PrintingTable: THtmlTable;
    ScaleX, ScaleY: single;
    SkipDraw: Boolean;
    FNoBreak : Boolean;
    FCurrentStyle: TFontStyles;
    FCurrentForm : THtmlForm;
    constructor Create(Owner: THtmlViewerBase; APaintPanel: TWinControl);
    constructor CreateCopy(T: ThtDocument);
    destructor Destroy; override;
    function AddChPosObjectToIDNameList(const S: ThtString; Pos: Integer): Integer; {$ifdef UseInline} inline; {$endif}
    function CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean; override;
    function DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer; var ScrollWidth, Curs: Integer): Integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer; Y, XRef, YRef: Integer): Integer; override;
    function FindDocPos(SourcePos: Integer; Prev: Boolean): Integer; override;
    function FindSectionAtPosition(Pos: Integer; out TopPos, Index: Integer): TSectionBase;
    procedure GetFormControlData(FormData: TFormData);
    function GetSelLength: Integer;
    function GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
    function GetTheImage(const BMName: ThtString; var Transparent: ThtImageTransparency; out FromCache, Delay: Boolean): ThtImage;
    function GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType; override;
    procedure CancelActives;
    procedure CheckGIFList(Sender: TObject);
    procedure Clear; virtual;
    procedure ClearLists;
    procedure GetBackgroundImage;
    procedure HideControls;
    procedure InsertImage(const Src: ThtString; Stream: TStream; out Reformat: Boolean); overload;
    procedure InsertImage(const Src: ThtString; Bitmap: TBitmap; Color: TColor; Transparent: ThtImageTransparency; OwnsBitmap: Boolean; out Reformat: Boolean); overload;
    procedure LButtonDown(Down: Boolean);
    procedure ProcessInlines(SIndex: Integer; Prop: TProperties; Start: Boolean);
    procedure SetBackground(ABackground: TColor);
    procedure SetBackgroundImage(const Name: ThtString; const APrec: PtPositionRec);
    procedure SetFormControlData(FormData: TFormData);
    procedure SetYOffset(Y: Integer);
    procedure SetFonts(const Name, PreName: ThtString; ASize: Integer;
      AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor; LnksActive, LinkUnderLine: Boolean;
      ACodePage: TBuffCodePage; ACharSet: TFontCharSet; MarginHeight, MarginWidth: Integer);
    function ThemedColor(AColor: TColor; AStyleElement: ThtStyleElement): TColor;
    function ThemedColorToRGB(AColor: TColor; AStyleElement: ThtStyleElement): TColor;

    property UseQuirksMode : Boolean read FUseQuirksMode write FUseQuirksMode;
    property PropStack : THtmlPropStack read FPropStack write FPropStack;
    property Printed: Boolean read FPrinted write SetPrinted;

    property NoBreak : Boolean read FNoBreak write FNoBreak;  {set when in <NoBr>}
    property ViewPort: TRect read GetViewPort;  {ViewPort is the APaintPanel's client rect, except while printing}
    property PageArea: TRect write SetPageArea; {is the ViewPort while printing, set by THtmlViewer.Print}
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;

    {$ifdef has_StyleElements}
    property StyleElements : TStyleElements read FStyleElements write SetStyleElements;
    {$endif}
    property CurrentStyle: TFontStyles read FCurrentStyle write FCurrentStyle;  {as set by <b>, <i>, etc.}
    property CurrentForm : THtmlForm read FCurrentForm write FCurrentForm;
  end;

//------------------------------------------------------------------------------
// some more base sections
//------------------------------------------------------------------------------

  TLineBreak = class(TSectionBase)
  protected
    function TryGetClear(var Clear: ThtClearStyle): Boolean; override;
  end;

  TPage = class(TSectionBase)
  public
    constructor Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties);
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  THorzLine = class(TSectionBase) {a horizontal line, <hr>}
  public
    VSize: Integer;
    Color: TColor;
    Align: ThtJustify;
    UseDefBorder: Boolean;
    NoShade: Boolean;
    BkGnd: Boolean;
    Width, Indent: Integer;

    constructor Create(Parent: TCellBasic; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    procedure CopySelectedText; override;
    function DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer; override;
  end;

  TPreFormated = class(TSection)
//  {section for preformated, <pre>}
//  public
//    procedure ProcessText(TagIndex: Integer); override;
//    function DrawLogic(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
//      var MaxWidth, Curs: Integer): Integer; override;
//    procedure MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer); override;
  end;

var
  WaitStream: TMemoryStream;
  ErrorStream: TMemoryStream;
  WaitBitmap: TBitmap;
  ErrorBitmap: TBitmap;
{$ifdef UNICODE}
{$else}
  UnicodeControls: Boolean;
{$endif}

implementation

uses
{$ifdef UseVCLStyles}
  System.Types,
  System.UITypes,
  Vcl.Themes,
{$endif}
{$if defined(JPM_DEBUGGING)
  or defined(JPM_DEBUGGING_CREATE)
  or defined(JPM_DEBUGGING_LOGIC)
}
  CodeSiteLogging,
{$ifend}
{$IFNDEF NoGDIPlus}
  GDIPL2A,
{$ENDIF}
  HtmlView,
{$ifdef Compiler28_Plus}
 System.NetEncoding,
{$endif}
  Htmlsbs1;

//-- BG ---------------------------------------------------------- 14.01.2012 --
function Sum(const Arr: TIntArray; StartIndex, EndIndex: Integer): Integer; overload;
 {$ifdef UseInline} inline; {$endif}
// Return sum of array elements from StartIndex to EndIndex.
var
  I: Integer;
begin
  Result := 0;
  for I := StartIndex to EndIndex do
    Inc(Result, Arr[I]);
end;

//-- BG ---------------------------------------------------------- 14.01.2012 --
function Sum(const Arr: TIntArray): Integer; overload;
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
function SubArray(const Arr, Minus: TIntArray): TIntArray;
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
procedure SetArray(var Arr: TIntArray; Value, StartIndex, EndIndex: Integer); overload;
 {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  for I := StartIndex to EndIndex do
    Arr[I] := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2012 --
procedure SetArray(var Arr: TIntArray; Value: Integer); overload;
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
  out CountsPerType: TIntegerPerWidthType;
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
  const Widths: TIntArray;
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
  const Widths: TIntArray;
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

{ THtmlNode }

//-- BG ---------------------------------------------------------- 24.03.2011 --
procedure THtmlNode.AfterConstruction;
begin
  inherited AfterConstruction;

  if (Length(Id) > 0) and (Document <> nil) and (Document.IDNameList <> nil) then
    Document.IDNameList.AddObject(Id, Self);
end;

//-- BG ---------------------------------------------------------- 07.09.2013 --
function THtmlNode.CalcDisplayExtern: ThtDisplayStyle;
begin
  case FDisplay of
    pdInlineBlock,
    pdInlineTable:
      Result := pdInline;
  else
    Result := FDisplay;
  end;
end;

//-- BG ---------------------------------------------------------- 07.09.2013 --
function THtmlNode.CalcDisplayIntern: ThtDisplayStyle;
begin
  case FDisplay of
    pdInlineTable:
      Result := pdTable;

    pdInlineBlock:
      Result := pdBlock;
  else
    Result := FDisplay;
  end;
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
constructor THtmlNode.Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties; const ID: ThtString);
var
  MargArrayO: ThtVMarginArray;
  MargArray: ThtMarginArray;
  Cd: ThtConvData;
begin
  inherited Create(ID);
  FOwnerCell := Parent;
  if FOwnerCell <> nil then
  begin
    FOwnerBlock := FOwnerCell.OwnerBlock;
    FDocument := FOwnerCell.Document;
  end;
  if Attributes <> nil then
    FAttributes := Attributes.Clone;
  if Properties <> nil then
    FProperties := Properties.Clone;
// defaults are set automatically as Ord() of these defaults is 0:
//  FDisplay := pdUnassigned;
//  FFloating := aNone;
//  FPositioning := posStatic;
  TryGetClear(FClearing);

  if FProperties <> nil then
  begin
    FDisplay := FProperties.Display;
    FPositioning := FProperties.GetPosition;
    if not FProperties.GetFloat(FFloating) or (FPositioning in [posAbsolute, posFixed]) then
      FFloating := ANone;
    FProperties.GetVMarginArray(MargArrayO);
    FEmSize := FProperties.EmSize;
    FExSize := FProperties.ExSize;
    CD := ConvData(100, 100, FEmSize, FExSize, 0);
    ConvMargProp(piTop   , MargArrayO, CD, MargArray, Document.PixelsPerInch);
    ConvMargProp(piLeft  , MargArrayO, CD, MargArray, Document.PixelsPerInch);
    ConvMargProp(piRight , MargArrayO, CD, MargArray, Document.PixelsPerInch);
    ConvMargProp(piBottom, MargArrayO, CD, MargArray, Document.PixelsPerInch);
    FPositions[reTop   ] := MargArray[piTop   ];
    FPositions[reLeft  ] := MargArray[piLeft  ];
    FPositions[reRight ] := MargArray[piRight ];
    FPositions[reBottom] := MargArray[piBottom];
  end;
end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
constructor THtmlNode.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
begin
  inherited Create( Source.HtmlId );
  FOwnerCell := Parent;
  begin
    FOwnerBlock := FOwnerCell.OwnerBlock;
    FDocument   := FOwnerCell.Document;
  end;
  FAttributes   := Source.FAttributes;
  FProperties   := Source.FProperties;
  FClearing     := Source.FClearing;
  FDisplay      := Source.FDisplay;
  FPositioning  := Source.FPositioning;
  FPositions    := Source.FPositions;
  FFloating     := Source.FFloating;
  FIndent       := Source.FIndent;
end;

//-- BG ---------------------------------------------------------- 21.10.2016 --
destructor THtmlNode.Destroy;
var
  Idx: Integer;
begin
  if (Length(Id) > 0) and (Document <> nil) and (Document.IDNameList <> nil) then
    with Document.IdNameList do
    begin
      Idx := IndexOfObject(Self);
      if Idx >= 0 then
         Delete(Idx);
    end;

  if not IsCopy then
  begin
    FAttributes.Free;
    FProperties.Free;
  end;
  inherited;
end;

//-- BG ---------------------------------------------------------- 24.02.2016 --
function THtmlNode.TryGetClear(var Clear: ThtClearStyle): Boolean;
begin
  Result := False;
  if FProperties <> nil then
    Result := FProperties.GetClear(Clear);
end;

//-- BG ---------------------------------------------------------- 24.02.2016 --
function TSection.TryGetClear(var Clear: ThtClearStyle): Boolean;
var
  T: TAttribute;
begin
  T := nil;
  Result := inherited TryGetClear(Clear);
  if not Result then
    if FAttributes <> nil then
      if FAttributes.Find(ClearSy, T) then
        Result := TryStrToClearStyle(LowerCase(T.Name), Clear);
end;

////-- BG ---------------------------------------------------------- 23.03.2011 --
//function THtmlNode.FindAttribute(NameSy: TAttrSymb; out Attribute: TAttribute): Boolean;
//begin
//  Attribute := nil;
//  Result := (FAttributes <> nil) and FAttributes.Find(NameSy, Attribute);
//end;

//-- BG ---------------------------------------------------------- 24.03.2011 --
function THtmlNode.FindAttribute(const Name: ThtString; out Attribute: TAttribute): Boolean;
begin
  Attribute := nil;
  Result := (FAttributes <> nil) and FAttributes.Find(Name, Attribute);
end;

//-- BG ---------------------------------------------------------- 28.10.2015 --
function THtmlNode.GetAttribute(const Name: ThtString): ThtString;
var
  Attribute: TAttribute;
begin
  if FindAttribute(Name, Attribute) then
    Result := Attribute.Name
  else
    SetLength(Result, 0);
end;

//-- BG ---------------------------------------------------------- 24.02.2016 --
function THtmlNode.GetClearSpace(IMgr: TIndentManager; Y: Integer): Integer;
var
  CL, CR: Integer;
begin
  Result := 0;
  if FClearing <> clrNone then
  begin {may need to move down past floating image}
    IMgr.GetClearY(CL, CR);
    case FClearing of
      clrLeft:   Result := Max(0, CL - Y - 1);
      clrRight:  Result := Max(0, CR - Y - 1);
      clrBoth:   Result := Max(CL - Y - 1, Max(0, CR - Y - 1));
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 14.02.2016 --
function THtmlNode.GetContainingBlock: TBlock;
begin
  // if Result is nil, containing block is the viewport resp. page area
  case FPositioning of
    posFixed:
      Result := nil;

    posAbsolute:
    begin
      Result := OwnerBlock;
      while (Result <> nil) and not (Result.Positioning in [posAbsolute, posRelative, posFixed]) do
        Result := Result.OwnerBlock;
    end;

  else
    Result := OwnerBlock;
  end;
end;

//-- BG ---------------------------------------------------------- 14.02.2016 --
function THtmlNode.GetContainingBox: TRect;
var
  ContainingBlock: TBlock;
begin
  ContainingBlock := GetContainingBlock;
  if ContainingBlock <> nil then
  begin
    Result.Left := ContainingBlock.LeftP;
    Result.Top  := ContainingBlock.TopP;
    Result.Right := Result.Left + ContainingBlock.ContentWidth;
    Result.Bottom := Result.Top + ContainingBlock.MyCell.tcContentBot - ContainingBlock.MyCell.tcDrawTop;
  end
  else
    Result := FDocument.ViewPort;
end;

//-- BG ---------------------------------------------------------- 14.02.2016 --
function THtmlNode.GetProperty(PropIndex: ThtPropIndices): Variant;
begin
  if FProperties <> nil then
    Result := FProperties.Props[PropIndex]
  else
    Result := Unassigned;
end;

////-- BG ---------------------------------------------------------- 24.03.2011 --
//function THtmlNode.GetChild(Index: Integer): THtmlNode;
//begin
//  Result := nil; //TODO -oBG, 24.03.2011
//end;

////-- BG ---------------------------------------------------------- 23.03.2011 --
//function THtmlNode.GetParent: TBlock;
//begin
//  Result := FOwnerBlock;
//end;

//-- BG ---------------------------------------------------------- 04.08.2013 --
function THtmlNode.GetSymbol: TElemSymb;
begin
  Result := FProperties.PropSym;
end;

//-- BG ---------------------------------------------------------- 21.09.2016 --
procedure THtmlNode.GetVMarginArray(var MArray: ThtVMarginArray);
begin
  FProperties.GetVMarginArray(MArray);
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

//-- BG ---------------------------------------------------------- 04.08.2013 --
function THtmlNode.IsCopy: Boolean;
begin
  Result := Document.IsCopy;
end;

//-- BG ---------------------------------------------------------- 13.02.2016 --
function THtmlNode.IsInFlow: Boolean;
begin
  Result := not (FFloating in [aLeft, aRight]) and not (FPositioning in [posAbsolute, posFixed]);
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
//          if htCompareStr(Match.Value, Attribute.AsString) <> 0 then
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

type
  TSectionClass = class of TSectionBase;
  EProcessError = class(Exception);

constructor TFontObj.Create(ASection: TSection; F: ThtFont; Position: Integer);
begin
  inherited Create;
  FSection := ASection;
  TheFont := ThtFont.Create;
  TheFont.Assign(F);
  Pos := Position;
  UrlTarget := TUrlTarget.Create;
  FontChanged;
end;

{$ifndef NoTabLink}

procedure TFontObj.EnterEvent(Sender: TObject);
var
  List: TFontList;
  I, J: Integer;
begin
  Active := True;
{Make adjacent fonts in this link active also}
  List := FSection.Document.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I + 1 to List.Count - 1 do
      if (Self.UrlTarget.ID = List[J].UrlTarget.ID) then
        List[J].Active := True
      else
        Break;
  FSection.Document.ControlEnterEvent(Self);
end;

procedure TFontObj.ExitEvent(Sender: TObject);
var
  List: TFontList;
  I, J: Integer;
begin
  Active := False;
{Make adjacent fonts in this link inactive also}
  List := FSection.Document.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I + 1 to List.Count - 1 do
      if (Self.UrlTarget.ID = List[J].UrlTarget.ID) then
        List[J].Active := False
      else
        Break;
  FSection.Document.PPanel.Invalidate;
end;

procedure TFontObj.AssignY(Y: Integer);
var
  List: TFontList;
  I, J: Integer;
begin
  if UrlTarget.Url = '' then
    Exit;
  if Assigned(TabControl) then
    FYValue := Y
  else
  begin {Look back for the TFontObj with the TabControl}
    List := FSection.Document.LinkList;
    if List <> nil then
    begin
      I := List.IndexOf(Self);
      if I >= 0 then
        for J := I - 1 downto 0 do
          if (Self.UrlTarget.ID = List[J].UrlTarget.ID) then
          begin
            if Assigned(List[J].TabControl) then
            begin
              List[J].FYValue := Y;
              break;
            end;
          end
          else
            Break;
    end;
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
    Key := 0; {Key has been consumed}
  end
  else {send other keys to THtmlViewer}
    Viewer.KeyDown(Key, Shift);
end;

procedure TFontObj.CreateTabControl(TabIndex: Integer);
var
  PntPanel: TWinControl; //TPaintPanel;
  I, J: Integer;
  List: TFontList;
begin
  if Assigned(TabControl) then
    Exit;
  {Look back for the TFontObj with the TabControl}
  List := FSection.Document.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I - 1 downto 0 do
      if (Self.UrlTarget.ID = List[J].UrlTarget.ID) then
        if Assigned(List[J].TabControl) then
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
  TheFont.Assign(F);
  FontChanged;
end;

procedure TFontObj.ConvertFont(const FI: ThtFontInfo);
begin
  TheFont.Assign(FI, FSection.Document.PixelsPerInch);
  FontChanged;
end;

constructor TFontObj.CreateCopy(ASection: TSection; T: TFontObj);
begin
  inherited Create;
{$ifndef NoTabLink}
  FSection := ASection;
{$endif}
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
{$ifndef NoTabLink}
var Index: Integer;
{$endif}
begin
  FIArray.Free;
  TheFont.Free;
  UrlTarget.Free;
{$ifndef NoTabLink}
  if FSection <> nil then
    if FSection.Document <> nil then
      if FSection.Document.TabOrderList <> nil then
        with FSection.Document.TabOrderList do
        begin
          Index := IndexOfObject(TabControl);
          if Index >= 0 then
            Delete(Index);
        end;
{$endif}
  TabControl.Free;
  inherited Destroy;
end;

procedure TFontObj.SetVisited(Value: Boolean);
begin
  if Value <> FVisited then
  begin
    FVisited := Value;
    ConvertFont(FIArray.Ar[FontInfoIndex]);
    FontChanged;
  end;
end;

procedure TFontObj.SetHover(Value: Boolean);
begin
  if Value <> FHover then
  begin
    FHover := Value;
    ConvertFont(FIArray.Ar[FontInfoIndex]);
    FontChanged;
  end;
end;

procedure TFontObj.SetAllHovers(List: TFontList; Value: Boolean);
{Set/Reset Hover on this item and all adjacent item with the same URL}
var
  I, J: Integer;
begin
  SetHover(Value);
  I := List.IndexOf(Self);
  if I >= 0 then
  begin
    J := I + 1;
    while (J < List.Count) and (Self.UrlTarget.ID = List[J].UrlTarget.ID) do
    begin
      List[J].Hover := Value;
      Inc(J);
    end;
    J := I - 1;
    while (J >= 0) and (Self.UrlTarget.ID = List[J].UrlTarget.ID) do
    begin
      List[J].Hover := Value;
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
  inherited Create;
  for I := 0 to T.Count - 1 do
    Add(TFontObj.CreateCopy(ASection, T.Items[I]));
end;

//-- BG ---------------------------------------------------------- 10.02.2013 --
function TFontList.GetFont(Index: Integer): TFontObj;
begin
  Result := TFontObj(inherited Items[Index]);
end;

function TFontList.GetFontAt(Posn: Integer; out OHang: Integer): ThtFont;
{given a character index, find the font that's effective there}
var
  I, PosX: Integer;
  F: TFontObj;
begin
  I := 0;
  PosX := 0;
  while (I < Count) do
  begin
    PosX := Items[I].Pos;
    Inc(I);
    if PosX >= Posn then
      Break;
  end;
  Dec(I);
  if PosX > Posn then
    Dec(I);
  F := Items[I];
  OHang := F.Overhang;
  Result := F.TheFont;
end;

//-- BG ---------------------------------------------------------- 25.08.2013 --
function TFontList.GetFontObjAt(Posn, Leng: Integer; out Obj: TFontObj): Integer;
{Given a position, returns the FontObj which applies there and the number of chars before the font changes}
var
  I: Integer;
begin
  I := Count;
  while I > 0 do
  begin
    Dec(I);
    Obj := Items[I];
    if Obj.Pos <= Posn then
    begin
      Result := Leng - Posn;
      Exit;
    end;
    Leng := Obj.Pos;
  end;
  Obj := nil;
  Result := Leng - Posn;
end;

{----------------TFontList.GetFontObjAt}

function TFontList.GetFontObjAt(Posn: Integer): TFontObj;
{Given a position, returns the FontObj which applies there}
var
  I: Integer;
begin
  I := Count;
  while I > 0 do
  begin
    Dec(I);
    Result := Items[I];
    if Result.Pos <= Posn then
      Exit;
  end;
  Result := nil;
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
    FO := Items[I];
    if FO.Pos > N then
      Dec(FO.Pos);
    if (I > 0) and (Items[I - 1].Pos = FO.Pos) then
    begin
      FO1 := Items[I - 1];
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

{ TLinkList }

//-- BG ---------------------------------------------------------- 10.02.2013 --
constructor TLinkList.Create;
begin
  inherited Create(False);
end;

{ TImageObj.Create }

constructor TImageObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  I: Integer;
  S: ThtString;
  T: TAttribute;
begin
  inherited Create(Parent,Position,L,Prop);
  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        SrcSy:
          FSource := htTrim(Name);

        AltSy:
          begin
            SetAlt(CodePage, Name);
            Title := Alt;
          end;

        BorderSy:
          begin
            BorderWidth := Min(Max(0, Value), 10);
//            if not Prop.HasBorderWidth then
//              Prop.BorderWidth := Min(Max(0, Value), 10);
//            if not Prop.HasBorderStyle then
//              Prop.BorderStyle := bssSolid;
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

        TranspSy:
          Transparent := itrLLCorner;

        ActiveSy:
          FHoverImage := True;

        NameSy:
          Document.IDNameList.AddObject(Name, Self);
      end;

  NoBorder := (BorderWidth = 0) and (not Prop.HasBorderStyle or not Prop.HasBorderWidth);
  T := nil;
  if L.Find(TitleSy, T) then
    Title := T.Name; {has higher priority than Alt loaded above}
end;

constructor TImageObj.SimpleCreate(Parent: TCellBasic; const AnURL: ThtString);
begin
  inherited SimpleCreate(Parent);
  FSource := AnURL;
end;

constructor TImageObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TImageObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  AltHeight := T.AltHeight;
  AltWidth := T.AltWidth;
  FHover := T.FHover;
  FHoverImage := T.FHoverImage;
  FImage := T.Image;
  FSource := T.FSource;
  IsMap := T.IsMap;
  MapName := T.MapName;
  Missing := T.Missing;
  ObjHeight := T.ObjHeight;
  ObjWidth := T.ObjWidth;
  OrigImage := T.OrigImage;
  Swapped := T.Swapped;
  Transparent := T.Transparent;
  UseMap := T.UseMap;
end;

destructor TImageObj.Destroy;
var
  I: Integer;
begin
  if not IsCopy then
  begin
    I := Document.IDNameList.IndexOfObject(Self);
    if I >= 0 then
      Document.IDNameList.Delete(I);
    if (Source <> '') and Assigned(OrigImage) then
      Document.ImageCache.DecUsage(htUpperCase(htTrim(Source)));
    if (Image is ThtGifImage) and ThtGifImage(Image).Gif.IsCopy then
      Image.Free;
  end;
  inherited Destroy;
end;

function TImageObj.GetGraphic: TGraphic;
begin
  if (Image = nil) or (Image = ErrorImage) then
    Result := nil
  else
    Result := Image.Graphic;
end;

procedure TImageObj.SetHover(Value: ThtHover);
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
            Document.AniList.Add(Image);
          end;
        end
      else
      begin
        Animate := False;
        if NumFrames <= 3 then
          CurrentFrame := 1;
        Document.AniList.Remove(Image);
      end;
      FHover := Value;
      Document.PPanel.Invalidate;
    end;
end;

{----------------TImageObj.ReplaceImage}

procedure TImageObj.ReplaceImage(NewImage: TStream);
var
  TmpImage: ThtImage;
  I: Integer;
begin
  try
    Transparent := itrNone;
    TmpImage := LoadImageFromStream(NewImage, Transparent);
    if Assigned(TmpImage) then
    begin
      // remove current image
      Document.AniList.Remove(FImage);
      if not Swapped then
      begin
        {OrigImage is left in cache and kept}
        Swapped := True;
      end
      else {swapped already}
      begin
        FImage.Free;
      end;

      // set new image
      FImage := TmpImage;
      if FImage.IsAnimated then
      begin
        if not FHoverImage then
        begin
          FImage.Animate := True;
          Document.AniList.Add(FImage);
        end
        else
        begin
          FImage.Animate := False;
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
  except
    // replacing image failed
  end;
end;

{----------------TImageObj.InsertImage}

function TImageObj.InsertImage(const UName: ThtString; Error: Boolean; out Reformat: Boolean): Boolean;
var
  TmpImage: ThtImage;
  FromCache, DelayDummy: Boolean;
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
      TmpImage := Document.GetTheImage(UName, Transparent, FromCache, DelayDummy);
      if TmpImage = nil then
        Exit;

      if TmpImage.IsAnimated then
      begin
        if FromCache then {it would be}
          FImage := TmpImage.Clone {it's in Cache already, make copy}
        else
          FImage := TmpImage;
        if not FHoverImage then
        begin
          FImage.Animate := True;
          Document.AniList.Add(FImage);
          if Assigned(Document.Timer) then
            Document.Timer.Enabled := True;
        end
        else
          FImage.Animate := False;
      end
      else
        FImage := TmpImage;
      OrigImage := TmpImage;
    end;
    Missing := False;

    if not ClientSizeKnown then
      Reformat := True; {need to get the dimensions}
  end;
end;

{----------------TImageObj.DrawLogic}

procedure TImageObj.DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer);
{calculate the height and width}
var
  TmpImage: ThtImage;
  ViewImages, FromCache: Boolean;
  Rslt: ThtString;
  ARect: TRect;
  SubstImage: Boolean;
  HasBlueBox: Boolean;
  UName: ThtString;
begin
  ViewImages := Document.ShowImages;
  case FDisplay of
    pdNone:
    begin
      ObjHeight := 0;
      ObjWidth := 0;

      ClientHeight := ObjHeight;
      ClientWidth := ObjWidth;
      Exit;
    end;
  end;

  if ViewImages then
  begin
    if FImage = nil then
    begin
      TmpImage := nil;
      UName := htTrim(FSource);
      if UName <> '' then
      begin
        if not Assigned(Document.GetBitmap) and not Assigned(Document.GetImage) then
          FSource := htTrim(Document.TheOwner.HtmlExpandFilename(FSource))
        else if Assigned(Document.OnExpandName) then
        begin
          Document.OnExpandName(Document.TheOwner, FSource, Rslt);
          FSource := htTrim(Rslt);
        end;
        if Document.MissingImages.IndexOf(FSource) = -1 then
          TmpImage := Document.GetTheImage(FSource, Transparent, FromCache, Missing)
        else
          Missing := True; {already in list, don't request it again}
      end;

      if TmpImage = nil then
      begin
        if Missing then
        begin
          FImage := DefImage;
          Document.MissingImages.AddObject(FSource, Self); {add it even if it's there already}
        end
        else
        begin
          FImage := ErrorImage;
        end;
      end
      else if TmpImage.IsAnimated then
      begin
        if FromCache then
          FImage := TmpImage.Clone {it's in Cache already, make copy}
        else
          FImage := TmpImage;
        if not FHoverImage then
        begin
          FImage.Animate := True;
          Document.AniList.Add(FImage);
          if Assigned(Document.Timer) then
            Document.Timer.Enabled := True;
        end
        else
          FImage.Animate := False;
      end
      else
        FImage := TmpImage;
      OrigImage := TmpImage;
    end;
  end
  else
    FImage := DefImage;

  SubstImage := (Image = ErrorImage) or (Image = DefImage);

  HasBlueBox := not NoBorder and Assigned(FO) and (FO.URLTarget.Url <> '');
  if HasBlueBox then
    BorderWidth := Max(1, BorderWidth);

  if not ClientSizeKnown or PercentWidth or PercentHeight then
  begin
    CalcSize(AvailableWidth, AvailableHeight, Image.Width, Image.Height, not SubstImage);
    ObjWidth  := ClientWidth; // - MargArray[BorderLeftWidth] - MargArray[BorderRightWidth];
    ObjHeight := ClientHeight;// - MargArray[BorderTopWidth] - MargArray[BorderBottomWidth];
  end;

  if not ViewImages or SubstImage then
  begin
    if ClientSizeKnown or (not SubstImage and ((SpecWidth >= 0) or (SpecHeight >= 0))) then
    begin {size to whatever is specified}
      AltWidth := ObjWidth;
      AltHeight := ObjHeight;
    end
    else
    begin
      if FAlt <> '' then {Alt text and no size specified, take as much space as necessary}
      begin
        Canvas.Font.Name := FontSans; {use same font as in Draw}
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
      ClientHeight := AltHeight;
      ClientWidth  := AltWidth;
    end;
  end;


  DrawRect.Left := DrawXX;
  DrawRect.Top := DrawYY;
  DrawRect.Right := DrawRect.Left + ClientWidth;
  DrawRect.Bottom := DrawRect.Top + ClientHeight;
end;

{----------------TImageObj.DoDraw}
//var
//  LastExceptionMessage: String;
//  LastDdImage: ThtImage;
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
    // Yunqa.de removed: ddImage.Transp := Transparent;
    if (W > 0) and (H > 0) then
      if IsCopy then
        ddImage.Print(Canvas, XX, Y, W, H, clWhite)
      else
        ddImage.Draw(Canvas, XX, Y, W, H);
  except
    //on E: Exception do
    //begin
    //  LastExceptionMessage := E.Message;
    //  LastDdImage := ddImage;
    //end;
  end;
end;

{----------------TImageObj.Draw}

//-- BG ---------------------------------------------------------- 12.06.2010 --
procedure GetRaisedColors(SectionList: ThtDocument; Canvas: TCanvas; out Light, Dark: TColor);
var
  White, BlackBorder: Boolean;
begin
  BlackBorder := SectionList.Printing and SectionList.PrintMonoBlack and
    (GetDeviceCaps(Canvas.Handle, BITSPIXEL) = 1) and (GetDeviceCaps(Canvas.Handle, PLANES) = 1);
  if BlackBorder then
  begin
    Light := clBlack;
    Dark := clBlack;
  end
  else
  begin
    White := SectionList.Printing or (SectionList.ThemedColorToRGB( SectionList.Background, htseFont) = clWhite);
    Dark := SectionList.ThemedColorToRGB(clBtnShadow, htseFont);
    if White then
      Light := clSilver
    else
      Light := SectionList.ThemedColorToRGB(clBtnHighLight, htseFont);
  end;
end;

//BG, 15.10.2010: issue 28: Borland C++ Builder does not accept an array as a result of a function.
// Thus move htStyles and htColors from HtmlUn2.pas to HtmlSubs.pas the only unit where they are used

function htStyles(P0, P1, P2, P3: ThtBorderStyle): ThtBorderStyleArray;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[0] := P0;
  Result[1] := P1;
  Result[2] := P2;
  Result[3] := P3;
end;

function htColors(C0, C1, C2, C3: TColor): ThtColorArray;
 {$ifdef UseInline} inline; {$endif}
begin
  Result[0] := C0;
  Result[1] := C1;
  Result[2] := C2;
  Result[3] := C3;
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
function htRaisedColors(Light, Dark: TColor; Raised: Boolean): ThtColorArray; overload;
  {$ifdef UseInline} inline; {$endif}
begin
  if Raised then
    Result := htColors(Light, Light, Dark, Dark)
  else
    Result := htColors(Dark, Dark, Light, Light);
end;

//-- BG ---------------------------------------------------------- 12.06.2010 --
function htRaisedColors(SectionList: ThtDocument; Canvas: TCanvas; Raised: Boolean): ThtColorArray; overload;
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
  const Colors: ThtColorArray;
  Styles: ThtBorderStyleArray;
  ThemedColorToRGB: ThtThemedColor
); overload;
{$ifdef UseInline} inline; {$endif}
{Draws colored raised or lowered rectangles for table borders}
begin
  DrawBorder(Canvas, ORect, IRect, Colors, Styles, clNone, False, ThemedColorToRGB);
end;

procedure RaisedRect(SectionList: ThtDocument; Canvas: TCanvas;
  X1, Y1, X2, Y2: Integer;
  Raised: Boolean;
  W: Integer);
  {$ifdef UseInline} inline; {$endif}
{Draws raised or lowered rectangles for table borders}
begin
  RaisedRectColor(Canvas,
    Rect(X1, Y1, X2, Y2),
    Rect(X1 + W, Y1 + W, X2 - W, Y2 - W),
    htRaisedColors(SectionList, Canvas, Raised),
    htStyles(bssSolid, bssSolid, bssSolid, bssSolid),
    SectionList.ThemedColorToRGB
  );
end;

procedure TImageObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
var
  TmpImage: ThtImage;
  MiddleAlignTop: Integer;
  ViewImages: Boolean;
  SubstImage: Boolean;
  Ofst: Integer;
  SaveColor: TColor;
  ARect: TRect;
//  SaveWidth: Integer;
//  SaveStyle: TPenStyle;
  YY: Integer;
//  ORect, IRect: TRect;
//{$ifdef has_StyleElements}
//  LStyle : TStyleElements;
//{$endif}
begin
  ViewImages := Document.ShowImages;
  Dec(Y, Document.YOff);
  Dec(YBaseLine, Document.YOff);
//{$ifdef has_StyleElements}
//  LStyle := Document.StyleElements;
//{$endif}

  if ViewImages then
    TmpImage := Image
  else
    TmpImage := DefImage;
  SubstImage := not ViewImages or (TmpImage = ErrorImage) or (TmpImage = DefImage); {substitute image}

  with Canvas do
  begin
    Brush.Style := bsClear;
    Font.Size := 8;
    Font.Name := FontSans; {make this a property?}
    Font.Style := Font.Style - [fsBold];
  end;

  if SubstImage then
    Ofst := 4
  else
    Ofst := 0;

  if VertAlign = AMiddle then
    MiddleAlignTop := YBaseLine + FO.Descent - (FO.tmHeight div 2) - ((ClientHeight - VSpaceT + VSpaceB) div 2)
  else
    MiddleAlignTop := 0; {not used}

  DrawXX := X;
  DrawYY := Y;
  if Floating = ANone then
  begin
    case Positioning of
      posAbsolute:
      begin
        ARect := ContainingBox;
        if not IsAuto(Positions[reLeft]) then DrawXX := Positions[reLeft] + ARect.Left;
        if not IsAuto(Positions[reTop ]) then DrawYY := Positions[reTop ] + ARect.Top;
      end;

      posFixed:
      begin
        ARect := FDocument.ViewPort;
        if not IsAuto(Positions[reLeft]) then DrawXX := Positions[reLeft] + ARect.Left;
        if not IsAuto(Positions[reTop ]) then DrawYY := Positions[reTop ] + ARect.Top;
      end;

    else
      case VertAlign of
        ATop, ANone:
          DrawYY := Y + VSpaceT;
        AMiddle:
          DrawYY := MiddleAlignTop;
        ABottom, ABaseline:
          DrawYY := YBaseLine - ClientHeight - VSpaceB;
      end;

      if Positioning = posRelative then
      begin
        if not IsAuto(Positions[reLeft]) then Inc(DrawXX, Positions[reLeft]);
        if not IsAuto(Positions[reTop ]) then Inc(DrawYY, Positions[reTop ]);
      end;

    end;
//    if MargArray[BorderLeftWidth] > 0 then
//      Inc(DrawXX, MargArray[BorderLeftWidth]);
//    if MargArray[BorderTopWidth] > 0 then
//      Inc(DrawYY, MargArray[BorderTopWidth]);
  end;

  if not SubstImage or ((AltHeight >= 16 + 8) and (AltWidth >= 16 + 8)) then
    Self.DoDraw(Canvas, DrawXX + Ofst, DrawYY + Ofst, TmpImage);
  Inc(DrawYY, Document.YOff);

  DrawRect.Left := DrawXX;
  DrawRect.Top := DrawYY;
  DrawRect.Right := DrawRect.Left + ClientWidth;
  DrawRect.Bottom := DrawRect.Top + ClientHeight;

  SetTextAlign(Canvas.Handle, TA_Top);
  if SubstImage and (BorderWidth = 0) then
  begin
    Canvas.Font.Color := Document.ThemedColorToRGB(FO.TheFont.Color, htseFont);
  {calc the offset from the image's base to the alt= text baseline}
    case VertAlign of
      ATop, ANone:
        begin
          if FAlt <> '' then
            WrapTextW(         Canvas, X + 24, Y + Ofst                              , X + AltWidth - 2, Y + AltHeight - 1, FAlt);
          RaisedRect(Document, Canvas, X     , Y                                     , X + AltWidth    , Y + AltHeight    , False, 1);
        end;
      AMiddle:
        begin {MiddleAlignTop is always initialized}
          if FAlt <> '' then
            WrapTextW(         Canvas, X + 24, MiddleAlignTop + Ofst                 , X + AltWidth - 2, MiddleAlignTop + AltHeight - 1, FAlt);
          RaisedRect(Document, Canvas, X     , MiddleAlignTop                        , X + AltWidth    , MiddleAlignTop + AltHeight    , False, 1);
        end;
      ABottom, ABaseline:
        begin
          if FAlt <> '' then
            WrapTextW(         Canvas, X + 24, YBaseLine - AltHeight + Ofst - VSpaceB, X + AltWidth - 2, YBaseLine - VSpaceB - 1, FAlt);
          RaisedRect(Document, Canvas, X     , YBaseLine - AltHeight        - VSpaceB, X + AltWidth    , YBaseLine - VSpaceB    , False, 1);
        end;
    end;
  end;

  if (MargArray[BorderTopWidth] > 0) or (MargArray[BorderRightWidth] > 0) or (MargArray[BorderBottomWidth] > 0) or (MargArray[BorderLeftWidth] > 0) then
    with Canvas do
    begin
//      SaveColor := Pen.Color;
//      SaveWidth := Pen.Width;
//      SaveStyle := Pen.Style;
      Font.Color := Document.ThemedColorToRGB(FO.TheFont.Color, htseFont);
//      Pen.Color := Font.Color;
//      Pen.Width := BorderSize;
//      Pen.Style := psInsideFrame;
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

//        {draw border}
//        ORect := Rect(DrawXX, DrawYY, X + ClientWidth, DrawYY + ClientHeight);
//        IRect.Left   := ORect.Left   + MargArray[BorderLeftWidth  ];
//        IRect.Top    := ORect.Top    + MargArray[BorderTopWidth   ];
//        IRect.Right  := ORect.Right  - MargArray[BorderRightWidth ];
//        IRect.Bottom := ORect.Bottom - MargArray[BorderBottomWidth];
//        DrawBorder(Canvas, ORect, IRect,
//          htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
//          htStyles(ThtBorderStyle(MargArray[BorderLeftStyle]), ThtBorderStyle(MargArray[BorderTopStyle]), ThtBorderStyle(MargArray[BorderRightStyle]), ThtBorderStyle(MargArray[BorderBottomStyle])),
//          MargArray[BackgroundColor], Document.Printing{$ifdef has_StyleElements}, Document.StyleElements {$endif})
      finally
//        Pen.Color := SaveColor;
//        Pen.Width := SaveWidth;
//        Pen.Style := SaveStyle;
      end;
    end;

  if (Assigned(MyFormControl) and MyFormControl.Active or FO.Active) or
    IsCopy and Assigned(Document.LinkDrawnEvent) and (FO.UrlTarget.Url <> '')
  then
    with Canvas do
    begin
      SaveColor := SetTextColor(Handle, clBlack);
      Brush.Color := clWhite;
      case VertAlign of
        ATop, ANone:
          ARect := Rect(X, Y + VSpaceT, X + ClientWidth, Y + VSpaceT + ClientHeight);
        AMiddle:
          ARect := Rect(X, MiddleAlignTop, X + ClientWidth, MiddleAlignTop + ClientHeight);
        ABottom, ABaseline:
          ARect := Rect(X, YBaseLine - ClientHeight - VSpaceB, X + ClientWidth, YBaseLine - VSpaceB);
      end;
      if not IsCopy then
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

{----------------THtmlForm.Create}

constructor THtmlForm.Create(AMasterList: ThtDocument; L: TAttributeList);
var
  I: Integer;
begin
  inherited Create;
  Document := AMasterList;
  AMasterList.HtmlFormList.Add(Self);
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

destructor THtmlForm.Destroy;
begin
  ControlList.Free;
  inherited Destroy;
end;

procedure THtmlForm.InsertControl(Ctrl: TFormControlObj);
begin
  ControlList.Add(Ctrl);
  if not (Ctrl is THiddenFormControlObj) then
    Inc(NonHiddenCount);
end;

procedure THtmlForm.DoRadios(Radio: TRadioButtonFormControlObj);
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
        if htCompareText(RadioButton.FName, S) = 0 then
        begin
          RadioButton.Checked := False;
          RadioButton.TabStop := False; {first check turns off other tabstops}
          RadioButton.DoOnChange;
        end;
    end;
  end;
end;

procedure THtmlForm.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
        if (Ctrl is TRadioButtonFormControlObj) and htSameText(Ctrl.FName, S) then
        begin
          if B then
          begin
            with Ctrl as TRadioButtonFormControlObj do
            begin
              TheControl.SetFocus;
              Checked := True;
            end;
            Break;
          end;
          if Ctrl.TheControl = Sender then
            B := True
        end;
      end
    else
      for I := 0 to ControlList.Count - 1 do
      begin
        Ctrl := ControlList[I];
        if (Ctrl is TRadioButtonFormControlObj) and htSameText(Ctrl.FName, S) then
        begin
          if B then
          begin
            with Ctrl as TRadioButtonFormControlObj do
            begin
              TheControl.SetFocus;
              Checked := True;
            end;
            Break;
          end;
          if Ctrl.TheControl = Sender then
            B := True
        end;
      end;
    Key := 0; {Key has been consumed}
  end
  else {send other keys to THtmlViewer}
    Document.TheOwner.KeyDown(Key, Shift);
end;

procedure THtmlForm.ResetControls;
var
  I: Integer;
begin
  for I := 0 to ControlList.Count - 1 do
    TFormControlObj(ControlList.Items[I]).ResetToValue;
end;

procedure THtmlForm.ControlKeyPress(Sender: TObject; var Key: Char);
begin
  if (Sender is ThtEdit) then
    if (Key = #13) then
    begin
      SubmitTheForm('');
      Key := #0;
    end;
end;

function THtmlForm.GetFormSubmission: ThtStringList;
var
  I, J: Integer;
  S: ThtString;
begin
  Result := ThtStringList.Create;
  for I := 0 to ControlList.Count - 1 do
    with ControlList[I] do
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

procedure THtmlForm.SubmitTheForm(const ButtonSubmission: ThtString);
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

procedure THtmlForm.SetFormData(SL: ThtStringList);
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
      if htCompareText(FormControl.FName, SL.Names[J]) = 0 then
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

procedure THtmlForm.SetSizes(Canvas: TCanvas);
var
  I: Integer;
begin
  for I := 0 to ControlList.Count - 1 do
    TFormControlObj(ControlList.Items[I]).SetHeightWidth(Canvas);
end;

{----------------TFormControlObj.Create}

constructor TFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  I: Integer;
begin
  inherited Create(Parent,Position,L,Prop);
  StartCurs := Position;
  if not Assigned(Document.CurrentForm) then {maybe someone forgot the <form> tag}
    Document.CurrentForm := THtmlForm.Create(Document, nil);
  Document.FormControlList.Add(Self);
  MyForm := Document.CurrentForm;
  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        ValueSy:    Self.Value := Name;
        NameSy:     Self.FName := Name;
        IDSy:       FID := Name;
        OnClickSy:  OnClickMessage := Name;
        OnFocusSy:  OnFocusMessage := Name;
        OnBlurSy:   OnBlurMessage := Name;
        OnChangeSy: OnChangeMessage := Name;
        TitleSy:    FTitle := Name;
        DisabledSy: Disabled := (Lowercase(Name) <> 'no') and (Name <> '0');
        ReadonlySy: ReadOnly := True;

        TabIndexSy:
          if Value > 0 then
            {Adding leading 0's to the number ThtString allows it to be sorted numerically,
             and the Count takes care of duplicates}
            with Document.TabOrderList do
              AddObject(Format('%.5d%.3d', [Value, Count]), Self);

      end;

  //FAttributeList := L.CreateStringList;
  VertAlign := ABottom; {ABaseline set individually}
  MyForm.InsertControl(Self);
end;

constructor TFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  System.Move(T.MyForm, MyForm, PtrSub(@ShowIt, @MyForm));
  FId := T.FID;
  FName := T.FName;
end;

destructor TFormControlObj.Destroy;
begin
  //FAttributeList.Free;
  PaintBitmap.Free;
  inherited Destroy;
end;

procedure TFormControlObj.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TFormControlObj.ProcessProperties(Prop: TProperties);
var
  MargArrayO: ThtVMarginArray;
  MargArray: ThtMarginArray;
begin
  Prop.GetVMarginArray(MargArrayO);
  EmSize := Prop.EmSize;
  ExSize := Prop.ExSize;
  PercentWidth := (VarIsStr(MargArrayO[piWidth])) and (System.Pos('%', MargArrayO[piWidth]) > 0);
  ConvInlineMargArray(MargArrayO, 100, 200, EmSize, ExSize, 4, MargArray, Document.PixelsPerInch);

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
  Prop.GetVertAlign(VertAlign);
  Prop.GetFloat(FFloating);

  BkColor := Prop.GetBackgroundColor;
end;

procedure TFormControlObj.EnterEvent(Sender: TObject);
{Once form control entered, insure all form controls are tab active}
begin
  if IsCopy then
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

procedure TFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: Integer);
begin
  if not IsCopy then
  begin
    Show;
    Left := X1;
    Top := Y1;
  end;
end;

//-- BG ---------------------------------------------------------- 16.09.2013 --
procedure TFormControlObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
begin
  DrawInline1(Canvas, X, Y);
end;

//-- BG ---------------------------------------------------------- 28.08.2013 --
procedure TFormControlObj.DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer);
begin
//  inherited DrawLogicInline(Canvas,FO,AvailableWidth,AvailableHeight);
  if PercentWidth then
    ClientWidth := Max(10, Min(MulDiv(FWidth, AvailableWidth, 100), AvailableWidth - HSpaceL - HSpaceR));
  if PercentHeight then
    ClientHeight := Max(10, Min(MulDiv(FWidth, AvailableHeight, 100), AvailableHeight - VSpaceT - VSpaceB));
end;

procedure TFormControlObj.ResetToValue;
begin
end;

function TFormControlObj.GetSubmission(Index: Integer; out S: ThtString): Boolean;
begin
  Result := False;
end;

procedure TFormControlObj.FormControlClick(Sender: TObject);
begin
  if Assigned(Document.ObjectClick) then
    Document.ObjectClick(Document.TheOwner, Self, OnClickMessage);
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetClientHeight: Integer;
begin
  Result := TheControl.Height;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetClientLeft: Integer;
begin
  Result := TheControl.Left;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TFormControlObj.GetTabOrder: Integer;
begin
  Result := TheControl.TabOrder;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TFormControlObj.GetTabStop: Boolean;
begin
  Result := TheControl.TabStop;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetClientTop: Integer;
begin
  Result := TheControl.Top;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TFormControlObj.GetClientWidth: Integer;
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
procedure TFormControlObj.SetClientHeight(Value: Integer);
begin
  TheControl.Height := Value;
end;

procedure TFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.SetClientLeft(Value: Integer);
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
procedure TFormControlObj.SetClientTop(Value: Integer);
begin
  TheControl.Top := Value;
end;

{----------------TImageFormControlObj.Create}

constructor TImageFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(Parent,Position,L,Prop);
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
    {$ifdef has_StyleElements}
    StyleElements := Document.StyleElements;
    {$endif}
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
constructor TImageFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TImageFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  MyImage := T.MyImage;
end;

destructor TImageFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
    // TODO: BG, 29.08.2013: ... and MyImage??? Who owns it???
  end;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TImageFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TImageFormControlObj.GetSubmission(Index: Integer; out S: ThtString): Boolean;
begin
  Result := (Index <= 1) and (XPos >= 0);
  if Result then
  begin
    S := '';
    if FName <> '' then
      S := FName + '.';
    if Index = 0 then
      S := S + 'x=' + htString(IntToStr(XPos))
    else
    begin {index = 1}
      S := S + 'y=' + htString(IntToStr(YPos));
      XPos := -1;
    end;
  end;
end;

{----------------TRadioButtonFormControlObj.Create}

constructor TRadioButtonFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; // TPaintPanel;
  Ctrl: TFormControlObj;
  RadioButtonFormControlObj: TRadioButtonFormControlObj absolute Ctrl;
  I: Integer;
  SetTabStop: Boolean;
begin
  //TODO -oBG, 24.03.2011: what is ACell? Child or Parent?
  // I think it is parent and is used to address the co-radiobuttons
  inherited Create(Parent,Position,L,Prop);
  //xMyCell := ACell;
  PntPanel := Document.PPanel;
  FPanel := TFormRadioPanel.Create(PntPanel);
  FPanel.Parent := PntPanel;
  FControl := TFormRadioButton.Create(PntPanel);
  VertAlign := ABaseline;
  T := nil;
  if L.Find(CheckedSy, T) then
    IsChecked := True;
  with FControl do
  begin
    FPanel.Left := -4000; {so will be invisible until placed}
    FControl.Left := 0;
    if FDocument.PixelsPerInch > 100 then
    begin
      FPanel.Width := 16;
      FPanel.Height := 16;
    end
    else
    begin
      FPanel.Width := 13;
      FPanel.Height := 14;
    end;
    FControl.Align := alClient;
    IDName := Self.FName;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnKeyDown := MyForm.AKeyDown;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
    Parent := FPanel; {must precede Checked assignment}

  {The Tabstop for the first radiobutton in a group will be set in case no
   radiobuttons that follow are checked.  This insures that the tab key can
   access the group}
    SetTabStop := True;
  {Examine all other radiobuttons in this group (same FName)}
    for I := 0 to MyForm.ControlList.Count - 1 do
    begin
      Ctrl := TFormControlObj(MyForm.ControlList.Items[I]);
      if (Ctrl is TRadioButtonFormControlObj) and (RadioButtonFormControlObj.FControl <> FControl) then {skip the current radiobutton}
        if htCompareText(Ctrl.Name, htString(Name)) = 0 then {same group}
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
    {$ifdef has_StyleElements}
    StyleElements := Document.StyleElements;
    {$endif}
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

procedure TRadioButtonFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: Integer);
var
  OldStyle: TPenStyle;
  OldWidth, XW, YH, XC, YC: Integer;
  OldColor, OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  MonoBlack: Boolean;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
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
        Brush.Color := Document.ThemedColorToRGB(clBtnFace, htseClient)
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
        Pen.Color := Document.ThemedColorToRGB(clBtnShadow, htseClient);
      end;
      Arc(X1, Y1, XW, YH, XW, Y1, X1, YH);
      if not MonoBlack then
        Pen.Color := Document.ThemedColorToRGB(clBtnHighlight, htseClient);
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
    end
  else
  begin
    if OwnerCell.BkGnd then
      Color := OwnerCell.BkColor
    else
      Color := Document.Background;
    if Active and Document.TheOwner.ShowFocusRect then //MK20091107
    begin
      Canvas.Brush.Color := clWhite;
      if Document.PixelsPerInch > 100 then
        Canvas.DrawFocusRect(Rect(Left - 2, Top - 2, Left + 18, Top + 18))
      else
        Canvas.DrawFocusRect(Rect(Left - 3, Top - 2, Left + 16, Top + 16));
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TRadioButtonFormControlObj.GetChecked: Boolean;
begin
  Result := FControl.Checked;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
function TRadioButtonFormControlObj.GetClientLeft: Integer;
begin
  Result := FPanel.Left;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
function TRadioButtonFormControlObj.GetClientTop: Integer;
begin
  Result := FPanel.Top;
end;

function TRadioButtonFormControlObj.GetSubmission(Index: Integer; out S: ThtString): Boolean;
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

//-- BG ---------------------------------------------------------- 29.04.2019 --
procedure TRadioButtonFormControlObj.SetClientHeight(Value: Integer);
begin
  inherited;
  FPanel.Height := Value;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
procedure TRadioButtonFormControlObj.SetClientLeft(Value: Integer);
begin
  FPanel.Left := Value;
  FControl.Left := 0;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
procedure TRadioButtonFormControlObj.SetClientTop(Value: Integer);
begin
  FPanel.Top := Value;
  FControl.Top := 0;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
procedure TRadioButtonFormControlObj.SetClientWidth(Value: Integer);
begin
  inherited;
  FPanel.Width := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TRadioButtonFormControlObj.SetColor(const Value: TColor);
begin
  FPanel.Color := Value;
  FControl.Color := Value;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
procedure TRadioButtonFormControlObj.SetTabOrder(Value: Integer);
begin
  FPanel.TabOrder := Value;
end;

//-- BG ---------------------------------------------------------- 29.04.2019 --
procedure TRadioButtonFormControlObj.SetTabStop(Value: Boolean);
begin
  inherited;
  FPanel.TabStop := Value;
end;

//-- BG ---------------------------------------------------------- 30.08.2013 --
constructor TRadioButtonFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TRadioButtonFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  WasChecked := T.WasChecked;
  IsChecked := T.IsChecked;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TRadioButtonFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TRadioButtonFormControlObj.DoOnChange;
begin
  if Checked <> WasChecked then
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

{----------------TCellBasic.Create}

constructor TCellBasic.Create(Parent: TBlock);
begin
{$IFDEF JPM_DEBUGGING_CREATE}
  CodeSite.EnterMethod(Self,'TCellBasic.Create');
{$ENDIF}
  inherited Create;
  FOwnerBlock := Parent;
  if FOwnerBlock <> nil then
    FDocument := FOwnerBlock.Document;
//  FRenderList := TRenderSectionBaseList.Create(false);
{$IFDEF JPM_DEBUGGING_CREATE}
  CodeSite.ExitMethod(Self,'TCellBasic.Create');
{$ENDIF}
end;

{----------------TCellBasic.CreateCopy}

constructor TCellBasic.CreateCopy(Parent: TBlock; T: TCellBasic);
var
  I: Integer;
  Tmp, Tmp1: TSectionBase;
begin
  inherited Create;
  FOwnerBlock := Parent;
  if FOwnerBlock <> nil then
    FDocument := FOwnerBlock.Document;
  for I := 0 to T.Count - 1 do
  begin
    Tmp := T.Items[I];
    Tmp1 := TSectionClass(Tmp.ClassType).CreateCopy(Self, Tmp);
    Add(Tmp1, 0);
  end;
//  FRenderList := TRenderSectionBaseList.Create(false);
end;

//-- BG ---------------------------------------------------------- 01.12.2013 --
destructor TCellBasic.Destroy;
begin
//  FRenderList.Free;
  inherited;
end;

{----------------TCellBasic.Add}

procedure TCellBasic.Add(Item: TSectionBase; TagIndex: Integer);
var
  Section: TSection absolute Item;
begin
  if Assigned(Item) then
  begin
    if Item is TSection then
      Add(Section, TagIndex)
    else
      inherited Add(Item);
    Item.SetDocument(Document);
  end;
end;

//-- BG ---------------------------------------------------------- 14.01.2015 --
procedure TCellBasic.Add(var Section: TSection; TagIndex: Integer);
begin
  if Assigned(Section) then
  begin
    if Length(Section.XP) <> 0 then
    begin
      Section.ProcessText(TagIndex);
      if not (Section.WhiteSpaceStyle in [wsPre, wsPreWrap, wsPreLine]) and (Section.Len = 0)
        and not Section.AnchorName and (Section.FClearing = clrNone) then
      begin
        Section.CheckFree;
        FreeAndNil(Section); {discard empty TSections that aren't anchors}
        Exit;
      end;
    end;
    inherited Add(Section);
    Section.SetDocument(Document);
  end;
end;

//-- BG ---------------------------------------------------------- 07.09.2013 --
function TCellBasic.CalcDisplayExtern: ThtDisplayStyle;
var
  I: Integer;
begin
  // a list of elements is displayed inline, if all elements' are displayed inline.
  for I := Count - 1 downto 0 do
    if Items[i].CalcDisplayExtern <> pdInline then
    begin
      Result := pdBlock;
      Exit;
    end;
  Result := pdInline;
end;

function TCellBasic.CheckLastBottomMargin: Boolean;
{Look at the last item in this cell.  If its bottom margin was set to Auto,
 set it to 0}
var
  TB: TSectionBase;
  Block: TBlock absolute TB;
  I, J: Integer;
begin
  Result := False;

  I := 0; {find first block that isn't absolute positioning}
  while I < Count do
  begin
    TB := Items[I];
    if (TB is TSection) and (Length(Trim(TSection(TB).BuffS)) = 0) then
    else if not (TB is TBlock) or (TBlock(TB).Positioning <> PosAbsolute) then
        break;

    Inc(I);
  end;
  if I < Count then
  begin
    if TB is TBlock then
    begin
      if Block.TopAuto then
      begin
        Block.MargArray[MarginTop] := 0;
//        Result := True;
      end;
//      if TB is TBlockLI then
//        Result := TBlockLI(TB).MyCell.CheckLastBottomMargin;
    end
  end;

  J := Count - 1; {find the preceding block that isn't absolute positioning}
  while J >= I do
  begin
    TB := Items[J];
    if (TB is TSection) and (Length(Trim(TSection(TB).BuffS)) = 0) then
    else if not (TB is TBlock) or (TBlock(TB).Positioning <> PosAbsolute) then
      break;

    Dec(J);
  end;
  if J >= 0 then
  begin
    if TB is TBlock then
    begin
      if Block.BottomAuto then
      begin
        Block.MargArray[MarginBottom] := 0;
        Result := True;
      end;
      if TB is TBlockLI then
        Result := TBlockLI(TB).MyCell.CheckLastBottomMargin;
    end;
  end;
end;

{----------------TCellBasic.GetURL}

function TCellBasic.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): ThtguResultType;
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
      // BG, 01.04.2013: cannot reduce workload via DrawTop/DrawBot as
      // absolutely positioned children may reside beyond these margins.
      //if (Y >= DrawTop) and (Y < DrawBot) then
      //begin
        Result := GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
        if Result <> [] then
          Exit;
      //end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 04.08.2013 --
function TCellBasic.IsCopy: Boolean;
begin
  Result := Document.IsCopy;
end;

{----------------TCellBasic.FindCursor}

function TCellBasic.FindCursor(Canvas: TCanvas; X, Y: Integer;
  out XR, YR, Ht: Integer; out Intext: Boolean): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    with Items[I] do
    begin
      // BG, 01.04.2013: cannot reduce workload via DrawTop/DrawBot as
      // absolutely positioned children may reside beyond these margins.
      //if (Y >= DrawTop) and (Y < DrawBot) then
      //begin
        Result := Items[I].FindCursor(Canvas, X, Y, XR, YR, Ht, InText);
        if Result >= 0 then
          Exit;
      //end;
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

function TCellBasic.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
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

function TCellBasic.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
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

{$ifdef UseFormTree}
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
      Tree := Tree + Indent + Copy(TSection(Item).BuffS, 1, 10) + CrChar + LfChar
    else
      Tree := Tree + Indent + '----'^M^J;
  end;
end;
{$endif UseFormTree}

{----------------TCellBasic.GetChAtPos}

function TCellBasic.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean;
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

{----------------TCellBasic.CopySelectedText}

procedure TCellBasic.CopySelectedText;
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
      CopySelectedText;
    end;
end;

{----------------TCellBasic.DoLogic}

function TCellBasic.DoLogic(Canvas: TCanvas; Y, Width, AHeight, BlHt: Integer;
  var ScrollWidth: Integer; var Curs: Integer): Integer;
{Do the entire layout of the cell or document.  Return the total cell or document pixel height}
var
  I, Sw, TheCount: Integer;
  H: Integer;
//  InlineSection: TInlineSection;
//  Item: TSectionBase;
begin
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.EnterMethod(Self, Format('TCellBasic.DoLogic, %d Items', [Count]));
  CodeSite.SendFmtMsg('Y = [%d]',[Y]);
  CodeSite.SendFmtMsg('Width = [%d]',[Width]);
  CodeSite.SendFmtMsg('AHeight = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt = [%d]',[BlHt]);
  CodeSite.SendFmtMsg('ScrollWidth = [%d]',[ScrollWidth]);
  CodeSite.SendFmtMsg('Curs = [%d]',[Curs]);
  CodeSite.AddSeparator;
{$ENDIF}

//TODO -oBG, 15.03.2014: support display:inline
{$ifdef DO_PD_INLINE}
//  //TODO -oBG, 24.06.2012: merge sections with display=inline etc.
//  //TODO -oBG, 30.11.2013: To be more precise:
//  // Reading the HTML file must produce the actual document tree rather than preprocessing inline elements to TSections.
//  //
//  // The Renderer (DoLogic()/DrawLogic()) must produce a processing list. In the list consecutive elements of all child
//  // levels with display=inline are gathered in the (anonymous) TInlineSection element while block-level elements are
//  // added to the processing list directly. This ends up in a list of block level elements, just like TCellBasic has
//  // been before property Display was introduced.
//  //
//  // Actually TInlineSection is intended to collect content of TSections of consecutive inline elements and must draw
//  // borders and background of the inlined elements.
//  InlineSection := nil;
//  FRenderList.Clear;
//  for I := 0 to Count - 1 do
//  begin
//    Item := Items[I];
//    if Item.CalcDisplayExtern = pdInline then
//    begin
//      if InlineSection = nil then
//      begin
//        InlineSection := TInlineSection.Create(Self);
//        FRenderList.Add(InlineSection);
//      end;
//      if Item is TSection then
//        InlineSection.AddElement(TSection(Item));
//    end
//    else
//    begin
//      InlineSection := nil;
//      FRenderList.Add(Items[I]);
//    end;
//  end;
{$endif}

  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
  begin
    try
      Sw := 0;
      Inc(H, Items[I].DrawLogic1(Canvas, 0, Y + H, 0, 0, Width, AHeight, BlHt, IMgr, Sw, Curs));
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
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.SendFmtMsg('ScrollWidth = [%d]',[ScrollWidth]);
  CodeSite.SendFmtMsg('Curs = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TCellBasic.DoLogic');
{$ENDIF}
end;

{----------------TCellBasic.MinMaxWidth}

procedure TCellBasic.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
{Find the Width the cell would take if no wordwrap, Max, and the width if wrapped
 at largest word, Min}
var
  I, Mn, Mx: Integer;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellBasic.MinMaxWidth');
  try
{$ENDIF}
  Max := 0; Min := 0;
  for I := 0 to Count - 1 do
  begin
    Items[I].MinMaxWidth(Canvas, Mn, Mx, AvailableWidth, AvailableHeight);
    Max := Math.Max(Max, Mx);
    Min := Math.Max(Min, Mn);
  end;
{$IFDEF JPM_DEBUGGING}
  finally
    CodeSite.ExitMethod(Self, Format('TCellBasic.MinMaxWidth %d .. %d', [Min, Max]));
  end;
{$ENDIF}
end;

{----------------TCellBasic.Draw}

function TCellBasic.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: Integer; Y, XRef, YRef: Integer): Integer;
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

constructor TBlock.Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
var
  S: ThtString;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,Format('TBlock.Create "%s"', [TagClass]));
  {$IFDEF JPM_DEBUGGING_STYLES}
  StyleUn.LogProperties(Prop,'Prop');
  CodeSite.AddSeparator;
  {$ENDIF}
  {$ENDIF}
  inherited Create(Parent, 0, Attributes, Prop);
  MyCell := TBlockCell.Create(Self);
  DrawList := TSectionBaseList.Create(False);

  if Document.UseQuirksMode and (Self is TTableBlock) then
    Prop.GetVMarginArrayDefBorder(MargArrayO, clSilver)
  else
    Prop.GetVMarginArray(MargArrayO);
  HasBorderStyle := Prop.HasBorderStyle;
  FGColor := Prop.Props[Color];

  BlockTitle := Prop.PropTitle;
  if not (Self is TBodyBlock) and not (Self is TTableAndCaptionBlock)
    and Prop.GetBackgroundImage(S) and (S <> '') then
  begin {body handles its own image}
    BGImage := TImageObj.SimpleCreate(MyCell, S);
    Prop.GetBackgroundPos(EmSize, ExSize, PRec);
  end;

  Visibility := Prop.GetVisibility;
  Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
  if Positioning <> posStatic then
  begin
    ZIndex := 10 * Prop.GetZIndex;
    if (Positioning = posAbsolute) and (ZIndex = 0) then
      ZIndex := 1; {abs on top unless otherwise specified}
  end;
  if (Floating in [ALeft, ARight]) and (ZIndex = 0) then
    ZIndex := 1;
  if not IsInFlow then
  begin
    MyIMgr := TIndentManager.Create;
    MyCell.IMgr := MyIMgr;
  end;
  if not (Self is TTableBlock) and not (Self is TTableAndCaptionBlock) then
    CollapseAdjoiningMargins;
  HideOverflow := Prop.IsOverflowHidden;
  if Prop.Props[TextAlign] = 'right' then
    Justify := Right
  else if Prop.Props[TextAlign] = 'center' then
    Justify := Centered
  else
    Justify := Left;
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,Format('TBlock.Create "%s"', [TagClass]));
  {$ENDIF}
end;

//-- BG ---------------------------------------------------------- 17.05.2014 --
function Collapse(A, B: Integer): Integer; {$ifdef UseInline} inline; {$endif}
begin
  if A >= 0 then
    if B >= 0 then
      Result := Max(A, B)
    else
      Result := A + B
  else
    if B >= 0 then
      Result := B + A
    else
      Result := Min(A, B);
end;

procedure TBlock.CollapseAdjoiningMargins;
{adjacent vertical margins need to be reduced}
var
  TB: TSectionBase;
  Block: TBlock absolute TB;
  LastMargin, I: Integer;
  Tag: TElemSymb;
  CD: ThtConvData;
begin
  CD := ConvData(100, 100 {width and height not known at this point},  EmSize, ExSize, BorderWidth);
  ConvVertMargins(MargArrayO, CD, MargArray, Document.PixelsPerInch);
  TopAuto := MarginTop in CD.IsAutoParagraph;
  BottomAuto := MarginBottom in CD.IsAutoParagraph;
  if (MargArray[PaddingTop] <> 0) or (MargArray[BorderTopWidth] <> 0) then {do nothing}
  else if Positioning = posAbsolute then
  begin
    if TopAuto then
      MargArray[MarginTop] := 0;
  end
  else if Floating in [ALeft, ARight] then {do nothing}
  else if Display = pdNone then {do nothing}
  else
  begin
    TB := nil;
    I := OwnerCell.Count - 1; {find the preceding block that isn't absolute positioning}
    while I >= 0 do
    begin
      TB := OwnerCell[I];
      if TB.Display <> pdNone then
        if not (TB is TBlock) or (TBlock(TB).Positioning <> PosAbsolute) then
          break;
      Dec(I);
    end;

    if OwnerCell.OwnerBlock <> nil then
      Tag := OwnerCell.OwnerBlock.Symbol
    else
      Tag := OtherChar;
    if I < 0 then
    begin {no previous non absolute block, remove any Auto paragraph space}
      case Tag of
        BodySy:
          MargArray[MarginTop] := Max(0, MargArray[MarginTop] - OwnerBlock.MargArray[MarginTop]);
      else
        if TopAuto then
          MargArray[MarginTop] := 0;
      end;
    end
    else
    begin
      if ((TB is TTableBlock) or (TB is TTableAndCaptionBlock)) and (TBlock(TB).Floating in [ALeft, ARight]) and TopAuto then
        MargArray[MarginTop] := 0
      else if TB is TBlock then
      begin
        if Block.Positioning = posStatic then
        begin
          LastMargin := Block.MargArray[MarginBottom];
          Block.MargArray[MarginBottom] := 0;
          MargArray[MarginTop] := Collapse(LastMargin, MargArray[MarginTop]);
        end
      end
      else if (Tag = LISy) and TopAuto and (Symbol in [ULSy, OLSy]) then
        MargArray[MarginTop] := 0; {removes space from nested lists}
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 16.05.2014 --
procedure TBlock.CollapseNestedMargins;
var
  TB: TSectionBase;
  Block: TBlock absolute TB;
  I, J, M: Integer;
begin
  if (OwnerCell <> Document) and (MargArray[piHeight] = 0) and (MargArray[piMinHeight] = 0) then
  begin
    J := 0;
    if MargArray[BorderTopWidth] = 0 then
    begin
      // collapse with first sectionbase, if it is an in-flow block.

      // find block to collapse
      while J < MyCell.Count do
      begin
        TB := MyCell[J];
        if TB.Display <> pdNone then
          if not (TB is TBlock) or ((Block.Positioning <> PosAbsolute) and (Block.Floating = aNone)) then
            break;
        Inc(J);
      end;

      if J < MyCell.Count then
      begin
        if (TB is TBlock) and (Block.Positioning = posStatic) then
        begin
          // collapse margins, if it has no top padding
          if (Block.MargArray[PaddingTop] = 0) then
          begin
            M := Block.MargArray[MarginTop];
            Block.MargArray[MarginTop] := 0;
            MargArray[MarginTop] := Collapse(MargArray[MarginTop], M);
          end;
        end;
      end;
    end;

    if MargArray[BorderBottomWidth] = 0 then
    begin
      // collapse with last sectionbase, if it is an in-flow block.

      // find block to collapse
      I := MyCell.Count - 1;
      while I >= J do
      begin
        TB := MyCell[I];
        if TB.Display <> pdNone then
          if not (TB is TBlock) or ((Block.Positioning <> PosAbsolute) and (Block.Floating = aNone)) then
            break;
        Dec(I);
      end;

      if I >= 0 then
      begin
        if (TB is TBlock) and (Block.Positioning = posStatic) then
        begin
          // collapse margins, if it has no bottom padding
          if (Block.MargArray[PaddingBottom] = 0) then
          begin
            M := Block.MargArray[MarginBottom];
            Block.MargArray[MarginBottom] := 0;
            MargArray[MarginBottom] := Collapse(MargArray[MarginBottom], M);
          end;
        end;
      end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 09.10.2010 --
procedure TBlock.ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self, Format('TBlock.ContentMinMaxWidth TagClass = [%s]', [TagClass]));
  try
{$ENDIF}

  MyCell.MinMaxWidth(Canvas, Min, Max, AvailableWidth, AvailableHeight);

{$IFDEF JPM_DEBUGGING}
  finally
    CodeSite.ExitMethod(Self, Format('TBlock.ContentMinMaxWidth %d .. %d', [Min, Max]));
  end;
{$ENDIF}
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
procedure TBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer);
begin
{$IFDEF JPM_DEBUGGING_CONV}
  CodeSite.EnterMethod(Self,'TBlock.ConvMargArray');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);
  CodeSite.AddSeparator;
{$ENDIF}

  StyleUn.ConvMargArray(MargArrayO, BaseWidth, BaseHeight, EmSize, ExSize, BorderWidth, AutoCount, MargArray, Document.PixelsPerInch);

{$IFDEF JPM_DEBUGGING_CONV}
  CodeSite.ExitMethod(Self,'TBlock.ConvMargArray');
{$ENDIF}
end;

{----------------TBlock.CreateCopy}

constructor TBlock.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TBlock absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  System.Move(T.MargArray, MargArray, PtrSub(@Converted, @MargArray) + Sizeof(Converted));
  MyCell := TBlockCell.CreateCopy(Self, T.MyCell);
  DrawList := TSectionBaseList.Create(False);
  if Assigned(T.BGImage) and Document.PrintTableBackground then
    BGImage := TImageObj.CreateCopy(MyCell, T.BGImage);
  MargArrayO := T.MargArrayO;
  if not IsInFlow then
  begin
    MyIMgr := TIndentManager.Create;
    MyCell.IMgr := MyIMgr;
  end;
  BlockTitle := T.BlockTitle; // Thanks to Nagy Ervin.
end;

destructor TBlock.Destroy;
begin
  BGImage.Free;
  TiledImage.Free;
  if MyIMgr <> nil then
  begin
    MyCell.IMgr := nil;
    FreeAndNil(MyIMgr);
  end;
  FreeAndNil(MyCell);
  DrawList.Free;
  inherited Destroy;
end;

procedure TBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
var
  MinCell, MaxCell: Integer;
  LeftSide, RightSide, AutoCount: Integer;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self, Format('TBlock.MinMaxWidth TagClass = [%s]', [TagClass]));
  try
{$ENDIF}

  if (Display = pdNone) or (Positioning in [PosAbsolute, posFixed]) then
  begin
    Min := 0;
    Max := 0;
    Exit;
  end;

{$ifdef DO_BLOCK_INLINE}
  if Display = pdInline then
  begin
    inherited MinMaxWidth(Canvas, Min, Max, AvailableWidth, AvailableHeight);
    exit;
  end;
{$endif}

  ConvMargArray(AvailableWidth, AvailableHeight, AutoCount);
  //HideOverflow := HideOverflow and (MargArray[piWidth] <> Auto) and (MargArray[piWidth] > 20);
  if MargArray[MarginLeft] = Auto then
    MargArray[MarginLeft] := 0;
  if MargArray[MarginRight] = Auto then
    MargArray[MarginRight] := 0;
  if MargArray[piWidth] = Auto then
    MargArray[piWidth] := 0;
  LeftSide := MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
  RightSide := MargArray[MarginRight] + MargArray[BorderRightWidth] + MargArray[PaddingRight];
  if HideOverflow and (MargArray[piWidth] <> Auto) and (MargArray[piWidth] > 20) then
  begin
    MinCell := MargArray[piWidth];
    MaxCell := MinCell;
  end
  else
    ContentMinMaxWidth(Canvas, MinCell, MaxCell, AvailableWidth - LeftSide - RightSide, AvailableHeight);
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
  if (MargArray[piMinWidth] > 0) and (MargArray[piMinWidth] > Min) then
    Min := MargArray[piMinWidth];
  if (MargArray[piMaxWidth] > 0) and (MargArray[piMaxWidth] < Max) then
    Max := MargArray[piMaxWidth];

{$IFDEF JPM_DEBUGGING}
  finally
    CodeSite.ExitMethod(Self, Format('TBlock.MinMaxWidth %d .. %d', [Min, Max]));
  end;
{$ENDIF}
end;

{----------------TBlock.GetURL}

function TBlock.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj}; out ATitle: ThtString): ThtguResultType;
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
    if not (guTitle in Result) and (BlockTitle <> '') and PtInRect(MyRect, Point(X, Y - Document.YOFF)) then
    begin
      ATitle := BlockTitle;
      Include(Result, guTitle);
    end;
  end;
end;

{----------------TBlock.FindString}

function TBlock.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
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

function TBlock.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
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

function TBlock.FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer;
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
        with Items[I] do
        begin
          // BG, 01.04.2013: cannot reduce workload via DrawTop/DrawBot as
          // absolutely positioned children may reside beyond these margins.
          //if (Y >= DrawTop) and (Y < DrawBot) then
          //begin
            Result := FindCursor(Canvas, X, Y, XR, YR, CaretHt, Intext);
            if Result >= 0 then
              Exit;
          //end;
        end;
  end;
end;

procedure TBlock.AddSectionsToList;
begin
  MyCell.AddSectionsToList;
end;

{----------------TBlock.PtInObject}

function TBlock.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;
{Y is absolute}
var
  I: Integer;
begin
  case Display of
    pdNone:
    begin
      Result := False;
      Exit;
    end;

  else
    {check this in z order}
    for I := DrawList.Count - 1 downto 0 do
      if DrawList.Items[I].PtInObject(X, Y, Obj, IX, IY) then
      begin
        Result := True;
        Exit;
      end;

  end;

  Result := inherited PtInObject(X, Y, Obj, IX, IY);
end;

{----------------TBlock.GetChAtPos}

function TBlock.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean;
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

//-- BG ---------------------------------------------------------- 30.11.2013 --
function TBlock.CalcDisplayExtern: ThtDisplayStyle;
begin
  Result := inherited CalcDisplayExtern;
  case Result of
    pdInline:
      Result := MyCell.CalcDisplayExtern;
  end;
end;

//-- BG ---------------------------------------------------------- 07.09.2013 --
function TBlock.CalcDisplayIntern: ThtDisplayStyle;
begin
  Result := inherited CalcDisplayIntern;
//  case Result of
//    pdNone:;
//  else
//    //Result := MyCell.CalcDisplayExtern;
//  end;
end;

function TBlock.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean;
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

function TBlock.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
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

procedure TBlock.CopySelectedText;
begin
  case Display of
    pdNone:;
{$ifdef DO_BLOCK_INLINE}
    pdInline: inherited CopySelectedText;
{$endif}
  else
    MyCell.CopySelectedText;
    if (Symbol = PSy) and (Document.SelE > MyCell.StartCurs + MyCell.Len) then
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
    begin
      if (MargArray[piLeft] = Auto) and (MargArray[piRight] <> Auto) then
        MargArray[piWidth] := MaxWidth
      else
        MargArray[piWidth] := Max(MinWidth, AWidth - BordWidth - LeftP);
    end
    else if (Floating in [ALeft, ARight]) then
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
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.FindWidth');
  CodeSite.SendFmtMsg('AWidth    = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight   = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('AutoCount = [%d]',[AutoCount]);
  CodeSite.SendFmtMsg('Self.TagClass = [%s]',[TagClass ]);
  CodeSite.AddSeparator;
{$ENDIF}

  ContentMinMaxWidth(Canvas, MinWidth, MaxWidth, AWidth, AHeight);
  //HideOverflow := HideOverflow and (MargArray[piWidth] <> Auto) and (MargArray[piWidth] > 20);
  case AutoCount of
    0:
      begin
        if (Justify in [centered, Right]) and (Positioning = posStatic)
          and not (Floating in [ALeft, ARight]) and
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
          if (Floating in [ALeft, ARight]) then
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
        MargArray[MarginRight] := Marg2 - MargArray[MarginLeft];
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

{----------------TBlock.DrawLogic}

function TBlock.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
var
  ScrollWidth, YClear: Integer;
  LIndex, RIndex: Integer;
  SaveID: TObject;
  TotalWidth, LeftWidths, RightWidths, MiscWidths: Integer;
  AutoCount: Integer;
  BlockHeight: Integer;
  IB, Xin: Integer;

  function GetClientContentBot(ClientContentBot: Integer): Integer;
  var
    H: Integer;
    S: Variant;
  begin
    S := MargArrayO[piHeight];
    if VarIsIntNull(S) or VarIsAuto(S) then
      H := Auto
    else
    begin
      if Pos('%', VarToStr(S)) > 0 then
        H := LengthConv(S, False, AHeight, EmSize, ExSize, 0, Document.PixelsPerInch)
      else
        H := MargArray[piHeight];
    end;

    case H of
      IntNull,
      Auto:
        Result := Max(ContentTop, ClientContentBot);
    else
      Result := Max(H + ContentTop, ContentTop);
    end;
  end;

  procedure DrawLogicAsBlock;
  var
    LIndent, RIndent: Integer;
    ARect: TRect;
  begin
    YDraw := Y;
    Xin := X;
    ClearAddOn := GetClearSpace(IMgr, Y);
    StartCurs := Curs;
    MaxWidth := AWidth;

    ConvMargArray(AWidth, AHeight, AutoCount);
    HasBorderStyle :=
      (ThtBorderStyle(MargArray[BorderTopStyle]) <> bssNone) or
      (ThtBorderStyle(MargArray[BorderRightStyle]) <> bssNone) or
      (ThtBorderStyle(MargArray[BorderBottomStyle]) <> bssNone) or
      (ThtBorderStyle(MargArray[BorderLeftStyle]) <> bssNone);

    ApplyBoxSettings(MargArray, Document.UseQuirksMode);
    ContentWidth := FindWidth(Canvas, AWidth, AHeight, AutoCount);
    LeftWidths   := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
    RightWidths  := MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth];
    MiscWidths   := LeftWidths + RightWidths;
    TotalWidth   := MiscWidths + ContentWidth;

    case Positioning of
      posAbsolute:
        ARect := ContainingBox;

      posFixed:
        ARect := Document.ViewPort;
    end;

    FIndent := LeftWidths;
    TopP := 0;
    LeftP := 0;
    case Positioning of
      posAbsolute,
      posFixed:
      begin
        if not IsAuto(Positions[reLeft]) then
          X := ARect.Left + Positions[reLeft]
        else if not IsAuto(Positions[reRight]) and not IsAuto(MargArray[piWidth]) then
          X := ARect.Right - Positions[reRight] - MargArray[piWidth];
        LeftP := X;

        if not IsAuto(Positions[reTop]) then
          Y := ARect.Top + Positions[reTop]
        else if not IsAuto(Positions[reBottom]) and not IsAuto(MargArray[piHeight]) then
          Y := ARect.Bottom - Positions[reBottom] - MargArray[piHeight];
        TopP := Y;

        DrawTop := TopP + Max(0, MargArray[MarginTop]); {Border top}
      end;
    else
      if Positioning = posRelative then
      begin
        if not IsAuto(Positions[reLeft]) then Inc(LeftP, Positions[reLeft]);
        if not IsAuto(Positions[reTop ]) then Inc(TopP, Positions[reTop ]);
      end;

      case Floating of
        ALeft:
        begin
          YClear := Y;
          LIndent := IMgr.AlignLeft(YClear, TotalWidth);
          FIndent := LIndent + LeftWidths - X;
        end;

        ARight:
        begin
          YClear := Y;
          RIndent := IMgr.AlignRight(YClear, TotalWidth);
          FIndent := RIndent + LeftWidths - X;
        end;
      else
        YClear := Y + ClearAddon;
      end;
      DrawTop := YClear + Max(0, MargArray[MarginTop]); {Border top}
    end;
    Inc(X, FIndent);

    ContentTop := DrawTop + MargArray[PaddingTop] + MargArray[BorderTopWidth];

    if not IsInFlow then
    begin
      RefIMgr := IMgr;
      if MyCell.IMgr = nil then
      begin
        MyIMgr := TIndentManager.Create;
        MyCell.IMgr := MyIMgr;
      end;
      IMgr := MyCell.IMgr;
      IMgr.Init(0, ContentWidth);
    end
    else
    begin
      MyCell.IMgr := IMgr;
    end;

    SaveID := IMgr.CurrentID;
    IMgr.CurrentID := Self;

    LIndex := IMgr.SetLeftIndent(X, YClear);
    RIndex := IMgr.SetRightIndent(X + ContentWidth, YClear);

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
          ContentWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
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
          ContentWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
        MaxWidth := ScrollWidth + MiscWidths - MargArray[MarginRight] + LeftP - Xin;
        ClientContentBot := GetClientContentBot(MyCell.tcContentBot);
        IB := IMgr.ImageBottom; {check for image overhang}
        if IB > ClientContentBot then
          ClientContentBot := IB;
      end;

      posFixed:
      begin
        MyCell.DoLogicX(Canvas,
          X,
          ContentTop,
          XRef + LeftP + MargArray[MarginLeft] + MargArray[BorderLeftWidth],
          YRef + TopP + MargArray[MarginTop] + MargArray[BorderTopWidth],
          ContentWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
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
        ContentWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
      MaxWidth := Indent + ScrollWidth + RightWidths;
      ClientContentBot := GetClientContentBot(MyCell.tcContentBot);
    end;
    Len := Curs - StartCurs;

    ContentBot := ClientContentBot + MargArray[PaddingBottom] + MargArray[BorderBottomWidth] + MargArray[MarginBottom];
    if ContentBot = ContentTop then
      if IsInFlow then
      begin
        // no content, hide top margin
        DrawTop := YClear;
        ContentTop := DrawTop;
        ClientContentBot := ContentTop;
        ContentBot := ClientContentBot;
      end;

    DrawBot := Max(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom] + MargArray[BorderBottomWidth];

    Result := ContentBot - Y;

    if Assigned(BGImage) and Document.ShowImages then
    begin
      BGImage.DrawLogicInline(Canvas, nil, 100, 0);
      if BGImage.Image = ErrorImage then
      begin
        FreeAndNil(BGImage);
        NeedDoImageStuff := False;
      end
      else
      begin
        BGImage.ClientSizeKnown := True; {won't need reformat on InsertImage}
        NeedDoImageStuff := True;
      end;
    end;
    SectionHeight := Result;
    IMgr.FreeLeftIndentRec(LIndex);
    IMgr.FreeRightIndentRec(RIndex);
    if not IsInFlow then
    begin
      case Positioning of
        posAbsolute,
        posFixed:
          DrawHeight := 0
      else
        DrawHeight := 0; //SectionHeight;
        case Floating of
          ALeft:  RefIMgr.AddLeft(YClear, ContentBot, TotalWidth);
          ARight: RefIMgr.AddRight(YClear, ContentBot, TotalWidth);
        end;
      end;
      //SectionHeight := 0;
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

    //>-- DZ
    DrawRect.Left   := X - FIndent + MargArray[MarginLeft];
    DrawRect.Top    := DrawTop;
    DrawRect.Right  := DrawRect.Left + ContentWidth;
    DrawRect.Bottom := DrawRect.Top + SectionHeight;
  end;

  procedure DrawLogicInline;
  begin
    //TODO -oBG, 15.03.2014: draw logic inline
    DrawLogicAsBlock;
    //DrawRect.Right := DrawRect.Left + MyCell.TextWidth;
  end;

  procedure Invisible;
  begin
    SectionHeight := 0;
    DrawHeight := 0;
    ContentBot := 0;
    DrawBot := 0;
    MaxWidth := 0;
    Result := 0;

    //>-- DZ
    DrawRect.Left   := X;
    DrawRect.Top    := DrawTop;
    DrawRect.Right  := DrawRect.Left + ContentWidth;
    DrawRect.Bottom := DrawRect.Top + SectionHeight;
  end;

begin {TBlock.DrawLogic1}
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.EnterMethod(Self,Format('TBlock.DrawLogic1 "%s"', [TagClass]));
  CodeSite.SendFmtMsg('AWidth, AHeight = [%4d, %4d]', [AWidth, AHeight]);
{$IFDEF JPM_DEBUGGING_LOGIC1}
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
{$ENDIF}
  case CalcDisplayIntern of

    pdInline:
      DrawLogicInline;

    pdNone:
      Invisible;
  else
//    pdBlock:
//    pdTable:
      DrawLogicAsBlock;
  end;
{$IFDEF JPM_DEBUGGING_LOGIC}
{$IFDEF JPM_DEBUGGING_LOGIC1}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
{$ENDIF}
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlock.DrawLogic1');
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
    Inserted: Boolean;
  begin
    Inserted := False;
    for J := I1 to I2 - 1 do
      if SBZIndex < DrawList[J].ZIndex then
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

function TBlock.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;

  procedure DrawAsBlock;
  var
    Y, YO: Integer;
    HeightNeeded, Spacing: Integer;
  begin
    Y := YDraw;
    YO := Y - Document.YOff;
    Result := Y + SectionHeight;

    if Visibility = viHidden then
      Exit;

    if Document.SkipDraw then
    begin
      Document.SkipDraw := False;
      Exit;
    end;

    if Document.Printing and (Positioning <> posAbsolute) then
    begin
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
      else if (Self is TTableBlock) and not TTableBlock(Self).Table.HeadOrFoot then {ordinary tables}
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
            Spacing := CellSpacingVert div 2;
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

    end;

    case Positioning of
      posRelative:
        DrawBlock(Canvas, ARect, IMgr, X + LeftP, Y + TopP, XRef, YRef);

      posAbsolute:
        DrawBlock(Canvas, ARect, IMgr, LeftP, TopP, 0, 0);

      posFixed:
        DrawBlock(Canvas, ARect, IMgr, LeftP, TopP, 0, 0);

    else
      if Floating in [ALeft, ARight] then
        DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef)
      else
        DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef);
    end;
  end;

  procedure DrawInline;
  begin
    DrawAsBlock;
  end;

begin
  case CalcDisplayIntern of

    pdInline:
      DrawInline;

    pdNone:
      Result := 0;

  else
//    pdBlock:
//    pdTable:
      DrawAsBlock;
  end;
end;

{----------------TBlock.DrawBlock}

procedure TBlock.DrawBlock(Canvas: TCanvas; const ARect: TRect;
  IMgr: TIndentManager; X, Y, XRef, YRef: Integer);

var
  YOffset: Integer;
  XR, YB, RefX, RefY, TmpHt: Integer;
  SaveID: TObject;
  ImgOK, HasBackgroundColor: Boolean;
  IT, IH, FT, IW: Integer;
  Rgn, SaveRgn, SaveRgn1: HRgn;
  OpenRgn: Boolean;
  PdRect, CnRect: TRect; // padding rect, content rect
  NegativeMargins: Boolean;
begin
  if Document.Printing and not Document.PrintBackground then
    NeedDoImageStuff := False;
  YOffset := Document.YOff;

  case Floating of
    ALeft, ARight:
    begin
      //X := IMgr.LfEdge + Indent;
      X := X + Indent;
      RefX := X - MargArray[PaddingLeft] - MargArray[BorderLeftWidth];
      XR := X + ContentWidth + MargArray[PaddingRight] + MargArray[BorderRightWidth];
      RefY := DrawTop;
      YB := ContentBot - MargArray[MarginBottom];
    end;
  else
// BG, 08.09.2013: inline vs block:
//  X of centered blocks may differ from (DrawRect.Left - MargArray[MarginLeft]) calculated in DrawLogic1().
    RefX := X + MargArray[MarginLeft];
    X := X + Indent;
//    RefX := DrawRect.Left;
//    X := RefX - MargArray[MarginLeft] + Indent;
    XR := X + ContentWidth + MargArray[PaddingRight] + MargArray[BorderRightWidth]; {current right edge}
    RefY := Y + ClearAddon + MargArray[MarginTop];
    YB := ContentBot - MargArray[MarginBottom];
    case Positioning of
      posRelative:
        Inc(YB, TopP);

    end;
  end;

  // MyRect is the outmost rectangle of this block incl. border and padding but without margins in screen coordinates.
  case Positioning of
    posFixed:
      MyRect := Rect(RefX, RefY, XR, YB);
  else
    MyRect := Rect(RefX, RefY - YOffset, XR, YB - YOffset);
  end;

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

  //>-- DZ
  DrawRect.Top    := MyRect.Top + YOffset;
  DrawRect.Left   := MyRect.Left;
  DrawRect.Bottom := MyRect.Bottom + YOffset;
  DrawRect.Right  := MyRect.Right;

  IT := Max(0, ARect.Top - 2 - PdRect.Top);
  FT := Max(PdRect.Top, ARect.Top - 2); {top of area drawn, screen coordinates}
  IH := Min(PdRect.Bottom, ARect.Bottom) - FT; {height of area actually drawn}
  IW := PdRect.Right - PdRect.Left;

  NegativeMargins := (MargArray[MarginLeft] < 0) or (MargArray[MarginTop] < 0) or (MargArray[MarginBottom] < 0) or (MargArray[MarginRight] < 0);
  SaveRgn1 := 0;
  OpenRgn := ((Positioning <> PosStatic) or NegativeMargins) and (Document.TableNestLevel > 0);
  if OpenRgn then
  begin
    SaveRgn1 := CreateRectRgn(0, 0, 1, 1);
    case GetClipRgn(Canvas.Handle, SaveRgn1) of
      -1, // failed to get the clip region
       0: // there is no clip region
        begin
          DeleteObject(SaveRgn1);
          SaveRgn1 := 0;
        end;
    else
      //1: got the clip region
      SelectClipRgn(Canvas.Handle, 0);
    end;
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
            if Floating in [ALeft, ARight] then
              TmpHt := DrawBot - ContentTop + MargArray[PaddingTop] + MargArray[PaddingBottom]
            else
              TmpHt := ClientContentBot - ContentTop + MargArray[PaddingTop] + MargArray[PaddingBottom];

            BgImage.Image.TileImage( PRec,
              MargArray[PaddingLeft] + ContentWidth + MargArray[PaddingRight], TmpHt,
              TiledImage, NoMask);
            if IsCopy then
              TiledImage.Bitmap.HandleType := bmDIB;
          except {bad image, get rid of it}
            FreeAndNil(BGImage);
            FreeAndNil(TiledImage);
          end;
          NeedDoImageStuff := False;
        end;

        if Document.NoOutput then
          exit;

        ImgOK := not NeedDoImageStuff and (BGImage <> nil) and (BGImage.Image <> DefImage) and (TiledImage <> nil) and Document.ShowImages;

        if HasBackgroundColor {and (not Document.Printing or Document.PrintBackground)} then
        begin {color the Padding Region}
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := Document.ThemedColorToRGB(MargArray[BackgroundColor], htseClient) or PalRelative;
        end
        else
          Canvas.Brush.Style := bsClear;

        if ImgOK then
          if not IsCopy then
            TiledImage.DrawUnstretched(Canvas, PdRect.Left, FT, IW, IH, 0, IT, HasBackgroundColor )
          else
            TiledImage.PrintUnstretched(Canvas, PdRect.Left, FT, IW, IH, 0, IT, HasBackgroundColor and Document.PrintBackground )
        else
          if not Document.Printing or Document.PrintBackground then
            Canvas.FillRect(Rect(PdRect.Left, FT, PdRect.Right, FT + IH));
      except
      end;
    end;

    if HideOverflow then
    begin
      Rgn := 0;
      SaveRgn := 0;
      if Floating = ANone then
        GetClippingRgn(Canvas, CnRect, Document.Printing, Rgn, SaveRgn)
      else
        GetClippingRgn(Canvas, PdRect, Document.Printing, Rgn, SaveRgn);
      SelectClipRgn(Canvas.Handle, Rgn);
    end;
    try
      SaveID := IMgr.CurrentID;
      Imgr.CurrentID := Self;
      case Positioning of
        posRelative:
          DrawTheList(Canvas, ARect, ContentWidth, X,
            RefX + MargArray[BorderLeftWidth] + MargArray[PaddingLeft],
            Y + MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop]);

        posAbsolute:
          DrawTheList(Canvas, ARect, ContentWidth, X,
            RefX + MargArray[BorderLeftWidth],
            Y + MargArray[MarginTop] + MargArray[BorderTopWidth]);

        posFixed:
          DrawTheList(Canvas, ARect, ContentWidth, X,
            PdRect.Left,
            PdRect.Top);
      else
        DrawTheList(Canvas, ARect, ContentWidth, X, XRef, YRef);
      end;
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
    if SaveRgn1 <> 0 then
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
        htStyles(ThtBorderStyle(MargArray[BorderLeftStyle]), ThtBorderStyle(MargArray[BorderTopStyle]), ThtBorderStyle(MargArray[BorderRightStyle]), ThtBorderStyle(MargArray[BorderBottomStyle])),
        MargArray[BackgroundColor], Document.Printing, Document.ThemedColorToRGB)
end;

procedure TBlock.DrawTheList(Canvas: TCanvas; const ARect: TRect; ClipWidth, X, XRef, YRef: Integer);
{draw the list sorted by Z order.}
var
  I: Integer;
  SaveID: TObject;
begin
  if not IsInFlow then
    with MyCell do
    begin
      SaveID := IMgr.CurrentID;
      IMgr.Reset(RefIMgr.LfEdge);
      IMgr.ClipWidth := ClipWidth;
      IMgr.CurrentID := SaveID;
    end
  else
    MyCell.IMgr.ClipWidth := ClipWidth;
  for I := 0 to DrawList.Count - 1 do
    DrawList[I].Draw1(Canvas, ARect, MyCell.IMgr, X, XRef, YRef);
end;

{$ifdef UseFormTree}
procedure TBlock.FormTree(const Indent: ThtString; var Tree: ThtString);
var
  MyIndent: ThtString;
  TM, BM: ThtString;
begin
  MyIndent := Indent + '   ';
  TM := IntToStr(MargArray[MarginTop]);
  BM := IntToStr(MargArray[MarginBottom]);
  Tree := Tree + Indent + TagClass + '  ' + TM + '  ' + BM + CrChar + LfChar;
  MyCell.FormTree(MyIndent, Tree);
end;
{$endif UseFormTree}

//-- BG ---------------------------------------------------------- 24.08.2010 --
function TBlock.GetBorderWidth: Integer;
begin
  Result := 3;
end;

{----------------TTableAndCaptionBlock.Create}

constructor TTableAndCaptionBlock.Create(
  Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties; ATableBlock: TTableBlock);
var
  I: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableAndCaptionBlock.Create');
   {$ENDIF}
  inherited Create(Parent, Attributes, Prop);
  TableBlock := ATableBlock;
  Justify := TableBlock.Justify;

  for I := 0 to Attributes.Count - 1 do
    with Attributes[I] do
      case Which of
        AlignSy:
          if htCompareText(Name, 'CENTER') = 0 then
            Justify := Centered
          else if htCompareText(Name, 'LEFT') = 0 then
          begin
            if FFloating = ANone then
              FFloating := ALeft;
          end
          else if htCompareText(Name, 'RIGHT') = 0 then
          begin
            if FFloating = ANone then
              FFloating := ARight;
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

constructor TTableAndCaptionBlock.CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode);
var
  T: TTableAndCaptionBlock absolute Source;
  Item: TObject;
  I1, I2: Integer;
begin
  inherited CreateCopy(OwnerCell,Source);
  TopCaption := T.TopCaption;
  Justify := T.Justify;
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

  TableBlock.FFloating := ANone;
  TableBlock.Table.Float := False;

  CaptionBlock.MinMaxWidth(Canvas, Mn, Mx, AWidth, AHeight);
  FWidth := TableBlock.FindWidth1(Canvas, AWidth, AHeight, MargArray[MarginLeft] + MargArray[MarginRight]);
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

procedure TTableAndCaptionBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
var
  Mx, Mn, MxTable, MnTable: Integer;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self, Format('TTableAndCaptionBlock.MinMaxWidth TagClass = [%s]', [TagClass]));
  try
{$ENDIF}
  TableBlock.MinMaxWidth(Canvas, MnTable, MxTable, AvailableWidth, AvailableHeight);
  FCaptionBlock.MinMaxWidth(Canvas, Mn, Mx, AvailableWidth, AvailableHeight);
  Min := Math.Max(MnTable, Mn);
  Max := Math.Max(MxTable, Mn);
{$IFDEF JPM_DEBUGGING}
  finally
    CodeSite.ExitMethod(Self, Format('TTableAndCaptionBlock.MinMaxWidth %d .. %d', [Min, Max]));
  end;
{$ENDIF}
end;

function TTableAndCaptionBlock.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
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

constructor TTableBlock.Create(Parent: TCellBasic; Attr: TAttributeList; Prop: TProperties; TableLevel: Integer);
var
  I, AutoCount: Integer;
  BorderWidth: Integer;
  Percent: Boolean;
  TheProps, MyProps: TProperties;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TTableBlock.Create');
   {$ENDIF}

  Table := THtmlTable.Create(Self, Parent, Attr, Prop);

  // BG, 20.01.2013: translate table attributes to block property defaults:
  MyProps := TProperties.CreateCopy(Prop);
  TheProps := MyProps;
  try
    if Table.BorderColor <> clNone then
      MyProps.SetPropertyDefaults([BorderBottomColor, BorderRightColor, BorderTopColor, BorderLeftColor], Table.BorderColor)
    else
    begin
      if Table.HasBorderWidthAttr then
        MyProps.SetPropertyDefaults([BorderBottomColor, BorderRightColor, BorderTopColor, BorderLeftColor], clGray)
      else
        MyProps.SetPropertyDefaults([BorderBottomColor, BorderRightColor, BorderTopColor, BorderLeftColor], clNone);
    end;

     MyProps.SetPropertyDefault(BorderSpacingHorz, Table.CellSpacingHorz);
     MyProps.SetPropertyDefault(BorderSpacingVert, Table.CellSpacingVert);

    if Table.HasBorderWidthAttr then
      MyProps.SetPropertyDefaults([BorderBottomWidth, BorderRightWidth, BorderTopWidth, BorderLeftWidth], Table.brdWidthAttr);

    case Table.Frame of
      tfBox, tfBorder:
        MyProps.SetPropertyDefaults([BorderBottomStyle, BorderRightStyle, BorderTopStyle, BorderLeftStyle], bssOutset);

      tfHSides:
      begin
        MyProps.SetPropertyDefaults([BorderTopStyle, BorderBottomStyle], bssSolid);
        MyProps.SetPropertyDefaults([BorderLeftStyle, BorderRightStyle], bssNone);
      end;

      tfVSides:
      begin
        MyProps.SetPropertyDefaults([BorderTopStyle, BorderBottomStyle], bssNone);
        MyProps.SetPropertyDefaults([BorderLeftStyle, BorderRightStyle], bssSolid);
      end;

      tfAbove:
      begin
        MyProps.SetPropertyDefault(BorderTopStyle, bssSolid);
        MyProps.SetPropertyDefaults([BorderBottomStyle, BorderRightStyle, BorderLeftStyle], bssNone);
      end;

      tfBelow:
      begin
        MyProps.SetPropertyDefault(BorderBottomStyle, bssSolid);
        MyProps.SetPropertyDefaults([BorderRightStyle, BorderTopStyle, BorderLeftStyle], bssNone);
      end;

      tfLhs:
      begin
        MyProps.SetPropertyDefault(BorderLeftStyle, bssSolid);
        MyProps.SetPropertyDefaults([BorderBottomStyle, BorderRightStyle, BorderTopStyle], bssNone);
      end;

      tfRhs:
      begin
        MyProps.SetPropertyDefault(BorderRightStyle, bssSolid);
        MyProps.SetPropertyDefaults([BorderBottomStyle, BorderTopStyle, BorderLeftStyle], bssNone);
      end;
    else
      if Table.HasBorderWidthAttr then
        if Table.brdWidthAttr > 0 then
          MyProps.SetPropertyDefaults([BorderBottomStyle, BorderRightStyle, BorderTopStyle, BorderLeftStyle], bssOutset)
        else
          MyProps.SetPropertyDefaults([BorderBottomStyle, BorderRightStyle, BorderTopStyle, BorderLeftStyle], bssNone);
    end;

    inherited Create(Parent, Attr, TheProps);
  finally
    MyProps.Free;
  end;

  Justify := NoJustify;

  for I := 0 to Attr.Count - 1 do
    with Attr[I] do
      case Which of

        AlignSy:
          if htCompareText(Name, 'CENTER') = 0 then
            Justify := Centered
          else if htCompareText(Name, 'LEFT') = 0 then
          begin
//TODO: BG, 14.07.2013: The table block is not floating, but justified!
            if FFloating = ANone then
              FFloating := ALeft;
//            Justify := Left;
          end
          else if htCompareText(Name, 'RIGHT') = 0 then
          begin
//TODO: BG, 14.07.2013: The table block is not floating, but justified!
            if FFloating = ANone then
              FFloating := ARight;
//            Justify := Right;
          end;

        BGColorSy:
          BkGnd := TryStrToColor(Name, False, BkColor);

        BackgroundSy:
          if not Assigned(BGImage) and (Length(Name) > 0) then
          begin
            BGImage := TImageObj.SimpleCreate(MyCell, Name);
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

  if Table.BorderWidth > 0 then
    BorderWidth := Table.BorderWidth
  else
    BorderWidth := 3;

{need to see if width is defined in style}
  Percent := (VarIsStr(MargArrayO[piWidth])) and (Pos('%', MargArrayO[piWidth]) > 0);
  StyleUn.ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, BorderWidth, AutoCount, MargArray, Document.PixelsPerInch);
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

  CollapseAdjoiningMargins;
  Table.Float := Floating in [ALeft, ARight];
  if Table.Float and (ZIndex = 0) then
    ZIndex := 1;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TTableBlock.Create');
   {$ENDIF}
end;

{----------------TTableBlock.CreateCopy}

constructor TTableBlock.CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode);
var
  T: TTableBlock absolute Source;
  Item: TObject;
begin
  inherited CreateCopy(OwnerCell,Source);
  System.Move(T.WidthAttr, WidthAttr, PtrSub(@Justify, @WidthAttr) + Sizeof(Justify));
  Item := MyCell.Items[0];
  Table := Item as THtmlTable;
end;

{----------------TTableBlock.MinMaxWidth}

procedure TTableBlock.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
var
  TmpWidth: Integer;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self, Format('TTableBlock.MinMaxWidth TagClass = [%s]', [TagClass]));
  try
{$ENDIF}
  if AsPercent then
    TmpWidth := 0
  else
    TmpWidth := Math.Max(0, WidthAttr - MargArray[BorderLeftWidth] - MargArray[BorderRightWidth]
      - MargArray[PaddingLeft] - MargArray[PaddingRight]);
  Table.tblWidthAttr := TmpWidth;
  inherited MinMaxWidth(Canvas, Min, Max, AvailableWidth, AvailableHeight);
  if TmpWidth > 0 then
  begin
    Min := Math.Max(Min, TmpWidth);
    Max := Min;
  end;
{$IFDEF JPM_DEBUGGING}
  finally
    CodeSite.ExitMethod(Self, Format('TTableBlock.MinMaxWidth %d .. %d', [Min, Max]));
  end;
{$ENDIF}
end;

{----------------TTableBlock.FindWidth1}

function TTableBlock.FindWidth1(Canvas: TCanvas; AWidth, AHeight, ExtMarg: Integer): Integer;
{called by TTableAndCaptionBlock to assist in it's FindWidth Calculation.
 This method is called before TTableBlockFindWidth but is called only if there
 is a caption on the table.  AWidth is the full width available to the
 TTableAndCaptionBlock.}
var
  PaddingAndBorder: Integer;
  Min, Max, Allow: Integer;
begin
  MargArray[MarginLeft] := 0;
  MargArray[MarginRight] := 0;
  MargArray[MarginTop] := 0;
  MargArray[MarginBottom] := 0;

  PaddingAndBorder :=
    MargArray[BorderLeftWidth] + MargArray[PaddingLeft] +
    MargArray[BorderRightWidth] + MargArray[PaddingRight];
  Table.tblWidthAttr := 0;
  if WidthAttr > 0 then
  begin
    if AsPercent then
      Result := Math.Min(MulDiv(AWidth, WidthAttr, 1000), AWidth - ExtMarg)
    else
      Result := WidthAttr;
    Result := Result - PaddingAndBorder;
    Table.tblWidthAttr := Result;
    Table.MinMaxWidth(Canvas, Min, Max, AWidth, AHeight);
    Result := Math.Max(Min, Result);
    Table.tblWidthAttr := Result;
  end
  else
  begin
    Table.MinMaxWidth(Canvas, Min, Max, AWidth, AHeight);
    Allow := AWidth - PaddingAndBorder;
    if Max <= Allow then
      Result := Max
    else if Min >= Allow then
      Result := Min
    else
      Result := Allow;
  end;
  Result := Result + PaddingAndBorder;
end;

//-- BG ---------------------------------------------------------- 24.08.2010 --
function TTableBlock.GetBorderWidth: Integer;
begin
  Result := Table.BorderWidth;
  if Result = 0 then
    Result := 3;
end;

{----------------TTableBlock.FindWidth}

function TTableBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: Integer): Integer;
var
  LeftSide, RightSide: Integer;
  Min, Max, M, P: Integer;
  LJustify: ThtJustify;
begin
  if not HasCaption then
  begin
    inherited FindWidth(Canvas, AWidth, AHeight, AutoCount);
//    if MargArray[MarginLeft] = Auto then
//      MargArray[MarginLeft] := 0;
//    if MargArray[MarginRight] = Auto then
//      MargArray[MarginRight] := 0;
//
//    if Floating in [ALeft, ARight] then
//    begin
//      if MargArray[MarginLeft] = 0 then
//        MargArray[MarginLeft] := HSpace;
//      if MargArray[MarginRight] = 0 then
//        MargArray[MarginRight] := HSpace;
//      if MargArray[MarginTop] = 0 then
//        MargArray[MarginTop] := VSpace;
//      if MargArray[MarginBottom] = 0 then
//        MargArray[MarginBottom] := VSpace;
//    end;
  end
  else
  begin
    MargArray[MarginLeft] := 0;
    MargArray[MarginRight] := 0;
  end;

  if BkGnd and (MargArray[BackgroundColor] = clNone) then
    MargArray[BackgroundColor] := BkColor;
  Table.FBkGnd := (MargArray[BackgroundColor] <> clNone) and not Assigned(BGImage);
  Table.FBkColor := MargArray[BackgroundColor]; {to be passed on to cells}

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
      Table.MinMaxWidth(Canvas, Min, Max, AWidth, AHeight);
      Table.tblWidthAttr := Math.Max(Min, Math.Min(Max, Result));
    end;
    Result := Table.tblWidthAttr;
  end
  else
  begin
    Result := AWidth - LeftSide - RightSide;
    Table.MinMaxWidth(Canvas, Min, Max, AWidth, AHeight);
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

  if Result + LeftSide + RightSide < AWidth then
  begin
    if (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
    begin
      M := AWidth - LeftSide - Result - RightSide;
      LJustify := Justify;
      if LJustify = NoJustify then
        if VarIsAuto(MargArrayO[MarginLeft]) then
          if VarIsAuto(MargArrayO[MarginRight]) then
            LJustify := Centered
          else
            LJustify := Right
        else
          if VarIsAuto(MargArrayO[MarginRight]) then
            LJustify := Left;

      case LJustify of
        Centered:
        begin
          MargArray[MarginLeft]  := M div 2;
          MargArray[MarginRight] := M - MargArray[MarginLeft];
        end;

        Right:
          MargArray[MarginLeft] := M;

        Left:
          MargArray[MarginRight] := M;
      end;
    end;
  end;
end;

function TTableBlock.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
var
  X1, Tmp: Integer;
begin
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.EnterMethod(Self, Format('TTableBlock.DrawLogic1 TagClass = [%s]', [Self.TagClass] ));
{$IFDEF JPM_DEBUGGING_LOGIC1}
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
{$ENDIF}
  if not (Floating in [ALeft, ARight]) then
  begin
    Tmp := X;
    X := Max(Tmp, IMgr.LeftIndent(Y));
    TableIndent := X - Tmp;
    X1 := Min(Tmp + AWidth, IMgr.RightSide(Y));
    AWidth := X1 - X;
  end;
  Result := inherited DrawLogic1(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
{$IFDEF JPM_DEBUGGING_LOGIC}
{$IFDEF JPM_DEBUGGING_LOGIC1}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
{$ENDIF}
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TTableBlock.DrawLogic1');
{$ENDIF}
end;

function TTableBlock.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
begin
  X := X + TableIndent;
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
end;

procedure TTableBlock.AddSectionsToList;
begin {Sections in Table not added only table itself}
  Document.PositionList.Add(Table);
end;

constructor THRBlock.CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode);
var
  T: THRBlock absolute Source;
begin
  inherited CreateCopy(OwnerCell,Source);
  Align := T.Align;
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
  if not IsCopy then
    THorzline(MyHRule).VSize := MargArray[piHeight];

{$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'THRBlock.FindWidth');
{$ENDIF}
end;

{----------------TBlockLI.Create}

constructor TBlockLI.Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties;
  Sy: TElemSymb; APlain: Boolean; AIndexType: ThtChar; AListNumb, ListLevel: Integer);
var
  Tmp: ThtBulletStyle;
  S: ThtString;
begin
  inherited Create(Parent, Attributes, Prop);

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
            case AIndexType of // type="disc|circle|square"
              'd': FListStyleType := lbDisc;
              'c': FListStyleType := lbCircle;
              's': FListStyleType := lbSquare;
            else
              case ListLevel mod 3 of
                1: FListStyleType := lbDisc;
                2: FListStyleType := lbCircle;
                0: FListStyleType := lbSquare;
              end;
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

  if (VarType(MargArrayO[MarginLeft]) in varInt) and (MargArrayO[MarginLeft] = IntNull) then
    case Sy of

      OLSy, ULSy, DirSy, MenuSy, DLSy:
        MargArrayO[MarginLeft] := 0;
        
    else
      MargArrayO[MarginLeft] := ListIndent;
    end;

  FListNumb := AListNumb;
  FListFont := ThtFont.Create;
  FListFont.Assign(Prop.Font);

  S := Prop.GetListStyleImage;
  if S <> '' then
    Image := TImageObj.SimpleCreate(MyCell, S);
end;

constructor TBlockLI.CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode);
var
  T: TBlockLI absolute Source;
begin
  inherited CreateCopy(OwnerCell,Source);
  FListType := T.FListType;
  FListNumb := T.FListNumb;
  FListStyleType := T.FListStyleType;
  if Assigned(T.Image) then
    Image := TImageObj.CreateCopy(MyCell, T.Image);
  FListFont := ThtFont.Create;
  FListFont.Assign(T.ListFont);
end;

destructor TBlockLI.Destroy;
begin
  ListFont.Free;
  Image.Free;
  inherited Destroy;
end;

function TBlockLI.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
begin
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.EnterMethod(Self,'TBlockLI.DrawLogic1');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );

  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
    Image.DrawLogicInline(Canvas, nil, 100, 0);
    if Image.Image = ErrorImage then
      FreeAndNil(Image);
  end;
  Document.FirstLineHtPtr := @FirstLineHt;
  FirstLineHt := 0;
  try
    Result := inherited DrawLogic1(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
  finally
    Document.FirstLineHtPtr := nil;
  end;
{$IFDEF JPM_DEBUGGING_LOGIC}
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
  CodeSite.ExitMethod(Self,'TBlockLI.DrawLogic1');
{$ENDIF}
end;

//-- BG ---------------------------------------------------------- 31.01.2012 --
procedure TBlockLI.SetListFont(const Value: TFont);
begin
  FListFont.Assign(Value);
end;

{----------------TBlockLI.Draw}

function TBlockLI.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;

const
  MaxNumb = 26;
  LowerAlpha: string = 'abcdefghijklmnopqrstuvwxyz';
  HigherAlpha: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LowerRoman: array[1..MaxNumb] of string = ('i', 'ii', 'iii', 'iv', 'v', 'vi',
    'vii', 'viii', 'ix', 'x', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvi', 'xvii',
    'xviii', 'xix', 'xx', 'xxi', 'xxii', 'xxiii', 'xxiv', 'xxv', 'xxvi');
  HigherRoman: array[1..MaxNumb] of string = ('I', 'II', 'III', 'IV', 'V', 'VI',
    'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 'XV', 'XVI', 'XVII',
    'XVIII', 'XIX', 'XX', 'XXI', 'XXII', 'XXIII', 'XXIV', 'XXV', 'XXVI');
var
  NStr: string;
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
    if Assigned(Image) and (Image.Image <> DefImage) and Document.ShowImages then
      Image.DoDraw(Canvas, X - 16, YB - Image.ObjHeight, Image.Image)
    else if not (ListType in [None, Definition]) then
    begin
      if ListStyleType in [lbDecimal, lbLowerAlpha, lbLowerRoman, lbUpperAlpha, lbUpperRoman] then
      begin
        AlphaNumb := Min(ListNumb, MaxNumb);
        case ListStyleType of
          lbLowerAlpha: NStr := LowerAlpha[AlphaNumb];
          lbUpperAlpha: NStr := HigherAlpha[AlphaNumb];
          lbLowerRoman: NStr := LowerRoman[AlphaNumb];
          lbUpperRoman: NStr := HigherRoman[AlphaNumb];
        else
          NStr := IntToStr(ListNumb);
        end;
        Canvas.Font := ListFont;
        Canvas.Font.Color := Document.ThemedColorToRGB(ListFont.Color, htseFont);
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
          Pen.Color := Document.ThemedColorToRGB(ListFont.Color, htseFont);
          Pen.Style := psSolid;
          BrushStyle := Brush.Style;
          BrushColor := Brush.Color;
          Brush.Style := bsSolid;
          Brush.Color := Document.ThemedColorToRGB(ListFont.Color, htseFont);
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

constructor TBodyBlock.Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
var
  PRec: PtPositionRec;
  Image: ThtString;
  Val: TColor;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBodyBlock.Create');
  {$IFDEF JPM_DEBUGGING_STYLES}
  StyleUn.LogProperties(Prop,'Prop');
  CodeSite.AddSeparator;
  {$ENDIF}
  {$ENDIF}
  inherited Create(Parent,Attributes,Prop);
  FPositioning := PosStatic; {7.28}
  Prop.GetBackgroundPos(0, 0, PRec);
  if Prop.GetBackgroundImage(Image) and (Image <> '') then
    Document.SetBackgroundImage(Image, PRec);
  Val := Prop.GetBackgroundColor;
  if Val <> clNone then
    Document.SetBackGround(Val or PalRelative);
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TBodyBlock.Create');
  {$ENDIF}
end;

{----------------TBodyBlock.GetURL}

function TBodyBlock.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): ThtguResultType;
begin
  Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  if not (guTitle in Result) and (BlockTitle <> '') then
  begin
    ATitle := BlockTitle;
    Include(Result, guTitle);
  end;
end;

{----------------TBodyBlock.DrawLogic}

function TBodyBlock.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
var
  ScrollWidth: Integer;
  Lindex, RIndex, AutoCount: Integer;
  SaveID: TObject;
  ClientContentBot: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBodyBlock.DrawLogic1');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
  StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderWidth, AutoCount, MargArray, Document.PixelsPerInch);
  if IsAuto(MargArray[MarginLeft]) then MargArray[MarginLeft] := 0;
  if IsAuto(MargArray[MarginRight]) then MargArray[MarginRight] := 0;
  ApplyBoxSettings(MargArray,Document.UseQuirksMode);

  X := MargArray[MarginLeft] + MargArray[PaddingLeft] + MargArray[BorderLeftWidth];
  ContentWidth := IMgr.Width - (X + MargArray[MarginRight] + MargArray[PaddingRight] + MargArray[BorderRightWidth]);

  DrawTop := MargArray[MarginTop];

  MyCell.IMgr := IMgr;

  SaveID := IMgr.CurrentID;
  Imgr.CurrentID := Self;
  LIndex := IMgr.SetLeftIndent(X, Y);
  RIndex := IMgr.SetRightIndent(X + ContentWidth, Y);

  ContentTop := Y + MargArray[MarginTop] + MargArray[PaddingTop] + MargArray[BorderTopWidth];
  MyCell.DoLogicX(Canvas, X, ContentTop, 0, 0, ContentWidth,
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
  MaxWidth := Max(IMgr.Width, Max(ScrollWidth, ContentWidth) + MargArray[MarginLeft] + MargArray[MarginRight]);
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
  CodeSite.ExitMethod(Self,'TBodyBlock.DrawLogic1');
  {$ENDIF}
end;

{----------------TBodyBlock.Draw}

function TBodyBlock.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  SaveID: TObject;
  Y: Integer;
begin
  Y := YDraw;
  Result := Y + SectionHeight;

  X := IMgr.LfEdge + MargArray[MarginLeft] + MargArray[BorderLeftWidth] + MargArray[PaddingLeft];
  SaveID := IMgr.CurrentID;
  Imgr.CurrentID := Self;
  DrawTheList(Canvas, ARect, ContentWidth, X, IMgr.LfEdge, 0);
  Imgr.CurrentID := SaveID;

  //>-- DZ
  DrawRect.Top    := Y;
  DrawRect.Left   := X;
  DrawRect.Right  := DrawRect.Left + ContentWidth;
  DrawRect.Bottom := DrawRect.Top + DrawHeight;
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
  FDocument := Self;
  inherited Create(nil);
  {$ifdef has_StyleElements}
  FStyleElements := Owner.StyleElements;
  {$Endif}
  FPropStack := THtmlPropStack.Create;
  TheOwner := Owner;
  PPanel := APaintPanel;
  IDNameList := TIDObjectList.Create; //(Self);
  HtmlFormList := THtmlFormList.Create;
  AniList := ThtImageList.Create(False);
  MapList := ThtMap.Create;
  FormControlList := TFormControlObjList.Create(False);
  MissingImages := ThtStringList.Create;
  MissingImages.Sorted := False;
  LinkList := TLinkList.Create;
  PanelList := TPanelObjList.Create(False);
  Styles := THtmlStyleList.Create(Self);
  DrawList := TDrawList.Create;
  PositionList := TSectionBaseList.Create(False);
  TabOrderList := TStringList.Create;
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
  System.Move(T.ShowImages, ShowImages, PtrSub(@Background, @ShowImages) + Sizeof(Integer));
  PreFontName := T.PreFontName;
  HtmlFormList := THtmlFormList.Create; {no copy of list made}
  AniList := ThtImageList.Create(False);
  MapList := ThtMap.Create;
  MissingImages := ThtStringList.Create;
  PanelList := TPanelObjList.Create(False);
  DrawList := TDrawList.Create;
  FDocument := Self;
  inherited CreateCopy(nil, T);
  ScaleX := 1.0;
  ScaleY := 1.0;

  UseQuirksMode := T.UseQuirksMode;
  {$ifdef has_StyleElements}
  StyleElements := T.StyleElements;
  {$Endif}
end;

destructor ThtDocument.Destroy;
begin
  inherited Destroy; // Yunqa.de: Destroy calls Clear, so do this first.
  IDNameList.Free;
  HtmlFormList.Free;
  MapList.Free;
  AniList.Free;
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
    InlineList.Free;
  FPropStack.Free;
end;

{$ifdef has_StyleElements}
procedure ThtDocument.SetStyleElements(const AValue : TStyleElements);
begin
  if AValue <> FStyleElements then
  begin
    FStyleElements := AValue;
    Self.UpdateStyleElements;
  end;
end;

procedure ThtDocument.UpdateStyleElements;
var
  i: Integer;
begin
  if FormControlList <> nil then
    for i := 0 to FormControlList.Count - 1 do
      FormControlList[i].TheControl.StyleElements := FStyleElements;
end;
{$endif}

function ThtDocument.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject {TImageFormControlObj};
  out ATitle: ThtString): ThtguResultType;
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

//-- BG ---------------------------------------------------------- 14.02.2016 --
function ThtDocument.GetViewPort: TRect;
begin
  if Printing then
    Result := FPageArea
  else
    Result := PPanel.ClientRect;
end;

procedure ThtDocument.LButtonDown(Down: Boolean);
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
  Image: ThtImage;
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

  for I := 0 to AniList.Count - 1 do
  begin
    Image := AniList[I];
    if Image is ThtGifImage then
      with ThtGifImage(Image).Gif do
        if ShowIt then
          CheckTime(PPanel);
  end;

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
  for I := 0 to HtmlFormList.Count - 1 do
    with HtmlFormList[I] do
      for J := 0 to ControlList.Count - 1 do
        ControlList[J].ShowIt := False;
  for I := 0 to PanelList.Count - 1 do
    PanelList[I].ShowIt := False; {same for panels}
end;

procedure ThtDocument.SetYOffset(Y: Integer);
begin
  YOff := Y;
  YOffChange := True;
  HideControls;
end;

//-- BG ---------------------------------------------------------- 08.01.2023 --
function ThtDocument.ThemedColor(AColor: TColor; AStyleElement: ThtStyleElement): TColor;
begin
  if TheOwner <> nil then
    Result := TheOwner.ThemedColor( AColor, AStyleElement )
  else
  begin
    Result := AColor;
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
{$endif}
  end;
end;

//-- BG ---------------------------------------------------------- 08.01.2023 --
function ThtDocument.ThemedColorToRGB(AColor: TColor; AStyleElement: ThtStyleElement): TColor;
begin
  if TheOwner <> nil then
    Result := TheOwner.ThemedColorToRGB( AColor, AStyleElement )
  else
    Result := ColorToRGB( ThemedColor( AColor, AStyleElement ));
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
  if BackgroundImageLoaded and (BackgroundImageName <> '') then
    ImageCache.DecUsage(BackgroundImageName);
  BackgroundImageName := '';
  BackgroundImageLoaded := False;
  AniList.Clear;
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
  HtmlFormList.Clear;
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
// extracted from CopyToClipboardAsText(), GetSelLength() and GetSelTextBuf().
function ThtDocument.CopyToBuffer(Buffer: TSelTextCount): Integer;
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
        CopySelectedText;
      end;
    end;
    Result := CB.Terminate;
  finally
    FreeAndNil(CB);
  end;
end;

function ThtDocument.GetSelLength: Integer;
begin
  if SelE > SelB then
    Result := CopyToBuffer(TSelTextCount.Create)
  else
    Result := 0; {nothing to do}
end;

//------------------------------------------------------------------------------
function ThtDocument.GetSelTextBuf(Buffer: PWideChar; BufSize: Integer): Integer;
begin
  if SelE > SelB then
    Result := CopyToBuffer(TSelTextBuf.Create(Buffer, BufSize))
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
  var ScrollWidth, Curs: Integer): Integer;
var
  I, J: Integer;
  Image: ThtImage;
  Obj: TObject;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'ThtDocument.DoLogic');
   {$ENDIF}
  Inc(CycleNumber);
  TableNestLevel := 0;
  InLogic2 := False;
  if Assigned(Timer) then
    Timer.Enabled := False;
  for I := 0 to HtmlFormList.Count - 1 do
    HtmlFormList[I].SetSizes(Canvas);
  SetTextJustification(Canvas.Handle, 0, 0);
  TInlineList(InlineList).NeedsConverting := True;

{set up the tab order for form controls according to the TabIndex attributes}
  if Assigned(TabOrderList) and (TabOrderList.Count > 0) then
  begin
    J := 0; {tab order starts with 0}
    for I := 0 to TabOrderList.Count - 1 do {list is sorted into proper order}
    begin
      Obj := TabOrderList.Objects[I];
      if Obj is TFormControlObj then
      begin
        TFormControlObj(Obj).TabOrder := J;
        Inc(J);
      end
      else if Obj is ThtTabControl then
      begin
        ThtTabControl(Obj).TabOrder := J;
        Inc(J);
      end
      else
        Assert(False, 'Unexpected item in TabOrderList');
    end;
    TabOrderList.Clear; {only need do this once}
  end;

  Result := inherited DoLogic(Canvas, Y, Width, AHeight, BlHt, ScrollWidth, Curs);

  for I := 0 to AniList.Count - 1 do
  begin
    Image := AniList[I];
    if Image is ThtGifImage then
      with ThtGifImage(Image).Gif do
      begin
        ShowIt := False; // when gif is drawn, it will set it to true again. Avoids animating invisible gifs.
        Animate := False; {starts iteration count from 1}
        if not Self.IsCopy then
          Animate := True;
      end;
  end;
  if not IsCopy and not Assigned(Timer) then
  begin
    Timer := TTimer.Create(TheOwner);
    Timer.Interval := 50;
    Timer.OnTimer := CheckGIFList;
  end;
  if Assigned(Timer) then
    Timer.Enabled := AniList.Count >= 1;
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
  Showing: Boolean;

{$IFNDEF FastRadio}
  function ActiveInList: Boolean; {see if active control is a form control}
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
    if DrawList.Count > 0 then
    begin
      DrawList.DrawImages;
      DrawList.Clear;
      Printed := True;
    end;
  finally
    if OldPal <> 0 then
      SelectPalette(Canvas.Handle, OldPal, True);
  end;
  if YOffChange then
  begin
    AdjustFormControls;
  {Hide all TPanelObj's that aren't displayed}
    for I := 0 to PanelList.Count - 1 do
      with PanelList[I] do
        if not ShowIt then
          Panel.Hide;
  end;
  if YOffChange or XOffChange then
  begin
{$ifdef LCL}
    PPanel.Invalidate;
{$endif}
    XOffChange := False;
    YOffChange := False;
  end;
  if Assigned(Timer) then
    Timer.Enabled := AniList.Count >= 1;
end;

procedure ThtDocument.SetFonts(const Name, PreName: ThtString; ASize: Integer;
  AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor; LnksActive, LinkUnderLine: Boolean;
  ACodePage: TBuffCodePage; ACharSet: TFontCharSet; MarginHeight, MarginWidth: Integer);
begin
  Styles.PixelsPerInch := PixelsPerInch;
  Styles.Initialize(Name, PreName, ASize, AColor, AHotspot, AVisitedColor,
    AActiveColor, LinkUnderLine, ACodePage, ACharSet, MarginHeight, MarginWidth);
  InitializeFontSizes(ASize);
  PreFontName := htStringToString(PreName);
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

procedure ThtDocument.SetBackgroundImage(const Name: ThtString; const APrec: PtPositionRec);
begin
  BackgroundImage := nil;
  BackgroundImageName := Name;
  BackgroundImageLoaded := False;
  BackgroundImagePosition := APrec;
end;

//------------------------------------------------------------------------------
procedure ThtDocument.InsertMissingImage(const UName: ThtString; J: Integer; Error: Boolean; out Reformat: Boolean);
var
  Rformat: Boolean;
  Obj: TObject;
begin
  Reformat := False;
  while J >= 0 do
  begin
    Obj := MissingImages.Objects[J];
    if Obj = Self then
    begin
      if not IsCopy and not Error then
      begin
        BackgroundImageLoaded := False; {the background image, set to load}
        GetBackgroundImage();
      end;
    end
    else if Obj is TImageObj then
    begin
      TImageObj(Obj).InsertImage(UName, Error, Rformat);
      Reformat := Reformat or Rformat;
    end;
    MissingImages.Delete(J);
    J := MissingImages.IndexOf(UName);
  end;
end;

//------------------------------------------------------------------------------
procedure ThtDocument.InsertImage(const Src: ThtString; Stream: TStream; out Reformat: Boolean);
var
  UName: ThtString;
  I, J: Integer;
  Image: ThtImage;
  Error: Boolean;
  Obj: TObject;
  Transparent: ThtImageTransparency;
begin
  Image := nil;
  Error := False;
  Reformat := False;
  UName := htTrim(Src);
  I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
  J := MissingImages.IndexOf(UName); {see if it's in missing image list}
  if (I = -1) and (J >= 0) then
  begin
    Transparent := itrNone;
    Obj := MissingImages.Objects[J];
    if (Obj is TImageObj) then
    begin
      Transparent := TImageObj(Obj).Transparent;
    end;
    Image := LoadImageFromStream(Stream, Transparent);
    if Image <> nil then {put in Cache}
    begin
      ImageCache.AddObject(UName, Image); {put new bitmap in list}
      ImageCache.DecUsage(UName); {this does not count as being used yet}
    end
    else
      Error := True; {bad stream or Nil}
  end;

  if (I >= 0) or (Image <> nil) or Error then {a valid image in the Cache or Bad stream}
    InsertMissingImage(UName, J, Error, Reformat);
end;

//-- BG ---------------------------------------------------------- 10.01.2015 --
procedure ThtDocument.InsertImage(const Src: ThtString; Bitmap: TBitmap; Color: TColor; Transparent: ThtImageTransparency; OwnsBitmap: Boolean; out Reformat: Boolean);
var
  UName: ThtString;
  I, J: Integer;
  Image: ThtImage;
  Error: Boolean;
begin
  Image := nil;
  Error := False;
  Reformat := False;
  UName := htUpperCase(htTrim(Src));
  I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
  J := MissingImages.IndexOf(UName); {see if it's in missing image list}
  if (I = -1) and (J >= 0) then
  begin
    if Bitmap <> nil then
    begin
        if Color <> -1 then
          Transparent := itrIntrinsic;
        Image := ThtBitmapImage.Create(Bitmap, Transparent, Color, OwnsBitmap);
    end;

    if Image <> nil then {put in Cache}
    begin
      ImageCache.AddObject(UName, Image); {put new bitmap in list}
      ImageCache.DecUsage(UName); {this does not count as being used yet}
    end
    else
      Error := True; {bad stream or Nil}
  end;

  if (I >= 0) or Assigned(Image) or Error then {a valid image in the Cache or Bad stream}
    InsertMissingImage(UName, J, Error, Reformat);
end;

//------------------------------------------------------------------------------
function ThtDocument.GetTheImage(const BMName: ThtString; var Transparent: ThtImageTransparency; out FromCache, Delay: Boolean): ThtImage;
{Transparent may be set to itrNone or itrLLCorner on entry but may discover it's itrIntrinsic here}

  procedure GetTheBitmap;
  {Note: bitmaps gotten by OnBitmapRequest are on "loan".  We do not destroy them}
  var
    Color: TColor;
    Bitmap: TBitmap;
  begin
    if Assigned(GetBitmap) then
    begin {the OnBitmapRequest event}
      Bitmap := nil;
      Color := -1;
      GetBitmap(TheOwner, BMName, Bitmap, Color);
      if Bitmap = WaitBitmap then
        Delay := True
      else if Bitmap = ErrorBitmap then
        Result := nil
      else if Bitmap <> nil then
      begin
        if Color <> -1 then
          Transparent := itrIntrinsic;
        try
          Result := ThtBitmapImage.Create(Bitmap, Transparent, Color, False);
        finally
          if Assigned(GottenBitmap) then
            GottenBitmap(TheOwner, BMName, Bitmap, Color);
        end;
      end;
    end;
  end;

  procedure GetTheStream;
  {Note: streams gotten by OnImageRequest are on "loan".  We do not destroy them}
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
      begin
        try
          Result := LoadImageFromStream(Stream, Transparent);
        finally
          if Assigned(GottenImage) then
            GottenImage(TheOwner, BMName, Stream);
        end;
      end;
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
      Name := htString(StringReplace(htStringToString(Name), '%0A', #$0A, [rfReplaceAll]));
      Source := TStringStream.Create(Copy(htStringToString(Name), I + 8, MaxInt));
      try
        Stream := TMemoryStream.Create;
        try
          {$IFDEF Compiler28_Plus}
          TNetEncoding.Base64.Decode(Source, Stream);
          {$ELSE}
          DecodeStream(Source, Stream);
          {$ENDIF}
          Result := LoadImageFromStream(Stream, Transparent);
        finally
          Stream.Free;
        end;
      finally
        Source.Free;
      end;
    end;
  end;

  procedure GetTheFile(Name: ThtString);
  var
    Stream: TStream;
    Scheme, Specific, ResType: ThtString;
  begin
    Name := TheOwner.HtmlExpandFilename(Name);
    SplitScheme(Name, Scheme, Specific);
    if Scheme = 'res' then
    begin
      Specific := HTMLToRes(Name, ResType);
      Stream := TResourceStream.Create(HInstance, htStringToString(Specific), PChar({$ifdef LCL}string(ResType){$else}ResType{$endif}) );
    end
    else if htFileExists(Name) then
      Stream := TFileStream.Create( htStringToString(Name), fmOpenRead or fmShareDenyWrite)
    else
      Stream := nil;

    if Stream <> nil then
      try
        Result := LoadImageFromStream(Stream, Transparent);
      finally
        Stream.Free;
      end;
  end;

var
  UName, Name: ThtString;
  I: Integer;
begin
  Result := nil;
  Delay := False;
  FromCache := False;
  if BMName <> '' then
  begin
    Name := htTrim(BMName);
    //UName := htUpperCase(Name);
    UName := Name;
    I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
    if I >= 0 then
    begin {yes, handle the case where the image is already loaded}
      Result := ImageCache.GetImage(I);
      FromCache := True;
    end
    else
    begin
      {The image is not loaded yet, need to get it}
      if Copy(Name, 1, 11) = 'data:image/' then
        GetTheBase64(Name)
      else
      begin
        GetTheBitmap;
        if (Result = nil) and not Delay then
          GetTheStream;
        if (Result = nil) and not Delay then
          GetTheFile(BMName);
      end;

      if Result <> nil then {put in Image List for use later also}
        ImageCache.AddObject(UName, Result); {put new image in list}
    end;
  end;
end;

//------------------------------------------------------------------------------
function ThtDocument.FindSectionAtPosition(Pos: Integer; out TopPos, Index: Integer): TSectionBase;
var
  I: Integer;
begin
  for I := PositionList.Count - 1 downto 0 do
  begin
    Result := PositionList[I];
    if Result.YPosition <= Pos then
    begin
      TopPos := Result.YPosition;
      Index := I;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure ThtDocument.GetBackgroundImage;
var
  Dummy1: ThtImageTransparency;
  FromCache, Delay: Boolean;
  Rslt: ThtString;
  I: Integer;
  UName: ThtString;
begin
  UName := htTrim(BackgroundImageName);
  if ShowImages and (UName <> '') then
    if BackgroundImage = nil then
    begin
      if BackgroundImageLoaded then
      begin
        I := ImageCache.IndexOf(UName); {first see if the bitmap is already loaded}
        if I >= 0 then
          BackgroundImage := ImageCache.GetImage(I);
      end
      else
      begin
        Dummy1 := itrNone;
        if not Assigned(GetBitmap) and not Assigned(GetImage) then
          BackgroundImageName := TheOwner.HtmlExpandFilename(BackgroundImageName)
        else if Assigned(OnExpandName) then
        begin
          OnExpandName(TheOwner, BackgroundImageName, Rslt);
          BackgroundImageName := Rslt;
        end;
        BackgroundImage := GetTheImage(BackgroundImageName, Dummy1, FromCache, Delay); {might be Nil}
        if Delay then
          MissingImages.AddObject(htTrim(BackgroundImageName), Self);
        BackgroundImageLoaded := True;
      end;
      if (BackgroundImage <> nil) and not IsCopy then
        if BackgroundImage.IsAnimated then
        begin
          AniList.Add(BackgroundImage);
          BackgroundImage.Animate := True;
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure ThtDocument.GetFormControlData(FormData: TFormData);
var
  I: Integer;
begin
  for I := 0 to HtmlFormList.Count - 1 do
    FormData.Add(HtmlFormList[I].GetFormSubmission);
end;

procedure ThtDocument.SetFormControlData(FormData: TFormData);
var
  I: Integer;
begin
  try
    for I := 0 to Min(FormData.Count, HtmlFormList.Count) - 1 do
      HtmlFormList[I].SetFormData(FormData[I]);
  except
  end;
end;

//-- BG ---------------------------------------------------------- 14.02.2016 --
procedure ThtDocument.SetPageArea(const Value: TRect);
begin
  FPageArea := Value;
end;

//-- BG ---------------------------------------------------------- 16.07.2017 --
procedure ThtDocument.SetPrinted(const Value: Boolean);
begin
  FPrinted := Value;
end;

//------------------------------------------------------------------------------
function ThtDocument.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
begin
  Result := inherited FindDocPos(SourcePos, Prev);
  if Result < 0 then {if not found return 1 past last ThtChar}
    Result := Len;
end;

//------------------------------------------------------------------------------
function ThtDocument.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean;
var
  Beyond: Boolean;
begin
  Beyond := Cursor >= Len;
  if Beyond then
    Cursor := Len - 1;
  Result := inherited CursorToXY(Canvas, Cursor, X, Y);
  if Beyond then
    X := X + 15;
end;

procedure ThtDocument.ProcessInlines(SIndex: Integer; Prop: TProperties; Start: Boolean);
{called when an inline property is found to specify a border}
var
  I, EmSize, ExSize: Integer;
  Result: ThtInlineRec;
  MargArrayO: ThtVMarginArray;
  Dummy1: Integer;
begin
 {$ifdef JPM_DEBUGGING}
 CodeSite.EnterMethod(Self,'ThtDocument.ProcessInlines');
 {$endif}
  with TInlineList(InlineList) do
  begin
    if Start then
    begin {this is for border start}
      Result := ThtInlineRec.Create;
      Add(Result);
      with Result do
      begin
        StartBDoc := SIndex; {Source index for border start}
        IDB := Prop.ID; {property ID}
        EndB := 999999; {end isn't known yet}
        Prop.GetVMarginArray(MargArrayO);
        EmSize := Prop.EmSize;
        ExSize := Prop.ExSize;
        ConvMargArray(MargArrayO, 200, 200, EmSize, ExSize, 0{4}, Dummy1, MargArray, PixelsPerInch);
      end;
    end
    else {this call has end information}
      for I := Count - 1 downto 0 do {the record we want is probably the last one}
      begin
        Result := Items[I];
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
    with Items[I] do
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
    Result := Items[I].StartB
  else
    Result := 99999999;
end;

{$ifdef UseGenerics}
{$else}
//-- BG ---------------------------------------------------------- 06.10.2016 --
function TInlineList.Get(Index: Integer): ThtInlineRec;
begin
  Result := inherited Get(Index);
end;
{$endif}

function TInlineList.GetEndB(I: Integer): Integer;
begin
  if NeedsConverting then
    AdjustValues;
  if (I < Count) and (I >= 0) then
    Result := Items[I].EndB
  else
    Result := 99999999;
end;

{ TCellObjBase }

//-- BG ---------------------------------------------------------- 19.02.2013 --
procedure TCellObjBase.AssignTo(Destin: TCellObjBase);
begin
  Move(FColSpan, Destin.FColSpan, PtrSub(@FSpecHt, @FColSpan) + sizeof(FSpecHt) );
end;

{ TDummyCellObj }

//-- BG ---------------------------------------------------------- 19.02.2013 --
function TDummyCellObj.Clone(Parent: TBlock): TCellObjBase;
begin
  Result := TDummyCellObj.Create(RowSpan);
  AssignTo(Result);
end;

//-- BG ---------------------------------------------------------- 19.02.2013 --
constructor TDummyCellObj.Create(RSpan: Integer);
begin
  inherited Create;
  FColSpan := 0;
  FRowSpan := RSpan;
end;

//-- BG ---------------------------------------------------------- 19.02.2013 --
procedure TDummyCellObj.Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacingHorz, CellSpacingVert: Integer; Border: Boolean; Light,
  Dark: TColor);
begin
  //abstract: inherited Draw(Canvas,ARect,X,Y,CellSpacing,Border,Light,Dark);
end;

//-- BG ---------------------------------------------------------- 19.02.2013 --
procedure TDummyCellObj.DrawLogic2(Canvas: TCanvas; Y, CellSpacingHorz,CellSpacingVert: Integer; var Curs: Integer);
begin
  //abstract: inherited DrawLogic2(Canvas,Y,CellSpacing,Curs);
end;

//-- BG ---------------------------------------------------------- 19.02.2013 --
function TDummyCellObj.GetCell: TCellObjCell;
begin
  Result := nil;
end;

{ TCellObj }

//-- BG ---------------------------------------------------------- 19.02.2013 --
procedure TCellObj.AssignTo(Destin: TCellObjBase);
var
  CellObj: TCellObj absolute Destin;
begin
  inherited AssignTo(Destin);
  if Destin is TCellObj then
  begin
    Move(FWd, CellObj.FWd, PtrSub(@FCell, @FWd));

    if CellObj.Cell.Document.PrintTableBackground then
    begin
      CellObj.Cell.BkGnd := Cell.BkGnd;
      CellObj.Cell.BkColor := Cell.BkColor;
      if Assigned(BGImage) then
        // TODO: BG, 26.08.2013: is this correct?
        CellObj.BGImage := TImageObj.CreateCopy(CellObj.Cell, BGImage);
    end
    else
      CellObj.Cell.BkGnd := False;
    CellObj.MargArrayO := MargArrayO;
    CellObj.MargArray := MargArray;
  end;
end;

//-- BG ---------------------------------------------------------- 19.02.2013 --
function TCellObj.Clone(Parent: TBlock): TCellObjBase;
begin
  Result := TCellObj.CreateCopy(Parent, Self);
end;

constructor TCellObj.Create(Parent: TTableBlock; AVAlign: ThtAlignmentStyle; Attr: TAttributeList; Prop: TProperties);
{Note: on entry Attr and Prop may be Nil when dummy cells are being created}
var
  I, AutoCount: Integer;
  Color: TColor;
  BackgroundImage: ThtString;
  Algn: ThtAlignmentStyle;
begin
 {$ifdef JPM_DEBUGGING}
 CodeSite.EnterMethod(Self,'TCellObj.Create');
 {$endif}
  inherited Create;
  FCell := TCellObjCell.Create(Parent);
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
            Cell.BkGnd := TryStrToColor(Name, False, Cell.BkColor);

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
    if Parent.Document.UseQuirksMode then
      Prop.GetVMarginArrayDefBorder(MargArrayO, clSilver)
    else
      Prop.GetVMarginArray(MargArrayO);
    EmSize := Prop.EmSize;
    ExSize := Prop.ExSize;
    ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, 0, AutoCount, MargArray, Cell.Document.PixelsPerInch);
    if VarIsStr(MargArrayO[piWidth]) and (MargArray[piWidth] >= 0) then
      FSpecWd := ToSpecWidth(MargArray[piWidth], MargArrayO[piWidth]);
    if VarIsStr(MargArrayO[piMinWidth]) and (MargArray[piMinWidth] >= 0) then
      FSpecWdMin := ToSpecWidth(MargArray[piMinWidth], MargArrayO[piMinWidth]);
    if VarIsStr(MargArrayO[piMaxWidth]) and (MargArray[piMaxWidth] >= 0) then
      FSpecWdMax := ToSpecWidth(MargArray[piMaxWidth], MargArrayO[piMaxWidth]);
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
      BGImage := TImageObj.SimpleCreate(Cell, BackgroundImage);
      Prop.GetBackgroundPos(EmSize, ExSize, FPRec);
    end;

  {In the following, Padding widths in percent aren't accepted}
    ConvMargArrayForCellPadding(MargArrayO, EmSize, ExSize, MargArray, Cell.Document.PixelsPerInch);
    FPad.Top := MargArray[PaddingTop];
    FPad.Right := MargArray[PaddingRight];
    FPad.Bottom := MargArray[PaddingBottom];
    FPad.Left := MargArray[PaddingLeft];

    HasBorderStyle := False;
    if ThtBorderStyle(MargArray[BorderTopStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Top := MargArray[BorderTopWidth];
    end;
    if ThtBorderStyle(MargArray[BorderRightStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Right := MargArray[BorderRightWidth];
    end;
    if ThtBorderStyle(MargArray[BorderBottomStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Bottom := MargArray[BorderBottomWidth];
    end;
    if ThtBorderStyle(MargArray[BorderLeftStyle]) <> bssNone then
    begin
      HasBorderStyle := True;
      FBrd.Left := MargArray[BorderLeftWidth];
    end;

    Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
    ShowEmptyCells := Prop.ShowEmptyCells;
    HideOverflow := Prop.IsOverflowHidden;
  end;
 {$ifdef JPM_DEBUGGING}
 CodeSite.ExitMethod(Self,'TCellObj.Create');
 {$endif}
end;

constructor TCellObj.CreateCopy(Parent: TBlock; T: TCellObj);
begin
  inherited Create;
  FCell := TCellObjCell.CreateCopy(Parent, T.Cell);
  T.AssignTo(Self);
end;

destructor TCellObj.Destroy;
begin
  Cell.Free;
  BGImage.Free;
  TiledImage.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetBorderBottom: Integer;
begin
  Result := FBrd.Bottom;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetBorderLeft: Integer;
begin
  Result := FBrd.Left;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetBorderRight: Integer;
begin
  Result := FBrd.Right;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetBorderTop: Integer;
begin
  Result := FBrd.Top;
end;

//-- BG ---------------------------------------------------------- 19.02.2013 --
function TCellObj.GetCell: TCellObjCell;
begin
  Result := FCell;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetPaddingBottom: Integer;
begin
  Result := FPad.Bottom;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetPaddingLeft: Integer;
begin
  Result := FPad.Left;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetPaddingRight: Integer;
begin
  Result := FPad.Right;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
function TCellObj.GetPaddingTop: Integer;
begin
  Result := FPad.Top;
end;

{----------------TCellObj.InitializeCell}

procedure TCellObj.Initialize(TablePadding: Integer; const BkImageName: ThtString;
  const APRec: PtPositionRec; Border: Boolean);
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
    BGImage := TImageObj.SimpleCreate(Cell, BkImageName);
    PRec := APrec;
  end;
end;

{----------------TCellObj.DrawLogic2}

procedure TCellObj.DrawLogic2(Canvas: TCanvas; Y, CellSpacingHorz, CellSpacingVert: Integer; var Curs: Integer);
var
  Dummy: Integer;
  Tmp: Integer;
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellObj.DrawLogic2');
  CodeSite.SendFmtMsg('Y           = [%d]',[Y]);
  CodeSite.SendFmtMsg('CellSpacing = [%d,%d]',[CellSpacingHorz, CellSpacingVert]);
  CodeSite.SendFmtMsg('Curs         = [%d]',[Curs]);
  CodeSite.AddSeparator;
   {$ENDIF}
  if Cell.Count > 0 then
  begin
    Tmp := Ht - VSize - (VrSpace + CellSpacingVert);
    case VAlign of
      ATop: YIndent := 0;
      AMiddle: YIndent := Tmp div 2;
      ABottom, ABaseline: YIndent := Tmp;
    end;
    Dummy := 0;
    Cell.DoLogic(Canvas, Y + FPad.Top + FBrd.Top + CellSpacingVert + YIndent, Wd - (HzSpace + CellSpacingHorz),
      Ht - VrSpace - CellSpacingVert, 0, Dummy, Curs);
  end;
  if Assigned(BGImage) and Cell.Document.ShowImages then
  begin
    BGImage.DrawLogicInline(Canvas, nil, 100, 0);
    if BGImage.Image = ErrorImage then
      FreeAndNil(BGImage)
    else
    begin
      BGImage.ClientSizeKnown := True; {won't need reformat on InsertImage}
      NeedDoImageStuff := True;
    end;
  end;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TCellObj.DrawLogic2');
   {$ENDIF}
end;

{----------------TCellObj.Draw}

procedure TCellObj.Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacingHorz, CellSpacingVert: Integer;
  Border: Boolean; Light, Dark: TColor);
var
  YO: Integer;
  BL, BT, BR, BB, PL, PT, PR, PB: Integer;
  ImgOK: Boolean;
  IT, IH, FT, Rslt: Integer;
  Rgn, SaveRgn: HRgn;
  Point: TPoint;
  SizeV, SizeW: TSize;
  HF, VF: double;
  BRect: TRect;
  IsVisible: Boolean;
begin
  YO := Y - Cell.Document.YOff;

  BL := X + CellSpacingHorz; {Border left and right}
  BR := X + Wd;
  PL := BL + FBrd.Left; {Padding left and right}
  PR := BR - FBrd.Right;

  BT := YO + CellSpacingVert; {Border Top and Bottom}
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
      else if BGImage.Image <> DefImage then
      begin
        if BGImage.Image = ErrorImage then {Skip the background image}
          FreeAndNil(BGImage)
        else
        try
          BGImage.Image.TileImage( PRec, Wd - CellSpacingHorz, Ht - CellSpacingVert, TiledImage, NoMask);
          if Cell.IsCopy then
            TiledImage.Bitmap.HandleType := bmDIB;
        except {bad image, get rid of it}
          FreeAndNil(BGImage);
          FreeAndNil(TiledImage);
        end;
        NeedDoImageStuff := False;
      end;
    end;

    ImgOK := not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Image <> DefImage) and Cell.Document.ShowImages;

    if Cell.BkGnd then
    begin
      Canvas.Brush.Color := Cell.Document.ThemedColorToRGB(Cell.BkColor, htseClient) or PalRelative;
      Canvas.Brush.Style := bsSolid;
      if not Cell.IsCopy or not ImgOK then
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

    if ImgOK and (TiledImage <> nil) then
    begin
      if not Cell.IsCopy then
        TiledImage.DrawUnstretched(Canvas, PL, FT, Pr - PL, IH, 0, IT, Cell.BkGnd)
      else
        TiledImage.PrintUnstretched(Canvas, PL, FT, Pr - PL, IH, 0, IT, Cell.BkGnd)
      ;
    end;
  except
  end;

  IsVisible := (YO < ARect.Bottom + 200) and (YO + Ht > -200);
  try
    if IsVisible and (Cell.Count > 0) then
    begin
      Rslt := 0;
      Rgn := 0;
      SaveRgn := 0;
      if HideOverflow then
      begin
      {clip cell contents to prevent overflow.  First check to see if there is
       already a clip region}
        SaveRgn := CreateRectRgn(0, 0, 1, 1);
        Rslt := GetClipRgn(Canvas.Handle, SaveRgn); {Rslt = 1 for existing region, 0 for none}
      {Form the region for this cell}
        GetWindowOrgEx(Canvas.Handle, Point); {when scrolling or animated Gifs, canvas may not start at X=0, Y=0}
        if not Cell.Document.Printing then
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
      end;
      try
        Cell.Draw(Canvas, ARect, Wd - HzSpace - CellSpacingHorz,
          X + FPad.Left + FBrd.Left + CellSpacingHorz,
          Y + FPad.Top + FBrd.Top + YIndent, ARect.Left, 0); {possibly should be IRgn.LfEdge}
      finally
        if HideOverflow then
        begin
          if Rslt = 1 then {restore any previous clip region}
            SelectClipRgn(Canvas.Handle, SaveRgn)
          else
            SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(Rgn);
          DeleteObject(SaveRgn);
        end;
      end;
    end;
  except
  end;

  Cell.DrawYY := Y;
  if IsVisible and ((Cell.Count > 0) or ShowEmptyCells) then
    try
      DrawBorder(Canvas, Rect(BL, BT, BR, BB), Rect(PL, PT, PR, PB),
        htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
        htStyles(ThtBorderStyle(MargArray[BorderLeftStyle]), ThtBorderStyle(MargArray[BorderTopStyle]), ThtBorderStyle(MargArray[BorderRightStyle]), ThtBorderStyle(MargArray[BorderBottomStyle])),
        MargArray[BackgroundColor], Cell.Document.Printing, Cell.Document.ThemedColorToRGB);
    except
    end;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetBorderBottom(const Value: Integer);
begin
  FBrd.Bottom := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetBorderLeft(const Value: Integer);
begin
  FBrd.Left := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetBorderRight(const Value: Integer);
begin
  FBrd.Right := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetBorderTop(const Value: Integer);
begin
  FBrd.Top := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetPaddingBottom(const Value: Integer);
begin
  FPad.Bottom := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetPaddingLeft(const Value: Integer);
begin
  FPad.Left := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetPaddingRight(const Value: Integer);
begin
  FPad.Right := Value;
end;

//-- BG ---------------------------------------------------------- 08.01.2012 --
procedure TCellObj.SetPaddingTop(const Value: Integer);
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
      with Attr[I] do
        case Which of
          BGColorSy:
            BkGnd := TryStrToColor(Name, False, BkColor);
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

constructor TCellList.CreateCopy(Parent: TBlock; T: TCellList);
var
  I: Integer;
begin
  inherited Create;
  BreakBefore := T.BreakBefore;
  BreakAfter := T.BreakAfter;
  KeepIntact := T.KeepIntact;
  RowType := T.Rowtype;
  for I := 0 to T.Count - 1 do
    if Assigned(T[I]) then
      Add(T[I].Clone(Parent))
    else
      Add(nil);
end;

procedure TCellList.Add(CellObjBase: TCellObjBase);
var
  CellObj: TCellObj absolute CellObjBase;
begin
  inherited Add(CellObjBase);
  if CellObjBase is TCellObj then
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
      if Items[I] is TCellObj then
        with TCellObj(Items[I]).Cell do
          if not BkGnd then
          begin
            BkGnd := True;
            BkColor := Self.BkColor;
          end;
end;

{----------------TCellList.DrawLogic1}

function TCellList.DrawLogicA(Canvas: TCanvas; const Widths: TIntArray; Span, CellSpacingHorz,CellSpacingVert, AHeight, Rows: Integer;
  out Desired: Integer; out Spec, More: Boolean): Integer;
{Find vertical size of each cell, Row height of this row.  But final Y position
 is not known at this time.
 Rows is number rows in table.
 AHeight is for calculating percentage heights}
var
  I, Dummy: Integer;
  DummyCurs, GuessHt: Integer;
begin
  Result := 0;
  Desired := 0;
  Spec := False;
  DummyCurs := 0;
  More := False;
  for I := 0 to Count - 1 do
  begin
    if Items[I] is TCellObj then
      with TCellObj(Items[I]) do
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

            VSize := Cell.DoLogic(Canvas, 0, Wd - HzSpace - CellSpacingHorz, Max(0, GuessHt - VrSpace), 0, Dummy, DummyCurs);
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

{----------------TCellList.DrawLogicB}

procedure TCellList.DrawLogicB(Canvas: TCanvas; Y, CellSpacingHorz, CellSpacingVert: Integer; var Curs: Integer);
{Calc Y indents. Set up Y positions of all cells.}
var
  I: Integer;
  CellObj: TCellObjBase;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCellObj.DrawLogicB');
  CodeSite.SendFmtMsg('Y           = [%d]',[Y]);
  CodeSite.SendFmtMsg('CellSpacing = [%d,%d]',[CellSpacingHorz, CellSpacingVert]);
  CodeSite.SendFmtMsg('Curs         = [%d]',[Curs]);
  CodeSite.AddSeparator;
{$ENDIF}
  for I := 0 to Count - 1 do
  begin
    CellObj := Items[I];
    if (CellObj <> nil) and (CellObj.ColSpan > 0) and (CellObj.RowSpan > 0) then
      CellObj.DrawLogic2(Canvas, Y, CellSpacingHorz,CellSpacingVert, Curs);
  end;
{$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Curs         = [%d]',[Curs]);
  CodeSite.ExitMethod(Self,'TCellObj.DrawLogicB');
{$ENDIF}
end;

{$ifdef UseGenerics}
{$else}
//-- BG ---------------------------------------------------------- 12.09.2010 --
function TCellList.GetCellObj(Index: Integer): TCellObjBase;
begin
  Result := inherited Get(Index);
end;
{$endif}

{----------------TCellList.Draw}

function TCellList.Draw(Canvas: TCanvas; Document: ThtDocument; const ARect: TRect; const Widths: TIntArray;
  X, Y, YOffset, CellSpacingHorz,CellSpacingVert: Integer; Border: Boolean; Light, Dark: TColor; MyRow: Integer): Integer;
var
  I, SpacingVert: Integer;
  YO: Integer;
  CellObj: TCellObjBase;
begin
  YO := Y - YOffset;
  Result := RowHeight + Y;
  SpacingVert := CellSpacingVert div 2;

  with Document do {check CSS page break properties}
    if Printing then
      if BreakBefore then
      begin
        if YO > ARect.Top then {page-break-before}
        begin
          if Y + SpacingVert < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Y + SpacingVert;
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
          if Y + SpacingVert < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Y + SpacingVert;
          end;
          Exit;
        end;
      end
      else if BreakAfter then
        if ARect.Top + YOff < Result then {page-break-after}
          if Result + SpacingVert < PageBottom then
          begin
            PageShortened := True;
            PageBottom := Result + SpacingVert;
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
      CellObj := Items[I];
      if (CellObj <> nil) and (CellObj.ColSpan > 0) and (CellObj.RowSpan > 0) then
        CellObj.Draw(Canvas, ARect, X, Y, CellSpacingHorz,CellSpacingVert, Border, Light, Dark);
      X := X + Widths[I];
    end;
end;

//-- BG ---------------------------------------------------------- 09.02.2013 --
function TryStrToTableFrame(const Str: ThtString; var Frame: TTableFrame): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  Upr: ThtString;
begin
  Upr := htUpperCase(Str);
  Result := True;
  if htCompareStr(Upr, 'VOID') = 0 then
    Frame := tfVoid
  else if htCompareStr(Upr, 'ABOVE') = 0 then
    Frame := tfAbove
  else if htCompareStr(Upr, 'BELOW') = 0 then
    Frame := tfBelow
  else if htCompareStr(Upr, 'HSIDES') = 0 then
    Frame := tfHSides
  else if htCompareStr(Upr, 'LHS') = 0 then
    Frame := tfLhs
  else if htCompareStr(Upr, 'RHS') = 0 then
    Frame := tfRhs
  else if htCompareStr(Upr, 'VSIDES') = 0 then
    Frame := tfVSides
  else if htCompareStr(Upr, 'BOX') = 0 then
    Frame := tfBox
  else if htCompareStr(Upr, 'BORDER') = 0 then
    Frame := tfBorder
  else
    Result := False;
end;

//-- BG ---------------------------------------------------------- 09.02.2013 --
function TryStrToTableRules(const Str: ThtString; var Rules: TTableRules): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  Upr: ThtString;
begin
  Upr := htUpperCase(Str);
  Result := True;
  if htCompareStr(Upr, 'NONE') = 0 then
    Rules := trNone
  else if htCompareStr(Upr, 'GROUPS') = 0 then
    Rules := trGroups
  else if htCompareStr(Upr, 'ROWS') = 0 then
    Rules := trRows
  else if htCompareStr(Upr, 'COLS') = 0 then
    Rules := trCols
  else if htCompareStr(Upr, 'ALL') = 0 then
    Rules := trAll
  else
    Result := False;
end;

{----------------THtmlTable.Create}

constructor THtmlTable.Create(OwnerTableBlock: TTableBlock; Parent: TCellBasic; Attr: TAttributeList; Prop: TProperties);
var
  I: Integer;
  A: TAttribute;
begin
  inherited Create(Parent, Attr, Prop);
  FOwnerTableBlock := OwnerTableBlock;
  if FDisplay = pdUnassigned then
    FDisplay := pdTable;
  Rows := TRowList.Create;

  FCellPadding := 1;
  FCellSpacingHorz := 2;
  FCellSpacingVert := 2;
  BorderColor := clBtnFace;
  BorderColorLight := clBtnHighLight;
  BorderColorDark := clBtnShadow;

  // BG, 20.01.2013: process BorderSy before FrameSy and RulesSy as it implies defaults.
  A := nil;
  HasBorderWidthAttr := Attr.Find(BorderSy, A);
  if HasBorderWidthAttr then
  begin
    //BG, 15.10.2010: issue 5: set border width only, if style does not set any border width:
    if A.Name = '' then
      BorderWidth := 1
    else
      BorderWidth := Min(100, Max(0, A.Value)); {Border=0 is no border}
    brdWidthAttr := BorderWidth;

    if BorderWidth <> 0 then
    begin
      Frame := tfBorder;
      Rules := trAll;
    end
    else
    begin
      Frame := tfVoid;
      Rules := trNone;
    end;
  end;

  for I := 0 to Attr.Count - 1 do
    with Attr[I] do
      case Which of
        FrameAttrSy:
          TryStrToTableFrame(Name, Frame);

        RulesSy:
          TryStrToTableRules(Name, Rules);

        CellSpacingSy:
          begin
            FCellSpacingHorz := Min(40, Max(-1, Value));
            FCellSpacingVert := CellSpacingHorz;
          end;

        CellPaddingSy:
          FCellPadding := Min(50, Max(0, Value));

        BorderColorSy:
          TryStrToColor(Name, False, BorderColor);

        BorderColorLightSy:
          TryStrToColor(Name, False, BorderColorLight);

        BorderColorDarkSy:
          TryStrToColor(Name, False, BorderColorDark);
      end;
  if Prop.Collapse then
  begin
    FCellSpacingHorz := -1;
    FCellSpacingVert := -1;
  end
  else if Prop.HasBorderSpacing then
  begin
    FCellSpacingHorz := Prop.GetBorderSpacingHorz;
    FCellSpacingVert := Prop.GetBorderSpacingVert;
  end;
end;

{----------------THtmlTable.CreateCopy}

constructor THtmlTable.CreateCopy(OwnerCell: TCellBasic; Source: THtmlNode);
var
  I: Integer;
  HtmlTable: THtmlTable absolute Source;
begin
  inherited CreateCopy(OwnerCell,Source);
  FOwnerTableBlock := OwnerCell.FOwnerBlock as TTableBlock;
  Rows := TRowList.Create;
  for I := 0 to HtmlTable.Rows.Count - 1 do
    Rows.Add(TCellList.CreateCopy(OwnerCell.OwnerBlock, HtmlTable.Rows[I]));

  Move(HtmlTable.Initialized, Initialized, PtrSub(@EndList, @Initialized));
  Rules := HtmlTable.Rules;
  BorderWidth := HtmlTable.BorderWidth;
  BorderColor := HtmlTable.BorderColor;

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
    FBkGnd := HtmlTable.BkGnd;
    FBkColor := HtmlTable.BkColor;
  end
  else
    FBkGnd := False;
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

procedure THtmlTable.DoColumns(Count: Integer; const SpecWidth: TSpecWidth; VAlign: ThtAlignmentStyle; const Align: ThtString);
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

  function DummyCell(RSpan: Integer): TCellObjBase;
  begin
    Result := TDummyCellObj.Create(RSpan);
//    if BkGnd then {transfer bgcolor to cell if no Table image}
//    begin
//      Result.Cell.BkGnd := True;
//      Result.Cell.BkColor := BkColor;
//    end;
  end;

  procedure AddDummyCellsForColSpansAndInitializeCells;
  var
    Cl, Rw, RowCount, K: Integer;
    Row: TCellList;
    CellObjBase: TCellObjBase;
    CellObj: TCellObj absolute CellObjBase;
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
        CellObjBase := Row[Cl];
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
    CellObj: TCellObjBase;
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
                if Rows[K].Count < NumCols then // in an invalid table definition spanned cells may overlap and thus required dummies could be present, yet.
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
    CellObj: TCellObjBase;
    Row: TCellList;

    function IsLastCellOfRow(): Boolean;
    begin
      // Is Row[Cl] resp. CellObj a cell in this row? (Cl >= 0)
      // With respect to rowspans from previous rows is it the last one? (Cl + CellObj.ColSpan >= NumCols)
      Result := (Cl >= 0) and (Cl + CellObj.ColSpan >= Row.Count);
    end;

  var
    Rw, I: Integer;
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
          // add missing cells
          for I := Row.Count to NumCols - 1 do
            Row.Add(DummyCell(1));
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
  CellObj: TCellObjBase;
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

procedure THtmlTable.IncreaseWidthsByWidth(WidthType: TWidthType; var Widths: TIntArray;
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

procedure THtmlTable.IncreaseWidthsByPercentage(var Widths: TIntArray;
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

procedure THtmlTable.IncreaseWidthsByMinMaxDelta(WidthType: TWidthType; var Widths: TIntArray;
  StartIndex, EndIndex, Excess, DeltaWidth, Count: Integer; const Deltas: TIntArray);
// Increases width of spanned columns relative to difference between min and max widths.
var
  AddedExcess, AddedDelta, I, Add: Integer;
  WidthSum, K, Remaining: Integer;
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
        // BG, 16.11.2013: thanks to Andreas Hausladen for spreading Excess:
        if WidthType = wtNone then
        begin
          WidthSum := 0;
          for K := EndIndex downto StartIndex do
            if ColumnSpecs[K] = WidthType then
              Inc(WidthSum, Widths[K] + Deltas[K]);

          // spread the remaining pixels to all columns by their width percentage
          Remaining := (Excess - AddedExcess) - DeltaWidth;
          if (Remaining > 0) and (WidthSum > 0) then // wrong display is better than crash
          begin
            for K := EndIndex downto StartIndex + 1 do
            begin
              if ColumnSpecs[K] = WidthType then
              begin
                Add := ((Widths[K] + Deltas[K]) * Remaining) div WidthSum;
                Inc(Widths[K], Add);
                Inc(AddedExcess, Add);
              end;
            end;
          end;
        end;

        // add the remaining pixels to the first column's width.
        Inc(Widths[I], Excess - AddedExcess);
        break;
      end;
end;

procedure THtmlTable.IncreaseWidthsRelatively(
  var Widths: TIntArray;
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

procedure THtmlTable.IncreaseWidthsEvenly(WidthType: TWidthType; var Widths: TIntArray;
  StartIndex, EndIndex, Required, Spanned, Count: Integer);
// Increases width of spanned columns of given type evenly.
var
  RemainingWidth, I: Integer;
begin
  RemainingWidth := Required;
  for I := EndIndex downto StartIndex do
    if ColumnSpecs[I] = WidthType then
      if Count > 1 then
      begin
        Dec(Count);
        // MulDiv for each column instead of 1 precalculated width for all columns avoids round off errors.
        Widths[I] := MulDiv(Widths[I], Required, Spanned);
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

procedure THtmlTable.GetMinMaxWidths(Canvas: TCanvas; AvailableWidth, AvailableHeight: Integer);
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

  procedure UpdateRelativeWidths(var Widths: TIntArray; const ColumnSpecs: TWidthTypeArray; StartIndex, EndIndex: Integer);
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
    Untouched, I: Integer;
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

    // Prevent MinWidths from getting larger than MaxWidths, otherwise the calculation goes crazy
    for I := StartIndex to EndIndex do
      if MinWidths[I] > MaxWidths[I] then
        MaxWidths[I] := MinWidths[I];
  end;

  procedure IncreaseMinMaxWidthsByMinMaxDelta(WidthType: TWidthType; StartIndex, EndIndex: Integer);
  var
    Deltas: TIntArray;
    I: Integer;
  begin
    Deltas := SubArray(MaxWidths, MinWidths);
    if CellMin > SpannedMin then
      IncreaseWidthsByMinMaxDelta(WidthType, MinWidths, StartIndex, EndIndex, CellMin - SpannedMin, SpannedMax - SpannedMin, SpannedCounts[WidthType], Deltas);
    if CellMax > SpannedMax then
      IncreaseWidthsByMinMaxDelta(WidthType, MaxWidths, StartIndex, EndIndex, CellMax - SpannedMax, SpannedMax - SpannedMin, SpannedCounts[WidthType], Deltas);

    // Prevent MinWidths from getting larger than MaxWidths, otherwise the calculation goes crazy
    for I := StartIndex to EndIndex do
      if MinWidths[I] > MaxWidths[I] then
        MaxWidths[I] := MinWidths[I];
  end;

var
  //
  I, J, K, Span, EndIndex: Integer;
  Cells: TCellList;
  CellObj: TCellObjBase;
  MaxSpans: TIntArray;
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
    ColumnSpecs[I] := wtNone;
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

  //SetLength(Heights, 0);
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
          CellObj.Cell.MinMaxWidth(Canvas, CellMin, CellMax, 0, 0);
          CellPercent := 0;
          CellRel := 0;

          with CellObj.SpecWdMin do
          begin
            case VType of
              wtAbsolute:
                if CellMin < Value then
                  CellMin := Value;
            end;
          end;

          with CellObj.SpecWdMax do
          begin
            case VType of
              wtAbsolute:
                if CellMax > Value then
                  CellMax := Value;
            end;
          end;

          with CellObj.SpecWd do
          begin
            CellSpec := VType;
            case VType of
              wtPercent:
                CellPercent := Value;

              wtAbsolute:
              begin
                // BG, 07.10.2012: issue 55: wrong CellMax calculation
                //CellMin := Max(CellMin, Value);   // CellMin should be at least the given absolute value
                //CellMax := Min(CellMax, Value);   // CellMax should be at most  the given absolute value
                CellMax := Value;   // CellMax should be at most  the given absolute value
              end;

              wtRelative:
                CellRel := Value;
            end;
          end;
          CellMax := Max(CellMax, CellMin); // CellMax should be at least CellMin
          Inc(CellMin, CellSpacingHorz + CellObj.HzSpace);
          Inc(CellMax, CellSpacingHorz + CellObj.HzSpace);

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

procedure THtmlTable.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
begin
  Initialize; {in case it hasn't been done}
  GetMinMaxWidths(Canvas, AvailableWidth, AvailableHeight);
  Min := Math.Max(Sum(MinWidths) + CellSpacingHorz, tblWidthAttr);
  Max := Math.Max(Sum(MaxWidths) + CellSpacingHorz, tblWidthAttr);

  if FOwnerTableBlock.MargArray[piMinWidth] > 0 then
  begin
    Min := Math.Max(Min, FOwnerTableBlock.MargArray[piMinWidth]);
    Max := Math.Max(Max, FOwnerTableBlock.MargArray[piMinWidth]);
  end;

  if FOwnerTableBlock.MargArray[piMaxWidth] > 0 then
  begin
    Min := Math.Min(Min, FOwnerTableBlock.MargArray[piMaxWidth]);
    Max := Math.Min(Max, FOwnerTableBlock.MargArray[piMaxWidth]);
  end;
end;

{----------------THtmlTable.DrawLogic}

function THtmlTable.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;

  function FindTableWidth: Integer;

    procedure IncreaseWidths(WidthType: TWidthType; MinWidth, NewWidth, Count: Integer);
    var
      Deltas: TIntArray;
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

    procedure CalcPercentDeltas(var PercentDeltas: TIntArray; NewWidth: Integer);
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

    procedure IncreaseWidthsByPercentage(var Widths: TIntArray;
      const PercentDeltas: TIntArray;
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
    Specified: Boolean;
    NewWidth, MaxWidth, MinWidth, D, W, I: Integer;
    Counts: TIntegerPerWidthType;
    PercentDeltas: TIntArray;
    PercentAbove0Count: Integer;
    PercentDeltaAbove0Count: Integer;
  begin
    Specified := tblWidthAttr > 0;
    if Specified then
      NewWidth := tblWidthAttr
    else
      NewWidth := IMgr.RightSide(Y) - IMgr.LeftIndent(Y);
    Dec(NewWidth, CellSpacingHorz);

    Initialize;

    {Figure the width of each column}
    GetMinMaxWidths(Canvas, AWidth, AHeight);

    MinWidth := Sum(MinWidths);
    if FOwnerTableBlock.MargArray[piMinWidth] > 0 then
      MinWidth := Max(MinWidth, FOwnerTableBlock.MargArray[piMinWidth]);

    MaxWidth := Sum(MaxWidths);
    if FOwnerTableBlock.MargArray[piMaxWidth] > 0 then
      MaxWidth := Min(MaxWidth, FOwnerTableBlock.MargArray[piMaxWidth]);

    {fill in the Widths array}
    if MinWidth > NewWidth then
      // The minimum table width is too wide. Thus use minimum widths, table might expand.
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

      // As minimum widths fit into table try to preserve given absolute widths:
      Counts[wtAbsolute] := 0;
      W := 0;
      for I := 0 to NumCols - 1 do
      begin
        case ColumnSpecs[I] of
          wtAbsolute:
          begin
            Widths[i] := MaxWidths[i];
            Inc(Counts[wtAbsolute]);
          end;
        end;
        Inc(W, Widths[i]);
      end;

      if W > NewWidth then
      begin
        // Given absolute widths plus minimum widths of other columns do not fit into table!
        // Thus reduce the absolute widths as less as possible:

        IncreaseWidths(wtAbsolute, W, NewWidth, Counts[wtAbsolute]);
      end
      else
      begin
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
          // Table (NewWidth) is too small for given percentage specifications.
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
    end;
    {Return Table Width}
    Result := CellSpacingHorz + Sum(Widths);
  end;

  function FindTableHeight: Integer;

    procedure FindRowHeights(Canvas: TCanvas; AHeight: Integer);
    var
      I, J, K, H, Span, TotalMinHt, TotalDesHt, AvailHt, AddOn, Sum, AddedOn, Desired, UnSpec: Integer;
      More, Mr, IsSpeced: Boolean;
      MinHts, DesiredHts: TIntArray;
      SpecHts: array of Boolean;
      F: double;
    begin
      if Rows.Count = 0 then
        Exit;

      Dec(AHeight, CellSpacingVert); {calculated heights will include one cellspacing each,
      this removes that last odd cellspacing}
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
      AvailHt := Max(0, AHeight - Rows.Count * CellSpacingVert);
      while More do
      begin
        More := False;
        for J := 0 to Rows.Count - 1 do
          with Rows[J] do
          begin
            if J + Span > Rows.Count then
              Break; {otherwise will overlap}
            H := DrawLogicA(Canvas, Widths, Span, CellSpacingHorz, CellSpacingVert, AvailHt, Rows.Count, Desired, IsSpeced, Mr) + CellSpacingVert;
            Inc(Desired, CellspacingVert);
            More := More or Mr;
            if Span = 1 then
            begin
              MinHts[J] := H;
              DesiredHts[J] := Desired;
              SpecHts[J] := SpecHts[J] or IsSpeced;
            end
            else if H > CellspacingVert then {if H=Cellspacing then no rowspan for this span}
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
      begin
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
        else if TotalDesHt > 0 then
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
      end
      else if TotalDesHt - TotalMinHt <> 0 then
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
    if (CycleNumber <> Document.CycleNumber) or (Length(Heights) = 0) then
      FindRowHeights(Canvas, AHeight)
    //else if Document.InLogic2 and (Document.TableNestLevel <= 10) then
    //  FindRowHeights(Canvas, AHeight)
    ;

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
          if Items[I] is TCellObj then
          begin
            CellObj := TCellObj(Items[I]);
            with CellObj do
            begin {find the actual height, Ht, of each cell}
              FHt := 0;
              for K := J to Min(J + RowSpan - 1, Rows.Count - 1) do
                Inc(FHt, Heights[K]);
              if RowSpanHeight < FHt then
                RowSpanHeight := FHt;
            end;
          end;
      {DrawLogicB is only called in nested tables if the outer table is calling DrawLogic2}
        if Document.TableNestLevel = 1 then
          Document.InLogic2 := True;
        try
          if Document.InLogic2 then
            DrawLogicB(Canvas, Y, CellSpacingHorz,CellSpacingVert, Curs);
        finally
          if Document.TableNestLevel = 1 then
            Document.InLogic2 := False;
        end;
        Inc(Y, RowHeight);
      end;
    HeadOrFoot := ((HeaderHeight > 0) or (FootHeight > 0)) and HasBody;
    Inc(Result, CellSpacingVert);
  end;

  procedure DrawLogicYIndent;
  var
    J: Integer;
  begin
    for J := 0 to Rows.Count - 1 do
      with Rows[J] do
      begin
        RowHeight := Heights[J];
      {DrawLogicB is only called in nested tables if the outer table is calling DrawLogic2}
            DrawLogicB(Canvas, Y, CellSpacingHorz,CellSpacingVert, Curs);
        Inc(Y, RowHeight);
      end;
  end;

var
  TopY: Integer;
  FirstLinePtr: PInteger;
begin {THtmlTable.DrawLogic}
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.EnterMethod(Self, Format('THtmlTable.DrawLogic1 "%s"', [TagClass]));
  CodeSite.SendFmtMsg('AWidth, AHeight = [%4d, %4d]', [AWidth, AHeight]);
{$IFDEF JPM_DEBUGGING_LOGIC1}
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
{$ENDIF}
{$ENDIF}
  Inc(Document.TableNestLevel);
  try
    if (CycleNumber <> Document.CycleNumber) or (YDraw <> Y) or (ContentTop <> Y) or (DrawTop <> Y) or (StartCurs <> Curs) or (Length(Heights) = 0)
    then
    begin
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
{$ifdef JPM_DEBUGGING_LOGIC}
      CodeSite.SendFmtMsg('Found TableWidth = %d', [TableWidth]);
{$endif}
      TableHeight := FindTableHeight;
{$ifdef JPM_DEBUGGING_LOGIC}
      CodeSite.SendFmtMsg('Found TableHeight = %d', [TableHeight]);
{$endif}
      // Notice: SectionHeight = TableHeight
      Len := Curs - StartCurs;
      DrawHeight := TableHeight;
      ContentBot := TopY + TableHeight;
      DrawBot := TopY + DrawHeight;

      try
        if Assigned(FirstLinePtr) then
          FirstLinePtr^ := YDraw + SectionHeight;
      except
      end;

      CycleNumber := Document.CycleNumber;
    end
    else
    begin
      if Document.TableNestLevel = 1 then
        Document.InLogic2 := True;
      try
        if Document.InLogic2 then
          DrawLogicYIndent
        else
          Curs := StartCurs + Len;
      finally
        if Document.TableNestLevel = 1 then
          Document.InLogic2 := False;
      end;
{$ifdef JPM_DEBUGGING_LOGIC}
      CodeSite.SendFmtMsg('Stored TableWidth = %d, TableHeight = %d', [TableWidth, TableHeight]);
{$endif}
    end;
  finally
    MaxWidth := TableWidth;
    Result := TableHeight;
    Dec(Document.TableNestLevel);
{$ifdef JPM_DEBUGGING_LOGIC}
{$ifdef JPM_DEBUGGING_LOGIC1}
    if Assigned(IMgr) then
    begin
      CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
      CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
      CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
    end
    else
    begin
      CodeSite.SendMsg('IMgr      = nil');
    end;
{$ENDIF}
    CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
    CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
    CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
    CodeSite.ExitMethod(Self, Format('THtmlTable.DrawLogic1 "%s"', [TagClass]));
{$ENDIF}
  end;
end;

{----------------THtmlTable.Draw}

function THtmlTable.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;

  procedure DrawTable(XX, YY, YOffset: Integer);
  var
    I: Integer;
  begin
    for I := 0 to Rows.Count - 1 do
      YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
        XX, YY, YOffset, CellSpacingHorz,CellSpacingVert, BorderWidth > 0, BorderColorLight,
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
            XX, YY, YOffset, CellSpacingHorz,CellSpacingVert, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        Document.PrintingTable := nil;
      end
      else
      begin {see if enough room on this page for header, 1 row, footer}
        if HeadOrFoot then
        begin
          Spacing := CellSpacingVert div 2;
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
        Document.PageBottom := SavePageBottom - FootHeight - CellspacingVert - BottomBorder - 5; {a little to spare}
        for I := 0 to Rows.Count - 1 do {do part of table}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacingHorz, CellSpacingVert, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        BodyBreak := Document.PageBottom;
        if FootStartRow >= 0 then
        begin
          TablePartRec.TablePart := DoFoot;
          TablePartRec.PartStart := Y + FootOffset;
          TablePartRec.PartHeight := FootHeight + Max(2 * CellspacingVert, CellspacingVert + 1) + BottomBorder;
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
            XX, YY, YOffset, CellSpacingHorz, CellSpacingVert, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        Document.TheOwner.TablePartRec.TablePart := Normal;
      end
      else
      begin {will do part of the table now}
    {Leave room for foot later}
        Document.PageBottom := Document.PageBottom - FootHeight + Max(CellspacingVert, 1) - BottomBorder;
        for I := 0 to Rows.Count - 1 do
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacingHorz, CellSpacingVert, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        BodyBreak := Document.PageBottom;
        if FootStartRow >= 0 then
        begin
          TablePartRec.TablePart := DoFoot;
          TablePartRec.PartStart := Y + FootOffset;
          TablePartRec.PartHeight := FootHeight + Max(2 * CellspacingVert, CellspacingVert + 1) + BottomBorder;
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
            XX, YY, YOffset, CellSpacingHorz,CellSpacingVert, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        Document.TheOwner.TablePartRec.TablePart := Normal;
        Document.PrintingTable := nil;
      end
      else
      begin
        SavePageBottom := Document.PageBottom;
        for I := 0 to Rows.Count - 1 do {do part of table}
          YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
            XX, YY, YOffset, CellSpacingHorz,CellSpacingVert, BorderWidth > 0, BorderColorLight,
            BorderColorDark, I);
        BodyBreak := Document.PageBottom;
        if FootStartRow >= 0 then
        begin
          TablePartRec.TablePart := DoFoot;
          TablePartRec.PartStart := Y + FootOffset;
          TablePartRec.PartHeight := FootHeight + Max(2 * CellspacingVert, CellspacingVert + 1) + BottomBorder;
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
            XX, YY, YOffset, CellSpacingHorz, CellSpacingVert, BorderWidth > 0, BorderColorLight,
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
        TablePartRec.FootHeight := FootHeight + Max(CellspacingVert, 1);
      end;
      Document.TheOwner.TablePartRec := TablePartRec;
    end;

    procedure DrawHead;
    var
      I: Integer;
    begin
      for I := 0 to HeaderRowCount - 1 do
        YY := Rows[I].Draw(Canvas, Document, ARect, Widths,
          XX, YY, YOffset, CellSpacingHorz,CellSpacingVert, BorderWidth > 0, BorderColorLight,
          BorderColorDark, I);
      TablePartRec.TablePart := DoBody1;
      TablePartRec.PartStart := BodyBreak - 1;
      TablePartRec.FootHeight := FootHeight + Max(CellspacingVert, 1) + BottomBorder;
      Document.TheOwner.TablePartRec := TablePartRec;
    end;

  begin
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
    YOffset := Document.YOff;
    YO := Y - YOffset;

    DrawX := X;

    //>-- DZ
    DrawRect.Top    := Y;
    DrawRect.Left   := X;
    DrawRect.Right  := DrawRect.Left + TableWidth;
    DrawRect.Bottom := DrawRect.Top + DrawHeight;
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
  out ATitle: ThtString): ThtguResultType;

  function GetTableURL(X: Integer; Y: Integer): ThtguResultType;
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
        if Row[I] is TCellObj then
        begin
          CellObj := TCellObj(Row[I]);
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

function THtmlTable.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;

  function GetTableObj(X: Integer; Y: Integer): Boolean;
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
        if Row[I] is TCellObj then
        begin
          CellObj := TCellObj(Row[I]);
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
  if (Y >= ContentTop) and (Y < ContentBot) and (X >= DrawX) and (X <= TableWidth + DrawX) and GetTableObj(X, Y) then
    Result := True
  else
    Result := inherited PtInObject(X, Y, Obj, IX, IY);
end;

{----------------THtmlTable.FindCursor}

function THtmlTable.FindCursor(Canvas: TCanvas; X, Y: Integer;
  out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer;

  function GetTableCursor(X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer;
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
        if Row[I] is TCellObj then
        begin
          CellObj := TCellObj(Row[I]);
          if (X >= XX) and (X < XX + CellObj.Wd) and (Y >= CellObj.Cell.DrawYY) and (Y < CellObj.Cell.DrawYY + CellObj.Ht) then
          begin
            Result := CellObj.Cell.FindCursor(Canvas, X, Y, XR, YR, CaretHt, InText);
            if Result >= 0 then
              Exit;
          end;
        end;
        Inc(XX, Widths[I]);
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

function THtmlTable.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean;
{note: returned X value is not correct here but it isn't used}
var
  I, J: Integer;
  Row: TCellList;
begin
  if (len > 0) and (Cursor >= StartCurs) and (Cursor < StartCurs + Len) then
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      for I := 0 to Row.Count - 1 do
      begin
        if Row[I] is TCellObj then
        begin
          Result := TCellObj(Row[I]).Cell.CursorToXy(Canvas, Cursor, X, Y);
          if Result then
            Exit;
        end;
      end;
    end;

  Result := False;
end;

{----------------THtmlTable.GetChAtPos}

function THtmlTable.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean;
var
  I, J: Integer;
  Row: TCellList;
begin
  Obj := nil;
  if (len > 0) and (Pos >= StartCurs) and (Pos < StartCurs + Len) then
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      for I := 0 to Row.Count - 1 do
      begin
        if Row[I] is TCellObj then
        begin
          Result := TCellObj(Row[I]).Cell.GetChAtPos(Pos, Ch, Obj);
          if Result then
            Exit;
        end;
      end;
    end;

  Result := False;
end;

{----------------THtmlTable.FindString}

function THtmlTable.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
var
  I, J: Integer;
  Row: TCellList;
begin
  for J := 0 to Rows.Count - 1 do
  begin
    Row := Rows[J];
    for I := 0 to Row.Count - 1 do
    begin
      if Row[I] is TCellObj then
      begin
        Result := TCellObj(Row[I]).Cell.FindString(From, ToFind, MatchCase);
        if Result >= 0 then
          Exit;
      end;
    end;
  end;
  Result := -1;
end;

{----------------THtmlTable.FindStringR}

function THtmlTable.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
var
  I, J: Integer;
  Row: TCellList;
begin
  for J := Rows.Count - 1 downto 0 do
  begin
    Row := Rows[J];
    for I := Row.Count - 1 downto 0 do
    begin
      if Row[I] is TCellObj then
      begin
        Result := TCellObj(Row[I]).Cell.FindStringR(From, ToFind, MatchCase);
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
begin
  for J := 0 to Rows.Count - 1 do
  begin
    Row := Rows[J];
    for I := 0 to Row.Count - 1 do
    begin
      if Row[I] is TCellObj then
      begin
        Result := TCellObj(Row[I]).Cell.FindSourcePos(DocPos);
        if Result >= 0 then
          Exit;
      end;
    end;
  end;
  Result := -1;
end;

{----------------THtmlTable.FindDocPos}

function THtmlTable.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
var
  I, J: Integer;
  Row: TCellList;
begin
  if not Prev then
    for J := 0 to Rows.Count - 1 do
    begin
      Row := Rows[J];
      if Row <> nil then
        for I := 0 to Row.Count - 1 do
        begin
          if Row[I] is TCellObj then
          begin
            Result := TCellObj(Row[I]).Cell.FindDocPos(SourcePos, Prev);
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
          if Row[I] is TCellObj then
          begin
            Result := TCellObj(Row[I]).Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then
              Exit;
          end;
        end;
    end;
  Result := -1;
end;

{----------------THtmlTable.CopySelectedText}

procedure THtmlTable.CopySelectedText;
var
  I, J: Integer;
  Row: TCellList;
begin
  for J := 0 to Rows.Count - 1 do
  begin
    Row := Rows[J];
    for I := 0 to Row.Count - 1 do
      if Row[I] is TCellObj then
        TCellObj(Row[I]).Cell.CopySelectedText;
  end;
end;

//-- BG ---------------------------------------------------------- 01.12.2013 --
constructor TSection.Create(Parent: TCellBasic);
begin
  inherited Create(Parent, nil, nil);
  FDisplay := pdInline; // A section is always inline as the section is intended to handle consecutive inline elements.
  FBuff := PWideChar(BuffS);
  Fonts := TFontList.Create;
  Images := TSizeableObjList.Create;
  FormControls := TFormControlObjList.Create(False);
  Lines := TLineRecList.Create;
end;

{----------------TSection.Create}

constructor TSection.Create(Parent: TCellBasic; Attr: TAttributeList; Prop: TProperties; AnURL: TUrlTarget; FirstItem: Boolean);
var
  FO: TFontObj;
//  T: TAttribute;
  S, C: ThtString;
//  Clr: ThtClearStyle;
  Percent: Boolean;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSection.Create');
{$IFDEF JPM_DEBUGGING_STYLES}
  StyleUn.LogProperties(Prop,'Prop');
  CodeSite.AddSeparator;
{$ENDIF}
{$ENDIF}
  if Attr <> nil then
  begin
    S := Attr.TheID;
    C := Attr.TheClass;
  end;
  inherited Create(Parent, Attr, Prop, S, C);
  if FDisplay = pdUnassigned then
    FDisplay := pdInline;
  FBuff := PWideChar(BuffS);
  Fonts := TFontList.Create;
  Images := TSizeableObjList.Create;
  FormControls := TFormControlObjList.Create(False);
  Lines := TLineRecList.Create;

  FO := TFontObj.Create(Self, Prop.Font, 0);
  FO.Title := Prop.PropTitle;
  Prop.GetVertAlign(FO.SScript);

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
  else if Document.NoBreak then
    WhiteSpaceStyle := wsNoWrap
  else
    WhiteSpaceStyle := wsNormal;
  if VarIsOrdinal(Prop.Props[piWhiteSpace]) then
    WhiteSpaceStyle := ThtWhiteSpaceStyle(Prop.Props[piWhiteSpace])
  else if VarIsStr(Prop.Props[piWhiteSpace]) then
  begin
    if not TryStrToWhiteSpace(Prop.Props[piWhiteSpace],WhiteSpaceStyle) then
    begin
    end;
//    if Prop.Props[piWhiteSpace] = 'pre' then
//      WhiteSpaceStyle := wsPre
//    else if Prop.Props[piWhiteSpace] = 'nowrap' then
//      WhiteSpaceStyle := wsNoWrap
//    else if Prop.Props[piWhiteSpace] = 'pre-wrap' then
//      WhiteSpaceStyle := wsPreWrap
//    else if Prop.Props[piWhiteSpace] = 'pre-line' then
//      WhiteSpaceStyle := wsPreLine
//    else if Prop.Props[piWhiteSpace] = 'normal' then
 //     WhiteSpaceStyle := wsNormal;
  end;
{$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TSection.Create');
{$ENDIF}
end;

{----------------TSection.CreateCopy}

constructor TSection.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TSection absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  Len := T.Len;
  BuffS := T.BuffS;
  SetLength(BuffS, Length(BuffS));
  FBuff := PWideChar(BuffS);
  Brk := T.Brk;
  Fonts := TFontList.CreateCopy(Self, T.Fonts);
  //TODO -oBG, 24.03.2011: TSection has no Cell, but owns images. Thus Parent must be a THtmlNode.
  //  and ThtDocument should become a TBodyBlock instead of a SectionList.
  Images := TSizeableObjList.CreateCopy(Parent {must be Self}, T.Images);
  FormControls := TFormControlObjList.CreateCopy(Parent, T.FormControls);
  Lines := TLineRecList.Create;
  Justify := T.Justify;
  LineHeight := T.LineHeight;
  FirstLineIndent := T.FirstLineIndent;
  FLPercent := T.FLPercent;
  BreakWord := T.BreakWord;
  WhiteSpaceStyle := T.WhiteSpaceStyle;
end;

{----------------TSection.Destroy}

destructor TSection.Destroy;
var
  i: Integer;
begin
  { Yunqa.de: Do not leave references to deleted font objects in the
    HtmlViewer's link list. Otherwise TURLTarget.SetLast might see an access
    violation. }
  if Document <> nil then
    if Document.LinkList <> nil then
      for i := 0 to Fonts.Count - 1 do
        Document.LinkList.Remove(Fonts[i]);

  SIndexList.Free;
  Lines.Free;
  FormControls.Free;
  Images.Free;
  Fonts.Free;
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
  Tok: TTokenObj;
begin
  Tok := TTokenObj.Create;
  Tok.AddUnicodeChar(C, Index);
  AddTokenObj(Tok);
  Tok.Free;
end;

function TSection.GetIndexObj(I: Integer): ThtIndexObj;
begin
  Result := SIndexList[I];
end;

procedure TSection.AddOpBrk;
var
  L: Integer;
begin
  L := Length(Brk);
  if L > 0 then
    Brk[L - 1] := twOptional;
end;

{----------------TSection.AddTokenObj}

procedure TSection.AddTokenObj(T: TTokenObj);
var
  L, I, J: Integer;
  C: ThtTextWrap;
  St, StU: ThtString;
  Small: Boolean;
  LastProps: TProperties;
begin
  if T.Count = 0 then
    Exit;

  { Yunqa.de: Simple hack to support <span style="display:none"> }
  LastProps := Document.PropStack.Last;
  if (LastProps.Display = pdNone) and (LastProps.PropSym in [
    SpanSy, NoBrSy, WbrSy, FontSy, BSy, ISy, SSy, StrikeSy, USy, SubSy, SupSy, BigSy, SmallSy, TTSy,
    EmSy, StrongSy, CodeSy, KbdSy, SampSy, DelSy, InsSy, CiteSy, VarSy, MarkSy, TimeSy, ASy])
  then
    Exit;

  case LastProps.GetTextTransform of
    txUpper:
      St := htUpperCase(T.S);
    txLower:
      St := htLowerCase(T.S);
  else
    St := T.S;
  end;

  L := Len + T.Count;
  if Length(XP) < L + 3 then
    Allocate(L + 500); {L+3 to permit additions later}

  // add positions in source
  Move(T.I[1], XP[Len], T.Count * Sizeof(Integer));

  // BG, 31.08.2011: added: WhiteSpaceStyle
  if Document.NoBreak or (WhiteSpaceStyle in [wsPre, wsPreLine, wsNoWrap]) then
    C := twNo
  else
    C := twYes;

  J := Length(Brk);
  SetLength(Brk, J + T.Count);
  for I := J to J + T.Count - 1 do
    Brk[I] := C;

  if LastProps.GetFontVariant = 'small-caps' then
  begin
    StU := htUpperCase(St);
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
            Document.PropStack.PushNewProp(SmallSy, nil, nil); {change to smaller font}
            ChangeFont(Document.PropStack.Last);
            Small := True;
          end;
        end
        else if StU[I] = St[I] then
        begin {St[I] was uppercase and Small is set}
          Document.PropStack.PopAProp(SmallSy);
          ChangeFont(Document.PropStack.Last);
          Small := False;
        end;
      end;
      Inc(Len);
    end;
    if Small then {change back to regular font}
    begin
      Document.PropStack.PopAProp(SmallSy);
      ChangeFont(Document.PropStack.Last);
    end;
  end
  else
  begin
    BuffS := BuffS + St;
    Len := L;
  end;
  FBuff := PWideChar(BuffS);
end;

{----------------TSection.ProcessText}

procedure TSection.ProcessText(TagIndex: Integer);
const
  Shy = #173; {soft hyphen}
var
  I: Integer;
  FO: TFontObj;

  procedure Remove(I: Integer);
  var L: Integer;
  begin
    L := Length(BuffS) - I;
    if L > 0 then
       Move(XP[I], XP[I - 1], L * Sizeof(Integer));
    L := Length(Brk) - I;
    if L > 0 then
        Move(Brk[I], Brk[I - 1], L * Sizeof(ThtTextWrap));
    SetLength(Brk, Length(Brk) - 1);
    System.Delete(BuffS, I, 1);
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
    Allocate(Length(BuffS) + 500);
    XP[Length(BuffS) - 1] := XP[Length(BuffS) - 2] + 1;
  end
  else
  begin
    while (Length(BuffS) > 0) and (BuffS[1] = ' ') do
      Remove(1);

    I := WidePos(Shy, BuffS);
    while I > 0 do
    begin
      Remove(I);
      if (I > 1) and (Brk[I - 2] <> twNo) then
        Brk[I - 2] := twSoft;
      I := WidePos(Shy, BuffS);
    end;

    if WhiteSpaceStyle in [wsNormal, wsNoWrap, wsPreLine] then
    begin
      while (Length(BuffS) > 0) and (BuffS[1] = ' ') do
        Remove(1);

      I := WidePos('  ', BuffS);
      while I > 0 do
      begin
        if Brk[I - 1] = twNo then
          Remove(I)
        else
          Remove(I + 1);
        I := WidePos('  ', BuffS);
      end;

      {After floating images at start, delete an annoying space}
      for I := Length(BuffS) - 1 downto 1 do
        if (BuffS[I] = ImgPan) and (Images.FindObject(I - 1).Floating in [ALeft, ARight])
          and (BuffS[I + 1] = ' ') then
          Remove(I + 1);

      I := WidePos(UnicodeString(' '#8), BuffS); {#8 is break char}
      while I > 0 do
      begin
        Remove(I);
        I := WidePos(UnicodeString(' '#8), BuffS);
      end;

      I := WidePos(UnicodeString(#8' '), BuffS);
      while I > 0 do
      begin
        Remove(I + 1);
        I := WidePos(UnicodeString(#8' '), BuffS);
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
        //XP[Length(BuffS) - 1] := TagIndex;
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
  IO: ThtIndexObj;
begin
  FBuff := PWideChar(BuffS);
  Len := Length(BuffS);
  if Len > 0 then
  begin
    SetLength(Brk, Length(Brk) + 1);
    Brk[Length(Brk) - 1] := twYes;

    if not IsCopy then
    begin
      Last := 0; {to prevent warning msg}
      SIndexList := ThtIndexObjList.Create;
      for I := 0 to Len - 1 do
      begin
        if (I = 0) or (XP[I] <> Last + 1) then
        begin
          IO := ThtIndexObj.Create;
          IO.Pos := I;
          IO.Index := XP[I];
          SIndexList.Add(IO);
        end;
        Last := XP[I];
      end;
      SetLength(XP, 0);
    end;

    Inc(Document.SectionCount);
    SectionNumber := Document.SectionCount;
  end;
end;

{----------------TSection.Allocate}

procedure TSection.Allocate(N: Integer);
begin
  if Length(XP) < N then
    SetLength(XP, N);
end;

{----------------TSection.ChangeFont}

procedure TSection.ChangeFont(Prop: TProperties);
var
  FO: TFontObj;
  LastUrl: TUrlTarget;
  Align: ThtAlignmentStyle;
begin
  FO := Fonts[Fonts.Count - 1];
  LastUrl := FO.UrlTarget;
  if FO.Pos = Len then
    FO.ReplaceFont(Prop.Font) {fontobj already at this position, modify it}
  else
  begin
    FO := TFontObj.Create(Self, Prop.Font, Len);
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

procedure TSection.HRef(IsHRef: Boolean; List: ThtDocument; AnURL: TUrlTarget;
  Attributes: TAttributeList; Prop: TProperties);
var
  FO: TFontObj;
  Align: ThtAlignmentStyle;
begin
  FO := Fonts[Fonts.Count - 1];
  if FO.Pos = Len then
    FO.ReplaceFont(Prop.Font) {fontobj already at this position, modify it}
  else
  begin
    FO := TFontObj.Create(Self, Prop.Font, Len);
    Fonts.Add(FO);
  end;

  if IsHRef then
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

//-- BG ---------------------------------------------------------- 12.11.2011 --
function TSection.AddFrame(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TFrameObj;
begin
  Result := TFrameObj.Create(ACell, Len, L, Prop);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for iframe}
end;

function TSection.AddImage(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TImageObj;
begin
  Result := TImageObj.Create(ACell, Len, L, Prop);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for image}
end;

function TSection.AddPanel(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TPanelObj;
begin
  Result := TPanelObj.Create(ACell, Len, L, Prop, False);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for panel}
end;

function TSection.AddProgress(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TProgressObj;
begin
  Result := TProgressObj.Create(ACell, Len, L, Prop, False);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for progress}
end;

function TSection.AddMeter(L: TAttributeList; ACell: TCellBasic; Index: Integer; Prop: TProperties): TMeterObj;
begin
  Result := TMeterObj.Create(ACell, Len, L, Prop, False);
  Images.Add(Result);
  AddChar(ImgPan, Index); {marker for progress}
end;

function TSection.CreatePanel(L: TAttributeList; ACell: TCellBasic; Prop: TProperties): TPanelObj;
{Used by object tag}
begin
  Result := TPanelObj.Create(ACell, Len, L, Prop, True);
end;

procedure TSection.AddPanel1(PO: TPanelObj; Index: Integer);
{Used for Object Tag}
begin
  Images.Add(PO);
  AddChar(ImgPan, Index); {marker for panel}
end;

{----------------TSection.AddFormControl}

function TSection.AddFormControl(Which: TElemSymb; AMasterList: ThtDocument;
  L: TAttributeList; ACell: TCellBasic; Index: Integer;
  Prop: TProperties): TFormControlObj;
var
  T: TAttribute;
  FCO: TFormControlObj;
  S: ThtString;
  IO: TImageObj;
  ButtonControl: TButtonFormControlObj;
  FCT: (fctUnknown, fctImage, fctFile);
begin
  S := '';
  FCT := fctUnknown;
  case Which of
    InputSy:
    begin
      FCO := nil;
      T := nil;
      if L.Find(TypeSy, T) then
      begin
        S := LowerCase(T.Name);
        if (S = 'submit') or (S = 'reset') or (S = 'button') then
          FCO := TButtonFormControlObj.Create(ACell, Len, L, Prop)
        else if S = 'radio' then
          FCO := TRadioButtonFormControlObj.Create(ACell, Len, L, Prop)
        else if S = 'checkbox' then
          FCO := TCheckBoxFormControlObj.Create(ACell, Len, L, Prop)
        else if S = 'hidden' then
          FCO := THiddenFormControlObj.Create(ACell, Len, L, Prop)
        else if S = 'image' then
        begin
          FCT := fctImage;
          FCO := TImageFormControlObj.Create(ACell, Len, L, Prop);
        end
        else if S = 'file' then
        begin
          FCT := fctFile;
          FCO := TEditFormControlObj.Create(ACell, Len, L, Prop);
        end;
      end;
      if FCO = nil then
        FCO := TEditFormControlObj.Create(ACell, Len, L, Prop);
    end;

    SelectSy:
    begin
      if L.Find(MultipleSy, T) or L.Find(SizeSy, T) and (T.Value > 1) then
        FCO := TListBoxFormControlObj.Create(ACell, Len, L, Prop)
      else
        FCO := TComboFormControlObj.Create(ACell, Len, L, Prop);
    end;
  else
    FCO := TTextAreaFormControlObj.Create(ACell, Len, L, Prop);
  end;

  case FCT of

    fctImage:
    begin
      IO := AddImage(L, ACell, Index, Prop); {leave out of FormControlList}
      IO.MyFormControl := TImageFormControlObj(FCO);
      TImageFormControlObj(FCO).MyImage := IO;
    end;

    fctFile:
    begin
      FormControls.Add(FCO);
      AddChar(FmCtl, Index); {marker for FormControl}
      Brk[Len - 1] := twNo; {don't allow break between these two controls}
      ButtonControl := TButtonFormControlObj.Create(ACell, Len, L, Prop);
      ButtonControl.MyEdit := TEditFormControlObj(FCO);
      FormControls.Add(ButtonControl);
    {the following fixup puts the ID on the TEdit and deletes it from the Button}
      if L.TheID <> '' then
        Document.IDNameList.AddObject(L.TheID, FCO);
      FCO.Value := ''; {value attribute should not show in TEdit}
      TEditFormControlObj(FCO).Text := '';
      AddChar(FmCtl, Index);
      Brk[Len - 1] := twNo;
    end;

  else
    FormControls.Add(FCO);
    AddChar(FmCtl, Index); {marker for FormControl}
  end;

  if Prop.HasBorderStyle then {start of inline border}
    Document.ProcessInlines(Index, Prop, True);
{$ifdef has_StyleElements}
  if FCO.GetControl <> nil then begin
    FCO.GetControl.StyleElements := AMasterList.StyleElements;
  end;
{$endif}
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
  Cnt, XX, YY, I, J, J1, J2, J3: Integer;
  Picture: Boolean;
  FlObj: TFloatingObj; //TSizeableObj;
  FcObj: TFloatingObj; //TFormControlObj;
  FO: TFontObj;
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
    //Fonts.GetFontAt(Start - Buff, OHang).AssignToCanvas(Canvas);
    J1 := Fonts.GetFontObjAt(Start - Buff, Len, FO);
    FO.TheFont.AssignToCanvas(Canvas);
    J2 := Images.GetObjectAt(Start - Buff, FlObj);
    J3 := FormControls.GetObjectAt(Start - Buff, FcObj);
    if (J2 = 0) or (J3 = 0) then
    begin
      if J2 <> 0 then
        FlObj := FcObj;
      I := 1; J := 1;
      Picture := True;
      if FlObj.IsInFlow then
      begin
        Inc(XX, FlObj.TotalWidth);
        if XX > Width then
          break;
      end;
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
 {$ifdef UseInline} inline; {$endif}
begin
  Result := Ord(C) >= $3000;
end;

//-- BG ---------------------------------------------------------- 27.01.2012 --
function CanWrapAfter(PC: PWideChar): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  case PC^ of
    WideChar('-'):
      case (PC+1)^ of
        WideChar('0')..WideChar('9'):
          Result := False;
      else
        Result := True;
      end;

    WideChar('/'), WideChar('?'):
      Result := True
  else
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 20.09.2010 --
function CanWrap(PC: PWideChar): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  case PC^ of
    WideChar('-'):
      case (PC+1)^ of
        WideChar('0')..WideChar('9'):
          Result := False;
      else
        Result := True;
      end;

    WideChar(' '), WideChar('/'), WideChar('?'), ImgPan, FmCtl, BrkCh:
      Result := True
  else
    Result := WrapChar(PC^);
  end;
end;

{----------------TSection.MinMaxWidth}

procedure TSection.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
{Min is the width the section would occupy when wrapped as tightly as possible.
 Max, the width if no wrapping were used.}

  procedure MinMaxWidthOfBlocks(Objects: TFloatingObjList);
  var
    I: Integer;
    Obj: TFloatingObj;
  begin
    for I := 0 to Objects.Count - 1 do {call drawlogic for all the objects}
    begin
      Obj := Objects[I];
      Obj.DrawLogicInline(Canvas, Fonts.GetFontObjAt(Obj.StartCurs), AvailableWidth, AvailableHeight);
      if not Obj.PercentWidth then
      begin
        if Obj.Positioning in [posAbsolute, posFixed] then
        begin
          // does not affect the block width
          Brk[Obj.StartCurs] := twYes; {allow break after positioned object}
        end
        else if Obj.Floating in [ALeft, ARight] then
        begin
          Inc(Max, Obj.TotalWidth);
          Brk[Obj.StartCurs] := twYes; {allow break after floating object}
          Min := Math.Max(Min, Obj.TotalWidth);
        end
        else
          Min := Math.Max(Min, Obj.ClientWidth);
      end;
    end;
  end;

var
  SoftHyphen: Boolean;

  function FindTextWidthB(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: Boolean): TSize;
  begin
    Result := FindTextSize(Canvas, Start, N, RemoveSpaces);
    if Start = Buff then
      if FLPercent = 0 then {not a percent}
        Inc(Result.cx, FirstLineIndent)
      else
        Result.cx := (100 * Result.cx) div (100 - FLPercent);
    if SoftHyphen then
      Inc(Result.cx, Canvas.TextWidth('-'));
  end;

var
  I, FloatMin: Integer;
  P, P1: PWideChar;
begin
  Min := 0;
  Max := 0;
  if Len = 0 then
    Exit;

  if not BreakWord and (WhiteSpaceStyle in [wsPre, wsNoWrap]) then
  begin
    if StoredMax.cx = 0 then
    begin
      StoredMax := FindTextSize(Canvas, Buff, Len - 1, False);
      Max := StoredMax.cx;
    end
    else
      Max := StoredMax.cx;
    Min := Math.Min(MaxHScroll, Max);
    Exit;
  end;

{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self, 'TSection.MinMaxWidth');
{$ENDIF}
  if (StoredMin.cx > 0) and (Images.Count = 0) then
  begin
    Min := StoredMin.cx;
    Max := StoredMax.cx;
{$IFDEF JPM_DEBUGGING}
    CodeSite.ExitMethod(Self, Format('TSection.MinMaxWidth Stored %d .. %d', [Min, Max]));
{$ENDIF}
    Exit;
  end;

  MinMaxWidthOfBlocks(Images);
  MinMaxWidthOfBlocks(FormControls);
  FloatMin := Min;

  SoftHyphen := False;
  P := Buff;
  P1 := StrScanW(P, BrkCh); {look for break char}
  while Assigned(P1) do
  begin
    Max := Math.Max(Max, FindTextWidthB(Canvas, P, P1 - P, False).cx);
    P := P1 + 1;
    P1 := StrScanW(P, BrkCh);
  end;
  P1 := StrScanW(P, #0); {look for the end}
  Inc(Max, FindTextWidthB(Canvas, P, P1 - P, True).cx); // + FloatMin;

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
      SoftHyphen := False;
      if CanWrap(P1) and (Brk[I - 1] = twYes) then
      begin
        Inc(P1);
        Inc(I);
      end
      else
      begin
        repeat
          Inc(P1);
          Inc(I);
          case Brk[I - 2] of
            twSoft, twOptional:
              break;
          end;
        until (P1^ = #0) or (CanWrap(P1) and (Brk[I - 1] = twYes));
        SoftHyphen := Brk[I - 2] = twSoft;
        if CanWrapAfter(P1) then
        begin
          Inc(P1);
          Inc(I);
        end;
      end;
      Min := Math.Max(Min, FindTextWidthB(Canvas, P, P1 - P, True).cx);
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
      Min := Math.Max(Min, FindTextWidthB(Canvas, P, 1, True).cx);
      Inc(P);
    end;

  Min := Math.Max(FloatMin, Min);
  StoredMin.cx := Min;
  StoredMax.cx := Max;
  StoredMin.cy := 0;
  StoredMax.cy := 0;
{$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self, Format('TSection.MinMaxWidth %d .. %d', [Min, Max]));
{$ENDIF}
end;

{----------------TSection.FindTextWidth}

function TSection.FindTextSize(Canvas: TCanvas; Start: PWideChar; N: Integer; RemoveSpaces: Boolean): TSize;
{find actual line width of N chars starting at Start.  If RemoveSpaces set,
 don't count spaces on right end}
var
  I, J, J1: Integer;
  FlObj: TFloatingObj; //TSizeableObj;
  FcObj: TFloatingObj; //TFormControlObj;
  FO: TFontObj;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSection.FindTextWidth');
{$ENDIF}
  Result.cx := 0;
  Result.cy := 0;
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
    J := Images.GetObjectAt(Start - Buff, FlObj);
    J1 := FormControls.GetObjectAt(Start - Buff, FcObj);
    if (J = 0) or (J1 = 0) then {it's an image or a form control}
    begin
    {Here we count floating images as 1 ThtChar but do not include their width,
      This is required for the call in FindCursor}
      if J <> 0 then
        FlObj := FcObj;
      if FlObj.IsInFlow then
      begin
        Inc(Result.cx, FlObj.TotalWidth);
        Result.cy := Max(Result.cy, FlObj.TotalHeight);
      end;
      Dec(N); {image counts as one ThtChar}
      Inc(Start);
    end
    else
    begin
      //Fonts.GetFontAt(Start - Buff, OHang).AssignToCanvas(Canvas);
      I := Min(J, J1);
      I := Min(I, Min(Fonts.GetFontObjAt(Start - Buff, Len, FO), N));
      FO.TheFont.AssignToCanvas(Canvas);
      //Assert(I > 0, 'I less than or = 0 in FindTextWidth');
      with GetTextExtent(Canvas.Handle, Start, I) do
      begin
        Inc(Result.cx, cx + FO.Overhang);
        Result.cy := Max(Result.cy, cy);
      end;
      if I = 0 then
        Break;
      Dec(N, I);
      Inc(Start, I);
    end;
  end;
  {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self, Format('TSection.FindTextWidth w = %d, h = %d', [Result.cx, Result.cy]));
  {$ENDIF}
end;

{----------------TSection.FindTextWidthA}

function TSection.FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: Integer): Integer;
{find actual line width of N chars starting at Start.
 BG: The only difference to FindTextWidth is the '- OHang' when incrementing the result.}
var
  I, J, J1: Integer;
  FlObj: TFloatingObj; //TSizeableObj;
  FcObj: TFloatingObj; //TFormControlObj;
  FO: TFontObj;
begin
  Result := 0;
  while N > 0 do
  begin
    J := Images.GetObjectAt(Start - Buff, FlObj);
    J1 := FormControls.GetObjectAt(Start - Buff, FcObj);
    if (J = 0) or (J1 = 0) then {it's an image or a form control}
    begin
    {Here we count floating images as 1 ThtChar but do not include their width,
      This is required for the call in FindCursor}
      if J <> 0 then
        FlObj := FcObj;
      if FlObj.IsInFlow then
        Inc(Result, FlObj.TotalWidth);
      Dec(N); {image counts as one ThtChar}
      Inc(Start);
    end
    else
    begin
      I := Min(Min(J, J1), Min(Fonts.GetFontObjAt(Start - Buff, Len, FO), N));
      FO.TheFont.AssignToCanvas(Canvas);
      Assert(I > 0, 'I less than or = 0 in FindTextWidthA');
      Inc(Result, GetTextExtent(Canvas.Handle, Start, I).cx - FO.Overhang);
      if I = 0 then
        Break;
      Dec(N, I);
      Inc(Start, I);
    end;
  end;
end;

{----------------TSection.DrawLogic}

function TSection.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
{returns height of the section}

  function FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; MaxChars, X, Y: Integer;
    IMgr: TIndentManager; var ImgY, ImgHt: Integer; var DoneFlObjPos: PWideChar): Integer;
  {Given a width, find the count of chars (<= Max) which will fit allowing for font changes.
    Line wrapping will be done later}
  //BG, 06.02.2011: Why are there 2 methods and why can't GetURL and FindCursor use the formatting results of DrawLogic?
  //  FindCountThatFits1() is part of TSection.DrawLogic() and fills IMgr with the embedded floating objects.
  //  TSection.FindCountThatFits() is used in TSection.GetURL() and TSection.FindCursor().


  type
    TResultCode = (rsOk, rsContinue, rsBreak);

    function DrawLogicOfObject(FlObj: TFloatingObj; var XX, YY, Width, Cnt, FloatingImageCount: Integer): TResultCode;
    var
      X1, X2, W, H: Integer;
    begin
      Result := rsOk;
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
              FlObj.FIndent := IMgr.AlignLeft(ImgY, W) + FlObj.HSpaceL;
              IMgr.AddLeft(ImgY, ImgY + H, W);
            end;

            ARight:
            begin
              FlObj.FIndent := IMgr.AlignRight(ImgY, W) + FlObj.HSpaceL;
              IMgr.AddRight(ImgY, ImgY + H, W);
            end;
          end;
          FlObj.DrawXX := FlObj.FIndent;
          FlObj.DrawYY := ImgY + FlObj.VSpaceT;
          ImgHt := Max(ImgHt, H);
          DoneFlObjPos := Start;

          // go on with the line:
          X1 := IMgr.LeftIndent(Y);
          X2 := IMgr.RightSide(Y);
          Width := X2 - X1;
          Inc(FloatingImageCount);
          if Cnt >= FloatingImageCount then
            Result := rsContinue;
        end;
      end
      else if not (FlObj.Positioning in [posAbsolute, posFixed]) then
      begin
        ImgHt := Max(ImgHt, FlObj.TotalHeight);
        Inc(XX, FlObj.TotalWidth);
        if XX > Width then
          Result := rsBreak;
      end;
    end;

  var
    Cnt,
    I,  // number of fitting chars
    J,  // number of chars up to next font change or object
    J1, // number of chars up to next font change
    J2, // number of chars up to next image
    J3, // number of chars up to next form control
    X1, X2, Width, D, H: Integer;
    XX: Integer; // current width of row in pixels (== current horizontal position).
    YY: Integer; // current height of row in pixels.
    Picture: Boolean;
    FlObj: TFloatingObj; //TSizeableObj;
    FcObj: TFloatingObj; //TFormControlObj;
    FO: TFontObj;
    BrChr, TheStart: PWideChar;
    //Font,
    LastFont: ThtFont;
    Save: TSize;
    FoundBreak: Boolean;
    HyphenWidth: Integer;
    FloatingImageCount: Integer;
    InitialFloatingLeftCount: Integer;
    InitialFloatingRightCount: Integer;
  begin
    LastFont := nil;
    TheStart := Start;
    ImgHt := 0;
    InitialFloatingLeftCount := IMgr.L.Count;
    InitialFloatingRightCount := IMgr.R.Count;

    BrChr := StrScanW(TheStart, BrkCh); {see if a break char}
    FoundBreak := Assigned(BrChr) and (BrChr - TheStart < MaxChars);
    if FoundBreak then
    begin
      MaxChars := BrChr - TheStart;
      if MaxChars = 0 then
      begin
        Result := 1;
        Exit; {single character fits}
      end;
    end;

    X1 := IMgr.LeftIndent(Y);
    if Start = Buff then
      Inc(X1, FirstLineIndent);
    X2 := IMgr.RightSide(Y);
    Width := X2 - X1;

    if (Start = Buff) and (Images.Count = 0) and (FormControls.Count = 0) then
      if Fonts.GetFontObjAt(0, Len, FO) = Len then
        if MaxChars * Fonts[0].tmMaxCharWidth <= Width then {try a shortcut}
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
      J1 := Min(Fonts.GetFontObjAt(Start - Buff, Len, FO), MaxChars - Cnt);
      if FO.TheFont <> LastFont then {may not have to load font}
      begin
        LastFont := FO.TheFont;
        LastFont.AssignToCanvas(Canvas);
      end;
      J2 := Images.GetObjectAt(Start - Buff, FlObj);
      J3 := FormControls.GetObjectAt(Start - Buff, FcObj);
      if (J2 = 0) or (J3 = 0) then
      begin {next is an object}
        if J2 <> 0 then
          FlObj := FcObj;
        I := 1;
        J := 1;
        Picture := True;
        case DrawLogicOfObject(FlObj, XX, YY, Width, Cnt, FloatingImageCount) of
          rsContinue:
          begin
            // after an object floating to the left or right, we must retry to
            // fit the remaining space between the floating objects.
            Start := TheStart;
            Cnt := 0;
            XX := 0;
            continue;
          end;

          rsBreak:
            break;
        end;
      end
      else
      begin
        Picture := False;
        J := Min(J1, J2);
        J := Min(J, J3);
        I := FitText(Canvas.Handle, Start, J, Width - XX, Save);
        if (I > 0) and (Brk[TheStart - Buff + Cnt + I - 1] = twSoft) then
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

    // adjust floating objects top position, in case they have been moved down and line height has been changed.
    H := YY; //Max(YY, ImgHt);
    IMgr.AdjustY(InitialFloatingLeftCount, InitialFloatingRightCount, Y, H);

    D := 0;
    for I := 0 to Images.Count - 1 do
      with Images[I] do
        if Floating in [ALeft, ARight] then
          if DrawYY > Y + VSpaceT then
          begin
            if DrawYY < Y + H + VSpaceT then
              D := Y + H + VSpaceT - DrawYY;
            Inc(DrawYY, D);
          end;

    D := 0;
    for I := 0 to FormControls.Count - 1 do
      with FormControls[I] do
        if Floating in [ALeft, ARight] then
          if DrawYY > Y + VSpaceT then
          begin
            if DrawYY < Y + H + VSpaceT then
              D := Y + H + VSpaceT - DrawYY; // FYValue;
            Inc(DrawYY, D);
          end;
  end;

  procedure DoDrawLogic;

    procedure DrawLogicOfObjects(Objects: TFloatingObjList; Width: Integer);
    var
      I: Integer;
      Obj: TFloatingObj;
    begin
      for I := 0 to Objects.Count - 1 do {call drawlogic for all the objects}
      begin
        Obj := Objects[I];
        Obj.DrawLogicInline(Canvas, Fonts.GetFontObjAt(Obj.StartCurs), 0, 0);
        // BG, 28.08.2011:
        if OwnerBlock.HideOverflow then
        begin
          if Obj.ClientWidth > Width then
            Obj.ClientWidth := Width;
        end
        else
          MaxWidth := Max(MaxWidth, Obj.ClientWidth); {HScrollBar for wide images}
      end;
    end;

  var
    PStart, Last: PWideChar;
    ImgHt: Integer;
    Finished: Boolean;
    LR: ThtLineRec;
    AccumImgBot: Integer;

    procedure LineComplete(NN: Integer);
    var
      I, J, DHt, Desc, Tmp, TmpRt, Cnt, H, SB, SA: Integer;
      FO: TFontObj;
      Align: ThtAlignmentStyle;
      NoChar: Boolean;
      P: PWideChar;
      FlObj: TFloatingObj; //TSizeableObj;
      FcObj: TFloatingObj; //TFormControlObj;
      LRTextWidth: Integer;
      OHang: Integer;

      function FindSpaces: Integer;
      var
        I: Integer;
      begin
        Result := 0;
        for I := 0 to NN - 2 do {-2 so as not to count end spaces}
          if ((PStart + I)^ = SpcChar) or ((PStart + I)^ = NbSpcChar) then
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

      Align := ANone;
      if not NoChar then
      begin
        repeat
          J := Fonts.GetFontObjAt(PStart - Buff + Cnt, Len, FO);
          Tmp := FO.GetHeight(Desc);
          DHt := Max(DHt, Tmp);
          LR.Descent := Max(LR.Descent, Desc);
          Inc(Cnt, J);
        until Cnt >= NN;
        Align := FO.SScript;
      end;

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
//            aNone,
//            aBaseline,
//            aBottom:
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
        Inc(Cnt, Images.GetObjectAt(PStart - Buff + Cnt, FlObj));
        if Cnt < NN then
        begin
          H := FlObj.TotalHeight;
          if FlObj.Floating = ANone then
          begin
            FlObj.DrawYY := Y; {approx y dimension}
            if (FLObj is TImageObj) and Assigned(TImageObj(FLObj).MyFormControl) then
              TImageObj(FLObj).MyFormControl.DrawYY := Y; // FYValue := Y;
            if not (FlObj.Positioning in [posAbsolute, posFixed]) then
            begin
              case FlObj.VertAlign of
                aTop:
                  SA := Max(SA, H - DHt);

                aMiddle:
                  begin
                    if DHt = 0 then
                    begin
                      DHt := Fonts.GetFontObjAt(PStart - Buff).GetHeight(Desc);
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
        end;
        Inc(Cnt); {to skip by the image}
      until Cnt >= NN;

      Cnt := 0; {now check on form controls}
      repeat
        Inc(Cnt, FormControls.GetObjectAt(PStart - Buff + Cnt, FcObj));
        if Cnt < NN then
        begin
          H := FcObj.TotalHeight;
          if FcObj.Floating = ANone then
          begin
            case FcObj.VertAlign of
              ATop:
                SA := Max(SA, H - Dht);
              AMiddle:
                begin
                  Tmp := (FcObj.ClientHeight - DHt) div 2;
                  SA := Max(SA, Tmp + FcObj.VSpaceB);
                  SB := Max(SB, (FcObj.ClientHeight - DHt - Tmp + FcObj.VSpaceT));
                end;
              ABaseline:
                SB := Max(SB, H - (DHt - LR.Descent));
              ABottom:
                SB := Max(SB, H - DHt);
            end;
            if not IsCopy then
              FcObj.DrawYY := Y; //FYValue := Y;
          end;
        end;
        Inc(Cnt); {to skip by the control}
      until Cnt >= NN;

  {$IFNDEF NoTabLink}
      Cnt := 0; {now check URLs}
      repeat
        Inc(Cnt, Fonts.GetFontObjAt(PStart - Buff + Cnt, Len, FO));
        FO.AssignY(Y);
      until Cnt >= NN;
  {$ENDIF}

      LR.Start := PStart;
      LR.LineHt := DHt;
      LR.Ln := NN;
      if Brk[PStart - Buff + NN - 1] = twSoft then {see if there is a soft hyphen on the end}
        LR.Shy := True;
      TmpRt := IMgr.RightSide(Y);
      Tmp := IMgr.LeftIndent(Y);
      if PStart = Buff then
        Inc(Tmp, FirstLineIndent);

      LRTextWidth := FindTextSize(Canvas, PStart, NN, True).cx;
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
    N, NN, Width, I: Integer;
    Tmp: Integer;
    Obj: TFloatingObj;
    TopY, HtRef: Integer;
    //Ctrl: TFormControlObj;
    //BG, 06.02.2011: floating objects:
    PDoneFlObj: PWideChar;
    YDoneFlObj: Integer;
  begin {DoDrawLogic}
    SectionHeight := 0;
    AccumImgBot := 0;
    TopY := Y;
    PStart := Buff;
    Last := Buff + Len - 1;
    if Len = 0 then
    begin
      Result := GetClearSpace(IMgr, Y);
      DrawHeight := Result;
      SectionHeight := Result;
      ContentBot := Y + Result;
      DrawBot := ContentBot;
      MaxWidth := 0;
      DrawWidth := 0;

      DrawRect.Left   := X;
      DrawRect.Top    := DrawTop;
      DrawRect.Right  := DrawRect.Left + DrawWidth;
      DrawRect.Bottom := DrawBot;
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
      Obj := Images[I];
      Obj.DrawLogicInline(Canvas, Fonts.GetFontObjAt(Obj.StartCurs), Width, HtRef);
      // BG, 28.08.2011:
      if OwnerBlock.HideOverflow then
      begin
        if Obj.ClientWidth > Width then
          Obj.ClientWidth := Width;
      end
      else
        MaxWidth := Max(MaxWidth, Obj.ClientWidth); {HScrollBar for wide images}
    end;

    for I := 0 to FormControls.Count - 1 do
    begin
      Obj := FormControls[I];
      Obj.DrawLogicInline(Canvas, Fonts.GetFontObjAt(Obj.StartCurs), Width, HtRef);
      // BG, 28.08.2011:
      if OwnerBlock.HideOverflow then
      begin
        if Obj.ClientWidth > Width then
          Obj.ClientWidth := Width;
      end
      else
        MaxWidth := Max(MaxWidth, Obj.ClientWidth);
    end;

    YDoneFlObj := Y;
    PDoneFlObj := PStart - 1;
    while not Finished do
    begin
      MaxChars := Last - PStart + 1;
      if MaxChars <= 0 then
        Break;
      LR := ThtLineRec.Create(Document); {a new line}
      if Lines.Count = 0 then
      begin {may need to move down past floating image}
        Tmp := GetClearSpace(IMgr, Y);
        if Tmp > 0 then
        begin
          LR.LineHt := Tmp;
          Inc(SectionHeight, Tmp);
          LR.Ln := 0;
          LR.Start := PStart;
          Inc(Y, Tmp);
          Lines.Add(LR);
          LR := ThtLineRec.Create(Document);
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
        if ((P^ = SpcChar) {or (P^ = FmCtl,} or (P^ = ImgPan) or WrapChar(P^)) and (Brk[P - Buff] <> twNo) or (P^ = BrkCh) then
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
        if ((P^ = SpcChar) or (P^ = FmCtl) or (p^ = ImgPan) or WrapChar(P^)) and (Brk[P - Buff] <> twNo) or (P^ = BrkCh) then
        begin {move past spaces so as not to print any on next line}
          while (N < MaxChars) and ((P + 1)^ = ' ') do
          begin
            Inc(P);
            Inc(N);
          end;
          Finished := N >= MaxChars;
          LineComplete(N);
        end
        else if (N < MaxChars) and ((P + 1)^ = ' ') and (Brk[P - Buff + 1] <> twNo) then
        begin
          repeat
            Inc(P);
            Inc(N); {pass the space}
          until (N >= MaxChars) or ((P + 1)^ <> ' ');
          Finished := N >= MaxChars;
          LineComplete(N);
        end
        else if (N < MaxChars) and (((P + 1)^ = FmCtl) or ((P + 1)^ = ImgPan)) and (Brk[PStart - Buff + N - 1] <> twNo) then {an image or control}
        begin
          Finished := False;
          LineComplete(N);
        end
        else
        begin
          {non space, wrap it by backing off to previous wrappable char}
          while P > PStart do
          begin
            case Brk[P - Buff] of
              twNo: ;

              twSoft,
              twOptional:
                break;

            else
              if CanWrap(P) or WrapChar((P + 1)^) then
                break; // can wrap after this or before next char.
            end;
            Dec(P);
          end;

          if (P = PStart) and ((not ((P^ = FmCtl) or (P^ = ImgPan))) or (Brk[PStart - Buff] = twNo)) then
          begin
            {no space found, forget the wrap, write the whole word and any spaces found after it}
            if BreakWord then
              LineComplete(N)
            else
            begin
              P := PStart + N - 1;

              while (P <> Last) and not CanWrapAfter(P) and not (Brk[P - Buff] in [twSoft, twOptional])
              do
              begin
                case Brk[P - Buff + 1] of
                  twNo: ; // must not wrap after this char.
                else
                  case (P + 1)^ of
                    ' ', FmCtl, ImgPan, BrkCh:
                      break; // can wrap before this char.
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
                MaxWidth := Max(MaxWidth, FindTextSize(Canvas, PStart, P - PStart + 1, True).cx);
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
      with Lines[0] do
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

    DrawRect.Left   := X;
    DrawRect.Top    := DrawTop;
    DrawRect.Right  := DrawRect.Left + MaxWidth;
    DrawRect.Bottom := DrawBot;
  end; { DoDrawLogic}

var
  Dummy: Integer;
  Save: Integer;
begin {TSection.DrawLogic}
{$IFDEF JPM_DEBUGGING_LOGIC}
  CodeSite.EnterMethod(Self,'TSection.DrawLogic1');
{$IFDEF JPM_DEBUGGING_LOGIC1}
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then
  begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end
  else
  begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
{$ENDIF}
{$ENDIF}

  YDraw := Y;
  DrawTop := Y;
  ContentTop := Y;
  StartCurs := Curs;
  Lines.Clear;
  TextWidth := 0;

  if WhiteSpaceStyle in [wsPre, wsNoWrap] then
  begin
    if Len = 0 then
    begin
      Result := Fonts.GetFontObjAt(0).FontHeight;
      SectionHeight := Result;
      MaxWidth := 0;
      DrawHeight := Result;
      ContentBot := Y + Result;
      DrawBot := ContentBot;

      DrawRect.Left   := X;
      DrawRect.Top    := DrawTop;
      DrawRect.Right  := DrawRect.Left + MaxWidth;
      DrawRect.Bottom := DrawRect.Top + SectionHeight;
      exit;
    end;

    if not BreakWord then
    begin
    {call with large width to prevent wrapping}
      Save := IMgr.Width;
      IMgr.Width := 32000;
      DoDrawLogic;
      IMgr.Width := Save;
      MinMaxWidth(Canvas, Dummy, MaxWidth, AWidth, AHeight); {return MaxWidth}
      DrawRect.Right := DrawRect.Left + MaxWidth;
      exit;
    end;
  end;

  DoDrawLogic;

{$IFDEF JPM_DEBUGGING_LOGIC}
{$IFDEF JPM_DEBUGGING_LOGIC1}
  if Assigned(IMgr) then begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end else begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
{$ENDIF}
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TSection.DrawLogic1');
{$ENDIF}
end;

{----------------TSection.CheckForInlines}

procedure TSection.CheckForInlines(LR: ThtLineRec);
{called before each line is drawn the first time to check if there are any
 inline borders in the line}
var
  I: Integer;
  BR: ThtBorderRec;
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
            BorderList := ThtBorderRecList.Create;
            FirstDraw := True; {there will be more processing needed}
          end;
          BR := ThtBorderRec.Create;
          BorderList.Add(BR);
          with BR do
          begin
            BR.MargArray := Items[I].MargArray; {get border data}
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
  MySelB, MySelE: Integer;
  YOffset, Y, Desc: Integer;

  //>-- DZ 19.09.2012
  procedure AdjustDrawRect( aTop, aLeft, aWidth, aHeight: Integer ); overload;
  begin
     //dec( aTop, Document.YOff );

     if DrawRect.Top > aTop then
       DrawRect.Top:= aTop;

     if DrawRect.Left > aLeft then
       DrawRect.Left:= aLeft;

     if DrawRect.Right < aLeft + aWidth then
       DrawRect.Right:= aLeft + aWidth;

     if DrawRect.Bottom < aTop + aHeight then
       DrawRect.Bottom:= aTop + aHeight;
  end;

  //>-- DZ 19.09.2012
  procedure AdjustDrawRect( const aRect: TRect ); overload;
  begin
    AdjustDrawRect( aRect.Top, aRect.Left, aRect.Right - aRect.Left, aRect.Bottom - aRect.Top );
  end;

  procedure DrawTheText(LineNo: Integer);
  var
    I, J, J1, J2, J3, J4,
    Addon, TopP, BottomP, LeftT, Tmp, K: Integer;
    FlObj: TFloatingObj;
    FcObj: TFormControlObj;
    FO: TFontObj;
    ARect: TRect;
    Inverted, NewCP: Boolean;
    ForeColor, BackColor: TColor;
    BkMode: Integer;
    CPx, CPy, CP1x: Integer;
    BR: ThtBorderRec;
    LR: ThtLineRec;
    Start: PWideChar;
    Cnt, Descent: Integer;
    St: ThtString;
    Dx: array of Integer;
    pDx: PInteger;

    function AddHyphen(P: PWideChar; N: Integer): ThtString;
    var
      I: Integer;
    begin
      SetLength(Result, N + 1);
      for I := 1 to N do
        Result[I] := P[I - 1];
      Result[N + 1] := ThtChar('-');
    end;

    function CharacterJustification(P: PWideChar; N: Integer): Integer;
    var
      Fit: Integer;
      Size: TSize;
      Ratio: Double;
      Width: Integer;
      Offset: Integer;
      Extra: Integer;
      Spaces: Integer;
      Inserted: Integer;
      PEnd: PWideChar;
    begin
      if (LR.Spaces = 0) or (LR.Extra = 0) then
      begin
        pDx := nil;
        // TextExtent:
        Result := GetTextExtent(Canvas.Handle, P, N).cx;
      end
      else
      begin
        SetLength(Dx, N);
        pDx := @Dx[0];
        GetTextExtentExPointW(Canvas.Handle, P, N, LR.DrawWidth, @Fit, pDx, Size);

        // pDx, resp Dx contants the width of Fit partial strings. Dx[0] width of P[0], Dx[1] width of P[0] and P[1], ...
        // To justify output in ExtTextOut it needs offsets from one character to the next one: Dx[0] is offset from P[0] to P[1], ...
        Width := 0;
        Spaces := 0;
        Inserted := 0;
        Ratio := LR.Extra / LR.Spaces;
        PEnd := @P[N];
        while P <> PEnd do
        begin
          Offset := pDx^ - Width; // remove length of previous partial string

          // insert additional pixels at spaces
          case P^ of
            SpcChar,
            NbSpcChar:
            begin
              Inc(Spaces);

              // add rounded number of pixels:
              Extra := Floor(Spaces * Ratio + 1E-6) - Inserted;
              Inc(Inserted, Extra);
              Inc(Offset, Extra);
            end;
          end;

          Width := pDx^;
          pDx^ := Offset;
          Inc(pDx);
          Inc(P);
        end;

        // Reset pDx for ExtTextOut().
        pDx := @Dx[0];

        // TextExtent:
        Result := Size.cx + Inserted;
      end;
    end;

    function ChkInversion(Start: PWideChar; out Count: Integer): Boolean;
    var
      LongCount, C: Integer;
    begin
      Result := False;
      C := Start - Buff;
      Count := 32000;
      if IsCopy then
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
    LR := Lines[LineNo];
    Start := LR.Start;
    Cnt := LR.Ln;
    Descent := LR.Descent;

    NewCP := True;
    CPy := Y + LR.DrawY;  //Todo: Someone needs to find a sensible default value.
    CPx := X + LR.LineIndent;
    {$IFNDEF Compiler32_Plus}CP1x := CPx;{$ENDIF}
    LR.DrawY := Y - LR.LineHt;
    LR.DrawXX := CPx;
    AdjustDrawRect( LR.DrawY, LR.DrawXX, LR.DrawWidth, LR.LineHt ); //>-- DZ 19.09.2012
    while Cnt > 0 do
    begin
      I := 1;
      J1 := Fonts.GetFontObjAt(Start - Buff, Len, FO) - 1;
      J2 := Images.GetObjectAt(Start - Buff, FlObj) - 1;
      J4 := FormControls.GetObjectAt(Start - Buff, FcObj) - 1;

    {if an inline border, find it's boundaries}
      if LR.FirstDraw and Assigned(LR.BorderList) then
        for K := 0 to LR.BorderList.Count - 1 do {may be several inline borders}
        begin
          BR := LR.BorderList[K];
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
      ForeColor := Canvas.Font.Color;
      BackColor := FO.TheFont.bgColor;
      if ForeColor <> clNone then
         ForeColor := Document.ThemedColorToRGB(ForeColor, htseFont);
      if BackColor <> clNone then
         BackColor := Document.ThemedColorToRGB(BackColor, htseFont);
      Canvas.Font.Color := ForeColor;
      if J2 = -1 then
      begin {it's an image or panel}
        if FlObj is TImageObj then
        begin
          if FlObj.Floating in [ALeft, ARight] then
          begin
            //BG, 02.03.2011: Document is the Document, thus we must
            //  feed it with document coordinates: X,Y is in document coordinates,
            //  but might not be the coordinates of the upper left corner of the
            //  containing block, the origin of the Obj's coordinates. If each block
            //  had its own IMgr and nested blocks had nested IMgrs with coordinates
            //  relative to the containing block, the document coordinates of an inner
            //  block were the sum of all LfEdges of the containing blocks.
            //
            // correct x-position for floating images: IMgr.LfEdge + Obj.Indent
            if not LR.FirstDraw then
              Document.DrawList.AddImage(TImageObj(FlObj), Canvas,
                IMgr.LfEdge + FlObj.FIndent, FlObj.DrawYY, Y - Descent, FO)
            else if Assigned(LR.BorderList) then
              {if a boundary is on a floating image, remove it}
              for K := LR.BorderList.Count - 1 downto 0 do
              begin
                BR := LR.BorderList[K];
                if (Start - Buff = BR.BStart) and (BR.BEnd = BR.BStart + 1) then
                  LR.BorderList.Delete(K);
              end;
          end
          else
          begin
            SetTextJustification(Canvas.Handle, 0, 0);
//            if OwnerBlock <> nil then
//              FlObj.Positioning := OwnerBlock.Positioning
//            else
//              FlObj.Positioning := posStatic;
            TImageObj(FlObj).DrawInline(Canvas, CPx + FlObj.HSpaceL, LR.DrawY, Y - Descent, FO);
            Document.Printed := True;
          {see if there's an inline border for the image}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := 0 to LR.BorderList.Count - 1 do
              begin
                BR := LR.BorderList[K];
                if (Start - Buff >= BR.BStart) and (Start - Buff <= BR.BEnd) then
                begin {there is a border here, find the image dimensions}
                  TopP := 0;
                  if not (FlObj.Positioning in [posAbsolute, posFixed]) then
                  begin
                    case FlObj.VertAlign of

                      ATop, ANone:
                        TopP := Y - LR.LineHt + FlObj.VSpaceT;

                      AMiddle:
                        TopP := Y - Descent + FO.Descent - FO.tmHeight div 2 - (FlObj.ClientHeight - FlObj.VSpaceT + FlObj.VSpaceB) div 2;

                      ABottom, ABaseline:
                        TopP := Y - Descent - FlObj.VSpaceB - FlObj.ClientHeight;

                    end;
                    BottomP := TopP + FlObj.ClientHeight;

                    if Start - Buff = BR.BStart then
                    begin {border starts at image}
                      BR.bRect.Top := TopP;
                      BR.bRect.Left := CPx + FlObj.HSpaceL;
                      if BR.BEnd = BR.BStart + 1 then {border ends with image also, rt side set by image width}
                        BR.bRect.Right := BR.bRect.Left + FlObj.ClientWidth;
                      BR.bRect.Bottom := BottomP;
                    end
                    else if Start - Buff <> BR.BEnd then
                    begin {image is included in border and may effect the border top and bottom}
                      BR.bRect.Top := Min(BR.bRect.Top, TopP);
                      BR.bRect.Bottom := Max(BR.bRect.Bottom, BottomP);
                    end;
                  end;
                end;
              end;

            if not (FlObj.Positioning in [posAbsolute, posFixed]) then
            begin
              CPx := CPx + FlObj.TotalWidth;
              NewCP := True;
            end;
          end;
        end
        else
        begin {it's a Panel or Frame}
          if FlObj is TControlObj then
            TControlObj(FlObj).ShowIt := True;
          if FlObj.Floating in [ALeft, ARight] then
          begin
            LeftT := IMgr.LfEdge + FlObj.FIndent;
            TopP := FlObj.DrawYY;
            {check for border.  For floating panel, remove it}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := LR.BorderList.Count - 1 downto 0 do
              begin
                BR := LR.BorderList[K];
                if (Start - Buff = BR.BStart) and (BR.BEnd = BR.BStart + 1) then
                  LR.BorderList.Delete(K);
              end;
          end
          else
          begin
            LeftT := CPx + FlObj.HSpaceL;
            case FlObj.VertAlign of
              ANone,
              ATop:      TopP := Y - LR.LineHt + FlObj.VSpaceT;

              AMiddle:   TopP := Y - FO.tmHeight div 2 - (FlObj.ClientHeight - FlObj.VSpaceT + FlObj.VSpaceB) div 2;

              ABottom,
              ABaseline: TopP := Y - Descent - FlObj.ClientHeight - FlObj.VSpaceB;
            else
                         TopP := 0; {to eliminate warning msg}
            end;
          {Check for border on inline panel}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := 0 to LR.BorderList.Count - 1 do
              begin
                BR := LR.BorderList[K];
                if (Start - Buff >= BR.BStart) and (Start - Buff <= BR.BEnd) then
                begin
                  if (Start - Buff = BR.BStart) then
                  begin {border starts on panel}
                    BR.bRect.Top := TopP;
                    BR.bRect.Left := CPx + FlObj.HSpaceL;
                    if BR.BEnd = BR.BStart + 1 then {border also ends with panel}
                      BR.bRect.Right := BR.bRect.Left + FlObj.ClientWidth;
                    BR.bRect.Bottom := TopP + FlObj.ClientHeight;
                  end
                  else if Start - Buff = BR.BEnd then
                  else
                  begin {Panel is included in border, may effect top and bottom}
                    BR.bRect.Top := Min(BR.bRect.Top, TopP);
                    BR.bRect.Bottom := Max(BR.bRect.Bottom, TopP + FlObj.ClientHeight);
                  end;
                end;
              end;
            Inc(CPx, FlObj.TotalWidth);
            NewCP := True;
          end;

          FlObj.DrawInline(Canvas, LeftT, TopP - YOffset, TopP - YOffset - Descent, FO);
          Document.Printed := True;

        end;
      end
      else if J4 = -1 then
      begin {it's a form control}
        if not FcObj.Hidden then
        begin
          FcObj.ShowIt := True;
          if FcObj.Floating in [ALeft, ARight] then
          begin
            LeftT := IMgr.LfEdge + FcObj.Indent;
            TopP := FcObj.DrawYY - YOffset;
            {check for border.  For floating panel, remove it}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := LR.BorderList.Count - 1 downto 0 do
              begin
                BR := LR.BorderList[K];
                if (Start - Buff = BR.BStart) and (BR.BEnd = BR.BStart + 1) then
                  LR.BorderList.Delete(K);
              end;
          end
          else
          begin
            LeftT := CPx + FcObj.HSpaceL;
            case FcObj.VertAlign of
              ANone,
              ATop:      TopP := LR.DrawY + FcObj.VSpaceT - YOffset;
              AMiddle:   TopP := Y - ((LR.LineHt + FcObj.ClientHeight) div 2) - YOffset;
              ABaseline: TopP := Y - FcObj.ClientHeight - FcObj.VSpaceB - Descent - YOffset; {sits on baseline}
              ABottom:   TopP := Y - FcObj.ClientHeight - FcObj.VSpaceB - YOffset;
            else
                         TopP := Y; {never get here}
            end;
            if FcObj is TRadioButtonFormControlObj then
              Inc(Topp, 2)
            else if FcObj is TCheckBoxFormControlObj then
              Inc(Topp, 1);

          {Check for border}
            if LR.FirstDraw and Assigned(LR.BorderList) then
              for K := 0 to LR.BorderList.Count - 1 do
              begin
                BR := LR.BorderList[K];
                if (Start - Buff >= BR.BStart) and (Start - Buff <= BR.BEnd) then
                begin
                  if (Start - Buff = BR.BStart) then
                  begin {border starts with Form control}
                    BR.bRect.Top := ToPP + YOffSet;
                    BR.bRect.Left := CPx + FcObj.HSpaceL;
                    if BR.BEnd = BR.BStart + 1 then {border is confined to form control}
                      BR.bRect.Right := BR.bRect.Left + FcObj.ClientWidth;
                    BR.bRect.Bottom := TopP + YOffSet + FcObj.ClientHeight;
                  end
                  else if Start - Buff = BR.BEnd then
                  else
                  begin {form control is included in border}
                    BR.bRect.Top := Min(BR.bRect.Top, ToPP + YOffSet);
                    BR.bRect.Bottom := Max(BR.bRect.Bottom, TopP + YOffSet + FcObj.ClientHeight);
                  end;
                end;
              end;
          end;

          FcObj.DrawInline(Canvas, LeftT, TopP, TopP - Descent, FO);
          Document.Printed := True;

          if not (FcObj.Floating in [ALeft, ARight]) then
          begin
            Inc(CPx, FcObj.TotalWidth);
            NewCP := True;
          end;
        end;
      end
      else
      begin
        J := Min(J1, J2);
        J := Min(J, J4);
        Inverted := ChkInversion(Start, J3);
        J := Min(J, J3 - 1);
        I := Min(Cnt, J + 1);

        BkMode := Opaque;
        if Inverted then
        begin
          if BackColor = clNone then
            Canvas.Font.Color := ForeColor xor $FFFFFF
          else
            Canvas.Font.Color := BackColor;
          BackColor := ForeColor;
        end
        else
          if BackColor = clNone then
            BkMode := Transparent;

        if BkMode = Opaque then
        begin
          Canvas.Brush.Color := BackColor;
          Canvas.Brush.Style := bsSolid;
{$ifdef LCLgtk2}
          // background is missing if Canvas.Brush.Color = GetBkColor(Canvas.Handle)
          if Canvas.Brush.Color = GetBkColor(Canvas.Handle) then
             Canvas.Brush.Color := Canvas.Brush.Color xor $010101;
{$endif}
        end
        else
          Canvas.Brush.Style := bsClear;
        SetBkMode(Canvas.Handle, BkMode);

        if Document.Printing then
        begin
          if Document.PrintMonoBlack and
            (GetDeviceCaps(Canvas.Handle, NumColors) in [0..2]) then
          begin
            if Canvas.Font.Color <> clWhite then
              Canvas.Font.Color := clBlack; {Print black}
          end;
          if not Document.PrintTableBackground then
          begin
            if (Canvas.Font.Color and $E0E0) = $E0E0 then
              Canvas.Font.Color := $2A0A0A0; {too near white or yellow, make it gray}
          end;
        end;

        SetTextAlign(Canvas.Handle, TA_BaseLine);
      {figure any offset for subscript or superscript}
        with FO do
          case SScript of
            aSuper: Addon := -(FontHeight div 3);
            aSub  : Addon := Descent div 2 + 1;
          else
            Addon := 0;
          end;
        NewCP := NewCP or (Addon <> 0);
      {calc a new CP if required}
        if NewCP then
        begin
          CPy := Y - Descent + Addon - YOffset;
          NewCP := Addon <> 0;
        end;

        if not Document.NoOutput and (Canvas.Font.Size <> 0) and (Canvas.Font.Color <> clNone) then
        begin
          Tmp := I;
          if Cnt - I <= 0 then
            case (Start + I - 1)^ of
              ' ', BrkCh:
                Dec(Tmp); {at end of line, don't show space or break}
            end;

          if (WhiteSpaceStyle in [wsPre, wsPreLine, wsNoWrap]) and not OwnerBlock.HideOverflow then
          begin {so will clip in Table cells}
            ARect := Rect(IMgr.LfEdge, Y - LR.LineHt - LR.SpaceBefore - YOffset, X + IMgr.ClipWidth, Y - YOffset + 1);
            ExtTextOutW(Canvas.Handle, CPx, CPy, ETO_CLIPPED, @ARect, Start, Tmp, nil);
            CP1x := CPx + GetTextExtent(Canvas.Handle, Start, Tmp).cx;
          end
          else
          begin
            SetTextJustification(Canvas.Handle, 0, 0);
            if not IsWin95 then {use TextOutW}
            begin
              if (Cnt - I <= 0) and LR.Shy then
              begin
                St := AddHyphen(Start, Tmp);
                CP1x := CPx + CharacterJustification(PWideChar(St), Length(St));
                ExtTextOutW(Canvas.Handle, CPx, CPy, 0, nil, PWideChar(St), Length(St), pDx);
              end
              else
              begin
                CP1x := CPx + CharacterJustification(Start, Tmp);
                ExtTextOutW(Canvas.Handle, CPx, CPy, 0, nil, Start, Tmp, pDx);
              end
            end
            else
            begin {Win95}
            {Win95 has bug which extends text underline for proportional font in TextOutW.
             Use clipping to clip the extra underline.}
              CP1x := CPx + CharacterJustification(Start, Tmp);
              ARect := Rect(CPx, Y - LR.LineHt - LR.SpaceBefore - YOffset, CP1x, Y - YOffset + 1);
              ExtTextOutW(Canvas.Handle, CPx, CPy, ETO_CLIPPED, @ARect, Start, Tmp, pDx);
            end;
          end;
          Document.Printed := True;
        {Put in a dummy caret to show character position}
          if Document.ShowDummyCaret and not Inverted
            and (MySelB = Start - Buff) then
          begin
            Canvas.Pen.Color := Canvas.Font.Color;
            Tmp := Y - Descent + FO.Descent + Addon - YOffset;
            Canvas.Brush.Color := clWhite;
            Canvas.Rectangle(CPx, Tmp, CPx + 1, Tmp - FO.FontHeight);
          end;

          if FO.Active or IsCopy and Assigned(Document.LinkDrawnEvent)
            and (FO.UrlTarget.Url <> '') then
          begin
            Tmp := Y - Descent + FO.Descent + Addon - YOffset;
            ARect := Rect(CPx, Tmp - FO.FontHeight, CP1x + 1, Tmp);
            if FO.Active then
            begin
              Canvas.Brush.Color := clWhite;
              Canvas.Font.Color := clBlack; {black font needed for DrawFocusRect}
              Canvas.Handle; {Dummy call needed to make Delphi add font color change to handle}
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
            and ((MySelB = Len) and (Document.SelB = Document.Len))
            and (Cnt = I) and (LineNo = Lines.Count - 1) then
          begin
            Canvas.Pen.Color := Canvas.Font.Color;
            Tmp := Y - Descent + FO.Descent + Addon - YOffset;
            Canvas.Brush.Color := clWhite;
            Canvas.Rectangle(CPx, Tmp, CPx + 1, Tmp - FO.FontHeight);
          end;
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
        BR := LR.BorderList[K];
        if BR.OpenEnd or (BR.BRect.Right = 0) then
          BR.BRect.Right := CPx;

        AdjustDrawRect(BR.bRect); //>-- DZ 19.09.2012
      end;
  end;

  procedure DoDraw(I: Integer);
  {draw the Ith line in this section}
  var
    BR: ThtBorderRec;
    K: Integer;
    XOffset: Integer;
  begin
    with Lines[I] do
    begin
      Inc(Y, LineHt + SpaceBefore);
      if FirstDraw then
      begin {see if any inline borders in this line}
        CheckForInlines(Lines[I]);
        if FirstDraw then {if there are, need a first pass to get boundaries}
        begin
          FirstX := X;
          DrawTheText(I);
        end;
      end;
      XOffset := X - FirstX;
      FirstDraw := False;
      Drawn := True;
      if Assigned(BorderList) then {draw any borders found in this line}
        for K := 0 to BorderList.Count - 1 do
        begin
          BR := BorderList[K];
          BR.DrawTheBorder(Canvas, XOffset, YOffSet, Document.Printing, Document.ThemedColorToRGB);
        end;
      DrawTheText(I); {draw the text, etc., in this line}
      Inc(Y, SpaceAfter);
    end;
    Document.FirstPageItem := False;
  end;

var
  I: Integer;
  DC: HDC;
begin {TSection.Draw}
  Y := YDraw;
  Result := Y + SectionHeight;

  if (OwnerBlock <> nil) and (OwnerBlock.Positioning = posFixed) then
    YOffset := 0
  else
    YOffset := Document.YOff;

{Only draw if will be in display rectangle}
  if (Len > 0) and (Y - YOffset + DrawHeight + 40 >= ARect.Top) and (Y - YOffset - 40 < ARect.Bottom) then
  begin
    DC := Canvas.Handle;
    SetTextAlign(DC, TA_BaseLine);

    MySelB := Document.SelB - StartCurs;
    MySelE := Document.SelE - StartCurs;
    for I := 0 to Lines.Count - 1 do
      if Document.Printing then
        with Lines[I] do
        begin
          if (Y + LineImgHt <= Document.PageBottom) then
          begin
            if (Y - YOffSet + LineImgHt - 1 > ARect.Top) then
              DoDraw(I)
            else
              Inc(Y, SpaceBefore + LineHt + SpaceAfter);
          end
          else if (LineImgHt >= ARect.Bottom - ARect.Top) or Document.PageShortened then
            DoDraw(I)
          else
          begin
            if (OwnerBlock <> nil) and (OwnerBlock.Positioning = posAbsolute) then
              DoDraw(I)
            else if Y < Document.PageBottom then
              Document.PageBottom := Y; {Dont' print, don't want partial line}
          end;
        end
      else
        with Lines[I] do
          if ((Y - YOffset + LineImgHt + 40 >= ARect.Top) and (Y - YOffset - 40 < ARect.Bottom)) then
            DoDraw(I)
          else {do not completely draw extremely long paragraphs}
            Inc(Y, SpaceBefore + LineHt + SpaceAfter);
  end;
end;

{----------------TSection.CopySelectedText}

procedure TSection.CopySelectedText;
var
  I, Strt, X1, X2: Integer;
  MySelB, MySelE: Integer;
  TextAdded: Boolean;
begin
  TextAdded := False;
  MySelB := Document.SelB - StartCurs;
  MySelE := Document.SelE - StartCurs;
  for I := 0 to Lines.Count - 1 do
    with Lines.Items[I] do
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
      // Something was copied. Possibly add CR.
      TextAdded := True;
    end;
  if (MySelE > Len) and TextAdded then
    Document.CB.AddTextCR('', 0);
end;

{----------------TSection.PtInObject}

function TSection.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;
{Y is distance from start of section}
begin
  if Images.PtInObject(X, Y, Obj, IX, IY) then
    Result := True
  else
    Result := inherited PtInObject(X, Y, Obj, IX, IY);
end;

{----------------TSection.GetURL}

function TSection.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj};
  out ATitle: ThtString): ThtguResultType;
 {Y is absolute}
var
  I, L, Width, IX, IY, Posn: Integer;
  FO: TFontObj;
  LR: ThtLineRec;
  IMap, UMap: Boolean;
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
    if ImageObj.Title <> '' then
    begin
      ATitle := ImageObj.Title;
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
      FO := Fonts.GetFontObjAt(Posn);
      if (FO.UrlTarget.Url <> '') then
      begin {found an URL}
        Include(Result, guUrl);
        UrlTarg := MakeCopy(FO.UrlTarget);
        Document.ActiveLink := FO;
        if IMap then
          UrlTarg.Url := UrlTarg.Url + htString('?' + IntToStr(IX) + ',' + IntToStr(IY));
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
        LR := Lines[I];
        if (Y > LR.DrawY) and (Y <= LR.DrawY + LR.LineHt) and LR.Drawn then
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
      FO := Fonts.GetFontObjAt(L + (Start - Buff));
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
  out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer;
{Given an X, Y, find the character position and the resulting XR, YR position
 for a caret along with its height, CaretHt.  Coordinates are relative to this
 section}
var
  I, H, L, Width, TotalHt, L1, W, Delta, OHang: Integer;
  LR: ThtLineRec;

begin
  Result := -1;
  I := 0; H := ContentTop; L1 := 0;
  if H >= Y then
    Exit;
  LR := nil;
  while I < Lines.Count do
  begin
    LR := Lines[I];
    with LR do
      TotalHt := LineHt + SpaceBefore + SpaceAfter;
    if H + TotalHt > Y then
      Break;
    Inc(H, TotalHt);
    Inc(I);
    Inc(L1, LR.Ln); {L1 accumulates ThtChar count of previous lines}
  end;
  if (I >= Lines.Count) then
    Exit;

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
    W := FindTextSize(Canvas, Start, L, False).cx;
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

function TSection.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
{find the first occurance of the ThtString, ToFind, with a cursor value >= From.
 ToFind is in lower case if MatchCase is False. ToFind is known to have a length of at least one.
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

function TSection.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
{find the first occurance of the ThtString, ToFind, with a cursor value <= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length of at least one.
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
  IO: ThtIndexObj;
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

function TSection.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
{for a given Source position, find the nearest document position either Next or
 previous}
var
  I: Integer;
  IO, IOPrev: ThtIndexObj;
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
  {after the last ThtIndexObj in list}
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
  {after the last ThtIndexObj in list}
    Result := StartCurs + IOPrev.Pos + (SourcePos - IOPrev.Index);
  end;
end;

{----------------TSection.CursorToXY}

function TSection.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean;
var
  I, Curs: Integer;
  LR: ThtLineRec;
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
      LR := Lines[I];
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
    X := LR.DrawXX + FindTextSize(Canvas, LR.Start, Curs, False).cx;
    if LR.Spaces > 0 then
      SetTextJustification(Canvas.Handle, 0, 0);
  end
  else
    X := LR.DrawXX;
  Result := True;
end;

{----------------TSection.GetChAtPos}

function TSection.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean;
begin
  Result := False;
  if (Len = 0) or (Pos < StartCurs) or (Pos >= StartCurs + Len) then
    Exit;
  Ch := Buff[Pos - StartCurs];
  Obj := Self;
  Result := True;
end;

{----------------TPanelObj.Create}

constructor TPanelObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties; ObjectTag: Boolean);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  Source, AName, AType: ThtString;
begin
  inherited Create(Parent, Position, L, Prop);
  VertAlign := ABottom; {default}
  FFloating := ANone;
  PntPanel := {TPaintPanel(}Document.PPanel{)};
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
{$ifdef has_StyleElements}
    Panel.StyleElements := Document.StyleElements;
{$endif}
  end;

  if not PercentWidth and (SpecWidth > 0) then
    Panel.Width := SpecWidth;

  if not PercentHeight and (SpecHeight > 0) then
    Panel.Height := SpecHeight;

  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        SrcSy:
          Source := Name;

        NameSy:
          begin
            AName := Name;
            try
              Panel.Name := htStringToString(Name);
            except {duplicate name will be ignored}
            end;
          end;

        AltSy:
          begin
            SetAlt(CodePage, Name);
            Title := Alt; {use Alt as default Title}
          end;

        BorderSy:
          begin
            NoBorder := Value = 0;
            BorderWidth := Min(Max(0, Value), 10);
          end;

        TypeSy:
          AType := Name;
      end;

  with Panel do
  begin
    Caption := '';
    if not ObjectTag and Assigned(Document.PanelCreateEvent) then
      Document.PanelCreateEvent(Document.TheOwner, AName, AType, Source, Panel);
    SetWidth := Width;
    SetHeight := Height;
  end;
  Document.PanelList.Add(Self);
end;

constructor TPanelObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TPanelObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  Panel := ThvPanel.Create(nil);
  with T.Panel do
    Panel.SetBounds(Left, Top, Width, Height);
  Panel.FVisible := T.Panel.FVisible;
  Panel.Color := T.Panel.Color;
  Panel.Parent := Document.PPanel;
  SetHeight := T.SetHeight;
  SetWidth := T.SetWidth;
  OPanel := T.Panel; {save these for printing}
  OSender := T.Document.TheOwner;
  PanelPrintEvent := T.Document.PanelPrintEvent;
end;

destructor TPanelObj.Destroy;
begin
  if Assigned(Document) and Assigned(Document.PanelDestroyEvent) then
    Document.PanelDestroyEvent(Document.TheOwner, Panel);
  Panel.Free;
  inherited Destroy;
end;

procedure TPanelObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
var
  Bitmap: TBitmap;
  OldHeight, OldWidth: Integer;
begin
  inherited DrawInline(Canvas,X,Y,YBaseline,FO);
  if IsCopy then
  begin
    if Panel.FVisible then
    begin
      if Assigned(PanelPrintEvent) then
      begin
        Bitmap := TBitmap.Create;
        OldHeight := Opanel.Height;
        OldWidth := Opanel.Width;
        try
          Bitmap.Height := ClientHeight;
          Bitmap.Width := ClientWidth;
          OPanel.SetBounds(OPanel.Left, OPanel.Top, ClientWidth, ClientHeight);
          PanelPrintEvent(OSender, OPanel, Bitmap);
          PrintBitmap(Canvas, X, Y, ClientWidth, ClientHeight, Bitmap);
        finally
          OPanel.SetBounds(OPanel.Left, OPanel.Top, OldWidth, OldHeight);
        end;
      end;
    end;
  end
  else
  begin
    if Panel.FVisible then
      Panel.Show
    else
      Panel.Hide;
  end;
end;

//-- BG ---------------------------------------------------------- 15.12.2011 --
function TPanelObj.GetBackgroundColor: TColor;
begin
  if Panel <> nil then
    Result := Panel.Color
  else
    Result := inherited GetBackgroundColor;
end;

//-- BG ---------------------------------------------------------- 16.11.2011 --
function TPanelObj.GetControl: TWinControl;
begin
  Result := Panel;
end;

{----------------TProgressObj.Create}

constructor TProgressObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties; ObjectTag: Boolean);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  PositionSet: Boolean;
begin
  inherited Create(Parent, Position, L, Prop);
  PositionSet := false;
  VertAlign := ABottom; {default}
  FFloating := ANone;
  PntPanel := Document.PPanel;
  Progress := ThvProgressBar.Create(PntPanel);
  with Progress do
  begin
    Left := -4000;
    Top := -4000;
    Parent := PntPanel;
    Max := 100;  // making sure this is the default
    Smooth := true;
    Height := 20;
    Width := 100;
    Visible := True;
{$ifdef has_StyleElements}
    StyleElements := Document.StyleElements; // TODO: does this work? Cannot see in Delphi 6
{$endif}
  end;

  if not PercentWidth and (SpecWidth > 0) then
    Progress.Width := SpecWidth;

  if not PercentHeight and (SpecHeight > 0) then
    Progress.Height := SpecHeight;

  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        MaxSy:
          Progress.Max := StrToIntDef( string(Name), 100 );

        ValueSy:
        begin
          Progress.Position := StrToIntDef( string(Name), 0 );
          PositionSet := true;
        end;
      end;

  SetWidth := Progress.Width;
  SetHeight := Progress.Height;

  // if the value attribute is not set, then the progressbar goes to the indeterminate state 'pbstMarquee'.
  if PositionSet then
    Progress.Style := pbstNormal
  else
    Progress.Style := pbstMarquee;
end;

constructor TProgressObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TProgressObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  Progress := ThvProgressBar.Create(nil);
  with T.Progress do
    Progress.SetBounds(Left, Top, Width, Height);
  Progress.Visible := T.Progress.Visible;
  Progress.Parent := Document.PPanel;
  Progress.Max := T.Progress.Max;
  Progress.Position := T.Progress.Position;
  SetHeight := T.SetHeight;
  SetWidth := T.SetWidth;
end;

destructor TProgressObj.Destroy;
begin
  Progress.Free;
  inherited Destroy;
end;

procedure TProgressObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
begin
  inherited DrawInline(Canvas,X,Y,YBaseline,FO);

  if Progress.Visible then
    Progress.Show
  else
    Progress.Hide;
end;

function TProgressObj.GetControl: TWinControl;
begin
  Result := Progress;
end;


{----------------TMeterObj.Create}

constructor TMeterObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties; ObjectTag: Boolean);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
  //PositionSet: Boolean;
begin
  inherited Create(Parent, Position, L, Prop);
  //PositionSet := false;
  VertAlign := ABottom; {default}
  FFloating := ANone;
  PntPanel := Document.PPanel;
  Meter := ThvMeter.Create(PntPanel);
  with Meter do
  begin
    Left := -4000;
    Top := -4000;
    Parent := PntPanel;
    Max := 100;  // making sure this is the default
    Smooth := true;
    Height := 20;
    Width := 100;
    Visible := True;
{$ifdef has_StyleElements}
    StyleElements := Document.StyleElements; // TODO: does this work? Cannot see in Delphi 6
{$endif}
  end;

  if not PercentWidth and (SpecWidth > 0) then
    Meter.Width := SpecWidth;

  if not PercentHeight and (SpecHeight > 0) then
    Meter.Height := SpecHeight;

  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        HighSy:
          Meter.High := StrToIntDef( string(Name), 100 );

        LowSy:
          Meter.Low := StrToIntDef( string(Name), 0 );

        MaxSy:
          Meter.Max := StrToIntDef( string(Name), 100 );

        MinSy:
          Meter.Min := StrToIntDef( string(Name), 0 );

        OptimumSy:
          Meter.Optimum := StrToIntDef( string(Name), 50 );

        ValueSy:
        begin
          Meter.Position := StrToIntDef( string(Name), 0 );
          //PositionSet := true;
        end;
      end;

  SetWidth := Meter.Width;
  SetHeight := Meter.Height;


  Meter.Style := pbstNormal;
end;

constructor TMeterObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TMeterObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  Meter := ThvMeter.Create(nil);
  with T.Meter do
    Meter.SetBounds(Left, Top, Width, Height);
  Meter.Visible := T.Meter.Visible;
  Meter.Parent := Document.PPanel;
  Meter.Max := T.Meter.Max;
  Meter.Position := T.Meter.Position;
  SetHeight := T.SetHeight;
  SetWidth := T.SetWidth;
end;

destructor TMeterObj.Destroy;
begin
  Meter.Free;
  inherited Destroy;
end;

procedure TMeterObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
begin
  inherited DrawInline(Canvas,X,Y,YBaseline,FO);

  if Meter.Visible then
    Meter.Show
  else
    Meter.Hide;
end;

function TMeterObj.GetControl: TWinControl;
begin
  Result := Meter;
end;



{----------------TCell.Create}

constructor TCell.Create(Parent: TBlock);
begin
   {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TCell.Create');
   {$ENDIF}
  inherited Create(Parent);
  IMgr := TIndentManager.Create;
   {$IFDEF JPM_DEBUGGING}
  CodeSite.ExitMethod(Self,'TCell.Create');
   {$ENDIF}
end;

{----------------TCell.CreateCopy}

constructor TCell.CreateCopy(Parent: TBlock; T: TCellBasic);
begin
  inherited CreateCopy(Parent, T);
  IMgr := TIndentManager.Create;
end;

destructor TCell.Destroy;
begin
  IMgr.Free;
  inherited Destroy;
end;

{----------------TCell.DoLogic}

function TCell.DoLogic(Canvas: TCanvas; Y: Integer; Width, AHeight, BlHt: Integer;
  var ScrollWidth, Curs: Integer): Integer;
{Do the entire layout of the cell or document. Return the total document pixel height}
var
  IB: Integer;
  LIndex, RIndex: Integer;
  SaveID: TObject;
begin
{$IFDEF JPM_DEBUGGING_LOGIC}
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
  IB := IMgr.ImageBottom - Y; //YValue; {check for image overhang}
  IMgr.CurrentID := SaveID;
  if IB > Result then
    Result := IB;
{$IFDEF JPM_DEBUGGING_LOGIC}
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

constructor TCellObjCell.CreateCopy(Parent: TBlock; T: TCellObjCell);
begin
  inherited CreateCopy(Parent, T);
  MyRect := T.MyRect;
end;

{----------------TCellObjCell.GetUrl}

function TCellObjCell.GetURL(Canvas: TCanvas; X, Y: Integer; out UrlTarg: TUrlTarget;
  out FormControl: TIDObject{TImageFormControlObj}; out ATitle: ThtString): ThtguResultType;
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

  function DoBlockLogic: Integer;
  // Returns CellHeight
  var
    I, Sw, Tmp: Integer;
    SB: TSectionBase;
  begin
    Result := 0;
    for I := 0 to Count - 1 do
    begin
      SB := Items[I];
      Sw := 0;
      Tmp := SB.DrawLogic1(Canvas, X, Y + Result, XRef, YRef, Width, AHeight, BlHt, IMgr, Sw, Curs);
      Inc(Result, Tmp);
      if OwnerBlock.HideOverflow then
        ScrollWidth := Width
      else
        ScrollWidth := Max(ScrollWidth, Sw);
      if SB is TSection then
        TextWidth := Max(TextWidth, TSection(SB).TextWidth);
      if not (SB is TBlock) or (TBlock(SB).Positioning <> posAbsolute) then
        tcContentBot := Max(tcContentBot, SB.ContentBot);
      tcDrawTop := Min(tcDrawTop, SB.DrawTop);
      tcDrawBot := Max(tcDrawBot, SB.DrawBot);
    end;
  end;

//TODO -oBG, 15.03.2014: support display:inline
{$ifdef DO_PD_INLINE}
//  function DoInlineLogic: Integer;
//  // Returns CellHeight
//  var
//    I, Sw, Tmp: Integer;
//    LineSize: TSize;
//    SB: TSectionBase;
//    SC: TSection;
//  begin
//    Result := 0;
//    LineSize.cx := 0;
//    LineSize.cy := 0;
//    for I := 0 to Count - 1 do
//    begin
//      SB := Items[I];
//      if SB is TSection then
//        SC := TSection(SB)
//      else
//        SC := nil;
//      Tmp := SB.DrawLogic1(Canvas, X + LineSize.cx, Y + Result, XRef, YRef, Width, AHeight, BlHt, IMgr, Sw, Curs);
//      if (SC <> nil) {and (SC.WhiteSpaceStyle in [wsPre, wsNoWrap])} then
//      begin
//        // Each section accumulates elements up to complete lines.
//        Inc(Result, Tmp);
//        if OwnerBlock.HideOverflow then
//          ScrollWidth := Width
//        else
//          ScrollWidth := Max(ScrollWidth, Sw);
//        TextWidth := Max(TextWidth, TSection(SB).TextWidth);
//      end
//      else
//      begin
//        if LineSize.cy < Tmp then
//          LineSize.cy := Tmp;
//        Inc(LineSize.cx, SB.DrawRect.Right - SB.DrawRect.Left);
//        if LineSize.cx > Width then
//        begin
//          Inc(Result, LineSize.cy);
//          if OwnerBlock.HideOverflow then
//            ScrollWidth := Width
//          else
//            ScrollWidth := Max(ScrollWidth, LineSize.cx);
//          if TextWidth < LineSize.cx then
//            TextWidth := LineSize.cx;
//          LineSize.cx := 0;
//          LineSize.cy := 0;
//        end;
//      end;
//      if not (SB is TBlock) or (TBlock(SB).Positioning <> posAbsolute) then
//        tcContentBot := Max(tcContentBot, SB.ContentBot);
//      tcDrawTop := Min(tcDrawTop, SB.DrawTop);
//      tcDrawBot := Max(tcDrawBot, SB.DrawBot);
//    end;
//    Inc(Result, LineSize.cy);
//  end;
{$endif}

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
//  YValue := Y;
  StartCurs := Curs;

  ScrollWidth := 0;
  TextWidth := 0;
  tcContentBot := 0;
  tcDrawTop := 990000;
  tcDrawBot := 0;

//TODO -oBG, 15.03.2014: support display:inline
{$ifdef DO_PD_INLINE}
  if CalcDisplayExtern = pdInline then
    CellHeight := DoInlineLogic
  else
{$endif}
    CellHeight := DoBlockLogic;

  Len := Curs - StartCurs;
  Result := CellHeight;
  {$IFDEF JPM_DEBUGGING}
  CodeSite.SendFmtMsg('Curs = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TBlockCell.DoLogicX');
  {$ENDIF}
end;

{ TDrawList }

procedure TDrawList.AddImage(Obj: TImageObj; Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
var
  Result: TImageRec;
begin
  Result := TImageRec.Create;
  Result.AObj := Obj;
  Result.ACanvas := Canvas;
  Result.AX := X;
  Result.AY := Y;
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
        AObj.DrawInline(ACanvas, AX, AY, AYBaseline, AFO);
    Inc(I);
  end;
end;

{----------------TFormRadioButton.GetChecked:}

//function TFormRadioButton.GetChecked: Boolean;
//begin
//  Result := FChecked;
//end;

//procedure TFormRadioButton.CreateWnd;
//begin
//  inherited CreateWnd;
//  inherited SetChecked(FInitiallyChecked);
//end;

//procedure TFormRadioButton.SetChecked(Value: Boolean);
//var
//  RadioGroup: TWinControl; // radio group panel
//begin
//  if GetKeyState(VK_TAB) < 0 then {ignore if tab key down}
//    Exit;
//  if FChecked <> Value then
//  begin
//    FChecked := Value;
//    TabStop := Value;
//    inherited SetChecked(FChecked);
//  end;
//end;

procedure TFormRadioButton.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {else don't get the arrow keys}
end;

{----------------ThtTabcontrol.Destroy}

destructor ThtTabControl.Destroy;
var
  ParentForm: TCustomForm;
begin
  // if this control was focused, return focus to the parent
  if Focused then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) then
      ParentForm.ActiveControl := Self.Parent;
  end;
  inherited Destroy;
end;

procedure ThtTabcontrol.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {this to eat the arrow keys}
end;

{----------------ThtLineRec.Create}

constructor ThtLineRec.Create(SL: ThtDocument);
begin
  inherited Create;
  if SL.InlineList.Count > 0 then
    FirstDraw := True;
end;

procedure ThtLineRec.Clear;
begin
  FreeAndNil(BorderList);
end;

destructor ThtLineRec.Destroy;
begin
  BorderList.Free;
  inherited Destroy;
end;

{----------------ThtBorderRec.DrawTheBorder}

procedure ThtBorderRec.DrawTheBorder(Canvas: TCanvas; XOffset, YOffSet: Integer; Printing: Boolean; ThemedColorToRGB: ThtThemedColor);
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
    Canvas.Brush.Color := ThemedColorToRGB(MargArray[BackgroundColor], htseClient) or PalRelative;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(IRect);
  end;

  ORect.Left := IRect.Left - MargArray[BorderLeftWidth];
  ORect.Top := IRect.Top - MargArray[BorderTopWidth];
  ORect.Right := IRect.Right + MargArray[BorderRightWidth];
  ORect.Bottom := IRect.Bottom + MargArray[BorderBottomWidth];

  DrawBorder(Canvas, ORect, IRect,
    htColors(MargArray[BorderLeftColor], MargArray[BorderTopColor], MargArray[BorderRightColor], MargArray[BorderBottomColor]),
    htStyles(ThtBorderStyle(MargArray[BorderLeftStyle]), ThtBorderStyle(MargArray[BorderTopStyle]), ThtBorderStyle(MargArray[BorderRightStyle]), ThtBorderStyle(MargArray[BorderBottomStyle])),
    MargArray[BackgroundColor], Printing, ThemedColorToRGB)
end;

{----------------TPage.Draw1}

constructor TPage.Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties);
begin
  inherited Create(Parent, Attributes, Properties);
  if FDisplay = pdUnassigned then
    FDisplay := pdBlock;
end;

function TPage.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
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

constructor THorzLine.Create(Parent: TCellBasic; L: TAttributeList; Prop: TProperties);
var
  LwName: ThtString;
  I: Integer;
  TmpColor: TColor;
begin
  inherited Create(Parent, L, Prop);
  if FDisplay = pdUnassigned then
    FDisplay := pdBlock;
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
  UseDefBorder := not Prop.BorderStyleNotBlank;
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

constructor THorzLine.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: THorzLine absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  System.Move(T.VSize, VSize, PtrSub(@BkGnd, @VSize) + Sizeof(BkGnd));
end;

procedure THorzLine.CopySelectedText;
begin
  Document.CB.AddTextCR('', 0);
end;

function THorzLine.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth: Integer; var Curs: Integer): Integer;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'THorzLine.DrawLogic1');
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
  Indent := Max(0, IMgr.LeftIndent(Y) - X);
  Width := Min(AWidth, IMgr.RightSide(Y) - X) - Indent;
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
  CodeSite.ExitMethod(Self,'THorzLine.DrawLogic1');
   {$ENDIF}
end;

{----------------THorzLine.Draw}

function THorzLine.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
var
  XR: Integer;
  YT, YO, Y: Integer;
  White, BlackBorder: Boolean;
begin
  Y := YDraw;
  Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
  YO := Y - Document.YOff;
  if (YO + SectionHeight >= ARect.Top) and (YO < ARect.Bottom) and (not Document.Printing or (Y < Document.PageBottom)) then
  begin
    Inc(X, Indent);
    YT := YO;
    XR := X + Width - 1;
    with Canvas do
    begin
      if Color <> clNone then
      begin
        Brush.Color := Document.ThemedColorToRGB(Color, htseClient) or $2000000;
        Brush.Style := bsSolid;
        FillRect(Rect(X, YT, XR + 1, YT + VSize));
      end
      else
      begin
        if UseDefBorder then begin
          with Document do
          begin
            White := Printing or (Document.ThemedColorToRGB(Background, htseFont) = clWhite);
            BlackBorder := NoShade or (Printing and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and (GetDeviceCaps(Handle, PLANES) = 1));
          end;
          if BlackBorder then
            Pen.Color := clBlack
          else if White then
            Pen.Color := clSilver
          else
            Pen.Color := Document.ThemedColorToRGB(clBtnHighLight, htseClient);
          MoveTo(XR, YT);
          LineTo(XR, YT + VSize - 1);
          LineTo(X, YT + VSize - 1);
          if BlackBorder then
            Pen.Color := clBlack
          else
            Pen.Color := Document.ThemedColorToRGB(clBtnShadow, htseFont);
          LineTo(X, YT);
          LineTo(XR, YT);
        end;
      end;
      Document.FirstPageItem := False; {items after this will not be first on page}
    end;
  end;
end;

{ THtmlPropStack }

{ Add a TProperties to the PropStack. }
procedure THtmlPropStack.PushNewProp(Sym: TElemSymb; Properties: TProperties; Attributes: TAttributeList; const APseudo: ThtString = '');
var
  NewProp: TProperties;
  Tag: ThtString;
begin
  Tag := SymbToStr(Sym);
  NewProp := TProperties.Create(Self, Document.UseQuirksMode, Document.PixelsPerInch);
  NewProp.PropSym := Sym;
  NewProp.Inherit(Tag, Last);
  Add(NewProp);
  NewProp.Combine(Document.Styles, Sym, Tag, APseudo, Properties, Attributes, Count - 1);
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

procedure THtmlPropStack.PopAProp(Sym: TElemSymb);
{pop and free a TProperties from the Prop stack.  It should be on top but in
 case of a nesting error, find it anyway}
var
  I, J: Integer;
begin
  for I := Count - 1 downto 1 do
    if Items[I].PropSym = Sym then
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
  Self.FUseQuirksMode := Document.UseQuirksMode;
end;

//-- BG ---------------------------------------------------------- 08.03.2011 --
procedure THtmlStyleList.SetLinksActive(Value: Boolean);
begin
//  inherited SetLinksActive(Value);
  Document.LinksActive := Value;
end;

{ TFieldsetBlock }

//-- BG ---------------------------------------------------------- 09.10.2010 --
procedure TFieldsetBlock.ContentMinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
var
  LegendMin, LegendMax: Integer;
  ContentMin, ContentMax: Integer;
begin
  Legend.MinMaxWidth(Canvas, LegendMin, LegendMax, AvailableWidth, AvailableHeight);
  inherited ContentMinMaxWidth(Canvas, ContentMin, ContentMax, AvailableWidth, AvailableHeight);
  Min := Math.Max(ContentMin, LegendMin);
  Max := Math.Max(ContentMax, LegendMax);
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
procedure TFieldsetBlock.ConvMargArray(BaseWidth, BaseHeight: Integer; out AutoCount: Integer);
var
  PaddTop, Delta: Integer;
begin
  inherited ConvMargArray(BaseWidth, BaseHeight,AutoCount);
  MargArray[MarginTop] := VMargToMarg(MargArrayO[MarginTop], False, BaseHeight, EmSize, ExSize, 10, Document.PixelsPerInch);
  Delta := Legend.CellHeight - (MargArray[MarginTop] + MargArray[BorderTopWidth] + MargArray[PaddingTop]);
  if Delta > 0 then
  begin
    PaddTop := Delta div 2;
    MargArray[MarginTop] := MargArray[MarginTop] + Delta - PaddTop;
    MargArray[PaddingTop] := MargArray[PaddingTop] + PaddTop;
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
constructor TFieldsetBlock.Create(Parent: TCellBasic; Attributes: TAttributeList; Prop: TProperties);
var
  Index: ThtPropIndices;
begin
  inherited Create(Parent,Attributes,Prop);
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
  FLegend := TBlockCell.Create(Self);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
constructor TFieldsetBlock.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TFieldsetBlock absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FLegend := TBlockCell.CreateCopy(Self, T.FLegend);
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
destructor TFieldsetBlock.Destroy;
begin
  FLegend.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 06.10.2010 --
function TFieldsetBlock.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
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
    Legend.Draw(Canvas, ARect, ContentWidth, Rect.Left + 2, YDraw, XRef, YRef);
    Rect := CalcClipRect(Canvas, Rect, Document.Printing);
    ExcludeClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
  end;
end;

//-- BG ---------------------------------------------------------- 05.10.2010 --
function TFieldsetBlock.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer;
  IMgr: TIndentManager; var MaxWidth, Curs: Integer): Integer;
var
  BorderWidth: TRect;
  AutoCount, BlockHeight, ScrollWidth, L, LI, RI: Integer;
  SaveID: TObject;
begin
  {$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TBlock.DrawLogic1');
  CodeSite.SendFmtMsg('Self.TagClass = [%s]', [Self.TagClass] );
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
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
    StyleUn.ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, self.BorderWidth, AutoCount, MargArray, Document.PixelsPerInch);
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
    ContentWidth := AWidth - BorderWidth.Left - BorderWidth.Right;

    SaveID := IMgr.CurrentID;
    IMgr.CurrentID := Self;
    Legend.IMgr := IMgr;
    LI := IMgr.SetLeftIndent(L, Y);
    RI := IMgr.SetRightIndent(L + ContentWidth, Y);
    Legend.DoLogicX(Canvas, X + BorderWidth.Left, Y, XRef, YRef, ContentWidth, MargArray[piHeight], BlockHeight, ScrollWidth, Curs);
    IMgr.FreeLeftIndentRec(LI);
    IMgr.FreeRightIndentRec(RI);
    IMgr.CurrentID := SaveID;

    Result := inherited DrawLogic1(Canvas, X, Y, XRef, YRef, AWidth, AHeight, BlHt, IMgr, MaxWidth, Curs);
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
  CodeSite.ExitMethod(Self,'TBlock.DrawLogic1');
   {$ENDIF}
end;


procedure TFormControlObj.SetValue(const Value: ThtString);
begin
  FValue := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.SetClientWidth(Value: Integer);
begin
  TheControl.Width := Value;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TFormControlObj.Show;
begin
  TheControl.Show;
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
        Show; {makes it tab active}
        Left := -4000; {even if it can't be seen}
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
        Hide; {hides and turns off tabs}
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TFormControlObjList.GetItem(Index: Integer): TFormControlObj;
begin
{$ifdef UseGenerics}
  Result := inherited Items[Index] as TFormControlObj;
{$else}
  Result := Get(Index);
{$endif}
end;

{ TSizeableObj }

//-- BG ---------------------------------------------------------- 12.11.2011 --
procedure TSizeableObj.CalcSize(AvailableWidth, AvailableHeight, SetWidth, SetHeight: Integer; IsClientSizeSpecified: Boolean);
// Extracted from TPanelObj.DrawLogic() and TImageObj.DrawLogic()

  function CroppedSize(S: Integer; piMin, piMax: ThtPropertyIndex): Integer;
  begin
    Result := S;

    if MargArray[piMax] > 0 then
      Result := Min(Result, MargArray[piMax]);

    if MargArray[piMin] > 0 then
      Result := Max(Result, MargArray[piMin]);
  end;

var
  MargArrayO: ThtVMarginArray;
  H, W: Integer;
begin
  if FProperties <> nil then
    FProperties.GetVMarginArray(MargArrayO)
  else
  begin
    MargArrayO[piMinWidth] := IntNull;
    MargArrayO[piMaxWidth] := IntNull;
    MargArrayO[piMinHeight] := IntNull;
    MargArrayO[piMaxHeight] := IntNull;
  end;

  ConvInlineMargArray(MargArrayO, AvailableWidth, AvailableHeight, EmSize, ExSize, BorderWidth, MargArray, Document.PixelsPerInch);

  if PercentWidth then
  begin
    W := CroppedSize(MulDiv(AvailableWidth, SpecWidth, 100), piMinWidth, piMaxWidth);
    if SpecHeight > 0 then
      if PercentHeight then
        H := MulDiv(AvailableHeight, SpecHeight, 100)
      else
        H := SpecHeight
    else
      H := MulDiv(W, SetHeight, SetWidth);
    H := CroppedSize(H, piMinHeight, piMaxHeight);
  end
  else if PercentHeight then
  begin
    H := CroppedSize(MulDiv(AvailableHeight, SpecHeight, 100), piMinHeight, piMaxHeight);
    if SpecWidth > 0 then
      W := SpecWidth
    else
      W := MulDiv(H, SetWidth, SetHeight);
    W := CroppedSize(W, piMinWidth, piMaxWidth);
  end
  else if (SpecWidth > 0) and (SpecHeight > 0) then
  begin {Both width and height specified}
    H := SpecHeight;
    W := SpecWidth;
    H := CroppedSize(H, piMinHeight, piMaxHeight);
    W := CroppedSize(W, piMinWidth, piMaxWidth);
    ClientSizeKnown := True;
  end
  else if SpecHeight > 0 then
  begin
    H := SpecHeight;
    H := CroppedSize(H, piMinHeight, piMaxHeight);
    W := MulDiv(H, SetWidth, SetHeight);
    W := CroppedSize(W, piMinWidth, piMaxWidth);
    ClientSizeKnown := IsClientSizeSpecified;
  end
  else if SpecWidth > 0 then
  begin
    W := SpecWidth;
    W := CroppedSize(W, piMinWidth, piMaxWidth);
    H := MulDiv(W, SetHeight, SetWidth);
    H := CroppedSize(H, piMinHeight, piMaxHeight);
    ClientSizeKnown := IsClientSizeSpecified;
  end
  else
  begin {neither height and width specified}
    H := SetHeight;
    W := SetWidth;
    CalcAutoMinMaxConstraints(W, H, MargArray[piMinWidth], MargArray[piMaxWidth], MargArray[piMinHeight], MargArray[piMaxHeight], W, H);
    ClientSizeKnown := IsClientSizeSpecified;
  end;

  ClientWidth := W;
  ClientHeight := H;
  if ClientSizeKnown then
    if (Pos('%', MargArrayO[piMinWidth]) > 0) or (Pos('%', MargArrayO[piMaxWidth]) > 0) or (Pos('%', MargArrayO[piMinHeight]) > 0) or (Pos('%', MargArrayO[piMaxHeight]) > 0) then
      ClientSizeKnown := False;
end;

//-- BG ---------------------------------------------------------- 12.11.2011 --
constructor TSizeableObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  I: Integer;
  NewHSpace: Integer;
  NewVSpace: Integer;
  S: ThtString;
begin
  inherited Create(Parent,Position,L,Prop);
  NewHSpace := -1;
  NewVSpace := -1;
  SpecHeight := -1;
  SpecWidth := -1;

  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of

        HeightSy:
        begin
          if System.Pos('%', Name) = 0 then
          begin
              SpecHeight := Value;
          end
          else if (Value >= 0) and (Value <= 100) then
          begin
            SpecHeight := Value;
            PercentHeight := True;
          end;
        end;

        WidthSy:
          if System.Pos('%', Name) = 0 then
          begin
            SpecWidth := Value;
          end
          else if (Value >= 0) and (Value <= 100) then
          begin
            SpecWidth := Value;
            PercentWidth := True;
          end;

        HSpaceSy:
          NewHSpace := Min(40, Abs(Value));

        VSpaceSy:
          NewVSpace := Min(40, Abs(Value));

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
              FFloating := ALeft;
            end
            else if S = 'RIGHT' then
            begin
              VertAlign := ANone;
              FFloating := ARight;
            end;
          end;
      end;

  if NewVSpace >= 0 then
  begin
    VSpaceT := NewVSpace;
    VSpaceB := VSpaceT;
  end;

  if NewHSpace >= 0 then
  begin
    HSpaceL := NewHSpace;
    HSpaceR := HSpaceL;
  end
  else
  begin
    HSpaceR := HSpaceL;
    if Self.Document.UseQuirksMode then
    begin
       case Floating of
         ALeft :
           begin
             HSpaceL := 0;
             HSpaceR := ImageSpace;
           end;
         ARight :
           begin
             HSpaceL := ImageSpace;
             HSpaceR := 0;
           end;
       end;
    end;
  end;

end;

constructor TSizeableObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TSizeableObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  BorderWidth := T.BorderWidth;
  FAlt := T.FAlt;
  FClientHeight := T.FClientHeight;
  FClientWidth := T.FClientWidth;
  ClientSizeKnown := T.ClientSizeKnown;
  NoBorder := T.NoBorder;
  SpecHeight := T.SpecHeight;
  SpecWidth := T.SpecWidth;
  Title := T.Title;
end;

//-- BG ---------------------------------------------------------- 30.08.2013 --
procedure TSizeableObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
begin
  if not IsCopy then
  begin
    DrawXX := X;
    DrawYY := Y;
  end;
end;

//-- BG ---------------------------------------------------------- 21.09.2016 --
procedure TSizeableObj.DrawLogicInline(Canvas: TCanvas; FO: TFontObj;
  AvailableWidth, AvailableHeight: Integer);
begin

end;

//-- BG ---------------------------------------------------------- 06.08.2013 --
function TSizeableObj.GetClientHeight: Integer;
begin
  Result := FClientHeight;
end;

//-- BG ---------------------------------------------------------- 06.08.2013 --
function TSizeableObj.GetClientWidth: Integer;
begin
  Result := FClientWidth;
end;

procedure TSizeableObj.ProcessProperties(Prop: TProperties);
const
  DummyHtWd = 200;
var
  MargArrayO: ThtVMarginArray;
  Align: ThtAlignmentStyle;
begin
  if Prop.GetVertAlign(Align) then
    VertAlign := Align;
  if Prop.GetFloat(Align) and (Align <> ANone) then
  begin
//    if HSpaceR = 0 then
//    begin {default is different for Align = left/right}
//      HSpaceR := ImageSpace;
//      HSpaceL := ImageSpace;
//    end;
    VertAlign := ANone;
    FFloating := Align;
  end;
  if Title = '' then {a Title attribute will have higher priority than inherited}
    Title := Prop.PropTitle;
  Prop.GetVMarginArray(MargArrayO);
  EmSize := Prop.EmSize;
  ExSize := Prop.ExSize;
  ConvInlineMargArray(MargArrayO, DummyHtWd, DummyHtWd, EmSize, ExSize, BorderWidth, MargArray, Document.PixelsPerInch);

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
    if MargArray[piWidth] <> Auto then
      if (VarIsStr(MargArrayO[piWidth]))
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
    if MargArray[piHeight] <> Auto then
      if (VarIsStr(MargArrayO[piHeight]))
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
    FFloating := Align;

  if Prop.BorderStyleNotBlank then
  begin
    NoBorder := True; {will have inline border instead}
    BorderWidth := 0;
  end
  else if Prop.HasBorderStyle then
  begin
    Inc(HSpaceL, MargArray[BorderLeftWidth]);
    Inc(HSpaceR, MargArray[BorderRightWidth]);
    Inc(VSpaceT, MargArray[BorderTopWidth]);
    Inc(VSpaceB, MargArray[BorderBottomWidth]);
  end;
end;

//-- BG ---------------------------------------------------------- 14.10.2012 --
function TSizeableObj.PtInDrawRect(X, Y: Integer; var IX, IY: Integer): Boolean;
var
  XO, YO, W, H: Integer;
begin
  if DrawRect.Left <> 999999999 then
    Result := inherited PtInDrawRect(X, Y, IX, IY)
  else
  begin
    // BG, 31.08.2013: deprecated. Method in base class does the same, doesn't it?
    XO := X - DrawXX; {these are actual image, box if any is outside}
    YO := Y - DrawYY;
    W := ClientWidth - 2 * BorderWidth;
    H := ClientHeight - 2 * BorderWidth;
    Result := (XO >= 0) and (XO < W) and (YO >= 0) and (YO < H);
    if Result then
    begin
      IX := XO;
      IY := YO;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 30.11.2010 --
procedure TSizeableObj.SetAlt(CodePage: Integer; const Value: ThtString);
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

//-- BG ---------------------------------------------------------- 31.08.2013 --
procedure TSizeableObj.SetClientHeight(Value: Integer);
begin
  FClientHeight := Value;
end;

//-- BG ---------------------------------------------------------- 31.08.2013 --
procedure TSizeableObj.SetClientWidth(Value: Integer);
begin
  FClientWidth := Value;
end;

//-- BG ---------------------------------------------------------- 12.11.2011 --
constructor TSizeableObj.SimpleCreate(Parent: TCellBasic);
begin
  inherited Create(Parent, 0, nil, nil);
  VertAlign := ABottom; {default}
  NoBorder := True;
  BorderWidth := 0;
  SpecHeight := -1;
  SpecWidth := -1;
end;

{ TSectionBase }

//-- BG ---------------------------------------------------------- 20.09.2009 --
constructor TSectionBase.Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties);
var
  TheId, TheClass: ThtString;
begin
  if Properties <> nil then
  begin
    TheID := Properties.PropID;
    TheClass := Properties.PropClass;
  end
  else if Attributes <> nil then
  begin
    TheId := Attributes.TheId;
    TheClass := Attributes.TheClass;
  end;
  Create(Parent, Attributes, Properties, TheId, TheClass);
end;

//-- BG ---------------------------------------------------------- 20.09.2009 --
constructor TSectionBase.Create(Parent: TCellBasic; Attributes: TAttributeList; Properties: TProperties; const TheId, TheClass: ThtString);
begin
  inherited Create(Parent, Attributes, Properties, TheId);
  if Properties <> nil then
  begin
    TagClass := Properties.PropTag;
  end;
  htAppendStr(TagClass, '.' + TheClass + '#' + TheId);

  ContentTop := 999999999; {large number in case it has Display: none; }

  DrawRect.Top    := 999999999;
  DrawRect.Left   := 999999999;
  DrawRect.Bottom := 0;
  DrawRect.Right  := 0;
end;

constructor TSectionBase.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TSectionBase absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  StartCurs := T.StartCurs;
  Len := T.Len;
  SectionHeight := T.SectionHeight;
  ZIndex := T.ZIndex;
end;

procedure TSectionBase.CopySelectedText;
begin
end;

function TSectionBase.GetYPosition: Integer;
begin
  Result := ContentTop;
end;

function TSectionBase.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
// Computes all coordinates of the section.
//
// Normal sections, absolutely positioned blocks and floating blocks start at given (X,Y) relative to document origin.
// Table cells start at given (X,Y) coordinates relative to the outmost containing block.
//
// Returns the nominal height of the section (without overhanging floating blocks)
var
  ClearAddon: Integer;
begin
{$IFDEF JPM_DEBUGGING}
  CodeSite.EnterMethod(Self,'TSectionBase.DrawLogic1');
  CodeSite.SendFmtMsg('X, XRef  = [%4d, %4d]', [X, XRef]);
  CodeSite.SendFmtMsg('Y, YRef  = [%4d, %4d]', [Y, YRef]);
  CodeSite.SendFmtMsg('AWidth   = [%d]',[AWidth]);
  CodeSite.SendFmtMsg('AHeight  = [%d]',[AHeight]);
  CodeSite.SendFmtMsg('BlHt     = [%d]',[BlHt]);
  if Assigned(IMgr) then
  begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end
  else
  begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.AddSeparator;
{$ENDIF}

  case FDisplay of
    pdNone:
      ClearAddon := 0;
  else
    ClearAddon := GetClearSpace(IMgr, Y);
  end;

  StartCurs := Curs;
  Len := 0;
  MaxWidth := 0;
  SectionHeight := 0;
  DrawHeight := SectionHeight;
  YDraw := Y + ClearAddon;

  ContentTop := YDraw;
  ContentBot := ContentTop + SectionHeight;

  DrawTop := YDraw;
  DrawBot := DrawTop + DrawHeight;

  Result := ContentBot - Y;

  //>-- DZ
  DrawRect.Top    := DrawTop;
  DrawRect.Left   := X;
  DrawRect.Right  := DrawRect.Left + MaxWidth;
  DrawRect.Bottom := DrawBot;
{$IFDEF JPM_DEBUGGING}
  if Assigned(IMgr) then
  begin
    CodeSite.SendFmtMsg('IMgr.LfEdge    = [%d]',[ IMgr.LfEdge ] );
    CodeSite.SendFmtMsg('IMgr.Width     = [%d]',[ IMgr.Width ] );
    CodeSite.SendFmtMsg('IMgr.ClipWidth = [%d]',[ IMgr.ClipWidth ] );
  end
  else
  begin
    CodeSite.SendMsg('IMgr      = nil');
  end;
  CodeSite.SendFmtMsg('MaxWidth = [%d]',[MaxWidth]);
  CodeSite.SendFmtMsg('Curs     = [%d]',[Curs]);
  CodeSite.SendFmtMsg('Result   = [%d]',[Result]);
  CodeSite.ExitMethod(Self,'TSectionBase.DrawLogic1');
{$ENDIF}
end;

function TSectionBase.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
// returns the pixel row, where the section ends.
begin
  Result := YDraw + SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X, Y: Integer;
  out UrlTarg: TUrlTarget; out FormControl: TIDObject{TImageFormControlObj};
  out ATitle: ThtString): ThtguResultType;
begin
  Result := [];
  UrlTarg := nil;
  FormControl := nil;
end;

//-- BG ---------------------------------------------------------- 14.10.2012 --
function TSectionBase.PtInDrawRect(X, Y: Integer; var IX, IY: Integer): Boolean;
// inspired by >-- DZ 19.09.2012
begin
  case Display of
    pdNone:
      Result := False;
  else
    // BG, 14.10.2012: why isn't there a Document.XOff representing the horizontal scroll position?
    // Dec(X, Document.XOff);
    //Dec(Y, Document.YOff);
    Result := (X >= DrawRect.Left) and (X < DrawRect.Right) and (Y >= DrawRect.Top) and (Y < DrawRect.Bottom);
    if Result then
    begin
      IX := X - DrawRect.Left;
      IY := Y - DrawRect.Top;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 14.10.2012 --
function TSectionBase.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;
begin
  Result := PtInDrawRect(X, Y, IX, IY);
  if Result then
    Obj := Self;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X, Y: Integer; out XR, YR, CaretHt: Integer; out Intext: Boolean): Integer;
begin
  Result := -1;
  InText := False;
end;

function TSectionBase.FindString(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindStringR(From: Integer; const ToFind: UnicodeString; MatchCase: Boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindSourcePos(DocPos: Integer): Integer;
begin
  Result := -1;
end;

function TSectionBase.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
begin
  Result := -1;
end;

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TSectionBase.GetChAtPos(Pos: Integer; out Ch: WideChar; out Obj: TSectionBase): Boolean;
begin
  Result := False;
  Ch := #0;
  Obj := nil;
end;

procedure TSectionBase.SetDocument(List: ThtDocument);
begin
  FDocument := List;
end;

procedure TSectionBase.MinMaxWidth(Canvas: TCanvas; out Min, Max: Integer; AvailableWidth, AvailableHeight: Integer);
begin
  Min := 0;
  Max := 0;
end;

procedure TSectionBase.AddSectionsToList;
begin
  Document.addSectionsToPositionList(Self);
end;

{ TSectionBaseList }

function TSectionBaseList.CursorToXY(Canvas: TCanvas; Cursor: Integer; var X, Y: Integer): Boolean;
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

function TSectionBaseList.FindDocPos(SourcePos: Integer; Prev: Boolean): Integer;
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

{$ifdef UseGenerics}
{$else}
function TSectionBaseList.GetItem(Index: Integer): TSectionBase;
begin
  Result := inherited Get(Index);
end;
{$endif}

function TSectionBaseList.PtInObject(X, Y: Integer; var Obj: TObject; var IX, IY: Integer): Boolean;
{Y is absolute}
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].PtInObject(X, Y, Obj, IX, IY) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

{ TChPosObj }

//-- BG ---------------------------------------------------------- 04.03.2011 --
constructor TChPosObj.Create(Document: ThtDocument; Pos: Integer);
begin
  inherited Create('');
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

{ TControlObj }

//-- BG ---------------------------------------------------------- 12.11.2011 --
procedure TControlObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
var
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
  OldPenColor: TColor;
  SaveFont: TFont;
begin
  inherited DrawInline(Canvas,X,Y,YBaseline,FO);
  if IsCopy then
  begin
    OldBrushStyle := Canvas.Brush.Style; {save style first}
    OldBrushColor := Canvas.Brush.Color;
    OldPenColor := Canvas.Pen.Color;
    Canvas.Pen.Color := Document.ThemedColorToRGB(FO.TheFont.Color, htseFont);
    Canvas.Brush.Color := Document.ThemedColorToRGB(BackgroundColor, htseClient);
    Canvas.Brush.Style := bsSolid;
    try
      // paint a rectangular placeholder
      Canvas.Rectangle(X, Y, X + ClientWidth, Y + ClientHeight);
      if FAlt <> '' then
      begin
        // show the alternative text.
        SaveFont := TFont.Create;
        try
          SaveFont.Assign(Canvas.Font);
          Canvas.Font.Size := 8;
          Canvas.Font.Name := FontSans;
          WrapTextW(Canvas, X + 5, Y + 5, X + ClientWidth - 5, Y + ClientHeight - 5, FAlt);
        finally
          Canvas.Font := SaveFont;
          SaveFont.Free;
        end;
      end;
    finally
      Canvas.Brush.Color := OldBrushColor;
      Canvas.Brush.Style := OldBrushStyle; {style after color as color changes style}
      Canvas.Pen.Color := OldPenColor;
    end;
  end
  else
  begin
    ClientControl.Left := X;
    ClientControl.Top := Y;
  end;
end;

procedure TControlObj.DrawLogicInline(Canvas: TCanvas; FO: TFontObj; AvailableWidth, AvailableHeight: Integer);
var
  Control: TControl;
begin
  if not ClientSizeKnown or PercentWidth or PercentHeight then
  begin
    CalcSize(AvailableWidth, AvailableHeight, SetWidth, SetHeight, True);
    if not IsCopy then
      if (ClientWidth > 0) and (ClientHeight > 0) then
      begin
        Control := ClientControl;
        if Control <> nil then
          Control.SetBounds(Control.Left, Control.Top, ClientWidth, ClientHeight);
      end;
  end;
end;

//-- BG ---------------------------------------------------------- 15.12.2011 --
function TControlObj.GetBackgroundColor: TColor;
begin
  Result := clNone;
end;

{ TFrameObj }

//-- BG ---------------------------------------------------------- 12.11.2011 --
constructor TFrameObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  I: Integer;
begin
  inherited Create(Parent,Position,L,Prop);
  for I := 0 to L.Count - 1 do
    with L[I] do
      case Which of
        SrcSy:
          FSource := htTrim(Name);

        AltSy:
          SetAlt(CodePage, Name);

        FrameBorderSy:
          NoBorder := Value = 0;

        MarginWidthSy:
          frMarginWidth := Value;

        MarginHeightSy:
          frMarginHeight := Value;

        ScrollingSy:
          NoScroll := htCompareText(Name, 'NO') = 0; {auto and yes work the same}

      end;

  CreateFrame;
  try
    UpdateFrame;
  except
  end;
  SetWidth := FViewer.Width;
  SetHeight := FViewer.Height;
end;

//-- BG ---------------------------------------------------------- 12.11.2011 --
constructor TFrameObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TFrameObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FSource := T.FSource;
  frMarginWidth := T.frMarginWidth;
  frMarginHeight := T.frMarginHeight;
  NoScroll := T.NoScroll;
  CreateFrame;
end;

//-- BG ---------------------------------------------------------- 13.11.2011 --
procedure TFrameObj.CreateFrame;
// Create a THtmlViewer, TFrameViewer or TFrameBrowser depending on host component.
var
  PaintPanel: TPaintPanel;
  HtmlViewer: THtmlViewer;
begin
  PaintPanel := TPaintPanel(Document.PPanel);
  HtmlViewer := PaintPanel.ParentViewer;
  FViewer := HtmlViewer.CreateIFrameControl;
  FViewer.Parent := PaintPanel;
  if FSource <> '' then
  begin
    if Assigned(HtmlViewer.OnExpandName) then
      HtmlViewer.OnExpandName(HtmlViewer, FSource, FUrl)
    else
      FUrl := HtmlViewer.HtmlExpandFilename(FSource);
  end;
end;

//-- BG ---------------------------------------------------------- 12.11.2011 --
destructor TFrameObj.Destroy;
begin
  FreeAndNil(FViewer);
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 16.11.2011 --
procedure TFrameObj.DrawInline(Canvas: TCanvas; X, Y, YBaseline: Integer; FO: TFontObj);
var
  HtmlViewer: THtmlViewer;
  Bitmap: TBitmap;
begin
  // BG, 11.12.2011: frames aren't printable.
  if FViewer is THtmlViewer then
  begin
    HtmlViewer := THtmlViewer(FViewer);
    Bitmap := TBitmap.Create;
    try
      Bitmap.HandleType := bmDIB;
      Bitmap.PixelFormat := pf24Bit;
      Bitmap.Width := ClientWidth;
      Bitmap.Height := ClientHeight;
      HtmlViewer.Draw(
        Bitmap.Canvas,
        HtmlViewer.VScrollBar.Position,
        HtmlViewer.Width,
        ClientWidth,
        ClientHeight);
      PrintBitmap(Canvas, X, Y, ClientWidth, ClientHeight, Bitmap);
      //Bitmap.SaveToFile(Format('C:\temp\%x.bmp', [Integer(Self)]));
    finally
      Bitmap.Free;
    end;
  end
  else
    inherited DrawInline(Canvas,X,Y,YBaseline,FO);
end;

//-- BG ---------------------------------------------------------- 15.12.2011 --
function TFrameObj.GetBackgroundColor: TColor;
begin
  if FViewer <> nil then
    Result := FViewer.DefBackground
  else
    Result := inherited GetBackgroundColor;
end;

//-- BG ---------------------------------------------------------- 16.11.2011 --
function TFrameObj.GetControl: TWinControl;
begin
  Result := FViewer;
end;

//-- BG ---------------------------------------------------------- 14.11.2011 --
procedure TFrameObj.UpdateFrame;
var
  LCurrentStyle: TFontStyles; {as set by <b>, <i>, etc.}
  LCurrentForm: THtmlForm;
  LPropStack: THtmlPropStack;
  LNoBreak: Boolean; {set when in <NoBr>}
begin
  if FUrl <> '' then
  begin
    LCurrentForm := Document.CurrentForm;
    LCurrentStyle := Document.CurrentStyle;
    LNoBreak := Document.NoBreak;
    LPropStack := Document.PropStack;
    Document.PropStack := THtmlPropStack.Create;
    try
      FViewer.Load(FUrl);
    finally
      Document.PropStack.Free;
      Document.PropStack := LPropStack;
      Document.CurrentForm := LCurrentForm;
      Document.CurrentStyle := LCurrentStyle;
      Document.NoBreak := LNoBreak;
    end;
  end;
end;

{$ifdef UseGenerics}
{$else}

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

{$endif}

{ TColSpec }

//-- BG ---------------------------------------------------------- 12.01.2012 --
constructor TColSpec.Create(const Width: TSpecWidth; Align: ThtString; VAlign: ThtAlignmentStyle);
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

{ TFloatingObj }

//-- BG ---------------------------------------------------------- 12.11.2011 --
function TFloatingObj.Clone(Parent: TCellBasic): TFloatingObj;
begin
  Result := TFloatingObjClass(ClassType).CreateCopy(Parent, Self);
end;

//-- BG ---------------------------------------------------------- 04.08.2013 --
constructor TFloatingObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
begin
  inherited Create(Parent,Position,L,Prop);
  StartCurs := Position;
  VertAlign := ABottom; {default}
end;

//-- BG ---------------------------------------------------------- 04.08.2013 --
constructor TFloatingObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TFloatingObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  System.Move(T.VertAlign, VertAlign, PtrSub(@PercentHeight, @VertAlign) + SizeOf(PercentHeight));
  DrawXX := T.DrawXX;
  DrawYY := T.DrawYY;
end;

//-- BG ---------------------------------------------------------- 08.09.2013 --
function TFloatingObj.Draw1(Canvas: TCanvas; const ARect: TRect; IMgr: TIndentManager; X, XRef, YRef: Integer): Integer;
begin
  Result := ContentBot;
end;

//-- BG ---------------------------------------------------------- 08.09.2013 --
function TFloatingObj.DrawLogic1(Canvas: TCanvas; X, Y, XRef, YRef, AWidth, AHeight, BlHt: Integer; IMgr: TIndentManager;
  var MaxWidth, Curs: Integer): Integer;
begin
  if Positioning in [posAbsolute, posFixed] then
    Result := 0
  else
    Result := SectionHeight;
end;

//-- BG ---------------------------------------------------------- 14.02.2016 --
function TFloatingObj.GetYPosition: Integer;
begin
  Result := DrawYY;
end;

//-- BG ---------------------------------------------------------- 02.03.2011 --
function TFloatingObj.TotalHeight: Integer;
begin
  Result := VSpaceT + ClientHeight + VSpaceB;
end;

//-- BG ---------------------------------------------------------- 02.03.2011 --
function TFloatingObj.TotalWidth: Integer;
begin
  Result := HSpaceL + ClientWidth + HSpaceR;
end;

{ TFloatingObjList }

//-- BG ---------------------------------------------------------- 05.08.2013 --
constructor TFloatingObjList.CreateCopy(Parent: TCellBasic; T: TFloatingObjList);
var
  I: Integer;
begin
  inherited Create;
  if T <> nil then
    for I := 0 to T.Count - 1 do
      Add(T.Items[I].Clone(Parent));
end;

//-- BG ---------------------------------------------------------- 07.08.2013 --
procedure TFloatingObjList.Decrement(N: Integer);
{called when a character is removed to change the Position figure}
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with Items[I] do
      if StartCurs > N then
        Dec(StartCurs)
      else
        break;
end;

//-- BG ---------------------------------------------------------- 07.08.2013 --
function TFloatingObjList.FindObject(Posn: Integer): TFloatingObj;
{find the object at a given character position}
begin
  if GetObjectAt(Posn, Result) <> 0 then
    Result := nil;
end;

{$ifdef UseGenerics}
{$else}
//-- BG ---------------------------------------------------------- 07.08.2013 --
function TFloatingObjList.GetItem(Index: Integer): TFloatingObj;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 07.08.2013 --
procedure TFloatingObjList.SetItem(Index: Integer; const Item: TFloatingObj);
begin
  Put(Index, Item);
end;
{$endif}

//-- BG ---------------------------------------------------------- 05.08.2013 --
function TFloatingObjList.GetObjectAt(Posn: Integer; out Obj): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].StartCurs - Posn;
    if Result >= 0 then
    begin
      TFloatingObj(Obj) := Items[I];
      Exit;
    end;
  end;

  TFloatingObj(Obj) := nil;
  Result := 99999999;
end;

//-- BG ---------------------------------------------------------- 05.08.2013 --
function TFloatingObjList.PtInImage(X, Y: Integer; out IX, IY, Posn: Integer; out AMap, UMap: Boolean; out MapItem: TMapItem; out ImageObj: TImageObj): Boolean;

  function FindMap(const MapList: ThtMap; const MapName: ThtString): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to MapList.Count - 1 do
    begin
      MapItem := MapList[I];
      if MapItem.MapName = MapName then
      begin
        Result := True;
        break;
      end;
    end;
  end;

var
  I: Integer;
  Obj: TObject;
  ImgObj: TImageObj absolute Obj;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Obj := Items[I];
    if Obj is TImageObj then
    begin
      if ImgObj.PtInDrawRect(X, Y, IX, IY) then
      begin
        Result := True;
        AMap := ImgObj.IsMap;
        Posn := ImgObj.StartCurs;
        UMap := False;
        ImageObj := ImgObj;
        if ImgObj.UseMap then
          UMap := FindMap(ImgObj.Document.MapList, ImgObj.MapName);
        break;
      end;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 05.08.2013 --
function TFloatingObjList.PtInObject(X, Y: Integer; out Obj: TObject; out IX, IY: Integer): Boolean;
var
  I: Integer;
  Item: TFloatingObj;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.PtInDrawRect(X, Y, IX, IY) then
    begin
      Obj := Item;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{ TBlockBase }

//-- BG ---------------------------------------------------------- 31.08.2013 --
constructor TBlockBase.Create(Parent: TCellBasic; Position: Integer; Attributes: TAttributeList; Prop: TProperties);
begin
  inherited Create(Parent, Attributes, Prop);
  if FDisplay = pdUnassigned then
    FDisplay := pdBlock;
  StartCurs := Position;
end;

//-- BG ---------------------------------------------------------- 31.08.2013 --
constructor TBlockBase.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
//var
//  T: TBlockBase absolute Source;
begin
  inherited CreateCopy(Parent,Source);
end;

//{ TRenderSectionBaseList }
//
////-- BG ---------------------------------------------------------- 01.12.2013 --
//procedure TRenderSectionBaseList.Notify(Ptr: Pointer; Action: TListNotification);
//var
//  Obj: TObject absolute Ptr;
//begin
//  inherited;
//  if (Action = lnDeleted) and (Obj is TInlineSection) then
//    Obj.Free;
//end;
//
//{ TInlineSection }
//
////-- BG ---------------------------------------------------------- 01.12.2013 --
//procedure TInlineSection.AddElement(Section: TSection);
//var
//  I: Integer;
//begin
//  // append Buff, Fonts, Images and FormControls
//  BuffS := Buffs + Section.BuffS;
//  Buff := PWideChar(Buffs);
//
//  for I := 0 to Section.Fonts.Count - 1 do
//    Fonts.Add(TFontObj.CreateCopy(Section, Section.Fonts[I]));
//
//  for I := 0 to Section.Images.Count - 1 do
//    Images.Add(Section.Images[I].Clone(Section.Images[I].OwnerCell));
//
//  for I := 0 to Section.FormControls.Count - 1 do
//    FormControls.Add(Section.FormControls[I].Clone(Section.FormControls[I].OwnerCell));
//end;
//
////-- BG ---------------------------------------------------------- 01.12.2013 --
//constructor TInlineSection.Create(Parent: TCellBasic);
//begin
//  inherited Create(Parent);
//end;

//-- BG ---------------------------------------------------------- 24.02.2016 --

{ TLineBreak }

//-- BG ---------------------------------------------------------- 24.02.2016 --
function TLineBreak.TryGetClear(var Clear: ThtClearStyle): Boolean;
var
  T: TAttribute;
begin
  Result := inherited TryGetClear(Clear);
  if not Result then
    if FAttributes <> nil then
    begin
      T := nil;
      if FAttributes.Find(ClearSy, T) then
        Result := TryStrToClearStyle(LowerCase(T.Name), Clear);
    end;
end;

{$ifdef UseGenerics}
{$else}

{ ThtIndexObjList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function ThtIndexObjList.Get(Index: Integer): ThtIndexObj;
begin
  Result := inherited Get(Index);
end;

{ ThtBorderRecList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function ThtBorderRecList.Get(Index: Integer): ThtBorderRec;
begin
  Result := inherited Get(Index);
end;

{ TLineRecList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TLineRecList.Get(Index: Integer): ThtLineRec;
begin
  Result := inherited Get(Index);
end;

{ THtmlFormList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function THtmlFormList.Get(Index: Integer): THtmlForm;
begin
  Result := inherited Get(Index);
end;

{ TFormData }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TFormData.Get(Index: Integer): ThtStringList;
begin
  Result := inherited Get(Index);
end;

{ TPanelObjList }

//-- BG ---------------------------------------------------------- 06.10.2016 --
function TPanelObjList.Get(Index: Integer): TPanelObj;
begin
  Result := inherited Get(Index);
end;
{$endif}

initialization
{$ifdef UNICODE}
{$else}
  {$ifdef UseElPack}
    UnicodeControls := True;
  {$endif}

  {$ifdef UseTNT}
    UnicodeControls := not IsWin32Platform;
  {$endif}
{$endif}
  WaitStream := TMemoryStream.Create;
  ErrorStream := TMemoryStream.Create;
  WaitBitmap := TBitmap.Create;
  ErrorBitmap := TBitmap.Create;
finalization
  ErrorBitmap.Free;
  WaitBitmap.Free;
  ErrorStream.Free;
  WaitStream.Free;
end.

