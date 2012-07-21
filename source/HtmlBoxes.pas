{
Version   12
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
}

{$I htmlcons.inc}

unit HtmlBoxes;

interface

uses
  Windows, Graphics, Classes, Controls, StdCtrls,
  //
  HtmlControls,
  HtmlDraw,
  HtmlElements,
  HtmlFonts,
  HtmlGlobals,
  HtmlImages,
  HtmlStyles,
  StyleTypes;

type

//------------------------------------------------------------------------------
// THtmlBox is base class for the visual representation of THtmlElements
//------------------------------------------------------------------------------

  THtmlBox = class;

  THtmlBoxList = {$ifdef UseEnhancedRecord} record {$else} class {$endif}
  private
    FFirst: THtmlBox;
    FLast: THtmlBox;
  public
    function IsEmpty: Boolean;
    function Contains(View: THtmlBox): Boolean;
    procedure Add(View: THtmlBox; Before: THtmlBox = nil);
    procedure Clear;
    procedure Init;
    procedure Remove(View: THtmlBox);
    procedure Sort;
    property First: THtmlBox read FFirst;
    property Last: THtmlBox read FLast;
  end;

  THtmlBox = class
  private
    // structure
    FParent: THtmlBox;       // parent view or nil if this is the root view.
    FPrev: THtmlBox;         // previous sibling in parent's content.
    FNext: THtmlBox;         // next sibling in parent's content.
    FChildren: THtmlBoxList; // the content.
    // position
    FBounds: TRect;          // outer bounds of box relative to FParent (but without FRelativeOffset).
    FRelativeOffset: TRect;  // offset of position:relative
    FDisplay: TDisplayStyle;
    FFloat: TBoxFloatStyle;
    FPosition: TBoxPositionStyle;
    // background image
    FImage: ThtImage;
    FTiled: Boolean;
    FTileWidth: Integer;
    FTileHeight: Integer;
    // border
    FMargins: TRectIntegers;
    FBorderWidths: TRectIntegers;
    FBorderColors: TRectColors;
    FBorderStyles: TRectStyles;
    FBackgroundColor: TColor;
    // content
    FPadding: TRectIntegers;
    // content: text
    FText: ThtString;
    FFont: ThtFont;
    FAlignment: TAlignment;
    FElement: THtmlElement; // referenced, don't free
    FProperties: TResultingPropertyMap; // owned
    procedure SetImage(const Value: ThtImage);
    function GetContentRect: TRect;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetFont(const Value: ThtFont);
    procedure SetProperties(const Value: TResultingPropertyMap);
  protected
    function Clipping: Boolean;
    function IsVisible: Boolean; virtual;
  public
    // construction / destruction
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // children
    procedure AppendChild(Child: THtmlBox);
    procedure InsertChild(Child, Before: THtmlBox);
    procedure ExtractChild(Child: THtmlBox);
    procedure SortChildren;
    // change events sent by THtmlViewer
    procedure Resized; virtual;
    procedure Rescaled(const NewScale: Double); virtual;
    //
    procedure Paint(Canvas: TScalingCanvas); virtual;
    //
    property Parent: THtmlBox read FParent;
    property Prev: THtmlBox read FPrev;
    property Next: THtmlBox read FNext;
    property Element: THtmlElement read FElement write FElement;
    property Properties: TResultingPropertyMap read FProperties write SetProperties;
    property BoundsRect: TRect read FBounds write FBounds;
    property Display: TDisplayStyle read FDisplay write FDisplay;
    property Float: TBoxFloatStyle read FFloat write FFloat;
    property Position: TBoxPositionStyle read FPosition write FPosition;
    property RelativeOffset: TRect read FRelativeOffset write FRelativeOffset;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property Visible: Boolean read IsVisible;
    //
    property Margins: TRectIntegers read FMargins write FMargins;
    property BorderWidths: TRectIntegers read FBorderWidths write FBorderWidths;
    property BorderColors: TRectColors read FBorderColors write FBorderColors;
    property BorderStyles: TRectStyles read FBorderStyles write FBorderStyles;
    property Color: TColor read FBackgroundColor write FBackgroundColor;
    property ContentRect: TRect read GetContentRect;
    property Paddings: TRectIntegers read FPadding write FPadding;
    //
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Children: THtmlBoxList read FChildren;
    property Font: ThtFont read FFont write SetFont;
    property Text: ThtString read FText write FText;
    property Image: ThtImage read FImage write SetImage;
    property Tiled: Boolean read FTiled write FTiled;
    property TileHeight: Integer read FTileHeight write FTileHeight;
    property TileWidth: Integer read FTileWidth write FTileWidth;
  end;

//------------------------------------------------------------------------------
// THtmlElementBox is (base) class for ordinary HTML elements
//------------------------------------------------------------------------------

  THtmlElementBox = class(THtmlBox)
  end;

//------------------------------------------------------------------------------
// THtmlControlBox is base class for the visual representations of the root
//                 THtmlElements BODY and FRAMESET.
//------------------------------------------------------------------------------

  THtmlControlBox = class(THtmlElementBox)
  protected
    function GetControl: TControl; virtual; abstract;
    function IsVisible: Boolean; override;
  public
    procedure Resized; override;
    property Control: TControl read GetControl;
  end;

//------------------------------------------------------------------------------
// THtmlScrollControl is base class for the scroll boxes visually representing
//                    the root THtmlElements BODY and FRAMESET.
//------------------------------------------------------------------------------

  THtmlScrollControl = class(TCustomHtmlScrollBox)
  private
    // BG, 19.08.2011:  FBox is a reference to the box, that owns this scroll control.
    // Do not free in desctructor!
    FBox: THtmlBox;
  protected
    function GetContentHeight: Integer; override;
    function GetContentWidth: Integer; override;
    procedure Init(Box: THtmlBox);
    procedure Paint; override;
  end;

//------------------------------------------------------------------------------
// THtmlFramesetBox
//------------------------------------------------------------------------------

  THtmlFramesetControl = class(THtmlScrollControl)
  end;

  THtmlFramesetBox = class(THtmlControlBox)
  private
    FControl: THtmlFramesetControl;
  protected
    function GetControl: TControl; override;
  public
    constructor Create(Control: THtmlFramesetControl);
    procedure Rescaled(const NewScale: Double); override;
    property Control: THtmlFramesetControl read FControl;
  end;

//------------------------------------------------------------------------------
// THtmlBodyBox
//------------------------------------------------------------------------------

  THtmlBodyControl = class(THtmlScrollControl)
  end;

  THtmlBodyBox = class(THtmlControlBox)
  private
    FControl: THtmlBodyControl;
  protected
    function GetControl: TControl; override;
  public
    constructor Create(Control: THtmlBodyControl);
    procedure Rescaled(const NewScale: Double); override;
    property Control: THtmlBodyControl read FControl;
  end;

//------------------------------------------------------------------------------
// THtmlFormControlBox
//------------------------------------------------------------------------------

//  THtmlFormControlBox = class(THtmlBox)
//  protected
//    function GetControl: TWinControl; virtual; abstract;
//  public
//    constructor Create(Owner: TControl; ParentBox: THtmlBox);
//    property Control: TWinControl read GetControl;
//  end;

//------------------------------------------------------------------------------
// THtmlAnonymousBox
//------------------------------------------------------------------------------
// The renderer generates THtmlAnonymousBoxes to group lines of inline elements
// and text.
//------------------------------------------------------------------------------

  THtmlAnonymousBox = class(THtmlBox)
  end;

implementation

{ THtmlBoxList }

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlBoxList.Clear;
var
  View: THtmlBox;
begin
  FLast := nil;
  while First <> nil do
  begin
    View := First;
    FFirst := View.Next;
    View.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 18.12.2011 --
function THtmlBoxList.Contains(View: THtmlBox): Boolean;
var
  Box: THtmlBox;
begin
  Box := First;
  while Box <> nil do
  begin
    if Box = View then
    begin
      Result := True;
      Exit;
    end;
    Box := Box.Next;
  end;
  Result := False;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlBoxList.Init;
begin
  FFirst := nil;
  FLast := nil;
end;


{
//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure THtmlBoxList.Append(View: THtmlBox);
// append View
var
  Prev: THtmlBox;
begin
  assert(View.Next = nil, 'Don''t add chained links twice. View.Next is not nil.');
  assert(View.Prev = nil, 'Don''t add chained links twice. View.Prev is not nil.');

end;
}


//-- BG ---------------------------------------------------------- 18.12.2011 --
procedure THtmlBoxList.Add(View, Before: THtmlBox);
{
 Insert View before Before. If Before is nil, append after last box.
}
begin
  assert(View.Next = nil, 'Don''t insert chained links twice. View.Next is not nil.');
  assert(View.Prev = nil, 'Don''t insert chained links twice. View.Prev is not nil.');

  if Before = nil then
  begin
    // append after last

    // link to prev
    if Last = nil then
      FFirst := View
    else
      Last.FNext := View;
    View.FPrev := Last;

    // link to next
    FLast := View;
  end
  else if Before = First then
  begin
    // insert before first

    // link to next
    if First = nil then
      FLast := View
    else
      First.FPrev := View;
    View.FNext := First;

    // link to prev
    FFirst := View;
  end
  else
  begin
    // link between Before.Prev and Before.

    assert(Contains(Before), 'Box to insert before is not mine.');
    assert(Before.Prev <> nil, 'Illegal link status: ''Before'' is not first link, but has no previous link.');

    // link to prev
    Before.Prev.FNext := View;
    View.FPrev := Before.Prev;

    // link to next
    Before.FPrev := View;
    View.FNext := Before;
  end;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
function THtmlBoxList.IsEmpty: Boolean;
begin
  Result := FFirst = nil;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlBoxList.Remove(View: THtmlBox);
var
  Prev: THtmlBox;
  Next: THtmlBox;
begin
  Prev := View.Prev;
  Next := View.Next;

  if Prev = nil then
  begin
    if First = View then
      FFirst := Next;
  end
  else
  begin
    Prev.FNext := Next;
    View.FPrev := nil;
  end;

  if Next = nil then
  begin
    if Last = View then
      FLast := Prev;
  end
  else
  begin
    Next.FPrev := Prev;
    View.FNext := nil;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2011 --
procedure THtmlBoxList.Sort;
{
 Move boxes with relative position to the end as they may overlap and hide other
 boxes.
}
var
  View, Next, FirstMoved: THtmlBox;
begin
  if First <> Last then
  begin
    // at least 2 entries, thus sorting required:
    FirstMoved := nil; // stop at end of list (still nil) or at first relocated box.
    View := First;
    while View <> FirstMoved do
    begin
      Next := View.Next;
      if View.Position = posRelative then
      begin
        Remove(View);
        Add(View);
        if FirstMoved = nil then
          FirstMoved := View;
      end;
      View := Next;
    end;
  end;
end;

{ THtmlBox }

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure THtmlBox.AfterConstruction;
begin
  inherited;
  FFont := ThtFont.Create;
{$ifdef UseEnhancedRecord}
{$else}
  FChildren := THtmlBoxList.Create;
{$endif}
end;

//-- BG ---------------------------------------------------------- 18.12.2011 --
procedure THtmlBox.AppendChild(Child: THtmlBox);
begin
  InsertChild(Child, nil);
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure THtmlBox.BeforeDestruction;
begin
{$ifdef UseEnhancedRecord}
  FChildren.Clear;
{$else}
  FChildren.Free;
{$endif}
  FFont.Free;
  FProperties.Free;
  if Parent <> nil then
    Parent.ExtractChild(Self);
  Image := nil;
  inherited;
end;

//-- BG ---------------------------------------------------------- 13.04.2011 --
function THtmlBox.Clipping: Boolean;
begin
  Result := False;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
constructor THtmlBox.Create;
begin
  inherited Create;
  FBackgroundColor := clNone;
  FBorderColors := NoneColors;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.ExtractChild(Child: THtmlBox);
begin
  FChildren.Remove(Child);
  Child.FParent := nil;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlBox.GetContentRect: TRect;
begin
  DeflateRect(Result, BoundsRect, Margins);
  DeflateRect(Result, BorderWidths);
  DeflateRect(Result, Paddings);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlBox.GetHeight: Integer;
begin
  Result := FBounds.Bottom - FBounds.Top;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlBox.GetWidth: Integer;
begin
  Result := FBounds.Right- FBounds.Left;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.InsertChild(Child: THtmlBox; Before: THtmlBox);
begin
  FChildren.Add(Child, Before);
  Child.FParent := Self;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlBox.IsVisible: Boolean;
begin
  Result := (FDisplay <> pdNone) and ((FParent = nil) or FParent.Visible);
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure THtmlBox.Paint(Canvas: TScalingCanvas);

  procedure DrawTestOutline(const Rect: TRect);
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBlack;
    Canvas.FrameRect(Rect);
  end;

  function CreateClipRegion(Clipping: Boolean; const Rect: TRect; out ExistingRegion, ClippingRegion: Integer): Boolean;
  {
   Returns True, if there is a draw area because either Clipping is false or clip region is not empty.
  }
  var
    ScrRect: TRect;
  begin
    ClippingRegion := 0;
    ExistingRegion := 0;
    Result := True;
    if Clipping then
    begin
      ScrRect.TopLeft := Canvas.ToWindowUnits(Rect.TopLeft);
      ScrRect.BottomRight := Canvas.ToWindowUnits(Rect.BottomRight);
      ClippingRegion := CreateRectRgnIndirect(ScrRect);
      ExistingRegion := GetClipRegion(Canvas);
      if ExistingRegion <> 0 then
        Result := CombineRgn(ClippingRegion, ClippingRegion, ExistingRegion, RGN_AND) in [SIMPLEREGION, COMPLEXREGION];
      if Result then
        SelectClipRgn(Canvas.Handle, ClippingRegion);
    end;
  end;

  procedure DeleteClipRegion(ExistingRegion, ClippingRegion: Integer);
  begin
    if ClippingRegion <> 0 then
    begin
      SelectClipRgn(Canvas.Handle, ExistingRegion);
      DeleteObject(ClippingRegion);
      if ExistingRegion <> 0 then
        DeleteObject(ExistingRegion);
    end;
  end;

  procedure DrawImage(const Rect: TRect);
  var
    ClippingRegion, ExistingRegion: Integer;
    Tile, TiledEnd: TPoint;
  begin
    if CreateClipRegion(True, Rect, ExistingRegion, ClippingRegion) then
      try
        if (Image = ErrorImage) or (Image = DefImage) then
        begin
          Image.Draw(Canvas, Rect.Left + 4, Rect.Top + 4, Image.Width, Image.Height);
        end
        else if Tiled then
        begin
          // prepare horizontal tiling
          if TileWidth = 0 then
            TileWidth := Image.Width;
          Tile.X := 0; // GetPositionInRange(bpCenter, 0, Rect.Right - Rect.Left - TileWidth) + Rect.Left;
          AdjustForTiling(Tiled, Rect.Left, Rect.Right, TileWidth, Tile.X, TiledEnd.X);

          // prepare vertical tiling
          if TileHeight = 0 then
            TileHeight := Image.Height;
          Tile.Y := 0; // GetPositionInRange(bpCenter, 0, Rect.Bottom - Rect.Top - TileHeight) + Rect.Top;
          AdjustForTiling(Tiled, Rect.Top, Rect.Bottom, TileHeight, Tile.Y, TiledEnd.Y);

          // clip'n'paint
          Image.DrawTiled(Canvas, Tile.X, Tile.Y, TiledEnd.X, TiledEnd.Y, TileWidth, TileHeight);
        end
        else
          Image.Draw(Canvas, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
      finally
        DeleteClipRegion(ExistingRegion, ClippingRegion);
      end;
  end;

var
  Rect: TRect;
  Child: THtmlBox;
  Org: TPoint;
  Flags, ContentRegion, ExistingRegion: Integer;
begin
  if not Visible then
    exit;
  Rect := BoundsRect;
  if Position = posRelative then
    OffsetRect(Rect, RelativeOffset);

  // background
  if Color <> clNone then
  begin
    Canvas.Brush.Color := ThemedColor(Color);
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect);
  end;
{$ifdef DEBUG}
  DrawTestOutline(Rect);
{$endif}
  if Image <> nil then
    DrawImage(Rect);
  DeflateRect(Rect, Margins);

  // border
  DrawBorder(Canvas, Rect, BorderWidths, BorderColors, BorderStyles, clNone);
  DeflateRect(Rect, BorderWidths);

  // content
  DeflateRect(Rect, Paddings);
  if CreateClipRegion(Clipping, Rect, ExistingRegion, ContentRegion) then
    try
      if Length(Text) > 0 then
      begin
        // text
        DrawTestOutline(Rect);
        case Alignment of
          taLeftJustify: Flags := DT_LEFT;
          taRightJustify: Flags := DT_RIGHT;
          taCenter: Flags := DT_CENTER;
        else
          Flags := 0;
        end;
        Canvas.Brush.Style := bsClear;
        Canvas.Font := Font;
        DrawTextW(Canvas.Handle, PhtChar(Text), Length(Text), Rect, Flags + DT_NOCLIP + DT_VCENTER + DT_SINGLELINE);
      end;

      // children
      if Children.First <> nil then
      begin
        GetViewportOrgEx(Canvas.Handle, Org);
        SetViewportOrgEx(Canvas.Handle, Org.X + Rect.Left, Org.Y + Rect.Top, nil);
        Child := Children.First;
        repeat
          Child.Paint(Canvas);
          Child := Child.Next;
        until Child = nil;
        SetViewportOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
      end;
    finally
      DeleteClipRegion(ExistingRegion, ContentRegion);
    end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlBox.Rescaled(const NewScale: Double);
var
  Child: THtmlBox;
begin
  Child := Children.First;
  while Child <> nil do
  begin
    Child.Rescaled(NewScale);
    Child := Child.Next;
  end;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.Resized;
var
  Child: THtmlBox;
begin
  Child := Children.First;
  while Child <> nil do
  begin
    Child.Resized;
    Child := Child.Next;
  end;
end;

//-- BG ---------------------------------------------------------- 03.05.2011 --
procedure THtmlBox.SetFont(const Value: ThtFont);
begin
  FFont.Assign(Value);
end;

//-- BG ---------------------------------------------------------- 16.04.2011 --
procedure THtmlBox.SetImage(const Value: ThtImage);
begin
  if FImage <> Value then
  begin
    if FImage <> nil then
      FImage.EndUse;
    FImage := Value;
    if FImage <> nil then
      FImage.BeginUse;
  end;
end;

procedure THtmlBox.SetProperties(const Value: TResultingPropertyMap);
begin
  if FProperties <> Value then
  begin
    if FProperties <> nil then
      FProperties.Free;
    FProperties := Value;
  end;
end;

//-- BG ---------------------------------------------------------- 14.12.2011 --
procedure THtmlBox.SortChildren;
begin
  Children.Sort;
end;

{ THtmlControlBox }

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlControlBox.IsVisible: Boolean;
begin
  Result := GetControl.Visible and inherited IsVisible;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlControlBox.Resized;
begin
  inherited;
  Control.BoundsRect := ContentRect;
end;

{ THtmlScrollControl }

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlScrollControl.GetContentHeight: Integer;
begin
  Result := FBox.Height;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlScrollControl.GetContentWidth: Integer;
begin
  Result := FBox.Width;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlScrollControl.Init(Box: THtmlBox);

  function GetParent(C: TComponent): TWinControl;
  begin
    while C <> nil do
    begin
      if C is TWinControl then
      begin
        Result := TWinControl(C);
        exit;
      end;
      C := C.Owner;
    end;
    Result := nil;
  end;

begin
  FBox := Box;
  Parent := GetParent(Owner);
  Align := alClient;
  ScrollBars := ssBoth;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlScrollControl.Paint;
begin
  inherited;
  FBox.Paint(Canvas);
end;

{ THtmlFramesetBox }

//-- BG ---------------------------------------------------------- 24.04.2011 --
constructor THtmlFramesetBox.Create(Control: THtmlFramesetControl);
begin
  inherited Create;
  FControl := Control;
  FControl.Init(Self);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlFramesetBox.GetControl: TControl;
begin
  Result := FControl;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlFramesetBox.Rescaled(const NewScale: Double);
begin
  inherited;
  FControl.Scale := NewScale;
end;

{ THtmlBodyBox }

//-- BG ---------------------------------------------------------- 24.04.2011 --
constructor THtmlBodyBox.Create(Control: THtmlBodyControl);
begin
  inherited Create;
  FControl := Control;
  FControl.Init(Self);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlBodyBox.GetControl: TControl;
begin
  Result := FControl;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlBodyBox.Rescaled(const NewScale: Double);
begin
  inherited;
  FControl.Scale := NewScale;
end;

end.
