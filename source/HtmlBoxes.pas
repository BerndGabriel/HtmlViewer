{
HtmlViewer Version 12
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

unit HtmlBoxes;

interface

uses
  Windows, Graphics, Classes, Controls, StdCtrls,
  //
  HtmlControls,
  HtmlDraw,
  HtmlElements,
  HtmlGlobals,
  HtmlImages,
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
    procedure Add(View: THtmlBox);
    procedure Init;
    procedure Clear;
    procedure Remove(View: THtmlBox);
    property First: THtmlBox read FFirst;
    property Last: THtmlBox read FLast;
  end;

  THtmlBox = class
  private
    // structure
    FParent: THtmlBox;                    // parent view or nil if this is the root view.
    FPrev: THtmlBox;                      // previous sibling in parent's content.
    FNext: THtmlBox;                      // next sibling in parent's content.
    FChildren: THtmlBoxList;              // the content.
    FBounds: TRect;
    // border
    FMargins: TRectIntegers;
    FBorderWidths: TRectIntegers;
    FBorderColors: TRectColors;
    FBorderStyles: TRectStyles;
    FBackgroundColor: TColor;
    // content
    FPadding: TRectIntegers;
    FAlignment: TAlignment;
    // content: text
    FText: ThtString;
    FFont: TFont;
    // content: image
    FImage: ThtImage;
    FTiled: Boolean;
    FTileWidth: Integer;
    FTileHeight: Integer;
    procedure SetImage(const Value: ThtImage);
    function GetContentRect: TRect;
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    function Clipping: Boolean;
    function IsVisible: Boolean; virtual;
    procedure AddChild(Child: THtmlBox);
    procedure ExtractChild(Child: THtmlBox);
    procedure SetParent(const Value: THtmlBox);
    property Parent: THtmlBox read FParent write SetParent;
    property Prev: THtmlBox read FPrev;
    property Next: THtmlBox read FNext;
  public
    // construction / destruction
    constructor Create(ParentBox: THtmlBox);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    // change events sent by THtmlViewer
    procedure Resized; virtual;
    procedure Rescaled(const NewScale: Double); virtual;
    //
    procedure Paint(Canvas: TScalingCanvas); virtual;
    //
    property BoundsRect: TRect read FBounds write FBounds;
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
    property Padding: TRectIntegers read FPadding write FPadding;
    //
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Children: THtmlBoxList read FChildren;
    property Font: TFont read FFont;
    property Text: ThtString read FText write FText;
    property Image: ThtImage read FImage write SetImage;
    property Tiled: Boolean read FTiled write FTiled;
    property TileHeight: Integer read FTileHeight write FTileHeight;
    property TileWidth: Integer read FTileWidth write FTileWidth;
  end;

//------------------------------------------------------------------------------
// THtmlControlBox
//------------------------------------------------------------------------------

  THtmlControlBox = class(THtmlBox)
  protected
    function GetControl: TControl; virtual; abstract;
    function IsVisible: Boolean; override;
  public
    procedure Resized; override;
    property Control: TControl read GetControl;
  end;

//------------------------------------------------------------------------------
// THtmlScrollControl
//------------------------------------------------------------------------------

  THtmlScrollControl = class(TCustomHtmlScrollBox)
  private
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
    constructor Create(ParentBox: THtmlBox; Control: THtmlFramesetControl);
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
    constructor Create(ParentBox: THtmlBox; Control: THtmlBodyControl); overload;
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

  THtmlAnonymousBox = class(THtmlBox)
  end;

implementation

{ THtmlBoxList }

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure THtmlBoxList.Add(View: THtmlBox);
// append View
var
  Prev: THtmlBox;
begin
  assert(View.Next = nil, 'Don''t add chained links twice. View.Next is not nil.');
  assert(View.Prev = nil, 'Don''t add chained links twice. View.Prev is not nil.');

  Prev := Last;

  // link to prev
  if Prev = nil then
    FFirst := View
  else
    Prev.FNext := View;
  View.FPrev := Prev;

  // link to next
  FLast := View;
  View.FNext := nil;
end;

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

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlBoxList.Init;
begin
  FFirst := nil;
  FLast := nil;
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

{ THtmlBox }

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.AddChild(Child: THtmlBox);
begin
  FChildren.Add(Child);
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure THtmlBox.AfterConstruction;
begin
  inherited;
  FFont := TFont.Create;
{$ifdef UseEnhancedRecord}
{$else}
  FChildren := THtmlBoxList.Create;
{$endif}
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure THtmlBox.BeforeDestruction;
begin
{$ifdef UseEnhancedRecord}
{$else}
  FChildren.Free;
{$endif}
  FFont.Free;
  if FImage <> nil then
    FImage.EndUse;
  inherited;
end;

//-- BG ---------------------------------------------------------- 13.04.2011 --
function THtmlBox.Clipping: Boolean;
begin
  Result := False;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
constructor THtmlBox.Create(ParentBox: THtmlBox);
begin
  inherited Create;
  Parent := ParentBox;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.ExtractChild(Child: THtmlBox);
begin
  FChildren.Remove(Child);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function THtmlBox.GetContentRect: TRect;
begin
  DeflateRect(Result, BoundsRect, Margins);
  DeflateRect(Result, BorderWidths);
  DeflateRect(Result, Padding);
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
function THtmlBox.IsVisible: Boolean;
begin
  Result := (FParent = nil) or (FParent.Visible); 
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure THtmlBox.Paint(Canvas: TScalingCanvas);

  procedure DrawTestOutline(const Rect: TRect);
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBlack;
    Canvas.FrameRect(Rect);
  end;

var
  Rect, ScrRect: TRect;
  ContentWidth, ContentHeight: Integer;
  Child: THtmlBox;
  Org, Tile, TiledEnd: TPoint;
  Flags, ContentRegion, ExistingRegion, CombinedRegionResult: Integer;
begin
  if not Visible then
    exit;

  DrawTestOutline(BoundsRect);
  DeflateRect(Rect, BoundsRect, Margins);

  // border
  DrawBorder(Canvas, Rect, BorderWidths, BorderColors, BorderStyles, Color);
  DeflateRect(Rect, BorderWidths);

  // content
  DeflateRect(Rect, Padding);
  CombinedRegionResult := SIMPLEREGION;
  ContentRegion := 0;
  ExistingRegion := 0;
  if Clipping or Tiled then
  begin
    ScrRect.TopLeft := Canvas.ToWindowUnits(Rect.TopLeft);
    ScrRect.BottomRight := Canvas.ToWindowUnits(Rect.BottomRight);
    ContentRegion := CreateRectRgnIndirect(ScrRect);
    ExistingRegion := GetClipRegion(Canvas);
    if ExistingRegion <> 0 then
      CombinedRegionResult := CombineRgn(ContentRegion, ContentRegion, ExistingRegion, RGN_AND);
  end;
  try
    if CombinedRegionResult in [SIMPLEREGION, COMPLEXREGION] then
    begin
      ContentWidth := Rect.Right - Rect.Left;
      ContentHeight := Rect.Bottom - Rect.Top;

      if Image <> nil then
      begin
        // image
        if (Image = ErrorImage) or (Image = DefImage) then
        begin
          Image.Draw(Canvas, Rect.Left + 4, Rect.Top + 4, Image.Width, Image.Height);
        end
        else if Tiled then
        begin
          // prepare horizontal tiling
          if TileHeight = 0 then
            TileHeight := Image.Height;
          Tile.X := GetPositionInRange(bpCenter, 0, Rect.Right - Rect.Left - TileWidth) + Rect.Left;
          AdjustForTiling(Tiled, Rect.Left, Rect.Right, TileWidth, Tile.X, TiledEnd.X);

          // prepare vertical tiling
          if TileWidth = 0 then
            TileWidth := Image.Width;
          Tile.Y := GetPositionInRange(bpCenter, 0, Rect.Bottom - Rect.Top - TileHeight) + Rect.Top;
          AdjustForTiling(Tiled, Rect.Top, Rect.Bottom, TileHeight, Tile.Y, TiledEnd.Y);

          // clip'n'paint
          SelectClipRgn(Canvas.Handle, ContentRegion);
          Image.DrawTiled(Canvas, Tile.X, Tile.Y, TiledEnd.X, TiledEnd.Y, TileWidth, TileHeight);
          SelectClipRgn(Canvas.Handle, ExistingRegion);
        end
        else
          Image.Draw(Canvas, Rect.Left, Rect.Top, ContentWidth, ContentHeight);
      end;

      if Clipping then
        SelectClipRgn(Canvas.Handle, ContentRegion);

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

      // structure
      if Children.First <> nil then
      begin
        GetWindowOrgEx(Canvas.Handle, Org);
        SetWindowOrgEx(Canvas.Handle, Org.X + Rect.Left, Org.Y + Rect.Top, nil);
        Child := Children.First;
        repeat
          Child.Paint(Canvas);
          Child := Child.Next;
        until Child = nil;
        SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
      end;
    end;
  finally
    if ContentRegion <> 0 then
    begin
      SelectClipRgn(Canvas.Handle, ExistingRegion);
      DeleteObject(ContentRegion);
      if ExistingRegion <> 0 then
        DeleteObject(ExistingRegion);
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure THtmlBox.Rescaled(const NewScale: Double);
begin
// override me
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.Resized;
begin
// override me
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

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure THtmlBox.SetParent(const Value: THtmlBox);
begin
  if FParent <> Value then
  begin
    if FParent <> nil then
      FParent.ExtractChild(Self);
    FParent := Value;
    if FParent <> nil then
      FParent.AddChild(Self);
  end;
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

procedure THtmlScrollControl.Paint;
begin
  inherited;
  FBox.Paint(Canvas);
end;

{ THtmlFramesetBox }

//-- BG ---------------------------------------------------------- 24.04.2011 --
constructor THtmlFramesetBox.Create(ParentBox: THtmlBox; Control: THtmlFramesetControl);
begin
  inherited Create(ParentBox);
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
constructor THtmlBodyBox.Create(ParentBox: THtmlBox; Control: THtmlBodyControl);
begin
  inherited Create(ParentBox);
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
