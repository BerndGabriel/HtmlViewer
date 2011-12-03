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

unit HtmlControls;

interface

uses
  Windows, Messages, Graphics, Classes, Controls, StdCtrls, Math;

type

//------------------------------------------------------------------------------
// TScrollCanvas implements a canvas for scaleable and scrollable content
//------------------------------------------------------------------------------
// ... and offers adequate transformation methods.
//
// "Window" is the rectangular area Rect(0, 0, Width, Height);
// "Content" is the rectangular area Rect(0, 0, ContentWidth, ContentHeight)
// "ContentPosition" is the content point that is shown at the window point (0, 0).
// What is shown at window point(Width, Height) depends on ContentPosition and Scale.
// A Scale > 1 enlarges the content, a Scale > 0 and < 1 shrinks the content.
// A Scale <= 0 is not specified. Default Scale is 1.
//------------------------------------------------------------------------------

  TCustomHtmlScrollBox = class;

  TScalingCanvasState = set of (scsPainting);

  TScalingCanvas = class(TControlCanvas)
  private
    FCanvasState: TScalingCanvasState;
    FControl: TCustomHtmlScrollBox;
    FWinExt: Integer; // scale nominator, window units
    FDocExt: Integer; // scale denominator, content units
    FContentPosition: TPoint;
    FWinClipRect: TRect;
    // Filled by BeforePaint and used by AfterPaint.
    FOldClipRegion: HRGN;
    FOldMapMode: Integer;
    FOldDocOrg, FOldWinOrg: TPoint;
    FOldDocExt, FOldWinExt: TSize;
    function GetScale: Double;
    procedure SetScale(const Value: Double);
    procedure SetContentPosition(const Value: TPoint);
    procedure SetContentPositionLeft(const Value: Integer);
    procedure SetContentPositionTop(const Value: Integer); // content position at viewport origin
  public
    constructor Create(Control: TCustomHtmlScrollBox);
    function ToContentUnits(const WindowPoint: TPoint): TPoint; overload;
    function ToContentUnits(WindowUnits: Integer): Integer; overload;
    function ToWindowUnits(const ContentPoint: TPoint): TPoint; overload;
    function ToWindowUnits(ContentUnits: Integer): Integer; overload;
    procedure BeforePaint;
    procedure AfterPaint;
    property ClipRect: TRect read FWinClipRect;
    property ContentPosition: TPoint read FContentPosition write SetContentPosition;
    property ContentPositionLeft: Integer read FContentPosition.X write SetContentPositionLeft;
    property ContentPositionTop: Integer read FContentPosition.Y write SetContentPositionTop;
    property DocExt: Integer read FDocExt; // scale denominator, content units
    property WinExt: Integer read FWinExt; // scale nominator, window units
    property Scale: Double read GetScale write SetScale;
    property CanvasState: TScalingCanvasState read FCanvasState;
  end;

//------------------------------------------------------------------------------
// THtmlScrollBox implements a control with scrollbars for scaleable content.
//------------------------------------------------------------------------------
// All scaling transformations are implemented in TScalingCanvas.
//
//
// Scrolling:
// The ScrollIncrement determines, how many window pixels the content is scrolled.
// Default ScrollIncrement is 17 (the default width of a scrollbar).
//
//
//------------------------------------------------------------------------------

  ThtScrollInfo = record
    Show: Boolean;      // show scrollbar
    DocSize: Integer;   // content size, window units
    WinSize: Integer;   // client area size, window units
  end;

  ThtScrollInfos = record
    H, V: ThtScrollInfo;
  end;

  THtmlScrollBoxState = set of (sbsUpdating);

  TCustomHtmlScrollBox = class(TWinControl)
  private
    FCanvas: TScalingCanvas;
    FState: THtmlScrollBoxState;
    FScrollIncrement: Integer; // how far to scroll with arrow buttons, window units
    FScrollBars: TScrollStyle; // which scrollbars are enabled?
    FScrollInfos: ThtScrollInfos; // used by UpdateScrollbars to remember the scrollbar settings.
    function GetContentPosition: TPoint;
    function GetContentPositionX: Integer;
    function GetContentPositionY: Integer;
    function GetScale: Double;
    function GetScrollInfo(Which: Integer): TScrollInfo;
    procedure SetContentPosition(const Value: TPoint);
    procedure SetContentPositionLeft(const Value: Integer);
    procedure SetContentPositionTop(const Value: Integer);
    procedure SetScale(const Value: Double);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure UpdateScrollBars;
  protected
    function GetContentHeight: Integer; virtual; abstract;
    function GetContentWidth: Integer; virtual; abstract;
    procedure Resize; override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property Canvas: TScalingCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ContentHeight: Integer read GetContentHeight;
    property ContentPosition: TPoint read GetContentPosition write SetContentPosition;
    property ContentPositionLeft: Integer read GetContentPositionX write SetContentPositionLeft;
    property ContentPositionTop: Integer read GetContentPositionY write SetContentPositionTop;
    property ContentWidth: Integer read GetContentWidth;
    property Scale: Double read GetScale write SetScale;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
  end;

implementation

var
  sbWidth, sbHeight: Integer; // UpdateScrollBars remembers the current scrollbar sizes here.

//-- BG ---------------------------------------------------------- 22.04.2011 --
function ClipPosition(const SI: TScrollInfo; var Value: Integer): Boolean;
begin
  if Value < SI.nMin then
    Value := SI.nMin
  else if Value > SI.nMax - Integer(SI.nPage) then
    Value := SI.nMax - Integer(SI.nPage);
  Result := Value <> SI.nPos;
end;

{ TScalingCanvas }

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TScalingCanvas.AfterPaint;
// Resets the content/window transformation parameters.
begin
  Exclude(FCanvasState, scsPainting);
  SetMapMode(Handle, FOldMapMode);
  if FOldMapMode in [MM_ISOTROPIC, MM_ANISOTROPIC] then
  begin
    SetWindowExtEx(Handle, FOldDocExt.cx, FOldDocExt.cy, nil);
    SetViewportExtEx(Handle, FOldWinExt.cx, FOldWinExt.cy, nil);
  end;
  SetWindowOrgEx(Handle, FOldDocOrg.X, FOldDocOrg.Y, nil);
  SetViewportOrgEx(Handle, FOldWinOrg.X, FOldWinOrg.Y, nil);
  SelectClipRgn(Handle, FOldClipRegion);
  if FOldClipRegion <> 0 then
    DeleteObject(FOldClipRegion);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TScalingCanvas.BeforePaint;
// Set the content/window transformation parameters.
//
// In DoubleBuffer mode Delphi 6 creates a buffer as large as the entire window,
// while Delphi 2007+ create a buffer as small as the ClipRect only an sets
// WindowOrg to the upper left corner of the ClipRect (which is wrong, as it
// sets device coordinates, which had to be set to ViewportOrg).
begin
  FOldClipRegion := CreateRectRgn(0, 0, 1, 1);
  GetClipRgn(Handle, FOldClipRegion);
  FWinClipRect := inherited ClipRect;
  FOldMapMode := GetMapMode(Handle);
  GetWindowOrgEx(Handle, FOldDocOrg);
  GetViewportOrgEx(Handle, FOldWinOrg);

  SetMapMode(Handle, MM_ISOTROPIC);
  SetWindowExtEx(Handle, FDocExt, FDocExt, @FOldDocExt);
  SetViewportExtEx(Handle, FWinExt, FWinExt, @FOldWinExt);
  SetWindowOrgEx(Handle, FContentPosition.X, FContentPosition.Y, nil);
  SetViewportOrgEx(Handle, -FWinClipRect.Left, -FWinClipRect.Top, nil);
  Include(FCanvasState, scsPainting);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
constructor TScalingCanvas.Create(Control: TCustomHtmlScrollBox);
begin
  inherited Create;
  FWinExt := 10000; // 10000 allows finer scales than current screens have pixels.
  FDocExt := 10000; // 10000 allows finer scales than current screens have pixels.
  FControl := Control;
  inherited Control := Control;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function TScalingCanvas.GetScale: Double;
begin
  if (FWinExt <> 0) and (FDocExt <> 0) then
    Result := FWinExt / FDocExt
  else
    Result := 1.0;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure TScalingCanvas.SetContentPosition(const Value: TPoint);
begin
  if (FContentPosition.X <> Value.X) or (FContentPosition.Y <> Value.Y) then
  begin
    FContentPosition := Value;
    FControl.UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure TScalingCanvas.SetContentPositionLeft(const Value: Integer);
begin
  if FContentPosition.X <> Value then
  begin
    FContentPosition.X := Value;
    FControl.UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure TScalingCanvas.SetContentPositionTop(const Value: Integer);
begin
  if FContentPosition.Y <> Value then
  begin
    FContentPosition.Y := Value;
    FControl.UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure TScalingCanvas.SetScale(const Value: Double);
// Converts the floating point value to a fraction, which is
// used by Windows via GetWindowExtEx and GetViewportExtEx.
var
  DocExt, WinExt: Integer;
  VAbs: Double;
begin
  // Scale = WinExt / DocExt
  VAbs := Abs(Value);
  if VAbs >= 10000 then
  begin
    WinExt := Round(Value);
    DocExt := 1;
  end
  else if VAbs >= 1000 then
  begin
    WinExt := Round(10 * Value);
    DocExt := 10;
  end
  else if VAbs >= 100 then
  begin
    WinExt := Round(100 * Value);
    DocExt := 100;
  end
  else if VAbs >= 10 then
  begin
    WinExt := Round(1000 * Value);
    DocExt := 1000;
  end
  else if VAbs >= 1 then
  begin
    WinExt := Round(10000 * Value);
    DocExt := 10000;
  end
  else if VAbs >= 0.1 then
  begin
    WinExt := 10000;
    DocExt := Round(10000 / Value);
  end
  else if VAbs >= 0.01 then
  begin
    WinExt := 1000;
    DocExt := Round(1000 / Value);
  end
  else if VAbs >= 0.001 then
  begin
    WinExt := 100;
    DocExt := Round(100 / Value);
  end
  else if VAbs >= 0.0001 then
  begin
    WinExt := 10;
    DocExt := Round(10 / Value);
  end
  else if VAbs > 1.0 / MaxInt then
  begin
    WinExt := 1;
    DocExt := Round(1 / Value);
  end
  else
  begin
    WinExt := 1;
    DocExt := MaxInt;
  end;

  if (DocExt <> FDocExt) or (WinExt <> FWinExt) then
  begin
    FDocExt := DocExt;
    FWinExt := WinExt;
    FControl.UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function TScalingCanvas.ToContentUnits(WindowUnits: Integer): Integer;
// Converts an amount of units of the window coordinate system to units of the content coordinate system.
// No translation, only the scale transformation. Use this for e.g. size or distance transformations.
//
// Complete coordinate translations are calculated with the overloaded version with the TPoint parameter.
begin
  Result := MulDiv(WindowUnits, FDocExt, FWinExt);
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function TScalingCanvas.ToContentUnits(const WindowPoint: TPoint): TPoint;
// Converts a point of the window coordinate system to a point in the content coordinate system.
// Includes both translation and scale transformation.
//
// To transform a size or distance value use the overloaded version with the Integer parameter.
begin
  assert(scsPainting in CanvasState, 'ToContentUnits: Coordinate transformation is valid while painting only');
  Result.X := MulDiv(WindowPoint.X + FWinClipRect.Left, FDocExt, FWinExt) + ContentPositionLeft;
  Result.Y := MulDiv(WindowPoint.Y + FWinClipRect.Top, FDocExt, FWinExt) + ContentPositionTop;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function TScalingCanvas.ToWindowUnits(ContentUnits: Integer): Integer;
// Converts an amount of units of the content coordinate system to units of the window coordinate system.
// No translation, only the scale transformation. Use this for e.g. size or distance transformations.
//
// Complete coordinate translations are calculated with the overloaded version with the TPoint parameter.
begin
  Result := MulDiv(ContentUnits, FWinExt, FDocExt);
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function TScalingCanvas.ToWindowUnits(const ContentPoint: TPoint): TPoint;
// Converts a point of the content coordinate system to a point in the window coordinate system.
// Includes both translation and scale transformation.
//
// To transform a size or distance value use the overloaded version with the Integer parameter.
begin
  assert(scsPainting in CanvasState, 'ToWindowUnits: Coordinate transformation is valid while painting only');
  Result.X := MulDiv(ContentPoint.X - ContentPositionLeft, FWinExt, FDocExt) - FWinClipRect.Left;
  Result.Y := MulDiv(ContentPoint.Y - ContentPositionTop, FWinExt, FDocExt) - FWinClipRect.Top;
end;

{ TCustomHtmlScrollBox }

//-- BG ---------------------------------------------------------- 22.04.2011 --
constructor TCustomHtmlScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True; // less flickering
  FCanvas := TScalingCanvas.Create(Self);
  FScrollIncrement := 17;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
destructor TCustomHtmlScrollBox.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function TCustomHtmlScrollBox.GetContentPosition: TPoint;
begin
  Result := Canvas.ContentPosition;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function TCustomHtmlScrollBox.GetContentPositionX: Integer;
begin
  Result := Canvas.ContentPositionLeft;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function TCustomHtmlScrollBox.GetContentPositionY: Integer;
begin
  Result := Canvas.ContentPositionTop;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
function TCustomHtmlScrollBox.GetScale: Double;
begin
  Result := Canvas.Scale;
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
function TCustomHtmlScrollBox.GetScrollInfo(Which: Integer): TScrollInfo;
begin
  Result.cbSize := SizeOf(TScrollInfo);
  Result.fMask := SIF_ALL;
  Windows.GetScrollInfo(Handle, Which, Result);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  SI: TScrollInfo;

  procedure SetHPosition(Value: Integer);
  begin
    if ClipPosition(SI, Value) then
      Canvas.ContentPositionLeft := Canvas.ToContentUnits(Value);
  end;

  procedure SetVPosition(Value: Integer);
  begin
    if ClipPosition(SI, Value) then
      Canvas.ContentPositionTop := Canvas.ToContentUnits(Value);
  end;

begin
  case Key of
    VK_PRIOR:
    begin
      SI := GetScrollInfo(SB_VERT);
      SetVPosition(SI.nPos - Integer(SI.nPage));
    end;

    VK_UP:
    begin
      SI := GetScrollInfo(SB_VERT);
      SetVPosition(SI.nPos - FScrollIncrement);
    end;

    VK_LEFT:
    begin
      SI := GetScrollInfo(SB_HORZ);
      SetHPosition(SI.nPos - FScrollIncrement);
    end;

    VK_RIGHT:
    begin
      SI := GetScrollInfo(SB_HORZ);
      SetHPosition(SI.nPos + FScrollIncrement);
    end;

    VK_DOWN:
    begin
      SI := GetScrollInfo(SB_VERT);
      SetVPosition(SI.nPos + FScrollIncrement);
    end;

    VK_NEXT:
    begin
      SI := GetScrollInfo(SB_VERT);
      SetVPosition(SI.nPos + Integer(SI.nPage));
    end;

  else
    inherited;
  end;

end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure TCustomHtmlScrollBox.Paint;
begin
// override me

// some test drawings:
//  Canvas.Pen.Color := clYellow;
//  Canvas.Brush.Color := clBlue;
//  Canvas.Rectangle(0, 0, ContentWidth, ContentHeight);
//  Canvas.Pen.Color := clRed;
//  Canvas.MoveTo(ContentPositionLeft - 20, ContentPositionTop);
//  Canvas.LineTo(ContentPositionLeft + 60, ContentPositionTop);
//  Canvas.MoveTo(ContentPositionLeft, ContentPositionTop - 20);
//  Canvas.LineTo(ContentPositionLeft, ContentPositionTop + 60);
//  Canvas.Pen.Color := clLime;
//  Canvas.MoveTo(300 - 20, 100);
//  Canvas.LineTo(300 + 60, 100);
//  Canvas.MoveTo(300, 100 - 20);
//  Canvas.LineTo(300, 100 + 60);
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      FCanvas.UpdateTextFlags;
      FCanvas.BeforePaint;
      try
        Paint;
      finally
        FCanvas.AfterPaint;
      end;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
procedure TCustomHtmlScrollBox.Resize;
begin
  inherited;
  UpdateScrollBars;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.SetContentPosition(const Value: TPoint);
begin
  Canvas.ContentPosition := Value;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.SetContentPositionLeft(const Value: Integer);
begin
  Canvas.ContentPositionLeft := Value;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.SetContentPositionTop(const Value: Integer);
begin
  Canvas.ContentPositionTop := Value;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.SetScale(const Value: Double);
begin
  Canvas.Scale := Value;
end;

//-- BG ---------------------------------------------------------- 19.04.2011 --
procedure TCustomHtmlScrollBox.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 19.04.2011 --
procedure TCustomHtmlScrollBox.UpdateScrollBars;

  function GetScrollInfos: ThtScrollInfos;
  begin
    if sbWidth = 0 then
    begin
      sbWidth := ClientWidth;
      sbHeight := ClientHeight;
      ShowScrollBar(Handle, SB_BOTH, True);
      Dec(sbWidth, ClientWidth);
      Dec(sbHeight, ClientHeight);
      ShowScrollBar(Handle, SB_BOTH, False);
    end;

    Result := FScrollInfos;

    Result.H.DocSize := Canvas.ToWindowUnits(ContentWidth);
    Result.H.WinSize := ClientWidth;
    if Result.V.Show then
      Inc(Result.H.WinSize, sbWidth);

    Result.V.DocSize := Canvas.ToWindowUnits(ContentHeight);
    Result.V.WinSize := ClientHeight;
    if Result.H.Show then
      Inc(Result.V.WinSize, sbHeight);

    case FScrollBars of
      ssVertical:
      begin
        Result.V.Show := Result.V.DocSize > Result.V.WinSize;
        if Result.V.Show then
          Dec(Result.H.WinSize, sbWidth);
        Result.H.Show := False;
      end;

      ssHorizontal:
      begin
        Result.V.Show := False;
        Result.H.Show := Result.H.DocSize > Result.H.WinSize;
        if Result.H.Show then
          Dec(Result.V.WinSize, sbHeight);
      end;

      ssBoth:
      begin
        Result.V.Show := Result.V.DocSize > Result.V.WinSize;
        if Result.V.Show then
          Dec(Result.H.WinSize, sbWidth);
        Result.H.Show := Result.H.DocSize > Result.H.WinSize;
        if Result.H.Show then
        begin
          Dec(Result.V.WinSize, sbWidth);
          if not Result.V.Show and (Result.V.DocSize > Result.V.WinSize) then
          begin
            Result.V.Show := True;
            Dec(Result.H.WinSize, sbWidth);
          end;
        end;
      end;
    else
      Result.H.Show := False;
      Result.V.Show := False;
    end;
  end;

var
  Info: TScrollInfo;
begin
  if sbsUpdating in FState then
    exit;

  Include(FState, sbsUpdating);
  try
    Info.cbSize := SizeOf(Info);
    Info.fMask := SIF_RANGE + SIF_PAGE + SIF_POS;
    Info.nMin := 0;

    FScrollInfos := GetScrollInfos;
    ShowScrollBar(Handle, SB_HORZ, FScrollInfos.H.Show);
    ShowScrollBar(Handle, SB_VERT, FScrollInfos.V.Show);

    if FScrollInfos.H.Show then
    begin
      Info.nMax := FScrollInfos.H.DocSize;
      Info.nPage := FScrollInfos.H.WinSize;
      Info.nPos := Canvas.ToWindowUnits(Canvas.ContentPositionLeft);
      SetScrollInfo(Handle, SB_HORZ, Info, True);
      Info := GetScrollInfo(SB_HORZ);
      Canvas.FContentPosition.X := Canvas.ToContentUnits(Info.nPos);
    end
    else
      Canvas.FContentPosition.X := 0;

    if FScrollInfos.V.Show then
    begin
      Info.nMax := FScrollInfos.V.DocSize;
      Info.nPage := FScrollInfos.V.WinSize;
      Info.nPos := Canvas.ToWindowUnits(Canvas.ContentPositionTop);
      SetScrollInfo(Handle, SB_VERT, Info, True);
      Info := GetScrollInfo(SB_VERT);
      Canvas.FContentPosition.Y := Canvas.ToContentUnits(Info.nPos);
    end
    else
      Canvas.FContentPosition.Y := 0;
  finally
    Exclude(FState, sbsUpdating);
  end;
  Invalidate;
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
procedure TCustomHtmlScrollBox.WMHScroll(var Message: TWMHScroll);
var
  SI: TScrollInfo;

  procedure SetPosition(Value: Integer);
  begin
    if ClipPosition(SI, Value) then
      Canvas.ContentPositionLeft := Canvas.ToContentUnits(Value);
  end;

begin
  SI := GetScrollInfo(SB_HORZ);
  case Message.ScrollCode of
    SB_LINEUP: SetPosition(SI.nPos - FScrollIncrement);
    SB_LINEDOWN: SetPosition(SI.nPos + FScrollIncrement);
    SB_PAGEUP: SetPosition(SI.nPos - Integer(SI.nPage));
    SB_PAGEDOWN: SetPosition(SI.nPos + Integer(SI.nPage));
    SB_THUMBPOSITION: SetPosition(SI.nTrackPos);
    SB_THUMBTRACK: SetPosition(SI.nTrackPos);
    SB_TOP: SetPosition(SI.nMin);
    SB_BOTTOM: SetPosition(SI.nMax);
    SB_ENDSCROLL: ;
  end;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlScrollBox.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
procedure TCustomHtmlScrollBox.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;

  procedure SetPosition(Value: Integer);
  begin
    if ClipPosition(SI, Value) then
      Canvas.ContentPositionTop := Canvas.ToContentUnits(Value);
  end;

begin
  SI := GetScrollInfo(SB_VERT);
  case Message.ScrollCode of
    SB_LINEUP: SetPosition(SI.nPos - FScrollIncrement);
    SB_LINEDOWN: SetPosition(SI.nPos + FScrollIncrement);
    SB_PAGEUP: SetPosition(SI.nPos - Integer(SI.nPage));
    SB_PAGEDOWN: SetPosition(SI.nPos + Integer(SI.nPage));
    SB_THUMBPOSITION: SetPosition(SI.nTrackPos);
    SB_THUMBTRACK: SetPosition(SI.nTrackPos);
    SB_TOP: SetPosition(SI.nMin);
    SB_BOTTOM: SetPosition(SI.nMax);
    SB_ENDSCROLL: ;
  end;
end;

end.
