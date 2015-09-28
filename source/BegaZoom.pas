{-------------------------------------------------------------------------------
Copyright (C) 2006-2012 by Bernd Gabriel.

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
-------------------------------------------------------------------------------}

{$include htmlcons.inc}

unit BegaZoom;

interface
{$ifndef NoFlatScrollbars}

uses
  Classes, Controls, ExtCtrls, Math, Types,
  // own units
  BegaScrollBox;

type
  TBegaZoomMode = (
    zmFitToPage,
    zmFitToHeight,
    zmFitToWidth,
    zm25, zm50, zm75,
    zm100, zm125, zm150,
    zm200, zm300, zm400,
    zmCustom);

  TBegaZoomModes = Set of TBegaZoomMode;
  TBegaZoomEvent = procedure(Sender: TObject; ZoomTo: Double) of object;

  //- BG --------------------------------------------------------- 05.11.2006 --
  TBegaZoomingScrollBox = class(TBegaScrollBox)
  private
    FZoom: Double;
    FOnZoomBy: TBegaZoomEvent;
    FOnZoomTo: TBegaZoomEvent;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    property OnZoomBy: TBegaZoomEvent read FOnZoomBy write FOnZoomBy;
    property OnZoomTo: TBegaZoomEvent read FOnZoomTo write FOnZoomTo;
    property ZoomFactor: Double read FZoom;
    procedure doZoomBy(ZoomBy: Double; Around: TPoint); virtual;
    procedure doZoomTo(ZoomTo: Double; Width, Height, BoxWidth, BoxHeight: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure zoomBy(Zoom: Double; Around: TPoint);
    procedure zoomTo(Zoom: Double; Width, Height: Integer);
    procedure zoomToFitIntoBox(Width, Height: Integer);
  end;

  //- BG --------------------------------------------------------- 23.01.2006 --
  TBegaZoomBox = class(TBegaZoomingScrollBox)
  private
    function getZoomedControl: TControl;
  protected
    property ZoomedControl: TControl read getZoomedControl;
    procedure doZoomBy(ZoomBy: Double; Around: TPoint); override;
    procedure doZoomTo(ZoomTo: Double; Width, Height, BoxWidth, BoxHeight: Integer); override;
  published
    property OnZoomBy;
    property OnZoomTo;
    property ZoomFactor;
  end;

procedure begaZoomControlAroundPoint(ZoomedControl: TControl; Zoom: Double; Around: TPoint);
function begaZoomFactorToMode(Factor: Double): TBegaZoomMode;

{$endif NoFlatScrollbars}
implementation
{$ifndef NoFlatScrollbars}

//- BG ----------------------------------------------------------- 12.11.2006 --
procedure begaZoomControlAroundPoint(ZoomedControl: TControl; Zoom: Double; Around: TPoint);
// zoom control around a fixed point (e.g. where mouse clicked).
var
  Size: TPoint;
  IPos: TPoint;
begin
  Size.X := round(ZoomedControl.Width * Zoom);
  Size.Y := round(ZoomedControl.Height * Zoom);
  IPos.X := round(Around.X - (Around.X - ZoomedControl.Left) * Zoom);
  IPos.Y := round(Around.Y - (Around.Y - ZoomedControl.Top) * Zoom);
  ZoomedControl.setBounds(IPos.X, IPos.Y, Size.X, Size.Y);
end;

//- BG ----------------------------------------------------------- 12.11.2006 --
function begaZoomFactorToMode(Factor: Double): TBegaZoomMode;
begin
  case round(Factor * 100) of
    25: Result := zm25;
    50: Result := zm50;
    75: Result := zm25;
   100: Result := zm100;
   125: Result := zm125;
   150: Result := zm150;
   200: Result := zm200;
   300: Result := zm300;
   400: Result := zm400;
  else
    Result := zmCustom;
  end;
end;

{ TBegaZoomingScrollBox }

//- BG ----------------------------------------------------------- 11.11.2006 --
constructor TBegaZoomingScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FZoom := 1.0;
end;

//- BG ----------------------------------------------------------- 12.07.2006 --
function TBegaZoomingScrollBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if Shift * [ssShift, ssAlt, ssCtrl] = [ssCtrl] then
  begin
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
    if not Result and not (ssAlt in Shift) then
      Result := VertScrollBar.DoMouseWheel(Shift, WheelDelta, MousePos);
    if not Result then
      Result := HorzScrollBar.DoMouseWheel(Shift, WheelDelta, MousePos);
  end
  else
  begin
    if WheelDelta > 0 then
      zoomBy(1.11111111111111111111111, ScreenToClient(Mouse.CursorPos))
    else
      zoomBy(0.9, ScreenToClient(Mouse.CursorPos));
    Result := true;
  end;
end;

//- BG ----------------------------------------------------------- 12.11.2006 --
procedure TBegaZoomingScrollBox.doZoomBy(ZoomBy: Double; Around: TPoint);
begin
  if assigned(FOnZoomBy) then
    FOnZoomBy(self, FZoom);
end;

//- BG ----------------------------------------------------------- 12.11.2006 --
procedure TBegaZoomingScrollBox.doZoomTo(ZoomTo: Double; Width, Height,
  BoxWidth, BoxHeight: Integer);
begin
  if assigned(FOnZoomTo) then
    FOnZoomTo(self, FZoom);
end;

//- BG ----------------------------------------------------------- 23.01.2006 --
procedure TBegaZoomingScrollBox.zoomBy(Zoom: Double; Around: TPoint);
begin
  FZoom := FZoom * Zoom;
  doZoomBy(Zoom, Around);
end;

//- BG ----------------------------------------------------------- 23.01.2006 --
procedure TBegaZoomingScrollBox.zoomTo(Zoom: Double; Width, Height: Integer);
var
  BoxWidth, BoxHeight: Integer;
begin
  FZoom := Zoom;
  // calc width
  Width := trunc(FZoom * Width);
  BoxWidth := HorzScrollBar.ControlSize(csWithoutBar);
  if Width > BoxWidth then
    BoxWidth := HorzScrollBar.ControlSize(csWithBar);
  // calc height
  Height := trunc(FZoom * Height);
  BoxHeight := VertScrollBar.ControlSize(csWithoutBar);
  if Height > BoxHeight then
    BoxHeight := VertScrollBar.ControlSize(csWithBar);
  // do zoom
  doZoomTo(FZoom, Width, Height, BoxWidth, BoxHeight);
end;

//- BG ----------------------------------------------------------- 23.01.2006 --
procedure TBegaZoomingScrollBox.zoomToFitIntoBox(Width, Height: Integer);
var
  BoxWidth, BoxHeight: Integer;
begin
  if (Width > 0) and (Height > 0) then
  begin
    BoxWidth := HorzScrollBar.ControlSize(csWithoutBar);
    BoxHeight := VertScrollBar.ControlSize(csWithoutBar);
    FZoom := min(BoxWidth/Width, BoxHeight/Height);
    Width := trunc(FZoom * Width);
    Height := trunc(FZoom * Height);
    doZoomTo(FZoom, Width, Height, BoxWidth, BoxHeight);
  end;
end;

{ TBegaZoomBox }

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaZoomBox.doZoomBy(ZoomBy: Double; Around: TPoint);
begin
  inherited;
  begaZoomControlAroundPoint(ZoomedControl, ZoomBy, Around);
  if ZoomedControl is TImage then
    TImage(ZoomedControl).Stretch := True;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaZoomBox.doZoomTo(ZoomTo: Double; Width, Height, BoxWidth, BoxHeight: Integer);
begin
  inherited;
  if ZoomedControl is TImage then
    TImage(ZoomedControl).Stretch := ZoomTo <> 1.0;
  ZoomedControl.setBounds(
    (BoxWidth - Width) div 2,
    (BoxHeight - Height) div 2,
    Width, Height);
end;

//- BG ----------------------------------------------------------- 23.01.2006 --
function TBegaZoomBox.getZoomedControl: TControl;
begin
  if ControlCount > 0 then
    Result := Controls[0]
  else
    Result := nil;
end;

{$endif NoFlatScrollbars}
end.
