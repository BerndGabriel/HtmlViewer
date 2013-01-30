{-------------------------------------------------------------------------------
$File$, (C) 2006..2007 by Bernd Gabriel. All rights reserved.
--------------------------------------------------------------------------------
Author       : $Author: Bernd $
Last CheckIn : $Date: 2007/01/18 22:57:51 $
Revision     : $Revision: 1.5 $

mailto: info@fast-function-factory.de

Log:
====

$Log: BegaSplitter.pas,v $
Revision 1.5  2007/01/18 22:57:51  Bernd
no message

Revision 1.4  2007/01/14 20:43:02  Bernd
TBegaSplitter (V) 2.0

Revision 1.3  2007/01/11 00:42:37  Bernd
no message

Revision 1.2  2007/01/10 22:18:14  Bernd
no message

Revision 1.1  2006/12/24 15:25:37  Bernd
no message

-------------------------------------------------------------------------------}

unit BegaSplitter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Math,
  // own units
  BegaObjects;

const
  cVersion = '(V) 2.0, 12/2006, 01/2007';
  cMinHandledSize = 7;

type

//------------------------------------------------------------------------------
// BG, 14.01.2007: modified copy of Delphi6-TSplitter
//------------------------------------------------------------------------------
  TBegaCustomSplitter = class(TGraphicControl)
  private
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMaxSize: Integer;
    FMinSize: NaturalNumber;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
    procedure updateNeighborhood; virtual;
    property Align default alLeft;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsPattern;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------
// BG, 14.01.2007: splitter with button to quickly hide one neighbor.
//------------------------------------------------------------------------------

  TBegaArrowOrientation = (aoLeft, aoRight, aoUp, aoDown);
  TBegaSplitterButtonAlign = (baLeftTop, baCenter, baRightBottom);
  TBegaSplitterButtonKind = (bkNone, bkNormal, bkFlat);

  TBegaSplitter = class(TBegaCustomSplitter)
  private
    FButtonAlign: TBegaSplitterButtonAlign;
    FButtonKind: TBegaSplitterButtonKind;
    FButtonSize: Integer;
    FSplitCursor: TCursor;
    FControl: TControl;
    FOldControlSize: Integer;
    procedure setButtonAlign(const Value: TBegaSplitterButtonAlign);
    procedure setButtonKind(const Value: TBegaSplitterButtonKind);
    procedure setButtonSize(const Value: Integer);
    function ButtonRect: TRect;
    procedure updateCursor(const Pt: TPoint);
    function getClosed: Boolean;
    procedure setClosed(const Value: Boolean);
  protected
    function isInButton(const Pt: TPoint): Boolean;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure updateNeighborhood; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Closed: Boolean read getClosed write setClosed default False;
  published
    property Align;
    property AutoSnap;
    property Beveled;
    property Color;
    property Constraints;
    property Enabled;
    property ButtonAlign: TBegaSplitterButtonAlign read FButtonAlign write setButtonAlign default baLeftTop;
    property ButtonKind: TBegaSplitterButtonKind read FButtonKind write setButtonKind default bkNormal;
    property ButtonSize: Integer read FButtonSize write setButtonSize default 80;
    property MinSize;
    property OnCanResize;
    property OnMoved;
    property OnPaint;
    property ParentColor;
    property ResizeStyle;
    property Visible;
  end;

implementation

uses Types;

{ TBegaCustomSplitter }

type
  TWinControlAccess = class(TWinControl);

constructor TBegaCustomSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Align := alLeft;
  Width := 7;
  Cursor := crHSplit;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
end;

destructor TBegaCustomSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TBegaCustomSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TBegaCustomSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TBegaCustomSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

function TBegaCustomSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TBegaCustomSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if Align in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TBegaCustomSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
//  if csDesigning in ComponentState then
//    { Draw outline }
//    with Canvas do
//    begin
//      Pen.Style := psDot;
//      Pen.Mode := pmXor;
//      Pen.Color := XorColor;
//      Brush.Style := bsClear;
//      Rectangle(0, 0, ClientWidth, ClientHeight);
//    end;
  FLineVisible := False;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

function TBegaCustomSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

function TBegaCustomSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Enabled then
    if Button = mbLeft then
    begin
      updateNeighborhood;
      FDownPos := Point(X, Y);
      if Assigned(FControl) then
      begin
        if Align in [alLeft, alRight] then
        begin
          FMaxSize := Parent.ClientWidth - FMinSize;
          for I := 0 to Parent.ControlCount - 1 do
            with Parent.Controls[I] do
              if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
          Inc(FMaxSize, FControl.Width);
        end
        else
        begin
          FMaxSize := Parent.ClientHeight - FMinSize;
          for I := 0 to Parent.ControlCount - 1 do
            with Parent.Controls[I] do
              if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
          Inc(FMaxSize, FControl.Height);
        end;
        UpdateSize(X, Y);
        AllocateLineDC;
        with ValidParentForm(Self) do
          if ActiveControl <> nil then
          begin
            FActiveControl := ActiveControl;
            FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
            TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
          end;
        if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      end;
    end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    Parent.DisableAlign;
    try
      case Align of
        alLeft:
          begin
            FControl.Width := FNewSize;
            Left := FControl.Width;
          end;
        alTop:
          begin
            FControl.Height := FNewSize;
            Top := FControl.Height;
          end;
        alRight:
          begin
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
            Left := max(FControl.Left - 1, 0);
          end;
        alBottom:
          begin
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
            Top := max(FControl.Top, -1);
          end;
      end;
    finally
      Parent.EnableAlign;
    end;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TBegaCustomSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TBegaCustomSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if Enabled then
    if (ssLeft in Shift) and Assigned(FControl) then
    begin
      CalcSplitSize(X, Y, NewSize, Split);
      if DoCanResize(NewSize) then
      begin
        if ResizeStyle in [rsLine, rsPattern] then DrawLine;
        FNewSize := NewSize;
        FSplit := Split;
        if ResizeStyle = rsUpdate then UpdateControlSize;
        if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      end;
    end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Enabled then
    if Assigned(FControl) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      UpdateControlSize;
      StopSizing;
    end;
end;

procedure TBegaCustomSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TBegaCustomSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  case Operation of
    opRemove:
      if FControl = AComponent then
        FControl := nil;
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaCustomSplitter.updateNeighborhood;
begin
  if csLoading in ComponentState then
    exit;
  FControl := FindControl;
end;

{ TBegaSplitter}

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure calcAlignment(
  AlignToStart, AlignToEnd, AlignedSize: Integer;
  Align: TBegaSplitterButtonAlign;
  out AlignedStart, AlignedEnd: Integer);
begin
  case Align of
    baLeftTop:      AlignedStart := AlignToStart;
    baCenter:       AlignedStart := max((AlignToStart + AlignToEnd - AlignedSize) div 2, AlignToStart);
    baRightBottom:  AlignedStart := max(AlignToEnd - AlignedSize, AlignToStart);
  end;
  AlignedEnd := min(AlignedStart + AlignedSize, AlignToEnd);
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
function TBegaSplitter.ButtonRect: TRect;
begin
  case Align of
    alTop, alBottom:
    begin
      Result.Top := 0;
      Result.Bottom := Height;
      calcAlignment(0, Width, ButtonSize, ButtonAlign, Result.Left, Result.Right);
    end;
    alLeft, alRight:
    begin
      Result.Left := 0;
      Result.Right := Width;
      calcAlignment(0, Height, ButtonSize, ButtonAlign, Result.Top, Result.Bottom);
    end;
  else
    calcAlignment(0, Width, ButtonSize, ButtonAlign, Result.Left, Result.Right);
    calcAlignment(0, Height, ButtonSize, ButtonAlign, Result.Top, Result.Bottom);
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
constructor TBegaSplitter.Create(AOwner: TComponent);
begin
  inherited;
  FButtonAlign := baLeftTop;
  FButtonKind  := bkNormal;
  FButtonSize  := 80;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
function TBegaSplitter.getClosed: Boolean;
begin
  Result := FOldSize = 0;
  if Parent <> nil then
    case Align of
      alLeft    : Result := Left <= 0;
      alRight   : Result := Left + Width >= Parent.Width;
      alTop     : Result := Top <= 0;
      alBottom  : Result := Top + Height >= Parent.Height;
    end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
function TBegaSplitter.isInButton(const Pt: TPoint): Boolean;
begin
  if ButtonKind <> bkNone then
    Result := PtInRect(ButtonRect, Pt)
  else
    Result := False;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.Loaded;
begin
  inherited;
  FSplitCursor := Cursor;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  updateCursor(Point(X,Y));
  inherited;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
    if isInButton(Point(X,Y)) then
      Closed := not Closed;
  inherited;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.Paint;

  procedure paintArrow(Rect: TRect; PointTo: TBegaArrowOrientation);
  var
    Size: Integer;
    Center: TPoint;
  begin
    Size := (min(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top) + 1) div 2;
    if Size >= 1 then
    begin
      Center.X := (Rect.Right + Rect.Left) div 2;
      Center.Y := (Rect.Top + Rect.Bottom) div 2;
      Rect.Left := Center.X - Size;
      Rect.Right := Center.X + Size;
      Rect.Top := Center.Y - Size;
      Rect.Bottom := Center.Y + Size;
      with Canvas do
      begin
        Pen.Color := clBtnText;
        Brush.Color := clBtnText;
        case PointTo of
          aoLeft:
            Polygon([
              Point(Rect.Right, Rect.Top - 1),
              Point(Rect.Right, Rect.Bottom + 1),
              Point(Rect.Left, Center.Y)]);
          aoRight:
            Polygon([
              Point(Rect.Left, Rect.Top - 1),
              Point(Rect.Left, Rect.Bottom + 1),
              Point(Rect.Right, Center.Y)]);
          aoUp:
            Polygon([
              Point(Rect.Left - 1, Rect.Bottom),
              Point(Rect.Right + 1, Rect.Bottom),
              Point(Center.X, Rect.Top)]);
          aoDown:
            Polygon([
              Point(Rect.Left - 1, Rect.Top),
              Point(Rect.Right + 1, Rect.Top),
              Point(Center.X, Rect.Bottom)]);
        end;
        Brush.Color := Color;
      end;
    end;
  end;

var
  Rc: TRect;
begin
  inherited;
  if ButtonKind <> bkNone then
  begin
    Rc := ButtonRect;
    if (Rc.Left < Rc.Right) and (Rc.Top < Rc.Bottom) then
//      if (csDesigning in ComponentState) and not Enabled then
//        with Canvas do
//        begin
//          InflateRect(Rc, -1, -1);
//          Rectangle(Rc);
//        end
//      else
      if Enabled then
        with Canvas do
        begin
          Brush.Color := Color;
          Brush.Style := bsSolid;
          Pen.Mode := pmCopy;
          Pen.Style := psSolid;
          if ButtonKind = bkFlat then
          begin
            dec(Rc.Right);
            dec(Rc.Bottom);
            Pen.Color := clBtnHighlight;
            Polyline([Point(Rc.Left, Rc.Bottom), Point(Rc.Right, Rc.Bottom), Point(Rc.Right, Rc.Top)]);
            Pen.Color := clBtnShadow;
            Polyline([Point(Rc.Right, Rc.Top), Point(Rc.Left, Rc.Top), Point(Rc.Left, Rc.Bottom)]);
            inc(Rc.Left);
            inc(Rc.Top);
          end;
          Pen.Color := clBtnHighlight;
          Rectangle(Rc);
          dec(Rc.Right);
          dec(Rc.Bottom);
          Pen.Color := clBtnShadow;
          Polyline([Point(Rc.Left, Rc.Bottom), Point(Rc.Right, Rc.Bottom), Point(Rc.Right, Rc.Top)]);
          InflateRect(Rc, -2, -2);
          if Closed then
            case Align of
              alTop: paintArrow(Rc, aoDown);
              alBottom: paintArrow(Rc, aoUp);
              alLeft: paintArrow(Rc, aoRight);
              alRight: paintArrow(Rc, aoLeft);
            end
          else
            case Align of
              alTop: paintArrow(Rc, aoUp);
              alBottom: paintArrow(Rc, aoDown);
              alLeft: paintArrow(Rc, aoLeft);
              alRight: paintArrow(Rc, aoRight);
            end;
        end;
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.setButtonAlign(const Value: TBegaSplitterButtonAlign);
begin
  if FButtonAlign <> Value then
  begin
    FButtonAlign := Value;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.setButtonKind(const Value: TBegaSplitterButtonKind);
begin
  if FButtonKind <> Value then
  begin
    FButtonKind := Value;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.setButtonSize(const Value: Integer);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.setClosed(const Value: Boolean);
begin
  if Value <> Closed then
  begin
    if Value then
    begin
      //close
      UpdateNeighborhood;
      if FOldSize = -1 then
      begin
        // first time
        if FControl = nil then
          case Align of
            alLeft   : FOldSize := Left;
            alRight  :
              if Parent <> nil then
                FOldSize := Parent.Width - Left - Width
              else
                FOldSize := 30;
            alTop    : FOldSize := Top;
            alBottom :
              if Parent <> nil then
                FOldSize := Parent.Height - Top - Height
              else
                FOldSize := 30;
          end
        else
          case Align of
            alTop, alBottom:
              FOldSize := FControl.Height;
            alLeft, alRight:
              FOldSize := FControl.Width;
          end;
      end;
      FOldControlSize := FOldSize;
      FNewSize := 0;
    end
    else
    begin
      //open;
      FNewSize := FOldControlSize;
    end;
    UpdateControlSize;
  end;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.updateCursor(const Pt: TPoint);
begin
  if isInButton(Pt) then
    Cursor := crDefault
  else
    Cursor := FSplitCursor;
end;

//- BG ----------------------------------------------------------- 14.01.2007 --
procedure TBegaSplitter.updateNeighborhood;
begin
  if not Closed then
    inherited;
end;

end.
