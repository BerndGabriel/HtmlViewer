{-------------------------------------------------------------------------------
Copyright (C) 2006-2014 by Bernd Gabriel.

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

unit BegaScrollBox;

interface
{$ifndef NoFlatScrollbars}

uses
{$ifdef UseVCLStyles}
  Vcl.Themes,
{$endif}
  Classes, SysUtils, Messages,
{$ifdef LCL}
  LclIntf, LclType, LMessages,
  HtmlMisc,
{$else}
  FlatSB,
{$endif LCL}
{$ifdef MSWindows}
  Windows,
  CommCtrl,
  MultiMon,
{$endif MSWindows}
  Controls, ExtCtrls, Forms, Graphics, Math, Menus,
  TypInfo;

type
  TBegaScrollingWinClientSize = (csCurrent, csWithoutBar, csWithBar);

  TBegaScrollingWinControl = class;

  TBegaScrollBar = class(TPersistent)
  private
    FControl: TBegaScrollingWinControl;
    FIncrement: TScrollBarInc;
    FPageIncrement: TScrollbarInc;
    //
    FRange: Integer;
    FPosition: Integer;
    FMaxPos: Integer;
    //
    FKind: TScrollBarKind;
    FMargin: Word;
    FVisible: Boolean;
    FTracking: Boolean;
    FScaled: Boolean;
    FSmooth: Boolean;
    FDelay: Integer;
    FButtonSize: Integer;
    FColor: TColor;
    FParentColor: Boolean;
    FSize: Integer;
    FStyle: TScrollBarStyle;
    FThumbSize: Integer;
    FPageDiv: Integer;
    FLineDiv: Integer;
    FUpdateNeeded: Boolean;
    constructor Create(AControl: TBegaScrollingWinControl; AKind: TScrollBarKind);
    procedure CalcAutoRange;
    function ControlSize(ControlSB, AssumeSB: Boolean): Integer; overload;
    procedure DoSetRange(Value: Integer);
    function GetScrollPos: Integer;
    function NeedsScrollBarVisible: Boolean;
    function IsIncrementStored: Boolean;
    procedure ScrollMessage(var Msg: TWMScroll);
    procedure SetButtonSize(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetParentColor(Value: Boolean);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TScrollBarStyle);
    procedure SetThumbSize(Value: Integer);
    procedure SetVisible(Value: Boolean);
    function IsRangeStored: Boolean;
    procedure Update(ControlSB, AssumeSB: Boolean);
  public
    function ControlSize(Which: TBegaScrollingWinClientSize): Integer; overload;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function IsScrollBarVisible: Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure ChangeBiDiPosition;
    property Kind: TScrollBarKind read FKind;
    property ScrollPos: Integer read GetScrollPos;
  published
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 0;
    property Color: TColor read FColor write SetColor default clBtnHighlight;
    property Increment: TScrollBarInc read FIncrement write FIncrement stored IsIncrementStored default 8;
    property Margin: Word read FMargin write FMargin default 0;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property Range: Integer read FRange write SetRange stored IsRangeStored default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property Smooth: Boolean read FSmooth write FSmooth default False;
    property Size: Integer read FSize write SetSize default 0;
    property Style: TScrollBarStyle read FStyle write SetStyle default ssRegular;
    property ThumbSize: Integer read FThumbSize write SetThumbSize default 0;
    property Tracking: Boolean read FTracking write FTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TBegaScrollingWinControl = class(TWinControl)
   {$ifdef UseVCLStyles}
  strict private
    class destructor Destroy;
    class constructor Create;
   {$endif}
  private
    FAutoRangeCount: Integer;
    FAutoScroll: Boolean;
    FHorzScrollBar: TBegaScrollBar;
    FUpdatingScrollBars: Boolean;
    FVertScrollBar: TBegaScrollBar;
    FPanningEnabled: Boolean;
    FPanning: Boolean;
    FPanningStart: TPoint;
    procedure CalcAutoRange;
    procedure ScaleScrollBars(M, D: Integer);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetHorzScrollBar(Value: TBegaScrollBar);
    procedure SetVertScrollBar(Value: TBegaScrollBar);
    procedure UpdateScrollBars;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    function GetMaxScroll: TPoint;
  protected
    function AutoScrollEnabled: Boolean; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure AutoScrollInView(AControl: TControl); virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoFlipChildren; override;
    procedure Resizing(State: TWindowState); virtual;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property PanningEnabled: Boolean read FPanningEnabled write FPanningEnabled default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isPanning: Boolean; virtual;
    procedure DisableAutoRange;
    procedure EnableAutoRange;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ScrollInView(AControl: TControl);
  published
    property HorzScrollBar: TBegaScrollBar read FHorzScrollBar write SetHorzScrollBar;
    property VertScrollBar: TBegaScrollBar read FVertScrollBar write SetVertScrollBar;
  end;
 {$ifdef UseVCLStyles}
 TBegaScrollingWinControlStyleHook = class(TScrollingStyleHook)
  strict protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

 {$endif}
  TBegaScrollBox = class(TBegaScrollingWinControl)
  private
    FBorderStyle: TBorderStyle;
    procedure SetBorderStyle(Value: TBorderStyle);
//    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
{$ifndef LCL}
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
{$endif LCL}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
{$ifndef LCL}
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
{$endif LCL}
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
{$ifndef LCL}
    property Ctl3D;
{$endif LCL}
    property Font;
    property PanningEnabled;
    property ParentBiDiMode;
    property ParentColor;
{$ifndef LCL}
    property ParentCtl3D;
{$endif LCL}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{$ifndef LCL}
    property OnCanResize;
{$endif LCL}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  EBegaScrollBoxEnumException = class(Exception)
  public
    constructor createEnum(EnumType: PTypeInfo; const Value);
  end;

{$endif NoFlatScrollbars}
implementation
{$ifndef NoFlatScrollbars}

 {$ifdef UseVCLStyles}
 uses
   {$ifndef HasSystemUITypes}
   Vcl.Styles,
   {$endif}
   System.UITypes;
 {$endif}

{ TBegaScrollBar }

constructor TBegaScrollBar.Create(AControl: TBegaScrollingWinControl; AKind: TScrollBarKind);
begin
  inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPageIncrement := 80;
  FIncrement := FPageIncrement div 10;
  FVisible := True;
  FDelay := 10;
  FLineDiv := 4;
  FPageDiv := 12;
  FColor := clBtnHighlight;
  FParentColor := True;
  FUpdateNeeded := True;
end;

function TBegaScrollBar.IsIncrementStored: Boolean;
begin
  Result := not Smooth;
end;

procedure TBegaScrollBar.Assign(Source: TPersistent);
begin
  if Source is TBegaScrollBar then
  begin
    Visible := TBegaScrollBar(Source).Visible;
    Range := TBegaScrollBar(Source).Range;
    Position := TBegaScrollBar(Source).Position;
    Increment := TBegaScrollBar(Source).Increment;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TBegaScrollBar.ChangeBiDiPosition;
begin
  if Kind = sbHorizontal then
    if IsScrollBarVisible then
      if not FControl.UseRightToLeftScrollBar then
        Position := 0
      else
        Position := Range;
end;

procedure TBegaScrollBar.CalcAutoRange;
var
  I: Integer;
  NewMin, NewWidth: Integer;

  procedure processLeftTop(ALeftTop, AWidthHeight: Integer);
  var
    Size: Integer;
  begin
    Size := AWidthHeight + math.max(ALeftTop, 0);
    if NewMin   > ALeftTop then NewMin   := ALeftTop;
    if NewWidth < Size     then NewWidth := Size;
  end;

  procedure processRightBottom(AWidthHeight: Integer);
  begin
    if NewWidth < AWidthHeight then NewWidth := AWidthHeight;
  end;

  procedure ProcessHorz(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alLeft:  processLeftTop(Control.Left, Control.Width);
        alRight: processRightBottom(Control.Width);
        alNone:
          if Control.Anchors * [akLeft, akRight] = [akLeft] then
            processLeftTop(Control.Left, Control.Width)
          else if Control.Anchors * [akLeft, akRight] = [akRight] then
            processRightBottom(Control.Width);
      end;
  end;

  procedure ProcessVert(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alTop:  processLeftTop(Control.Top, Control.Height);
        alBottom: processRightBottom(Control.Height);
        alNone:
          if Control.Anchors * [akTop, akBottom] = [akTop] then
            processLeftTop(Control.Top, Control.Height)
          else if Control.Anchors * [akTop, akBottom] = [akBottom] then
            processRightBottom(Control.Height);
      end;
  end;
var
  NeedScrollBar: Boolean;
  MaxSize, NewSize, NewMax: Integer;
begin
  if FControl.FAutoScroll then
  begin
    if FControl.AutoScrollEnabled then
    begin
      MaxSize := ControlSize(False, False);
      NewMin := 0;
      NewWidth := MaxSize;
      for I := 0 to FControl.ControlCount - 1 do
        if Kind = sbHorizontal then
          ProcessHorz(FControl.Controls[I])
        else
          ProcessVert(FControl.Controls[I]);
      if NewMin > 0 then
        NewMin := 0;
      NewMax := NewMin + NewWidth;
      NeedScrollBar := (NewMin < 0) or (NewMax > MaxSize);
      NewSize := ControlSize(False, NeedScrollBar);
      if NewMax < NewSize then
        NewMax := NewSize;
      FPosition := -NewMin;
      DoSetRange(NewMax - NewMin);
    end
    else
      DoSetRange(0);
  end;
end;

function TBegaScrollBar.IsScrollBarVisible: Boolean;
var
  Style: Longint;
begin
  Style := WS_HSCROLL;
  if Kind = sbVertical then Style := WS_VSCROLL;
  Result := (Visible) and
            (GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0);
end;

function TBegaScrollBar.ControlSize(ControlSB, AssumeSB: Boolean): Integer;
var
  BorderAdjust: Integer;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Style: Longint;
  begin
    Style := WS_HSCROLL;
    if Code = SB_VERT then Style := WS_VSCROLL;
    Result := GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0;
  end;

  function Adjustment(Code, Metric: Word): Integer;
  begin
    Result := 0;
    if not ControlSB then
      if AssumeSB and not ScrollBarVisible(Code) then
        Result := -(GetSystemMetrics(Metric) - BorderAdjust)
      else if not AssumeSB and ScrollBarVisible(Code) then
        Result := GetSystemMetrics(Metric) - BorderAdjust;
  end;

begin
  BorderAdjust := Integer(GetWindowLong(FControl.Handle, GWL_STYLE) and (WS_BORDER or WS_THICKFRAME) <> 0);
  if Kind = sbVertical then
    Result := FControl.ClientHeight + Adjustment(SB_HORZ, SM_CXHSCROLL)
  else
    Result := FControl.ClientWidth + Adjustment(SB_VERT, SM_CYVSCROLL);
end;

//- BG ----------------------------------------------------------- 22.01.2006 --
function TBegaScrollBar.ControlSize(
  Which: TBegaScrollingWinClientSize): Integer;
begin
  case Which of
    csCurrent:    Result := ControlSize(True, False);
    csWithoutBar: Result := ControlSize(False, False);
    csWithBar:    Result := ControlSize(False, True);
  else
    raise EBegaScrollBoxEnumException.createEnum(TypeInfo(TBegaScrollingWinClientSize), Which);
  end;
end;

function TBegaScrollBar.GetScrollPos: Integer;
begin
  Result := 0;
  if Visible then Result := Position;
end;

function TBegaScrollBar.NeedsScrollBarVisible: Boolean;
begin
  Result := FRange > ControlSize(False, False);
end;

procedure TBegaScrollBar.ScrollMessage(var Msg: TWMScroll);
var
  Incr, FinalIncr, Count: Integer;
  CurrentTime, StartTime, ElapsedTime: Longint;

  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    if FKind = sbVertical then Code := SB_VERT;
    Result := Msg.Pos;
    if FlatSB_GetScrollInfo(FControl.Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  with Msg do
  begin
    if FSmooth and (ScrollCode in [SB_LINEUP, SB_LINEDOWN, SB_PAGEUP, SB_PAGEDOWN]) then
    begin
      case ScrollCode of
        SB_LINEUP, SB_LINEDOWN:
          begin
            Incr := FIncrement div FLineDiv;
            FinalIncr := FIncrement mod FLineDiv;
            Count := FLineDiv;
          end;
        SB_PAGEUP, SB_PAGEDOWN:
          begin
            Incr := FPageIncrement;
            FinalIncr := Incr mod FPageDiv;
            Incr := Incr div FPageDiv;
            Count := FPageDiv;
          end;
      else
        Count := 0;
        Incr := 0;
        FinalIncr := 0;
      end;
      CurrentTime := 0;
      while Count > 0 do
      begin
        StartTime := GetCurrentTime;
        ElapsedTime := StartTime - CurrentTime;
        if ElapsedTime < FDelay then Sleep(FDelay - ElapsedTime);
        CurrentTime := StartTime;
        case ScrollCode of
          SB_LINEUP: SetPosition(FPosition - Incr);
          SB_LINEDOWN: SetPosition(FPosition + Incr);
          SB_PAGEUP: SetPosition(FPosition - Incr);
          SB_PAGEDOWN: SetPosition(FPosition + Incr);
        end;
        FControl.Update;
        Dec(Count);
      end;
      if FinalIncr > 0 then
      begin
        case ScrollCode of
          SB_LINEUP: SetPosition(FPosition - FinalIncr);
          SB_LINEDOWN: SetPosition(FPosition + FinalIncr);
          SB_PAGEUP: SetPosition(FPosition - FinalIncr);
          SB_PAGEDOWN: SetPosition(FPosition + FinalIncr);
        end;
      end;
    end
    else
      case ScrollCode of
        SB_LINEUP: SetPosition(FPosition - FIncrement);
        SB_LINEDOWN: SetPosition(FPosition + FIncrement);
        SB_PAGEUP: SetPosition(FPosition - ControlSize(True, False));
        SB_PAGEDOWN: SetPosition(FPosition + ControlSize(True, False));
        SB_THUMBPOSITION:
            if FRange > 32767 then
              SetPosition(GetRealScrollPosition) else
              SetPosition(Pos);
        SB_THUMBTRACK:
          if Tracking then
            if FRange > 32767 then
              SetPosition(GetRealScrollPosition) else
              SetPosition(Pos);
        SB_TOP: SetPosition(0);
        SB_BOTTOM: SetPosition(FMaxPos);
        SB_ENDSCROLL: begin end;
      end;
  end;
end;

procedure TBegaScrollBar.SetButtonSize(Value: Integer);
const
  SysConsts: array[TScrollBarKind] of Integer = (SM_CXHSCROLL, SM_CXVSCROLL);
var
  NewValue: Integer;
begin
  if Value <> ButtonSize then
  begin
    NewValue := Value;
    if NewValue = 0 then
      Value := GetSystemMetrics(SysConsts[Kind]);
    FButtonSize := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
    if NewValue = 0 then
      FButtonSize := 0;
  end;
end;

procedure TBegaScrollBar.SetColor(Value: TColor);
begin
  if Value <> Color then
  begin
    FColor := Value;
    FParentColor := False;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
  end;
end;

procedure TBegaScrollBar.SetParentColor(Value: Boolean);
begin
  if ParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then Color := clBtnHighlight;
  end;
end;

procedure TBegaScrollBar.SetPosition(Value: Integer);
var
  Code: Word;
  Form: TCustomForm;
  Delta: Integer;
begin
  if csReading in FControl.ComponentState then
    FPosition := Value
  else
  begin
    if Value < 0 then
      Value := 0
    else if Value > FMaxPos then
      Value := FMaxPos;
    if Kind = sbHorizontal then
      Code := SB_HORZ
    else
      Code := SB_VERT;
    if Value <> FPosition then
    begin
      Delta := FPosition - Value;
      if Kind = sbHorizontal then
        FControl.ScrollBy(Delta, 0)
      else
        FControl.ScrollBy(0, Delta);
      if csDesigning in FControl.ComponentState then
      begin
        Form := GetParentForm(FControl);
        if (Form <> nil) and (Form.Designer <> nil) then
          Form.Designer.Modified;
      end;
      FPosition := Value;
    end;
    if FlatSB_GetScrollPos(FControl.Handle, Code) <> FPosition then
      FlatSB_SetScrollPos(FControl.Handle, Code, FPosition, True);
  end;
end;

procedure TBegaScrollBar.SetSize(Value: Integer);
const
  SysConsts: array[TScrollBarKind] of Integer = (SM_CYHSCROLL, SM_CYVSCROLL);
var
  NewValue: Integer;
begin
  if Value <> Size then
  begin
    NewValue := Value;
    if NewValue = 0 then
      Value := GetSystemMetrics(SysConsts[Kind]);
    FSize := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
    if NewValue = 0 then
      FSize := 0;
  end;
end;

procedure TBegaScrollBar.SetStyle(Value: TScrollBarStyle);
begin
  if Style <> Value then
  begin
    FStyle := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
  end;
end;

procedure TBegaScrollBar.SetThumbSize(Value: Integer);
begin
  if Value <> ThumbSize then
  begin
    FThumbSize := Value;
    FUpdateNeeded := True;
    FControl.UpdateScrollBars;
  end;
end;

procedure TBegaScrollBar.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then
    FRange := 0;
  FControl.UpdateScrollBars;
end;

function TBegaScrollBar.IsRangeStored: Boolean;
begin
  Result := not FControl.AutoScroll;
end;

procedure TBegaScrollBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FControl.UpdateScrollBars;
end;

procedure TBegaScrollBar.Update(ControlSB, AssumeSB: Boolean);
type
  TPropKind = (pkStyle, pkButtonSize, pkThumbSize, pkSize, pkBkColor);
const
  Props: array[TScrollBarKind, TPropKind] of Integer = (
    { Horizontal }
    (WSB_PROP_HSTYLE, WSB_PROP_CXHSCROLL, WSB_PROP_CXHTHUMB, WSB_PROP_CYHSCROLL,
     WSB_PROP_HBKGCOLOR),
    { Vertical }
    (WSB_PROP_VSTYLE, WSB_PROP_CYVSCROLL, WSB_PROP_CYVTHUMB, WSB_PROP_CXVSCROLL,
     WSB_PROP_VBKGCOLOR));
//  Kinds: array[TScrollBarKind] of Integer = (WSB_PROP_HSTYLE, WSB_PROP_VSTYLE);
  Styles: array[TScrollBarStyle] of Integer = (FSB_REGULAR_MODE,
    FSB_ENCARTA_MODE, FSB_FLAT_MODE);
  Code: array[TScrollBarKind] of Word = (SB_HORZ, SB_VERT);
var
  ScrollInfo: TScrollInfo;

  procedure UpdateScrollProperties(Redraw: Boolean);
  begin
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkStyle], Styles[Style], Redraw);
    if ButtonSize > 0 then
      FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkButtonSize], ButtonSize, False);
    if ThumbSize > 0 then
      FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkThumbSize], ThumbSize, False);
    if Size > 0 then
      FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkSize], Size, False);
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkBkColor],
      ColorToRGB(Color), False);
  end;
var
  BarSize: Integer;
begin
  if Visible then
  begin
    BarSize := ControlSize(ControlSB, AssumeSB);
    if BarSize > FRange then
      BarSize := FRange;
  end
  else
    BarSize := FRange;
  FMaxPos := FRange - BarSize;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  if FMaxPos > 0 then
    ScrollInfo.nMax := FRange
  else
    ScrollInfo.nMax := 0;
  ScrollInfo.nPage := BarSize + 1;
  ScrollInfo.nPos := FPosition;
  ScrollInfo.nTrackPos := FPosition;
  UpdateScrollProperties(FUpdateNeeded);
  FUpdateNeeded := False;
  FlatSB_SetScrollInfo(FControl.Handle, Code[Kind], ScrollInfo, True);
  SetPosition(FPosition);
  FPageIncrement := (ControlSize(True, False) * 9) div 10;
  if Smooth then FIncrement := FPageIncrement div 10;
end;

//- BG ----------------------------------------------------------- 21.01.2006 --
procedure TBegaScrollBar.SetRange(Value: Integer);
begin
  FControl.FAutoScroll := False;
  FScaled := True;
  DoSetRange(Value);
end;

//- BG ----------------------------------------------------------- 22.02.2006 --
function TBegaScrollBar.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := Visible;
  Position := Position - WheelDelta;
end;

{ TBegaScrollingWinControl }

constructor TBegaScrollingWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorzScrollBar := TBegaScrollBar.Create(Self, sbHorizontal);
  FVertScrollBar := TBegaScrollBar.Create(Self, sbVertical);
  FAutoScroll := True;
  FPanningEnabled := True;
end;

destructor TBegaScrollingWinControl.Destroy;
begin
  FHorzScrollBar.Free;
  FVertScrollBar.Free;
  inherited Destroy;
end;

procedure TBegaScrollingWinControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TBegaScrollingWinControl.CreateWnd;
begin
  inherited CreateWnd;
  //! Scroll bars don't move to the Left side of a TBegaScrollingWinControl when the
  //! WS_EX_LeftScrollBar flag is set and InitializeFlatSB is called.
  //! A call to UnInitializeFlatSB does nothing.
  if not SysLocale.MiddleEast and
     not CheckWin32Version(5, 1) then
    InitializeFlatSB(Handle);
  UpdateScrollBars;
end;

procedure TBegaScrollingWinControl.AlignControls(AControl: TControl; var ARect: TRect);
begin
  CalcAutoRange;
  inherited AlignControls(AControl, ARect);
end;

function TBegaScrollingWinControl.AutoScrollEnabled: Boolean;
begin
  Result := not AutoSize and not (DockSite and UseDockManager);
end;

procedure TBegaScrollingWinControl.DoFlipChildren;
var
  Loop: Integer;
  TheWidth: Integer;
  ScrollBarActive: Boolean;
  FlippedList: TList;
begin
  FlippedList := TList.Create;
  try
    TheWidth := ClientWidth;
    with HorzScrollBar do begin
      ScrollBarActive := (IsScrollBarVisible) and (TheWidth < Range);
      if ScrollBarActive then
      begin
        TheWidth := Range;
        Position := 0;
      end;
    end;

    for Loop := 0 to ControlCount - 1 do with Controls[Loop] do
    begin
      FlippedList.Add(Controls[Loop]);
      Left := TheWidth - Width - Left;
    end;

    { Allow controls that have associations to realign themselves }
    for Loop := 0 to FlippedList.Count - 1 do
      TControl(FlippedList[Loop]).Perform(CM_ALLCHILDRENFLIPPED, 0, 0);

    if ScrollBarActive then
      HorzScrollBar.ChangeBiDiPosition;
  finally
     FlippedList.Free;
  end;
end;

procedure TBegaScrollingWinControl.CalcAutoRange;
begin
  if FAutoRangeCount <= 0 then
  begin
    HorzScrollBar.CalcAutoRange;
    VertScrollBar.CalcAutoRange;
  end;
end;

procedure TBegaScrollingWinControl.SetAutoScroll(Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if Value then CalcAutoRange else
    begin
      HorzScrollBar.setRange(0);
      VertScrollBar.setRange(0);
    end;
  end;
end;

procedure TBegaScrollingWinControl.SetHorzScrollBar(Value: TBegaScrollBar);
begin
  FHorzScrollBar.Assign(Value);
end;

procedure TBegaScrollingWinControl.SetVertScrollBar(Value: TBegaScrollBar);
begin
  FVertScrollBar.Assign(Value);
end;

procedure TBegaScrollingWinControl.UpdateScrollBars;
begin
  if not FUpdatingScrollBars and HandleAllocated then
    try
      FUpdatingScrollBars := True;
      if FVertScrollBar.NeedsScrollBarVisible then
      begin
        FHorzScrollBar.Update(False, True);
        FVertScrollBar.Update(True, False);
      end
      else if FHorzScrollBar.NeedsScrollBarVisible then
      begin
        FVertScrollBar.Update(False, True);
        FHorzScrollBar.Update(True, False);
      end
      else
      begin
        FVertScrollBar.Update(False, False);
        FHorzScrollBar.Update(True, False);
      end;
    finally
      FUpdatingScrollBars := False;
    end;
end;

procedure TBegaScrollingWinControl.AutoScrollInView(AControl: TControl);
begin
  if (AControl <> nil) and not (csLoading in AControl.ComponentState) and
    not (csLoading in ComponentState) then
    ScrollInView(AControl);
end;

procedure TBegaScrollingWinControl.DisableAutoRange;
begin
  Inc(FAutoRangeCount);
end;

procedure TBegaScrollingWinControl.EnableAutoRange;
begin
  if FAutoRangeCount > 0 then
  begin
    Dec(FAutoRangeCount);
    if (FAutoRangeCount = 0) and (FHorzScrollBar.Visible or
      FVertScrollBar.Visible) then CalcAutoRange;
  end;
end;

procedure TBegaScrollingWinControl.ScrollInView(AControl: TControl);
var
  Rect: TRect;
begin
  if AControl = nil then Exit;
  Rect := AControl.ClientRect;
  Dec(Rect.Left, HorzScrollBar.Margin);
  Inc(Rect.Right, HorzScrollBar.Margin);
  Dec(Rect.Top, VertScrollBar.Margin);
  Inc(Rect.Bottom, VertScrollBar.Margin);
  Rect.TopLeft := ScreenToClient(AControl.ClientToScreen(Rect.TopLeft));
  Rect.BottomRight := ScreenToClient(AControl.ClientToScreen(Rect.BottomRight));
  if Rect.Left < 0 then
    with HorzScrollBar do Position := Position + Rect.Left
  else if Rect.Right > ClientWidth then
  begin
    if Rect.Right - Rect.Left > ClientWidth then
      Rect.Right := Rect.Left + ClientWidth;
    with HorzScrollBar do Position := Position + Rect.Right - ClientWidth;
  end;
  if Rect.Top < 0 then
    with VertScrollBar do Position := Position + Rect.Top
  else if Rect.Bottom > ClientHeight then
  begin
    if Rect.Bottom - Rect.Top > ClientHeight then
      Rect.Bottom := Rect.Top + ClientHeight;
    with VertScrollBar do Position := Position + Rect.Bottom - ClientHeight;
  end;
end;

procedure TBegaScrollingWinControl.ScaleScrollBars(M, D: Integer);
begin
  if M <> D then
  begin
    if not (csLoading in ComponentState) then
    begin
      HorzScrollBar.FScaled := True;
      VertScrollBar.FScaled := True;
    end;
    HorzScrollBar.Position := 0;
    VertScrollBar.Position := 0;
    if not FAutoScroll then
    begin
      with HorzScrollBar do if FScaled then Range := MulDiv(Range, M, D);
      with VertScrollBar do if FScaled then Range := MulDiv(Range, M, D);
    end;
  end;
  HorzScrollBar.FScaled := False;
  VertScrollBar.FScaled := False;
end;

procedure TBegaScrollingWinControl.ChangeScale(M, D: Integer);
begin
  ScaleScrollBars(M, D);
  inherited ChangeScale(M, D);
end;

procedure TBegaScrollingWinControl.Resizing(State: TWindowState);
begin
  // Overridden by TCustomFrame
end;

procedure TBegaScrollingWinControl.WMSize(var Message: TWMSize);
var
  NewState: TWindowState;
begin
  Inc(FAutoRangeCount);
  try
    inherited;
    NewState := wsNormal;
    case Message.SizeType of
      SIZENORMAL: NewState := wsNormal;
      SIZEICONIC: NewState := wsMinimized;
      SIZEFULLSCREEN: NewState := wsMaximized;
    end;
    Resizing(NewState);
  finally
    Dec(FAutoRangeCount);
  end;
  FUpdatingScrollBars := True;
  try
    CalcAutoRange;
  finally
    FUpdatingScrollBars := False;
  end;
  if FHorzScrollBar.Visible or FVertScrollBar.Visible then
    UpdateScrollBars;
end;

procedure TBegaScrollingWinControl.WMHScroll(var Message: TWMHScroll);
begin
  if (Message.ScrollBar = 0) and FHorzScrollBar.Visible then
    FHorzScrollBar.ScrollMessage(Message) else
    inherited;
end;

procedure TBegaScrollingWinControl.WMVScroll(var Message: TWMVScroll);
begin
  if (Message.ScrollBar = 0) and FVertScrollBar.Visible then
    FVertScrollBar.ScrollMessage(Message) else
    inherited;
end;

procedure TBegaScrollingWinControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := Bounds(-HorzScrollBar.Position, -VertScrollBar.Position,
    Max(HorzScrollBar.Range, ClientWidth),
    Max(VertScrollBar.Range, ClientHeight));
  inherited AdjustClientRect(Rect);
end;

procedure TBegaScrollingWinControl.CMBiDiModeChanged(var Message: TMessage);
var
  Save: Integer;
begin
  Save := Message.WParam;
  try
    { prevent inherited from calling Invalidate & RecreateWnd }
    if not (Self is TBegaScrollBox) then Message.wParam := 1;
    inherited;
  finally
    Message.wParam := Save;
  end;
  if HandleAllocated then
  begin
    HorzScrollBar.ChangeBiDiPosition;
    UpdateScrollBars;
  end;
end;

//- BG ----------------------------------------------------------- 22.01.2006 --
function TBegaScrollingWinControl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and not (ssAlt in Shift) then
    Result := VertScrollBar.DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
    Result := HorzScrollBar.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

//- BG ----------------------------------------------------------- 13.07.2006 --
function TBegaScrollingWinControl.isPanning: Boolean;
begin
  Result := FPanning;
end;

//- BG ----------------------------------------------------------- 13.07.2006 --
procedure TBegaScrollingWinControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FPanningEnabled and (Shift * [ssShift, ssAlt, ssCtrl] = []) then
  begin
    FPanning := True;
    FPanningStart.X := X;
    FPanningStart.Y := Y;
  end
  else
    inherited;
end;

//-- BG ---------------------------------------------------------- 29.07.2013 --
function TBegaScrollingWinControl.GetMaxScroll(): TPoint;
var
  I: Integer;
  Control: TControl;
begin
  // Do not scroll topest leftest corner of all child controls into my client rect.
  Result.X := 0;
  Result.Y := 0;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    Result.X := Max(Result.X, -Control.Left);
    Result.Y := Max(Result.Y, -Control.Top);
  end;
end;

//- BG ----------------------------------------------------------- 13.07.2006 --
procedure TBegaScrollingWinControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Scroll: TPoint;
begin
  if FPanning then
  begin
    Scroll := GetMaxScroll;
    Scroll.X := Min(Scroll.X, X - FPanningStart.X);
    Scroll.Y := Min(Scroll.Y, Y - FPanningStart.Y);
    ScrollBy(Scroll.X, Scroll.Y);
    FPanningStart.X := X;
    FPanningStart.Y := Y;
  end
  else
    inherited;
end;

//- BG ----------------------------------------------------------- 13.07.2006 --
procedure TBegaScrollingWinControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Scroll: TPoint;
begin
  if FPanning then
  begin
    Scroll := GetMaxScroll;
    Scroll.X := Min(Scroll.X, X - FPanningStart.X);
    Scroll.Y := Min(Scroll.Y, Y - FPanningStart.Y);
    ScrollBy(Scroll.X, Scroll.Y);
    FPanning := False;
  end
  else
    inherited;
end;

 {$ifdef UseVCLStyles}
type
  TWinControlClassHook = class(TWinControl);

class constructor TBegaScrollingWinControl.Create;
{Workaround for an issue calling TCustomStyleEngine.RegisterStyleHook in Delphi XE2.
It turns out that an abstract method TCustomStyleEngine.Notification was being called
by TCustomStyleEngine.UnRegisterStyleHook . This could cause the program to raise an
Abstract Method exception when exiting.  This is the exact same issue as described
at http://code.google.com/p/virtual-treeview/issues/detail?id=345 .
}
begin
   {$ifdef HasSystemUITypes}
   TCustomStyleEngine.RegisterStyleHook(TBegaScrollingWinControl, TBegaScrollingWinControlStyleHook);
   {$else}
   TStyleEngine.RegisterStyleHook(TBegaScrollingWinControl, TBegaScrollingWinControlStyleHook);
   {$endif}
end;

class destructor TBegaScrollingWinControl.Destroy;
begin
  {$ifdef HasSystemUITypes}
  TCustomStyleEngine.UnRegisterStyleHook(TBegaScrollingWinControl, TBegaScrollingWinControlStyleHook);
   {$else}
  TStyleEngine.UnRegisterStyleHook(TBegaScrollingWinControl, TBegaScrollingWinControlStyleHook);
  {$endif}
end;

procedure TBegaScrollingWinControlStyleHook.WndProc(var Message: TMessage);
begin
  // Reserved for potential updates
  inherited;
end;

constructor TBegaScrollingWinControlStyleHook.Create(AControl: TWinControl);
begin
  inherited;
  OverrideEraseBkgnd := True;
  {$ifdef has_StyleElements}
  if seClient in Control.StyleElements then
    Brush.Color := StyleServices.GetStyleColor(scPanel)
  else
    Brush.Color :=  TWinControlClassHook(Control).Color;
  {$else}
  Brush.Color := StyleServices.GetStyleColor(scListBox);
  {$Endif}
end;

 {$endif}

{ TBegaScrollBox }

constructor TBegaScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  Width := 185;
  Height := 41;
  FBorderStyle := bsSingle;
end;

procedure TBegaScrollBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls {$ifndef LCL} and Ctl3D {$endif} and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TBegaScrollBox.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd {$ifdef LCL}(Self){$endif};
  end;
end;

{$ifndef LCL}
procedure TBegaScrollBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;
{$endif}

{ EBegaScrollBoxEnumException }

//- BG ----------------------------------------------------------- 23.01.2005 --
constructor EBegaScrollBoxEnumException.createEnum(EnumType: PTypeInfo; const Value);
begin
  inherited createFmt('unsupported: %s.%s', [EnumType.Name, GetEnumName(EnumType, integer(Value))]);
end;

{$endif NoFlatScrollbars}
end.
