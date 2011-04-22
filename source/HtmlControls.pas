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

unit HtmlControls;

interface

uses
  Windows, Messages, Graphics, Classes, Controls, StdCtrls, Math;

type

//------------------------------------------------------------------------------
// THtmlScrollBox implements a canvas for scaleable and scrollable content
//------------------------------------------------------------------------------
// Content is the rectangular area Rect(0, 0, ContentWidth, ContentHeight)
// What is painted in the THtmlScrollBox depends on its ClientRect, the Scale
// and the ContentPosition (the point that maps to the ClientRect's origin.
//
// Scaling:
// A Scale > 1 enlarges the content, a Scale > 0 and < 1 shrinks the content.
// A Scale <= 0 is not specified. Default Scale is 1.
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

  THtmlScrollBox = class(TCustomControl)
  private
    FState: THtmlScrollBoxState;
    FWinExt: Integer; // scale nominator, window units
    FDocExt: Integer; // scale denominator, content units
    FScrollIncrement: Integer; // how far to scroll with arrow buttons, window units
    FContentPosition: TPoint; // content position at viewport origin
    FScrollBars: TScrollStyle; // which scrollbars are enabled?
    FScrollInfos: ThtScrollInfos; // used by UpdateScrollbars to remember the scrollbar settings.
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
    procedure Paint; override;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    property DocExt: Integer read FDocExt; // scale denominator, content units
    property WinExt: Integer read FWinExt; // scale nominator, window units
  public
    constructor Create(AOwner: TComponent); override;
    function ToContentUnits(WindowUnits: Integer): Integer;
    function ToWindowUnits(ContentUnits: Integer): Integer;
    property ContentHeight: Integer read GetContentHeight;
    property ContentPosition: TPoint read FContentPosition write SetContentPosition;
    property ContentPositionLeft: Integer read FContentPosition.X write SetContentPositionLeft;
    property ContentPositionTop: Integer read FContentPosition.Y write SetContentPositionTop;
    property ContentWidth: Integer read GetContentWidth;
    property Scale: Double read GetScale write SetScale;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
  end;

implementation

var
  sbWidth, sbHeight: Integer;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function NewPosition(const SI: TScrollInfo; var Value: Integer): Boolean;
begin
  if Value < SI.nMin then
    Value := SI.nMin
  else if Value > SI.nMax - Integer(SI.nPage) then
    Value := SI.nMax - Integer(SI.nPage);
  Result := Value <> SI.nPos;
end;

{ THtmlScrollBox }

//-- BG ---------------------------------------------------------- 22.04.2011 --
constructor THtmlScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FScrollIncrement := 17;
  FDocExt := 10000;
  FWinExt := 10000;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function THtmlScrollBox.GetScale: Double;
begin
  if (FWinExt <> 0) and (FDocExt <> 0) then
    Result := FWinExt / FDocExt
  else
    Result := 1.0;
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
function THtmlScrollBox.GetScrollInfo(Which: Integer): TScrollInfo;
begin
  Result.cbSize := SizeOf(TScrollInfo);
  Result.fMask := SIF_ALL;
  Windows.GetScrollInfo(Handle, Which, Result);
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure THtmlScrollBox.Paint;
begin
  SetMapMode(Canvas.Handle, MM_ISOTROPIC);
  SetWindowOrgEx(Canvas.Handle, ContentPositionLeft, ContentPositionTop, nil);
  SetWindowExtEx(Canvas.Handle, FDocExt, FDocExt, nil);
  SetViewportExtEx(Canvas.Handle, FWinExt, FWinExt, nil);

  Canvas.Pen.Color := clYellow;
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(0, 0, ContentWidth, ContentHeight);
  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(ContentPositionLeft - 20, ContentPositionTop);
  Canvas.LineTo(ContentPositionLeft + 60, ContentPositionTop);
  Canvas.MoveTo(ContentPositionLeft, ContentPositionTop - 20);
  Canvas.LineTo(ContentPositionLeft, ContentPositionTop + 60);
  Canvas.Pen.Color := clLime;
  Canvas.MoveTo(300 - 20, 100);
  Canvas.LineTo(300 + 60, 100);
  Canvas.MoveTo(300, 100 - 20);
  Canvas.LineTo(300, 100 + 60);
  inherited;

  SetMapMode(Canvas.Handle, MM_TEXT);
  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
procedure THtmlScrollBox.Resize;
begin
  inherited;
  UpdateScrollBars;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure THtmlScrollBox.SetContentPosition(const Value: TPoint);
begin
  if (FContentPosition.X <> Value.X) or (FContentPosition.Y <> Value.Y) then
  begin
    FContentPosition := Value;
    UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure THtmlScrollBox.SetContentPositionLeft(const Value: Integer);
begin
  if FContentPosition.X <> Value then
  begin
    FContentPosition.X := Value;
    UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure THtmlScrollBox.SetContentPositionTop(const Value: Integer);
begin
  if FContentPosition.Y <> Value then
  begin
    FContentPosition.Y := Value;
    UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
procedure THtmlScrollBox.SetScale(const Value: Double);
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
    UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 19.04.2011 --
procedure THtmlScrollBox.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
  end;
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function THtmlScrollBox.ToContentUnits(WindowUnits: Integer): Integer;
begin
  Result := MulDiv(WindowUnits, FDocExt, FWinExt);
end;

//-- BG ---------------------------------------------------------- 22.04.2011 --
function THtmlScrollBox.ToWindowUnits(ContentUnits: Integer): Integer;
begin
  Result := MulDiv(ContentUnits, FWinExt, FDocExt);
end;

//-- BG ---------------------------------------------------------- 19.04.2011 --
procedure THtmlScrollBox.UpdateScrollBars;

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

    Result.H.DocSize := ToWindowUnits(ContentWidth);
    Result.H.WinSize := ClientWidth;
    if Result.V.Show then
      Inc(Result.H.WinSize, sbWidth);

    Result.V.DocSize := ToWindowUnits(ContentHeight);
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
      Info.nPos := ToWindowUnits(ContentPositionLeft);
      SetScrollInfo(Handle, SB_HORZ, Info, True);
      Info := GetScrollInfo(SB_HORZ);
      FContentPosition.X := ToContentUnits(Info.nPos);
    end;

    if FScrollInfos.V.Show then
    begin
      Info.nMax := FScrollInfos.V.DocSize;
      Info.nPage := FScrollInfos.V.WinSize;
      Info.nPos := ToWindowUnits(ContentPositionTop);
      SetScrollInfo(Handle, SB_VERT, Info, True);
      Info := GetScrollInfo(SB_VERT);
      FContentPosition.Y := ToContentUnits(Info.nPos);
    end;
  finally
    Exclude(FState, sbsUpdating);
  end;
  Invalidate;
end;

//-- BG ---------------------------------------------------------- 20.04.2011 --
procedure THtmlScrollBox.WMHScroll(var Message: TWMHScroll);
var
  SI: TScrollInfo;

  procedure SetPosition(Value: Integer);
  begin
    if NewPosition(SI, Value) then
      ContentPositionLeft := ToContentUnits(Value);
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

//-- BG ---------------------------------------------------------- 20.04.2011 --
procedure THtmlScrollBox.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;

  procedure SetPosition(Value: Integer);
  begin
    if NewPosition(SI, Value) then
      ContentPositionTop := ToContentUnits(Value);
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
