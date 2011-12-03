{
Version   12
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2010 by HtmlViewer Team
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

unit HtmlDraw;

interface

uses
  Windows, Graphics, Types, Math,
  //
  HtmlGlobals,
  StyleTypes;

procedure DeflateRect(var Rect: TRect; const Delta: TRectIntegers); overload;
procedure DeflateRect(var Rect: TRect; const Source: TRect; const Delta: TRectIntegers); overload;
procedure InflateRect(var Rect: TRect; const Source: TRect; const Delta: TRectIntegers); overload;

procedure DrawBorder(Canvas: TCanvas; ORect: TRect; const W: TRectIntegers; const C: TRectColors; const S: TRectStyles; BGround: TColor);

implementation

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure DeflateRect(var Rect: TRect; const Delta: TRectIntegers); overload;
begin
  Inc(Rect.Top, Delta[reTop]);
  Inc(Rect.Left, Delta[reLeft]);
  Dec(Rect.Right, Delta[reRight]);
  Dec(Rect.Bottom, Delta[reBottom]);
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure DeflateRect(var Rect: TRect; const Source: TRect; const Delta: TRectIntegers); overload;
begin
  Rect.Top := Source.Top + Delta[reTop];
  Rect.Left := Source.Left + Delta[reLeft];
  Rect.Right := Source.Right - Delta[reRight];
  Rect.Bottom := Source.Bottom - Delta[reBottom];
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure InflateRect(var Rect: TRect; const Source: TRect; const Delta: TRectIntegers); overload;
begin
  Rect.Top := Source.Top - Delta[reTop];
  Rect.Left := Source.Left - Delta[reLeft];
  Rect.Right := Source.Right + Delta[reRight];
  Rect.Bottom := Source.Bottom + Delta[reBottom];
end;

{----------------DrawBorder}

procedure DrawBorder(Canvas: TCanvas; ORect: TRect; const W: TRectIntegers; const C: TRectColors; const S: TRectStyles; BGround: TColor);
{Draw the 4 sides of a border.  The sides may be of different styles or colors.
 The side indices, 0,1,2,3, represent left, top, right, bottom.
 ORect is the outside rectangle of the border, IRect the inside Rectangle.
 BGround is the background color used for the bssDouble style}

  function Darker(Color: TColor): TColor;
  {find a somewhat darker color for shading purposes}
  const
    F = 0.75; // F < 1 makes color darker
  var
    Red, Green, Blue: Byte;
  begin
    if Color < 0 then
      Color := GetSysColor(Color and $FFFFFF)
    else
      Color := Color and $FFFFFF;
    Red := Color and $FF;
    Green := (Color and $FF00) shr 8;
    Blue := (Color and $FF0000) shr 16;
    Result := RGB(Round(F * Red), Round(F * Green), Round(F * Blue));
  end;

  function Lighter(Color: TColor): TColor;
  {find a somewhat lighter color for shading purposes}
  const
    F = 1.15; // F > 1 makes color lighter
  var
    Red, Green, Blue: Byte;
  begin
    if Color < 0 then
      Color := GetSysColor(Color and $FFFFFF)
    else
      Color := Color and $FFFFFF;
    if Color = 0 then
      Result := 0
    else
    begin
      Red := Color and $FF;
      Green := (Color and $FF00) shr 8;
      Blue := (Color and $FF0000) shr 16;
      Result := RGB(Min(255, Round(F * Red)), Min(255, Round(F * Green)), Min(255, Round(F * Blue)));
    end;
  end;

type
  TBorderPointArray = array[0..3] of TPoint;

var
  MapMode: Integer;
  WindOrg, ViewOrg: TPoint;
  WindExt, ViewExt: TSize;

  function CreateOnePolygonRgn(const PO, PI: TBorderPointArray; I: Integer; Transform: Boolean): HRGN;

    function ToViewport(P: TPoint): TPoint;
    begin
      Result.X := MulDiv(P.X - WindOrg.X, ViewExt.cx, WindExt.cx) + ViewOrg.X;
      Result.Y := MulDiv(P.Y - WindOrg.Y, ViewExt.cy, WindExt.cy) + ViewOrg.Y;
    end;

  var
    P: TBorderPointArray;
  begin
    if not Transform then
    begin
      P[0] := PO[I];
      P[3] := PI[I];
      Inc(I);
      if I = 4 then
        I := 0;
      P[1] := PO[I];
      P[2] := PI[I];
    end
    else
    begin
      P[0] := ToViewport(PO[I]);
      P[3] := ToViewport(PI[I]);
      Inc(I);
      if I = 4 then
        I := 0;
      P[1] := ToViewport(PO[I]);
      P[2] := ToViewport(PI[I]);
    end;
    Result := CreatePolygonRgn(P, 4, ALTERNATE);
  end;

  procedure DrawOnePolygon(const PO, PI: TBorderPointArray; I: Integer; Color: TColor);
  {Here we draw a 4 sided polygon (by filling a region). This represents one side (or part of a side) of a border.}
  var
    R: HRgn;
  begin
    if Color = clNone then
      exit;
    R := CreateOnePolygonRgn(PO, PI, I, False);
    try
      with Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := Color or PalRelative;
        FillRgn(Handle, R, Brush.Handle);
      end;
    finally
      DeleteObject(R);
    end;
  end;

var
  PO, PI, PM, P1, P2: TBorderPointArray;
  I: Integer;
  E: TRectEdge;
  IRect, MRect: TRect;
  lb: TLogBrush;
  Pn, OldPn: HPen;
  D: TRectIntegers;
  InPath: boolean;
  PenType, Start: Integer;
  StyleSet: set of TBorderStyle;
  R, OldClpRgn: HRGN;
  HasOldClpRgn: Integer;
begin
  if (W[reLeft] = 0) and (W[reTop] = 0) and (W[reRight] = 0) and (W[reBottom] = 0) then
    exit;

{find the outside and inside corner points for the border segments}
  with ORect do
  begin
    PO[0] := Point(Left, Bottom);
    PO[1] := TopLeft;
    PO[2] := Point(Right, Top);
    PO[3] := BottomRight;
  end;
  DeflateRect(IRect, ORect, W);
  with IRect do
  begin
    PI[0] := Point(Left, Bottom);
    PI[1] := TopLeft;
    PI[2] := Point(Right, Top);
    PI[3] := BottomRight;
  end;

{Find out what style types are represented in this border}
  StyleSet := [];
  for E := low(E) to high(E) do
    Include(StyleSet, S[E]);

{Points midway between the outer and inner rectangle are needed for
 ridge, groove, dashed, dotted styles}
  MapMode := MM_TEXT;
  if [bssRidge, bssGroove, bssDotted, bssDashed] * StyleSet <> [] then
  begin
    MRect := Rect(
      (ORect.Left + IRect.Left) div 2,
      (ORect.Top + IRect.Top) div 2,
      (ORect.Right + IRect.Right + 1) div 2,
      (ORect.Bottom + IRect.Bottom + 1) div 2);
    with MRect do
    begin
      PM[0] := Point(Left, Bottom);
      PM[1] := TopLeft;
      PM[2] := Point(Right, Top);
      PM[3] := BottomRight;
    end;
    if [bssDotted, bssDashed] * StyleSet <> [] then
    begin
      MapMode := GetMapMode(Canvas.Handle);
      if MapMode <> MM_TEXT then
      begin
        GetWindowOrgEx(Canvas.Handle, WindOrg);
        GetViewportOrgEx(Canvas.Handle, ViewOrg);
// We support MM_TEXT, MM_ISOTROPIC, MM_ANISOTROPIC only.
//        case MapMode of
//          MM_ISOTROPIC,
//          MM_ANISOTROPIC:
//          begin
            GetWindowExtEx(Canvas.Handle, WindExt);
            GetViewportExtEx(Canvas.Handle, ViewExt);
//          end;
//        end;
      end;
    end;
  end;

{the Double style needs the space between inner and outer rectangles divided
 into three parts}
  if bssDouble in StyleSet then
  begin
    for E := low(E) to high(E) do
      D[E] := (W[E] + 2) div 3;

    DeflateRect(MRect, ORect, D);
    with MRect do
    begin
      P1[0] := Point(Left, Bottom);
      P1[1] := TopLeft;
      P1[2] := Point(Right, Top);
      P1[3] := BottomRight;
    end;

    for E := low(E) to high(E) do
      D[E] := (W[E]) div 3;
    InflateRect(MRect, IRect, D);
    with MRect do
    begin
      P2[0] := Point(Left, Bottom);
      P2[1] := TopLeft;
      P2[2] := Point(Right, Top);
      P2[3] := BottomRight;
    end;
  end;

  InPath := False;
  Pn := 0;
  OldPn := 0;
  Start := 0;
  try
    for I := 0 to 3 do
    begin
      E := TRectEdge(I);
      case S[E] of
        bssSolid:
          DrawOnePolygon(PO, PI, I, C[E]);

        bssInset:
          if I <= 1 then
            DrawOnePolygon(PO, PI, I, Darker(C[E]))
          else
            DrawOnePolygon(PO, PI, I, Lighter(C[E]));

        bssOutset:
          if I >= 2 then
            DrawOnePolygon(PO, PI, I, Darker(C[E]))
          else
            DrawOnePolygon(PO, PI, I, Lighter(C[E]));

        bssGroove:
          if I <= 1 then
          begin
            DrawOnePolygon(PO, PM, I, Darker(C[E]));
            DrawOnePolygon(PM, PI, I, Lighter(C[E]));
          end
          else
          begin
            DrawOnePolygon(PO, PM, I, Lighter(C[E]));
            DrawOnePolygon(PM, PI, I, Darker(C[E]));
          end;

        bssRidge:
          if I >= 2 then
          begin
            DrawOnePolygon(PO, PM, I, Darker(C[E]));
            DrawOnePolygon(PM, PI, I, Lighter(C[E]));
          end
          else
          begin
            DrawOnePolygon(PO, PM, I, Lighter(C[E]));
            DrawOnePolygon(PM, PI, I, Darker(C[E]));
          end;

        bssDouble:
        begin
          DrawOnePolygon(PO, P1, I, C[E]);
          DrawOnePolygon(P1, P2, I, BGround);
          DrawOnePolygon(P2, PI, I, C[E]);
        end;

        bssDashed, bssDotted:
        begin
          OldClpRgn := CreateRectRgn(0, 0, 1, 1); // just a dummy to hold the actual old clip region after GetClipRgn
          HasOldClpRgn := GetClipRgn(Canvas.Handle, OldClpRgn);
          R := CreateOnePolygonRgn(PO, PI, I, MapMode <> MM_TEXT);
          SelectClipRgn(Canvas.Handle, R);
          try
            if BGround <> clNone then
              DrawOnePolygon(PO, PI, I, BGround);
            if not InPath then
            begin
              lb.lbStyle := BS_SOLID;
              lb.lbColor := C[E] or PalRelative;
              lb.lbHatch := 0;
              if S[E] = bssDotted then
                PenType := PS_Dot or ps_EndCap_Round
              else
                PenType := PS_Dash or ps_EndCap_Square;
              Pn := ExtCreatePen(PS_GEOMETRIC or PenType or ps_Join_Miter, W[E], lb, 0, nil);
              OldPn := SelectObject(Canvas.Handle, Pn);
              BeginPath(Canvas.Handle);
              MoveToEx(Canvas.Handle, PM[I].x, PM[I].y, nil);
              Start := I;
              InPath := True;
            end;
            if C[E] <> clNone then
              LineTo(Canvas.Handle, PM[(I + 1) mod 4].x, PM[(I + 1) mod 4].y);
            if (I = 3) or (S[Succ(E)] <> S[E]) or (C[Succ(E)] <> C[E]) or (W[Succ(E)] <> W[E]) then
            begin
              if (I = 3) and (Start = 0) then
                CloseFigure(Canvas.Handle); {it's a closed path}
              EndPath(Canvas.Handle);
              StrokePath(Canvas.Handle);
              SelectObject(Canvas.Handle, OldPn);
              DeleteObject(Pn);
              Pn := 0;
              InPath := False;
            end;
          finally
            if HasOldClpRgn = 1 then
            begin
              SelectClipRgn(Canvas.Handle, OldClpRgn);
              DeleteObject(OldClpRgn);
            end
            else
              SelectClipRgn(Canvas.Handle, 0);
            DeleteObject(R);
          end;
        end;
      end;
    end;
  finally
    if Pn <> 0 then
    begin
      SelectObject(Canvas.Handle, OldPn);
      DeleteObject(Pn);
    end;
  end;
end;

end.
