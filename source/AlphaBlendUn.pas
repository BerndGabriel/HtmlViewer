{
Version   11.3
Copyright 2012 by J. Peter Mugaas

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
unit AlphaBlendUn;

interface
{$I htmlcons.inc}
uses
  Windows, Graphics, Types, SysUtils,
  HTMLUn2, HtmlGlobals, msimg32;

function TransparentTextOutput(ACanvas : TCanvas;
  const AX, AY : Integer;
  ATxt : PWideChar; const ALen : Integer;
  const ATop, AHeight : Integer;
  const AOpacity : Byte; const ABreakExtra, ABreakCount : Integer) : Integer; {$ifdef UseInline} inline; {$endif}
procedure TransparentCircle(ACanvas : TCanvas; const AX, AY, ARad: Integer; const AOpacity : Byte); {$ifdef UseInline} inline; {$endif}
procedure TransparentEllipse(ACanvas : TCanvas; const ALeftX, ALeftY, ARightX, ARightY : Integer; const AOpacity : Byte); {$ifdef UseInline} inline; {$endif}
procedure TransparentRectangle(ACanvas : TCanvas; const ALeftX, ALeftY, ARightX, ARightY : Integer; const AOpacity : Byte); {$ifdef UseInline} inline; {$endif}
procedure TransparentDrawFocusRect(ACanvas : TCanvas; ARect : TRect; const AOpacity : Byte);    {$ifdef UseInline} inline; {$endif}
procedure TransparentWrapTextW(ACanvas : TCanvas; const AX, AY, AWidth, AHeight : Integer; const AStr : String; const AOpacity : Byte); {$ifdef UseInline} inline; {$endif}
function TransparentExtTextOutW(ACanvas : TCanvas; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PWideChar; Count: Longint; Dx: PInteger; const AOpacity : Byte): BOOL; {$ifdef UseInline} inline; {$endif}
procedure TransparentFillRect(ACanvas : TCanvas; ARect : TRect; const AOpacity : Byte);  {$ifdef UseInline} inline; {$endif}

{Set up a bitmap that we will draw to.  This will then be superimposed on the canvas
with the AlphaBlend function.}
function SetupAlphaBlendBmp(ASrcCanvas : TCanvas; const AWidth, AHeight : Integer) : Graphics.TBitmap; {$ifdef UseInline} inline; {$endif}
procedure AlphaDrawTransparentBitmap(ASource: Graphics.TBitmap;
  ADest: TCanvas;
  ASrcLeft, ASrcTop, ASrcWidth, ASrcHeight : Integer;
  ADestLeft, ADestTop, ADestWidth, ADestHeight : Integer;
  AOpacity: Byte); overload; {$ifdef UseInline} inline; {$endif}
procedure AlphaDrawTransparentBitmap(Source: Graphics.TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload;  {$ifdef UseInline} inline; {$endif}
procedure AlphaDrawTransparentBitmap(Source: Graphics.TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload; {$ifdef UseInline} inline; {$endif}


implementation

{
These were barrowed from Vcl.GraphUtil
Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
}
procedure AlphaDrawTransparentBitmap(ASource: Graphics.TBitmap;
  ADest: TCanvas;
  ASrcLeft, ASrcTop, ASrcWidth, ASrcHeight : Integer;
  ADestLeft, ADestTop, ADestWidth, ADestHeight : Integer;
  AOpacity: Byte);
var
  BlendFunc: t_BLENDFUNCTION;
begin
  BlendFunc.BlendOp := c_AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := AOpacity;
  if ASource.PixelFormat = pf32bit then
    BlendFunc.AlphaFormat := c_AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;
  jpm_AlphaBlend(ADest.Handle, ADestLeft, ADestTop, ADestWidth, ADestHeight,
                     ASource.Canvas.Handle, ASrcLeft, ASrcTop , ASrcWidth, ASrcHeight, BlendFunc);
end;

procedure AlphaDrawTransparentBitmap(Source: Graphics.TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
var
  BlendFunc: t_BLENDFUNCTION;
begin
  BlendFunc.BlendOp := c_AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;
  if Source.PixelFormat = pf32bit then
    BlendFunc.AlphaFormat := c_AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;
  jpm_AlphaBlend(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                     Source.Canvas.Handle, 0, 0, Source.Width, Source.Height, BlendFunc);
end;

procedure AlphaDrawTransparentBitmap(Source: Graphics.TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
var
  BlendFunc: t_BLENDFUNCTION;
begin
  BlendFunc.BlendOp := c_AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;
  if Source.PixelFormat = pf32bit then
    BlendFunc.AlphaFormat := c_AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;
  jpm_AlphaBlend(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                     Source.Canvas.Handle, SourceRect.Left, SourceRect.Top, SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top, BlendFunc);
end;

function SetupAlphaBlendBmp(ASrcCanvas : TCanvas; const AWidth, AHeight : Integer) : Graphics.TBitmap;
begin
  Result := Graphics.TBitmap.Create;
  Result.Width := AWidth;
  Result.height := AHeight;
  Result.Canvas.Font.Assign(ASrcCanvas.Font);
  Result.Canvas.Brush.Assign(ASrcCanvas.Brush);
  Result.Canvas.Pen.Assign(ASrcCanvas.Pen);
end;

function TransparentExtTextOutW(ACanvas : TCanvas; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PWideChar; Count: Longint; Dx: PInteger; const AOpacity : Byte): BOOL;
var
  LB : Graphics.TBitmap;
begin
  if Assigned(jpm_AlphaBlend) then begin
    if (AOpacity = $FF)  then begin
      Result := ExtTextOutW(ACanvas.Handle,X,Y,Options,Rect,Str,Count,Dx);
    end else begin
      LB := SetupAlphaBlendBmp(ACanvas,Rect^.Right - Rect^.Left,Rect^.Bottom - Rect^.Top);
      try
        Result := ExtTextOutW(LB.Canvas.Handle,X,Y,Options,Rect,Str,Count,Dx);
        AlphaDrawTransparentBitmap(LB, ACanvas,Rect^,AOpacity);
      finally
        LB.Free;
      end;
    end;
  end else begin
    Result := ExtTextOutW(ACanvas.Handle,X,Y,Options,Rect,Str,Count,Dx);
  end;
end;

function TransparentTextOutput(ACanvas : TCanvas;
  const AX, AY : Integer;
  ATxt : PWideChar; const ALen : Integer;
  const ATop, AHeight : Integer;
  const AOpacity : Byte; const ABreakExtra, ABreakCount : Integer) : Integer;

var
  LB : Graphics.TBitmap;
  LSize : TSize;

begin
  if Assigned(jpm_AlphaBlend) then begin
    if (AOpacity = $FF)  then begin
      Windows.TextOutW(ACanvas.Handle, AX, AY, ATxt, ALen);
      Result := AX + GetXExtent(ACanvas.Handle, ATxt, ALen);
    end else begin
      GetTSize(ACanvas.Handle,PWideChar(ATxt), ALen,LSize);
      LB := SetupAlphaBlendBmp(ACanvas, LSize.cx, AHeight );
      try
        SetTextAlign(LB.Canvas.Handle, TA_BASELINE);
        SetTextJustification(LB.Canvas.Handle, ABreakExtra, ABreakCount);
        Windows.TextOutW(LB.Canvas.Handle,0, AY - ATop,ATxt,ALen);
        AlphaDrawTransparentBitmap(LB, ACanvas,
           0, 0, LSize.cx, AHeight,
          AX, ATop, LSize.cx, AHeight,
          AOpacity);
        Result := AX + LSize.cx;
      finally
        LB.Free;
      end;
    end;
  end else begin
    TextOutW(ACanvas.Handle, AX, AY, PWideChar(ATxt), ALen);
    Result := AX + GetXExtent(ACanvas.Handle, PWideChar(ATxt), ALen);
  end;
end;

procedure TransparentDrawFocusRect(ACanvas : TCanvas; ARect : TRect; const AOpacity : Byte);

var
  LB: Graphics.TBitmap;
  Width, Height: Integer;
begin
  if Assigned(jpm_AlphaBlend) then begin
    if (AOpacity = $FF)  then begin
      ACanvas.DrawFocusRect(ARect);
    end else begin
      Width := ARect.Right - ARect.Left;
      Height := ARect.Bottom - ARect.Top;
      LB := SetupAlphaBlendBmp(ACanvas,Width,Height);
      try
        LB.Canvas.DrawFocusRect(Rect(0,0,Width,Height ));
        AlphaDrawTransparentBitmap(LB, ACanvas,ARect,AOpacity);
      finally
        LB.Free;
      end;
    end;
  end else begin
    ACanvas.DrawFocusRect(ARect);
  end;
end;

procedure TransparentEllipse(ACanvas : TCanvas; const ALeftX, ALeftY, ARightX, ARightY : Integer; const AOpacity : Byte);
var
  LB : Graphics.TBitmap;
begin
  if Assigned(jpm_AlphaBlend) then begin

    if (AOpacity = $FF)  then begin
      ACanvas.Ellipse(ALeftX, ALeftY, ARightX, ARightY);
    end else begin
      LB := SetupAlphaBlendBmp(ACanvas,ARightX - ALeftX,ARightY - ALeftY);
      try
        LB.Canvas.Ellipse(0,0,ARightX - ALeftX,ARightY - ALeftY);
        AlphaDrawTransparentBitmap(LB, ACanvas,Rect(ALeftX,ALeftY,ARightX,ARightY),AOpacity);
      finally
        LB.Free;
      end;
    end;
  end else begin
    ACanvas.Ellipse(ALeftX, ALeftY, ARightX, ARightY);
  end;
end;

procedure TransparentCircle(ACanvas : TCanvas; const AX, AY, ARad: Integer; const AOpacity : Byte);
var
  LB : Graphics.TBitmap;
begin
  if Assigned(jpm_AlphaBlend) then begin

    if (AOpacity = $FF)  then begin
      ACanvas.Ellipse(AX, AY - ARad, AX + ARad, AY);
    end else begin
      LB := SetupAlphaBlendBmp(ACanvas,ARad,AY);
      try
        LB.Canvas.Ellipse(0,0,ARad,AY);
        AlphaDrawTransparentBitmap(LB, ACanvas,Rect(AX,AY - ARad,AX + ARad,AY),AOpacity);
      finally
        LB.Free;
      end;
    end;
  end else begin
    ACanvas.Ellipse(AX, AY - ARad, AX + ARad, AY);
  end;
end;

procedure TransparentRectangle(ACanvas : TCanvas; const ALeftX, ALeftY, ARightX, ARightY : Integer; const AOpacity : Byte);
var
  LB : Graphics.TBitmap;
begin
  if Assigned(jpm_AlphaBlend) then begin

    if (AOpacity = $FF) then begin
      ACanvas.Rectangle(ALeftX,ALeftY,ARightX,ARightY);
    end else begin
      LB := SetupAlphaBlendBmp(ACanvas,ARightX - ALeftX,ARightY - ALeftY);
      try
        LB.Canvas.Rectangle(0,0,ARightX - ALeftX,ARightY - ALeftY);
        AlphaDrawTransparentBitmap(LB, ACanvas,Rect(ALeftX,ALeftY,ARightX,ARightY),AOpacity);

      finally
        LB.Free;
      end;
    end;
  end else begin
    ACanvas.Rectangle(ALeftX,ALeftY,ARightX,ARightY);
  end;
end;

procedure TransparentWrapTextW(ACanvas : TCanvas; const AX, AY, AWidth, AHeight : Integer; const AStr : String; const AOpacity : Byte);
var
  LB : Graphics.TBitmap;
begin
  if Assigned(jpm_AlphaBlend) then begin
    if (AOpacity = $FF) then begin
      WrapTextW(ACanvas, AX, AY, AWidth, AHeight,AStr);
    end else begin
      LB := SetupAlphaBlendBmp(ACanvas,AWidth,AHeight);
      try
        WrapTextW(ACanvas,0,0,AWidth,AHeight,AStr);
        AlphaDrawTransparentBitmap(LB, ACanvas,Rect(AX,AY,AWidth+AX,AHeight+AY),AOpacity);
      finally
        LB.Free;
      end;
    end;
  end else begin
    WrapTextW(ACanvas, AX, AY, AWidth, AHeight,AStr);
  end;
end;

procedure TransparentFillRect(ACanvas : TCanvas; ARect : TRect; const AOpacity : Byte);
var
  LR : TRect;
  LB : Graphics.TBitmap;
  Width, Height: Integer;
begin
  if Assigned(jpm_AlphaBlend) then begin
    if (AOpacity = $FF) then begin
      ACanvas.FillRect(ARect);
    end else begin
      Width := ARect.Right - ARect.Left;
      Height := ARect.Bottom - ARect.Top;
      LB := SetupAlphaBlendBmp(ACanvas, Width, Height);
      try
        LR.Left := 0;
        LR.Top := 0;
        LR.Right := Width;
        LR.Bottom := Height;
        LB.Canvas.FillRect(LR);
        AlphaDrawTransparentBitmap(LB, LR, ACanvas, ARect, AOpacity);
      finally
        LB.Free;
      end;
    end
  end else begin
    ACanvas.FillRect(ARect);
  end;
end;

end.
