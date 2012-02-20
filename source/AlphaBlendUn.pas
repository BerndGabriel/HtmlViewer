unit AlphaBlendUn;

interface
{$I htmlcons.inc}
uses Graphics, Types, SysUtils;

procedure TransparentFillRect(ACanvas : TCanvas; Rect : TRect; const AOpacity : Byte);

implementation
{$ifdef Use_VCLGraphUtil}
uses
  Winapi.Windows,
  System.UITypes,
  Vcl.GraphUtil;
{$endif}

procedure TransparentFillRect(ACanvas : TCanvas; Rect : TRect; const AOpacity : Byte);
{$ifdef Use_VCLGraphUtil}
var LR : TRect;
  LB : Graphics.TBitmap;
begin
  if AOpacity = $FF then begin
    ACanvas.FillRect(Rect);
  end else begin
    LR.Left := 0;
    LR.Top := 0;
    LR.Width := Rect.Width;
    LR.Height := Rect.Height;
    LB := Graphics.TBitmap.Create;
    try
      LB.SetSize(LR.Width,LR.Height);
      LB.Canvas.Brush.Assign(ACanvas.Brush);
      LB.Canvas.FillRect(LR);
      DrawTransparentBitmap(LB, ACanvas,Rect, AOpacity  );
    finally
      LB.Free;
    end;
  end;
{$else}
  ACanvas.FillRect(Rect);
{$endif}
end;

end.
