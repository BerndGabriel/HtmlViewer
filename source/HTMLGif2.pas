{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2015 by Bernd Gabriel

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

unit HTMLGif2;

interface

uses
{$ifdef LCL}
  LclIntf, IntfGraphics, FpImage, LclType, HtmlMisc,
{$else}
  Windows, mmSystem,
{$endif}
  SysUtils, Classes, Graphics, Controls, ExtCtrls, Math,
  HtmlGlobals, htmlgif1;

type
  ThtBitmap = class(TBitmap)
  protected
    FMask: TBitmap;
    FTransparent: boolean;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure StretchDraw(ACanvas: TCanvas; const DestRect, SrcRect: TRect);
    function GetMask: TBitmap;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Mask: TBitmap read GetMask;
  end;

  TDisposalType = (
    dtUndefined,    {Take no action}
    dtDoNothing,    {Leave graphic, next frame goes on top of it}
    dtToBackground, {Restore original background for next frame}
    dtToPrevious);  {Restore image as it existed before this frame}

  TgfFrame = class
  private
    { private declarations }
    frLeft: Integer;
    frTop: Integer;
    frWidth: Integer;
    frHeight: Integer;

    frDelay: Integer;
    frDisposalMethod: TDisposalType;
    TheEnd: boolean; {end of what gets copied}

  public
    constructor Create;
    constructor CreateCopy(Item: TgfFrame);
    destructor Destroy; override;
  end;

  TgfFrameList = class(TList)
  private
    function GetFrame(I: integer): TgfFrame;
  public
    {note: Frames is 1 based, goes from [1..Count]}
    property Frames[I: integer]: TgfFrame read GetFrame; default;
  end;

  TGIFImage = class(ThtBitmap)
  private
    FAnimated: Boolean;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FCurrentFrame: Integer;
    FNumFrames: Integer;
    FNumIterations: Integer;
    FVisible: Boolean;

    TheEnd: Boolean; {copy to here}

    FAnimate: Boolean;
    FStretchedRect: TRect;
    WasDisposal: TDisposalType;

    Strip: ThtBitmap;
    Frames: TgfFrameList;

    CurrentIteration: Integer;
    LastTime: DWord;
    CurrentInterval: DWord;

    procedure SetAnimate(AAnimate: Boolean);
    procedure SetCurrentFrame(AFrame: Integer);

    procedure NextFrame(OldFrame: Integer);

  public
    ShowIt: Boolean;
    IsCopy: Boolean; {set if this is a copy of one in Cache}

    constructor Create; override;
    constructor CreateCopy(Item: TGIFImage);
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;

    procedure Draw(Canvas: TCanvas; const ARect: TRect); override;

    property IsAnimated: Boolean read FAnimated;
    property IsTransparent: Boolean read FTransparent;
    property NumFrames: Integer read FNumFrames;
    property NumIterations: Integer read FNumIterations;

    procedure CheckTime(WinControl: TWinControl);

    property Animate: Boolean read FAnimate write SetAnimate;
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    property Visible: Boolean read FVisible write FVisible;
  end;

function LoadGifFromStream(out NonAnimated: Boolean; Stream: TStream): TGifImage;

implementation

function LoadGifFromStream(out NonAnimated: Boolean; Stream: TStream): TGifImage;
begin
  Result := TGifImage.Create;
  Result.LoadFromStream(Stream);
  NonAnimated := not Result.IsAnimated;
end;

{----------------TgfFrame.Create}

constructor TgfFrame.Create;
begin
  inherited Create;
end;

constructor TgfFrame.CreateCopy(Item: TgfFrame);
begin
  inherited Create;
  System.Move(Item.frLeft, frLeft, PtrSub(@TheEnd, @frLeft));
end;

{----------------TgfFrame.Destroy}

destructor TgfFrame.Destroy;
begin
  inherited Destroy;
end;

{----------------TGIFImage.Create}

constructor TGIFImage.Create;
begin
  inherited Create;
  FVisible := True;
  FCurrentFrame := 1;
  Frames := TgfFrameList.Create;
  CurrentIteration := 1;
end;

constructor TGIFImage.CreateCopy(Item: TGIFImage);
var
  I: integer;
begin
  inherited Create;
  Assign(Item);
  FImageWidth := Item.FImageWidth;
  FImageHeight := Item.FImageHeight;
  System.Move(Item.FAnimated, FAnimated, PtrSub(@TheEnd, @FAnimated));
  IsCopy := True;

  Strip := ThtBitmap.Create;
  Strip.Assign(Item.Strip);

  Frames := TgfFrameList.Create;
  for I := 1 to FNumFrames do
    Frames.Add(TgfFrame.CreateCopy(Item.Frames[I]));
  FCurrentFrame := 1;
  CurrentIteration := 1;
  if FAnimated then
    WasDisposal := dtToBackground;
end;

{----------------TGIFImage.Destroy}

destructor TGIFImage.Destroy;
var
  I: Integer;
begin
  for I := Frames.Count downto 1 do
    Frames[I].Free;
  Frames.Free;
  Strip.Free;
  inherited Destroy;
end;

{----------------TGIFImage.Draw}

procedure TGIFImage.Draw(Canvas: TCanvas; const ARect: TRect);
var
  SRect: TRect;
  ALeft: integer;
begin
  if FVisible and (FNumFrames > 0) then
  begin
    FStretchedRect := ARect;
    with Frames[FCurrentFrame] do
    begin
      ALeft := (FCurrentFrame - 1) * Width;
      SRect := Rect(ALeft, 0, ALeft + Width, Height); {current frame location in Strip bitmap}
    end;

    Canvas.CopyMode := cmSrcCopy;
  {draw the correct portion of the strip}
    SetStretchBltMode(Canvas.Handle, ColorOnColor);
    Strip.StretchDraw(Canvas, FStretchedRect, SRect);
  end;
end;

{----------------TGifImage.CheckTime}

procedure TGifImage.CheckTime(WinControl: TWinControl);
var
  ThisTime: DWord;
begin
  if not FAnimate then
    Exit;

  ThisTime := timeGetTime;
  if ThisTime - LastTime < CurrentInterval then
    Exit;

  LastTime := ThisTime;

  if (FCurrentFrame = FNumFrames) then
  begin
    if (FNumIterations > 0) and (CurrentIteration >= FNumIterations) then
    begin
      SetAnimate(False);
      Exit;
    end;
    Inc(CurrentIteration);
  end;
  NextFrame(FCurrentFrame);
  Inc(FCurrentFrame);
  if (FCurrentFrame > FNumFrames) or (FCurrentFrame <= 0) then
    FCurrentFrame := 1;

  InvalidateRect(WinControl.Handle, @FStretchedRect, True);

  CurrentInterval := Max(Frames[FCurrentFrame].frDelay, 1);
end;

{----------------TGIFImage.SetAnimate}

procedure TGIFImage.SetAnimate(AAnimate: Boolean);
begin
  if AAnimate = FAnimate then
    Exit;

  FAnimate := AAnimate;
  if AAnimate and (FNumFrames > 1) then
  begin
    CurrentInterval := Max(Frames[FCurrentFrame].frDelay, 1);
    LastTime := timeGetTime;
  end;
end;

{----------------TGIFImage.SetCurrentFrame}

procedure TGIFImage.SetCurrentFrame(AFrame: Integer);
begin
  if AFrame = FCurrentFrame then
    Exit;

  NextFrame(FCurrentFrame);
  if AFrame > FNumFrames then
    FCurrentFrame := 1
  else if AFrame < 1 then
    FCurrentFrame := FNumFrames
  else
    FCurrentFrame := AFrame;
  if FAnimated then
    WasDisposal := dtToBackground;
end;

//-- BG ---------------------------------------------------------- 27.08.2015 --
procedure TGIFImage.LoadFromStream(Stream: TStream);
var
  AGif: TGif;
  Frame: TgfFrame;
  I: integer;
  ABitmap, AMask: TBitmap;
begin
  AGif := TGif.Create;
  try
    AGif.LoadFromStream(Stream);

    FNumFrames := AGif.ImageCount;
    FAnimated := FNumFrames > 1;
    FImageWidth := AGif.Width;
    FImageHeight := AGif.Height;
    FNumIterations := AGif.LoopCount;
    if FNumIterations < 0 then {-1 means no loop block}
      FNumIterations := 1
    else if FNumIterations > 0 then
      Inc(FNumIterations); {apparently this is the convention}
    FTransparent := AGif.Transparent;

    Strip := ThtBitmap.Create;
    ABitmap := AGif.GetStripBitmap(AMask);
    try
      Strip.Assign(ABitmap);
      Strip.FMask := AMask;
      Strip.FTransparent := Assigned(AMask);
    finally
      ABitmap.Free;
    end;
    if Strip.Palette <> 0 then
      DeleteObject(Strip.ReleasePalette);
    Strip.Palette := CopyPalette(ThePalette);

    for I := 0 to FNumFrames - 1 do
    begin
      Frame := TgfFrame.Create;
      try
        Frame.frDisposalMethod := TDisposalType(AGif.ImageDisposal[I]);
        Frame.frLeft := AGif.ImageLeft[I];
        Frame.frTop := AGif.ImageTop[I];
        Frame.frWidth := AGif.ImageWidth[I];
        Frame.frHeight := AGif.ImageHeight[I];
        Frame.frDelay := Max(30, AGif.ImageDelay[I] * 10);
      except
        Frame.Free;
        raise;
      end;
      Frames.Add(Frame);
    end;
    if IsAnimated then
      WasDisposal := dtToBackground;
  finally
    AGif.Free;
  end;

  inherited Assign(Strip);
  Width := FImageWidth;
  Transparent := False;
end;

{----------------ThtBitmap.GetMask:}

function ThtBitmap.GetMask: TBitmap;
{This returns mask for frame 1.  Content is black, background is white}
begin
  if not FTransparent then
    Result := nil
  else
    Result := FMask;
end;

{----------------TGIFImage.NextFrame}

procedure TGIFImage.NextFrame(OldFrame: Integer);
begin
  WasDisposal := Frames[OldFrame].frDisposalMethod;
end;

{----------------TgfFrameList.GetFrame}

function TgfFrameList.GetFrame(I: integer): TgfFrame;
begin
  Assert((I <= Count) and (I >= 1), 'Frame index out of range');
  Result := TgfFrame(Items[I - 1]);
end;

{ ThtBitmap }
//var
//  AHandle: THandle;

procedure ThtBitmap.Assign(Source: TPersistent);
var
  htSource: ThtBitmap absolute Source;

begin
  inherited;
  if Source is ThtBitmap then
  begin
    FTransparent := htSource.FTransparent;
    if htSource.FMask = nil then
      FreeAndNil(FMask)
    else
    begin
      if FMask = nil then
        FMask := TBitmap.Create;
      FMask.Assign(htSource.FMask);
    end
  end
  else
  begin
    FTransparent := False;
    FreeAndNil(FMask);
  end;
end;

destructor ThtBitmap.Destroy;
begin
  FMask.Free;
  inherited;
end;

{----------------ThtBitmap.Draw}

procedure ThtBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
  MaskDC: HDC;
  Save: THandle;
begin
  with Rect do
  begin
    //AHandle := ACanvas.Handle; {LDB}
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if Palette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.Handle, Palette, True);
      RealizePalette(ACanvas.Handle);
      RestorePalette := True;
    end;
    BPP := GetDeviceCaps(ACanvas.Handle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.Handle, PLANES);
    DoHalftone := (BPP <= 8) and (PixelFormat in [pf15bit, pf16bit, pf24bit]);
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.Handle, pt);
      SetStretchBltMode(ACanvas.Handle, HALFTONE);
      SetBrushOrgEx(ACanvas.Handle, pt.x, pt.y, @pt);
    end
    else if not Monochrome then
      SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      //AHandle := Canvas.Handle; {LDB}
      if FTransparent then
      begin
        Save := 0;
        MaskDC := 0;
        try
          MaskDC := CreateCompatibleDC(0); {LDB}
          Save := SelectObject(MaskDC, MaskHandle);
          TransparentStretchBlt(ACanvas.Handle, Left, Top, Right - Left,
            Bottom - Top, Canvas.Handle, 0, 0, Width,
            Height, FMask.Canvas.Handle, 0, 0); {LDB}
        finally
          if Save <> 0 then
            SelectObject(MaskDC, Save);
          if MaskDC <> 0 then
            DeleteDC(MaskDC);
        end;
      end
      else
        StretchBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.Handle, 0, 0, Width,
          Height, ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.Handle, OldPalette, True);
    end;
  end;
end;

procedure ThtBitmap.StretchDraw(ACanvas: TCanvas; const DestRect, SrcRect: TRect);
{Draw parts of this bitmap on ACanvas}
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
begin
  with DestRect do
  begin
    //AHandle := ACanvas.Handle; {LDB}
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if Palette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.Handle, Palette, True);
      RealizePalette(ACanvas.Handle);
      RestorePalette := True;
    end;
    BPP := GetDeviceCaps(ACanvas.Handle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.Handle, PLANES);
    DoHalftone := (BPP <= 8) and (PixelFormat in [pf15bit, pf16bit, pf24bit]);
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.Handle, pt);
      SetStretchBltMode(ACanvas.Handle, HALFTONE);
      SetBrushOrgEx(ACanvas.Handle, pt.x, pt.y, @pt);
    end
    else if not Monochrome then
      SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      //AHandle := Canvas.Handle; {LDB}
      if FTransparent then
        TransparentStretchBlt(ACanvas.Handle, Left, Top, Right - Left,
          Bottom - Top, Canvas.Handle,
          SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
          FMask.Canvas.Handle, SrcRect.Left, SrcRect.Top) {LDB}
      else
        StretchBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.Handle,
          SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
          ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.Handle, OldPalette, True);
    end;
  end;
end;

end.