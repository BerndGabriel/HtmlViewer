{
Version   11
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
  TRGBColor = packed record
    Red,
      Green,
      Blue: Byte;
  end;

  TDisposalType = (dtUndefined, {Take no action}
    dtDoNothing, {Leave graphic, next frame goes on top of it}
    dtToBackground, {restore original background for next frame}
    dtToPrevious); {restore image as it existed before this frame}

type
  ThtBitmap = class(TBitmap)
  protected
    htMask: TBitmap;
    htTransparent: boolean;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure StretchDraw(ACanvas: TCanvas; const DestRect,
      SrcRect: TRect);
  public
    destructor Destroy; override;
  end;

  TGIFImage = class;

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

    IsCopy: boolean;

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

  TGIFImage = class(TPersistent)
  private
    { Private declarations }
    FAnimated: Boolean;
    FCurrentFrame: Integer;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FNumFrames: Integer;
    FNumIterations: Integer;
    FTransparent: Boolean;
    FVisible: Boolean;
    Strip: ThtBitmap;

    TheEnd: boolean; {copy to here}

    FBitmap: TBitmap;
    FMaskedBitmap, FMask: TBitmap;
    FAnimate: Boolean;
    FStretchedRect: TRect;
    WasDisposal: TDisposalType;

    Frames: TgfFrameList;

    CurrentIteration: Integer;
    LastTime: DWord;
    CurrentInterval: DWord;

    procedure SetAnimate(AAnimate: Boolean);
    procedure SetCurrentFrame(AFrame: Integer);
    function GetMaskedBitmap: TBitmap;
    function GetMask: TBitmap;
    function GetBitMap: TBitmap;

    procedure NextFrame(OldFrame: Integer);

  public
    ShowIt: boolean;
    IsCopy: boolean; {set if this is a copy of one in Cache}

    { Public declarations }
    constructor Create;
    constructor CreateCopy(Item: TGIFImage);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X, Y, Wid, Ht: integer);
    property Bitmap: TBitmap read GetBitmap;
    property MaskedBitmap: TBitmap read GetMaskedBitmap;
    property Mask: TBitmap read GetMask;
    property IsAnimated: Boolean read FAnimated;
    property IsTransparent: Boolean read FTransparent;
    property NumFrames: Integer read FNumFrames;
    property NumIterations: Integer read FNumIterations;

    procedure CheckTime(WinControl: TWinControl);

    property Width: integer read FImageWidth;
    property Height: integer read FImageHeight;
    property Animate: Boolean read FAnimate write SetAnimate;
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    property Visible: Boolean read FVisible write FVisible;
  end;

function CreateAGifFromStream(Stream: TStream): TGifImage;
// BG, 01.04.2012: unused: function CreateAGif(const Name: string; var NonAnimated: boolean): TGifImage;

implementation

function CreateBitmap(Width, Height: integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
end;

function CreateAGifFromStream(Stream: TStream): TGifImage;
var
  AGif: TGif;
  Frame: TgfFrame;
  I: integer;
  ABitmap, AMask: TBitmap;
begin
  AGif := TGif.Create;
  try
    AGif.LoadFromStream(Stream);
    Result := TGifImage.Create;
    try
      Result.FNumFrames := AGif.ImageCount;
      Result.FAnimated := Result.FNumFrames > 1;
      Result.FImageWidth := AGif.Width;
      Result.FImageHeight := AGif.Height;
      Result.FNumIterations := AGif.LoopCount;
      if Result.FNumIterations < 0 then {-1 means no loop block}
        Result.FNumIterations := 1
      else if Result.FNumIterations > 0 then
        Inc(Result.FNumIterations); {apparently this is the convention}
      Result.FTransparent := AGif.Transparent;

      with Result do
      begin
        Strip := ThtBitmap.Create;
        ABitmap := AGif.GetStripBitmap(AMask);
        try
          Strip.Assign(ABitmap);
          Strip.htMask := AMask;
          Strip.htTransparent := Assigned(AMask);
        finally
          ABitmap.Free;
        end;
        if Result.Strip.Palette <> 0 then
          DeleteObject(Result.Strip.ReleasePalette);
        Result.Strip.Palette := CopyPalette(ThePalette);
      end;

      for I := 0 to Result.FNumFrames - 1 do
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
        Result.Frames.Add(Frame);
      end;
      if Result.IsAnimated then
        Result.WasDisposal := dtToBackground;
    except
      Result.Free;
      raise;
    end;
  finally
    AGif.Free;
  end;
end;

// BG, 01.04.2012: unused:
//function CreateAGif(const Name: string; var NonAnimated: boolean): TGifImage;
//var
//  Stream: TFileStream;
//begin
//  Result := nil;
//  try
//    Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
//    try
//      Result := CreateAGifFromStream(NonAnimated, Stream);
//    finally
//      Stream.Free;
//    end;
//  except
//  end;
//end;

{----------------TgfFrame.Create}

constructor TgfFrame.Create;
begin
  inherited Create;
end;

constructor TgfFrame.CreateCopy(Item: TgfFrame);
begin
  inherited Create;
  System.Move(Item.frLeft, frLeft, PtrSub(@TheEnd, @frLeft));
  IsCopy := True;
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
  FImageWidth := Item.Width;
  FimageHeight := Item.Height;
  System.Move(Item.FAnimated, FAnimated, PtrSub(@TheEnd, @FAnimated));
  IsCopy := True;

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
  FreeAndNil(FBitmap);
  if not IsCopy then
    FreeAndNil(Strip);
  FMaskedBitmap.Free;
  FreeAndNil(FMask);
  inherited Destroy;
end;

{----------------TGIFImage.Draw}

procedure TGIFImage.Draw(Canvas: TCanvas; X, Y, Wid, Ht: integer);
var
  SRect: TRect;
  ALeft: integer;
begin
  if (FVisible) and (FNumFrames > 0) then
  begin
    FStretchedRect := Rect(X, Y, X + Wid, Y + Ht);
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

{----------------TGIFImage.GetBitmap}

function TGIFImage.GetBitmap: TBitmap;
begin
  Result := GetMaskedBitmap;
end;

{----------------TGIFImage.GetMaskedBitmap:}

function TGIFImage.GetMaskedBitmap: TBitmap;
{This returns frame 1}
begin
  if not Assigned(FMaskedBitmap) then
  begin
    FMaskedBitmap := TBitmap.Create;
    FMaskedBitmap.Assign(Strip);
    FMaskedBitmap.Width := FImageWidth;
    if Strip.htTransparent then
    begin
      FMask := TBitmap.Create;
      FMask.Assign(Strip.htMask);
      FMask.Width := FImageWidth;
    end;
    FMaskedBitmap.Transparent := False;
  end;
  Result := FMaskedBitmap;
end;

{----------------TGIFImage.GetMask:}

function TGIFImage.GetMask: TBitmap;
{This returns mask for frame 1.  Content is black, background is white}
begin
  if not FTransparent then
    Result := nil
  else
  begin
    if not Assigned(FMask) then
      GetMaskedBitmap;
    Result := FMask;
  end;
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

destructor ThtBitmap.Destroy;
begin
  htMask.Free;
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
      if htTransparent then
      begin
        Save := 0;
        MaskDC := 0;
        try
          MaskDC := CreateCompatibleDC(0); {LDB}
          Save := SelectObject(MaskDC, MaskHandle);
          TransparentStretchBlt(ACanvas.Handle, Left, Top, Right - Left,
            Bottom - Top, Canvas.Handle, 0, 0, Width,
            Height, htMask.Canvas.Handle, 0, 0); {LDB}
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
      if htTransparent then
        TransparentStretchBlt(ACanvas.Handle, Left, Top, Right - Left,
          Bottom - Top, Canvas.Handle,
          SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
          htMask.Canvas.Handle, SrcRect.Left, SrcRect.Top) {LDB}
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
