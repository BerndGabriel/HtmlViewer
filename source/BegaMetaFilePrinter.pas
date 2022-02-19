{-------------------------------------------------------------------------------
Copyright (C) 2006-2022 by Bernd Gabriel.

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

unit BegaMetaFilePrinter;

interface
{$ifndef NoMetaFile}

uses
{$ifdef MSWindows}
  Windows,
{$endif}
  Classes, Messages, SysUtils,
{$ifdef LCL}
  LclIntf, LclType, LMessages,
{$endif}
  Dialogs, Controls, Forms, Graphics, Math, Printers, Types, Contnrs,
  //
  MetaFilePrinter;

type
  TBegaMetaFilePrinter = class;

  TBegaRect = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TBegaSize = record
    Width: Integer;
    Height: Integer;
  end;

//------------------------------------------------------------------------------
// BG, 11.03.2006: IBegaPrintable: interface of objects, printable by TBegaMetaFilePrinter
//------------------------------------------------------------------------------

  IBegaPrintable = interface
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure BeforeFirstPage(PageWidth, PageHeight: Integer; out PrintWidth, PrintHeight: Integer);
    procedure Print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer);
    property Height: Integer read getHeight;
    property Width: Integer read getWidth;
  end;

//------------------------------------------------------------------------------
// BG, 11.03.2006: TBegaPrintable: helps printing controls via TBegaMetaFilePrinter
//------------------------------------------------------------------------------

  TBegaPrintable = class(TInterfacedObject, IBegaPrintable)
  protected
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    procedure BeforeFirstPage(PageWidth, PageHeight: Integer; out PrintWidth, PrintHeight: Integer); virtual;
    procedure Print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer); virtual; abstract;
  public
    property Height: Integer read getHeight;
    property Width: Integer read getWidth;
  end;

  //BG, 11.03.2006
  TBegaPrintableControl = class(TBegaPrintable, IBegaPrintable)
  private
    FControl: TControl;
  public
    constructor Create(ControlToPrint: TControl);
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure Print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer); override;
  end;

  //BG, 05.12.2006
  TBegaPrintableGraphic = class(TBegaPrintable, IBegaPrintable)
  private
    FGraphic: TGraphic;
  public
    constructor Create(GraphicToPrint: TGraphic);
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure Print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer); override;
  end;

//------------------------------------------------------------------------------
// BG, 11.03.2006: TBegaMetaFilePrinter: advanced print previews with TMetaFilePrinter
//------------------------------------------------------------------------------

  //BG, 11.03.2006
  TBegaMetaFilePrinter = class(TMetaFilePrinter)
  private
    FPrintScale: double;  // > 1 enlarges print/preview
    FPrintMargins: TRect; // in millimeters
    function GetPreviewPrintArea: TBegaRect;
    function GetPrintArea: TBegaRect; // in pixels
    function GetPrintMargins: TRect; // FPrintMargins in pixels
    procedure DoBeforeFirstPage(out PreviewArea: TBegaRect; out WDpi: Integer; out hrgnClip: HRGN);
    function GetPreviewPaperArea: TBegaRect;
    function GetPaperArea: TBegaRect;
    function Printer2Preview(Value: TBegaRect): TBegaRect;
    function Printer2PreviewX(Value: Integer): Integer;
    function Printer2PreviewY(Value: Integer): Integer;
    function GetPageCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPreviewArea(PrintArea: TBegaRect; WDpi: Integer): TBegaRect;
    procedure AddPage(Page: TMetaFile);
    procedure AddPages(Source: TBegaMetaFilePrinter; RemovePagesFromSource: Boolean);
    procedure Clear;
    procedure CopySettingsFrom(Source: TBegaMetaFilePrinter);
    procedure Preview(Printable: IBegaPrintable);
    procedure Print(Printable: IBegaPrintable; FirstPage: Integer = 1; LastPage: Integer = 9999);
    procedure PrintDoc(Printer: TPrinter; FirstPage: Integer = 1; LastPage: Integer = 9999; Copies: Integer = 1);
    procedure PrintTo(Printer: TPrinter; FirstPage: Integer = 1; LastPage: Integer = 9999);
    procedure UpdatePrinterCaps;
    property PageCount: Integer read getPageCount;
    property PaperArea: TBegaRect read getPaperArea; // in printer pixels
    property PrintArea: TBegaRect read getPrintArea; // in printer pixels
    property PreviewPaperArea: TBegaRect read getPreviewPaperArea; // in screen pixels
    property PreviewPrintArea: TBegaRect read getPreviewPrintArea; // in screen pixels
    property PrintMargins: TRect read FPrintMargins write FPrintMargins; // in millimeters
    property PrintScale: double read FPrintScale write FPrintScale;
  end;

{$endif NoMetaFile}
implementation
{$ifndef NoMetaFile}

{ TBegaPrintable }

procedure TBegaPrintable.BeforeFirstPage(
  PageWidth, PageHeight: Integer;
  out PrintWidth, PrintHeight: Integer);
begin
  PrintWidth := Width;
  PrintHeight := Height;
end;

{ TBegaPrintableControl }

//- BG ----------------------------------------------------------- 11.03.2006 --
constructor TBegaPrintableControl.Create(ControlToPrint: TControl);
begin
  inherited create;
  FControl := ControlToPrint;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableControl.GetHeight: Integer;
begin
  Result := FControl.Height;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableControl.GetWidth: Integer;
begin
  Result := FControl.Width;
end;

//- BG ----------------------------------------------------------- 11.03.2006 --
procedure TBegaPrintableControl.Print(Canvas: TCanvas;
  var PrintedWidth, PrintedHeight: Integer);
begin
  IntersectClipRect(Canvas.Handle, 0, 0, FControl.Width, FControl.Height);
  FControl.Perform(WM_PAINT, Integer(Canvas.Handle), 0);
end;

{ TBegaPrintableGraphic }

type
  TBegaGraphic = class(TGraphic)
  end;

//- BG ----------------------------------------------------------- 04.12.2006 --
constructor TBegaPrintableGraphic.Create(GraphicToPrint: TGraphic);
begin
  inherited Create;
  FGraphic := GraphicToPrint;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableGraphic.GetHeight: Integer;
begin
  Result := FGraphic.Width;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableGraphic.GetWidth: Integer;
begin
  Result := FGraphic.Height;
end;

//- BG ----------------------------------------------------------- 04.12.2006 --
procedure TBegaPrintableGraphic.Print(Canvas: TCanvas; var PrintedWidth,
  PrintedHeight: Integer);
begin
  IntersectClipRect(Canvas.Handle, 0, 0, FGraphic.Width, FGraphic.Height);
  TBegaGraphic(FGraphic).Draw(Canvas, Rect(0, 0, FGraphic.Width, FGraphic.Height));
end;

{ TBegaMetaFilePrinter }

//- BG ----------------------------------------------------------- 20.04.2006 --
procedure TBegaMetaFilePrinter.AddPage(Page: TMetaFile);
begin
  FPages.Add(Page);
end;

//- BG ----------------------------------------------------------- 20.04.2006 --
procedure TBegaMetaFilePrinter.AddPages(Source: TBegaMetaFilePrinter; RemovePagesFromSource: Boolean);
var
  NewCount: Integer;
  Index: Integer;
  SourceOwnsObjects: Boolean;
begin
  NewCount := FPages.Count + Source.FPages.Count;
  if FPages.Capacity < NewCount then
    FPages.Capacity := NewCount;
  for Index := 0 to Source.FPages.Count - 1 do
    FPages.Add(Source.FPages[Index]);
  FPages.OwnsObjects := RemovePagesFromSource;
  if RemovePagesFromSource then
  begin
    SourceOwnsObjects := Source.FPages.OwnsObjects;
    Source.FPages.OwnsObjects := False;
    Source.FPages.Clear;
    Source.FPages.OwnsObjects := SourceOwnsObjects;
  end;
end;

//- BG ----------------------------------------------------------- 29.04.2006 --
procedure TBegaMetaFilePrinter.Clear;
begin
  FPages.Clear;
  FreeAndNil(FCurCanvas);
end;

//- BG ----------------------------------------------------------- 29.04.2006 --
procedure TBegaMetaFilePrinter.CopySettingsFrom(Source: TBegaMetaFilePrinter);
begin
  FPrintScale := Source.FPrintScale;
  FPrintMargins := Source.FPrintMargins;
  //
  Assign(Source);
end;

//- BG ----------------------------------------------------------- 10.03.2006 --
constructor TBegaMetaFilePrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintScale := 1.0;
  FPrintMargins.Left := 20;
  FPrintMargins.Right := 20;
  FPrintMargins.Top := 20;
  FPrintMargins.Bottom := 20;

  try
    if Printer.Printers.Count > 0 then
      GetPrinterCapsOf(Printer);
  except
    on E: EPrinter do
    begin
      Printer.Refresh;
      if Printer.Printers.Count > 0 then
        GetPrinterCapsOf(Printer);
    end;
  end;
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
procedure TBegaMetaFilePrinter.DoBeforeFirstPage(
  out PreviewArea: TBegaRect;
  out WDpi: Integer;
  out hrgnClip: HRGN);
var
  Area: TBegaRect;
begin
  Area := GetPrintArea;
  PreviewArea := Printer2Preview(Area);
  WDpi := Ceil(Screen.PixelsPerInch / PrintScale);
  hrgnClip := CreateRectRgn(
    Area.Left,
    Area.Top,
    Area.Left + Area.Width - 1,
    Area.Top + Area.Height - 1);
end;

//- BG ----------------------------------------------------------- 23.03.2007 --
function TBegaMetaFilePrinter.GetPageCount: Integer;
begin
  Result := PageNumber;
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.GetPaperArea: TBegaRect;
begin
  Result.Left := 0;
  Result.Top  := 0;
  Result.Width := PaperWidth;
  Result.Height := PaperHeight;
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
function TBegaMetaFilePrinter.GetPreviewArea(PrintArea: TBegaRect; WDpi: Integer): TBegaRect;
begin
  Result.Left   := MulDiv(PrintArea.Left  , WDpi, PixelsPerInchX);
  Result.Top    := MulDiv(PrintArea.Top   , WDpi, PixelsPerInchY);
  Result.Width  := MulDiv(PrintArea.Width , WDpi, PixelsPerInchX);
  Result.Height := MulDiv(PrintArea.Height, WDpi, PixelsPerInchY);
end;

//- BG ----------------------------------------------------------- 23.03.2007 --
function TBegaMetaFilePrinter.GetPreviewPaperArea: TBegaRect;
begin
  // converts PrinterMargins given in mm to preview pixels
  Result := Printer2Preview(PaperArea);
end;

//- BG ----------------------------------------------------------- 23.03.2007 --
function TBegaMetaFilePrinter.GetPreviewPrintArea: TBegaRect;
begin
  Result := Printer2Preview(PrintArea)
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
function TBegaMetaFilePrinter.GetPrintArea: TBegaRect;
var
  Margins: TRect;
begin
  // printer area without margins in printer pixels
  Margins := getPrintMargins;
  Result.Left   := Max(Margins.Left, OffsetX);
  Result.Top    := Max(Margins.Top, OffsetY);
  Result.Width  := Min(Max(PaperWidth - Result.Left - Margins.Right, 0), PageWidth);
  Result.Height := Min(Max(PaperHeight - Result.Top  - Margins.Bottom, 0), PageHeight);
end;

//- BG ----------------------------------------------------------- 11.03.2006 --
function TBegaMetaFilePrinter.GetPrintMargins: TRect;
begin
  // converts PrinterMargins given in mm to printer pixels
  Result.Left   := Floor(FPrintMargins.Left  / 25.4 * PixelsPerInchX);
  Result.Top    := Floor(FPrintMargins.Top   / 25.4 * PixelsPerInchY);
  Result.Right  := Ceil(FPrintMargins.Right  / 25.4 * PixelsPerInchX);
  Result.Bottom := Ceil(FPrintMargins.Bottom / 25.4 * PixelsPerInchY);
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
procedure TBegaMetaFilePrinter.Preview(Printable: IBegaPrintable);
var
  DC: HDC;
  Done: Boolean;
  hrgnClip: HRGN;
  PreviewArea: TBegaRect;
  PrintedHeight: Integer;
  PrintedWidth: Integer;
  SaveIndex: Integer;
  WDpi: integer;
  XOrigin: Integer;
  XPixels: Integer;
  YOrigin: Integer;
  YPixels: Integer;
begin
  hrgnClip := 0;
  BeginDoc;
  try
    DoBeforeFirstPage(PreviewArea, WDpi, hrgnClip);
    Printable.BeforeFirstPage(PreviewArea.Width, PreviewArea.Height, XPixels, YPixels);

    Done := False;
    YOrigin := 0;
    while not Done and (YOrigin < YPixels) do
    begin
      XOrigin := 0;
      while not Done and (XOrigin < XPixels) do
      begin
        // first page is created in BeginDoc
        if (XOrigin <> 0) or (YOrigin <> 0) then
          NewPage;

        // prepare canvas
        DC := Canvas.Handle;
        SetMapMode(DC, mm_AnIsotropic);
        SetWindowExtEx(DC, WDpi, WDpi, nil);
        SetViewportExtEx(DC, PixelsPerInchX, PixelsPerInchY, nil);
        SetWindowOrgEx(DC, XOrigin - PreviewArea.Left, YOrigin - PreviewArea.Top, nil);
        SelectClipRgn(DC, hrgnClip);

        // print a page
        Canvas.Lock;
        SaveIndex := SaveDC(DC);
        PrintedWidth := PreviewArea.Width;
        PrintedHeight:= PreviewArea.Height;
        Printable.print(Canvas, PrintedWidth, PrintedHeight);
        RestoreDC(DC, SaveIndex);
        Canvas.Unlock;

        // notify that a page has been printed
        //Application.ProcessMessages;
        if Assigned(FOnPageEvent) then
          FOnPageEvent(Self, FPages.Count, Done);

        // proceed to next page
        if PrintedWidth > 0 then
          Inc(XOrigin, Min(PrintedWidth, PreviewArea.Width))
        else
          XOrigin := XPixels;
      end;
      // proceed to next page
      if PrintedHeight > 0 then
        Inc(YOrigin, Min(PrintedHeight, PreviewArea.Height))
      else
        YOrigin := YPixels;
    end;
  finally
    if hRgnClip <> 0 then
      DeleteObject(hrgnClip);
    EndDoc;
  end;
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
procedure TBegaMetaFilePrinter.Print(Printable: IBegaPrintable; FirstPage, LastPage: Integer);
var
  CurrentCanvas: TCanvas;
  CurrentPage: Integer;
  DC: HDC;
  Done: Boolean;
  hrgnClip: HRGN;
  NoPrintCanvas: TCanvas;
  PreviewArea: TBegaRect;
  PrintedHeight: Integer;
  PrintedWidth: Integer;
  SaveIndex: Integer;
  WDpi: integer;
  XOrigin: Integer;
  XPixels: Integer;
  YOrigin: Integer;
  YPixels: Integer;
begin
  if FirstPage > 1 then
    NoPrintCanvas := TCanvas.Create
  else
    NoPrintCanvas := nil;

  hrgnClip := 0;
  Printer.BeginDoc;
  try
    getPrinterCapsOf(Printer);
    doBeforeFirstPage(PreviewArea, WDpi, hrgnClip);
    Printable.beforeFirstPage(PreviewArea.Width, PreviewArea.Height, XPixels, YPixels);

    Done := False;
    YOrigin := 0;
    CurrentPage := 1;
    while not Done and (YOrigin < YPixels) do
    begin
      XOrigin := 0;
      while not Done and (XOrigin < XPixels) do
      begin
        // first page is created in BeginDoc
        if (XOrigin <> 0) or (YOrigin <> 0) then
        begin
          Printer.NewPage;
          inc(CurrentPage);
        end;
        if CurrentPage < FirstPage then
          CurrentCanvas := NoPrintCanvas
        else
          CurrentCanvas := Printer.Canvas;

        // prepare canvas
        DC := CurrentCanvas.Handle;
        SetMapMode(DC, mm_AnIsotropic);
        SetWindowExtEx(DC, WDpi, WDpi, nil);
        SetViewportExtEx(DC, PixelsPerInchX, PixelsPerInchY, nil);
        SetWindowOrgEx(DC, XOrigin - PreviewArea.Left, YOrigin - PreviewArea.Top, nil);
        SelectClipRgn(DC, hrgnClip);

        // print a page
        CurrentCanvas.Lock;
        SaveIndex := SaveDC(DC);
        PrintedWidth := PreviewArea.Width;
        PrintedHeight:= PreviewArea.Height;
        Printable.print(CurrentCanvas, PrintedWidth, PrintedHeight);
        RestoreDC(DC, SaveIndex);
        CurrentCanvas.Unlock;

        // notify that a page has been printed
        //Application.ProcessMessages;
        if Assigned(FOnPageEvent) then
          FOnPageEvent(Self, CurrentPage, Done);

        // proceed to next page
        if CurrentPage >= LastPage then
          Done := True
        else if PrintedWidth > 0 then
          Inc(XOrigin, Min(PrintedWidth, PreviewArea.Width))
        else
          XOrigin := XPixels;
      end;
      // proceed to next page
      if PrintedHeight > 0 then
        Inc(YOrigin, Min(PrintedHeight, PreviewArea.Height))
      else
        YOrigin := YPixels;
    end;
  finally
    NoPrintCanvas.free;
    if hRgnClip <> 0 then
      DeleteObject(hrgnClip);
    Printer.EndDoc;
  end;
end;

//- BG ----------------------------------------------------------- 27.05.2006 --
procedure TBegaMetaFilePrinter.PrintDoc(Printer: TPrinter; FirstPage, LastPage, Copies: Integer);
var
  Index: Integer;
begin
  Printer.BeginDoc;
  try
    for Index := 1 to Copies do
      PrintTo(Printer, FirstPage, LastPage);
    Printer.EndDoc;
  except
    Printer.Abort;
    raise;
  end;
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.Printer2Preview(Value: TBegaRect): TBegaRect;
begin
  Result.Left   := Printer2PreviewX(Value.Left);
  Result.Top    := Printer2PreviewY(Value.Top);
  Result.Width  := Printer2PreviewX(Value.Width);
  Result.Height := Printer2PreviewY(Value.Height);
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.Printer2PreviewX(Value: Integer): Integer;
begin
  Result := Round(Value * Screen.PixelsPerInch / (PrintScale * PixelsPerInchX));
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.Printer2PreviewY(Value: Integer): Integer;
begin
  Result := Round(Value * Screen.PixelsPerInch / (PrintScale * PixelsPerInchY));
end;

//- BG ----------------------------------------------------------- 10.04.2006 --
procedure TBegaMetaFilePrinter.PrintTo(Printer: TPrinter; FirstPage, LastPage: Integer);
var
  CurrentPage: Integer;
  MetaFile: TMetafile;
  Done: Boolean;
  Area: TBegaRect;
begin
  Area := self.PrintArea;
  Area.Left := Min(OffsetX, Area.Left);
  Area.Top  := Min(OffsetY, Area.Top);
  Done := True;
  if LastPage > FPages.Count then
    LastPage := FPages.Count;
  for CurrentPage := FirstPage to LastPage do
  begin
    MetaFile := MetaFiles[CurrentPage - 1];
    Printer.Canvas.StretchDraw(
      Rect(
        -Area.Left,
        -Area.Top,
        Printer.PageWidth - Area.Left,
        Printer.PageHeight - Area.Top),
      MetaFile);

    // notify that a page has been printed
    if Assigned(FOnPageEvent) then
    begin
      FOnPageEvent(Self, CurrentPage, Done);
      if Done then
        break;
    end;

    if CurrentPage < LastPage then
      Printer.NewPage;
  end;
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaMetaFilePrinter.UpdatePrinterCaps;
begin
  GetPrinterCapsOf(Printer);
end;

{$endif NoMetaFile}
end.
