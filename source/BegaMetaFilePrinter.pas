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
  Dialogs, Controls, Forms, Graphics, Math, Printers, Types,
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
    function getHeight: Integer;
    function getWidth: Integer;
    procedure beforeFirstPage(PageWidth, PageHeight: Integer; out PrintWidth, PrintHeight: Integer);
    procedure print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer);
    property Height: Integer read getHeight;
    property Width: Integer read getWidth;
  end;

//------------------------------------------------------------------------------
// BG, 11.03.2006: TBegaPrintable: helps printing controls via TBegaMetaFilePrinter
//------------------------------------------------------------------------------

  TBegaPrintable = class(TInterfacedObject, IBegaPrintable)
  protected
    function getHeight: Integer; virtual; abstract;
    function getWidth: Integer; virtual; abstract;
    procedure beforeFirstPage(PageWidth, PageHeight: Integer; out PrintWidth, PrintHeight: Integer); virtual;
    procedure print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer); virtual; abstract;
  public
    property Height: Integer read getHeight;
    property Width: Integer read getWidth;
  end;

  //BG, 11.03.2006
  TBegaPrintableControl = class(TBegaPrintable, IBegaPrintable)
  private
    FControl: TControl;
  public
    constructor create(ControlToPrint: TControl);
    function getHeight: Integer; override;
    function getWidth: Integer; override;
    procedure print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer); override;
  end;

  //BG, 05.12.2006
  TBegaPrintableGraphic = class(TBegaPrintable, IBegaPrintable)
  private
    FGraphic: TGraphic;
  public
    constructor create(GraphicToPrint: TGraphic);
    function getHeight: Integer; override;
    function getWidth: Integer; override;
    procedure print(Canvas: TCanvas; var PrintedWidth, PrintedHeight: Integer); override;
  end;

//------------------------------------------------------------------------------
// BG, 11.03.2006: TBegaMetaFilePrinter: advanced print previews with TMetaFilePrinter
//------------------------------------------------------------------------------

  //BG, 11.03.2006
  TBegaMetaFilePrinter = class(TMetaFilePrinter)
  private
    FPrintScale: double;  // > 1 enlarges print/preview
    FPrintMargins: TRect; // in millimeters
    FOwnsPages: Boolean;
    function getPreviewPrintArea: TBegaRect;
    function getPrintArea: TBegaRect; // in pixels
    function getPrintMargins: TRect; // FPrintMargins in pixels
    procedure doBeforeFirstPage(out PreviewArea: TBegaRect; out WDpi: Integer; out hrgnClip: HRGN);
    function getPreviewPaperArea: TBegaRect;
    function getPaperArea: TBegaRect;
    function printer2preview(Value: TBegaRect): TBegaRect;
    function printer2previewX(Value: Integer): Integer;
    function printer2previewY(Value: Integer): Integer;
    function getPageCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function getPreviewArea(PrintArea: TBegaRect; WDpi: Integer): TBegaRect;
    procedure addPage(Page: TMetaFile);
    procedure addPages(Source: TBegaMetaFilePrinter; RemovePagesFromSource: Boolean);
    procedure clear;
    procedure copySettingsFrom(Source: TBegaMetaFilePrinter);
    procedure preview(Printable: IBegaPrintable);
    procedure print(Printable: IBegaPrintable; FirstPage: Integer = 1; LastPage: Integer = 9999);
    procedure printDoc(Printer: TPrinter; FirstPage: Integer = 1; LastPage: Integer = 9999; Copies: Integer = 1);
    procedure printTo(Printer: TPrinter; FirstPage: Integer = 1; LastPage: Integer = 9999);
    procedure updatePrinterCaps;
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

procedure TBegaPrintable.beforeFirstPage(
  PageWidth, PageHeight: Integer;
  out PrintWidth, PrintHeight: Integer);
begin
  PrintWidth := Width;
  PrintHeight := Height;
end;

{ TBegaPrintableControl }

//- BG ----------------------------------------------------------- 11.03.2006 --
constructor TBegaPrintableControl.create(ControlToPrint: TControl);
begin
  inherited create;
  FControl := ControlToPrint;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableControl.getHeight: Integer;
begin
  Result := FControl.Height;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableControl.getWidth: Integer;
begin
  Result := FControl.Width;
end;

//- BG ----------------------------------------------------------- 11.03.2006 --
procedure TBegaPrintableControl.print(Canvas: TCanvas;
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
constructor TBegaPrintableGraphic.create(GraphicToPrint: TGraphic);
begin
  inherited create;
  FGraphic := GraphicToPrint;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableGraphic.getHeight: Integer;
begin
  Result := FGraphic.Width;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
function TBegaPrintableGraphic.getWidth: Integer;
begin
  Result := FGraphic.Height;
end;

//- BG ----------------------------------------------------------- 04.12.2006 --
procedure TBegaPrintableGraphic.print(Canvas: TCanvas; var PrintedWidth,
  PrintedHeight: Integer);
begin
  IntersectClipRect(Canvas.Handle, 0, 0, FGraphic.Width, FGraphic.Height);
  TBegaGraphic(FGraphic).Draw(Canvas, Rect(0, 0, FGraphic.Width, FGraphic.Height));
end;

{ TBegaMetaFilePrinter }

//- BG ----------------------------------------------------------- 20.04.2006 --
procedure TBegaMetaFilePrinter.addPage(Page: TMetaFile);
begin
  FMFList.add(Page);
end;

//- BG ----------------------------------------------------------- 20.04.2006 --
procedure TBegaMetaFilePrinter.addPages(Source: TBegaMetaFilePrinter; RemovePagesFromSource: Boolean);
var
  NewCount: Integer;
  Index: Integer;
begin
  NewCount := FMFList.Count + Source.FMFList.Count;
  if FMFList.Capacity < NewCount then
    FMFList.Capacity := NewCount;
  for Index := 0 to Source.FMFList.Count - 1 do
    FMFList.add(Source.FMFList[Index]);
  FOwnsPages := RemovePagesFromSource;
  if RemovePagesFromSource then
    Source.FMFList.clear;
end;

//- BG ----------------------------------------------------------- 29.04.2006 --
procedure TBegaMetaFilePrinter.clear;
begin
  if FOwnsPages then
    FreeMetaFiles
  else
  begin
    FMFList.clear;
    FreeAndNil(FCurCanvas);
  end;
end;

//- BG ----------------------------------------------------------- 29.04.2006 --
procedure TBegaMetaFilePrinter.copySettingsFrom(Source: TBegaMetaFilePrinter);
begin
  FPrintScale := Source.FPrintScale;
  FPrintMargins := Source.FPrintMargins;
  //
  assign(Source);
end;

//- BG ----------------------------------------------------------- 10.03.2006 --
constructor TBegaMetaFilePrinter.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FPrintScale := 1.0;
  FPrintMargins.Left := 20;
  FPrintMargins.Right := 20;
  FPrintMargins.Top := 20;
  FPrintMargins.Bottom := 20;
  FOwnsPages := True;

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
procedure TBegaMetaFilePrinter.doBeforeFirstPage(
  out PreviewArea: TBegaRect;
  out WDpi: Integer;
  out hrgnClip: HRGN);
var
  Area: TBegaRect;
begin
  Area := getPrintArea;
  PreviewArea := printer2preview(Area);
  WDpi := ceil(Screen.PixelsPerInch / PrintScale);
  hrgnClip := CreateRectRgn(
    Area.Left,
    Area.Top,
    Area.Left + Area.Width - 1,
    Area.Top + Area.Height - 1);
end;

//- BG ----------------------------------------------------------- 23.03.2007 --
function TBegaMetaFilePrinter.getPageCount: Integer;
begin
  Result := PageNumber;
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.getPaperArea: TBegaRect;
begin
  Result.Left := 0;
  Result.Top  := 0;
  Result.Width := PaperWidth;
  Result.Height := PaperHeight;
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
function TBegaMetaFilePrinter.getPreviewArea(PrintArea: TBegaRect; WDpi: Integer): TBegaRect;
begin
  Result.Left   := MulDiv(PrintArea.Left  , WDpi, PixelsPerInchX);
  Result.Top    := MulDiv(PrintArea.Top   , WDpi, PixelsPerInchY);
  Result.Width  := MulDiv(PrintArea.Width , WDpi, PixelsPerInchX);
  Result.Height := MulDiv(PrintArea.Height, WDpi, PixelsPerInchY);
end;

//- BG ----------------------------------------------------------- 23.03.2007 --
function TBegaMetaFilePrinter.getPreviewPaperArea: TBegaRect;
begin
  // converts PrinterMargins given in mm to preview pixels
  Result := printer2preview(PaperArea);
end;

//- BG ----------------------------------------------------------- 23.03.2007 --
function TBegaMetaFilePrinter.getPreviewPrintArea: TBegaRect;
begin
  Result := printer2preview(PrintArea)
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
function TBegaMetaFilePrinter.getPrintArea: TBegaRect;
var
  Margins: TRect;
begin
  // printer area without margins in printer pixels
  Margins := getPrintMargins;
  Result.Left   := max(Margins.Left, OffsetX);
  Result.Top    := max(Margins.Top, OffsetY);
  Result.Width  := min(max(PaperWidth - Result.Left - Margins.Right, 0), PageWidth);
  Result.Height := min(max(PaperHeight - Result.Top  - Margins.Bottom, 0), PageHeight);
end;

//- BG ----------------------------------------------------------- 11.03.2006 --
function TBegaMetaFilePrinter.getPrintMargins: TRect;
begin
  // converts PrinterMargins given in mm to printer pixels
  Result.Left   := Floor(FPrintMargins.Left  / 25.4 * PixelsPerInchX);
  Result.Top    := Floor(FPrintMargins.Top   / 25.4 * PixelsPerInchY);
  Result.Right  := Ceil(FPrintMargins.Right  / 25.4 * PixelsPerInchX);
  Result.Bottom := Ceil(FPrintMargins.Bottom / 25.4 * PixelsPerInchY);
end;

//- BG ----------------------------------------------------------- 12.03.2006 --
procedure TBegaMetaFilePrinter.preview(Printable: IBegaPrintable);
var
  DC: HDC;
  Done: boolean;
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
    doBeforeFirstPage(PreviewArea, WDpi, hrgnClip);
    Printable.beforeFirstPage(PreviewArea.Width, PreviewArea.Height, XPixels, YPixels);

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
          FOnPageEvent(Self, FMFList.Count, Done);

        // proceed to next page
        if PrintedWidth > 0 then
          inc(XOrigin, min(PrintedWidth, PreviewArea.Width))
        else
          XOrigin := XPixels;
      end;
      // proceed to next page
      if PrintedHeight > 0 then
        inc(YOrigin, min(PrintedHeight, PreviewArea.Height))
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
procedure TBegaMetaFilePrinter.print(Printable: IBegaPrintable; FirstPage, LastPage: Integer);
var
  CurrentCanvas: TCanvas;
  CurrentPage: Integer;
  DC: HDC;
  Done: boolean;
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
          inc(XOrigin, min(PrintedWidth, PreviewArea.Width))
        else
          XOrigin := XPixels;
      end;
      // proceed to next page
      if PrintedHeight > 0 then
        inc(YOrigin, min(PrintedHeight, PreviewArea.Height))
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
procedure TBegaMetaFilePrinter.printDoc(Printer: TPrinter; FirstPage, LastPage, Copies: Integer);
var
  Index: Integer;
begin
  Printer.BeginDoc;
  try
    for Index := 1 to Copies do
      printTo(Printer, FirstPage, LastPage);
    Printer.EndDoc;
  except
    Printer.Abort;
    raise;
  end;
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.printer2preview(Value: TBegaRect): TBegaRect;
begin
  Result.Left   := printer2previewX(Value.Left);
  Result.Top    := printer2previewY(Value.Top);
  Result.Width  := printer2previewX(Value.Width);
  Result.Height := printer2previewY(Value.Height);
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.printer2previewX(Value: Integer): Integer;
begin
  Result := Round(Value * Screen.PixelsPerInch / (PrintScale * PixelsPerInchX));
end;

//- BG ----------------------------------------------------------- 24.03.2007 --
function TBegaMetaFilePrinter.printer2previewY(Value: Integer): Integer;
begin
  Result := Round(Value * Screen.PixelsPerInch / (PrintScale * PixelsPerInchY));
end;

//- BG ----------------------------------------------------------- 10.04.2006 --
procedure TBegaMetaFilePrinter.printTo(Printer: TPrinter; FirstPage, LastPage: Integer);
var
  CurrentPage: Integer;
  MetaFile: TMetafile;
  Done: Boolean;
  Area: TBegaRect;
begin
  Area := self.PrintArea;
  Area.Left := min(OffsetX, Area.Left);
  Area.Top  := min(OffsetY, Area.Top);
  Done := True;
  if LastPage > FMFList.Count then
    LastPage := FMFList.Count;
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
procedure TBegaMetaFilePrinter.updatePrinterCaps;
begin
  getPrinterCapsOf(Printer);
end;

{$endif NoMetaFile}
end.
