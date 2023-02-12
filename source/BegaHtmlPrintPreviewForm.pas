{-------------------------------------------------------------------------------
Copyright (C) 2006-2023 by Bernd Gabriel.

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

unit BegaHtmlPrintPreviewForm;

{$include htmlcons.inc}

interface
{$ifndef NoFlatScrollbars}
{$ifndef NoMetaFile}

uses
{$ifdef LCL}
  LclIntf, LclType, PrintersDlgs,
{$else}
  Windows,
{$endif}
  Controls, Forms, Types, Printers, //Math,
  // shared units
  HtmlView,
  FramView,
  // own units
  BegaMetaFilePrinter,
  BegaPreview,
  BegaPreviewForm;

type
  TBegaHtmlPrintPreviewForm = class(TBegaCustomPrintPreviewForm)
  private
    FFrameViewer: TFVBase; // TFrameViewer or TFrameBrowser
    FHtmlViewer: THtmlViewer;
    function CanPrint: Boolean;
    function CurrentViewer: THtmlViewer;
    function PreviewGetSize(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; out Width, Height: Integer): Boolean;
    procedure PreviewCreatePages(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; var Done: Boolean);
    procedure PreviewPrintPages(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; FirstPage, LastPage, Copies: Integer);
    procedure SetFrameViewer(const Value: TFVBase);
    procedure SetHtmlViewer(const Value: THtmlViewer);
    procedure UpdateEvents;
  public
    property FrameViewer: TFVBase read FFrameViewer write setFrameViewer;
    property HtmlViewer: THtmlViewer read FHtmlViewer write setHtmlViewer;
  end;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
implementation
{$ifndef NoFlatScrollbars}
{$ifndef NoMetaFile}

//- BG ----------------------------------------------------------- 26.03.2007 --
function TBegaHtmlPrintPreviewForm.canPrint: Boolean;
begin
  Result := poPrintEnabled in Preview.PreviewOptions;
end;

//- BG ----------------------------------------------------------- 26.03.2007 --
function TBegaHtmlPrintPreviewForm.CurrentViewer: THtmlViewer;
begin
  if FHtmlViewer <> nil then
    Result := FHtmlViewer
  else if FFrameViewer <> nil then
    Result := FFrameViewer.ActiveViewer
  else
    Result := nil;
end;

//- BG ----------------------------------------------------------- 26.03.2007 --
procedure TBegaHtmlPrintPreviewForm.PreviewCreatePages(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; var Done: Boolean);
var
  OldCursor: TCursor;
  Viewer: THtmlViewer;
begin
  Viewer := CurrentViewer;
  if canPrint and (Viewer <> nil) then
  begin
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      MFPrinter.PrintMargins := Rect(
        round(Viewer.PrintMarginLeft   * 10.0),
        round(Viewer.PrintMarginTop    * 10.0),
        round(Viewer.PrintMarginRight  * 10.0),
        round(Viewer.PrintMarginBottom * 10.0));
      Viewer.PrintScale := 1.0 / MFPrinter.PrintScale;
      Viewer.PrintPreview(MFPrinter);
      Done := True;
    finally
      Screen.Cursor := OldCursor;
    end;
  end;
end;

//- BG ----------------------------------------------------------- 14.03.2006 --
function TBegaHtmlPrintPreviewForm.PreviewGetSize(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; out Width, Height: Integer): Boolean;
var
  OldCursor: TCursor;
  Viewer: THtmlViewer;
begin
  Viewer := CurrentViewer;
  Result := canPrint and (Viewer <> nil);
  if Result then
  begin
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      MFPrinter.PrintMargins := Rect(
        round(Viewer.PrintMarginLeft   * 10.0),
        round(Viewer.PrintMarginTop    * 10.0),
        round(Viewer.PrintMarginRight  * 10.0),
        round(Viewer.PrintMarginBottom * 10.0));
      Viewer.PrintScale := 1.0 / MFPrinter.PrintScale;
      Viewer.NumPrinterPages(MFPrinter, Width, Height);
    finally
      Screen.Cursor := OldCursor;
    end;
  end;
end;

//- BG ----------------------------------------------------------- 30.03.2007 --
procedure TBegaHtmlPrintPreviewForm.PreviewPrintPages(Sender: TObject;
  MFPrinter: TBegaMetaFilePrinter; FirstPage, LastPage, Copies: Integer);
var
  OldCursor: TCursor;
  Viewer: THtmlViewer;
  CopyNo: Integer;
begin
  Viewer := CurrentViewer;
  if canPrint and (Viewer <> nil) then
  begin
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      for CopyNo := 1 to Copies do
        Viewer.Print(FirstPage, LastPage);
    finally
      Screen.Cursor := OldCursor;
    end;
  end;
end;

//- BG ----------------------------------------------------------- 26.03.2007 --
procedure TBegaHtmlPrintPreviewForm.SetFrameViewer(const Value: TFVBase);
begin
  FFrameViewer := Value;
  FHtmlViewer := nil;
  UpdateEvents;
  Preview.createPages;
end;

//- BG ----------------------------------------------------------- 26.03.2007 --
procedure TBegaHtmlPrintPreviewForm.SetHtmlViewer(
  const Value: THtmlViewer);
begin
  FFrameViewer := nil;
  FHtmlViewer := Value;
  UpdateEvents;
  Preview.createPages;
end;

//- BG ----------------------------------------------------------- 27.03.2007 --
procedure TBegaHtmlPrintPreviewForm.UpdateEvents;
begin
  if CurrentViewer <> nil then
  begin
    Preview.OnGetContentSize := PreviewGetSize;
    Preview.OnCreatePages := PreviewCreatePages;
    Preview.OnPrintPages := PreviewPrintPages;
  end
  else
  begin
    Preview.OnGetContentSize := nil;
    Preview.OnCreatePages := nil;
    Preview.OnPrintPages := nil;
  end;
end;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
end.
