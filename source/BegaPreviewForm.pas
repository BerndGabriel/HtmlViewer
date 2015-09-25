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

unit BegaPreviewForm;

interface
{$ifndef NoFlatScrollbars}
{$ifndef NoMetaFile}

uses
  Classes, SysUtils,
{$ifdef LCL}
  LclIntf, LclType, PrintersDlgs,
{$else}
  Windows,
{$endif}
  Variants, Graphics, Controls, Forms, Dialogs,
  // own units
  BegaMetaFilePrinter,
  BegaPreview;

type
  TBegaCustomPrintPreviewForm = class(TForm)
  private
    FMargins: TRect;
  protected
    procedure UpdateActions; override;
  public
    Preview: TBegaPreviewFrame;
    constructor Create(AOwner: TComponent); override;
    property PrintMargins: TRect read FMargins write FMargins; // in mm
  end;

  TBegaPrintPreviewForm = class(TBegaCustomPrintPreviewForm)
  private
    FPrintable: IBegaPrintable;
    function PreviewGetSize(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; out Width, Height: Integer): Boolean;
    procedure PreviewCreatePages(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; var Done: Boolean);
    procedure setPrintable(const Value: IBegaPrintable);
  public
    property PrintableIntf: IBegaPrintable read FPrintable write setPrintable;
  end;

  TBegaMetaFilePrintPreviewForm = class(TBegaCustomPrintPreviewForm)
  private
    FMetafilePrinter: TBegaMetaFilePrinter;
    procedure PreviewCreatePages(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; var Done: Boolean);
    procedure setMetafilePrinter(const Value: TBegaMetaFilePrinter);
  public
    property MetafilePrinter: TBegaMetaFilePrinter read FMetafilePrinter write setMetafilePrinter;
  end;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
implementation
{$ifndef NoFlatScrollbars}
{$ifndef NoMetaFile}

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

{ TBegaPreviewForm }

//- BG ----------------------------------------------------------- 03.11.2006 --
constructor TBegaCustomPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited;
  Preview := TBegaPreviewFrame.create(Self);
  Preview.Parent := self;
  Preview.Align := alClient;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
procedure TBegaCustomPrintPreviewForm.UpdateActions;
begin
  inherited;
  Preview.updateActions;
end;

{ TBegaPrintPreviewForm }

//- BG ----------------------------------------------------------- 04.12.2006 --
procedure TBegaPrintPreviewForm.PreviewCreatePages(Sender: TObject;
  MFPrinter: TBegaMetaFilePrinter; var Done: Boolean);
begin
  MFPrinter.PrintMargins := PrintMargins;
  MFPrinter.preview(FPrintable);
  Done := True;
end;

//- BG ----------------------------------------------------------- 04.12.2006 --
function TBegaPrintPreviewForm.PreviewGetSize(Sender: TObject;
  MFPrinter: TBegaMetaFilePrinter; out Width, Height: Integer): Boolean;
begin
  Width := FPrintable.Width;
  Height := FPrintable.Height;
  Result := True;
end;

//- BG ----------------------------------------------------------- 05.12.2006 --
procedure TBegaPrintPreviewForm.setPrintable(const Value: IBegaPrintable);
begin
  if FPrintable <> Value then
  begin
    FPrintable := Value;
    if Value = nil then
    begin
      Preview.OnCreatePages := nil;
      Preview.OnGetContentSize := nil;
    end
    else
    begin
      Preview.OnCreatePages := PreviewCreatePages;
      Preview.OnGetContentSize := PreviewGetSize;
    end;
  end;
end;

{ TBegaMetaFilePrintPreviewForm }

//- BG ----------------------------------------------------------- 22.03.2007 --
procedure TBegaMetaFilePrintPreviewForm.PreviewCreatePages(Sender: TObject;
  MFPrinter: TBegaMetaFilePrinter; var Done: Boolean);
begin
  MFPrinter.addPages(FMetafilePrinter, False);
  MFPrinter.copySettingsFrom(FMetafilePrinter);
  Done := True;
end;

//- BG ----------------------------------------------------------- 22.03.2007 --
procedure TBegaMetaFilePrintPreviewForm.setMetafilePrinter(
  const Value: TBegaMetaFilePrinter);
begin
  if FMetafilePrinter <> Value then
  begin
    FMetafilePrinter := Value;
    if Value = nil then
    begin
      Preview.OnCreatePages := nil;
    end
    else
    begin
      Preview.OnCreatePages := PreviewCreatePages;
    end;
  end;
  Preview.createPages;
end;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
end.
