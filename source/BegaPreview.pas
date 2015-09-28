{-------------------------------------------------------------------------------
Copyright (C) 2006-2014 by Bernd Gabriel.

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

unit BegaPreview;

interface
{$ifndef NoFlatScrollbars}
{$ifndef NoMetaFile}

uses
  Classes, SysUtils,
{$ifdef LCL}
  LclIntf, LclType, PrintersDlgs,
{$else}
  Windows, Messages, ImgList, ToolWin,
{$endif}
  Graphics, Controls, Forms, Math,
  StdCtrls, ExtCtrls, Buttons, Printers, Spin, ComCtrls,
  Dialogs,
  // shared units
  MetaFilePrinter,
  // own units
  BegaMetaFilePrinter,
  BegaPreviewPanel,
  BegaZoom, BegaScrollBox;

const
   crZoom = 40;
   crHandDrag = 41;
   ZOOMFACTOR = 1.11111111111111111111;


//- BG ----------------------------------------------------------- 22.12.2006 --
// use actions and you get all what you want! visible and enabled and event!
type
  TBegaPreviewOption = (
    poPrinterSetupEnabled,
    poPrintEnabled,
    poAllOnOneEnabled,
    poSaveFileEnabled,
    poSendMailEnabled,
    poOpenInExcelEnabled,
    poEditReportPropertiesEnabled,
    poContentZoomVisible);
  TBegaPreviewOptions = set of TBegaPreviewOption;
const
  CBegaPreviewOptionsDefault = [
    poPrinterSetupEnabled,
    poPrintEnabled,
    poAllOnOneEnabled,
    poSaveFileEnabled,
    poSendMailEnabled,
    poContentZoomVisible];

type
  TBegaEditEvent = procedure(Sender: TObject; var Edited: Boolean) of Object;
  TBegaAllOnOneEvent = procedure(Sender: TObject; const AllOnOne: Boolean) of Object;
  TBegaGetPageEvent = function(Sender: TObject; PageNo: Integer): String of object;
  TBegaMetaFileGetSizeEvent = function(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; out Width, Height: Integer): Boolean of object;
  TBegaMetaFilePrinterEvent = procedure(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; var Done: Boolean) of object;
  TBegaMetaFilePrintPagesEvent = procedure(Sender: TObject; MFPrinter: TBegaMetaFilePrinter; FirstPage, LastPage, Copies: Integer) of object;
  TBegaMetaFileSetupEvent = procedure(Sender: TObject; out Changed: Boolean) of object;

  TBegaCustomPreviewFrame = class(TFrame)
    AllOnOne: TToolButton;
    cboxPage: TComboBox;
    ContentFitHeight: TToolButton;
    ContentFitWidth: TToolButton;
    ContentZoomBox: TComboBox;
    ContentZoomLabel: TPanel;
    CurrentPageLabel: TPanel;
    FirstPage: TToolButton;
    FitHeight: TToolButton;
    FitWidth: TToolButton;
    Grid: TToolButton;
    HintLabel: TLabel;
    imgList: TImageList;
    ContentZoomBoxLabel: TLabel;
    Label2: TLabel;
    LastPage: TToolButton;
    NextPage: TToolButton;
    OpenInExcel: TToolButton;
    Panel1: TPanel;
    Panning: TToolButton;
    PreviewZoomLabel: TPanel;
    PrevPage: TToolButton;
    Print: TToolButton;
    PrintDialog: TPrintDialog;
    PrinterSetup: TToolButton;
    PrinterSetupDialog: TPrinterSetupDialog;
    SaveFile: TToolButton;
    SendMail: TToolButton;
    StatBarPanel: TPanel;
    ToolBar: TToolBar;
    ContentZoomSeparator: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton9: TToolButton;
    UnitsBox: TComboBox;
    ZoomBox: TComboBox;
    Zooming: TToolButton;
    ReportProperties: TToolButton;
    PagesPerRow: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    RowsPerPreview: TSpinEdit;
    PreviewBox: TBegaZoomBox;
    PreviewPanel: TBegaMetafilePreviewPanel;
    procedure AllOnOneClick(Sender: TObject);
    procedure ContentFitHeightClick(Sender: TObject);
    procedure ContentFitWidthClick(Sender: TObject);
    procedure ContentZoomBoxChange(Sender: TObject);
    procedure FirstPageClick(Sender: TObject);
    procedure FitHeightClick(Sender: TObject);
    procedure FitWidthClick(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure LastPageClick(Sender: TObject);
    procedure NextPageClick(Sender: TObject);
    procedure OpenInExcelClick(Sender: TObject);
    procedure PageChange(Sender: TObject);
    procedure PagesPerRowChange(Sender: TObject);
    procedure PanningClick(Sender: TObject);
    procedure PreviewBoxResize(Sender: TObject);
    procedure PreviewPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PreviewPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PreviewPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PreviewPanelUpdatePreview(Sender: TBegaCustomPreviewPanel; var Width, Height: Integer);
    procedure PrevPageClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure PrinterSetupClick(Sender: TObject);
    procedure ReportPropertiesClick(Sender: TObject);
    procedure RowsPerPreviewChange(Sender: TObject);
    procedure SaveFileClick(Sender: TObject);
    procedure SendMailClick(Sender: TObject);
    procedure UnitsBoxChange(Sender: TObject);
    procedure ZoomBoxChange(Sender: TObject);
    procedure ZoomingClick(Sender: TObject);
    procedure PreviewPanelPaintPage(Sender: TBegaCellPreviewPanel; Canvas: TCanvas; PageIndex: Integer);
  private
    FMFPrinter: TBegaMetaFilePrinter;
    FOnAllOnOne: TBegaAllOnOneEvent;
    FOnCreatePages: TBegaMetaFilePrinterEvent;
    FOnEditReportProperties: TBegaEditEvent;
    FOnGetContentSize: TBegaMetaFileGetSizeEvent;
    FOnGetPageTitle: TBegaGetPageEvent;
    FOnOpenInExcel: TNotifyEvent;
    FOnPrinterSetup: TBegaMetaFileSetupEvent;
    FOnPrintPages: TBegaMetaFilePrintPagesEvent;
    FOnSaveFile: TNotifyEvent;
    FOnSendMail: TNotifyEvent;
    FOptions: TBegaPreviewOptions;
    FZoom: double;
    Moving: Boolean;
    function getContentSize(out ContentSize: TBegaSize): Boolean;
    function getLastAvailablePage: Integer;
    function getPage(Index: Integer): TMetafile;
    function getPageCount: Integer;
    function getVisiblePageCount: Integer;
    function PixelsPerInch: Integer;
    procedure printPages(FirstPage, LastPage, Copies: Integer);
    procedure setCurrentPage(Val: integer);
    procedure setOptions(const Value: TBegaPreviewOptions);
    procedure setContentZoomMode(Zoom: TBegaZoomMode);
    procedure setPreviewZoomMode(Zoom: TBegaZoomMode);
    property LastAvailablePage: Integer read getLastAvailablePage; // due to visible page count
    property VisiblePageCount: Integer read getVisiblePageCount;
    property Zoom: Double read FZoom write FZoom;
    function getContentZoomMode: TBegaZoomMode;
    function getPreviewZoomMode: TBegaZoomMode;
    function getCurrentPage: Integer;
  protected
    procedure Loaded; override;
    //
    function buttonEnabled(Button: TBegaPreviewOption): Boolean; virtual;
    // doGetSize() calculates size of all pages, which enables content zoom.
    function canGetContentSize: Boolean; virtual;
    procedure doGetContentSize(var Done: Boolean; out Width, Height: Integer); virtual;
    // doCreatePageMenu() populates MFPrinter with pages
    procedure doCreatePageMenu(var Done: Boolean); virtual;
    // doCreatePages() populates MFPrinter with pages
    procedure doCreatePages(var Done: Boolean); virtual;
    // doPrintPages() prints pages of MFPrinter or do you prefer printing elsehow?
    procedure doPrintPages(var Done: Boolean); virtual;
    // doPrinterSetup(): override or use OnPrinterSetup to reduce number of printer setup dialogs:
    procedure doPrinterSetup(var Done, Changed: Boolean); virtual;
    //
    procedure doAllOnOne(var Done: Boolean); virtual;
    //
    procedure doOpenInExcel(var Done: Boolean); virtual;
    //
    procedure updateBox;
    procedure updateContentZoom;
    procedure updatePage;
    procedure updatePageMenu;
    property CurrentPage: Integer read getCurrentPage write SetCurrentPage; // 1..n
    property MFPrinter: TBegaMetaFilePrinter read FMFPrinter;
    property OnAllOnOne: TBegaAllOnOneEvent read FOnAllOnOne write FOnAllOnOne;
    property OnCreatePages: TBegaMetaFilePrinterEvent read FOnCreatePages write FOnCreatePages;
    property OnEditReportProperties: TBegaEditEvent read FOnEditReportProperties write FOnEditReportProperties;
    property OnGetContentSize: TBegaMetaFileGetSizeEvent read FOnGetContentSize write FOnGetContentSize;
    property OnGetPageTitle: TBegaGetPageEvent read FOnGetPageTitle write FOnGetPageTitle;
    property OnOpenInExcel: TNotifyEvent read FOnOpenInExcel write FOnOpenInExcel;
    property OnPrinterSetup: TBegaMetaFileSetupEvent read FOnPrinterSetup write FOnPrinterSetup;
    property OnPrintPages: TBegaMetaFilePrintPagesEvent read FOnPrintPages write FOnPrintPages;
    property OnSaveFile: TNotifyEvent read FOnSaveFile write FOnSaveFile;
    property OnSendMail: TNotifyEvent read FOnSendMail write FOnSendMail;
    property PageCount: Integer read getPageCount;
    property Pages[Index: Integer]: TMetafile read getPage;
    property PreviewOptions: TBegaPreviewOptions read FOptions write setOptions default CBegaPreviewOptionsDefault;
    property ContentZoomMode: TBegaZoomMode read getContentZoomMode write setContentZoomMode stored false; // is stored in ContentZoomBox
    property PreviewZoomMode: TBegaZoomMode read getPreviewZoomMode write setPreviewZoomMode stored false; // is stored in ZoomBox
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure createPages;
    procedure updateActions;
  end;

  //BG, 20.04.2006: split: TBegaPreviewFrame
  TBegaPreviewFrame = class(TBegaCustomPreviewFrame)
  published
    property ContentZoomMode;
    property OnCreatePages;
    property OnEditReportProperties;
    property OnGetContentSize;
    property OnGetPageTitle;
    property OnOpenInExcel;
    property OnPrinterSetup;
    property OnPrintPages;
    property OnSaveFile;
    property OnSendMail;
    property PreviewOptions;
    property PreviewZoomMode;
  end;

procedure loadPreviewCursors;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
implementation
{$ifndef NoFlatScrollbars}
{$ifndef NoMetaFile}

{$ifdef UseVCLStyles}
uses
  System.Types,
  System.UITypes,
  Vcl.Themes;
{$endif}

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$R ZoomAndHandCursor.res}
{$endif}

//- BG ----------------------------------------------------------- 04.11.2006 --
var PreviewCursorsLoaded: Boolean;
procedure loadPreviewCursors;
begin
  if not PreviewCursorsLoaded then
  begin
    PreviewCursorsLoaded := True;
{$ifndef LCL}
    Screen.Cursors[crZoom] := LoadCursor(hInstance, 'BEGA_ZOOM_CURSOR');
    Screen.Cursors[crHandDrag] := LoadCursor(hInstance, 'BEGA_HAND_CURSOR');
{$endif}
  end;
end;

//- BG ----------------------------------------------------------- 04.11.2006 --
function zoom2ZoomMode(Zoom: Double): TBegaZoomMode;
begin
  case round(Zoom * 100) of
     25: Result := zm25;
     50: Result := zm50;
     75: Result := zm75;
    100: Result := zm100;
    125: Result := zm125;
    150: Result := zm150;
    200: Result := zm200;
    300: Result := zm300;
    400: Result := zm400;
  else
    Result := zmCustom;
  end;
end;

//- BG ----------------------------------------------------------- 25.05.2006 --
function TBegaCustomPreviewFrame.buttonEnabled(
  Button: TBegaPreviewOption): Boolean;
begin
  Result := Button in PreviewOptions;
end;

//- BG ----------------------------------------------------------- 20.04.2006 --
function TBegaCustomPreviewFrame.canGetContentSize: Boolean;
begin
  Result := assigned(FOnGetContentSize);
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.ContentFitHeightClick(Sender: TObject);
begin
  ContentZoomBox.ItemIndex := 1;
  updateContentZoom;
  createPages;
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.ContentFitWidthClick(Sender: TObject);
begin
  ContentZoomBox.ItemIndex := 2;
  updateContentZoom;
  createPages;
end;

//- BG ----------------------------------------------------------- 14.03.2006 --
procedure TBegaCustomPreviewFrame.ContentZoomBoxChange(Sender: TObject);
begin
  updateContentZoom;
  createPages;
end;

//- BG ----------------------------------------------------------- 21.02.2006 --
constructor TBegaCustomPreviewFrame.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := CBegaPreviewOptionsDefault;
  FZoom := 1.0;
  ZoomBox.ItemIndex := 0;
  ContentZoomBox.ItemIndex := 6;
  UnitsBox.ItemIndex := 0;
  loadPreviewCursors;
//  ZoomingClick(Zooming);
  FMFPrinter := TBegaMetaFilePrinter.Create(Self);
  updateActions;
end;

//- BG ----------------------------------------------------------- 21.02.2006 --
procedure TBegaCustomPreviewFrame.createPages;
var
  Done: Boolean;
begin
  Done := False;
  MFPrinter.updatePrinterCaps;
  MFPrinter.clear;
  doCreatePages(Done);
  if Done then
  begin
    updateBox;
    updatePageMenu;
    CurrentPage := 1;
  end;
  updatePage;
  updateActions;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
destructor TBegaCustomPreviewFrame.Destroy;
begin
  MFPrinter.free;
  inherited;
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.doAllOnOne(var Done: Boolean);
begin
  if assigned(FOnAllOnOne) then
    FOnAllOnOne(self, AllOnOne.Down);
  // there is no default 'all' action
end;

//- BG ----------------------------------------------------------- 26.04.2006 --
procedure TBegaCustomPreviewFrame.doCreatePageMenu(var Done: Boolean);
var
  Index: Integer;
begin
  cboxPage.Clear;
  if assigned(FOnGetPageTitle) then
    for Index := 1 to PageCount do
      cboxPage.Items.Add(FOnGetPageTitle(self, Index))
  else
    for Index := 1 to PageCount do
      cboxPage.Items.Add(intToStr(Index));
  if PageCount > 0 then
    cboxPage.ItemIndex := 0;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.doCreatePages(var Done: Boolean);
begin
  if assigned(FOnCreatePages) then
    FOnCreatePages(self, MFPrinter, Done);
  // there is no default page generation
end;

//- BG ----------------------------------------------------------- 14.03.2006 --
procedure TBegaCustomPreviewFrame.doGetContentSize(var Done: Boolean; out Width, Height: Integer);
begin
  if assigned(FOnGetContentSize) then
    Done := FOnGetContentSize(self, MFPrinter, Width, Height)
  // there is no default content size calculation
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.doOpenInExcel(var Done: Boolean);
begin
  if assigned(FOnOpenInExcel) then
  begin
    Done := True;
    FOnOpenInExcel(self);
  end;
  // there is no default opening in excel
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.doPrinterSetup(var Done, Changed: Boolean);
begin
  if assigned(FOnPrinterSetup) then
  begin
    Done := True;
    FOnPrinterSetup(self, Changed);
  end;
  if not Done then
  begin
    Done := True;
    Changed := PrinterSetupDialog.Execute;
  end;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.doPrintPages(var Done: Boolean);
var
  FirstPage, LastPage, Copies: Integer;
begin
  if not Done then
  begin
    //BG, 10.04.2006: print metafiles by myself:
    Done := True;
    PrintDialog.MinPage := 1;
    PrintDialog.MaxPage := PageCount;
    PrintDialog.Options := [poPageNums, poWarning];
    if PrintDialog.Execute then
    begin
      FirstPage := 1;
      LastPage := 0;
      case PrintDialog.PrintRange of
        prAllPages:
        begin
          FirstPage := PrintDialog.MinPage;
          LastPage := PrintDialog.MaxPage;
        end;
        prPageNums:
        begin
          FirstPage := PrintDialog.FromPage;
          LastPage := PrintDialog.ToPage;
        end;
      end;
      //BG, 14.10.2010: if destination printer does the copying, the we print 1 copy only and let the printer sweat.
{$ifdef LCL}
      if Printer.CanRenderCopies then
{$else LCL}
      if pcCopies in Printer.Capabilities then
{$endif LCL}
        Copies := 1
      else
        Copies := PrintDialog.Copies;
      if assigned(FOnPrintPages) then
        FOnPrintPages(self, MFPrinter, FirstPage, LastPage, Copies)
      else
        printPages(FirstPage, LastPage, Copies);
    end;
  end;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.FirstPageClick(Sender: TObject);
begin
  CurrentPage := 1;
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.FitHeightClick(Sender: TObject);
begin
   ZoomBox.ItemIndex := 1;
   ZoomBoxChange(nil);
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.FitWidthClick(Sender: TObject);
begin
   ZoomBox.ItemIndex := 2;
   ZoomBoxChange(nil);
end;

//- BG ----------------------------------------------------------- 20.04.2006 --
function TBegaCustomPreviewFrame.getContentSize(
  out ContentSize: TBegaSize): Boolean;
begin
  Result := False;
  ContentSize.Width := 0;
  ContentSize.Height := 0;
  doGetContentSize(Result, ContentSize.Width, ContentSize.Height);
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
function TBegaCustomPreviewFrame.getContentZoomMode: TBegaZoomMode;
var
  Index: Integer;
begin
  Index := ContentZoomBox.ItemIndex;
  if Index >= 0 then
    Result := TBegaZoomMode(Index)
  else
    Result := zmFitToPage;
end;

//- BG ----------------------------------------------------------- 08.11.2006 --
function TBegaCustomPreviewFrame.getCurrentPage: Integer;
begin
  Result := PreviewPanel.FirstPage + 1;
end;

//- BG ----------------------------------------------------------- 28.04.2006 --
function TBegaCustomPreviewFrame.getLastAvailablePage: Integer;
begin
  Result := PageCount - VisiblePageCount + 1;
end;

//- BG ----------------------------------------------------------- 27.04.2006 --
function TBegaCustomPreviewFrame.getPage(Index: Integer): TMetafile;
begin
  Result := MFPrinter.MetaFiles[Index];
end;

//- BG ----------------------------------------------------------- 26.04.2006 --
function TBegaCustomPreviewFrame.getPageCount: Integer;
begin
  Result := MFPrinter.LastAvailablePage;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
function TBegaCustomPreviewFrame.getPreviewZoomMode: TBegaZoomMode;
var
  Index: Integer;
begin
  Index := ZoomBox.ItemIndex;
  if Index >= 0 then
    Result := TBegaZoomMode(Index)
  else
    Result := zmFitToPage;
end;

//- BG ----------------------------------------------------------- 08.04.2006 --
function TBegaCustomPreviewFrame.getVisiblePageCount: Integer;
begin
  Result := PagesPerRow.Value;
  if RowsPerPreview.Value > 1 then
    Result := PagesPerRow.Value * RowsPerPreview.Value;
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.GridClick(Sender: TObject);
begin
  updatePage;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.LastPageClick(Sender: TObject);
begin
  CurrentPage := LastAvailablePage;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewFrame.Loaded;
begin
  inherited;
  PreviewPanel.MFPrinter := MFPrinter;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.NextPageClick(Sender: TObject);
begin
  CurrentPage := CurrentPage + 1;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.PageChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := cboxPage.ItemIndex;
  if Index >= 0 then
    CurrentPage := Index + 1;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewFrame.PagesPerRowChange(Sender: TObject);
begin
  PreviewPanel.ColumnCount := PagesPerRow.Value;
  updateBox;
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.PanningClick(Sender: TObject);
begin
  if Panning.Down then
    PreviewPanel.Cursor := crHandDrag
  else
    PreviewPanel.Cursor := crDefault;
end;

//------------------------------------------------------------------------------
function TBegaCustomPreviewFrame.PixelsPerInch: Integer;
var
  Form: TCustomForm;
begin
  if csDesigning in ComponentState then
  begin
    Result := Screen.PixelsPerInch;
    exit;
  end;
  Form := GetParentForm(self);
  if Form is TForm then
    Result := TForm(Form).PixelsPerInch
{$ifndef LCL}
  else if Form is TCustomActiveForm then
    Result := TCustomActiveForm(Form).PixelsPerInch
{$endif LCL}
  else
    Result := Screen.PixelsPerInch;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewBoxResize(Sender: TObject);
begin
  updateBox;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewPanelUpdatePreview(
  Sender: TBegaCustomPreviewPanel; var Width, Height: Integer);
begin
  Width := max(Width, PreviewBox.ClientWidth);
  Height := max(Height, PreviewBox.ClientHeight);

  {Make sure the scroll bars are hidden if not needed}
  if (Width <= PreviewBox.ClientWidth) and (Height <= PreviewBox.ClientHeight) then
  begin
    PreviewBox.HorzScrollBar.Visible := False;
    PreviewBox.VertScrollBar.Visible := False;
  end
  else
  begin
    PreviewBox.HorzScrollBar.Visible := True;
    PreviewBox.VertScrollBar.Visible := True;
  end;

end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.PrevPageClick(Sender: TObject);
begin
   CurrentPage := CurrentPage - 1;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.PrintClick(Sender: TObject);
var
  Done: Boolean;
begin
  Done := False;
  doPrintPages(Done);
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.PrinterSetupClick(Sender: TObject);
var
  Done, Changed: Boolean;
begin
  Done := False;
  Changed := False;
  doPrinterSetup(Done, Changed);
  if Changed then
  begin
    MFPrinter.updatePrinterCaps;
    updateContentZoom;
    createPages;
  end;
end;

//- BG ----------------------------------------------------------- 27.05.2006 --
procedure TBegaCustomPreviewFrame.printPages(FirstPage, LastPage, Copies: Integer);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    MFPrinter.printDoc(Printer, FirstPage, LastPage, Copies);
  finally
    Screen.Cursor := OldCursor;
  end;
end;

//- BG ----------------------------------------------------------- 27.08.2006 --
procedure TBegaCustomPreviewFrame.ReportPropertiesClick(Sender: TObject);
var
  Edited: Boolean;
begin
  Edited := False;
  if assigned(FOnEditReportProperties) then
    FOnEditReportProperties(self, Edited);
  if Edited then
    updatePage;
end;

//- BG ----------------------------------------------------------- 06.04.2006 --
procedure TBegaCustomPreviewFrame.SaveFileClick(Sender: TObject);
begin
  if assigned(FOnSaveFile) then
    FOnSaveFile(self);
end;

//- BG ----------------------------------------------------------- 06.04.2006 --
procedure TBegaCustomPreviewFrame.SendMailClick(Sender: TObject);
begin
  if assigned(FOnSendMail) then
    FOnSendMail(self);
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.SetCurrentPage(Val: integer);
begin
  if CurrentPage <> Val then
  begin
    cboxPage.ItemIndex := Val - 1;
    PreviewPanel.FirstPage := Val - 1;
    updateActions;
  end;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
procedure TBegaCustomPreviewFrame.setContentZoomMode(Zoom: TBegaZoomMode);
begin
  ContentZoomBox.ItemIndex := ord(Zoom);
  ContentZoomBoxChange(ContentZoomBox);
end;

//- BG ----------------------------------------------------------- 06.04.2006 --
procedure TBegaCustomPreviewFrame.setOptions(const Value: TBegaPreviewOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    updateActions;
  end;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
procedure TBegaCustomPreviewFrame.setPreviewZoomMode(Zoom: TBegaZoomMode);
begin
  ZoomBox.ItemIndex := ord(Zoom);
  ZoomBoxChange(ZoomBox);
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.UnitsBoxChange(Sender: TObject);
begin
  if Grid.down then
    updatePage;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.updateActions;
var
  LastDocPage: Integer;
  LastAvlPage: Integer;
  LastVisPage: Integer;
  canZoomContent: Boolean;
begin
  canZoomContent := poContentZoomVisible in FOptions;
  if canZoomContent then
  begin
    ContentZoomLabel.Caption := Format('Content zoom %1.0n', [MFPrinter.PrintScale * 100]) + '%';
  end
  else
    ContentZoomLabel.Caption := '';

  PreviewZoomLabel.Caption := Format('Preview zoom %1.0n', [Zoom * 100]) + '%';
  LastDocPage := PageCount;
  LastAvlPage := LastAvailablePage;
  LastVisPage := min(CurrentPage + VisiblePageCount - 1, LastDocPage);
  FirstPage.Enabled := CurrentPage > 1;
  PrevPage.Enabled  := FirstPage.Enabled;
  if CurrentPage <= 0 then
    CurrentPageLabel.Caption := 'No pages'
  else if LastVisPage = CurrentPage then
    CurrentPageLabel.Caption := Format('Page %d of %d', [CurrentPage, LastDocPage])
  else
    CurrentPageLabel.Caption := Format('Pages %d..%d of %d', [CurrentPage, LastVisPage, LastDocPage]);
  NextPage.Enabled  := CurrentPage < LastAvlPage;
  LastPage.Enabled  := NextPage.Enabled;
  cboxPage.Enabled := LastAvlPage > 1;
//    if CurrentPage <= 0 then
//      cboxPage.ItemIndex := 0
//    else
//      cboxPage.ItemIndex := CurrentPage - 1;

  SendMail.Visible    := assigned(FOnSendMail);
  SendMail.Enabled    := buttonEnabled(poSendMailEnabled);
  SaveFile.Visible    := assigned(FOnSaveFile);
  SaveFile.Enabled    := buttonEnabled(poSaveFileEnabled);
  OpenInExcel.Visible := assigned(FOnOpenInExcel);
  OpenInExcel.Enabled := buttonEnabled(poOpenInExcelEnabled);
  AllOnOne.Visible    := assigned(FOnAllOnOne);
  AllOnOne.Enabled    := buttonEnabled(poAllOnOneEnabled);
  ReportProperties.Visible := assigned(FOnEditReportProperties);
  ReportProperties.Enabled := buttonEnabled(poEditReportPropertiesEnabled);
  PrinterSetup.Enabled       := buttonEnabled(poPrinterSetupEnabled);
  Print.Enabled       := buttonEnabled(poPrintEnabled);

  ContentZoomSeparator.Visible := canZoomContent;
  ContentZoomBoxLabel.Visible := canZoomContent;
  ContentZoomBox.Visible := canZoomContent;
  ContentFitHeight.Visible := canZoomContent;
  ContentFitWidth.Visible := canZoomContent;
  ContentFitHeight.Enabled := canGetContentSize;
  ContentFitWidth.Enabled := canGetContentSize;
  ContentFitHeight.Down := ContentZoomBox.ItemIndex = 1;
  ContentFitWidth.Down := ContentZoomBox.ItemIndex = 2;

  FitHeight.Down  := ZoomBox.ItemIndex = 1;
  FitWidth.Down := ZoomBox.ItemIndex = 2;

  Panning.Enabled := PreviewBox.VertScrollBar.Visible or PreviewBox.HorzScrollBar.Visible or (PreviewPanel.Left <> 0) or (PreviewPanel.Top <> 0);
  if not Panning.Enabled then
  begin
    Panning.Down := False;
    PanningClick(Panning);
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewFrame.updateBox;
var
  z: double;
  WidthInInch: double;
  HeightInInch: double;

  function calcZoomFactor(
    PreviewPixelCount: Integer;
    PreviewBorderPixelCount: Integer;
    PreviewPixelsBetweenPages: Integer;
    PreviewPixelsPerInch: Integer;
    PageCount: Integer;
    PaperSizeInInch: double): double;
  begin
    Result := (PreviewPixelCount - 2 * PreviewBorderPixelCount - (max(0, PageCount - 1)) * PreviewPixelsBetweenPages)
      / (PreviewPixelsPerInch * PaperSizeInInch * PageCount);
  end;

  function calcZoomFactorX(): double;
  begin
    Result := calcZoomFactor(
      PreviewBox.ClientWidth,
      PreviewPanel.BorderWidth,
      PreviewPanel.InnerWidth,
      PixelsPerInch,
      PreviewPanel.ColumnCount,
      WidthInInch);
  end;

  function calcZoomFactorY(): double;
  begin
    Result := calcZoomFactor(
      PreviewBox.ClientHeight,
      PreviewPanel.BorderWidth,
      PreviewPanel.InnerWidth,
      PixelsPerInch,
      PreviewPanel.RowCount,
      HeightInInch);
  end;

begin
  if (MFPrinter.PaperWidth <> 0) and (MFPrinter.PixelsPerInchX <> 0) then
    WidthInInch := MFPrinter.PaperWidth / MFPrinter.PixelsPerInchX
  else
    WidthInInch := 8.5;

  if (MFPrinter.PaperHeight <> 0) and (MFPrinter.PixelsPerInchY <> 0) then
    HeightInInch := MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY
  else
    HeightInInch := 11;

  if ZoomBox.ItemIndex = -1 then
    ZoomBox.ItemIndex := 0;
  case ZoomBox.ItemIndex of
    0: z := min(calcZoomFactorX, calcZoomFactorY);
    1: z := calcZoomFactorY;
    2: z := calcZoomFactorX; 
    3: z := 0.25;
    4: z := 0.50;
    5: z := 0.75;
    6: z := 1.00;
    7: z := 1.25;
    8: z := 1.50;
    9: z := 2.00;
    10: z := 3.00;
    11: z := 4.00;
  else
    z := Zoom;
  end;
  Zoom := z;
  PreviewPanel.MFPrinter := MFPrinter;
  PreviewPanel.updatePreview(
    trunc(PixelsPerInch * z * WidthInInch),
    trunc(PixelsPerInch * z * HeightInInch));
  updateActions;
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.updateContentZoom;
var
  z: Double;
  PreviewArea: TBegaRect;
  ContentSize: TBegaSize;
begin
  z := MFPrinter.PrintScale;
  case ContentZoomBox.ItemIndex of
    0: // fit to size
    begin
      PreviewArea := MFPrinter.getPreviewArea(MFPrinter.PrintArea, Screen.PixelsPerInch);
      if getContentSize(ContentSize) then
        z := min(
          PreviewArea.Height / ContentSize.Height,
          PreviewArea.Width / ContentSize.Width);
    end;
    1: // fit to height
    begin
      PreviewArea := MFPrinter.getPreviewArea(MFPrinter.PrintArea, Screen.PixelsPerInch);
      if getContentSize(ContentSize) then
        z := PreviewArea.Height / ContentSize.Height;
    end;
    2: // fit to width
    begin
      PreviewArea := MFPrinter.getPreviewArea(MFPrinter.PrintArea, Screen.PixelsPerInch);
      if getContentSize(ContentSize) then
        z := PreviewArea.Width / ContentSize.Width;
    end;
    3: z := 0.25;
    4: z := 0.50;
    5: z := 0.75;
    6: z := 1.00;
    7: z := 1.25;
    8: z := 1.50;
    9: z := 2.00;
    10: z := 3.00;
    11: z := 4.00;
    12: z := 8.00;
  end;

  MFPrinter.PrintScale := z;
  updateActions;
end;

//- BG ----------------------------------------------------------- 21.02.2006 --
procedure TBegaCustomPreviewFrame.updatePage;
begin
  PreviewPanel.invalidate;
end;

//- BG ----------------------------------------------------------- 26.04.2006 --
procedure TBegaCustomPreviewFrame.updatePageMenu;
var
  Done: Boolean;
begin
  Done := False;
  doCreatePageMenu(Done);
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.ZoomBoxChange(Sender: TObject);
begin
  updateBox;
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.ZoomingClick(Sender: TObject);
begin
  if Zooming.Down then
    PreviewPanel.Cursor := crZoom
  else
    PreviewPanel.Cursor := crDefault;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sx, sy : single;
  Pos: TPoint;
begin
  if Zooming.Down then
  begin
    sx := X / PreviewPanel.Width;
    sy := Y / PreviewPanel.Height;

    if (ssLeft  in Shift) and (Zoom < 100.0) then
     Zoom := Zoom * ZOOMFACTOR;
    if (ssRight in Shift) and (Zoom > 0.01) then
     Zoom := Zoom / ZOOMFACTOR;
    ZoomBox.ItemIndex := ord(zoom2ZoomMode(Zoom));

    updateBox;

    PreviewBox.HorzScrollBar.Position := round(sx * PreviewPanel.Width) - PreviewBox.Width div 2;
    PreviewBox.VertScrollBar.Position := round(sy * PreviewPanel.Height) - PreviewBox.Height div 2;
  end
  else if Panning.Down then
  begin
    Moving := True;
    Pos := TControl(Sender).ClientToParent(Point(X, Y), PreviewBox);
    PreviewBox.MouseDown(Button, Shift, Pos.X, Pos.Y);
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Pos: TPoint;
begin
  if Moving then
  begin
    Pos := TControl(Sender).ClientToParent(Point(X, Y), PreviewBox);
    PreviewBox.MouseMove(Shift, Pos.X, Pos.Y);
    PreviewPanel.UpdatePreview;
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pos: TPoint;
begin
  if Moving then
  begin
    Moving := False;
    Pos := TControl(Sender).ClientToParent(Point(X, Y), PreviewBox);
    PreviewBox.MouseUp(Button, Shift, Pos.X, Pos.Y);
    PreviewPanel.UpdatePreview;
  end;
end;

//-- BG ---------------------------------------------------------- 31.08.2009 --
procedure TBegaCustomPreviewFrame.PreviewPanelPaintPage(Sender: TBegaCellPreviewPanel;
  Canvas: TCanvas; PageIndex: Integer);
var
  x, x1: Integer;
  y, y1: Integer;
  Factor: Double;
begin
  if Grid.Down then
  begin
    SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
    Canvas.Pen.Color := clLtGray;
    if UnitsBox.ItemIndex = 0 then
      Factor := 1.0
    else
      Factor := 2.54;

    for x := 1 to Round(MFPrinter.PaperWidth / MFPrinter.PixelsPerInchX * Factor) do
    begin
      x1 := Round(MFPrinter.PixelsPerInchX * x / Factor);
      Canvas.MoveTo(x1, 0);
      Canvas.LineTo(x1, MFPrinter.PaperHeight);
    end;

    for y := 1 to Round(MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY * Factor) do
    begin
      y1 := Round(MFPrinter.PixelsPerInchY * y / Factor);
      Canvas.MoveTo(0, y1);
      Canvas.LineTo(MFPrinter.PaperWidth, y1);
    end;
  end;
end;

//- BG ----------------------------------------------------------- 08.11.2006 --
procedure TBegaCustomPreviewFrame.RowsPerPreviewChange(Sender: TObject);
begin
  PreviewPanel.RowCount := RowsPerPreview.Value;
  updateBox;
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.AllOnOneClick(Sender: TObject);
var
  Done: Boolean;
begin
  Done := False;
  doAllOnOne(done);
  if Done then
    updatePage;
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.OpenInExcelClick(Sender: TObject);
var
  Done: Boolean;
begin
  Done := False;
  doOpenInExcel(done);
end;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
end.
