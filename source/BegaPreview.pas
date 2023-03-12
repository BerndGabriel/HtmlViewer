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
    function GetContentSize(out ContentSize: TBegaSize): Boolean;
    function GetLastAvailablePage: Integer;
    function GetPage(Index: Integer): TMetafile;
    function GetPageCount: Integer;
    function GetVisiblePageCount: Integer;
    function PixelsPerInch: Integer;
    procedure PrintPages(FirstPage, LastPage, Copies: Integer);
    procedure SetCurrentPage(Val: integer);
    procedure SetOptions(const Value: TBegaPreviewOptions);
    procedure SetContentZoomMode(Zoom: TBegaZoomMode);
    procedure SetPreviewZoomMode(Zoom: TBegaZoomMode);
    property LastAvailablePage: Integer read getLastAvailablePage; // due to visible page count
    property VisiblePageCount: Integer read getVisiblePageCount;
    property Zoom: Double read FZoom write FZoom;
    function GetContentZoomMode: TBegaZoomMode;
    function GetPreviewZoomMode: TBegaZoomMode;
    function GetCurrentPage: Integer;
  protected
    procedure Loaded; override;
    //
    function ButtonEnabled(Button: TBegaPreviewOption): Boolean; virtual;
    // DoGetSize() calculates size of all pages, which enables content zoom.
    function CanGetContentSize: Boolean; virtual;
    procedure DoGetContentSize(var Done: Boolean; out Width, Height: Integer); virtual;
    // DoCreatePageMenu() populates MFPrinter with pages
    procedure DoCreatePageMenu(var Done: Boolean); virtual;
    // DoCreatePages() populates MFPrinter with pages
    procedure DoCreatePages(var Done: Boolean); virtual;
    // DoPrintPages() prints pages of MFPrinter or do you prefer printing elsehow?
    procedure DoPrintPages(var Done: Boolean); virtual;
    // DoPrinterSetup(): override or use OnPrinterSetup to reduce number of printer setup dialogs:
    procedure DoPrinterSetup(var Done, Changed: Boolean); virtual;
    //
    procedure DoAllOnOne(var Done: Boolean); virtual;
    //
    procedure DoOpenInExcel(var Done: Boolean); virtual;
    //
    procedure UpdateBox;
    procedure UpdateContentZoom;
    procedure UpdatePage;
    procedure UpdatePageMenu;
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
    procedure CreatePages;
    procedure UpdateActions;
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

procedure LoadPreviewCursors;

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

type
  //BG, 05.02.2023: don't add virtual methods or fields. It is only used to access protected stuff of TCustomForm.
  TBegaCustomForm = class(TCustomForm)
  public
    property PixelsPerInch;
  end;

//- BG ----------------------------------------------------------- 04.11.2006 --
var PreviewCursorsLoaded: Boolean;
procedure LoadPreviewCursors;
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
function Zoom2ZoomMode(Zoom: Double): TBegaZoomMode;
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
function TBegaCustomPreviewFrame.ButtonEnabled(
  Button: TBegaPreviewOption): Boolean;
begin
  Result := Button in PreviewOptions;
end;

//- BG ----------------------------------------------------------- 20.04.2006 --
function TBegaCustomPreviewFrame.CanGetContentSize: Boolean;
begin
  Result := Assigned(FOnGetContentSize);
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.ContentFitHeightClick(Sender: TObject);
begin
  ContentZoomBox.ItemIndex := 1;
  UpdateContentZoom;
  CreatePages;
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.ContentFitWidthClick(Sender: TObject);
begin
  ContentZoomBox.ItemIndex := 2;
  UpdateContentZoom;
  CreatePages;
end;

//- BG ----------------------------------------------------------- 14.03.2006 --
procedure TBegaCustomPreviewFrame.ContentZoomBoxChange(Sender: TObject);
begin
  UpdateContentZoom;
  CreatePages;
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
  LoadPreviewCursors;
  FMFPrinter := TBegaMetaFilePrinter.Create(Self);
  //FMFPrinter.PreviewPixelsPerInch := PixelsPerInch;
  //PreviewPanel.MFPrinter := FMFPrinter;
  UpdateActions;
end;

//- BG ----------------------------------------------------------- 21.02.2006 --
procedure TBegaCustomPreviewFrame.CreatePages;
var
  Done: Boolean;
begin
  Done := False;
{$ifdef LCL}
  MFPrinter.PreviewPixelsPerInch := PixelsPerInch;
{$endif}
  MFPrinter.UpdatePrinterCaps;
  MFPrinter.Clear;
  DoCreatePages(Done);
  if Done then
  begin
    UpdateBox;
    UpdatePageMenu;
    CurrentPage := 1;
  end;
  UpdatePage;
  UpdateActions;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
destructor TBegaCustomPreviewFrame.Destroy;
begin
  MFPrinter.Free;
  inherited;
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.DoAllOnOne(var Done: Boolean);
begin
  if Assigned(FOnAllOnOne) then
    FOnAllOnOne(self, AllOnOne.Down);
  // there is no default 'all' action
end;

//- BG ----------------------------------------------------------- 26.04.2006 --
procedure TBegaCustomPreviewFrame.DoCreatePageMenu(var Done: Boolean);
var
  Index: Integer;
begin
  cboxPage.Clear;
  if Assigned(FOnGetPageTitle) then
    for Index := 1 to PageCount do
      cboxPage.Items.Add(FOnGetPageTitle(self, Index))
  else
    for Index := 1 to PageCount do
      cboxPage.Items.Add(intToStr(Index));
  if PageCount > 0 then
    cboxPage.ItemIndex := 0;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.DoCreatePages(var Done: Boolean);
begin
  if Assigned(FOnCreatePages) then
    FOnCreatePages(self, MFPrinter, Done);
  // there is no default page generation
end;

//- BG ----------------------------------------------------------- 14.03.2006 --
procedure TBegaCustomPreviewFrame.DoGetContentSize(var Done: Boolean; out Width, Height: Integer);
begin
  if Assigned(FOnGetContentSize) then
    Done := FOnGetContentSize(self, MFPrinter, Width, Height)
  // there is no default content size calculation
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.DoOpenInExcel(var Done: Boolean);
begin
  if Assigned(FOnOpenInExcel) then
  begin
    Done := True;
    FOnOpenInExcel(self);
  end;
  // there is no default opening in excel
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.DoPrinterSetup(var Done, Changed: Boolean);
begin
  if Assigned(FOnPrinterSetup) then
  begin
    Done := True;
    FOnPrinterSetup(Self, Changed);
  end;
  if not Done then
  begin
    Done := True;
    Changed := PrinterSetupDialog.Execute;
  end;
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.DoPrintPages(var Done: Boolean);
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
      if Assigned(FOnPrintPages) then
        FOnPrintPages(self, MFPrinter, FirstPage, LastPage, Copies)
      else
        PrintPages(FirstPage, LastPage, Copies);
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
function TBegaCustomPreviewFrame.GetContentSize(
  out ContentSize: TBegaSize): Boolean;
begin
  Result := False;
//  MFPrinter.PreviewPixelsPerInch := PixelsPerInch;
  ContentSize.Width := 0;
  ContentSize.Height := 0;
  DoGetContentSize(Result, ContentSize.Width, ContentSize.Height);
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
function TBegaCustomPreviewFrame.GetContentZoomMode: TBegaZoomMode;
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
function TBegaCustomPreviewFrame.GetCurrentPage: Integer;
begin
  Result := PreviewPanel.FirstPage + 1;
end;

//- BG ----------------------------------------------------------- 28.04.2006 --
function TBegaCustomPreviewFrame.GetLastAvailablePage: Integer;
begin
  Result := PageCount - VisiblePageCount + 1;
end;

//- BG ----------------------------------------------------------- 27.04.2006 --
function TBegaCustomPreviewFrame.GetPage(Index: Integer): TMetafile;
begin
  Result := MFPrinter.MetaFiles[Index];
end;

//- BG ----------------------------------------------------------- 26.04.2006 --
function TBegaCustomPreviewFrame.GetPageCount: Integer;
begin
  Result := MFPrinter.LastAvailablePage;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
function TBegaCustomPreviewFrame.GetPreviewZoomMode: TBegaZoomMode;
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
function TBegaCustomPreviewFrame.GetVisiblePageCount: Integer;
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
//  PreviewPanel.MFPrinter := MFPrinter;
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
{$if defined(LCL) or not defined(Compiler31_Plus)}
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Result := {$ifndef LCL}TBegaCustomForm{$endif}(Form).PixelsPerInch
  else
    Result := Screen.PixelsPerInch;
{$else}
begin
  Result := FCurrentPPI;
{$ifend}
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewBoxResize(Sender: TObject);
begin
  UpdateBox;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewFrame.PreviewPanelUpdatePreview(
  Sender: TBegaCustomPreviewPanel; var Width, Height: Integer);
begin
  Width := Max(Width, PreviewBox.ClientWidth);
  Height := Max(Height, PreviewBox.ClientHeight);

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
  DoPrintPages(Done);
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.PrinterSetupClick(Sender: TObject);
var
  Done, Changed: Boolean;
begin
  Done := False;
  Changed := False;
  DoPrinterSetup(Done, Changed);
  if Changed then
  begin
    MFPrinter.updatePrinterCaps;
    UpdateContentZoom;
    CreatePages;
  end;
end;

//- BG ----------------------------------------------------------- 27.05.2006 --
procedure TBegaCustomPreviewFrame.PrintPages(FirstPage, LastPage, Copies: Integer);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    MFPrinter.PrintDoc(Printer, FirstPage, LastPage, Copies);
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
  if Assigned(FOnEditReportProperties) then
    FOnEditReportProperties(self, Edited);
  if Edited then
    UpdatePage;
end;

//- BG ----------------------------------------------------------- 06.04.2006 --
procedure TBegaCustomPreviewFrame.SaveFileClick(Sender: TObject);
begin
  if Assigned(FOnSaveFile) then
    FOnSaveFile(self);
end;

//- BG ----------------------------------------------------------- 06.04.2006 --
procedure TBegaCustomPreviewFrame.SendMailClick(Sender: TObject);
begin
  if Assigned(FOnSendMail) then
    FOnSendMail(self);
end;

//- BG ----------------------------------------------------------- 06.03.2006 --
procedure TBegaCustomPreviewFrame.SetCurrentPage(Val: integer);
begin
  if CurrentPage <> Val then
  begin
    cboxPage.ItemIndex := Val - 1;
    PreviewPanel.FirstPage := Val - 1;
    UpdateActions;
  end;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
procedure TBegaCustomPreviewFrame.SetContentZoomMode(Zoom: TBegaZoomMode);
begin
  ContentZoomBox.ItemIndex := ord(Zoom);
  ContentZoomBoxChange(ContentZoomBox);
end;

//- BG ----------------------------------------------------------- 06.04.2006 --
procedure TBegaCustomPreviewFrame.SetOptions(const Value: TBegaPreviewOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    UpdateActions;
  end;
end;

//- BG ----------------------------------------------------------- 03.11.2006 --
procedure TBegaCustomPreviewFrame.SetPreviewZoomMode(Zoom: TBegaZoomMode);
begin
  ZoomBox.ItemIndex := Ord(Zoom);
  ZoomBoxChange(ZoomBox);
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.UnitsBoxChange(Sender: TObject);
begin
  if Grid.Down then
    UpdatePage;
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
  LastVisPage := Min(CurrentPage + VisiblePageCount - 1, LastDocPage);
  FirstPage.Enabled := CurrentPage > 1;
  PrevPage.Enabled  := FirstPage.Enabled;
  if CurrentPage <= 0 then
    CurrentPageLabel.Caption := 'No pages'
  else if LastVisPage = CurrentPage then
    CurrentPageLabel.Caption := Format('Page %d of %d', [CurrentPage, LastDocPage])
  else
    CurrentPageLabel.Caption := Format('Pages %d..%d of %d', [CurrentPage, LastVisPage, LastDocPage]);
  NextPage.Enabled    := CurrentPage < LastAvlPage;
  LastPage.Enabled    := NextPage.Enabled;
  cboxPage.Enabled    := LastAvlPage > 1;

  SendMail.Visible    := Assigned(FOnSendMail);
  SendMail.Enabled    := ButtonEnabled(poSendMailEnabled);

  SaveFile.Visible    := Assigned(FOnSaveFile);
  SaveFile.Enabled    := ButtonEnabled(poSaveFileEnabled);

  OpenInExcel.Visible := Assigned(FOnOpenInExcel);
  OpenInExcel.Enabled := ButtonEnabled(poOpenInExcelEnabled);

  AllOnOne.Visible    := Assigned(FOnAllOnOne);
  AllOnOne.Enabled    := ButtonEnabled(poAllOnOneEnabled);

  ReportProperties.Visible  := Assigned(FOnEditReportProperties);
  ReportProperties.Enabled  := ButtonEnabled(poEditReportPropertiesEnabled);

  PrinterSetup.Enabled      := ButtonEnabled(poPrinterSetupEnabled);
  Print.Enabled             := ButtonEnabled(poPrintEnabled);

  ContentZoomSeparator.Visible  := canZoomContent;
  ContentZoomBoxLabel.Visible   := canZoomContent;
  ContentZoomBox.Visible        := canZoomContent;

  ContentFitHeight.Visible  := canZoomContent;
  ContentFitHeight.Enabled  := canGetContentSize;
  ContentFitHeight.Down     := ContentZoomBox.ItemIndex = 1;

  ContentFitWidth.Visible   := canZoomContent;
  ContentFitWidth.Enabled   := canGetContentSize;
  ContentFitWidth.Down      := ContentZoomBox.ItemIndex = 2;

  FitHeight.Down  := ZoomBox.ItemIndex = 1;
  FitWidth.Down   := ZoomBox.ItemIndex = 2;

  Panning.Enabled := PreviewBox.VertScrollBar.Visible or PreviewBox.HorzScrollBar.Visible or (PreviewPanel.Left <> 0) or (PreviewPanel.Top <> 0);
  if not Panning.Enabled then
  begin
    Panning.Down := False;
    PanningClick(Panning);
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewFrame.UpdateBox;
var
  z: Double;
  WidthInInch: Double;
  HeightInInch: Double;

  function CalcZoomFactor(
    PreviewPixelCount: Integer;
    PreviewBorderPixelCount: Integer;
    PreviewPixelsBetweenPages: Integer;
    PreviewPixelsPerInch: Integer;
    PageCount: Integer;
    PaperSizeInInch: double): double;
  begin
    Result := (PreviewPixelCount - 2 * PreviewBorderPixelCount - (Max(0, PageCount - 1)) * PreviewPixelsBetweenPages)
      / (PreviewPixelsPerInch * PaperSizeInInch * PageCount);
  end;

  function CalcZoomFactorX: Double;
  begin
    Result := CalcZoomFactor(
      PreviewBox.ClientWidth,
      PreviewPanel.BorderWidth,
      PreviewPanel.InnerWidth,
      PixelsPerInch,
      PreviewPanel.ColumnCount,
      WidthInInch);
  end;

  function CalcZoomFactorY: Double;
  begin
    Result := CalcZoomFactor(
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
    0: z := Min(CalcZoomFactorX, CalcZoomFactorY);
    1: z := CalcZoomFactorY;
    2: z := CalcZoomFactorX;
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
  PreviewPanel.UpdatePreview(
    Trunc(PixelsPerInch * z * WidthInInch),
    Trunc(PixelsPerInch * z * HeightInInch));
  UpdateActions;
end;

//- BG ----------------------------------------------------------- 15.03.2006 --
procedure TBegaCustomPreviewFrame.UpdateContentZoom;
var
  z: Double;
  PreviewArea: TBegaRect;
  ContentSize: TBegaSize;
begin
  z := MFPrinter.PrintScale;
  case ContentZoomBox.ItemIndex of
    0: // fit to size
    begin
      PreviewArea := MFPrinter.GetPreviewArea(MFPrinter.PrintArea, Screen.PixelsPerInch);
      if GetContentSize(ContentSize) then
        z := Min(
          PreviewArea.Height / ContentSize.Height,
          PreviewArea.Width / ContentSize.Width);
    end;
    1: // fit to height
    begin
      PreviewArea := MFPrinter.GetPreviewArea(MFPrinter.PrintArea, Screen.PixelsPerInch);
      if GetContentSize(ContentSize) then
        z := PreviewArea.Height / ContentSize.Height;
    end;
    2: // fit to width
    begin
      PreviewArea := MFPrinter.GetPreviewArea(MFPrinter.PrintArea, Screen.PixelsPerInch);
      if GetContentSize(ContentSize) then
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
  UpdateActions;
end;

//- BG ----------------------------------------------------------- 21.02.2006 --
procedure TBegaCustomPreviewFrame.UpdatePage;
begin
  PreviewPanel.Invalidate;
end;

//- BG ----------------------------------------------------------- 26.04.2006 --
procedure TBegaCustomPreviewFrame.UpdatePageMenu;
var
  Done: Boolean;
begin
  Done := False;
  DoCreatePageMenu(Done);
end;

//------------------------------------------------------------------------------
procedure TBegaCustomPreviewFrame.ZoomBoxChange(Sender: TObject);
begin
  UpdateBox;
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
    ZoomBox.ItemIndex := Ord(zoom2ZoomMode(Zoom));

    UpdateBox;

    PreviewBox.HorzScrollBar.Position := Round(sx * PreviewPanel.Width) - PreviewBox.Width div 2;
    PreviewBox.VertScrollBar.Position := Round(sy * PreviewPanel.Height) - PreviewBox.Height div 2;
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
  UpdateBox;
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.AllOnOneClick(Sender: TObject);
var
  Done: Boolean;
begin
  Done := False;
  DoAllOnOne(done);
  if Done then
    UpdatePage;
end;

//- BG ----------------------------------------------------------- 22.12.2006 --
procedure TBegaCustomPreviewFrame.OpenInExcelClick(Sender: TObject);
var
  Done: Boolean;
begin
  Done := False;
  DoOpenInExcel(done);
end;

{$endif NoMetaFile}
{$endif NoFlatScrollbars}
end.
