{*************************************************************}
{*                                                           *}
{*  Thanks to Chris Wallace for most of the ideas and        *}
{*  code associated with Print Preview and the Preview Form  *}
{*                                                           *}
{*************************************************************}

{$ifDef ver150}  {Delphi 7}
{$Define Delphi7_Plus}
{$endif}
{$ifDef ver170}  {Delphi 2005}
{$Define Delphi7_Plus}
{$endif}
{$ifDef ver180}  {Delphi 2006}
{$Define Delphi7_Plus}     {9.4}
{$endif}

unit PreviewForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, MetaFilePrinter, HTMLView, PrintStatusForm;

const
   crZoom = 40;
   crHandDrag = 41;
   ZOOMFACTOR = 1.5;

type
  TPreviewForm = class(TForm)
    ToolBarPanel: TPanel;
    GridBut: TSpeedButton;
    ZoomCursorBut: TSpeedButton;
    HandCursorBut: TSpeedButton;
    OnePageBut: TSpeedButton;
    TwoPageBut: TSpeedButton;
    PrintBut: TBitBtn;
    NextPageBut: TBitBtn;
    PrevPageBut: TBitBtn;
    CloseBut: TBitBtn;
    ZoomBox: TComboBox;
    StatBarPanel: TPanel;
    CurPageLabel: TPanel;
    ZoomLabel: TPanel;
    Panel1: TPanel;
    HintLabel: TLabel;
    MoveButPanel: TPanel;
    FirstPageSpeed: TSpeedButton;
    PrevPageSpeed: TSpeedButton;
    NextPageSpeed: TSpeedButton;
    LastPageSpeed: TSpeedButton;
    PageNumSpeed: TSpeedButton;
    ScrollBox1: TScrollBox;
    ContainPanel: TPanel;
    PagePanel: TPanel;
    PB1: TPaintBox;
    PagePanel2: TPanel;
    PB2: TPaintBox;
    PrintDialog1: TPrintDialog;
    FitPageBut: TSpeedButton;
    FitWidthBut: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    UnitsBox: TComboBox;
    Bevel7: TBevel;
    procedure CloseButClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure GridButClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ZoomBoxChange(Sender: TObject);
    procedure TwoPageButClick(Sender: TObject);
    procedure NextPageButClick(Sender: TObject);
    procedure PrevPageButClick(Sender: TObject);
    procedure FirstPageSpeedClick(Sender: TObject);
    procedure LastPageSpeedClick(Sender: TObject);
    procedure ZoomCursorButClick(Sender: TObject);
    procedure HandCursorButClick(Sender: TObject);
    procedure PB1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PB1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PB1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrintButClick(Sender: TObject);
    procedure PageNumSpeedClick(Sender: TObject);
    procedure OnePageButMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FitPageButClick(Sender: TObject);
    procedure FitWidthButClick(Sender: TObject);
    procedure UnitsBoxChange(Sender: TObject);
  private
    Viewer: ThtmlViewer;
  protected
    FCurPage      : integer;
    OldHint       : TNotifyEvent;
    DownX, DownY  : integer;
    Moving        : boolean;
    MFPrinter     : TMetaFilePrinter;
    procedure     DrawMetaFile(PB: TPaintBox; mf: TMetaFile);
    procedure     OnHint(Sender: TObject);
    procedure     SetCurPage(Val: integer);
    procedure     CheckEnable;
    property      CurPage: integer read FCurPage write SetCurPage;
  public
    Zoom          : double;
    constructor CreateIt(AOwner: TComponent; AViewer: ThtmlViewer; var Abort: boolean);
    destructor Destroy; override;
  end;


implementation

uses
   Gopage;

{$R *.DFM}
{$R GRID.RES}

constructor TPreviewForm.CreateIt(AOwner: TComponent; AViewer: ThtmlViewer;
                var Abort: boolean);
var
  StatusForm: TPrnStatusForm;
begin
inherited Create(AOwner);
   ZoomBox.ItemIndex := 0;
   UnitsBox.ItemIndex := 0;
   Screen.Cursors[crZoom] := LoadCursor(hInstance, 'ZOOM_CURSOR');
   Screen.Cursors[crHandDrag] := LoadCursor(hInstance, 'HAND_CURSOR');
   ZoomCursorButClick(nil);
Viewer := AViewer;
MFPrinter := TMetaFilePrinter.Create(Self);
StatusForm := TPrnStatusForm.Create(Self);
try
  StatusForm.DoPreview(Viewer, MFPrinter, Abort);
finally
  StatusForm.Free;
  end;
end;

destructor TPreviewForm.Destroy;
begin
inherited;
end;

procedure TPreviewForm.CloseButClick(Sender: TObject);
begin
   Close;
end;

procedure TPreviewForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   Action := caFree;
   Application.OnHint := OldHint;
   MFPrinter.Free;
end;

procedure TPreviewForm.ScrollBox1Resize(Sender: TObject);
const
   BORD = 20;
var
   z        : double;
   tmp      : integer;
   TotWid   : integer;
begin
   case ZoomBox.ItemIndex of
      0  : FitPageBut.Down  := True;
      1  : FitWidthBut.Down := True;
   else
      begin
         FitPageBut.Down  := False;
         FitWidthBut.Down := False;
      end;
   end;

   if ZoomBox.ItemIndex = -1 then
      ZoomBox.ItemIndex := 0;

   Case ZoomBox.ItemIndex of
      0: z := ((ScrollBox1.ClientHeight - BORD) / PixelsPerInch) /
               (MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY);
      1: z := ((ScrollBox1.ClientWidth - BORD) / PixelsPerInch) /
               (MFPrinter.PaperWidth / MFPrinter.PixelsPerInchX);
      2: z := Zoom;
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
      z := 1;
   end;

   if ZoomBox.ItemIndex<>0 then OnePageBut.Down := True;

   PagePanel.Height := TRUNC(PixelsPerInch * z * MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY); 
   PagePanel.Width  := TRUNC(PixelsPerInch * z * MFPrinter.PaperWidth  / MFPrinter.PixelsPerInchX);

   PagePanel2.Visible := TwoPageBut.Down;
   if TwoPageBut.Down then
      begin
         PagePanel2.Width  := PagePanel.Width;
         PagePanel2.Height := PagePanel.Height;
      end;

   TotWid := PagePanel.Width + BORD;
   if TwoPageBut.Down then
      TotWid := TotWid + PagePanel2.Width + BORD;

   // Resize the Contain Panel
   tmp := PagePanel.Height + BORD;
   if tmp < ScrollBox1.ClientHeight then
      tmp := ScrollBox1.ClientHeight-1;
   ContainPanel.Height := tmp;

   tmp := TotWid;
   if tmp < ScrollBox1.ClientWidth then
      tmp := ScrollBox1.ClientWidth-1;
   ContainPanel.Width := tmp;

   // Center the Page Panel
   if PagePanel.Height + BORD < ContainPanel.Height then
      PagePanel.Top := ContainPanel.Height div 2 - PagePanel.Height div 2
   else
      PagePanel.Top := BORD div 2;
   PagePanel2.Top := PagePanel.Top;

   if TotWid < ContainPanel.Width then
      PagePanel.Left := ContainPanel.Width div 2 - (TotWid - BORD) div 2
   else
      PagePanel.Left := BORD div 2;
   PagePanel2.Left := PagePanel.Left + PagePanel.Width + BORD;

   {Make sure the scroll bars are hidden if not needed}
   if (PagePanel.Width +BORD <= ScrollBox1.Width) and     
      (PagePanel.Height +BORD <= ScrollBox1.Height) then
     begin
     ScrollBox1.HorzScrollBar.Visible := False;
     ScrollBox1.VertScrollBar.Visible := False;
     end
   else
     begin
     ScrollBox1.HorzScrollBar.Visible := True;
     ScrollBox1.VertScrollBar.Visible := True;
     end;

   // Set the Zoom Variable
   Zoom := z;
   ZoomLabel.Caption := Format('%1.0n', [z * 100]) + '%';
end;

procedure TPreviewForm.DrawMetaFile(PB: TPaintBox; mf: TMetaFile);
begin
   PB.Canvas.Draw(0, 0, mf);
end;

procedure TPreviewForm.PBPaint(Sender: TObject);
var
   PB       : TPaintBox;
   x1, y1   : integer;
   x, y     : integer;
   Factor   : double;
   Draw     : boolean;
   Page     : integer;
begin
   PB := Sender as TPaintBox;

   if PB = PB1 then
      begin
         Draw := CurPage < MFPrinter.LastAvailablePage;
         Page := CurPage;
      end
   else
      begin
         // PB2
         Draw := TwoPageBut.Down and (CurPage+1 < MFPrinter.LastAvailablePage);
         Page := CurPage + 1;
      end;

   SetMapMode(PB.Canvas.Handle, MM_ANISOTROPIC);
   SetWindowExtEx(PB.Canvas.Handle, MFPrinter.PaperWidth, MFPrinter.PaperHeight, nil);    
   SetViewportExtEx(PB.Canvas.Handle, PB.Width, PB.Height, nil);
   SetWindowOrgEx(PB.Canvas.Handle, -MFPrinter.OffsetX, -MFPrinter.OffsetY, nil);
   if Draw then
      DrawMetaFile(PB, MFPrinter.MetaFiles[Page]);

   if GridBut.Down then
      begin
         SetWindowOrgEx(PB.Canvas.Handle, 0, 0, nil);   
         PB.Canvas.Pen.Color := clLtGray;
         if UnitsBox.ItemIndex = 0 then
           Factor := 1.0
         else Factor := 2.54;

         for x := 1 to Round(MFPrinter.PaperWidth / MFPrinter.PixelsPerInchX * Factor) do
            begin
               x1 := Round(MFPrinter.PixelsPerInchX * x / Factor);
               PB.Canvas.MoveTo(x1, 0);
               PB.Canvas.LineTo(x1, MFPrinter.PaperHeight);
            end;

      for y := 1 to Round(MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY * Factor) do
         begin
            y1 := Round(MFPrinter.PixelsPerInchY * y / Factor);
            PB.Canvas.MoveTo(0, y1);
            PB.Canvas.LineTo(MFPrinter.PaperWidth, y1);
         end;
      end;
end;

procedure TPreviewForm.GridButClick(Sender: TObject);
begin
   PB1.Invalidate;
   PB2.Invalidate;
end;

procedure TPreviewForm.OnHint(Sender: TObject);
begin
   HintLabel.Caption := Application.Hint;
end;


procedure TPreviewForm.FormShow(Sender: TObject);
begin
   CurPage := 0;
   OldHint := Application.OnHint;
   Application.OnHint := OnHint;
   CheckEnable;
  {$ifdef delphi7_plus}
   PagePanel.ParentBackground := False;
   PagePanel2.ParentBackground := False;
  {$endif}
   ScrollBox1Resize(Nil);   {make sure it gets sized}
end;

procedure TPreviewForm.SetCurPage(Val: integer);
var
   tmp : integer;
begin
   FCurPage := Val;
   tmp := 0;
   if MFPrinter <> nil then
      tmp := MFPrinter.LastAvailablePage;
   CurPageLabel.Caption := Format('Page %d of %d', [Val+1, tmp]);
   PB1.Invalidate;
   PB2.Invalidate;
end;

procedure TPreviewForm.ZoomBoxChange(Sender: TObject);
begin
   ScrollBox1Resize(nil);
   ScrollBox1Resize(nil);
end;

procedure TPreviewForm.TwoPageButClick(Sender: TObject);
begin
   ZoomBox.ItemIndex := 0;
   ScrollBox1Resize(nil);
end;

procedure TPreviewForm.NextPageButClick(Sender: TObject);
begin
   CurPage := CurPage + 1;
   CheckEnable;
end;

procedure TPreviewForm.PrevPageButClick(Sender: TObject);
begin
   CurPage := CurPage - 1;
   CheckEnable;
end;

procedure TPreviewForm.CheckEnable;
begin
   NextPageBut.Enabled := CurPage+1 < MFPrinter.LastAvailablePage;
   PrevPageBut.Enabled := CurPage > 0;

   NextPageSpeed.Enabled := NextPageBut.Enabled;
   PrevPageSpeed.Enabled := PrevPageBut.Enabled;

   FirstPageSpeed.Enabled := PrevPageBut.Enabled;
   LastPageSPeed.Enabled  := NextPageBut.Enabled;

   PageNumSpeed.Enabled := MFPrinter.LastAvailablePage > 1;
end;


procedure TPreviewForm.FirstPageSpeedClick(Sender: TObject);
begin
   CurPage := 0;
   CheckEnable;
end;

procedure TPreviewForm.LastPageSpeedClick(Sender: TObject);
begin
   CurPage := MFPrinter.LastAvailablePage-1;
   CheckEnable;
end;

procedure TPreviewForm.ZoomCursorButClick(Sender: TObject);
begin
   PB1.Cursor := crZoom;
   PB2.Cursor := crZoom;
end;

procedure TPreviewForm.HandCursorButClick(Sender: TObject);
begin
   PB1.Cursor := crHandDrag;
   PB2.Cursor := crHandDrag;  
end;

procedure TPreviewForm.PB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   sx, sy : single;
   nx, ny : integer;
begin
   if ZoomCursorBut.Down then
      begin
         sx := X / PagePanel.Width;
         sy := Y / PagePanel.Height;

         if (ssLeft  in Shift) and (Zoom < 20.0) then Zoom := Zoom * ZOOMFACTOR;
         if (ssRight in Shift) and (Zoom > 0.1) then Zoom := Zoom / ZOOMFACTOR;
         ZoomBox.ItemIndex := 2;
         ScrollBox1Resize(nil);

         nx := TRUNC(sx * PagePanel.Width);
         ny := TRUNC(sy * PagePanel.Height);
         ScrollBox1.HorzScrollBar.Position := nx - ScrollBox1.Width div 2;
         ScrollBox1.VertScrollBar.Position := ny - ScrollBox1.Height div 2;
      end;

   if HandCursorBut.Down then
      begin
         DownX  := X;
         DownY  := Y;
         Moving := True;
      end;
end;

procedure TPreviewForm.PB1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Moving then
      begin
         ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + (DownX - X);
         ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + (DownY - Y);
      end;
end;

procedure TPreviewForm.PB1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   Moving := False;
end;

procedure TPreviewForm.PrintButClick(Sender: TObject);
var
  StatusForm: TPrnStatusForm;
  Dummy: boolean;
begin
with PrintDialog1 do
  begin
  MaxPage  := 9999;
  ToPage   := 1;
  Options  := [poPageNums];
  StatusForm := TPrnStatusForm.Create(Self);
  if Execute then
    if PrintRange = prAllPages then
      StatusForm.DoPrint(Viewer, FromPage, 9999, Dummy)
    else
      StatusForm.DoPrint(Viewer, FromPage, ToPage, Dummy);
  StatusForm.Free;
  end;
end;

procedure TPreviewForm.PageNumSpeedClick(Sender: TObject);
var
   gp : TGoPageForm;
begin
   gp := TGoPageForm.Create(Self);
   gp.PageNum.MaxValue := MFPrinter.LastAvailablePage;
   gp.PageNum.Value := CurPage + 1;

   if gp.ShowModal = mrOK then
      begin
         CurPage := gp.PageNum.Value - 1;
         CheckEnable;
      end;
   gp.Free;
end;

procedure TPreviewForm.OnePageButMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   ZoomBox.ItemIndex := 0;
   ScrollBox1Resize(nil);
end;

procedure TPreviewForm.FitPageButClick(Sender: TObject);
begin
   ZoomBox.ItemIndex := 0;
   ZoomBoxChange(nil);
end;

procedure TPreviewForm.FitWidthButClick(Sender: TObject);
begin
   ZoomBox.ItemIndex := 1;
   ZoomBoxChange(nil);
end;

procedure TPreviewForm.UnitsBoxChange(Sender: TObject);
begin
if GridBut.down then
  begin
  PB1.Invalidate;
  PB2.Invalidate;
  end;
end;

end.
