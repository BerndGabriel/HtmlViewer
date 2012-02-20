unit PrintStatusForm;

{$include ..\..\source\htmlcons.inc}

interface

uses
{$ifdef LCL}
  LclIntf, LclType, PrintersDlgs,
{$else}
  Windows,
{$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, Printers,
  vwPrint,
  MetaFilePrinter,
  HtmlView;

type
  TPrnStatusForm = class(TForm)
    InfoLabel: TLabel;
    StatusLabel: TLabel;
    CancelButton: TBitBtn;
    procedure CancelButtonClick(Sender: TObject);
  private
    FCanceled: Boolean;
    FCopyNo, FCopies: Integer;
    procedure PageEvent(Sender: TObject; PageNum: Integer; var Stop: boolean);
  public
    procedure DoPreview(Viewer: THtmlViewer; MFPrinter: TMetaFilePrinter; var Abort: Boolean);
    procedure DoPrint(Viewer: THtmlViewer; FromPg, ToPg: Integer; var Abort: Boolean); overload;
    procedure Print(Viewer: THtmlViewer; Printer: ThtPrinter; FromPage, ToPage, Copies: Integer; var Abort: Boolean); overload;
  end;

procedure PrintWithDialog(Owner: TComponent; PrintDialog: TPrintDialog; Viewer: THtmlViewer; PageCount: Integer = MaxInt);

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure PrintWithDialog(Owner: TComponent; PrintDialog: TPrintDialog; Viewer: THtmlViewer; PageCount: Integer);
var
  StatusForm: TPrnStatusForm;
  Dummy: Boolean;
  CopyCount, EndPage: Integer;
  Printer: TvwPrinter;
  Capabilities: TPrinterCapabilities;
begin
  with PrintDialog do
  begin
    MaxPage  := PageCount;
    ToPage   := 1;
    Options  := [poPageNums];
    if Execute then
    begin
      if PrintRange = prAllPages then
        EndPage := MaxInt
      else
        EndPage := ToPage;

      StatusForm := TPrnStatusForm.Create(Owner);
      try
        Printer := TvwPrinter.Create(Owner);
        try
{$ifdef LCL}
          Capabilities := [];
          if Printers.Printer.CanRenderCopies then
            Include(Capabilities, pcCopies);
{$else}
          Capabilities := Printers.Printer.Capabilities;
{$endif}
          if (Collate and not (pcCollation in Capabilities)) or ((Copies > 1) and not (pcCopies in Capabilities)) then
          begin
            CopyCount := Copies;
            try
              Copies := 1; // otherwise the printer might produce copies as well.
              StatusForm.Print(Viewer, Printer, FromPage, EndPage, CopyCount, Dummy);
            finally
              Copies := CopyCount; // remember copy count next time the dialog is opened again.
            end;
          end
          else
            StatusForm.Print(Viewer, Printer, FromPage, EndPage, 1, Dummy);
        finally
          Printer.Free;
        end;
      finally
        StatusForm.Free;
      end;
    end;
  end;
end;


procedure TPrnStatusForm.DoPreview(Viewer: ThtmlViewer; MFPrinter: TMetaFilePrinter; var Abort: Boolean);
begin
  Print(Viewer, MFPrinter, 1, MaxInt, 1, Abort);
end;

procedure TPrnStatusForm.DoPrint(Viewer: ThtmlViewer; FromPg, ToPg: Integer; var Abort: Boolean);
begin
  Print(Viewer, nil, FromPg, ToPg, 1, Abort);
end;

procedure TPrnStatusForm.PageEvent(Sender: TObject; PageNum: integer; var Stop: Boolean);
begin
  if FCanceled then
    Stop := True
  else if FCopies = 1 then
    if PageNum = 0 then
      StatusLabel.Caption := 'Formating'
    else
      StatusLabel.Caption := Format('Printing page number %d', [PageNum])
  else
    if PageNum = 0 then
      StatusLabel.Caption := Format('Formating copy %d', [FCopyNo, FCopies])
    else
      StatusLabel.Caption := Format('Printing page number %d of copy %d', [PageNum, FCopyNo]);
  Update;
end;

procedure TPrnStatusForm.Print(
  Viewer: THtmlViewer; Printer: ThtPrinter; FromPage, ToPage, Copies: Integer; var Abort: Boolean);
var
  What, CopyInfo, PagesInfo: string;
  OldPageEvent: TPageEvent;
begin
  if Caption = Name then
    Caption := Application.Title;
    
  FCopies := Copies;
  FCopyNo := 1;

  if Printer is TMetaFilePrinter then
    What := 'Previewing'
  else
    What := 'Printing';

  if Copies > 1 then
    CopyInfo := Format(' %d copies of', [Copies])
  else
    CopyInfo := '';

  if FromPage = ToPage then
    PagesInfo := Format(' page %d', [FromPage])
  else if (FromPage > 1) or (ToPage < MaxInt) then
    PagesInfo := Format(' pages %d to %d', [FromPage, ToPage])
  else
    PagesInfo := ' document';

  InfoLabel.Caption := What + CopyInfo + PagesInfo;

  Show;
  OldPageEvent := Viewer.OnPageEvent;
  try
    Viewer.OnPageEvent := PageEvent;

    while FCopyNo <= FCopies do
    begin
      Viewer.Print(Printer, FromPage, ToPage, ppAuto);
      Inc(FCopyNo);
    end;
  finally
    Viewer.OnPageEvent := OldPageEvent;
  end;
  Hide;
  Abort := FCanceled;
end;

procedure TPrnStatusForm.CancelButtonClick(Sender: TObject);
begin
  FCanceled := True;
  CancelButton.Enabled := False;
end;

end.
