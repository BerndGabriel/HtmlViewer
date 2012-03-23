unit PrintStatusForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, HTMLView, MetaFilePrinter;

type
  TPrnStatusForm = class(TForm)
    StatusLabel: TLabel;
    CancelButton: TBitBtn;
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    Viewer: ThtmlViewer;
    Canceled: boolean;
    MFPrinter: TMetaFilePrinter;
    FromPage, ToPage: integer;
    procedure PageEvent(Sender: TObject; PageNum: integer; var Stop: boolean);
  public
    { Public declarations }
  procedure DoPreview(AViewer: ThtmlViewer; AMFPrinter: TMetaFilePrinter;
              var Abort: boolean);
  procedure DoPrint(AViewer: ThtmlViewer; FromPg, ToPg: integer;
              var Abort: boolean);
  end;

var
  PrnStatusForm: TPrnStatusForm;

implementation

{$R *.DFM}


procedure TPrnStatusForm.DoPreview(AViewer: ThtmlViewer; AMFPrinter: TMetaFilePrinter;
              var Abort: boolean);
begin
Viewer := AViewer;
MFPrinter := AMFPrinter;
Viewer.OnPageEvent := PageEvent;
try                                   
  Show;
  Viewer.PrintPreview(MFPrinter);
  Hide;
  Abort := Canceled;
finally
  Viewer.OnPageEvent := Nil;      
  end;
end;

procedure TPrnStatusForm.DoPrint(AViewer: ThtmlViewer; FromPg, ToPg: integer;
              var Abort: boolean);
begin
Viewer := AViewer;
FromPage := FromPg;
ToPage := ToPg;
Viewer.OnPageEvent := PageEvent;
try                                   
  Show;
  Viewer.Print(FromPage, ToPage);          
  Hide;
  Abort := Canceled;
finally
  Viewer.OnPageEvent := Nil;      
  end;
end;

procedure TPrnStatusForm.PageEvent(Sender: TObject; PageNum: integer; var Stop: boolean);
begin   
if Canceled then
  Stop := True
else
  if PageNum = 0 then
    StatusLabel.Caption := 'Formating'
  else
    StatusLabel.Caption := 'Page Number '+ IntToStr(PageNum);
Update;
end;

procedure TPrnStatusForm.CancelButtonClick(Sender: TObject);
begin
Canceled := True;
end;

end.
