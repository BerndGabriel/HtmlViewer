program FrameDem;
{A program to demonstrate the TFrameViewer component}

uses
  Forms,
  FDemUnit in 'FDEMUNIT.PAS' {Form1},
  Fontdlg in 'FONTDLG.PAS' {FontForm},
  Submit in 'SUBMIT.PAS' {SubmitForm},
  HTMLAbt in 'HTMLABT.PAS' {AboutBox},
  PrintStatusForm in 'PRINTSTATUSFORM.PAS' {PrnStatusForm},
  Gopage in 'GOPAGE.PAS' {GoPageForm},
  PreviewForm in 'PREVIEWFORM.PAS' {PreviewForm},
  ImgForm in 'IMGFORM.PAS' {ImageForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
