program HtmlDemo;
{A program to demonstrate the ThtmlViewer component}

uses
  Forms,
  demounit in 'demounit.pas' {Form1},
  Submit in 'Submit.pas' {SubmitForm},
  Fontdlg in 'Fontdlg.pas' {FontForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
  ImgForm in 'ImgForm.pas' {ImageForm},
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
