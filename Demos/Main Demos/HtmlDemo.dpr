program HtmlDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{A program to demonstrate the ThtmlViewer component}

uses
{$ifdef FPC}
  Interfaces,
{$else}
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
{$endif}
  Forms,
  demounit in 'demounit.pas' {Form1},
  SUBMIT in 'SUBMIT.PAS' {SubmitForm},
  Fontdlg in 'Fontdlg.pas' {FontForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
  ImgForm in 'ImgForm.pas' {ImageForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
//  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
