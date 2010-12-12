program FrameDem;
{A program to demonstrate the TFrameViewer component}

{$include ..\..\source\htmlcons.inc}

uses
  Forms,
{$ifdef LCL}
  Interfaces, LResources,
{$endif}
  PRINTSTATUSFORM in 'PRINTSTATUSFORM.PAS' {PrnStatusForm},
  GOPAGE in 'GOPAGE.PAS' {GoPageForm},
  PREVIEWFORM in 'PREVIEWFORM.PAS' {PreviewForm},
  FDEMUNIT in 'FDEMUNIT.PAS' {Form1},
  FONTDLG in 'FONTDLG.PAS' {FontForm},
  SUBMIT in 'SUBMIT.PAS' {SubmitForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
  IMGFORM in 'IMGFORM.PAS' {ImageForm},
  DemoSubs in 'DemoSubs.pas';

{$R *.RES}

{$IFDEF WINDOWS}{$R FrameDem.rc}{$ENDIF}

begin
{$ifdef LCL}
  {$I FrameDem.lrs}
{$endif}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
