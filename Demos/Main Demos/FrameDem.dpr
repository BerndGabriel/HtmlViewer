program FrameDem;
{A program to demonstrate the TFrameViewer component}

{$include ..\..\source\htmlcons.inc}

uses
{$ifdef LCL}
  Interfaces,
{$endif}
{$ifdef Windows}
  PreviewForm,
{$endif}
  Forms
  ,PrintStatusForm
  ,Gopage
  ,FDemUnit
  ,Fontdlg
  ,Htmlabt
  ,ImgForm
  ,DemoSubs
  ;

{$R *.res}

{$IFDEF WINDOWS}{$R FrameDem.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


