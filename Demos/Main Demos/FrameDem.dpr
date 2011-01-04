program FrameDem;
{A program to demonstrate the TFrameViewer component}

{$include ..\..\source\htmlcons.inc}

uses
{$ifdef LCL}
  Interfaces,
{$else}
  PreviewForm,
{$endif}
  Forms, printer4lazarus
  ,PrintStatusForm
  ,Gopage
  ,FDemUnit
  ,Fontdlg
  ,Htmlabt
  ,ImgForm
  ,DemoSubs
  ;

{$R *.res}

{$ifdef MsWindows}{$R FrameDem.rc}{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

