program HeaderFooterDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{A program to demonstrate the ThtmlViewer component}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  HFUnit in 'HFUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
