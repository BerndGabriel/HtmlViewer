program HTMLEditor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  Interfaces,
{$ENDIF}
  Forms,
  HTMLEd1 in 'HTMLEd1.pas' {Form1},
  Htmlabt in '..\..\Main Demos\Htmlabt.pas' {AboutBox};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
