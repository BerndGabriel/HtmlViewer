program HeaderFooterDemo;
{A program to demonstrate the ThtmlViewer component}

uses
  Forms,
  HFUnit in 'HFUnit.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
