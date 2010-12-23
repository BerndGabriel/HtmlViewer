program MetaDemo;

uses
  Forms,
  MetaDemoUnit in 'MetaDemoUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
