program FlashDemo;

uses
  Forms,
  FlashUnit in 'FlashUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
