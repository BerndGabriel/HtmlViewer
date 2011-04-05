program HtmlViewer12Test;

uses
  Forms,
  HtmlViewer12TestForm in 'HtmlViewer12TestForm.pas' {FormHtmlViewer12Test};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormHtmlViewer12Test, FormHtmlViewer12Test);
  Application.Run;
end.
