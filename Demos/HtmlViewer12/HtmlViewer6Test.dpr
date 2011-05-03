program HtmlViewer6Test;

uses
  Forms,
  HtmlViewer6TestForm in 'HtmlViewer6TestForm.pas' {FormHtmlViewer6Test};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHtmlViewer12Test, FormHtmlViewer12Test);
  Application.Run;
end.
