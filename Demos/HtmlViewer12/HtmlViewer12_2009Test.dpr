program HtmlViewer12_2009Test;

uses
  Forms,
  HtmlViewer12_2009TestForm in 'HtmlViewer12_2009TestForm.pas' {FormHtmlViewer6Test};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHtmlViewer12Test, FormHtmlViewer12Test);
  Application.Run;
end.
