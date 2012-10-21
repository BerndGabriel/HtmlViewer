program LiveHtml;

uses
  Forms,
  LiveHtmlForm in 'LiveHtmlForm.pas' {FormLiveHtml};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLiveHtml, FormLiveHtml);
  Application.Run;
end.
