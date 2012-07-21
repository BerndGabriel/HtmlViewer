program FrameBrowserId10;

uses
  Forms,
  FBUnitId10 in 'FBUnitId10.pas' {HTTPForm},
  HttpAsyncId10 in 'HttpAsyncId10.pas',
  UrlConId10 in 'UrlConId10.pas',
  AuthUnit in 'AuthUnit.pas' {AuthForm},
  HTMLAbt in 'HTMLAbt.pas' {AboutBox},
  CachUnitId in 'CachUnitId.pas',
  DownLoadId in 'DownLoadId.pas' {DownLoadForm},
  ProxyDlg in 'ProxyDlg.pas' {ProxyForm},
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THTTPForm, HTTPForm);
  Application.CreateForm(TAuthForm, AuthForm);
  Application.Run;
end.
