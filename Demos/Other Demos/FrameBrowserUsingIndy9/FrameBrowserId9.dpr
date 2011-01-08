program FrameBrowserId9;

uses
  Forms,
  FBUnitId9 in 'FBUnitId9.pas' {HTTPForm},
  HttpAsyncId9 in 'HttpAsyncId9.pas',
  UrlConId9 in 'UrlConId9.pas',
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
