program FrameBrowserIcs;

uses
  Forms,
  FBUnitIcs in 'FBUnitIcs.pas' {HTTPForm},
  UrlConIcs in 'UrlConIcs.pas',
  AuthUnit in 'AuthUnit.pas' {AuthForm},
  HTMLAbt in 'HTMLAbt.pas' {AboutBox},
  CachUnitId in 'CachUnitId.pas',
  DownLoadId in 'DownLoadId.pas' {DownLoadForm},
  ProxyDlg in 'ProxyDlg.pas' {ProxyForm},
  PreviewForm in 'PreviewForm.pas' {PreviewForm},
  Gopage in 'Gopage.pas' {GoPageForm},
  PrintStatusForm in 'PrintStatusForm.pas' {PrnStatusForm},
  logwin in 'logwin.pas' {LogForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Experimental Browser using ICSv7';
  Application.CreateForm(THTTPForm, HTTPForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TAuthForm, AuthForm);
  Application.Run;
end.
