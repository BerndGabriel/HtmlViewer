{
Version   11.3
Copyright (c) 2012 by HtmlViewer Team

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

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
