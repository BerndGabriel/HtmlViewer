{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2016 by HtmlViewer Team

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

Note that the source modules HTMLGIF1.PAS, DITHERUNIT.PAS and UrlConId*.PAS
are covered by separate copyright notices located in those modules.

Thanks to the Indy Pit Crew for updating *Id9 to *Id10.
}

program FrameBrowserId10;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$R 'fbHelp.res' 'Resources\fbHelp.rc'}

uses
  {$ifdef FPC}
  Interfaces,
  {$endif }
  Forms,
  FBUnitId10 in 'FBUnitId10.pas' {HTTPForm},
  UrlConId10 in 'UrlConId10.pas',
  AuthUnit in 'AuthUnit.pas' {AuthForm},
  CachUnitId in 'CachUnitId.pas',
  DownLoadId in 'DownLoadId.pas' {DownLoadForm},
  ProxyDlg in 'ProxyDlg.pas' {ProxyForm},
  Htmlabt in '..\..\Main Demos\Htmlabt.pas' {AboutBox},
  UrlConVclZip in 'UrlConVclZip.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THTTPForm, HTTPForm);
  Application.CreateForm(TAuthForm, AuthForm);
  Application.Run;
end.
