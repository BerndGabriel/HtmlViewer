{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2011 by HtmlViewer Team
Copyright (c) 2012 by Angus Robertson delphi@magsys.co.uk
Copyright (c) 2013-2016 by HtmlViewer Team

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

program FrameBrowserDemoIcsW;

uses
  HighDPIInit in '..\..\..\bega\BegaViewer\HighDPIInit.pas',
  Forms,
  ProxyFormUnit in 'ProxyFormUnit.pas' {ProxyForm},
  LogFormUnit in 'LogFormUnit.pas' {LogForm},
  InfoFormUnit in 'InfoFormUnit.pas' {InfoForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
  FBrDemUnit in 'FBrDemUnit.pas' {HTTPForm},
  CacheUnit in 'CacheUnit.pas',
  UrlConVclZip in 'UrlConVclZip.pas',
  UrlConIcs in 'UrlConIcs.pas',
  AuthorizeFormUnit in 'AuthorizeFormUnit.pas' {AuthorizeForm},
  DownloadFormUnit in 'DownloadFormUnit.pas' {DownloadForm},
  HtmlDemoUtils in 'HtmlDemoUtils.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Experimental Browser using ICSv7/v8';
  Application.CreateForm(THTTPForm, HTTPForm);
  Application.Run;
end.
