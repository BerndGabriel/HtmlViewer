{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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

unit DemoSubs;

{$include ..\..\source\htmlcons.inc}

interface

uses
{$ifdef LCL}
  LclIntf, LclType,
{$else}
  Windows,
{$endif}
  SysUtils;

procedure StartProcess(CommandLine: string; ShowWindow: Word);

implementation

procedure StartProcess(CommandLine: string; ShowWindow: Word);
var
  PC: array[0..255] of {$ifdef Compiler20_Plus} WideChar {$else} AnsiChar {$endif};
{$ifdef Compiler20_Plus}
  si: TStartupInfo;
  pi: TProcessInformation;
{$endif}
begin
  StrPCopy(PC, CommandLine);
{$ifdef Compiler20_Plus}
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.lpReserved := nil;
  si.lpDesktop := nil;
  si.lpTitle := nil;
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := ShowWindow;
  CreateProcess(nil, PC, nil, nil, False, 0, nil, nil, si, pi);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
{$else}
{$ifdef LCL}
  OpenDocument(PC);
{$else}
  WinExec(PC, ShowWindow);
{$endif}
{$endif}
end;

end.
