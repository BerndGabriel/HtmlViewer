{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2023 by HtmlViewer Team

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

unit HtmlDemoUtils;

{$include ..\..\source\htmlcons.inc}

interface

uses
  Types, Forms;

function htGetNiceFormSize(const Monitor: TMonitor; FormPixelsPerInch: Integer): TRect;

implementation

uses
  Math;

//-- BG ------------------------------------------------------- 04.02.2023 --
function htGetNiceFormSize(const Monitor: TMonitor; FormPixelsPerInch: Integer): TRect;
var
  DX, DY: Double;
begin
  DX := Monitor.Width / 10;
  Dy := Monitor.Height / 8;
{$ifdef LCL}
  // LCL: PixelsPerInch is not yet changed to actual Monitor.PixelsPerInch.
  // but will be before showing. Therefore we calculate a smaller start size
  // and LCL will resize it later:
  DX *= FormPixelsPerInch / Monitor.PixelsPerInch;
  Dy *= FormPixelsPerInch / Monitor.PixelsPerInch;
{$endif}
  Result.Left := Floor(DX);
  Result.Top := Floor(DY);
  Result.Right := Result.Left + Floor(DX * 8);
  Result.Bottom := Result.Top + Floor(DY * 6);
end;

end.
