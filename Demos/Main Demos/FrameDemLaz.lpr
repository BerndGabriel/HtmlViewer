{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2023 by Bernd Gabriel

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

program FrameDemLaz;

{A program to demonstrate the TFrameViewer component}

{$mode delphi}{$H+}

uses
  Interfaces,
  Forms,
  Printer4Lazarus,
  Fontdlg in 'Fontdlg.pas' {FontForm},
  Htmlabt in 'Htmlabt.pas' {AboutBox},
  ImgForm in 'ImgForm.pas' {ImageForm},
  Submit in 'Submit.pas' {SubmitForm},
  FDemUnit in 'FDemUnit.PAS';

{$R *.res}

begin
  Application.Title:='';
  Application.Initialize;
  Application.Scaled:=True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

