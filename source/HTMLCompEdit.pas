{
Version   11
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

{$I htmlcons.inc}

unit HTMLCompEdit;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls,
  Dialogs, ExtCtrls,
  designintf, DesignEditors;

type

  THtComponentEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(index: Integer): string; override;
    procedure ExecuteVerb(index: Integer); override;
  end;

  THTMLEditor = class(THtComponentEditor)
  end;

  TFMVEditor = class(THtComponentEditor)
  end;

  TFMBEditor = class(THtComponentEditor)
  end;

procedure Register;

implementation

uses
  htmlview, htmlun2, framview, frambrwz;

procedure Register;
begin
  RegisterComponentEditor(THTMLViewer, THTMLEditor);
  RegisterComponentEditor(TFrameViewer, TFMVEditor);
  RegisterComponentEditor(TFrameBrowser, TFMBEditor);
end;

{ THtComponentEditor }

function THtComponentEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function THtComponentEditor.GetVerb(index: Integer): string;
begin
  Result := 'About...';
end;

procedure THtComponentEditor.ExecuteVerb(index: integer);
begin
  MessageDlg(
    GetComponent.ClassName + ', Version (V) ' + VersionNo + #13#13 +
    'Copyright (C) 2011-2012 by the HtmlViewer Project Team'#13 +
    '        Bernd Gabriel'#13 +
    '        J. Peter Mugaas'#13#13 +
    'Copyright (C) 2008-2010 by the HtmlViewer Project Team'#13 +
    '        Patrick van Logchem'#13 +
    '        Sebastian Zierer'#13 +
    '        Arvid Winkelsdorf'#13 +
    '        Bernd Gabriel'#13#13 +
    'Copyright (C) 1995-2008 by L. David Baldwin',
    mtInformation, [mbOk], 0);
end;

end.
