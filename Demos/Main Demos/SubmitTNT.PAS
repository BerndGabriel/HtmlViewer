{
Version   11
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

*********************************************************
*                                                       *
*           Thanks to Mike Lischke for his              *
*        assistance with the Unicode conversion         *
*                                                       *
*********************************************************

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

unit SubmitTnt;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
{$ifdef LCL}
  LResources,
{$endif}
  TntStdCtrls;

type
  TSubmitFormTnt = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    ActionText: TTntEdit;
    MethodText: TTntEdit;
    ResultBox: TTntListBox;
    procedure Button1Click(Sender: TObject);
  end;

function SubmitForm: TSubmitFormTnt;

implementation

{$ifdef LCL}
{$else}
{$R *.DFM}
{$endif}

var
  VForm: TSubmitFormTnt;

function SubmitForm: TSubmitFormTnt;
begin
  if VForm = nil then
    VForm := TSubmitFormTnt.Create(Application);
  Result := VForm;
end;

procedure TSubmitFormTnt.Button1Click(Sender: TObject);
begin
Close;
end;

{$ifdef LCL}
initialization
{$I Submit.lrs}
{$else}
{$endif}
end.
