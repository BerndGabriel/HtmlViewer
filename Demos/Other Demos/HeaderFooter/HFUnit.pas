{
Version   11.5
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

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

{Demo program to show usage of the OnPrintHTMLHeader/Footer events to add
 Headers and Footers to the Print job.}

{$ifdef ver140}
{$warn Symbol_Platform Off}   
{$endif}
{$ifdef ver150}
{$warn Symbol_Platform Off}   
{$endif}

unit HFUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  ShellAPI, WinProcs, WinTypes,
{$ELSE}
  PrintersDlgs,
{$ENDIF}
  Printers,
  SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Menus, StdCtrls, Htmlview, URLSubs, HTMLUn2;
                                        
type
  TForm1 = class(TForm)
    OpenDialog: TOpenDialog;
    MainMenu: TMainMenu;
    Panel2: TPanel;
    Panel3: TPanel;
    File1: TMenuItem;
    Open: TMenuItem;
    Edit1: TEdit;
    ReloadButton: TButton;
    Exit1: TMenuItem;
    PrintDialog: TPrintDialog;
    Viewer: THTMLViewer;
    Print1: TMenuItem;
    Printpreview: TMenuItem;
    procedure OpenFileClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure PrintpreviewClick(Sender: TObject);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
  private
    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
  public

  end;

var
  Form1: TForm1;

implementation

{$ifdef MsWindows}
uses
  PreviewForm;
{$endif}

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
begin
if Screen.Width <= 640 then
  Position := poDefault;  {keeps form on screen better}

OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
Caption := 'Header/Footer Demo';
{$ifdef MsWindows}
  DragAcceptFiles(Handle, True);
{$endif}
end;

procedure TForm1.FormShow(Sender: TObject);
var
  S: string;
  I: integer;
begin
if (ParamCount >= 1) then
  begin            {Parameter is file to load}
  S := CmdLine;
  I := Pos('" ', S);
  if I > 0 then
    Delete(S, 1, I+1)     {delete EXE name in quotes}
  else Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
  I := Pos('"', S);
  while I > 0 do     {remove any quotes from parameter}
    begin
    Delete(S, I, 1);
    I := Pos('"', S);
    end;
  Viewer.LoadFromFile(HtmlToDos(Trim(S)));
  end;
end;

procedure TForm1.OpenFileClick(Sender: TObject);
begin
if Viewer.CurrentFile <> '' then
  OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
if OpenDialog.Execute then
  begin
  Update;  
  Viewer.LoadFromFile(OpenDialog.Filename);
  Caption := Viewer.DocumentTitle;
  end;
end;

procedure TForm1.ReloadButtonClick(Sender: TObject);
{the Reload button was clicked}
begin
with Viewer do
  begin
  ReLoadButton.Enabled := False;
  ReLoad;
  ReLoadButton.Enabled := CurrentFile <> '';
  Viewer.SetFocus;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
Close;
end;

procedure TForm1.Print1Click(Sender: TObject);
begin
{$ifdef MsWindows}
with PrintDialog do
  if Execute then
    if PrintRange = prAllPages then
      viewer.Print(1, 9999)
    else
      Viewer.Print(FromPage, ToPage);
{$endif}
end;

procedure TForm1.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
begin
if ProcessingOn then
  begin    {disable various buttons and menuitems during processing}
  ReLoadButton.Enabled := False;
  Print1.Enabled := False;
  PrintPreview.Enabled := False;
  Open.Enabled := False;
  end
else
  begin
  ReLoadButton.Enabled := Viewer.CurrentFile <> '';
  Print1.Enabled := Viewer.CurrentFile <> '';
  PrintPreview.Enabled := Viewer.CurrentFile <> '';
  Open.Enabled := True;
  end;
end;

procedure TForm1.wmDropFiles(var Message: TMessage);
{$ifdef MsWindows}
var
  S: string[200];
  Ext: string;
  Count: integer;
{$endif}
begin
{$ifdef MsWindows}
Count := DragQueryFile(Message.WParam, 0, @S[1], 200);
Length(S) := Count;
DragFinish(Message.WParam);
if Count >0 then
  begin
  Ext := LowerCase(ExtractFileExt(S));
  if (Ext = '.htm') or (Ext = '.html') then
    Viewer.LoadFromFile(S);
  end;
{$endif}
Message.Result := 0;
end;

procedure TForm1.PrintpreviewClick(Sender: TObject);
{$ifdef MsWindows}
var
  pf: TPreviewForm;
  Abort: boolean;
{$endif}
begin
{$ifdef MsWindows}
pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
try
  if not Abort then
    pf.ShowModal;
finally
  pf.Free;
  end;
{$endif}
end;

function ReplaceStr(Const S, FromStr, ToStr: string): string;
{replace FromStr with ToStr in string S.
 for Delphi 6, 7, AnsiReplaceStr may be used instead.}
var
  I: integer;
begin
I := Pos(FromStr, S);
if I > 0 then
  begin
  Result := S;
  Delete(Result, I, Length(FromStr));
  Insert(ToStr, Result, I);
  end;
end;

procedure TForm1.ViewerPrintHTMLHeader(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
{This sample HTML header is loaded from a file.  Since the header content
 doesn't change between pages, it need only be loaded for the first page.}
var
  SL: TStringList;
  S: string;
begin
if HFViewer.DocumentSource = '' then
  begin  {only need to load once}
  SL := TStringList.Create;
  try
    SL.LoadFromFile(ExtractFilePath(Application.ExeName)+'ace.htm');
    {replace the marker strings in the HTML with the Title and Filename}
    S := ReplaceStr(SL.Text, '#title', Viewer.DocumentTitle);
    S := ReplaceStr(S, '#file', Viewer.CurrentFile);
    HFViewer.LoadFromString(S, ExtractFilePath(Application.ExeName));
  finally
    SL.Free;
    end;
  end;
end;

procedure TForm1.ViewerPrintHTMLFooter(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
{This sample footer is stored within the program.  It will display the page
 number and date so the content will change for each page.}
const
  Text =  '<html><head><style>'+
            'body  {font: Arial 10pt bold;}'+
          '</style></head>'+
          '<body marginwidth="0" bgcolor="Pink">'+
          '<table border="0" cellspacing="2" cellpadding="1" width="100%">'+
            '<tr>'+
              '<td>#date</td><td align="right">#page</td>'+
            '</tr>'+
          '</table></body></html>';

var
  S: string;
begin
{replace the marker strings with the date and page number}
S := ReplaceStr(Text, '#date', DateToStr(Date));
S := ReplaceStr(S, '#page', 'Page '+IntToStr(NumPage));
HFViewer.LoadFromString(S);
end;

end.
