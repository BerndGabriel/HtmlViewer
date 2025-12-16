{
Version   11.11
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2025 by HtmlViewer Team

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

unit AVIUnit;

interface

uses
{$ifdef LCL}
  LclIntf, LclType,
{$else}
  Windows, mmSystem, Vcl.MPlayer,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Htmlview, ExtCtrls, HTMLSubs, HTMLUn2;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Viewer: THTMLViewer;
{$ifndef LCL}
    MediaPlayer: TMediaPlayer;
{$endif}
    LoadButton: TButton;
    PlayButton: TButton;
    procedure ViewerPanelCreate(Sender: TObject; const AName, AType,
      SRC: String; Panel: ThvPanel);
    procedure ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
    procedure LoadButtonClick(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    APanel: ThvPanel;
    Memo: TMemo;
  public

  end;

var
  Form1: TForm1;

implementation

{$IFDEF LCL}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  URLSubs;

var
  Avi: String;
  I: Integer;

procedure TForm1.ViewerPanelCreate(Sender: TObject; const AName, AType,
  SRC: String; Panel: ThvPanel);
{the OnPanelCreate event}
begin
with Panel do
  begin
  color := clBtnFace;
  BorderStyle := bsSingle;
  if CompareText(AName, 'AviPanel') = 0 then
    begin
    APanel := Panel;
    Memo := TMemo.Create(APanel);
    Memo.Align := alClient;
    Memo.Hide;
    Memo.Parent := APanel;
    PlayButton.Enabled := True;
    end;
  end;
end;

procedure TForm1.ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
{the OnPanelDestroy event}
begin
{$ifndef LCL}
  MediaPlayer.Wait := True;
  MediaPlayer.Close;   {in case it's playing}
{$endif}
APanel := Nil;
end;

procedure TForm1.LoadButtonClick(Sender: TObject);
begin
Viewer.LoadFromFile('AviPanel.htm');
end;

procedure TForm1.PlayButtonClick(Sender: TObject);
begin
Memo.Hide;
{$ifndef LCL}
If Assigned(APanel) then
  With MediaPlayer do
    if FileExists(Avi) { *Converted from FileExists*  } then
      begin
      Filename := Avi;
      DeviceType := dtAutoSelect;
      Display := APanel; //Set display device to the Panel
      Open;
      Play;
      end
    else
      begin
      Memo.Text := 'You forgot to get the "speedis.avi" file!';
      Memo.Font.Size := 12;
      Memo.Show;
      end;
{$endif}
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{$ifndef LCL}
MediaPlayer.Wait := True;
MediaPlayer.Close;  
{$endif}
CanClose := True;
end;

initialization
  if (ParamCount >= 1) then
  begin            {Parameter is file to load}
    Avi := CmdLine;
    I := Pos('" ', Avi);
    if I > 0 then
      Delete(Avi, 1, I+1)  {delete EXE name in quotes}
    else
      Delete(Avi, 1, Length(ParamStr(0)));  {in case no quote marks}
    I := Pos('"', Avi);
    while I > 0 do     {remove any quotes from parameter}
    begin
      Delete(Avi, I, 1);
      I := Pos('"', Avi);
    end;
    Avi := HtmlToDos(Trim(Avi));
  end
  else
  begin
    Avi := 'speedis.avi';
  end;
end.
