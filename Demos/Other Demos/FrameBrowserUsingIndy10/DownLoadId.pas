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

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.

Special thanks go to the Indy Pit Crew that updated *Id9 to *Id10.
}
unit DownLoadId;

{$include htmlcons.inc}
{$include options.inc}

interface

uses
{$ifdef Compiler24_Plus}
  System.Types,
{$endif}
 {$ifdef TScrollStyleInSystemUITypes}
  System.UITypes,
{$endif}
{$IFnDEF FPC}
  WinTypes, WinProcs,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, mmSystem, UrlConId10;

const
  wm_DoIt = wm_User + 111;

type
  TDownLoadForm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Status: TLabel;
    TimeLeft: TLabel;
    CancelButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    BytesRead: LongInt;
    StartTime: LongInt;
    FileSize: LongInt;
    Connection: TURLConnection;
    procedure WMDoIt(var Message: TMessage); message WM_DoIt;
    procedure DocData(Sender: TObject);
    procedure DocBegin(Sender: TObject);
  public
    { Public declarations }
    DownLoadURL, Filename, Proxy, ProxyPort, UserAgent: string;
  end;

var
  DownLoadForm: TDownLoadForm;

implementation
{$ifdef HasSystemUITypes}
uses System.UITypes;
{$endif}

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TDownLoadForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, wm_DoIt, 0, 0);
end;

procedure TDownLoadForm.WMDoIt(var Message: TMessage);
begin
  BytesRead := 0;
  try
    Connection := TURLConnection.GetConnection(DownLoadURL);
    if Assigned(Connection) then
    try
      try
        Connection.Proxy := Proxy;
        Connection.ProxyPort := ProxyPort;
        Connection.UserAgent := UserAgent;
        Connection.OnDocData := DocData;
        Connection.OnDocBegin := DocBegin;
        Connection.Get(DownLoadURL);     {download it}
        Connection.InputStream.SaveToFile(Filename);
      except
        MessageDlg(Connection.ReasonPhrase, mtError, [mbOK], 0);
      end;
    finally
      Connection.Free;
    end;
  finally
    Close;
  end;
end;

procedure TDownLoadForm.DocData(Sender: TObject);
var
  hr, min, sec, rem: integer;
  KBytesPerSec: Double;
  Elapsed: LongInt;
begin
  BytesRead := Connection.RcvdCount;
  Elapsed := LongInt(GetTickCount { *Konvertiert von TimeGetTime* })-LongInt(StartTime);
  if Elapsed > 0 then
  begin
    KBytesPerSec := BytesRead/Elapsed;
    Rem := ((FileSize - BytesRead) * (Elapsed div 1000)) div BytesRead;
    Hr := Rem div 3600;
    Rem := Rem mod 3600;
    Min := Rem div 60;
    Sec := Rem mod 60;

    TimeLeft.Caption := Format('%2.2d:%2.2d:%2.2d', [Hr, Min, Sec]);
    Status.Caption := Format('%dk of %dk (at %4.1fk/sec)',
                     [BytesRead div 1024, FileSize div 1024, KBytesPerSec]);
    TimeLeft.Update;
  end;
end;

procedure TDownLoadForm.CancelButtonClick(Sender: TObject);
begin
  Connection.Abort;
end;

procedure TDownLoadForm.DocBegin(Sender: TObject);
begin
  Filesize := Connection.ContentLength;
  StartTime := GetTickCount; { *Konvertiert von TimeGetTime* }
end;

end.

