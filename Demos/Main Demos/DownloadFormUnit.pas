{
Version   11.7
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
unit DownloadFormUnit;

{$include htmlcons.inc}

interface

uses
{$ifdef Compiler24_Plus}
  System.Types,
{$endif}
{$ifdef TScrollStyleInSystemUITypes}
  System.UITypes,
{$endif}
{$ifdef LCL}
  LCLIntf, LCLType, LMessages,
{$else}
  WinTypes, WinProcs,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  URLSubs, UrlConn;

const
  wm_DoIt = wm_User + 111;

type
  TDownloadForm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Status: TLabel;
    TimeLeft: TLabel;
    CancelButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    FStartTick, FUpdatedTick: Cardinal;
    FDownLoad: ThtUrlDoc;
    FConnection: ThtConnection;
    procedure WMDoIt(var Message: TMessage); message WM_DoIt;
    procedure DocData(Sender: TObject);
    procedure DocBegin(Sender: TObject);
  public
    { Public declarations }
    DownLoadURL, Filename, Proxy, ProxyPort, UserAgent: string;
    Connections: ThtConnectionManager;

    destructor Destroy; override;
  end;

//var
//  DownloadForm: TDownloadForm;

implementation

uses
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
  LogFormUnit;

{$R *.dfm}

procedure TDownloadForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, wm_DoIt, 0, 0);
end;

procedure TDownloadForm.WMDoIt(var Message: TMessage);
var
  Msg: String;
begin
  try
    try
      FDownLoad := ThtUrlDoc.Create;
      FDownLoad.Url := DownLoadURL;

      FConnection := Connections.CreateConnection(GetProtocol(DownLoadURL));
      FConnection.OnDocData := DocData;
      FConnection.OnDocBegin := DocBegin;
      FConnection.LoadDoc(FDownLoad);     {download it}
      FDownLoad.SaveToFile(Filename);
    except
      on E: Exception do
      begin
        SetLength(Msg, 0);
        if FConnection <> nil then
          Msg := FConnection.ReasonPhrase;
        if Length(Msg) + Length(E.Message) > 0 then
          Msg := Msg + ' ';
        Msg := Msg + E.Message;
        MessageDlg(Msg, mtError, [mbOK], 0);
      end;
    end;
  finally
    Close;
  end;
end;

procedure TDownloadForm.DocData(Sender: TObject);

  function SizeToStr(Size: Int64): String;
  type
    TBinaryPrefix = (None, Ki, Mi, Gi, Ti, Pi, Ei, Zi, Yi);
  const
    CBinaryPrefix: array[TBinaryPrefix] of String = ('', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi', 'Yi');
  var
    Sized: Int64;
    I: TBinaryPrefix;
  begin
    I := None;
    repeat
      Sized := Size div 1024;
      if Sized < 10 then
        break;
      Inc(I);
      Size := Sized;
    until False;
    Result := IntToStr(Size) + ' ' + CBinaryPrefix[I];
  end;

var
  ReceivedSize: Int64;
  ExpectedSize: Int64;
  Speed: Int64;
  Now: Cardinal;
  Elapsed: Cardinal;
  H, M, S: Integer;
begin
  Now := GetTickCount;
  ReceivedSize := FConnection.ReceivedSize;
  ExpectedSize := FConnection.ExpectedSize;
  if (FUpdatedTick + 100 < Now) or (ReceivedSize = ExpectedSize) then
  begin
    FUpdatedTick := Now;
    Elapsed := Now - FStartTick;
    if Elapsed > 0 then
    begin
      if ReceivedSize < Int64(Elapsed) * 1000 then
        Speed := ReceivedSize * 1000 div Elapsed
      else
        Speed := ReceivedSize div Elapsed * 1000;

      Status.Caption := SizeToStr(ReceivedSize) + 'B of ' + SizeToStr(ExpectedSize) + 'B (at ' + SizeToStr(Speed) + 'B/sec)';

      if (ReceivedSize > 0) and (ExpectedSize > 0) then
      begin
        S := Round((ExpectedSize - ReceivedSize) / 1000 * Elapsed / ReceivedSize);
        H := S div 3600;
        S := S mod 3600;
        M := S div 60;
        S := S mod 60;
        TimeLeft.Caption := Format('%2.2d:%2.2d:%2.2d', [H, M, S]);
        TimeLeft.Update;
      end;
      if LogForm.LogActive[laDiag] then
        LogForm.Log('Downloaded: ' + Status.Caption + ', Time Left: ' + TimeLeft.Caption);
    end;
  end;
end;

procedure TDownloadForm.CancelButtonClick(Sender: TObject);
begin
  FConnection.Abort;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
destructor TDownloadForm.Destroy;
begin
  FDownLoad.Free;
  inherited;
end;

procedure TDownloadForm.DocBegin(Sender: TObject);
begin
  FStartTick := GetTickCount;
end;

end.

