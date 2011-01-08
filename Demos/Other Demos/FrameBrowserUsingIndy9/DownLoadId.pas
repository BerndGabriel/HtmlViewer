unit DownLoadId;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, mmSystem, UrlConId9;

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

{$R *.DFM}

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
Elapsed := LongInt(TimeGetTime)-LongInt(StartTime);
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
StartTime := TimeGetTime;
end;

end.

