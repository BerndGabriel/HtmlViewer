unit HttpAsyncId9;
{Use thread to make Get Asynchronous }

interface

uses
  {$ifdef UseSSL}
  Classes, IdHTTP, IdComponent, SysUtils, IdSSLOpenSSL;
  {$else}
  Classes, IdHTTP, IdComponent, SysUtils;
  {$endif}

type
  THTTPAsync = class(TThread)
  private
    { Private declarations }
    {$ifdef UseSSL}
    SSL: TIdSSLIOHandlerSocket;
    {$endif}
  protected
    procedure Execute; override;
    procedure WorkHandler(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
  public
    Url: string;
    Stream: TMemoryStream;
    HTTP: TIdHTTP;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure THTTPAsync.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ THTTPAsync }

constructor THTTPAsync.Create;
begin
inherited Create(True);
FreeOnTerminate := True;
Stream := TMemoryStream.Create;
HTTP := TIdHTTP.Create(Nil);
HTTP.HandleRedirects := True;
HTTP.ProtocolVersion := pv1_0;

//HTTP.OnWork := WorkHandler;
end;

destructor THTTPAsync.Destroy;
begin
Stream.Free;
Stream := Nil;
{$ifdef UseSSL}
SSL.Free;
{$endif}
HTTP.Free;
inherited;
end;

procedure THTTPAsync.Execute;
begin
{$ifdef UseSSL}
if Pos('https', Lowercase(Url)) > 0 then
  begin
  SSL := TIdSSLIOHandlerSocket.Create(Nil);
  SSL.SSLOptions.Method := sslvSSLv23;
  SSL.SSLOptions.Mode := sslmClient;
  HTTP.IOHandler := SSL;
  end;
{$endif}

try
  HTTP.Get(Url, Stream);
  if Terminated then
    OnTerminate := Nil;
except
  HTTP.DisconnectSocket;
  end;
end;

procedure THTTPAsync.WorkHandler(Sender: TObject; AWorkMode: TWorkMode;
              const AWorkCount: Integer);
begin
if Terminated then
  begin
  OnTerminate := Nil;
  HTTP.DisconnectSocket;
  end;
end;

end.

