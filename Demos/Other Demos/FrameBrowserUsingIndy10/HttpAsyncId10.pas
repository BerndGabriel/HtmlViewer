unit HttpAsyncId10;
{Use thread to make Get Asynchronous }

interface
{$include htmlcons.inc}
{$include options.inc}
uses
  {$ifdef UseZLib}
  IdCompressorZLib,
  {$endif}
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
    SSL: TIdSSLIOHandlerSocketOpenSSL;
    {$endif}
    {$ifdef UseZLib}
    Comp: TIdCompressorZLib;
    {$endif}
  protected
    procedure Execute; override;
    procedure WorkHandler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    Url: string;
    Stream: TMemoryStream;
    HTTP: TIdHTTP;

    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses IdURI;

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
  {$ifdef UseZLib}
  Comp := TIdCompressorZLib.Create;
  HTTP.Compressor := Comp;
  {$endif}
  HTTP.OnWork := WorkHandler;
end;

destructor THTTPAsync.Destroy;
begin
  Stream.Free;
  Stream := Nil;
{$ifdef UseSSL}
  SSL.Free;
{$endif}
  HTTP.Free;
  {$ifdef UseZLib}
  Comp.Free;
  {$endif}
  inherited;
end;

procedure THTTPAsync.Execute;
begin
{$ifdef UseSSL}
  if Pos('https', Lowercase(Url)) > 0 then
  begin
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    SSL.SSLOptions.Method := sslvSSLv23;
    SSL.SSLOptions.Mode := sslmClient;
    HTTP.IOHandler := SSL;
  end;
{$endif}

  try
    //URI's should be encoded as the pathes might contain spaces.
    //Verified at http://www.easyjet.com/en
    HTTP.Get( TIdURI.URLEncode( Url ), Stream);
    if Terminated then
      OnTerminate := Nil;
  except
    HTTP.Disconnect;
  end;
end;

procedure THTTPAsync.WorkHandler(ASender: TObject; AWorkMode: TWorkMode;
   AWorkCount: Int64);
begin
  if Terminated then
  begin
    OnTerminate := Nil;
    HTTP.Disconnect;
  end;
end;

end.

