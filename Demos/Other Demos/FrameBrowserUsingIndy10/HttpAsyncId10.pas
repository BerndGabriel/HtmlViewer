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

Thanks to the Indy Pit Crew for updating *Id9 to *Id10.
}

unit HttpAsyncId10;

{Use thread to make Get Asynchronous }

{$include htmlcons.inc}
{$include options.inc}

interface

uses
  Classes, SysUtils,
  {$ifdef UseSSL} IdSSLOpenSSL, {$endif}
  {$ifdef UseZLib} IdCompressorZLib, {$endif}
  IdHTTP, IdComponent;

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
{$ifdef HasHoNoParseMetaHTTPEquiv}
  HTTP.HTTPOptions := HTTP.HTTPOptions + [hoNoParseMetaHTTPEquiv];
{$endif}
  HTTP.HandleRedirects := True;
  HTTP.ProtocolVersion := pv1_0;

{$ifdef UseSSL}
//  if Pos('https', Lowercase(Url)) > 0 then
//  begin
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    SSL.SSLOptions.Method := sslvSSLv23;
    SSL.SSLOptions.Mode := sslmClient;
    HTTP.IOHandler := SSL;
//  end;
{$endif}
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

procedure THTTPAsync.WorkHandler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Terminated then
  begin
    OnTerminate := Nil;
    HTTP.Disconnect;
  end;
end;

end.

