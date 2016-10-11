{
Version   11.7
Copyright (c) 2016 by HtmlViewer Team

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

Note that the source modules HTMLGIF1.PAS, DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

unit UrlConId10;

{$include htmlcons.inc}
{$include OptionsId10.inc}

interface
{
Inspired by the former UrlConId10.PAS.

Thanks to the Indy Pit Crew for updating the former
UrlConId10.pas/FBUnitId10.pas to Indy 10
from Indy 9 in UrlConId9.pas/FBUnitId9.pas.

UrlConId9.PAS was written as UrlConId.PAS by Yves Urbain.
}
// This is the original copyright notice of UrlConId.PAS:
{*********************************************************}
{*                     UrlConId.PAS                      *}
{*                Copyright (c) 1999 by                  *}
{*                   Metaphor SPRL                       *}
{*                 All rights reserved.                  *}
{*                Written by Yves Urbain                 *}
{*********************************************************}

uses
{$ifdef LCL}
  LCLIntf, LCLType, LMessages,
{$else}
  ShellAPI, WinTypes, WinProcs,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IniFiles,
{$ifdef MSWindows}
  Windows,
{$endif}
  IdHTTP, IdComponent, IdCookie, IdCookieManager, IdAuthentication,
{$ifdef UseSSL}
  IdIntercept, IdSSLOpenSSL,
{$endif}
//{$ifdef LogIt}
//  IdLogFile,
//{$endif}
{$ifdef UseZLib}
  IdCompressorZLib,
{$endif}
  URLSubs, UrlConn, HtmlGlobals;

type
  THTTPConnection = class(ThtConnection)
  private
    FHttp: TidHTTP;
{$ifdef UseSSL}
    FSsl: TIdSSLIOHandlerSocketOpenSSL;
{$endif}
{$ifdef UseZLib}
    FComp: TIdCompressorZLib;
{$endif}
    FUrlBase: string;
    FAllow: ThtString;
    FNewLocation: string;

    FResponseText: string;
    FResponseCode: Integer;
    FRedirect: Boolean;

    FHeaderRequestData  : TStrings;
    FHeaderResponseData : TStrings;
    procedure HttpAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
    procedure HttpRedirect(Sender: TObject; var Dest: string; var NumRedirect: Integer; var Handled: boolean; var Method: TIdHTTPMethod);
    procedure HttpWorkBegin(Sender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure HttpWork(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure HttpWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Get(Doc: ThtUrlDoc); override;
    procedure Abort; override;
    property HeaderRequestData : TStrings               read FHeaderRequestData;
    property HeaderResponseData: TStrings               read FHeaderResponseData;
    property StatusCode        : Integer                read FResponseCode;
  end;

  ThtIndyHttpConnector = class(ThtProxyConnector)
  private
    FUserAgent: string;
    FCookieFile: ThtString;
    FCookieManager: TIdCookieManager;
    function StoreUserAgent: Boolean;
  protected
    class function GetDefaultProtocols: ThtString; override;
    class function GetVersion: string; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadCookies;
    procedure SaveCookies;

    function CreateConnection(const Protocol: ThtString): ThtConnection; override;
  published
    property OnGetAuthorization;
    property UserAgent: string read FUserAgent write FUserAgent stored StoreUserAgent;
    property CookieFile: ThtString read FCookieFile write FCookieFile;
    property CookieManager: TIdCookieManager read FCookieManager;
  end;

implementation

uses
  LogFormUnit,
  IdURI, IdGlobal, IdGlobalProtocols;

const
//  CUserAgent = 'Mozilla/4.0 (compatible; Indy Library)';
  CUserAgent = 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98)';
//  CUserAgent = 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0)';

var
  VChecked: Boolean;
  VCanSSL: Boolean;

//- BG ----------------------------------------------------------- 08.12.2006 --
function CanSSL: Boolean;
var
  h1, h2: THandle;
begin
  if not VChecked then
  begin
    // check to see the DLLs for Secure Socket Layer can be found
{$ifdef UseSSL}
{$ifdef MSWindows}
    {check to see the DLLs for Secure Socket Layer can be found}
    h1 := LoadLibrary('libeay32.dll');
    h2 := LoadLibrary('ssleay32.dll');
    if h2 = 0 then
      {alternative name for mingw32 versions of OpenSSL}
      h2 := LoadLibrary('libssl32.dll');
    FreeLibrary(h1);
    FreeLibrary(h2);

    VCanSSL := (h1 <> 0) and (H2 <> 0);
{$else}
    VCanSSL := True;
{$endif}
{$else}
    VCanSSL := False;
{$endif}
    VChecked := True;
  end;
  Result := VCanSSL;
end;

{ ThtIndyHttpConnector }

//-- BG ---------------------------------------------------------- 18.05.2016 --
constructor ThtIndyHttpConnector.Create(AOwner: TComponent);
begin
  inherited;
  FUserAgent := CUserAgent;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtIndyHttpConnector.CreateConnection(const Protocol: ThtString): ThtConnection;
var
  Connection: THTTPConnection absolute Result;
begin
  if FCookieManager = nil then
    LoadCookies();

  Result := THTTPConnection.Create;
  Connection.FHttp.CookieManager := FCookieManager;
  Connection.FHttp.ProxyParams.ProxyServer := ProxyServer;
  Connection.FHttp.ProxyParams.ProxyPort := StrToIntDef(ProxyPort, 80);
  Connection.FHttp.ProxyParams.ProxyUsername := ProxyUsername;
  Connection.FHttp.ProxyParams.ProxyPassword := ProxyPassword;
  Connection.FHttp.ProxyParams.BasicAuthentication := (ProxyUsername <> '') and (ProxyPassword <> '');     {9.2}
  Connection.FHttp.Request.UserAgent := UserAgent;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
class function ThtIndyHttpConnector.GetDefaultProtocols: ThtString;
begin
  Result := 'http';
  if CanSSL then
    Result := Result + ',https';
end;

class function ThtIndyHttpConnector.GetVersion: string;
begin
  Result := gsIdProductName + ' ' + gsIdVersion;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtIndyHttpConnector.StoreUserAgent: Boolean;
begin
  Result := FUserAgent <> CUserAgent;
end;

{NOTE about Cookies in Indy 10.

The cookie entine was completely rewritten in Indy 10.

The code does not load and save every cookie.  I do not consider this a bug at all
because:

1) Cookies expire
2) Some cookies are session cookies that are meant to only last for your session.
Those cookies do not have expiration timestamps.


}

//-- BG ---------------------------------------------------------- 22.05.2016 --
procedure ThtIndyHttpConnector.LoadCookies;
var
  M : TMemIniFile;

  procedure ReadCookieValues(
    AURI : TIdURI;
    AValues : TStrings);
  var i : Integer;
    s, LC, LA : string;
    LCookie : TIdCookie;
  begin
    for i := 0 to AValues.Count - 1 do
    begin
      s := AValues[i];
      Fetch(s,'=');
      LC := Fetch(s,'|');
      LA := Fetch(s,'|');
      LCookie := FCookieManager.CookieCollection.AddServerCookie(s,AURI);
      if LCookie <> nil then
      begin
        LCookie.CreatedAt := GMTToLocalDateTime(LC);
        LCookie.LastAccessed := GMTToLocalDateTime(LA);
      end;
    end;
  end;

var
  sects : TStringList;
  CookieValues : TStringList;
  i : Integer;
  LU : TIdURI;
begin
  if FCookieManager = nil then
    FCookieManager := TIdCookieManager.Create(Self);

  if FileExists(FCookieFile) then
  begin
    M := TMemIniFile.Create( FCookieFile );
    try
      sects := TStringList.Create;
      try
        CookieValues := TStringList.Create;
        LU := TIdURI.Create;
        try
          m.ReadSections(sects);
          for i := 0 to sects.Count - 1 do
          begin
            m.ReadSectionValues(sects[i], CookieValues);
            LU.URI := sects[i];
            ReadCookieValues(LU, CookieValues);
            CookieValues.Clear;
          end;
        finally
          LU.Free;
          CookieValues.Free;
        end;
      finally
        sects.Free;
      end;
    finally
      m.Free;
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
procedure ThtIndyHttpConnector.SaveCookies;
var
  M: TMemIniFile;
  i: Integer;
  LU: TIdURI;
  LCookie: TIdCookie;
begin
  if FCookieManager <> nil then
    if Length(FCookieFile) > 0 then
    begin
      M := TMemIniFile.Create( FCookieFile );
      try
        M.Clear;
        LU := TIdURI.Create;
        try
          M.Clear;
          for i := 0 to FCookieManager.CookieCollection.Count -1 do
          begin
            LCookie := FCookieManager.CookieCollection[i];
            if LCookie.Persistent then
            begin
              LU.URI := '';
              LU.Path := LCookie.Path;
              LU.Host := LCookie.Domain;
              if LCookie.Secure then
                LU.Protocol := 'https'
              else
                LU.Protocol := 'http';
              M.WriteString( LU.URI, IntToStr(i), LocalDateTimeToGMT( LCookie.CreatedAt ) + '|' + LocalDateTimeToGMT( LCookie.LastAccessed ) + '|' + LCookie.ServerCookie );
            end;
          end;
        finally
          LU.Free;
        end;
        M.UpdateFile;
      finally
        M.Free;
      end;
    end;
end;

{ THTTPConnection }

//-- BG ---------------------------------------------------------- 18.05.2016 --
constructor THTTPConnection.Create;
begin
  inherited Create;
  FHeaderRequestData := TStringList.Create;
  FHeaderResponseData := TStringList.Create;
  FResponseCode := 404;

  FHttp := TIdHTTP.Create(nil);
  FHttp.ProtocolVersion := pv1_1;
  FHttp.HandleRedirects := True;
  FHttp.OnAuthorization := HttpAuthorization;
  FHttp.OnRedirect := HttpRedirect;
  FHttp.OnWorkBegin := HttpWorkBegin;
  FHttp.OnWork := HttpWork;
  FHttp.OnWorkEnd := HttpWorkEnd;
{$ifdef UseSSL}
  if CanSSL then
  begin
    FSsl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    FSsl.SSLOptions.Method := sslvSSLv23;
    FSsl.SSLOptions.Mode := sslmClient;
    FHttp.IOHandler := FSsl;
  end;
{$endif}
{$ifdef UseZLib}
  FComp:= TIdCompressorZLib.Create(nil);
  FHttp.Compressor := FComp;
{$endif}
//{$ifdef LogIt}
//  FHttp.Intercept := HTTPForm.Log;
//{$endif}
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
destructor THTTPConnection.Destroy;
begin
  FHttp.Free;
{$ifdef UseZLib}
  FComp.Free;
{$endif}
{$ifdef UseSSL}
  FSsl.Free;
{$endif}
  FHeaderRequestData.Free;
  FHeaderResponseData.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPConnection.Get(Doc: ThtUrlDoc);
const
  MaxRedirect = 15;
var
  PostIt1, TryAgain, TryRealm, Redirect: Boolean;
  SendStream: TStringStream;
  Strings: TStringList;
  RedirectCount: Integer;
  Url1, Query1, S: ThtString;
begin
  FHttp.Request.Referer := Doc.Referer;
  Url1 := Doc.Url;
  Query1 := Doc.Query;
  PostIt1 := Doc.PostIt;
  FUrlBase := GetUrlBase(URL1);
  RedirectCount := -1;
  repeat
    TryRealm := True;
    TryAgain := False;
    Redirect := False;
    Inc(RedirectCount);
    Doc.Clear;
    try
      Aborted := False;
      FHttp.Request.BasicAuthentication := BasicAuthentication;
      FHttp.Request.Username := Username;
      FHttp.Request.Password := Password;
      try
        if PostIt1 then
        begin {Post}
          SendStream := TStringStream.Create(Query1 {$ifdef UNICODE}, TEncoding.ANSI {$endif});
          try
            if Length(Doc.QueryEncType) > 0 then
              FHttp.Request.ContentType := Doc.QueryEncType
            else
              FHttp.Request.ContentType := 'application/x-www-form-urlencoded';
            if LogForm.LogActive[laDiag] then
              LogForm.LogSynced('[' + IntToStr(SessionId) + '] THTTPConnection.Get Post: ' + Url1 + ', Data=' + Copy(Query1, 1, 132) + ', EncType=' + Doc.QueryEncType);  // not too much data
            FHttp.Post(URL1, SendStream, Doc.Stream);
          finally
            SendStream.Free;
          end;
        end
        else
        begin {Get}
          if Length(Query1) > 0 then
            Url1 := Url1 + '?' + Query1;
          if LogForm.LogActive[laDiag] then
            LogForm.LogSynced('[' + IntToStr(SessionId) + '] THTTPConnection.Get Get: ' + Url1);
          FHttp.Get(Url1, Doc.Stream);
        end;
      finally
        Doc.DocType := ContentType2DocType(FHttp.Response.ContentType);
        ReceivedSize := FHttp.Response.ContentLength;
        FResponseText := FHttp.ResponseText;
        FResponseCode := FHttp.ResponseCode;
        if LogForm.LogActive[laHttpHeader] then
        begin
          Strings := TStringList.Create;
          try
            Strings.Add( '[' + IntToStr(SessionId) + ']--- Http.Request: ''' + Url1 + ''' ---' );
            Strings.AddStrings( FHttp.Request.RawHeaders );
            Strings.Add( '[' + IntToStr(SessionId) + ']--- Http.Response ---' );
            Strings.AddStrings( FHttp.Response.RawHeaders );
            Strings.Add( '[' + IntToStr(SessionId) + ']Character Set = '+ FHttp.Response.CharSet );
            Strings.Add( '[' + IntToStr(SessionId) + ']---------------------' );
            LogForm.LogSynced(Strings);
          finally
            FreeAndNil(Strings);
          end;
        end;
      end;

      if FResponseCode = 401 then
      begin
        TryAgain := GetAuthorization(TryRealm);
        TryRealm := False;
      end
      else
        if Redirect then
        begin
          Url1 := FNewLocation;
          if FResponseCode = 303 then
          begin
            PostIt1 := False;
            Query1 := '';
          end;
          TryAgain := True;

        if {$ifdef UseSSL} not Assigned(FSsl) and {$endif} (GetProtocol(FNewLocation) = 'https') then
        begin
          TryAgain := False;
          S := '<p>Unsupported protocol: https';
          Doc.Stream.Position := Doc.Stream.Size;
          Doc.Stream.Write(S[1], Length(S) * SizeOf(S[1]));
        end;

      end;
    except
      case FResponseCode of
        401:
          begin
            TryAgain := GetAuthorization(TryRealm);
            TryRealm := False;
          end;

        405:
          if PostIt1 and (Pos('get', FAllow) > 0) then
          begin
            PostIt1 := False;
            TryAgain := True;
          end;

        301, 302:
          if FNewLocation <> '' then
          begin
            URL1 := FNewLocation;
            TryAgain := True;
          end;
      end;
      if not TryAgain or (RedirectCount >= MaxRedirect) then
        Raise;
    end;
    if LogForm.LogActive[laDiag] then
      LogForm.LogSynced('[' + IntToStr(SessionId) + ']THTTPConnection.Get Done: Status ' + IntToStr(StatusCode) + ': ''' + Url1 + '''');
  until not TryAgain;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPConnection.HttpRedirect(Sender: TObject; var Dest: string;
  var NumRedirect: Integer; var Handled: boolean; var Method: TIdHTTPMethod);
var
  OldProtocol: string;
  FullUrl: boolean;
begin
  FullUrl := IsFullUrl(Dest);
  if FullUrl then  {it's a full URL}
  begin
    Dest := Normalize(Dest);
    FNewLocation := Dest;
  end
  else
  begin
    FNewLocation := CombineURL(FUrlBase, Dest);
    FullURL := False;
  end;

  OldProtocol := GetProtocol(FUrlBase);
  FUrlBase := GetUrlBase(FNewLocation);

{The following is apparently no longer necessary}
  Handled := FullURL and (OldProtocol <> 'https') and (GetProtocol(FUrlBase) <> 'https');
  if not Handled then
  begin
    {we will handle it}
    FRedirect := True;
  end;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPConnection.HttpAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
begin
  if Authentication is TIdBasicAuthentication then
    Realm := TIdBasicAuthentication(Authentication).Realm;
  Handled := False;
end;

{----------------THTTPConnection.Abort}
procedure THTTPConnection.Abort;
begin
  inherited;
  if Assigned(FHttp) then
    FHttp.Disconnect;
end;

{----------------THTTPConnection.WorkBegin}
procedure THTTPConnection.HttpWorkBegin(Sender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
var
//  LocationFound: boolean;
  S: string;
begin
  Doc.Status := ucsInProgress;
  ExpectedSize := AWorkCountMax;
  ReceivedSize := 0;

//  LocationFound := FHttp.Response.Location <> '';
//  if LocationFound then
//    HeaderData('Location: '+ FHttp.Response.Location)
//  else if FHttp.Response.ContentType <> '' then
//    {Content type is unimportant on Location change}
//    HeaderData('content-type: '+ FHttp.Response.ContentType);

  S := FHttp.Response.RawHeaders.Values['Allow'];
  if S <> '' then
    FAllow := S;

  if Assigned(OnDocBegin) then
    OnDocBegin(Self);
end;

procedure THTTPConnection.HttpWork(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  ReceivedSize := AWorkCount;
  if Assigned(OnDocData) then
    OnDocData(Self);
end;

procedure THTTPConnection.HttpWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  Doc.Status := ucsLoaded;
  if Assigned(OnDocEnd) then
    OnDocEnd(Self);
end;

end.
