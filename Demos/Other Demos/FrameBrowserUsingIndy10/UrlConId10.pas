{*********************************************************}
{*                     UrlConId.PAS                      *}
{*                Copyright (c) 1999 by                  *}
{*                   Metaphor SPRL                       *}
{*                 All rights reserved.                  *}
{*                Written by Yves Urbain                 *}
{*********************************************************}

unit UrlConId10;

{$include htmlcons.inc}
{$include options.inc}

interface

{*********************************************************}
{*                                                       *}
{* This module contains a base class TURLConnection      *}
{* that defines that behaviour of connection to a Web    *}
{* resource: a HTML page, an image, ...                  *}
{* This base classes contains a class method that creates*}
{* the connection object that will handle connection to  *}
{* a resource described by a specific protocol specified *}
{  in an URL                                             *}
{* this method is 'getConnection'                        *}
{*                                                       *}
{* The implemented protocol are now :                    *}
{*    - http managed by THTTPConnection                  *}
{*    - file managed by TFileConnection                  *}
{*      e.g file://d:/myprojects/demo.htm                *}
{*    - res managed by TResConnection                    *}
{*      The HTLM pages are stored in the application     *}
{*      resources.                                       *}
{*      e.g res:///Welcome.html                          *}
{*    - zip managed by TZipConnection. Through the use   *}
{       of VCLZip (works also in Delphi 1) the HTML pages*}
{*      are extracted from a zip file                    *}
{*      e.g. zip://demo.zip/demo.htm URL will extract    *}
{*      demo.htm from the demo.zip (stored in the current*}
{*      Define IncludeZip to enable the use of           *)
{*      TZipConnection                                   *}
{*                                                       *}
{* - version 1.0d1: first version                        *}
{*********************************************************}

uses
{$ifdef LCL}
  LCLIntf, LCLType, LMessages,
{$else}
  ShellAPI, WinTypes, WinProcs,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$ifdef MSWindows}
  Windows,
{$endif}
  IdHTTP, IdComponent, IdCookieManager, IdAuthentication,
{$ifdef UseSSL}
  IdIntercept, IdSSLOpenSSL,
{$endif}
{$ifdef LogIt}
  IdLogFile,
{$endif}
{$ifdef UseZLib}
  IdCompressorZLib,
{$endif}
//{$ifdef IncludeZip}
//  VCLUnZIp, kpZipObj,
//{$endif}
  URLSubs, UrlConn;

type
  ThtIndyHttpConnector = class;

  THTTPConnection = class(ThtConnection)
  private
    ReturnedContentType: string;
    FResponseText: string;
    FRedirect: Boolean;
    FNewLocation: string;
    FUrlBase: string;
    FAllow: String;

    FConnector: ThtIndyHttpConnector;
    FHeaderRequestData  : TStrings;
    FHeaderResponseData : TStrings;
    FCookieManager     : TIdCookieManager;
    FStatusCode        : Integer;
    FHttp: TidHTTP;
{$ifdef UseSSL}
    FSsl: TIdSSLIOHandlerSocketOpenSSL;
{$endif}
{$ifdef UseZLib}
    FComp: TIdCompressorZLib;
{$endif}
    procedure HeaderData(const LastResponse: string);
    procedure HttpAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
    procedure HttpRedirect(Sender: TObject; var Dest: String; var NumRedirect: Integer; var Handled: boolean; var Method: TIdHTTPMethod);
    procedure HttpWorkBegin(Sender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure HttpWork(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure HttpWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
  public
    constructor Create(Connector: ThtIndyHttpConnector);
    destructor Destroy; override;

    procedure Get(Doc: ThtUrlDoc); override;
    procedure Abort; override;
    property CookieManager     : TIdCookieManager       read FCookieManager      write FCookieManager;
    property HeaderRequestData : TStrings               read FHeaderRequestData;
    property HeaderResponseData: TStrings               read FHeaderResponseData;
    property StatusCode        : Integer                read FStatusCode;
  end;

  ThtIndyHttpConnector = class(ThtProxyConnector)
  private
    FUserAgent         : string;
    FCookieManager: TIdCookieManager;
    function StoreUserAgent: Boolean;
  protected
    class function GetDefaultProtocols: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateConnection(const Protocol: String): ThtConnection; override;
  published
    property UserAgent: string read FUserAgent write FUserAgent stored StoreUserAgent;
    property CookieManager: TIdCookieManager read FCookieManager;
  end;

//{$ifdef IncludeZip}
//  TZipConnection = class(TURLConnectionId)
//  private
//    UnZipper: TVCLUnzip;
//  public
//    destructor Destroy; override;
//    procedure Get(const URl: String); override;
//  end;
//{$endif}

implementation

uses
{$ifdef LogIt} LogWin, FBUnitId10, {$endif}
  HTMLUn2,
  IdURI, IdGlobal;

const
//  CUsrAgent = 'Mozilla/4.0 (compatible; Indy Library)';
  CUsrAgent = 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98)';
//  CUsrAgent = 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0)';

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
  FCookieManager := TIdCookieManager.Create(Self);
  FUserAgent := CUsrAgent;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtIndyHttpConnector.CreateConnection(const Protocol: String): ThtConnection;
var
  Connection: THTTPConnection;
begin
  Connection := THTTPConnection.Create(Self);
{$ifdef UseSSL}
  if CanSSL then
  begin
    Connection.FSsl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    Connection.FSsl.SSLOptions.Method := sslvSSLv23;
    Connection.FSsl.SSLOptions.Mode := sslmClient;
  end;
{$endif}
  Result := Connection;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
destructor ThtIndyHttpConnector.Destroy;
begin

  inherited;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
class function ThtIndyHttpConnector.GetDefaultProtocols: string;
begin
  Result := 'http';
  if CanSSL then
    Result := Result + ',https';
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtIndyHttpConnector.StoreUserAgent: Boolean;
begin
  Result := FUserAgent <> CUsrAgent;
end;

{ THTTPConnection }

//-- BG ---------------------------------------------------------- 18.05.2016 --
constructor THTTPConnection.Create(Connector: ThtIndyHttpConnector);
begin
  inherited Create;
  FConnector := Connector;
  FHeaderRequestData := TStringList.Create;
  FHeaderResponseData := TStringList.Create;
  FStatusCode := 404;
{$ifdef UseZLib}
  FComp:= TIdCompressorZLib.Create(nil);
{$endif}
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
destructor THTTPConnection.Destroy;
begin
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
procedure THTTPConnection.HeaderData(const LastResponse: string);
{Selected Header data comes here}
var
  S: string;
  I: integer;
begin
  S := LastResponse;

{see if Authentication is required.  If so, need the Realm string}
  I := Pos('www-authenticate', Lowercase(S));
  if I > 0 then
  begin
    I := Pos('realm', Lowercase(S));
    if (I > 0) then
    begin
      S := Copy(S, I+5, Length(S));
      I := Pos('=', S);
      if I > 0 then
      begin
        S := Trim(Copy(S, I+1, Length(S)));
        Realm := S;
      end;
    end;
    Exit;
  end;

  {see what's allowed for error 405}
  I := Pos('allow:', LowerCase(S));
  if (I > 0) then
  begin
    FAllow := Lowercase(S);
    Exit;
  end;

end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPConnection.Get(Doc: ThtUrlDoc);
const
  MaxRedirect = 15;
var
  PostIt1, TryAgain, TryRealm, Redirect: Boolean;
  SendStream: TStringStream;
  RedirectCount: Integer;
  Url1, Query1, S: String;
begin
  Url1 := Doc.Url;
  FUrlBase := GetUrlBase(URL1);
  Query1 := Doc.Query;
  PostIt1 := Doc.PostIt;
  repeat
    TryRealm := True;
    TryAgain := False;
    Redirect := False;
    Inc(RedirectCount);
    Doc.Clear;
    try
      Aborted := False;
      FHttp := TIdHTTP.Create(Nil);
{$ifdef UseZLib}
      FHttp.Compressor := FComp;
{$endif}
{$ifdef LogIt}
      FHttp.Intercept := HTTPForm.Log;
{$endif}
      FHttp.CookieManager := FCookieManager;
      FHttp.HandleRedirects := True;
      FHttp.OnAuthorization := HttpAuthorization;
      FHttp.OnRedirect := HttpRedirect;
      FHttp.OnWorkBegin := HttpWorkBegin;
      FHttp.OnWork := HttpWork;
      FHttp.OnWorkEnd := HttpWorkEnd;
      FHttp.ProtocolVersion := pv1_1;
      FHttp.ProxyParams.ProxyServer := FConnector.ProxyServer;
      FHttp.ProxyParams.ProxyPort := StrToIntDef(FConnector.ProxyPort, 80);
      FHttp.ProxyParams.ProxyUsername := FConnector.ProxyUsername;
      FHttp.ProxyParams.ProxyPassword := FConnector.ProxyPassword;
      FHttp.ProxyParams.BasicAuthentication := (FConnector.ProxyUsername <> '') and (FConnector.ProxyPassword <> '');     {9.2}
      FHttp.Request.UserAgent := FConnector.UserAgent;
      FHttp.Request.BasicAuthentication := BasicAuthentication;
      FHttp.Request.Referer := Doc.Referer;
      FHttp.Request.Username := Username;
      FHttp.Request.Password := Password;
{$ifdef UseSSL}
      FHttp.IOHandler := FSsl;
{$endif}
      try
        if PostIt1 then
        begin {Post}
          SendStream := TStringStream.Create(Query1);
          try
{$ifdef LogIt}
            HTTPForm.LogLine('THTTPConnection.Get Post: ' + Url1 + ', Data=' + Copy(Query1, 1, 132) + ', EncType=' + Doc.QueryEncType);  // not too much data
{$endif}
            if Doc.QueryEncType = '' then
              FHttp.Request.ContentType := 'application/x-www-form-urlencoded'
            else
              FHttp.Request.ContentType := Doc.QueryEncType;
            FHttp.Post(TIdURI.URLEncode( URL1 ), SendStream, Doc.Stream);
          finally
            SendStream.Free;
          end;
        end
        else
        begin {Get}
          if Length(Query1) > 0 then
            Url1 := Url1 + '?' + Query1;
{$ifdef LogIt}
          HTTPForm.LogLine('THTTPConnection.Get Get: ' + Url1);
{$endif}
          FHttp.Get( TIdURI.URLEncode( Url1 ), Doc.Stream );
        end;
      finally
        ReturnedContentType := FHttp.Response.ContentType;
        ReceivedSize := FHttp.Response.ContentLength;
        FResponseText := FHttp.ResponseText;
        FStatusCode := FHttp.ResponseCode;
        FHeaderRequestData.AddStrings( FHttp.Request.RawHeaders );
        FHeaderResponseData.AddStrings( FHttp.Response.RawHeaders );
        FHeaderResponseData.Add( 'Character Set = '+ FHttp.Response.CharSet );
{$ifdef UseSSL}
        FHttp.IOHandler := Nil;
{$endif}
        FreeAndNil(FHttp);
      end;

      if FStatusCode = 401 then
      begin
        TryAgain := GetAuthorization(TryRealm);
        TryRealm := False;
      end
      else
        if Redirect then
        begin
          Url1 := FNewLocation;
          if FStatusCode = 303 then
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
      case FStatusCode of
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
{$ifdef LogIt}
    HTTPForm.LogLine('THTTPConnection.Get Done: Status ' + IntToStr(StatusCode));
{$endif}
  until not TryAgain;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPConnection.HttpRedirect(Sender: TObject; var Dest: String;
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
  LocationFound: boolean;
  S: string;
begin
  Doc.Status := ucsInProgress;
  ExpectedSize := AWorkCountMax;
  ReceivedSize := 0;

  LocationFound := FHttp.Response.Location <> '';
  if LocationFound then
    HeaderData('Location: '+ FHttp.Response.Location)
  else if FHttp.Response.ContentType <> '' then
    {Content type is unimportant on Location change}
    HeaderData('content-type: '+ FHttp.Response.ContentType);

  S := FHttp.Response.RawHeaders.Values['Allow'];
  if S <> '' then
    HeaderData('Allow: ' + S);

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

//{$ifdef IncludeZip}
//
//type
//  EZipFileError = class(Exception);
//
//{----------------TZipConnection.Destroy}
//destructor TZipConnection.Destroy;
//begin
//  If unzipper <> nil then
//     Unzipper.free;
//  inherited;
//end;
//
//procedure TZipConnection.Get(const URl: String);
//var
//   num, error, I : integer;
//   TheFile, Host, Ext : String;
//begin
//   { Syntax: zip://zipname/filetoextract
//   { The full path is needed, as:  zip://c:\dir1\subdir\htmlfiles.zip/demo.htm
//     or zip://c|/dir1/subdir/htmlfiles.zip/demo.htm }
//   error := 1;
//   try
//         if Unzipper = nil then
//            Unzipper := TVCLUnzip.Create(nil);
//         thefile := URL;
//
//         {remove any query string as it's not certain how to respond to a Form
//          submit with a zip: protocol.  The user can add a response if desired.}
//         I := Pos('?', TheFile);
//         if I > 0 then
//           TheFile := Copy(TheFile, 1, I-1);
//
//         TheFile := GetURLFilenameAndExt(URL);
//         Host := GetBase(URL);
//         Delete(Host, 1, 6);     {remove zip://}
//         Delete(Host, Length(Host), 1);  {remove trailing '/'}
//         Host := HTMLToDos(Host);
//
//         CheckInputStream;
//         InputStream.Clear;  {apparently req'd for unzip routines}
//
//         Ext := Uppercase(GetURLExtension(URL));
//         FContentType := HTMLType;
//         if (Ext = 'GIF') or (Ext = 'JPG') or (Ext = 'JPEG') or (Ext = 'JFIF') or (Ext = 'JPE')
//            or (Ext = 'PNG') or (Ext = 'BMP') or (Ext = 'RLE') or (Ext = 'DIB')
//    {$IFNDEF NoMetafile}
//           or (Ext = 'EMF') or (Ext = 'WMF')
//    {$ENDIF !NoMetafile}
//              {$IFNDEF NoGDIPlus}
//            or (Ext = 'TIF') or (Ext = 'TIFF')
//              {$ENDIF NoGDIPlus}
//         then
//            FContentType := ImgType
//         else
//            if (Ext = 'TXT') then
//              FContentType := TextType;
//
//         With Unzipper do
//         begin
//            if host <> ZipName Then
//               ZipName := host;              { set the zip filename}
//            try
//              { Extract files, return value is the number of files actually unzipped}
//              num := UnZipToStream( InputStream, TheFile );
//            except
//              raise EZipFileError.Create('Can''t open: '+URL);
//            end;
//            if num <> 1 then
//                raise EZipFileError.Create('Can''t open: '+URL);
//         end;
//         error := 0;
//         FStatusCode := 200;
//   finally
//         if Assigned(FOnRequestDone) then
//            FOnRequestDone(owner, httpGET, error);
//   end;
//end;
//{$endif}

end.
