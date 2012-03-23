
{To include the Zip: protocol, define "IncludeZip" by removing the "..."}
{...$Define IncludeZip}

{*********************************************************}
{*                     UrlConId.PAS                      *}
{*                Copyright (c) 1999 by                  *}
{*                   Metaphor SPRL                       *}
{*                 All rights reserved.                  *}
{*                Written by Yves Urbain                 *}
{*********************************************************}

 { Copyright (c) 2012 by Angus Robertson delphi@magsys.co.uk }

unit UrlConIcs;

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
{*                                                       *}
{* March 2012 - Angus Robertson replaced Indy with ICSv7 *}
{*      Suspect this program originally used ICS since   *}
{*      many types, vars and events have ICS names       *}
{*                                                       *}
{*                                                       *}
{*                                                       *}
{*********************************************************}

uses   WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls,
       Forms, Dialogs, ShellAPI, URLSubs, htmlview,
       OverbyteIcsWndControl, OverbyteIcsWsocket,
       OverbyteIcsHttpProt, OverbyteIcsHttpCCodzlib
{$ifdef IncludeZip}
       , VCLUnZIp, kpZipObj
{$endif}
       ;
type
//    THttpRequest     = (httpAbort, httpGET, httpPOST, httpHEAD);  use ICS version
//    THttpState       = (httpReady,         httpNotConnected, httpConnected,
//                        httpDnsLookup,     httpDnsLookupDone,
//                        httpWaitingHeader, httpWaitingBody,  httpAborting); // use ICS version
//    THttpRequestDone = procedure (Sender : TObject;
//                                  RqType : THttpRequest;
//                                  Error  : Word) of object;       // use ICS version

  TURLConnection = class(TObject)
  private
    FInputStream : TMemoryStream;
    FInputStreamOwn : Boolean; { true if the class ownes the stream }
    procedure SetInputStream(Value: TMemoryStream);
  protected
    FOnDocBegin       : TNotifyEvent;
    FOnDocData        : TDocDataEvent;
    FOnRequestDone    : THttpRequestDone;
    FOnRedirect       : TNotifyEvent;
    FContentType      : ThtmlFileType;
    FContentLength    : LongInt;
    FProxy            : string;
    FProxyPort        : string;
    FProxyUser        : string;
    FProxyPassword    : string;
    FUsername         : string;
    FPassword         : string;
    FUserAgent        : string;
    FBasicAuth: boolean;
    FContentTypePost   : String;
    FSendStream : TMemoryStream;
    FOwner : TComponent;
    FReasonPhrase : String;
    FStatusCode  : integer;
    FReferer: string;
    FRealm            : string ;           // ANGUS
    FCookie           : string;            // ANGUS
    FOnCookie         : TCookieRcvdEvent;  // ANGUS
  public
    constructor Create;
    destructor Destroy; override;
    procedure Get(const URL: String); virtual; abstract;
    procedure Post(const URL: String); virtual;
    procedure GetAsync(const URL: String); virtual;
    procedure PostAsync(const URL: String); virtual;
    procedure CheckInputStream; virtual;
    function RcvdCount : LongInt; virtual;
    function ReasonPhrase : String; virtual;
    function LastResponse : String; virtual;
    function StatusCode : LongInt; virtual;
    function State: THttpState; virtual;
    procedure Abort; virtual;
    function ContentType: ThtmlFileType; virtual;
    function ContentLength: LongInt; virtual;
    class function Getconnection(const URL : String) : TURLConnection;
    property OnDocBegin      : TNotifyEvent     read  FOnDocBegin
                                                write FOnDocBegin;
    property OnDocData       : TDocDataEvent    read  FOnDocData
                                                write FOnDocData;
    property OnRequestDone   : THttpRequestDone read  FOnRequestDone
                                                write FOnRequestDone;
    property Owner           : TComponent       read  FOwner
                                                write FOwner;
    property InputStream     : TMemoryStream    read  FInputStream
                                                write SetInputStream;
    property Proxy           : string           read  FProxy
                                                write FProxy;
    property ProxyPort       : string           read  FProxyPort
                                                write FProxyPort;
    property ProxyUser       : string           read  FProxyUser
                                                write FProxyUser;
    property ProxyPassword   : string           read  FProxyPassword
                                                write FProxyPassword;
    property Username        : string           read  FUsername
                                                write FUsername;
    property Password        : string           read  FPassword
                                                write FPassword;
    property UserAgent       : string           read  FUserAgent
                                                write FUserAgent;
    property OnRedirect: TNotifyEvent           read  FOnRedirect
                                                write FOnRedirect;
    property Referer         : string           read FReferer
                                                write FReferer;
    property Realm           : string           read  FRealm;      // ANGUS
    property Cookie          : string           read FCookie
                                                write FCookie;     // ANGUS
    property OnCookie        : TCookieRcvdEvent read FOnCookie
                                                write FOnCookie;   // ANGUS
    property BasicAuth: boolean read FBasicAuth write FBasicAuth;
    property ContentTypePost: String read FContentTypePost write FContentTypePost;
    property SendStream: TMemoryStream read FSendStream write FSendStream;
  end;

  THTTPConnection = class(TURLConnection)
  private
    ReturnedContentType: string;
    FResponseText: string;
    FResponseCode: integer;
//  FRcvdCount: integer;
    FState: ThttpState;
    FAborted: boolean;
    FWinsockLoaded: Boolean;
//  FLastResponse : String;
    procedure GetPostInit1;
    procedure GetPostFinal;
  protected
    HTTP: TSslHttpCli;
    procedure IcsStateChange(Sender: TObject);
    procedure IcsRequestDone (Sender : TObject; RqType : THttpRequest; Error  : Word);
    procedure IcsHttpHeaderData (Sender : TObject);
    procedure IcsHttpCommand (Sender : TObject; var S: String) ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Get(const URl: String); override;
    procedure GetAsync(const URl: String); override;
    procedure Post(const URl: String); override;
    function RcvdCount : LongInt; override;
    function ReasonPhrase : String; override;
    function LastResponse : String; override;
    function StatusCode : LongInt; override;
    function State: THttpState; override;
    function ContentType: ThtmlFileType; override;
    function ContentLength: LongInt; override;
    procedure Abort; override;
  end;

  TFileConnection = class(TURLConnection)
  public
    procedure Get(const URl: String); override;
  end;

  TResourceConnection = class(TURLConnection)
  public
    procedure Get(const URl: String); override;
  end;

{$ifdef IncludeZip}
  TZipConnection = class(TURLConnection)
  private
    UnZipper: TVCLUnzip;
  public
    destructor Destroy; override;
    procedure Get(const URl: String); override;
  end;
{$endif}

type
  TProcolHandlerFunc = function(const URL : String) : TUrlConnection;

var
  ProtocolHandler  : TProcolHandlerFunc;

implementation

uses
  htmlun2 , FBUnitIcs;

constructor TURLConnection.Create;
begin
inherited Create;
     FInputStream := nil;
     SendStream := nil;
     Owner := nil;
     FStatusCode := 404;
     FReasonPhrase := 'Can''t get file';
     FInputStreamOwn := false;
     FContentType := HTMLType;
end;

destructor TURLConnection.Destroy;
begin
     If FInputStreamOwn Then
        FInputStream.Free;
inherited Destroy;
end;

function TURLConnection.State: THttpState;
begin
     result := HTTPReady;
end;

function TURLConnection.RcvdCount : LongInt;
begin
     result := 0;
end;

function TURLConnection.LastResponse : String;
begin
     result := '';
end;

function TURLConnection.ReasonPhrase : String;
begin
     result := FReasonPhrase;
end;

function TURLConnection.StatusCode : LongInt;
begin
     StatusCode := FStatusCode;
end;

function TURLConnection.ContentType: ThtmlFileType;
begin
Result := FContentType;
end;

function TURLConnection.ContentLength: LongInt;
begin
Result := FContentLength;
end;

procedure TURLConnection.Abort;
begin
end;

procedure TURLConnection.CheckInputStream;
begin
  if FInputStream = nil then
  begin
       FInputStream := TMemoryStream.Create;
       FInputStreamOwn := true;
  end;
end;

class function TURLConnection.GetConnection(const URL : String) : TURLConnection;
var
   protocol : String;
begin
     result := nil;
     {If ProtocolHandler is defined call it else}
     If Assigned(ProtocolHandler) Then
        result := ProtocolHandler(URL);

     { Use default handlers }
     if result = nil then
     begin
         protocol := GetProtocol(URL);
         if (protocol = 'http') or (protocol = 'https') then
            result := THTTPConnection.Create
         else if protocol = 'file' then
            result := TFileConnection.Create
         else if protocol = 'res' then
            result := TResourceConnection.Create
{$ifdef IncludeZip}
         else if protocol = 'zip' then
            result := TZipConnection.Create
{$endif}
     end;
end;

procedure TURLConnection.Post(const URL : String);
begin
     Get(URL);
end;

procedure TURLConnection.GetAsync(const URL : String);
begin
   try
     Get(URL);
   { catch exception in order to let HTMLView perform correctly its DoLogic }
   except
   end;
end;

procedure TURLConnection.PostAsync(const URL : String);
begin
     Post(URL);
end;

procedure TURLConnection.SetInputStream(Value: TMemoryStream);
begin
if Assigned(FInputStream) and FInputStreamOwn then
  FInputStream.Free;
FInputStream := Value;
FInputStreamOwn := False;
end;

{----------------THTTPConnection.Create}
constructor THTTPConnection.Create;
begin
inherited Create;
end;

{----------------THTTPConnection.Destroy}
destructor THTTPConnection.Destroy;
begin
  if Assigned (HTTP) then begin
    HTTP.Free;
    HTTP := Nil;
  end;
inherited Destroy;
end;

procedure THTTPConnection.GetPostInit1;
{common initiation for Get, Post}
begin
  if not FWinsockLoaded then
  begin
    WSocketForceLoadWinsock;
    FWinsockLoaded := TRUE;
  end;
FAborted := False;
CheckInputStream;
if NOT Assigned (HTTP) then
    HTTP := TSslHttpCli.Create(Nil);
HTTP.SslContext := HTTPForm.SslContext ;
HTTP.FollowRelocation := True;
HTTP.OnLocationChange := FOnRedirect;
HTTP.OnDocBegin := FOnDocBegin;
HTTP.OnDocData := FOnDocData;
HTTP.OnHeaderData := IcsHttpHeaderData;
HTTP.OnCommand := IcsHttpCommand;
HTTP.OnCookie := FOnCookie;
HTTP.RequestVer := '1.1' ;
HTTP.Connection := 'Keep-Alive' ;
if FBasicAuth then HTTP.ServerAuth := httpAuthBasic;
HTTP.Reference := FReferer;
HTTP.OnStateChange := IcsStateChange;
HTTP.Proxy := FProxy;
HTTP.ProxyPort := FProxyPort;
HTTP.ProxyUsername := FProxyUser;
HTTP.ProxyPassword := FProxyPassword;
if (FProxyUser <> '') and (FProxyPassword <> '') then
                            HTTP.ProxyAuth := httpAuthBasic;
HTTP.Username := FUsername;
HTTP.Password := FPassword;
HTTP.Agent  := FUserAgent;
HTTP.Cookie := FCookie;
HTTP.Options := HTTP.Options + [httpoEnableContentCoding];
FState := httpReady;
end;

procedure THTTPConnection.GetPostFinal;
{common finalization for Get, Post}
begin
ReturnedContentType := HTTP.ContentType;
FContentLength := HTTP.ContentLength;
FResponseText := HTTP.ReasonPhrase;
FResponseCode := HTTP.StatusCode;
if HTTP.AuthorizationRequest.Count > 0 then  // ANGUS
    FRealm := HTTP.AuthorizationRequest [0] ;
// Angus - should keep header cache to stop page caching
HTTP.Free;
HTTP := Nil;
FState := httpNotConnected;
end;

{----------------THTTPConnection.Get}
procedure THTTPConnection.Get(const URL : String);
begin
GetPostInit1;
try
    HTTP.RcvdStream := FInputStream;
    HTTP.Url := URL;
    HTTP.Get;   // sync
Finally
  GetPostFinal;
  end;
end;

{----------------THTTPConnection.Post}
procedure THTTPConnection.Post(const URL : String);
begin
GetPostInit1;
try
//  HTTP.ContentType := ContentTypePost;
  HTTP.SendStream := SendStream;
  HTTP.RcvdStream := FInputStream;
  HTTP.Url := URL;
  HTTP.Post;  // sync
finally
  GetPostFinal;
  end;
end;

{----------------THTTPConnection.GetAsync}
procedure THTTPConnection.GetAsync(const URL : String);
begin
FAborted := False;
GetPostInit1;
HTTP.OnRequestDone := IcsRequestDone;
HTTP.RcvdStream := FInputStream;
HTTP.Url := URL;
FState := httpReady;
HTTP.GetAsync;
end;

procedure THTTPConnection.IcsHttpHeaderData (Sender : TObject);
begin
    HTTPForm.LogLine ('< ' + HTTP.LastResponse);
end;

procedure THTTPConnection.IcsHttpCommand (Sender : TObject; var S: String) ;
begin
    HTTPForm.LogLine ('> ' + S);
end;

{----------------THTTPConnection.RequestDone}
procedure THTTPConnection.IcsRequestDone (Sender : TObject; RqType : THttpRequest; Error  : Word);
var
  AHttp: TSslHttpCli;
begin
if RqType <> httpGET then exit ;
AHttp:= Sender as TSslHttpCli;
ReturnedContentType := AHTTP.ContentType;
FContentLength := AHTTP.ContentLength;
FResponseText := AHTTP.ReasonPhrase;
FResponseCode := AHTTP.StatusCode;

FState := httpNotConnected;

if not FAborted and (FResponseCode < 300) then   {Relocated images not handled}
  Error := 0
else Error := FResponseCode;
if Assigned(FOnRequestDone) then
  FOnRequestDone(Owner, httpGet, Error);
end;

procedure THTTPConnection.IcsStateChange(Sender: TObject);
begin
FState := (Sender as TSslHttpCli).State;
end;

function THTTPConnection.State: THttpState;
begin
result := FState;
end;

function THTTPConnection.ContentType: ThtmlFileType;
var
  Content: string;
begin
Content := Lowercase(ReturnedContentType);
if Pos('image/', Content) > 0 then    {image/*}
  Result := ImgType
else if Pos('/plain', Content) > 0 then  {text/plain}
  Result := TextType         {text/html}
else Result := HTMLType;
end;

function THTTPConnection.ContentLength: LongInt;
begin
Result := FContentLength;
end;

function THTTPConnection.RcvdCount : LongInt;
begin
    if Assigned(HTTP) then
      result := HTTP.RcvdCount
    else
      result := 0;
end;

function THTTPConnection.ReasonPhrase : String;
begin
     result := FResponseText;
end;

function THTTPConnection.LastResponse : String;
begin
    if Assigned(HTTP) then
      result := HTTP.LastResponse
    else
      result := '';
end;

function THTTPConnection.StatusCode : LongInt;
begin
     StatusCode := FResponseCode;
end;

{----------------THTTPConnection.Abort}
procedure THTTPConnection.Abort;
begin
FAborted := True;
if Assigned(HTTP) then
  HTTP.Abort;
end;

{----------------TFileConnection.Get}
procedure TFileConnection.Get(const URl: String);
var
   thefile, Ext : String;
   error, I : integer;
begin
  error := 1;
  try
     thefile := URL;

     {remove any query string as it's not certain how to respond to a Form
      submit with a file protocol.  The user can add a response if desired.}
     I := Pos('?', TheFile);
     if I > 0 then
       TheFile := Copy(TheFile, 1, I-1);

     Delete(thefile, 1,5+2);  { remove file:// }
     CheckInputStream;
     { We suppose that windows accept c:/test/test2 }
     if thefile[1] = '/' then
     begin
        Delete(thefile,1,1);
     end;
     TheFile := HTMLtoDOS(TheFile);
     FInputStream.LoadFromFile(thefile);
     error := 0;

     Ext := Lowercase(ExtractFileExt(TheFile));
     if (Ext = '.bmp') or (Ext = '.gif') or (Ext = '.jpg') or (Ext = '.jpeg')
           or (Ext = '.png') then
       FContentType := ImgType
     else if (Ext = '.txt') then
       FContentType := TextType
     else FContentType := HTMLType;
     FContentLength := FInputStream.Size;

     FStatusCode := 200;
     if Assigned(FOnRequestDone) then
        FOnRequestDone(owner, httpGET, error);
  except
     if Assigned(FOnRequestDone) then
        FOnRequestDone(owner, httpGET, error);
     raise;
  end;
end;

{----------------TResourceConnection.Get}
procedure TResourceConnection.Get(const URl: String);
var
   thefile, S, Ext : String;
   error : integer;
   HResInfo: HRSRC;
   HGlobal: THandle;
   Buffer, GoodType : pchar;
   I: integer;
begin
  error := 1;
  GoodType := '';
  try
     thefile := URL;

     {remove any query string as it's not certain how to respond to a Form
      submit with a res: protocol.  The user can add a response if desired.}
     I := Pos('?', TheFile);
     if I > 0 then
       TheFile := Copy(TheFile, 1, I-1);

    I := Pos('res:///', Lowercase(TheFile));
     if I > 0 then
       Delete(thefile, I,4+3)  { remove res:/// }
     else
       begin
       I :=  Pos('res://', Lowercase(TheFile));
       if I > 0 then
         Delete(thefile, I,4+2);  { accept res:// also }
       end;
     CheckInputStream;
     Ext := Uppercase(GetURLExtension(URL));
     if (Ext = 'HTM') or (Ext = 'HTML') or (Ext = 'CSS') then
        begin
        GoodType := 'HTML';
        FContentType := HTMLType;
        end
     else if (Ext = 'GIF') or (Ext = 'JPG') or (Ext = 'JPEG')
            or (Ext = 'PNG') or (Ext = 'BMP') then
        begin
        GoodType := PChar(Ext);
        FContentType := ImgType;
        end
     else if (Ext = 'TXT') then
        begin
        GoodType := 'TEXT';
        FContentType := TextType;
        end;
     HResInfo := FindResource(HInstance, pchar(thefile), GoodType);
     if HResInfo = 0 then
       begin    {try without the extension if can't find it with extension}
       I := Pos('.'+Ext, Uppercase(TheFile));
       if I>=0 then
         begin
         S := TheFile;
         System.Delete(S, I, Length(Ext)+1);
         HResInfo := FindResource(HInstance, pchar(S), GoodType);
         if HResInfo = 0 then
            raise EResNotFound.Create('Can''t find resource: '+thefile);
         end
       else raise EResNotFound.Create('Can''t find resource: '+thefile);
       end;
     HGlobal := LoadResource(HInstance, HResInfo);
     if HGlobal = 0 then
            raise EResNotFound.Create('Can''t load resource: '+thefile);
     Buffer := LockResource(HGlobal);
     InputStream.WriteBuffer(Buffer[0], SizeOfResource(HInstance, HResInfo));
     UnlockResource(HGlobal);
     FreeResource(HGlobal);
     error := 0;
     if Assigned(FOnRequestDone) then
        FOnRequestDone(owner, httpGET, error);
     FStatusCode := 200;
  except
     if Assigned(FOnRequestDone) then
        FOnRequestDone(owner, httpGET, error);
     raise
  end;
end;

{$ifdef IncludeZip}

type
  EZipFileError = class(Exception);

{----------------TZipConnection.Destroy}
destructor TZipConnection.Destroy;
begin
If unzipper <> nil then
   Unzipper.free;
inherited;   
end;

procedure TZipConnection.Get(const URl: String);
var
   num, error, I : integer;
   TheFile, Host, Ext : String;
begin
   { Syntax: zip://zipname/filetoextract
   { The full path is needed, as:  zip://c:\dir1\subdir\htmlfiles.zip/demo.htm
     or zip://c|/dir1/subdir/htmlfiles.zip/demo.htm }
   error := 1;
   try
         if Unzipper = nil then
            Unzipper := TVCLUnzip.Create(nil);
         thefile := URL;

         {remove any query string as it's not certain how to respond to a Form
          submit with a zip: protocol.  The user can add a response if desired.}
         I := Pos('?', TheFile);
         if I > 0 then
           TheFile := Copy(TheFile, 1, I-1);

         TheFile := GetURLFilenameAndExt(URL);
         Host := GetBase(URL);
         Delete(Host, 1, 6);     {remove zip://}
         Delete(Host, Length(Host), 1);  {remove trailing '/'}
         Host := HTMLToDos(Host);

         CheckInputStream;
         InputStream.Clear;  {apparently req'd for unzip routines}

         Ext := Uppercase(GetURLExtension(URL));
         FContentType := HTMLType;
         if (Ext = 'GIF') or (Ext = 'JPG') or (Ext = 'JPEG')
                or (Ext = 'PNG') or (Ext = 'BMP') then
            FContentType := ImgType
         else if (Ext = 'TXT') then
            FContentType := TextType;

         With Unzipper do
         begin
            if host <> ZipName Then
               ZipName := host;              { set the zip filename}
            try
              { Extract files, return value is the number of files actually unzipped}
              num := UnZipToStream( InputStream, TheFile );
            except
              raise EZipFileError.Create('Can''t open: '+URL);
              end;
            if num <> 1 then
                raise EZipFileError.Create('Can''t open: '+URL);
         end;
         error := 0;
         FStatusCode := 200;
         if Assigned(FOnRequestDone) then
            FOnRequestDone(owner, httpGET, error);
   finally
         if Assigned(FOnRequestDone) then
            FOnRequestDone(owner, httpGET, error);
   end;
end;
{$endif}

end.
