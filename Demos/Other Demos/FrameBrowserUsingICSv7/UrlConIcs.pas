{ To include the Zip: protocol, define "IncludeZip" by removing the "..." }
{ ...$Define IncludeZip }

{ ********************************************************* }
{ *                     UrlConId.PAS                      * }
{ *                Copyright (c) 1999 by                  * }
{ *                   Metaphor SPRL                       * }
{ *                 All rights reserved.                  * }
{ *                Written by Yves Urbain                 * }
{ ********************************************************* }

{ Copyright (c) 2012 by Angus Robertson delphi@magsys.co.uk }

unit UrlConIcs;

interface

{ ********************************************************* }
{ *                                                       * }
{ * This module contains a base class TURLConnection      * }
{ * that defines that behaviour of connection to a Web    * }
{ * resource: a HTML page, an image, ...                  * }
{ * This base classes contains a class method that creates* }
{ * the connection object that will handle connection to  * }
{ * a resource described by a specific protocol specified * }
{ in an URL                                             * }
{ * this method is 'getConnection'                        * }
{ *                                                       * }
{ * The implemented protocol are now :                    * }
{ *    - http managed by THTTPConnection                  * }
{ *    - file managed by TFileConnection                  * }
{ *      e.g file://d:/myprojects/demo.htm                * }
{ *    - res managed by TResConnection                    * }
{ *      The HTLM pages are stored in the application     * }
{ *      resources.                                       * }
{ *      e.g res:///Welcome.html                          * }
{ *    - zip managed by TZipConnection. Through the use   * }
{ of VCLZip (works also in Delphi 1) the HTML pages* }
{ *      are extracted from a zip file                    * }
{ *      e.g. zip://demo.zip/demo.htm URL will extract    * }
{ *      demo.htm from the demo.zip (stored in the current* }
{ *      Define IncludeZip to enable the use of           *)
  {*      TZipConnection                                   * }
{ *                                                       * }
{ * - version 1.0d1: first version                        * }
{ *                                                       * }
{ * March 2012 - Angus Robertson replaced Indy with ICSv7 * }
{ *      Suspect this program originally used ICS since   * }
{ *      many types, vars and events have ICS names       * }
{ *                                                       * }
{ * April 2012 - Angus simplified State                   * }
{ *                                                       * }
{ ********************************************************* }

uses
    WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls,
    Forms, Dialogs, ShellAPI, TypInfo, URLSubs, htmlview,
    OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsWinsock,
    OverbyteIcsHttpProt, OverbyteIcsHttpCCodzlib
{$IFDEF IncludeZip}
        , VCLUnZIp, kpZipObj
{$ENDIF}
        ;

type
    // THttpRequest     = (httpAbort, httpGET, httpPOST, httpHEAD);  use ICS version
    // THttpState       = (httpReady,         httpNotConnected, httpConnected,
    // httpDnsLookup,     httpDnsLookupDone,
    // httpWaitingHeader, httpWaitingBody,  httpAborting); // use ICS version
    // THttpRequestDone = procedure (Sender : TObject;
    // RqType : THttpRequest;
    // Error  : Word) of object;       // use ICS version

    TURLConnection = class(TObject)
    private
        FInputStream    : TMemoryStream;
        FInputStreamOwn : Boolean; { true if the class ownes the stream }
        procedure SetInputStream(Value : TMemoryStream);
    protected
        FOnDocBegin      : TNotifyEvent;
        FOnDocData       : TDocDataEvent;
        FOnRequestDone   : THttpRequestDone;
        FOnRedirect      : TNotifyEvent;
        FContentType     : ThtmlFileType;
        FContentLength   : LongInt;
        FProxy           : String;
        FProxyPort       : String;
        FProxyUser       : String;
        FProxyPassword   : String;
        FUsername        : String;
        FPassword        : String;
        FUserAgent       : String;
        FBasicAuth       : Boolean;
        FContentTypePost : String;
        FSendStream      : TStream;
        FOwner           : TComponent;
        FReasonPhrase    : String;
        FStatusCode      : integer;
        FReferer         : String;
        FRealm           : String; // ANGUS
        FCookie          : String; // ANGUS
        FOnCookie        : TCookieRcvdEvent; // ANGUS
        FSession         : integer; // ANGUS
    public
        constructor Create;
        destructor Destroy; override;
        procedure Get(const URL : String); virtual; abstract;
        procedure Post(const URL : String); virtual;
        procedure GetAsync(const URL : String); virtual;
        procedure PostAsync(const URL : String); virtual;
        procedure CheckInputStream; virtual;
        function RcvdCount : LongInt; virtual;
        function ReasonPhrase : String; virtual;
        function LastResponse : String; virtual;
        function StatusCode : LongInt; virtual;
        function State : THttpState; virtual;
        procedure Abort; virtual;
        procedure Close; virtual;  // ANGUS
        function ContentType : ThtmlFileType; virtual;
        function ContentLength : LongInt; virtual;
        class function Getconnection(const URL : String) : TURLConnection;
        property OnDocBegin : TNotifyEvent read FOnDocBegin write FOnDocBegin;
        property OnDocData : TDocDataEvent read FOnDocData write FOnDocData;
        property OnRequestDone : THttpRequestDone read FOnRequestDone
            write FOnRequestDone;
        property Owner : TComponent read FOwner write FOwner;
        property InputStream : TMemoryStream read FInputStream
            write SetInputStream;
        property Proxy : String read FProxy write FProxy;
        property ProxyPort : String read FProxyPort write FProxyPort;
        property ProxyUser : String read FProxyUser write FProxyUser;
        property ProxyPassword : String read FProxyPassword
            write FProxyPassword;
        property Username : String read FUsername write FUsername;
        property Password : String read FPassword write FPassword;
        property UserAgent : String read FUserAgent write FUserAgent;
        property OnRedirect : TNotifyEvent read FOnRedirect write FOnRedirect;
        property Referer : String read FReferer write FReferer;
        property Realm : String read FRealm; // ANGUS
        property Cookie : String read FCookie write FCookie; // ANGUS
        property OnCookie : TCookieRcvdEvent read FOnCookie write FOnCookie;
        // ANGUS
        property BasicAuth : Boolean read FBasicAuth write FBasicAuth;
        property ContentTypePost : String read FContentTypePost
            write FContentTypePost;
        property SendStream : TStream read FSendStream write FSendStream;
        property Session : integer read FSession write FSession;
    end;

    THTTPConnection = class(TURLConnection)
    private
        ReturnedContentType : String;
        FResponseText       : String;
        FResponseCode       : integer;
        // FRcvdCount: integer;
        FState         : THttpState;
        FAborted       : Boolean;
        FWinsockLoaded : Boolean;
        // FLastResponse : String;
        procedure GetPostInit1;
        procedure GetPostFinal;
    protected
        HTTP : TSslHttpCli;
        procedure IcsStateChange(Sender : TObject);
        procedure IcsRequestDone(Sender : TObject; RqType : THttpRequest;
            Error : Word);
        procedure IcsHttpHeaderData(Sender : TObject);
        procedure IcsHttpCommand(Sender : TObject; var S : String);
        procedure IcsHttpSessionConnected(Sender : TObject);
        procedure IcsHttpSessionClosed(Sender : TObject);
    public
        constructor Create;
        destructor Destroy; override;
        procedure Get(const URL : String); override;
        procedure GetAsync(const URL : String); override;
        procedure Post(const URL : String); override;
        function RcvdCount : LongInt; override;
        function ReasonPhrase : String; override;
        function LastResponse : String; override;
        function StatusCode : LongInt; override;
        function State : THttpState; override;
        function ContentType : ThtmlFileType; override;
        function ContentLength : LongInt; override;
        procedure Abort; override;
        procedure Close; override;  // ANGUS
    end;

    TFileConnection = class(TURLConnection)
    public
        procedure Get(const URL : String); override;
    end;

    TResourceConnection = class(TURLConnection)
    public
        procedure Get(const URL : String); override;
    end;

{$IFDEF IncludeZip}
    TZipConnection = class(TURLConnection)
    private
        UnZipper : TVCLUnzip;
    public
        destructor Destroy; override;
        procedure Get(const URL : String); override;
    end;
{$ENDIF}

type
    TProcolHandlerFunc = function(const URL : String) : TURLConnection;

var
    ProtocolHandler : TProcolHandlerFunc;

implementation

uses
    htmlun2, FBUnitIcs, Registry;

var
    GLmCompatLevel : LongWord; { AG }

function GetLMCompatLevel : integer; { AG }
const
    LSA_KEY    = 'System\CurrentControlSet\Control\LSA';
    VALUE_NAME = 'LMCompatibilityLevel';
begin
    Result := 0;
    try
        with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if KeyExists(LSA_KEY) then begin
                if OpenKey(LSA_KEY, False) and ValueExists(VALUE_NAME) then
                    Result := LongWord(ReadInteger(VALUE_NAME));
            end;
        finally
            Free;
        end;
    except
    end;
end;

constructor TURLConnection.Create;
begin
    inherited Create;
    FInputStream    := nil;
    SendStream      := nil;
    Owner           := nil;
    FStatusCode     := 404;
    FReasonPhrase   := 'Can''t get file';
    FInputStreamOwn := False;
    FContentType    := HTMLType;
end;

destructor TURLConnection.Destroy;
begin
    if FInputStreamOwn then
        FInputStream.Free;
    inherited Destroy;
end;

function TURLConnection.State : THttpState;
begin
    Result := HTTPReady;
end;

function TURLConnection.RcvdCount : LongInt;
begin
    Result := 0;
end;

function TURLConnection.LastResponse : String;
begin
    Result := '';
end;

function TURLConnection.ReasonPhrase : String;
begin
    Result := FReasonPhrase;
end;

function TURLConnection.StatusCode : LongInt;
begin
    StatusCode := FStatusCode;
end;

function TURLConnection.ContentType : ThtmlFileType;
begin
    Result := FContentType;
end;

function TURLConnection.ContentLength : LongInt;
begin
    Result := FContentLength;
end;

procedure TURLConnection.Abort;
begin
end;

procedure TURLConnection.Close;
begin
end;

procedure TURLConnection.CheckInputStream;
begin
    if FInputStream = nil then begin
        FInputStream    := TMemoryStream.Create;
        FInputStreamOwn := true;
    end;
end;

class function TURLConnection.Getconnection(const URL : String)
    : TURLConnection;
var
    protocol : String;
begin
    Result := nil;
    { If ProtocolHandler is defined call it else }
    if Assigned(ProtocolHandler) then
        Result := ProtocolHandler(URL);

    { Use default handlers }
    if Result = nil then begin
        protocol := GetProtocol(URL);
        if (protocol = 'http') or (protocol = 'https') then
            Result := THTTPConnection.Create
        else if protocol = 'file' then
            Result := TFileConnection.Create
        else if protocol = 'res' then
            Result := TResourceConnection.Create
{$IFDEF IncludeZip}
        else if protocol = 'zip' then
            Result := TZipConnection.Create
{$ENDIF}
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

procedure TURLConnection.SetInputStream(Value : TMemoryStream);
begin
    if Assigned(FInputStream) and FInputStreamOwn then
        FInputStream.Free;
    FInputStream    := Value;
    FInputStreamOwn := False;
end;

{ ----------------THTTPConnection.Create }
constructor THTTPConnection.Create;
begin
    inherited Create;
end;

{ ----------------THTTPConnection.Destroy }
destructor THTTPConnection.Destroy;
begin
    if Assigned(HTTP) then begin
        HTTP.Free;
        HTTP := nil;
    end;
    inherited Destroy;
end;

procedure THTTPConnection.GetPostInit1;
{ common initiation for Get, Post }
begin
    if not FWinsockLoaded then begin
        WSocketForceLoadWinsock;
        FWinsockLoaded := true;
    end;
    FAborted := False;
    CheckInputStream;
    if not Assigned(HTTP) then
        HTTP := TSslHttpCli.Create(nil);
    HTTP.ContentTypePost  := ContentTypePost;
    HTTP.SslContext       := HTTPForm.SslContext;
    HTTP.FollowRelocation := true;
    HTTP.OnLocationChange := FOnRedirect;
    HTTP.OnDocBegin       := FOnDocBegin;
    HTTP.OnDocData        := FOnDocData;
    HTTP.OnHeaderData     := IcsHttpHeaderData;
    HTTP.OnCommand        := IcsHttpCommand;
    HTTP.OnSessionConnected := IcsHttpSessionConnected;
    HTTP.OnSessionClosed  := IcsHttpSessionClosed;
    HTTP.OnCookie         := FOnCookie;
    HTTP.RequestVer       := '1.1';
    HTTP.Connection       := 'Keep-Alive';
    HTTP.Reference        := FReferer;
    HTTP.OnStateChange    := IcsStateChange;
    HTTP.Proxy            := FProxy;
    HTTP.ProxyPort        := FProxyPort;
    HTTP.ProxyUsername    := FProxyUser;
    HTTP.ProxyPassword    := FProxyPassword;
    if (FProxyUser <> '') and (FProxyPassword <> '') then
        HTTP.ProxyAuth := httpAuthBasic;
    HTTP.Username      := FUsername;
    HTTP.Password      := FPassword;
    HTTP.Agent         := FUserAgent;
    HTTP.Cookie        := FCookie;
    HTTP.Options       := HTTP.Options + [httpoEnableContentCoding];
    HTTP.LmCompatLevel := GLmCompatLevel; { AG }
end;

procedure THTTPConnection.GetPostFinal;
{ common finalization for Get, Post }
var
    I, SLen : integer;
    S       : String;
begin
    ReturnedContentType := HTTP.ContentType;
    FContentLength      := HTTP.ContentLength;
    FResponseText       := HTTP.ReasonPhrase;
    FResponseCode       := HTTP.StatusCode;
    if HTTP.AuthorizationRequest.Count > 0 then { AG }
    begin
        { the 'realm' here is just used for user/password lookups }
        S := HTTP.AuthorizationRequest[0];
        if StrIComp('NTLM', PChar(S)) = 0 then
            { there is no realm in NTLM }
            FRealm := 'NTLM @ ' + HTTP.Hostname
        else begin
            if StrLIComp(PChar(S), 'BASIC REALM="', 13) = 0 then begin
                I    := 14;
                SLen := Length(S);
                while (I <= SLen) and (S[I] <> '"') do
                    Inc(I);
                if S[I] = '"' then begin
                    FBasicAuth := true;
                    FRealm     := Copy(S, 1, I) + ' @ ' + HTTP.Hostname;
                end;
            end
            else begin
                if StrLIComp(PChar(S), 'DIGEST REALM="', 14) = 0 then begin
                    I    := 15;
                    SLen := Length(S);
                    while (I <= SLen) and (S[I] <> '"') do
                        Inc(I);
                    if S[I] = '"' then
                        FRealm := Copy(S, 1, I) + ' @ ' + HTTP.Hostname;
                end;
            end;
        end;
    end;
    // Angus - should keep header cache to stop page caching
end;

{ ----------------THTTPConnection.Get }
procedure THTTPConnection.Get(const URL : String);
begin
    GetPostInit1;
    try
        HTTP.OnRequestDone := Nil;
        HTTP.RcvdStream := FInputStream;
        HTTP.URL        := URL;
        HTTP.Get; // sync
    finally
        GetPostFinal;
    end;
end;

{ ----------------THTTPConnection.Post }
procedure THTTPConnection.Post(const URL : String);
begin
    GetPostInit1;
    try
        // HTTP.ContentType := ContentTypePost;
        HTTP.OnRequestDone := Nil;
        HTTP.SendStream := SendStream;
        HTTP.RcvdStream := FInputStream;
        HTTP.URL        := URL;
        HTTP.Post; // sync
    finally
        GetPostFinal;
    end;
end;

{ ----------------THTTPConnection.GetAsync }
procedure THTTPConnection.GetAsync(const URL : String);
begin
    FAborted := False;
    GetPostInit1;
    HTTP.OnRequestDone := IcsRequestDone;
    HTTP.RcvdStream    := FInputStream;
    HTTP.URL           := URL;
    HTTP.GetAsync;
end;

procedure THTTPConnection.IcsHttpHeaderData(Sender : TObject);
begin
    if HTTPForm.ShowLogHTTP.Checked then
        HTTPForm.LogLine('[' + IntToStr (Session) + '] < ' + HTTP.LastResponse);
end;

procedure THTTPConnection.IcsHttpCommand(Sender : TObject; var S : String);
begin
    if HTTPForm.ShowLogHTTP.Checked then
        HTTPForm.LogLine('[' + IntToStr (Session) + '] > ' + S);
end;

procedure THTTPConnection.IcsHttpSessionConnected(Sender : TObject);
begin
    if HTTPForm.ShowLogHTTP.Checked then
        HTTPForm.LogLine('[' + IntToStr (Session) + '] Connected to ' +
                                     (Sender as TSslHttpCli).Hostname);
end;

procedure THTTPConnection.IcsHttpSessionClosed(Sender : TObject);
begin
    if HTTPForm.ShowLogHTTP.Checked then
        HTTPForm.LogLine('[' + IntToStr (Session) + '] Disconnected');
end;

{ ----------------THTTPConnection.RequestDone }
procedure THTTPConnection.IcsRequestDone(Sender : TObject;
    RqType : THttpRequest; Error : Word);
var
    AHttp : TSslHttpCli;
begin
    if RqType <> httpGET then begin
        HTTPForm.LogLine('[' + IntToStr (Session) + '] Unexpected RequestDone');
        exit;
    end;
    AHttp               := Sender as TSslHttpCli;
    ReturnedContentType := AHttp.ContentType;
    FContentLength      := AHttp.ContentLength;
    FResponseText       := AHttp.ReasonPhrase;
    FResponseCode       := AHttp.StatusCode;

    if not FAborted and (FResponseCode < 300)
    then { Relocated images not handled }
        Error := 0
    else
        Error := FResponseCode;
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Owner, httpGET, Error);
end;

procedure THTTPConnection.IcsStateChange(Sender : TObject);
begin
    FState := (Sender as TSslHttpCli).State;
    if HTTPForm.ShowLogHTTP.Checked then
        HTTPForm.LogLine('[' + IntToStr (Session) + '] StateChange: ' +
                    GetEnumName (Typeinfo (THttpState), Ord (FState)));
end;

function THTTPConnection.State : THttpState;
begin
    if Assigned(HTTP) then
        FState := HTTP.State
    else
        FState := httpNotConnected;
    result := FState;
end;

function THTTPConnection.ContentType : ThtmlFileType;
var
    Content : String;
begin
    Content := Lowercase(ReturnedContentType);
    if Pos('image/', Content) > 0 then { image/* }
        Result := ImgType
    else if Pos('/plain', Content) > 0 then { text/plain }
        Result := TextType { text/html }
    else
        Result := HTMLType;
end;

function THTTPConnection.ContentLength : LongInt;
begin
    Result := FContentLength;
end;

function THTTPConnection.RcvdCount : LongInt;
begin
    if Assigned(HTTP) then
        Result := HTTP.RcvdCount
    else
        Result := 0;
end;

function THTTPConnection.ReasonPhrase : String;
begin
    Result := FResponseText;
end;

function THTTPConnection.LastResponse : String;
begin
    if Assigned(HTTP) then
        Result := HTTP.LastResponse
    else
        Result := '';
end;

function THTTPConnection.StatusCode : LongInt;
begin
    StatusCode := FResponseCode;
end;

{ ----------------THTTPConnection.Abort }
procedure THTTPConnection.Abort;
begin
    FAborted := true;
    if Assigned(HTTP) then
        HTTP.Abort;
end;

{ ----------------THTTPConnection.Close }
procedure THTTPConnection.Close;
begin
    if Assigned(HTTP) then begin
        if FState = httpReady then
            HTTP.Close;
    end;
end;

{ ----------------TFileConnection.Get }
procedure TFileConnection.Get(const URL : String);
var
    thefile, Ext : String;
    Error, I     : integer;
begin
    Error := 1;
    try
        thefile := URL;

        { remove any query String as it's not certain how to respond to a Form
          submit with a file protocol.  The user can add a response if desired. }
        I := Pos('?', thefile);
        if I > 0 then
            thefile := Copy(thefile, 1, I - 1);

        Delete(thefile, 1, 5 + 2); { remove file:// }
        CheckInputStream;
        { We suppose that windows accept c:/test/test2 }
        if thefile[1] = '/' then begin
            Delete(thefile, 1, 1);
        end;
        thefile := HTMLtoDOS(thefile);
        FInputStream.LoadFromFile(thefile);
        Error := 0;

        Ext := Lowercase(ExtractFileExt(thefile));
        if (Ext = '.bmp') or (Ext = '.gif') or (Ext = '.jpg') or (Ext = '.jpeg')
            or (Ext = '.png') then
            FContentType := ImgType
        else if (Ext = '.txt') then
            FContentType := TextType
        else
            FContentType := HTMLType;
        FContentLength   := FInputStream.Size;

        FStatusCode := 200;
        if Assigned(FOnRequestDone) then
            FOnRequestDone(Owner, httpGET, Error);
    except
        if Assigned(FOnRequestDone) then
            FOnRequestDone(Owner, httpGET, Error);
        raise;
    end;
end;

{ ----------------TResourceConnection.Get }
procedure TResourceConnection.Get(const URL : String);
var
    thefile, S, Ext  : String;
    Error            : integer;
    HResInfo         : HRSRC;
    HGlobal          : THandle;
    Buffer, GoodType : PChar;
    I                : integer;
begin
    Error    := 1;
    GoodType := '';
    try
        thefile := URL;

        { remove any query String as it's not certain how to respond to a Form
          submit with a res: protocol.  The user can add a response if desired. }
        I := Pos('?', thefile);
        if I > 0 then
            thefile := Copy(thefile, 1, I - 1);

        I := Pos('res:///', Lowercase(thefile));
        if I > 0 then
            Delete(thefile, I, 4 + 3) { remove res:/// }
        else begin
            I := Pos('res://', Lowercase(thefile));
            if I > 0 then
                Delete(thefile, I, 4 + 2); { accept res:// also }
        end;
        CheckInputStream;
        Ext := Uppercase(GetURLExtension(URL));
        if (Ext = 'HTM') or (Ext = 'HTML') or (Ext = 'CSS') then begin
            GoodType     := 'HTML';
            FContentType := HTMLType;
        end
        else if (Ext = 'GIF') or (Ext = 'JPG') or (Ext = 'JPEG') or
            (Ext = 'PNG') or (Ext = 'BMP') then begin
            GoodType     := PChar(Ext);
            FContentType := ImgType;
        end
        else if (Ext = 'TXT') then begin
            GoodType     := 'TEXT';
            FContentType := TextType;
        end;
        HResInfo := FindResource(HInstance, PChar(thefile), GoodType);
        if HResInfo = 0 then
        begin { try without the extension if can't find it with extension }
            I := Pos('.' + Ext, Uppercase(thefile));
            if I >= 0 then begin
                S := thefile;
                System.Delete(S, I, Length(Ext) + 1);
                HResInfo := FindResource(HInstance, PChar(S), GoodType);
                if HResInfo = 0 then
                    raise EResNotFound.Create('Can''t find resource: '
                        + thefile);
            end
            else
                raise EResNotFound.Create('Can''t find resource: ' + thefile);
        end;
        HGlobal := LoadResource(HInstance, HResInfo);
        if HGlobal = 0 then
            raise EResNotFound.Create('Can''t load resource: ' + thefile);
        Buffer := LockResource(HGlobal);
        InputStream.WriteBuffer(Buffer[0], SizeOfResource(HInstance, HResInfo));
        UnlockResource(HGlobal);
        FreeResource(HGlobal);
        Error := 0;
        if Assigned(FOnRequestDone) then
            FOnRequestDone(Owner, httpGET, Error);
        FStatusCode := 200;
    except
        if Assigned(FOnRequestDone) then
            FOnRequestDone(Owner, httpGET, Error);
        raise
    end;
end;

{$IFDEF IncludeZip}

type
    EZipFileError = class(Exception);

    { ----------------TZipConnection.Destroy }
destructor TZipConnection.Destroy;
begin
    if UnZipper <> nil then
        UnZipper.Free;
    inherited;
end;

procedure TZipConnection.Get(const URL : String);
var
    num, Error, I      : integer;
    thefile, Host, Ext : String;
begin
    { Syntax: zip://zipname/filetoextract
      { The full path is needed, as:  zip://c:\dir1\subdir\htmlfiles.zip/demo.htm
      or zip://c|/dir1/subdir/htmlfiles.zip/demo.htm }
    Error := 1;
    try
        if UnZipper = nil then
            UnZipper := TVCLUnzip.Create(nil);
        thefile      := URL;

        { remove any query String as it's not certain how to respond to a Form
          submit with a zip: protocol.  The user can add a response if desired. }
        I := Pos('?', thefile);
        if I > 0 then
            thefile := Copy(thefile, 1, I - 1);

        thefile := GetURLFilenameAndExt(URL);
        Host    := GetBase(URL);
        Delete(Host, 1, 6); { remove zip:// }
        Delete(Host, Length(Host), 1); { remove trailing '/' }
        Host := HTMLtoDOS(Host);

        CheckInputStream;
        InputStream.Clear; { apparently req'd for unzip routines }

        Ext          := Uppercase(GetURLExtension(URL));
        FContentType := HTMLType;
        if (Ext = 'GIF') or (Ext = 'JPG') or (Ext = 'JPEG') or (Ext = 'PNG') or
            (Ext = 'BMP') then
            FContentType := ImgType
        else if (Ext = 'TXT') then
            FContentType := TextType;

        with UnZipper do begin
            if Host <> ZipName then
                ZipName := Host; { set the zip filename }
            try
                { Extract files, return value is the number of files actually unzipped }
                num := UnZipToStream(InputStream, thefile);
            except
                raise EZipFileError.Create('Can''t open: ' + URL);
            end;
            if num <> 1 then
                raise EZipFileError.Create('Can''t open: ' + URL);
        end;
        Error       := 0;
        FStatusCode := 200;
        if Assigned(FOnRequestDone) then
            FOnRequestDone(Owner, httpGET, Error);
    finally
        if Assigned(FOnRequestDone) then
            FOnRequestDone(Owner, httpGET, Error);
    end;
end;
{$ENDIF}

initialization

GLmCompatLevel := GetLMCompatLevel; { AG }

end.
