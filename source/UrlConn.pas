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

{ Inspired by former UrlConId10.PAS written by Yves Urbain }

{$I htmlcons.inc}

unit UrlConn;

interface

uses
  Classes, Types, Contnrs, SysUtils, Forms,
  URLSubs, HtmlGlobals;

const
  ChtMaxRunningThreadCount = 20;

type
  EhtUrlConnException = class(Exception);

  ThtUrlDocStatus = (ucsInitial, ucsInProgress, ucsLoaded, ucsError);

  ThtConnector = class;
  ThtConnection = class;
  ThtUrlDoc = class;

  ThtGetAuthorizationEvent = function (Sender: ThtConnection; TryRealm: Boolean): Boolean of object;

//------------------------------------------------------------------------------
// A ThtConnectionManager gets a connection for a given transport protocol.
//
// Place a ThtConnectionManager component on your form and also place
// transport protocol specific ThtConnector components like ThtFileConnector
// and ThtResourceConnector on your form. They automatically register
// themselves at the ThtConnectionManager and offer creating connections for
// one or more protocols.
//
// Now the ThtConnectionManager is ready to support TFrameBrowser events like
// OnGetPostRequest() and OnGetImageRequest() by creating the appropriate
// ThtConnection descendant that can get the requested document resp. image.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtConnectionManager = class(TComponent)
  private
    FConnectors: TList;
    function GetCount: Integer;
    function GetConnector(Index: Integer): ThtConnector;
    function GetConnectorForProtocol(const Protocol: String): ThtConnector;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterConnector(Connector: ThtConnector);
    procedure UnregisterConnector(Connector: ThtConnector);

    function GetAllProtocols: String;
    function CreateConnection(const Protocol: String): ThtConnection;
    function TryCreateConnection(const Protocol: String; var Connection: ThtConnection): Boolean;
    function IndexOfProtocol(const Protocol: String): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: ThtConnector read GetConnector; default;
  published
    property AllProtocols: String read GetAllProtocols;
  end;

//------------------------------------------------------------------------------
// A ThtConnector descendant knows how to create connections for one or more
// protocols.
//
// To implement a new protocol derive one descendant from ThtConnector and
// another from ThtConnection.
//
// The ThtConnector descendant must override the following functions:
//
// - GetDefaultProtocols()
//     to tell the ThtConnectionManager which protocols it supports,
//
// - GetVersion()
//     to tell the application the ThtConnector's implementor and version and
//
// - CreateConnection(const Protocol: String)
//     which actually creates the connection for the given protocol.
//
// In case the connection may need authorization publish OnGetAuthorization
// rather than implementing another login/authorization dialog.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtConnector = class(TComponent)
  private
    FProtocols: String;
    FOnGetAuthorization: ThtGetAuthorizationEvent;
    FConnectionManager: ThtConnectionManager;

    procedure SetProtocols(const Value: String);
    function StoreProtocols: Boolean;
    procedure SetConnectionManger(const Value: ThtConnectionManager);
  protected
    class function GetDefaultProtocols: String; virtual; abstract;
    class function GetVersion: String; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetAuthorization(Connection: ThtConnection; TryRealm: Boolean): Boolean; virtual;
    property OnGetAuthorization: ThtGetAuthorizationEvent read FOnGetAuthorization write FOnGetAuthorization;
  public
    constructor Create(AOwner: TComponent); override;

    function CreateConnection(const Protocol: String): ThtConnection; virtual; abstract;
    function SupportsProtocol(const Protocol: String): Boolean;
  published
    property Version: String read GetVersion;
    property Protocols: String read FProtocols write SetProtocols stored StoreProtocols;
    property ConnectionManager: ThtConnectionManager read FConnectionManager write SetConnectionManger;
  end;

//----------------------------------------------------------------------------
// A ThtConnection gets a document via LoadDoc(Doc: ThtUrlDoc)
//
// LoadDoc():
//   When called at least Doc.Url must be filled.
//   On return if Doc.Status is ucsLoaded Doc.Stream is filled with the loaded document.
//   On return if Doc.Status is ucsInProgress the connection is loading the document asynchronously.
//
// The ThtConnection descendant must override the following function:
//
// - Get()
//     to load document Doc.Url into Doc.Stream
//
// The ThtConnection descendant may override the following function:
//
// - CreateUrlDoc()
//     to create an appropriate descendant of ThtUrlDoc.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtConnection = class
  private
    FDoc: ThtUrlDoc;
    FOnDocBegin: TNotifyEvent;
    FOnDocData: TNotifyEvent;
    FOnDocEnd: TNotifyEvent;

    FSessionId: Integer;
    FAborted: Boolean;
    FRealm: String;
    FPassword: String;
    FUsername: String;
    FBasicAuthentication: Boolean;
    FReasonPhrase: String;
    FReceivedSize: Int64;
    FExpectedSize: Int64;

  protected
    function GetAuthorization(TryRealm: Boolean): Boolean;
    function GetConnector: ThtConnector; virtual;
    procedure Get(Doc: ThtUrlDoc); virtual; abstract;
  public
    constructor Create;

    function CreateUrlDoc(PostIt: Boolean; const URL, Query, QueryEncType, Referer: String): ThtUrlDoc; virtual;
    procedure LoadDoc(Doc: ThtUrlDoc);
    procedure Abort; virtual;

    property Aborted: Boolean read FAborted write FAborted;
    property Realm: String read FRealm write FRealm;
    property Password: String read FPassword write FPassword;
    property Username: String read FUsername write FUsername;
    property BasicAuthentication: Boolean read FBasicAuthentication write FBasicAuthentication;

    function IsProcessing: Boolean; // Must not be virtual! Checks Self just like Free does.
    function ReasonPhrase: String; virtual;

    property SessionId   : Integer      read FSessionId;
    property Doc         : ThtUrlDoc    read FDoc;
    property Processing  : Boolean      read IsProcessing;
    property ReceivedSize: Int64        read FReceivedSize write FReceivedSize;
    property ExpectedSize: Int64        read FExpectedSize write FExpectedSize;
    property OnDocBegin  : TNotifyEvent read FOnDocBegin   write FOnDocBegin;
    property OnDocData   : TNotifyEvent read FOnDocData    write FOnDocData;
    property OnDocEnd    : TNotifyEvent read FOnDocEnd     write FOnDocEnd;
  end;

//------------------------------------------------------------------------------
// ThtUrlDoc holds the document specific data required for a document request.
//
// A ThtUrlDoc object is passed to ThtConnection.LoadDoc(Doc: ThtUrlDoc).
//   When called at least Doc.Url must be filled.
//   On return if Doc.Status is ucsLoaded Doc.Stream is filled with the loaded document.
//   On return if Doc.Status is ucsInProgress the connection is loading the document asynchronously.
//
// ThtUrlDoc objects are designed to be kept in a first level cache in memory.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtUrlDoc = class(TObject)
  private
    FUrl: String;
    FNewUrl: String;
    FReferer: String;
    FQuery: String;
    FQueryEncType: String;
    FStream: TStream;
    FPostIt: Boolean;
    FStatus: ThtUrlDocStatus;
    FDocType: ThtDocType;
    function GetStream: TStream;
    procedure SetNewUrl(const Value: String);
    procedure SetStream(const Value: TStream);
    function GetStreamSize: Int64;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(FileName: String);

    property Url: String read FUrl write FUrl;

    property Status: ThtUrlDocStatus read FStatus write FStatus;
    property DocType: ThtDocType read FDocType write FDocType;
    property Stream: TStream read GetStream write SetStream;
    property StreamSize: Int64 read GetStreamSize;

    property NewUrl: String read FNewUrl write SetNewUrl;
    property PostIt: Boolean read FPostIt write FPostIt;
    property Referer: String read FReferer write FReferer;
    property Query: String read FQuery write FQuery;
    property QueryEncType: String read FQueryEncType write FQueryEncType;
  end;

//------------------------------------------------------------------------------
// Use ThtProxyConnector as base class for protocols like http, ftp, etc.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtProxyConnector = class(ThtConnector)
  private
    FProxyServer: String;
    FProxyPort: String;
    FProxyUsername: String;
    FProxyPassword: String;
  published
    property ProxyServer  : String read FProxyServer   write FProxyServer;
    property ProxyPort    : String read FProxyPort     write FProxyPort;
    property ProxyUsername: String read FProxyUsername write FProxyUsername;
    property ProxyPassword: String read FProxyPassword write FProxyPassword;
  end;

//------------------------------------------------------------------------------
// ThtFileConnection and ThtFileConnector implement the protocol 'file'
// that gets files from a file system.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtFileConnection = class(ThtConnection)
  private
    procedure MakeDirOutput(AStream : TStream; const ADirName : String);
  protected
    procedure Get(ADoc: ThtUrlDoc); override;
  end;

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtFileConnector = class(ThtConnector)
  protected
    class function GetDefaultProtocols: string; override;
    class function GetVersion: string; override;
  public
    function CreateConnection(const Protocol: String): ThtConnection; override;
  end;

//------------------------------------------------------------------------------
// ThtResourceConnection and ThtResourceConnector implement the protocol 'res'
// that gets files from the application's resources.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtResourceConnection = class(ThtConnection)
  protected
    procedure Get(ADoc: ThtUrlDoc); override;
  end;

  //-- BG -------------------------------------------------------- 18.05.2016 --
  ThtResourceConnector = class(ThtConnector)
  protected
    class function GetDefaultProtocols: string; override;
    class function GetVersion: string; override;
  public
    function CreateConnection(const Protocol: String): ThtConnection; override;
  end;

//------------------------------------------------------------------------------
// ThtUrlDocLoaderThread can load a document asynchronously.
//
// Given document is loaded via given connection. After having loaded the document
// the OnLoaded event is fired.
//
// ThtUrlDocLoaderThread frees the connection but not the document as you might
// want to keep it in a cache. If not you must free the document.
//------------------------------------------------------------------------------

  ThtUrlDocLoadedEvent = procedure(Sender: ThtUrlDoc; Receiver: TObject) of object;

  //-- BG -------------------------------------------------------- 19.05.2016 --
  ThtUrlDocLoaderThread = class(TThread)
  private
    FConnection: ThtConnection;
    FUrlDoc: ThtUrlDoc;
    FOnLoaded: ThtUrlDocLoadedEvent;
    FReceiver: TObject;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure FreeConnection;
  public
    constructor Create(
      Connection: ThtConnection;
      UrlDoc: ThtUrlDoc;
      OnLoaded: ThtUrlDocLoadedEvent;
      Receiver: TObject);
    procedure Loaded;
    property Connection: ThtConnection read FConnection;
    property UrlDoc: ThtUrlDoc read FUrlDoc;
    property OnLoaded: ThtUrlDocLoadedEvent read FOnLoaded;
    property Receiver: TObject read FReceiver;
  end;

  ThtOnProgressEvent = procedure(Sender: TObject; Done, Total: Integer) of object;

//------------------------------------------------------------------------------
// ThtUrlDocLoaderThreadList can load documents asynchronously using
// ThtUrlDocLoaderThreads.
//
// Not to overcharge the systems ThtUrlDocLoaderThreadList runs at most
// FMaxRunningThreadCount threads at a time. If one thread terminates the next
// thread is started automatically until all documents are loaded.
//------------------------------------------------------------------------------

  //-- BG -------------------------------------------------------- 19.05.2016 --
  ThtUrlDocLoaderThreadList = class(TComponent)
  private
    FWaiting: TList;
    FRunning: TList;
    FDone: Integer;
    FFailed: Integer;
    FTotal: Integer;
    FMaxRunningThreadCount: Integer;
    FOnProgress: ThtOnProgressEvent;
    procedure Terminated(Sender: TObject);
    procedure StartNext;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RunningCount: Integer;
    function WaitingCount: Integer;
    procedure AddLoader(Thread: ThtUrlDocLoaderThread);
    property FailureCount: Integer read FFailed;
  published
    property MaxRunningThreadCount: Integer read FMaxRunningThreadCount default ChtMaxRunningThreadCount;
    property OnProgress: ThtOnProgressEvent read FOnProgress write FOnProgress;
  end;

//------------------------------------------------------------------------------

function ContentType2DocType(const AContentType: String): ThtDocType;
function FileExt2DocType(const AExt: String): ThtDocType;

//------------------------------------------------------------------------------
implementation

uses
  HtmlUn2;

var
  GNextSessionId: Integer;

//-- BG ---------------------------------------------------------- 29.08.2007 --
function ContentType2DocType(const AContentType: String): ThtDocType;
var
  ContentType: String;
begin
  ContentType := LowerCase(AContentType);
  if (Pos('text/html', ContentType) > 0) or (Pos('text/css', ContentType) > 0) then
    Result := HTMLType
  else if Pos('application/xhtml+xml', ContentType) > 0 then
    Result := XHtmlType
  else if Pos('image/', ContentType) > 0 then
    Result := ImgType
  else if Pos('text/plain', ContentType) > 0 then
    Result := TextType
  else
    Result := OtherType;
end;


//-- BG ---------------------------------------------------------- 25.08.2007 --
function FileExt2DocType(const AExt: String): ThtDocType;
var
  Ext: String;
begin
  Ext := ',' + LowerCase(AExt) +',';
  // get type
  if Pos(Ext, ',htm,html,css,php,asp,shtml,') > 0 then
    Result := HTMLType
  else if Pos(Ext, ',xht,xhtml,') > 0 then
    Result := XHtmlType
  else if Pos(Ext, ',gif,tiff,tif,jpg,jpeg,png,bmp,rle,dib,jpe,jfif,emf,wmf,') > 0 then
    Result := ImgType
  else if Pos(Ext, ',txt,ini,sql,') > 0 then
    Result := TextType
  else
    Result := OtherType;
end;

{ ThtUrlDoc }

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtUrlDoc.Clear;
begin
  FreeAndNil(FStream);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
destructor ThtUrlDoc.Destroy;
begin
  FStream.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtUrlDoc.GetStream: TStream;
begin
  if FStream = nil then
    FStream := TMemoryStream.Create;
  Result := FStream;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtUrlDoc.GetStreamSize: Int64;
begin
  if FStream <> nil then
    Result := FStream.Size
  else
    Result := 0;
end;

////-- BG ---------------------------------------------------------- 18.05.2016 --
//procedure ThtUrlDoc.SetName(const Value: String);
//begin
//  FName := Value;
//  if Length(FUrl) = 0 then
//    FUrl := Value;
//end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtUrlDoc.SetNewUrl(const Value: String);
begin
  FNewUrl := Value;
  if Length(FNewUrl) = 0 then
    FNewUrl := FUrl;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDoc.SetStream(const Value: TStream);
begin
  if FStream <> Value then
  begin
    FStream.Free;
    FStream := Value;
  end;
end;

////-- BG ---------------------------------------------------------- 18.05.2016 --
//procedure ThtUrlDoc.SetUrl(const Value: String);
//begin
//  FUrl := Value;
//  if Length(FName) = 0 then
//    FName := Value;
//end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtUrlDoc.SaveToFile(FileName: String);
var
  FileStream: TFileStream;
begin
  if (Status = ucsLoaded) and (FStream <> nil) then
  begin
    FileStream := TFileStream.Create(Filename, fmCreate);
    try
      FileStream.CopyFrom(Stream, 0);
    finally
      FileStream.free;
    end;
  end;
end;

{ ThtConnection }

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtConnection.Abort;
begin
  FAborted := True;
end;

constructor ThtConnection.Create;
begin
  inherited;
  FSessionId := GNextSessionId;
  Inc(GNextSessionId);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnection.CreateUrlDoc(PostIt: Boolean; const URL, Query, QueryEncType, Referer: String): ThtUrlDoc;
begin
  Result := ThtUrlDoc.Create;
  Result.PostIt := PostIt;
  Result.Url := URL;
  Result.Query := Query;
  Result.QueryEncType := QueryEncType;
  Result.Referer := Referer;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtConnection.LoadDoc(Doc: ThtUrlDoc);
begin
  FDoc := Doc;
  try
    Get(Doc);
  finally
    FDoc := nil;
  end;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtConnection.GetAuthorization(TryRealm: Boolean): Boolean;
var
  Connector: ThtConnector;
begin
  Connector := GetConnector;
  Result := (Connector <> nil) and Connector.GetAuthorization(Self, TryRealm);
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
function ThtConnection.GetConnector: ThtConnector;
begin
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtConnection.IsProcessing: Boolean;
begin
  Result := (Self <> nil) and (FDoc <> nil) and (FDoc.Status = ucsInProgress);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnection.ReasonPhrase: String;
begin
  Result := FReasonPhrase;
end;

{ ThtConnector }

//-- BG ---------------------------------------------------------- 18.05.2016 --
constructor ThtConnector.Create(AOwner: TComponent);
begin
  inherited;
  Protocols := GetDefaultProtocols;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtConnector.GetAuthorization(Connection: ThtConnection; TryRealm: Boolean): Boolean;
begin
  Result := Assigned(FOnGetAuthorization) and FOnGetAuthorization(Connection, TryRealm);
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
procedure ThtConnector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  case Operation of
// cannot register myself as ConnectionManager.FConnectors not yet created.
//    opInsert:
//      if not (csLoading in ComponentState) then
//        if (AComponent is ThtConnectionManager) and (FConnectionManager = nil) then
//          ConnectionManager := AComponent as ThtConnectionManager;

    opRemove:
      if AComponent = FConnectionManager then
        FConnectionManager := nil;
  end;
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
procedure ThtConnector.SetConnectionManger(const Value: ThtConnectionManager);
begin
  if FConnectionManager <> Value then
  begin
    if FConnectionManager <> nil then
      FConnectionManager.UnregisterConnector(Self);
    FConnectionManager := Value;
    if FConnectionManager <> nil then
      FConnectionManager.RegisterConnector(Self);
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtConnector.SetProtocols(const Value: String);
begin
  FProtocols := LowerCase(Value);
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtConnector.StoreProtocols: Boolean;
begin
  Result := FProtocols <> GetDefaultProtocols;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnector.SupportsProtocol(const Protocol: String): Boolean;
begin
  // enclosed in ',' to exactly (and not partially) match single protocol entry
  // and an item of a comma separated list.
  Result := Pos(',' + Protocol + ',', ',' + FProtocols + ',') > 0;
end;

{ ThtConnectionManager }

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.GetAllProtocols: String;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Count - 1 do
  begin
    if Length(Result) > 0 then
      Result := Result + ',';
    Result := Result + Items[Index].Protocols;
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
constructor ThtConnectionManager.Create(AOwner: TComponent);
begin
  inherited;
  FConnectors := TList.Create;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.CreateConnection(const Protocol: String): ThtConnection;
begin
  Result := GetConnectorForProtocol(Protocol).CreateConnection(Protocol);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
destructor ThtConnectionManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to FConnectors.Count - 1 do
    GetConnector(I).FConnectionManager := nil;
  FConnectors.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.GetConnector(Index: Integer): ThtConnector;
begin
  Result := ThtConnector(FConnectors[Index]);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.GetConnectorForProtocol(const Protocol: String): ThtConnector;
var
  Index: Integer;
begin
  Index := IndexOfProtocol(Protocol);
  if Index < 0 then
    raise EhtUrlConnException.CreateFmt(
      'Unsupported protocol ''%s''. Supported protocols are %s.', [Protocol, AllProtocols]);
  Result := Items[Index];
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.GetCount: Integer;
begin
  if FConnectors <> nil then
    Result := FConnectors.Count
  else
    Result := 0;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.IndexOfProtocol(const Protocol: String): Integer;
begin
  Result := Count - 1;
  while Result >= 0 do
    if Items[Result].SupportsProtocol(Protocol) then
      Exit
    else
      Dec(Result);
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
procedure ThtConnectionManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  case Operation of
    opInsert:
      if not (csLoading in ComponentState) then
        if AComponent is ThtConnector then
          (AComponent as ThtConnector).ConnectionManager := Self;

    opRemove:
      if AComponent <> Self then
        FConnectors.Remove(AComponent);
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtConnectionManager.RegisterConnector(Connector: ThtConnector);
begin
  if FConnectors.IndexOf(Connector) < 0 then
    FConnectors.Add(Connector);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtConnectionManager.TryCreateConnection(const Protocol: String; var Connection: ThtConnection): Boolean;
var
  Index: Integer;
begin
  Index := IndexOfProtocol(Protocol);
  Result := Index >= 0;
  if Result then
    Connection := Items[Index].CreateConnection(Protocol);
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtConnectionManager.UnregisterConnector(Connector: ThtConnector);
begin
  FConnectors.Remove(Connector);
end;

{ ThtFileConnection }

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtFileConnection.MakeDirOutput(AStream: TStream; const ADirName: String);
{This is for generating HTML from a directory listing including
special dir entries.  Note that I realize a lot of the markup is probably
unnecessary but it's a good idea to include it anyway to encourage
good HTML habits.}
var
  F : TSearchRec;
  TimeStamp: TDateTime;
  Name: String;
  Size: String;
  Text: TStringList;
begin
  Text := TStringList.Create;
  try
    Text.Add('<!DOCTYPE html>');
    Text.Add('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">');
    Text.Add('<head>');
    Text.Add('<meta charset="utf-16le" />');
    Text.Add('<base href="file://'+ EncodeUrl(DosToHTML(ADirName)) +'" />');
    Text.Add('<title>' + ADirName + '</title>');
    Text.Add('<style type="text/css">');
    Text.Add('  .fn {text-align:left; font-weight:normal;}');
    Text.Add('  .tm {text-align:center;}');
    Text.Add('  .sz {text-align:right;}');
    Text.Add('  table {width: 100%}');
    Text.Add('</style>');
    Text.Add('</head>');

    Text.Add('<body>');
    Text.Add('<h1>' + ADirName + '</h1>');
    if (FindFirst( IncludeTrailingPathDelimiter(ADirName) + '*.*', faAnyFile, F) = 0) then
    begin
      try
        Text.Add('<table>');

        Text.Add('<thead>');
        Text.Add('<tr><th scope="col">File Name</th><th scope="col">Last Modified</th><th scope="col">Size</th></tr>');
        Text.Add('</thead>');

        Text.Add('<tbody>');
        repeat
{$ifndef TSearchRecHasNoTimestamp}
            TimeStamp := FileDateToDateTime(F.Time);
{$else}
            TimeStamp := F.TimeStamp;
{$endif}
          if F.Attr and faDirectory <> 0 then
          begin
            if F.Name = '.' then
              Name := ADirName
            else if F.Name = '..' then
              Name := ExtractFileDir(ADirName)
            else
              Name := F.Name;
            Size := 'DIR';
          end
          else
          begin
            Name := F.Name;
            Size := IntToStr(F.Size);
          end;
          Text.Add(
            '<tr><th class="fn" scope="row"><a href="' + EncodeUrl(DosToHtml(Name))+'">' + Name + '</a></th>' +
            '<td class="tm">' + DateTimeToStr( TimeStamp ) + '</td><td class="sz">' + Size + '</td></tr>');
        until FindNext(F) <> 0;
        Text.Add('</tbody>');

        Text.Add('</table>');
      finally
        FindClose(F);
      end;
    end;
    Text.Add('</body>');
    Text.Add('</html>');
    Text.SaveToStream(AStream);
  finally
    Text.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtFileConnection.Get(ADoc: ThtUrlDoc);
var
  FileName, Ext : String;
  I : integer;
begin
  FileName := ADoc.Url;

  {remove any query string as it's not certain how to respond to a Form
   submit with a file protocol.  The user can add a response if desired.}
  SplitQuery(FileName);

  I :=  Pos('file://', LowerCase(FileName));
  if I > 0 then
  begin
    Delete(FileName, 1,5+2);  { remove file:// }
    { We suppose that windows accepts c:/test/test2 }
    if FileName[1] = '/' then
      Delete(FileName, 1, 1);
  end;

  FileName := HTMLtoDOS(FileName);
  if DirectoryExists(FileName) then
  begin
    MakeDirOutput(ADoc.Stream, FileName);
    ADoc.DocType := HTMLType;
  end
  else
  begin
    ADoc.Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Ext := Lowercase(ExtractFileExt(FileName));
    if Length(Ext) > 0 then
      Delete(Ext, 1, 1);
    ADoc.DocType := FileExt2DocType(Ext);
  end;
  ADoc.Status := ucsLoaded;

end;

{ ThtFileConnector }

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtFileConnector.CreateConnection(const Protocol: String): ThtConnection;
begin
  Result := ThtFileConnection.Create;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
class function ThtFileConnector.GetDefaultProtocols: string;
begin
  Result := 'file';
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
class function ThtFileConnector.GetVersion: string;
begin
  Result := 'HtmlViewer ' + VersionNo;
end;

{ ThtResourceConnection }

//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure ThtResourceConnection.Get(ADoc: ThtUrlDoc);
var
   FileName, S, Ext: String;
   HResInfo: HRSRC;
   GoodType: PChar;
   I: Integer;
begin
  FileName := ADoc.Url;

  {remove any query string as it's not certain how to respond to a Form
   submit with a file protocol.  The user can add a response if desired.}
  SplitQuery(FileName);
  { skip protocol: accept both res:// and res:/// }
  I :=  Pos('res://', LowerCase(FileName));
  if I > 0 then
  begin
    Delete(FileName, I, 4+2);
    if FileName[1] = '/' then
      Delete(FileName, 1, 1);
  end;

  Ext := LowerCase(GetURLExtension(FileName));
  ADoc.DocType := FileExt2DocType(Ext);
  case ADoc.DocType of
    XHTMLType,
    HTMLType:  GoodType := 'HTML';
    ImgType:   GoodType := PChar(UpperCase(Ext));
    TextType:  GoodType := 'TEXT';
  else
    GoodType := '';
  end;
  HResInfo := FindResource(HInstance, PChar(FileName), GoodType);
  if HResInfo = 0 then
  begin    {try without the extension if can't find it with extension}
    I := htPos('.' + Ext, LowerCase(FileName));
    if I >= 0 then
    begin
      S := FileName;
      SetLength(S, I - 1);
      HResInfo := FindResource(HInstance, PChar(S), GoodType);
      if HResInfo <> 0 then
        FileName := S;
    end;
  end;
  if HResInfo = 0 then
    // avoid Exception. ADoc.Stream = nil is okay. Next access to it will create an empty TMemoryStream.
    // raise EResNotFound.Create('Can''t find resource: '+FileName);
  else
    ADoc.Stream := TResourceStream.Create(HInstance, FileName, GoodType);
end;

{ ThtResourceConnector }

//-- BG ---------------------------------------------------------- 18.05.2016 --
function ThtResourceConnector.CreateConnection(const Protocol: String): ThtConnection;
begin
  Result := ThtResourceConnection.Create;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
class function ThtResourceConnector.GetDefaultProtocols: string;
begin
  Result := 'res';
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
class function ThtResourceConnector.GetVersion: string;
begin
  Result := 'HtmlViewer ' + VersionNo;
end;

{ ThtUrlDocLoaderThread }

//-- BG ---------------------------------------------------------- 19.05.2016 --
constructor ThtUrlDocLoaderThread.Create(Connection: ThtConnection;
  UrlDoc: ThtUrlDoc; OnLoaded: ThtUrlDocLoadedEvent; Receiver: TObject);
begin
  inherited Create(False);
  FConnection := Connection;
  FUrlDoc := UrlDoc;
  FOnLoaded := OnLoaded;
  FReceiver := Receiver;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThread.DoTerminate;
begin
  inherited;
  Synchronize(FreeConnection);
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThread.Execute;
begin
  if Terminated then
    Exit;
  UrlDoc.Status := ucsInProgress;
  try
    Connection.LoadDoc(UrlDoc);
    UrlDoc.Status := ucsLoaded;
  except
    UrlDoc.Status := ucsError;
  end;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThread.FreeConnection;
begin
  FreeAndNil(FConnection);
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThread.Loaded;
begin
  if Assigned(FOnLoaded) then
    FOnLoaded(FUrlDoc, FReceiver);
end;

{ ThtUrlDocLoaderThreadList }

//-- BG ---------------------------------------------------------- 19.05.2016 --
constructor ThtUrlDocLoaderThreadList.Create(AOwner: TComponent);
begin
  inherited;
  FWaiting := TList.Create;
  FRunning := TList.Create;
  FMaxRunningThreadCount := ChtMaxRunningThreadCount;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
destructor ThtUrlDocLoaderThreadList.Destroy;
var
  Thread: ThtUrlDocLoaderThread;
  Index: Integer;
begin
  for Index := FWaiting.Count - 1 downto 0 do
  begin
    Thread := FWaiting[Index];
    Thread.Terminate;
    if Thread.Suspended then
      Thread.Resume;
  end;

  for Index := FRunning.Count - 1 downto 0 do
  begin
    Thread := FRunning[Index];
    Thread.Terminate;
    if Thread.Suspended then
      Thread.Resume;
  end;

  while (FWaiting.Count > 0) or (FRunning.Count > 0) do
    Application.ProcessMessages; // let the threads come to an end...

  FWaiting.Free;
  FRunning.Free;

  inherited;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtUrlDocLoaderThreadList.RunningCount: Integer;
begin
  Result := FRunning.Count;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThreadList.AddLoader(Thread: ThtUrlDocLoaderThread);
begin
  if FWaiting.Count + FRunning.Count = 0 then
  begin
    FDone := 0;
    FTotal := 0;
    FFailed := 0;
  end;

  Thread.OnTerminate := Terminated;
  Thread.Priority := tpLower;
  FWaiting.add(Thread);

  Inc(FTotal);
  StartNext;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThreadList.Terminated(Sender: TObject);
var
  Thread: ThtUrlDocLoaderThread absolute Sender;
begin
  Assert(Sender is ThtUrlDocLoaderThread, Sender.ClassName + ' is not a ThtUrlDocLoaderThread');
  try
    Thread.Loaded;
  except
    // must continue, no matter what happened.
    Inc(FFailed);
  end;
  FWaiting.Remove(Sender); // paranoia or needed for termination?
  FRunning.Remove(Sender);

  Inc(FDone);
  if FWaiting.Count + FRunning.Count = 0 then
  begin
    FDone := 0;
    FTotal := 0;
  end;
  StartNext;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure ThtUrlDocLoaderThreadList.StartNext;
var
  Thread: TThread;
begin
  if Assigned(FOnProgress) then
    FOnProgress(self, FDone, FTotal);

  if (FWaiting.Count > 0) and (FRunning.Count < MaxRunningThreadCount) then
  begin
    Thread := FWaiting[0];
    FWaiting.Delete(0);
    FRunning.Add(Thread);
    Thread.Resume;
  end;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
function ThtUrlDocLoaderThreadList.WaitingCount: Integer;
begin
  Result := FWaiting.Count;
end;

end.
