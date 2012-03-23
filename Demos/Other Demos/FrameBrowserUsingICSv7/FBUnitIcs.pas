{
Version   11.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team
Copyright (c) 2012 by Angus Robertson delphi@magsys.co.uk

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

------------------------------------------------------------------------------------

This demo requires the HtmlViewer component from:

https://github.com/BerndGabriel/HtmlViewer or
http://code.google.com/p/thtmlviewer/

which must also be downloaded and installed before the demo can be built.

It also needs ICS v7 dated 21st March 2012 or later from:

http://wiki.overbyte.be/wiki/index.php/ICS_Download


19 March 2012 - Angus Robertson replaced Indy with ICSv7 (based on FBUnitIndy)
Added new diagnostic window showing all HTTP traffic and non-display HTML
Save window sizes and positions
Update UrlComboBox with each page visited
New settings to stop caching pages (initially off) and images (on)
Fixed bug with HotSpotTargetClick not always using a full URL
Save a few more file types as binary downloads rather than displaying them

21 March 2012 - fixed Unicode bugs with D2009 and later

22 March 2012 - added new setting to log HTML page code for diagnostics

23 March 2012 - added HTTP GZIP compression support




Pending - use NoCache header to stop dynamic pages being cached


Fixed bug in FramBrwz.pas with links not normalised
Fixed bug in htmlview.pas with Referrer having application file path added to front
Fixed bug in ReadHTML.pas handle meta without http-equiv="Content-Type" <meta charset="utf-8"> from HTML5
}

unit FBUnitIcs;

{$include htmlcons.inc}
{$WARN UNIT_PLATFORM OFF}

{A program to demonstrate the TFrameBrowser component}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI, Menus, StdCtrls, Buttons, ExtCtrls, Gauges, mmSystem, IniFiles, MPlayer,
  ImgList, ComCtrls, ToolWin,
  htmlun2, CachUnitId, URLSubs, htmlview, htmlsubs, FramBrwz, FramView,
  PreviewForm, DownLoadId, Readhtml, urlconIcs,
  OverbyteIcsWndControl, OverbyteIcsWsocket, OverbyteIcsHttpProt, OverbyteIcsCookies,
  {$if CompilerVersion >= 15}
    {$IF CompilerVersion < 23}
    XpMan,
    {$ifend}
  {$ifend}
  HtmlGlobals;

const
  (*UsrAgent = 'Mozilla/4.0 (compatible; ICS Library)';*)
  UsrAgent = 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98)';
  MaxHistories = 15;  {size of History list}
  wm_LoadURL = wm_User+124;
  wm_DownLoad = wm_User+125;

type
  ImageRec = class(TObject)
    Viewer: ThtmlViewer;
    ID, URL: string;
    Stream: TMemorystream;
  end;

  TImageHTTP = class(TComponent)
  {an HTTP component with a few extra fields}
  public
    ImRec: ImageRec;
    Url: String;
    Connection : TURLConnection;
    constructor CreateIt(AOwner: TComponent; IRec: TObject);
    destructor Destroy; override;
    procedure GetAsync;
  end;

  THTTPForm = class(TForm)
    MainMenu1: TMainMenu;
    HistoryMenuItem: TMenuItem;
    Help1: TMenuItem;
    File1: TMenuItem;
    Openfile1: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel2: TPanel;
    Status1: TPanel;
    Status3: TPanel;
    Status2: TPanel;
    Timer: TTimer;
    Options1: TMenuItem;
    DeleteCache1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ShowImages: TMenuItem;
    SaveDialog: TSaveDialog;
    Edit1: TMenuItem;
    Find1: TMenuItem;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    N2: TMenuItem;
    FindDialog: TFindDialog;
    PopupMenu: TPopupMenu;
    SaveImageAs: TMenuItem;
    N3: TMenuItem;
    OpenInNewWindow: TMenuItem;
    FrameBrowser: TFrameBrowser;
    PrintPreview: TMenuItem;
    Print1: TMenuItem;
    PrintDialog: TPrintDialog;
    Proxy1: TMenuItem;
    DemoInformation1: TMenuItem;
    About1: TMenuItem;
    Timer1: TTimer;
    CoolBar1: TCoolBar;
    ToolBar2: TToolBar;
    BackButton: TToolButton;
    FwdButton: TToolButton;
    ToolButton1: TToolButton;
    ReloadButton: TToolButton;
    UrlComboBox: TComboBox;
    Panel10: TPanel;
    ToolBar1: TToolBar;
    CancelButton: TToolButton;
    SaveUrl: TToolButton;
    ImageList1: TImageList;
    Panel3: TPanel;
    Animate1: TAnimate;
    Gauge: TProgressBar;
    SslContext: TSslContext;
    ShowDiagWindow: TMenuItem;
    IcsCookies: TIcsCookies;
    CachePages: TMenuItem;
    CacheImages: TMenuItem;
    ShowLogHTML: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure FwdButtonClick(Sender: TObject);
    procedure Openfile1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure DeleteCacheClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ShowImagesClick(Sender: TObject);
    procedure ReloadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure URLComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure SaveImageAsClick(Sender: TObject);
    procedure URLComboBoxClick(Sender: TObject);
    procedure RightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure SaveURLClick(Sender: TObject);
    procedure HTTPDocData1(Sender: TObject; Buffer : Pointer; Len: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Status2Resize(Sender: TObject);
    procedure Processing(Sender: TObject; ProcessingOn: Boolean);
    procedure PrintPreviewClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrintHeader(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean);
    procedure PrintFooter(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean);
    procedure About1Click(Sender: TObject);
    procedure ViewerClear(Sender: TObject);
    procedure Proxy1Click(Sender: TObject);
    procedure DemoInformation1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FrameBrowserMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$ifdef UNICODE}
    procedure BlankWindowRequest(Sender: TObject; const Target, URL: String);
    procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, RefererX: String; Reload: Boolean;
      var NewURL: String; var DocType: ThtmlFileType;
      var Stream: TMemoryStream);
    procedure GetImageRequest(Sender: TObject; const URL: String; var Stream: TStream);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: String; var Handled: Boolean);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: String);
    procedure FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: string);
    procedure FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: string);
{$else}
    procedure BlankWindowRequest(Sender: TObject; const Target, URL: WideString);
    procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, RefererX: WideString; Reload: Boolean;
      var NewURL: WideString; var DocType: ThtmlFileType;
      var Stream: TMemoryStream);
    procedure GetImageRequest(Sender: TObject; const URL: WideString; var Stream: TStream);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: WideString; var Handled: Boolean);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: WideString);
    procedure FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: WideString);
    procedure FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: WideString);
{$endif}
    procedure ShowDiagWindowClick(Sender: TObject);
    procedure LogLine (S: string);
    procedure IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie; var Save: Boolean);
    procedure CachePagesClick(Sender: TObject);
    procedure CacheImagesClick(Sender: TObject);
    procedure ShowLogHTMLClick(Sender: TObject);
  private
    { Private declarations }
    URLBase: String;
    Histories: array[0..MaxHistories-1] of TMenuItem;
    Pending: TList;       {a list of ImageRecs}
    HTTPList: TList;         {a list of the Image HTTPs currently existing}
    DiskCache: TDiskCache;
    NewLocation: string;
    ARealm: string;
    CurrentLocalFile,
    DownLoadUrl: string;
    Reloading: boolean;
    FoundObject: TImageObj;
    FoundObjectName: string;
    NewWindowFile: string;
    AnAbort: boolean;
    NumImageTot, NumImageDone: integer;
    AStream: TMemoryStream;
    Connection: TURLConnection;
    Proxy: string;
    ProxyPort: string;
    ProxyUser: string;
    ProxyPassword: string;
    TimerCount: integer;
    OldTitle: ThtString;
    HintWindow: ThtHintWindow;
    HintVisible: boolean;
    TitleViewer: ThtmlViewer;
    Allow: string;

    procedure EnableControls;
    procedure DisableControls;
    procedure ImageRequestDone(Sender: TObject; RqType: THttpRequest; Error: Word);
    procedure HistoryClick(Sender: TObject);
    procedure WMLoadURL(var Message: TMessage); message WM_LoadURL;
    procedure WMDownLoad(var Message: TMessage); message WM_DownLoad;
    procedure CheckEnableControls;
    procedure ClearProcessing;
    procedure Progress(Num, Den: integer);
    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
    procedure CheckException(Sender: TObject; E: Exception);
    procedure HTTPRedirect(Sender: TObject);
    procedure HTTPSetCookie(Sender: TObject; const Data: String; var Accept: Boolean);
    procedure CloseHints;
  public
    { Public declarations }
    FIniFilename: string;
  end;

  EContentTypeError = class(Exception);
  ESpecialException = class(Exception);

const
  CookieFile = 'Cookies.txt';
var
  HTTPForm: THTTPForm;
  Mon: TextFile;
  Monitor1: boolean;
  Cache: string;

implementation

uses HTMLAbt, ProxyDlg, AuthUnit, LogWin;

{$R fbHelp32.res}
{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$if CompilerVersion < 15}
    {$R manifest.res}
  {$ifend}
{$endif}

procedure StartProcess(CommandLine: string; ShowWindow: Word);
var
  si: _STARTUPINFO;
  pi: _PROCESS_INFORMATION;
begin
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := ShowWindow;
  UniqueString(CommandLine);
  if CreateProcess(nil, PChar(CommandLine), nil, nil, False, 0, nil, nil, si, pi) then
  begin
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  end;
end;

{----------------THTTPForm.FormCreate}
procedure THTTPForm.FormCreate(Sender: TObject);
var
  I, J: integer;
  IniFile: TIniFile;
  SL: TStringList;
  S: string;
begin
Top := Top div 2;
if Screen.Width <= 800 then   {make window fit appropriately}
  begin
  Left := Left div 2;
  Width := (Screen.Width * 9) div 10;
  Height := (Screen.Height * 7) div 8;
  end
else
  begin
  Width := 850;
  Height := 600;
  end;

Cache := ExtractFilePath(Application.ExeName)+'Cache\';
DiskCache := TDiskCache.Create(Cache);
Status1.Caption := '';

{Monitor1 will be set if this is the first instance opened}
Monitor1 := True;
try
  AssignFile(Mon, Cache+'Monitor.txt');  {a monitor file for optional use}
  Rewrite(Mon);
except
  Monitor1 := False;   {probably open in another instance}
  end;

{$ifdef LogIt}
if Monitor1 then
  begin
  DeleteFile(Cache+'LogFile.txt');
  Log := TModIdLogFile.Create(Self);
  Log.Filename := Cache+'LogFile.txt';
  Log.ReplaceCRLF := False;
  Log.Active := True;
  end;
{$endif}
IcsCookies.LoadFromFile(Cache+CookieFile);
IcsCookies.AutoSave := true;
AStream := TMemoryStream.Create;
Pending := TList.Create;
HTTPList := TList.Create;

FrameBrowser.HistoryMaxCount := MaxHistories;  {defines size of history list}

for I := 0 to MaxHistories-1 do
  begin      {create the MenuItems for the history list}
  Histories[I] := TMenuItem.Create(HistoryMenuItem);
  HistoryMenuItem.Insert(I, Histories[I]);
  with Histories[I] do
    begin
    Visible := False;
    OnClick := HistoryClick;
    Tag := I;
    end;
  end;

{Load animation from resource}
Animate1.ResName := 'StarCross';

{$ifdef ver140}   {delphi 6}
URLComboBox.AutoComplete := False;
{$endif}

I := Pos('.', Application.ExeName);
FIniFilename := Copy(Application.Exename, 1, I)+'ini';

ProxyPort := '80';
IniFile := TIniFile.Create(FIniFileName);
try
  Top := IniFile.ReadInteger('HTTPForm', 'Top', Top);
  Left := IniFile.ReadInteger('HTTPForm', 'Left',   Left);
  Width := IniFile.ReadInteger('HTTPForm', 'Width',  Width);
  Height := IniFile.ReadInteger('HTTPForm', 'Height', Height);
  ShowDiagWindow.Checked := IniFile.ReadBool('HTTPForm', 'ShowDiagWindow', ShowDiagWindow.Checked);
  CachePages.Checked := IniFile.ReadBool('HTTPForm', 'CachePages', CachePages.Checked);
  CacheImages.Checked := IniFile.ReadBool('HTTPForm', 'CacheImages', CacheImages.Checked);
  ShowLogHTML.Checked := IniFile.ReadBool('HTTPForm', 'ShowLogHTML', ShowLogHTML.Checked);
  Proxy := IniFile.ReadString('Proxy', 'ProxyHost', '');
  ProxyPort := IniFile.ReadString('Proxy', 'ProxyPort', '80');
  ProxyUser := IniFile.ReadString('Proxy', 'ProxyUsername', '');
  ProxyPassword := IniFile.ReadString('Proxy', 'ProxyPassword', '');
  SL := TStringList.Create;
  try
    IniFile.ReadSectionValues('favorites', SL);
    for I := 0 to SL.Count-1 do
      begin
      J := Pos('=', SL[I]);
      S := Copy(SL[I], J+1, 1000);
      if UrlCombobox.Items.IndexOf(S) < 0 then
          UrlCombobox.Items.Add(S);
      end;
  finally
    SL.Free;
    end;
finally
  IniFile.Free;
  end;
DragAcceptFiles(Handle, True);
Application.OnException := CheckException;

{$ifdef M4Viewer}
ProtocolHandler := M4ProtocolHandler;
{$endif}
HintWindow := ThtHintWindow.Create(Self);
HintWindow.Color := $C0FFFF;
end;

{----------------THTTPForm.FormDestroy}
procedure THTTPForm.FormDestroy(Sender: TObject);
var
  I: integer;
  IniFile: TIniFile;
begin
AStream.Free;
Pending.Free;
for I := 0 to HTTPList.Count-1 do
  TImageHTTP(HTTPList.Items[I]).Free;
HTTPList.Free;
if Monitor1 then
  CloseFile(Mon);
DiskCache.Free;
//CookieManager.Free;
if Assigned(Connection) then
  begin
  Connection.Free;
  Connection := Nil;
  end;
if Monitor1 then
  begin       {save only if this is the first instance}
  IniFile := TIniFile.Create(FIniFileName);
  try
    IniFile.WriteInteger('HTTPForm', 'Top', Top);
    IniFile.WriteInteger('HTTPForm', 'Left', Left);
    IniFile.WriteInteger('HTTPForm', 'Width', Width);
    IniFile.WriteInteger('HTTPForm', 'Height', Height);
    IniFile.WriteBool('HTTPForm', 'ShowDiagWindow', ShowDiagWindow.Checked);
    IniFile.WriteBool('HTTPForm', 'CachePages', CachePages.Checked);
    IniFile.WriteBool('HTTPForm', 'CacheImages', CacheImages.Checked);
    IniFile.WriteBool('HTTPForm', 'ShowLogHTML', ShowLogHTML.Checked);
    IniFile.WriteString('Proxy', 'ProxyHost', Proxy);
    IniFile.WriteString('Proxy', 'ProxyPort', ProxyPort);
    IniFile.WriteString('Proxy', 'ProxyUsername', ProxyUser);
    IniFile.WriteString('Proxy', 'ProxyPassword', ProxyPassword);
    IniFile.EraseSection('Favorites');
    for I := 0 to UrlCombobox.Items.Count - 1 do
      IniFile.WriteString('Favorites', 'Url'+IntToStr(I), UrlCombobox.Items[I]);
  finally
    IniFile.Free;
    end;
  end;
end;

procedure THTTPForm.LogLine (S: string);
begin
    if NOT ShowDiagWindow.Checked then exit;
    if NOT LogForm.Visible then LogForm.Visible := true;    
    LogForm.LogMemo.Lines.Add (S);
end;

{----------------THTTPForm.GetButtonClick}
procedure THTTPForm.GetButtonClick(Sender: TObject);
{initiate loading of a main document}
begin
DisableControls;
Status1.Caption := '';
Status2.Caption := '';
try
  {the following initiates one or more GetPostRequest's}
  URLCombobox.Text := Normalize(URLCombobox.Text);
  URLBase := GetUrlBase(URLCombobox.Text);
  FrameBrowser.LoadURL(URLCombobox.Text);
  Reloading := False;
finally
  CheckEnableControls;
  FrameBrowser.SetFocus;
  end;
end;

{----------------THTTPForm.FrameBrowserGetPostRequestEx}
procedure THTTPForm.FrameBrowserGetPostRequestEx(Sender: TObject;
  IsGet: Boolean; const URL, Query, EncType, RefererX: ThtString;
  Reload: Boolean; var NewURL: ThtString; var DocType: ThtmlFileType;
  var Stream: TMemoryStream);
{OnGetPostRequest handler.
 URL is what to load.
 IsGet is set for Get (as opposed to Post)
 Query a possible query string
 Reload set if don't want what's in cache
 NewURL return in the document location has changed (happens quite frequently).
 DocType is the type of document found -- HTMLType, TextType, or ImgType
 Stream is the stream answer to the request}
const
  MaxRedirect = 15;
var
  S, URL1, FName, Query1, LastUrl: string;
  AnsiQuery: AnsiString;
  Error, TryAgain, TryRealm: boolean;
  RedirectCount: integer;

  function GetAuthorization: boolean;
  var
    UName, PWord: string;
  begin
  with AuthForm do
    begin
    Result := GetAuthorization(TryRealm and (ARealm <> ''), ARealm, UName, PWord);
    TryRealm := False;    {only try it once}
    if Result then
      begin
      Connection.UserName := UName;
      Connection.Password := PWord;
      Connection.InputStream.Clear;   {delete any previous message sent}
      Connection.BasicAuth := True;
      end;
    end;
  end;

begin
CloseHints;   {may be a hint window open}
Query1 := Query;
Status1.Caption := '';
Status2.Caption := '';
FName := '';
Error := False;
AnAbort := False;
NumImageTot := 0;
NumImageDone := 0;
Progress(0, 0);
Gauge.Visible := True;
URL1 := Normalize(URL);
URLBase := GetUrlBase(URL1);
DisableControls;
NewLocation := '';     {will change if document referenced by URL has been relocated}
ARealm := '';
TryRealm := True;
AStream.Clear;
RedirectCount := 0;
if Reloading or Reload or (NOT CachePages.Checked) then
  begin     {don't want a cache file}
  DiskCache.RemoveFromCache(URL1);
  end
else
  DiskCache.GetCacheFilename(URL1, FName, DocType, NewLocation);  {if in cache already, get the cache filename}
if (FName = '') or not FileExists(FName) then
  begin       {it's not in cache}
  If Connection <> nil Then
    begin
    Connection.Free;
    Connection := Nil;
    end ;
  Connection := TURLConnection.GetConnection(URL1);
  if connection <> nil then
    begin
    Connection.OnDocData := HTTPForm.HTTPDocData1;  // progress only
    Connection.Referer := RefererX;
    LastURL := Url1;
    Connection.OnRedirect := HTTPForm.HTTPRedirect;
    Connection.OnCookie := HTTPForm.HTTPSetCookie;
    Connection.Proxy := Proxy;
    Connection.ProxyPort := ProxyPort;
    Connection.ProxyUser := ProxyUser;
    Connection.ProxyPassword := ProxyPassword;
    Connection.UserAgent := UsrAgent;
    Connection.Cookie := IcsCookies.GetCookies (Url1);
    try
      repeat
        TryAgain := False;
        Inc(RedirectCount);
        if Assigned(Connection.InputStream) then
          Connection.InputStream.Clear;
        try
          if IsGet then begin       {Get}
            if Query1 <> '' then begin
              LogLine ('FrameBrowser Get: ' + URL1+'?'+Query1);
              Connection.Get(URL1+'?'+Query1);
            end
            else begin
              LogLine ('FrameBrowser Get: ' + URL1);
              Connection.Get(URL1);
            end;
          end
          else begin      {Post}
            Connection.SendStream := TMemoryStream.Create;
            try
              LogLine ('FrameBrowser Post: ' + URL1+', Data=' +
                  Copy (Query1, 1, 132) + ', EncType=' + EncType);  // not too much data
              AnsiQuery := AnsiString (Query1);
              Connection.SendStream.WriteBuffer(AnsiQuery[1], Length(AnsiQuery));
              Connection.SendStream.Position := 0;
              if EncType = '' then
                Connection.ContentTypePost := 'application/x-www-form-urlencoded'
              else
                Connection.ContentTypePost := EncType;
              Connection.Post(URL1);
            finally
              Connection.SendStream.Free;
              end;
            end;
          if Connection.StatusCode = 401 then begin
            ARealm := Connection.Realm;
            TryAgain := GetAuthorization;
          end;
        except
          case Connection.StatusCode of
            401: begin
                  TryAgain := GetAuthorization;
                end;
            405:if not IsGet and (Pos('get', Allow) > 0) then
                  begin
                  IsGet := True;
                  TryAgain := True;
                  end;
            301, 302:
              if NewLocation <> '' then
                begin
                URL1 := NewLocation;
                TryAgain := True;
                end;
            end;
          if not TryAgain or (RedirectCount >= MaxRedirect) then
            Raise;
          end;
          LogLine ('FrameBrowserGetPostRequestEx Done: Status ' + IntToStr (Connection.StatusCode));
      until not TryAgain;

      DocType := Connection.ContentType;
      AStream.LoadFromStream(Connection.InputStream);
    except
      On ESpecialException do
        begin
        LogLine ('FrameBrowserGetPostRequestEx: Special Exception');
        Connection.Free;   {needs to be reset}
        Connection := Nil;
        Raise;
        end;
      On E: Exception do
        try
          LogLine ('FrameBrowserGetPostRequestEx: Exception - ' + E.Message);
          if AnAbort then
            Raise(ESpecialException.Create('Abort on user request'));

          Error := True;
          if Connection is THTTPConnection then
            with Connection do
              begin
              if InputStream.Size > 0 then   {sometimes error messages come in RcvdStream}
                AStream.LoadFromStream(InputStream)
              else
                begin
                if RedirectCount >= MaxRedirect then
                  S := 'Excessive Redirects'
                else S := ReasonPhrase+'<p>Statuscode: '+IntToStr(StatusCode);
                LogLine (S);
                AStream.Write(S[1], Length(S));
                end;
              DocType := HTMLType;
              end
          else  {other connection types}
            begin
            S := E.Message;          {Delphi 1}
            AStream.Write(S[1], Length(S));
            LogLine (S);
            end;
        finally
          Connection.Free;   {needs to be reset}
          Connection := Nil;
        end;
    end;
    Status1.Caption := 'Received ' + IntToStr(AStream.Size) + ' bytes';
    if CachePages.Checked then
    begin
        FName := DiskCache.AddNameToCache(LastUrl, NewLocation, DocType, Error);
        if FName <> '' then
          try
            AStream.SaveToFile(FName);   {it's now in cache}
          except
          end;
    end;
  end
  else
    begin        { unsupported protocol }
    S := 'Unsupported protocol: ' + URL1 ; //  GetProtocol(URL1);
 //   if Sender is TFrameBrowser then   {main document display}
 //     Raise(ESpecialException.Create(S))
 //   else   {else other errors will get displayed as HTML file}
      AStream.Write(S[1], Length(S));
      LogLine (S);
    end;
  end
else
  begin
  AStream.LoadFromFile(FName);
  LogLine ('FrameBrowserGetPostRequestEx: from Cache' + FName);
  end;
NewURL := NewLocation;   {in case location has been changed}
if ShowLogHTML.Checked then begin
  AStream.Position := 0;
  SetLength (AnsiQuery, AStream.Size);
  AStream.ReadBuffer(AnsiQuery[1], AStream.Size);
  LogLine (String (AnsiQuery));
end;
Stream := AStream;
end;

procedure THTTPForm.HTTPRedirect(Sender: TObject);
var
  Proto, Dest: string;
begin
Dest := (Sender as TSslHttpCli).Location;
LogLine ('Redirected to: ' + Dest);
Proto := GetProtocol(UrlBase);
if IsFullUrl(Dest) then  {it's a full URL}
  begin
  NewLocation:= Normalize(Dest);
  end
else
  begin
  NewLocation := CombineURL(URLBase, Dest);
  end;
URLBase := GetUrlBase(NewLocation);

// cookies may have been sent during redirection, so update again now
(Sender as TSslHttpCli).Cookie := IcsCookies.GetCookies (NewLocation);
end;

procedure THTTPForm.HTTPSetCookie(Sender: TObject; const Data: String; var Accept: Boolean);
begin
  IcsCookies.SetCookie (Data, (Sender as TSslHttpCli).Url);
end;

procedure THTTPForm.IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie; var Save: Boolean);
var
  S: string;
begin
  with ACookie do begin
      S := 'NewCookie: ' + CName + '=' + CValue + ', Domain=' + CDomain + ', Path=' + CPath ;
      if CPersist then
        S := S + ', Expires=' + DateTimeToStr (CExpireDT)
      else
        S := S + ', Not Persisent';
      if CSecureOnly then S := S + ', SecureOnly';
      if CHttpOnly then S := S + ', HttpOnly';
      LogLine (S);
  end;
end;

{----------------THTTPForm.GetImageRequest}
procedure THTTPForm.GetImageRequest(Sender: TObject; const URL: ThtString; var Stream: TStream);
{the OnImageRequest handler}
var
  S: string;
  IR: ImageRec;
  DocType: ThtmlFileType;
  ImHTTP: TImageHTTP;
  Dummy: string;
begin
if Reloading or (NOT CacheImages.Checked) then
  begin
  DiskCache.RemoveFromCache(URL);
  S := '';
  end
else
  DiskCache.GetCacheFilename(URL, S, DocType, Dummy);  {see if image is in cache file already}
if FileExists(S) and (DocType = ImgType) then
  begin                               {yes, it is}
  AStream.LoadFromFile(S);
  Stream := AStream;      {return image immediately}
  LogLine ('GetImageRequest, from Cache: ' + S);
  end
else if not ((GetProtocol(URL) = 'http')
       or (GetProtocol(URL) = 'https')) then
  begin
  if Assigned(Connection) then
    Connection.Free;
  Connection := TURLConnection.GetConnection(URL);
  if connection <> nil then
    begin
    LogLine ('GetImageRequest Start: ' + URL);
    try
      Connection.Get(URL);
      Stream := Connection.InputStream;
    except
      Stream := Nil;
      end;
    end
  else Stream := Nil;
  end
else
  begin          {http protocol, the image will need to be downloaded}
  Stream := WaitStream;   {wait indicator}
  IR := ImageRec.Create;
  IR.Viewer := Sender as ThtmlViewer;
  IR.URL := URL;
  IR.ID := URL;

  ImHTTP := TImageHTTP.CreateIt(Self, IR);
  ImHTTP.Connection.Owner := ImHTTP;
  ImHTTP.Connection.OnCookie := HTTPForm.HTTPSetCookie;
  ImHTTP.Connection.OnRequestDone := ImageRequestDone;
  ImHTTP.Connection.Proxy := Proxy;
  ImHTTP.Connection.ProxyPort := ProxyPort;
  ImHTTP.Connection.ProxyUser := ProxyUser;
  ImHTTP.Connection.ProxyPassword := ProxyPassword;
  ImHTTP.Connection.UserAgent := UsrAgent;
  ImHTTP.Connection.Cookie := IcsCookies.GetCookies (URL);
  DisableControls;
  try
    ImHTTP.GetAsync;  {This call does not stall. When done getting the image,
                       processing will continue at ImageRequestDone}
    HTTPList.Add(ImHTTP);
  except
    ImHTTP.Free;
    Stream := Nil;
    end;

  Inc(NumImageTot);
  Progress(NumImageDone, NumImageTot);
  end;
end;

{----------------THTTPForm.ImageRequestDone}

procedure THTTPForm.ImageRequestDone(Sender: TObject; RqType: THttpRequest;
  Error: Word);
{arrive here when ImHTTP.GetAsync has completed}
var
  ImHTTP: TImageHTTP;
begin
try
  ImHTTP := (Sender as TImageHTTP);
  if (RqType = httpGet) then
    begin
    if (Error = 0) then
      begin   {Add the image record to the Pending list to be inserted in the timer
               loop at TimerTimer}
      LogLine ('GetImageRequest Done OK');
      Pending.Add(ImHTTP.ImRec);
      ImHTTP.ImRec := Nil;  {so it won't get free'd below}
      Timer.Enabled := True;
      end
    else
      {this code will cause Error Image to appear if error occured when downloading image}
      begin   {Add the image record to the Pending list to be inserted in the timer
               loop at TimerTimer}
      Pending.Add(ImHTTP.ImRec);
      FreeAndNil(ImHTTP.ImRec.Stream);
      ImHTTP.ImRec := Nil;  {so it won't get free'd below}
      Timer.Enabled := True;
      end;
      (*Optional code to just ignore the error
      begin   {an error occured, forget this image}
      Inc(NumImageDone);
      Progress(NumImageDone, NumImageTot);
      end; *)
    end;
  HTTPList.Remove(ImHTTP);
  ImHTTP.Free;
  if NumImageDone >= NumImageTot then
    CheckEnableControls;
except
  end;
end;

{----------------THTTPForm.TimerTimer}
procedure THTTPForm.TimerTimer(Sender: TObject);
{Images cannot necessarily be inserted at the moment they become available since
 the system may be busy with other things.  Hence they are added to a Pending
 list and this timing loop repeatedly trys to insert them}
var
  FName: String;
begin
Timer.Enabled := False;
if Pending.Count > 0 then
  with ImageRec(Pending[0]) do
    if FrameBrowser.InsertImage(Viewer, ID, Stream) then
      begin
      if Assigned(Stream) then  {Stream can be Nil}
        begin
        {save image in cache file}
        if CacheImages.Checked then
        begin
            FName := DiskCache.AddNameToCache(URL, '', ImgType, False);
            if FName <> '' then
              try
                Stream.SaveToFile(FName);
              except
              end;
        end;
        Stream.Free;
        end;
      Pending.Delete(0);
      Free;
      Inc(NumImageDone);
      Progress(NumImageDone, NumImageTot);
      end;
if (Pending.Count > 0) or (HTTPList.Count > 0) then
  Timer.Enabled := True;   {still have images to process}
CheckEnableControls;
end;

procedure THTTPForm.CacheImagesClick(Sender: TObject);
begin
  CacheImages.Checked := NOT CacheImages.Checked;
end;

procedure THTTPForm.CachePagesClick(Sender: TObject);
begin
    CachePages.Checked := NOT CachePages.Checked;
end;

{----------------THTTPForm.CancelButtonClick}
procedure THTTPForm.CancelButtonClick(Sender: TObject);
begin
AnAbort := True;
If Assigned(Connection) then
  Connection.Abort;
ClearProcessing;
CheckEnableControls;
end;

{----------------THTTPForm.HistoryChange}
procedure THTTPForm.HistoryChange(Sender: TObject);
{OnHistoryChange handler -- history list has changed}
var
  I: integer;
  Cap: string;
begin
with Sender as TFrameBrowser do
  begin
   if History.Count > 0 then
       URLCombobox.Text := History [0]; // ANGUS
  {check to see which buttons are to be enabled}
  FwdButton.Enabled := FwdButtonEnabled;
  BackButton.Enabled := BackButtonEnabled;

  {Enable and caption the appropriate history menuitems}
  HistoryMenuItem.Visible := History.Count > 0;
  for I := 0 to MaxHistories-1 do
    with Histories[I] do
      if I < History.Count then
        Begin
        Cap := History.Strings[I];  {keep local file name}
        if TitleHistory[I] <> '' then
          Cap := Cap + '--' + TitleHistory[I];
        Caption := copy(Cap, 1, 80);
        Visible := True;
        Checked := I = HistoryIndex;
        end
      else Histories[I].Visible := False;
  Caption := DocumentTitle;    {keep the Form caption updated}
  FrameBrowser.SetFocus;
  end;
end;

{----------------THTTPForm.HistoryClick}
procedure THTTPForm.HistoryClick(Sender: TObject);
{A history list menuitem got clicked on}
var
  I: integer;
begin
  {Changing the HistoryIndex loads and positions the appropriate document}
I := (Sender as TMenuItem).Tag;
URLBase := GetUrlBase(FrameBrowser.History.Strings[I]); {update URLBase for new document}
FrameBrowser.HistoryIndex := I;
URLCombobox.Text := FrameBrowser.History [I]; // ANGUS
end;

{----------------THTTPForm.WMLoadURL}
procedure THTTPForm.WMLoadURL(var Message: TMessage);
begin
GetButtonClick(Self);
end;

{----------------THTTPForm.Openfile1Click}
procedure THTTPForm.Openfile1Click(Sender: TObject);
{Open a local disk file}
begin
if CurrentLocalFile <> '' then
  OpenDialog.InitialDir := ExtractFilePath(CurrentLocalFile)
else OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
OpenDialog.FilterIndex := 1;
OpenDialog.Filename := '';
if OpenDialog.Execute then
  begin
  UrlComboBox.Text := 'file:///'+DosToHTMLNoSharp(OpenDialog.Filename);
  GetButtonClick(Nil);
  Caption := FrameBrowser.DocumentTitle;
  CurrentLocalFile := FrameBrowser.CurrentFile;
  end;
end;

{----------------THTTPForm.HotSpotTargetCovered}
procedure THTTPForm.HotSpotTargetCovered(Sender: TObject; const Target, URL: ThtString);
{mouse moved over or away from a hot spot.  Change the status line}
begin
if URL = '' then
  Status3.Caption := ''
else if Target <> '' then
  Status3.Caption := 'Target: '+Target+'  URL: '+URL
else
  Status3.Caption := 'URL: '+URL
end;

{----------------THTTPForm.HotSpotTargetClick}
procedure THTTPForm.HotSpotTargetClick(Sender: TObject; const Target, URL: ThtString; var Handled: Boolean);
{a link was clicked.  URL is a full url here have protocol and path added.  If
 you need the actual link string, it's available in the TFrameBrowser URL property}
const
  snd_Async = $0001;  { play asynchronously }
var
  Protocol, Ext, FullUrl: string;
  PC: array[0..255] of char;
  S, Params: string;
  K: integer;
  Tmp: string;
begin
LogLine ('HotSpotTargetClick: ' + URL);
if IsFullURL(URL) then  // ANGUS not always a full URL...
    FullUrl := URL
  else
    FullUrl := CombineURL(URLBase, URL);
Protocol := GetProtocol(FullUrl);
if Protocol = 'mailto' then
  begin
  Tmp := FullUrl + #0;  {for Delphi 1}
  {call mail program}
  {Note: ShellExecute causes problems when run from Delphi 4 IDE}
  ShellExecute(Handle, nil, @Tmp[1], nil, nil, SW_SHOWNORMAL);
  Handled := True;
  Exit;
  end;
{Note: it would be nice to handle ftp protocol here also as some downloads use
 this protocol}

Ext := Lowercase(GetURLExtension(FullUrl));
if Pos('http', Protocol) > 0 then
  begin
  if (CompareText(Ext, 'zip') = 0) or (CompareText(Ext, 'exe') = 0) or
      (CompareText(Ext, 'pdf') = 0) or (CompareText(Ext, 'iso') = 0) or
       (CompareText(Ext, 'doc') = 0) or (CompareText(Ext, 'xls') = 0) then
    begin
    {download can't be done here.  Post a message to do it later at WMDownload}
    DownLoadURL := FullUrl;
    PostMessage(handle, wm_DownLoad, 0, 0);
    Handled := True;
    Exit;
    end;
  end;
if (Protocol = 'file') then
  begin
  S := FullUrl;
  K := Pos(' ', S);     {look for parameters}
  if K = 0 then K := Pos('?', S);  {could be '?x,y' , etc}
  if K > 0 then
    begin
    Params := Copy(S, K+1, 255); {save any parameters}
    setlength(S, K-1);            {truncate S}
    end
  else Params := '';
  S := HTMLToDos(S);
  if Ext = 'wav' then
    begin
    Handled := True;
    sndPlaySound(StrPCopy(PC, S), snd_ASync);
    end
  else if Ext = 'exe' then
    begin
    Handled := True;
    StartProcess(StrPCopy(PC, S+' '+Params), sw_Show);
    end
  else if (Ext = 'mid') or (Ext = 'avi')  then
    begin
    Handled := True;
    StartProcess(StrPCopy(PC, 'MPlayer.exe /play /close '+S), sw_Show);
    end;
  {else ignore other extensions}
  end;
end;

procedure THTTPForm.CheckEnableControls;
begin
if not FrameBrowser.Processing and
      (not Assigned(Connection) or (Connection.State in [httpReady, httpNotConnected])) and
      (HTTPList.Count = 0) then
  begin
  EnableControls;
  Status2.Caption   := 'DONE';
  end;
end;

procedure THTTPForm.ClearProcessing;
var
  I: integer;
begin
for I := 0 to HTTPList.Count-1 do   {free any Image HTTPs existing}
  with TImageHTTP(HTTPList.Items[I]) do
    begin
    Free;
    end;
HTTPList.Clear;
for I := 0 to Pending.Count-1 do
  begin
  ImageRec(Pending.Items[I]).Stream.Free;
  ImageRec(Pending.Items[I]).Free;
  end;
Pending.Clear; 
end;

procedure THTTPForm.ViewerClear(Sender: TObject);
{A ThtmlViewer is about to be cleared. Cancel any image processing destined for it}
var
  I: integer;
  Vw: ThtmlViewer;
begin
Vw := Sender as ThtmlViewer;
for I := HTTPList.Count-1 downto 0 do
  with TImageHTTP(HTTPList.Items[I]) do
    if ImRec.Viewer = Vw then
      begin
      Free;
      HTTPList.Delete(I);
      end;
for I := Pending.Count-1 downto 0 do
  with ImageRec(Pending.Items[I]) do
    if Viewer = Vw then
      begin
      Stream.Free;
      Free;
      Pending.Delete(I);
      end;
end;

procedure THTTPForm.DeleteCacheClick(Sender: TObject);
begin
DiskCache.EraseCache;
end;

procedure THTTPForm.wmDropFiles(var Message: TMessage);
{handles dragging of file into browser window}
var
  S: string;
  Count: integer;
begin
Count := DragQueryFile(Message.WParam, 0, @S[1], 200);
SetLength(S, Count);
DragFinish(Message.WParam);
if Count >0 then
  begin
  UrlComboBox.Text := 'file:///'+DosToHTMLNoSharp(S);
  GetButtonClick(Nil);
  CurrentLocalFile := FrameBrowser.CurrentFile;
  end;
Message.Result := 0;
end;

procedure THTTPForm.Exit1Click(Sender: TObject);
begin
Close;
end;

procedure THTTPForm.ShowDiagWindowClick(Sender: TObject);
begin
ShowDiagWindow.Checked := NOT ShowDiagWindow.Checked;
LogForm.Visible := ShowDiagWindow.Checked;
end;

procedure THTTPForm.ShowImagesClick(Sender: TObject);
begin
FrameBrowser.ViewImages := not FrameBrowser.ViewImages;
ShowImages.Checked := FrameBrowser.ViewImages;
end;

procedure THTTPForm.ShowLogHTMLClick(Sender: TObject);
begin
ShowLogHTML.Checked := NOT ShowLogHTML.Checked;
end;

procedure THTTPForm.ReloadClick(Sender: TObject);
{the Reload button was clicked}
begin
ReloadButton.Enabled := False;
Reloading := True;
FrameBrowser.Reload;
ReloadButton.Enabled := True;
FrameBrowser.SetFocus;
end;

procedure THTTPForm.WMDownLoad(var Message: TMessage);
{Handle download of file}
var
  DownLoadForm: TDownLoadForm;
begin
SaveDialog.Filename := GetURLFilenameAndExt(DownLoadURL);
SaveDialog.InitialDir := Cache;
if SaveDialog.Execute then
  begin
  DownLoadForm := TDownLoadForm.Create(Self);
  try
    with DownLoadForm do
      begin
      Filename := SaveDialog.Filename;
      DownLoadURL := Self.DownLoadURL;
      Proxy := Self.Proxy;
      ProxyPort := Self.ProxyPort;
      UserAgent := UsrAgent;
      ShowModal;
      end;
  finally
    DownLoadForm.Free;
   end;
  end;
end;

procedure THTTPForm.FormShow(Sender: TObject);
{OnShow handler.  Handles loading when a new instance is initiated by WinExec}
var
  S: string;
  I: integer;
begin

if (ParamCount >= 1) then
  begin            {Parameter is file to load}
  S := CmdLine;
  I := Pos('" ', S);
  if I > 0 then
    Delete(S, 1, I+1)  {delete EXE name in quotes}
  else Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
  I := Pos('"', S);
  while I > 0 do     {remove any quotes from parameter}
    begin
    Delete(S, I, 1);
    I := Pos('"', S);
    end;
  S := Trim(S);
  if not IsFullURL(S) then
    S := 'file:///'+DosToHtml(S);
  URLCombobox.Text := Trim(S);   {Parameter is URL to load}
  PostMessage(Handle, wm_LoadURL, 0, 0);
  end
{$ifdef M4Viewer}
 else
 begin
  SelectStartingMode;
  If M4ConfigDeleteCache Then
     DiskCache.EraseCache;
  URLCombobox.Text := startfile;
  SendMessage(Handle, wm_LoadURL, 0, 0);
 end;
{$else}
 else
   begin
   URLCombobox.Text := 'res:///page0.htm';
   SendMessage(Handle, wm_LoadURL, 0, 0);
   end;
{$endif}
end;

{----------------THTTPForm.BlankWindowRequest}
procedure THTTPForm.BlankWindowRequest(Sender: TObject; const Target, URL: ThtString);
{OnBlankWindowRequest handler.  Either a Target of _blank or an unknown target
 was called for.  Load a new instance}
var
  S: string;
begin
S := URL;
if not IsFullURL(S) then
  S := CombineURL(URLBase, S);

S := ParamStr(0)+' "'+S+'"';
StartProcess(PChar(S), sw_Show);
end;

procedure THTTPForm.Find1Click(Sender: TObject);
begin
FindDialog.Execute;
end;

procedure THTTPForm.FindDialogFind(Sender: TObject);
begin
with FindDialog do
  begin
  if not FrameBrowser.FindEx(FindText, frMatchCase in Options, not (frDown in Options)) then
    MessageDlg('No further occurances of "'+FindText+'"', mtInformation, [mbOK], 0);
  end;
end;

procedure THTTPForm.Edit1Click(Sender: TObject);
begin
with FrameBrowser do
  begin
  Copy1.Enabled := SelLength <> 0;
  SelectAll1.Enabled := (ActiveViewer <> Nil) and (ActiveViewer.CurrentFile <> '');
  Find1.Enabled := SelectAll1.Enabled;
  end;
end;

procedure THTTPForm.SelectAll1Click(Sender: TObject);
begin
FrameBrowser.SelectAll;
end;

procedure THTTPForm.Copy1Click(Sender: TObject);
begin
FrameBrowser.CopyToClipboard;
end;

procedure THTTPForm.URLComboBoxKeyPress(Sender: TObject; var Key: Char);
{trap CR in combobox}
begin
if (Key = #13) and (URLComboBox.Text <> '') then
  Begin
  Key := #0;
  GetButtonClick(Self);
  end;
end;

procedure THTTPForm.URLComboBoxClick(Sender: TObject);
begin
if URLComboBox.Text <> '' then
  GetButtonClick(Self);
end;

{----------------THTTPForm.RightClick}
procedure THTTPForm.RightClick(Sender: TObject;
            Parameters: TRightClickParameters);
{OnRightClick handler.  Bring up popup menu allowing saving of image or opening
 a link in another window}
var
  Pt: TPoint;
  S, Dest: string;
  I: integer;
  Viewer: ThtmlViewer;
  HintWindow: ThtHintWindow;
  ARect: TRect;
begin
Viewer := Sender as ThtmlViewer;
with Parameters do
  begin
  FoundObject := Image;
  if (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil) then
    begin
    if not IsFullUrl(FoundObject.Source) then
      FoundObjectName := CombineURL(FrameBrowser.GetViewerUrlBase(Viewer), FoundObject.Source)
    else FoundObjectName := FoundObject.Source;
    SaveImageAs.Enabled := True;
    end
  else SaveImageAs.Enabled := False;

  if URL <> '' then
    begin
    S := URL;
    I := Pos('#', S);
    if I >= 1 then
      begin
      Dest := System.Copy(S, I, 255);  {local destination}
      S := System.Copy(S, 1, I-1);     {the file name}
      end
    else
      Dest := '';    {no local destination}
    if S = '' then S := Viewer.CurrentFile;
    if IsFullUrl(S) then
      NewWindowFile := S + Dest
    else NewWindowFile := CombineURL(FrameBrowser.GetViewerUrlBase(Viewer), S) + Dest;
    OpenInNewWindow.Enabled := True;
    end
  else OpenInNewWindow.Enabled := False;

  GetCursorPos(Pt);
  if Length(CLickWord) > 0 then
    begin
    HintWindow := ThtHintWindow.Create(Self);
    try
      ARect := Rect(0,0,0,0);
      DrawText(HintWindow.Canvas.Handle, @ClickWord[1], Length(ClickWord), ARect, DT_CALCRECT);
      with ARect do
        HintWindow.ActivateHint(Rect(Pt.X+20, Pt.Y-(Bottom-Top)-15, Pt.x+30+Right, Pt.Y-15), ClickWord);
      PopupMenu.Popup(Pt.X, Pt.Y);
    finally
      HintWindow.Free;
      end;
    end
  else PopupMenu.Popup(Pt.X, Pt.Y);
  end;
end;

procedure THTTPForm.SaveImageAsClick(Sender: TObject);
{response to popup menu selection to save image}
var
  Stream: TMemoryStream;
  S: string;
  DocType: ThtmlFileType;
  AConnection: TURLConnection;
  Dummy: string;
begin
SaveDialog.InitialDir := Cache;
SaveDialog.Filename := GetURLFilenameAndExt(FoundObjectName);
if SaveDialog.Execute then
  begin
  Stream := TMemoryStream.Create;
  try
    if DiskCache.GetCacheFilename(FoundObjectName, S, DocType, Dummy) then
      begin
      Stream.LoadFromFile(S);
      Stream.SaveToFile(SaveDialog.Filename);
      end
    else
      begin
      AConnection := TURLConnection.GetConnection(FoundObjectName);
      if AConnection <> nil then
        try
          AConnection.InputStream := Stream;
          AConnection.Get(FoundObjectName);
          Stream.SaveToFile(SaveDialog.Filename);
        finally
          AConnection.Free;
          end;
      end;
  finally
    Stream.Free;
    end;
  end;
end;

procedure THTTPForm.OpenInNewWindowClick(Sender: TObject);
{response to popup menu selection to open link}
var
  PC: array[0..255] of char;
begin
StartProcess(StrPCopy(PC, ParamStr(0)+' "'+NewWindowFile+'"'), sw_Show);
end;

procedure THTTPForm.SaveURLClick(Sender: TObject);
{put the entry in the combobox in the list.  It will be saved on exit}
begin
with URLComboBox do
  begin
  if Items.IndexOf(Text) < 0 then
    Items.Add(Text);
  end;
end;

procedure THTTPForm.HTTPDocData1(Sender: TObject; Buffer : Pointer; Len: Integer);
begin
Status1.Caption := 'Text: ' + IntToStr(Connection.RcvdCount) + ' bytes';
Status1.Update;
Progress(Connection.RcvdCount, Connection.ContentLength);
end;

procedure THTTPForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
if Assigned(Connection) then
  Connection.Abort;
ClearProcessing;
end;

procedure THTTPForm.Progress(Num, Den: integer);
var
  Percent: integer;
begin
  if Den = 0 then
    Percent := 0
  else
    Percent := (100*Num) div Den;
  Gauge.Position := Percent;
  Gauge.Update;
end;

procedure THTTPForm.Status2Resize(Sender: TObject);
begin
Gauge.SetBounds(5, 7, Status2.ClientWidth-10, Status2.ClientHeight-14);
end;

procedure THTTPForm.CheckException(Sender: TObject; E: Exception);
begin
if E is ESpecialException then
  begin
  ShowMessage(E.Message);
  AnAbort := False;
  end
else Application.ShowException(E);
end;


procedure THTTPForm.DisableControls;
begin
  URLCombobox.Enabled:=false;
  CancelButton.Enabled:=true;
  ReloadButton.Enabled := False;
  Animate1.Visible := True;
  Animate1.Play(1, Animate1.FrameCount,0);
  Gauge.Visible := True;
end;

procedure THTTPForm.EnableControls;
begin
  URLCombobox.Enabled:=true;
  CancelButton.Enabled:=false;
  ReloadButton.Enabled := FrameBrowser.CurrentFile <> '';
  Reloading := False;
  Animate1.Active := False;
  Animate1.Visible := False;
  Gauge.Visible := False;
end;

procedure THTTPForm.BackButtonClick(Sender: TObject);
begin
FrameBrowser.GoBack;
if FrameBrowser.HistoryIndex > FrameBrowser.History.Count then
    URLCombobox.Text := FrameBrowser.History [FrameBrowser.HistoryIndex]; // ANGUS
CheckEnableControls;
end;

procedure THTTPForm.FwdButtonClick(Sender: TObject);
begin
FrameBrowser.GoFwd;
if FrameBrowser.HistoryIndex > FrameBrowser.History.Count then
    URLCombobox.Text := FrameBrowser.History [FrameBrowser.HistoryIndex]; // ANGUS
CheckEnableControls;
end;

{----------------TImageHTTP.CreateIt}
constructor TImageHTTP.CreateIt(AOwner: TComponent; IRec: TObject);
begin
inherited Create(AOwner);
ImRec := IRec as ImageRec;
ImRec.Stream := TMemoryStream.Create;
URL := ImRec.URL;
Connection := TURLConnection.GetConnection(URL);
Connection.InputStream := ImRec.Stream;
end;

{----------------TImageHTTP.GetAsync}
procedure TImageHTTP.GetAsync;
begin
Connection.GetAsync(URL);
end;

{----------------TImageHTTP.Destroy}
destructor TImageHTTP.Destroy;
begin
if Assigned(ImRec) then
  begin
  ImRec.Stream.Free;
  ImRec.Free;
  end;
Connection.Free;
inherited Destroy;
end;

{----------------THTTPForm.Processing}
procedure THTTPForm.Processing(Sender: TObject; ProcessingOn: Boolean);
begin
if ProcessingOn then
  begin    {disable various buttons and menuitems during processing}
  FwdButton.Enabled := False;
  BackButton.Enabled := False;
  ReloadButton.Enabled := False;
  end
else
  begin
  FwdButton.Enabled := FrameBrowser.FwdButtonEnabled;
  BackButton.Enabled := FrameBrowser.BackButtonEnabled; 
  ReloadButton.Enabled := FrameBrowser.CurrentFile <> '';
  CheckEnableControls;
  LogLine ('Page Completed' + #13#10);
  end;
end;

procedure THTTPForm.PrintPreviewClick(Sender: TObject);
var
  pf: TPreviewForm;
  Viewer: ThtmlViewer;
  Abort: boolean;
begin
Viewer := FrameBrowser.ActiveViewer;
if Assigned(Viewer) then
   begin
   pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
   try
     if not Abort then
       pf.ShowModal;
   finally
     pf.Free;
     end;
   end;
end;

procedure THTTPForm.File1Click(Sender: TObject);
begin
Print1.Enabled := FrameBrowser.ActiveViewer <> Nil;
PrintPreview.Enabled := Print1.Enabled;
end;

procedure THTTPForm.Print1Click(Sender: TObject);
begin
with PrintDialog do
  if Execute then
    if PrintRange = prAllPages then
      FrameBrowser.Print(1, 9999)
    else
      FrameBrowser.Print(FromPage, ToPage);
end;

procedure THTTPForm.PrintHeader(Sender: TObject; Canvas: TCanvas; NumPage,
  W, H: Integer; var StopPrinting: Boolean);
var
  AFont: TFont;
begin
AFont := TFont.Create;
AFont.Name := 'Arial';
AFont.Size := 8;
with Canvas do
  begin
  Font.Assign(AFont);
  SetBkMode(Handle, Transparent);
  SetTextAlign(Handle, TA_Bottom or TA_Left);
  if FrameBrowser.ActiveViewer <> Nil then
    begin
    TextOut(50, H-5, FrameBrowser.ActiveViewer.DocumentTitle);
    SetTextAlign(Handle, TA_Bottom or TA_Right);
    TextOut(W-50, H-5, FrameBrowser.ActiveViewer.CurrentFile);
    end;
  end;
AFont.Free;
end;

procedure THTTPForm.PrintFooter(Sender: TObject; Canvas: TCanvas; NumPage,
  W, H: Integer; var StopPrinting: Boolean);
var
  AFont: TFont;
begin
AFont := TFont.Create;
AFont.Name := 'Arial';
AFont.Size := 8;
with Canvas do
  begin
  Font.Assign(AFont);
  SetTextAlign(Handle, TA_Bottom or TA_Left);
  TextOut(50, 20, DateToStr(Date));
  SetTextAlign(Handle, TA_Bottom or TA_Right);
  TextOut(W-50, 20, 'Page '+IntToStr(NumPage));
  end;
AFont.Free;
end;

procedure THTTPForm.FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
begin
LogLine ('FrameBrowserMeta, HttpEq=' + HttpEq + ', MetaName=' + Name + ', MetaContent=' + Content);
end;

procedure THTTPForm.FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: ThtString);
begin
LogLine ('FrameBrowserScript, Name=' + Name + ', Src=' + Src + ', Script=' + Script);
end;

procedure THTTPForm.About1Click(Sender: TObject);
begin
AboutBox := TAboutBox.CreateIt(Self, 'FrameBrowser ICSv7 Demo', 'TFrameBrowser');
try
  AboutBox.ShowModal;
finally
  AboutBox.Free;
  end;
end;

procedure THTTPForm.Proxy1Click(Sender: TObject);
begin
ProxyForm := TProxyForm.Create(Self);
ProxyForm.ProxyEdit.Text := Proxy;
ProxyForm.PortEdit.Text := ProxyPort;
ProxyForm.ProxyUsername.Text := ProxyUser;
ProxyForm.ProxyPassword.Text := ProxyPassword;
try
  if ProxyForm.ShowModal = mrOK then
    begin
    Proxy := ProxyForm.ProxyEdit.Text;
    ProxyPort := ProxyForm.PortEdit.Text;
    ProxyUser := ProxyForm.ProxyUsername.Text;
    ProxyPassword := ProxyForm.ProxyPassword.Text;
    end;
finally
  ProxyForm.Free;
  end;
end;

procedure THTTPForm.DemoInformation1Click(Sender: TObject);
begin
UrlComboBox.Text := 'res:///page0.htm';
GetButtonClick(Self);
end;

procedure THTTPForm.FrameBrowserMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: string;
begin
if not Timer1.Enabled and (Sender is ThtmlViewer) and Assigned(ActiveControl)
         and ActiveControl.Focused then
  begin
  TitleViewer := ThtmlViewer(Sender);
  TitleStr := TitleViewer.TitleAttr;
  if TitleStr = '' then
    OldTitle := ''
  else if TitleStr <> OldTitle then
    begin
    TimerCount := 0;
    Timer1.Enabled := True;
    OldTitle := TitleStr;
    end;
  end;
end;

procedure THTTPForm.CloseHints;
begin
Timer1.Enabled := False;
HintWindow.ReleaseHandle;
HintVisible := False;
TitleViewer := Nil;
end;

procedure THTTPForm.Timer1Timer(Sender: TObject);
const
  StartCount = 2; {timer counts before hint window opens}
  EndCount = 20;  {after this many timer counts, hint window closes}
var
  Pt, Pt1: TPoint;
  ARect: TRect;
  TitleStr: ThtString;

begin
if not Assigned(TitleViewer) then
  begin
  CloseHints;
  Exit;
  end;
Inc(TimerCount);
GetCursorPos(Pt);
try       {in case TitleViewer becomes corrupted}
  Pt1 := TitleViewer.ScreenToClient(Pt);
  TitleStr := TitleViewer.TitleAttr;
  if (TitleStr = '') or not PtInRect(TitleViewer.ClientRect, Pt1)then
    begin
    OldTitle := '';
    CloseHints;
    Exit;
    end;
  if TitleStr <> OldTitle then
    begin
    TimerCount := 0;
    OldTitle := TitleStr;
    HintWindow.ReleaseHandle;
    HintVisible := False;
    Exit;
    end;

  if TimerCount > EndCount then
    CloseHints
  else if (TimerCount >= StartCount) and not HintVisible then
    begin
    ARect := HintWindow.CalcHintRect(300, TitleStr, Nil);
    with ARect do
      HintWindow.ActivateHint(Rect(Pt.X, Pt.Y+18, Pt.X+Right, Pt.Y+18+Bottom), TitleStr);
    HintVisible := True;
    end;
{note: this exception can occur when switching frames while TitleViewer is active.  It is
 adequately handled here and only appears in the IDE when "Stop on Delphi Exceptions" is
 turned on}
except
  CloseHints;
  end;
end;

end.

