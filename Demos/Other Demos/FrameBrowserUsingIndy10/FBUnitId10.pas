{
Version   11.4
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2013 by HtmlViewer Team

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
}
unit FBUnitId10;

{$include htmlcons.inc}
{$include options.inc}

{A program to demonstrate the TFrameBrowser component}


interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI, Menus, StdCtrls, Buttons, ExtCtrls, htmlun2, CachUnitId,
  URLSubs, htmlview, htmlsubs, Gauges, mmSystem,
{$ifdef UseOldPreviewForm}
  PreviewForm,
{$else UseOldPreviewForm}
  BegaHtmlPrintPreviewForm,
{$endif UseOldPreviewForm}
  DownLoadId, IniFiles,
  Readhtml, urlconId10, FramBrwz, FramView, MPlayer, IdBaseComponent,
  IdAntiFreezeBase, IdAntiFreeze, IdGlobal, IdGlobalProtocols,
  ImgList, ComCtrls, ToolWin, IdIntercept,
  IdHTTP, IdComponent, IdIOHandler, IdIOHandlerSocket, IdSSLOpenSSL, IdCookie, IdCookieManager,
  IdTCPConnection, IdTCPClient, IdAuthentication,
  {$if CompilerVersion >= 15}
    {$ifndef UseVCLStyles}
    XpMan,
    {$endif}
  {$ifend}
   {$ifdef UseVCLStyles}
   Vcl.Styles,
   Vcl.Themes,
   Vcl.ActnPopup,
   {$endif}
  idLogfile,
  HtmlGlobals;

const
  (*UsrAgent = 'Mozilla/4.0 (compatible; Indy Library)';*)
  UsrAgent = 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98)';
 // UsrAgent = 'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0)';
  MaxHistories = 15;  {size of History list}
  wm_LoadURL = wm_User+124;
  wm_DownLoad = wm_User+125;

type
  {$ifdef UseVCLStyles}
  TPopupMenu=class(Vcl.ActnPopup.TPopupActionBar);
  {$endif}
  ImageRec = class(TObject)
  public
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

  TModIdLogFile = class(TIdLogFile)
  public
      procedure LogWriteString(const AText: string); override;
  end;

  THTTPForm = class(TForm)
    MainMenu1: TMainMenu;
    HistoryMenuItem: TMenuItem;
    Help1: TMenuItem;
    File1: TMenuItem;
    Openfile1: TMenuItem;
    OpenDialog: TOpenDialog;
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
    StatusBarMain: TStatusBar;
    Gauge: TProgressBar;
    PopupMenu1: TPopupMenu;
    ViewImage: TMenuItem;
    CopyImagetoclipboard: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    View1: TMenuItem;
    HTTPHeaders1: TMenuItem;
    PageInfo1: TMenuItem;
    N4: TMenuItem;
    LibraryInformation1: TMenuItem;
    Source1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetButtonClick(Sender: TObject);
    procedure HTTPHeaderData(Sender: TObject);
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
    procedure HTTPDocData1(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    procedure AuthorizationEvent(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
{$ifdef UNICODE}
    procedure BlankWindowRequest(Sender: TObject; const Target, URL: String);
    procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, RefererX: String; Reload: Boolean;
      var NewURL: String; var DocType: ThtmlFileType;
      var Stream: TMemoryStream);
    procedure GetImageRequest(Sender: TObject; const URL: String; var Stream: TStream);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: String; var Handled: Boolean);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: String);
    procedure FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: string);
    procedure FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: string);
{$else}
    procedure BlankWindowRequest(Sender: TObject; const Target, URL: WideString);
    procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, RefererX: WideString; Reload: Boolean;
      var NewURL: WideString; var DocType: ThtmlFileType;
      var Stream: TMemoryStream);
    procedure GetImageRequest(Sender: TObject; const URL: WideString; var Stream: TStream);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: WideString; var Handled: Boolean);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: WideString);
    procedure FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: WideString);
    procedure FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: WideString);
{$endif}
    procedure StatusBarMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure HTTPHeaders1Click(Sender: TObject);
    procedure PageInfo1Click(Sender: TObject);
    procedure LibraryInformation1Click(Sender: TObject);
    procedure Source1Click(Sender: TObject);
  private
    { Private declarations }
    URLBase: String;
    Histories: array[0..MaxHistories-1] of TMenuItem;
    Pending: TList;       {a list of ImageRecs}
    HTTPList: TList;         {a list of the Image HTTPs currently existing}
    DiskCache: TDiskCache;
    NewLocation: string;
    Redirect: boolean;
    ARealm: string;
    CurrentLocalFile,
    DownLoadUrl: string;
    Reloading: boolean;
    CookieManager: TIdCookieManager;
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
    FHeaderRequestData : TStrings;
    FHeaderResponseData : TStrings;
    FMetaInfo : TStrings;
    {$ifdef LogIt}
    ShowDiagWindow: TMenuItem;
    {$endif}
    {$ifdef UseVCLStyles}
    sepVCLStyles,
    VCLStyles1 : TMenuItem;

    procedure VCLStyleClick(Sender: TObject);
    {$endif}
    {$ifdef LogIt}
   procedure ShowDiagWindowClick(Sender: TObject);
   {$endif}
    procedure EnableControls;
    procedure DisableControls;
    {$ifdef LogIt}
    procedure LogTStrings(AStrings : TStrings);
    {$endif}
    procedure ImageRequestDone(Sender: TObject; RqType: THttpRequest; Error: Word);
    procedure HistoryClick(Sender: TObject);
    procedure WMLoadURL(var Message: TMessage); message WM_LoadURL;
    procedure WMDownLoad(var Message: TMessage); message WM_DownLoad; 
    procedure CheckEnableControls;
    procedure ClearProcessing;
    procedure Progress(Num, Den: integer);
    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
    procedure CheckException(Sender: TObject; E: Exception);
    procedure HTTPRedirect(Sender: TObject; var Dest: String;
      var NumRedirect: Integer; var Handled: boolean; var Method: TIdHTTPMethod);
    procedure SaveCookies(ASender: TObject; ACookieCollection: TIdCookies);
    procedure LoadCookies(ACookieCollection: TIdCookieManager);
    procedure CloseHints;
    procedure FrameBrowserFileBrowse(Sender, Obj: TObject; var S: ThtString);
  public
    { Public declarations }
  {$ifdef UseSSL}
    SSL: TIdSSLIOHandlerSocketOpenSSL;
  {$endif}
  {$ifdef LogIt}
    Log: TModIdLogFile;
    {$endif}
    FIniFilename: string;
       {$ifdef LogIt}
    procedure LogLine (S: string);
    {$endif}
  end;

  EContentTypeError = class(Exception);
  ESpecialException = class(Exception);

const
  CookieFile = 'Cookies.ini';
var
  HTTPForm: THTTPForm;
  Mon: TextFile;
  Monitor1: boolean;
  Cache: string;

implementation

uses
  InfoDlg,
{$ifdef LogIt}
  logwin,
{$endif}
{$ifdef Compiler24_Plus}
  System.Types,
{$endif}
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
  {$ifdef UseZLib}
  IdZLibHeaders,
  IdCTypes,
  {$endif}
  IdURI,
  HTMLAbt, ProxyDlg, AuthUnit;

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$if CompilerVersion < 15}
    {$R manifest.res}
  {$ifend}
{$endif}

  {$ifdef LogIt}
procedure THTTPForm.LogLine (S: string);
begin
 //   if NOT ShowDiagWindow.Checked then exit;
    if NOT LogForm.Visible then LogForm.Visible := true;
    LogForm.LogMemo.Lines.Add (S);
end;
{$endif}

{$ifdef UseVCLStyles}
procedure THTTPForm.VCLStyleClick(Sender: TObject);
var
  s : String;
  m : TMenuItem;
begin
  m :=  (Sender as TMenuItem);
  s := TStyleManager.StyleNames[m.Tag];
  TStyleManager.SetStyle(s);
  m.Checked := True;
end;
{$endif}

{$ifdef LogIt}
procedure THTTPForm.ShowDiagWindowClick(Sender: TObject);
begin
  ShowDiagWindow.Checked := NOT ShowDiagWindow.Checked;
  LogForm.Visible := ShowDiagWindow.Checked;
end;
{$endif}
{----------------THTTPForm.FormCreate}
procedure THTTPForm.FormCreate(Sender: TObject);
var
  I, J: integer;
  IniFile: TIniFile;
  SL: TStringList;
  ProgressBarStyle : LongInt;
    {$ifdef UseVCLStyles}
  m : TMenuItem;
    {$endif}
begin
  Self.OpenDialog.Filter := GetFileMask;
  FMetaInfo := TStringList.Create;
  {$ifdef has_StyleElements}
  TStyleManager.AnimationOnControls := True;
  {$Endif}
    FHeaderRequestData := TStringList.Create;
    FHeaderResponseData := TStringList.Create;
  {$ifdef LogIt}
  logwin.LogForm := TLogForm.Create(nil);
  {$endif}
{$ifdef HasGestures}
  FrameBrowser.Touch.InteractiveGestureOptions := [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia];
  FrameBrowser.Touch.InteractiveGestures := [igPan];
{$endif}
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
  Self.StatusBarMain.Panels[0].Text := '';

{Monitor1 will be set if this is the first instance opened}
  Monitor1 := True;
  try
    AssignFile(Mon, Cache+'Monitor.txt');  {a monitor file for optional use}
    Rewrite(Mon);
  except
    Monitor1 := False;   {probably open in another instance}
  end;
  {$ifdef LogIt}
  ShowDiagWindow:= TMenuItem.Create(Options1);
  ShowDiagWindow.OnClick := ShowDiagWindowClick;
  ShowDiagWindow.Caption := '&Diagnostic Window...';
  Options1.Add(ShowDiagWindow);
  {$endif}
  {$ifdef UseVCLStyles}
  sepVCLStyles := TMenuItem.Create(Options1);
  sepVCLStyles.Caption := '-';
  Options1.Add(sepVCLStyles);
  VCLStyles1 := TMenuItem.Create(Options1);
  VCLStyles1.Caption := '&VCL Styles';
  Options1.Add(VCLStyles1);
  if   TStyleManager.Enabled then begin
    for i := Low(TStyleManager.StyleNames) to High(TStyleManager.StyleNames) do begin
      m := TMenuItem.Create(VCLStyles1);
      m.Caption := TStyleManager.StyleNames[i];
      m.Tag := i;
      m.OnClick := VCLStyleClick;
      m.RadioItem := True;
      VCLStyles1.Add(m);
    end;
  end;
  {$endif}
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

  CookieManager := TIdCookieManager.Create(Self);
  LoadCookies(CookieManager);
  CookieManager.OnDestroy := SaveCookies;

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
    {$ifdef LogIt}
    ShowDiagWindow.Checked := IniFile.ReadBool('HTTPForm', 'ShowDiagWindow', ShowDiagWindow.Checked);
    {$endif}
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
        UrlCombobox.Items.Add(Copy(SL[I], J+1, 1000));
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
//  HintWindow.Color := $C0FFFF;
  FrameBrowser.OnFileBrowse := FrameBrowserFileBrowse;
  Gauge.Parent := Self.StatusBarMain;
  //remove progress bar border
  ProgressBarStyle := GetWindowLong(Gauge.Handle,
                                    GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle
                      - WS_EX_STATICEDGE;
  SetWindowLong(Gauge.Handle,
                GWL_EXSTYLE,
                ProgressBarStyle);
end;

{----------------THTTPForm.FormDestroy}
procedure THTTPForm.FormDestroy(Sender: TObject);
var
  I: integer;
  IniFile: TIniFile;
begin
{$ifdef UseSSL}
  if Assigned(SSL) then
     SSL.Free;
{$endif}
{$ifdef LogIt}
  Log.Free;
{$endif}
  AStream.Free;
  Pending.Free;
  for I := 0 to HTTPList.Count-1 do
    TImageHTTP(HTTPList.Items[I]).Free;
  HTTPList.Free;
  if Monitor1 then
    CloseFile(Mon);
  DiskCache.Free;
  CookieManager.Free;
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
      {$ifdef LogIt}
      IniFile.WriteBool('HTTPForm', 'ShowDiagWindow', ShowDiagWindow.Checked);
      {$endif}
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
  FHeaderRequestData.Free;
  FHeaderResponseData.Free;
end;

{----------------THTTPForm.GetButtonClick}
procedure THTTPForm.GetButtonClick(Sender: TObject);
{initiate loading of a main document}
begin
  URLBase := GetUrlBase(URLCombobox.Text);
  DisableControls;
  StatusBarMain.Panels[0].Text := '';
  StatusBarMain.Panels[1].Style := psText;
  StatusBarMain.Panels[1].Text := '';
  try
    FHeaderRequestData.Clear;
    FHeaderResponseData.Clear;
  {the following initiates one or more GetPostRequest's}
    FrameBrowser.LoadURL(Normalize(URLCombobox.Text));
    Reloading := False;
  finally
    CheckEnableControls;
    FrameBrowser.SetFocus;
  end;
end;

{----------------THTTPForm.FrameBrowserFileBrowse}
//This fires when the user hits a file-browse button on a form.

procedure THTTPForm.FrameBrowserFileBrowse(Sender, Obj: TObject; var S: ThtString);
var LFile : String;
begin
  //
  if PromptForFileName(LFile,'Any File |*.*','','Select File','',False) then begin
    S := LFile;
  end;
  //
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
  StatusBarMain.Panels[0].Text := '';
  StatusBarMain.Panels[1].Text := '';
  FName := '';
  Error := False;
  AnAbort := False;
  NumImageTot := 0;
  NumImageDone := 0;
  Progress(0, 0);
  Gauge.Visible := True;
  URL1 := DecodeURL(Normalize(URL));
(*URLComboBox.Text := URL1; *)    {Probably don't want this}
  URLBase := GetUrlBase(URL1);
  DisableControls;
  NewLocation := '';     {will change if document referenced by URL has been relocated}
  ARealm := '';
  TryRealm := True;
  AStream.Clear;
  RedirectCount := 0;
  if Reloading or Reload then
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
    end;
    If {$ifdef UseSSL}not Assigned(SSL) and{$endif} (GetProtocol(URL1) = 'https') then  {guard against missing DLLs}
      Connection := Nil
    else
      Connection := TURLConnection.GetConnection(URL1);
    if connection <> nil then
    begin
      Connection.OnHeaderData := HTTPForm.HTTPHeaderData;
      Connection.OnDocData := HTTPForm.HTTPDocData1;
      Connection.Referer := RefererX;
      LastURL := Url1;
      Connection.OnRedirect := HTTPForm.HTTPRedirect;
      Connection.Proxy := Proxy;
      Connection.ProxyPort := ProxyPort;
      Connection.ProxyUser := ProxyUser;
      Connection.ProxyPassword := ProxyPassword;
      Connection.UserAgent := UsrAgent;
      Connection.CookieManager := CookieManager;
      Connection.OnAuthorization := AuthorizationEvent;
      try
        repeat
          TryAgain := False;
          Redirect := False;
          Inc(RedirectCount);
          if Assigned(Connection.InputStream) then
            Connection.InputStream.Clear;
          try
            if IsGet then
            begin       {Get}
              if Query1 <> '' then begin
                {$ifdef LogIt}
                 LogLine ('FrameBrowser Get: ' + URL1+'?'+Query1);
                 {$endif}
                Connection.Get(URL1+'?'+Query1)
              end else begin
                {$ifdef LogIt}
                 LogLine ('FrameBrowser Get: ' + URL1);
                 {$endif}
                Connection.Get(URL1);
              end;
            end
            else      {Post}
            begin
              Connection.SendStream := TMemoryStream.Create;
              try
                {$ifdef LogIt}
                LogLine ('FrameBrowser Post: ' + URL1+', Data=' +
                  Copy (Query1, 1, 132) + ', EncType=' + EncType);  // not too much data
                   {$endif}

                Connection.SendStream.WriteBuffer(Query1[1], Length(Query1));
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
            if Connection.StatusCode = 401 then
              TryAgain := GetAuthorization
            else
              if Redirect then
              begin
                Url1 := NewLocation;
                if Connection.StatusCode = 303 then
                begin
                  IsGet := True;
                 Query1 := '';
                end;
                TryAgain := True;

              if {$ifdef UseSSL} not Assigned(SSL) and {$endif} (GetProtocol(NewLocation) = 'https') then
              begin
                TryAgain := False;
                S := '<p>Unsupported protocol: https';
                Connection.InputStream.Position := Connection.InputStream.Size;
                Connection.InputStream.Write(S[1], Length(S));
              end;

            end;
        except
          case Connection.StatusCode of
            401: TryAgain := GetAuthorization;
            405: if not IsGet and (Pos('get', Allow) > 0) then
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
        {$ifdef LogIt}
         LogLine ('FrameBrowserGetPostRequestEx Done: Status ' + IntToStr (Connection.StatusCode));
         {$endif}
      until not TryAgain;
      if FHeaderRequestData.Count > 0 then begin
        FHeaderRequestData.Add('');
      end;
      FHeaderRequestData.Add( 'URL = "'+ URL +'"');
      FHeaderRequestData.Add('');
      FHeaderRequestData.AddStrings( Connection.HeaderRequestData );
      if FHeaderResponseData.Count > 0 then begin
        FHeaderResponseData.Add('');
      end;
      FHeaderResponseData.Add( 'URL = "'+ URL +'"');
      FHeaderResponseData.Add('');
      FHeaderResponseData.AddStrings( Connection.HeaderResponseData );
      DocType := Connection.ContentType;
      AStream.LoadFromStream(Connection.InputStream);
    except
      {$ifndef UseSSL}
      On ESpecialException do
      begin
        {$ifdef LogIt}
        LogLine ('FrameBrowserGetPostRequestEx: Special Exception');
        {$endif}
        Connection.Free;   {needs to be reset}
        Connection := Nil;
        Raise;
      end;
      {$endif}
      On E: Exception do
      try
       {$ifdef LogIt}
        LogLine ('FrameBrowserGetPostRequestEx: Exception - ' + E.Message);
        {$endif}
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
              else
                 S := ReasonPhrase+'<p>Statuscode: '+IntToStr(StatusCode);
              AStream.Write(S[1], Length(S));
            end;
            DocType := HTMLType;
          end
        else  {other connection types}
        begin
            S := E.Message;          {Delphi 1}
            AStream.Write(S[1], Length(S));
        end;
      finally
          Connection.Free;   {needs to be reset}
          Connection := Nil;
      end;
    end;
    StatusBarMain.Panels[0].Text := 'Received ' + IntToStr(AStream.Size) + ' bytes';
    FName := DiskCache.AddNameToCache(LastUrl, NewLocation, DocType, Error);
    if FName <> '' then
      try
        AStream.SaveToFile(FName);   {it's now in cache}
      except
      end;
    end
    else
    begin        { unsupported protocol }
      S := 'Unsupported protocol: ' + GetProtocol(URL1);
      if Sender is TFrameBrowser then   {main document display}
        Raise(ESpecialException.Create(S))
      else   {else other errors will get displayed as HTML file}
        AStream.Write(S[1], Length(S));
        {$ifdef LogIt}
       LogLine (S);
       {$endif}
    end;
  end
  else
  begin
    AStream.LoadFromFile(FName);
    {$ifdef LogIt}
    LogLine ('FrameBrowserGetPostRequestEx: from Cache' + FName);
     {$endif}
  end;
  NewURL := NewLocation;   {in case location has been changed}
  Stream := AStream;
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
  if Reloading then
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
    {$ifdef LogIt}
    LogLine ('GetImageRequest, from Cache: ' + S);
    {$endif}
  end
  else
    if not ((GetProtocol(URL) = 'http')
       {$ifdef UseSSL}
       or (GetProtocol(URL) = 'https')
       {$endif}
       ) then
    begin
      if Assigned(Connection) then
        Connection.Free;
      Connection := TURLConnection.GetConnection(URL);
      if connection <> nil then
      begin
        {$ifdef LogIt}
         LogLine ('GetImageRequest Start: ' + URL);
        {$endif}
        try
          Connection.Get(URL);
          Stream := Connection.InputStream;
       except
         Stream := Nil;
       end;
    end
    else
      Stream := Nil;
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
    ImHTTP.Connection.OnRequestDone := ImageRequestDone;
    ImHTTP.Connection.Proxy := Proxy;
    ImHTTP.Connection.ProxyPort := ProxyPort;
    ImHTTP.Connection.ProxyUser := ProxyUser;
    ImHTTP.Connection.ProxyPassword := ProxyPassword;
    ImHTTP.Connection.UserAgent := UsrAgent;
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
      {$ifdef LogIt}
        LogLine ('=====');
        LogLine ('');
         LogLine ('--- Request ---');
        LogTStrings( ImHTTP.Connection.HeaderRequestData );
        LogLine ('');
        LogLine ('--- Response ---');
        LogTStrings( ImHTTP.Connection.HeaderResponseData );
        LogLine ('');
        LogLine ('====');
      {$endif}
    if (RqType = httpGet) then
    begin
      if (Error = 0) then
      begin   {Add the image record to the Pending list to be inserted in the timer
               loop at TimerTimer}
      {$ifdef LogIt}
        LogLine ('GetImageRequest Done OK');
      {$endif}
        Pending.Add(ImHTTP.ImRec);
        ImHTTP.ImRec := Nil;  {so it won't get free'd below}
        Timer.Enabled := True;
      end
      else
      {this code will cause Error Image to appear if error occured when downloading image}
      begin   {Add the image record to the Pending list to be inserted in the timer
               loop at TimerTimer}
      {$ifdef LogIt}
        LogLine ('GetImageRequest Done - Error '+IntToStr(Error));
      {$endif}
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
          FName := DiskCache.AddNameToCache(URL, '', ImgType, False);
          if FName <> '' then
          try
            Stream.SaveToFile(FName);
          except
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
        else
          Histories[I].Visible := False;
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
  else
    OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
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
    StatusBarMain.Panels[2].Text := ''
  else
    if Target <> '' then
      StatusBarMain.Panels[2].Text := 'Target: '+Target+'  URL: '+URL
    else
      StatusBarMain.Panels[2].Text := 'URL: '+URL
end;

{----------------THTTPForm.HotSpotTargetClick}
procedure THTTPForm.HotSpotTargetClick(Sender: TObject; const Target, URL: ThtString; var Handled: Boolean);
{a link was clicked.  URL is a full url here have protocol and path added.  If
 you need the actual link string, it's available in the TFrameBrowser URL property}
const
  snd_Async = $0001;  { play asynchronously }
var
  Protocol, Ext: string;
  PC: array[0..255] of char;
  S, Params: string;
  K: integer;
  Tmp: string;
begin
  {$ifdef LogIt}
  LogLine ('HotSpotTargetClick: ' + URL);
  {$endif}
  Protocol := GetProtocol(URL);
  if Protocol = 'mailto' then
  begin
    Tmp := URL + #0;  {for Delphi 1}
  {call mail program}
  {Note: ShellExecute causes problems when run from Delphi 4 IDE}
    ShellExecute(Handle, nil, @Tmp[1], nil, nil, SW_SHOWNORMAL);
    Handled := True;
    Exit;
  end;
{Note: it would be nice to handle ftp protocol here also as some downloads use
 this protocol}

  Ext := Lowercase(GetURLExtension(URL));
  if Pos('http', Protocol) > 0 then
  begin
    if (CompareText(Ext, 'zip') = 0) or (CompareText(Ext, 'exe') = 0) then
    begin
    {download can't be done here.  Post a message to do it later at WMDownload}
      DownLoadURL := URL;
      PostMessage(handle, wm_DownLoad, 0, 0);
      Handled := True;
      Exit;
    end;
  end;
  if (Protocol = 'file') then
  begin
    S := URL;
    K := Pos(' ', S);     {look for parameters}
    if K = 0 then
      K := Pos('?', S);  {could be '?x,y' , etc}
    if K > 0 then
    begin
      Params := Copy(S, K+1, 255); {save any parameters}
      setlength(S, K-1);            {truncate S}
    end
    else
      Params := '';
    S := HTMLToDos(S);
    if Ext = 'wav' then
    begin
      Handled := True;
      sndPlaySound(StrPCopy(PC, S), snd_ASync);
    end
    else
      if Ext = 'exe' then
      begin
        Handled := True;
        ShellExecute(Handle, nil, PChar(S), PChar(Params), nil, sw_Show);
  //  WinExec(StrPCopy(PC, S+' '+Params), sw_Show);
      end
      else
        if (Ext = 'mid') or (Ext = 'avi')  then
        begin
          Handled := True;
          ShellExecute(Handle, nil, PChar('MPlayer.exe'), PChar(' /play /close ' +Params), nil, sw_Show);
  //  WinExec(StrPCopy(PC, 'MPlayer.exe /play /close '+S), sw_Show);
        end;
    {else ignore other extensions}
    UrlComboBox.Text := URL;
    Exit;
  end;
  UrlComboBox.Text := URL;   {other protocall}
end;

procedure THTTPForm.CheckEnableControls;
begin
  if not FrameBrowser.Processing and
      (not Assigned(Connection) or (Connection.State in [httpReady, httpNotConnected])) and
      (HTTPList.Count = 0) then
  begin
    EnableControls;
    StatusBarMain.Panels[1].Text   := 'DONE';
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

procedure THTTPForm.ShowImagesClick(Sender: TObject);
begin
  FrameBrowser.ViewImages := not FrameBrowser.ViewImages;
  ShowImages.Checked := FrameBrowser.ViewImages;
end;

procedure THTTPForm.Source1Click(Sender: TObject);

begin
  if Self.FrameBrowser.ActiveViewer <> nil then begin
    with TInfoForm.Create(Application) do
    try
      Caption := 'HTTP Headers';
      mmoInfo.Clear;
      mmoInfo.Lines.Text := FrameBrowser.ActiveViewer.DocumentSource;
      ShowModal;
    finally
      Free;
    end;
  end;
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
  {$ifdef UseSSL}
  h1, h2: THandle;
  {$endif}
begin
  {$ifdef UseSSL}  {check to see the DLLs for Secure Socket Layer can be found}
  h1 := LoadLibrary('libeay32.dll');
  h2 := LoadLibrary('ssleay32.dll');
  if h2 = 0 then begin
    {alternative name for mingw32 versions of OpenSSL}
    h2 := LoadLibrary('libssl32.dll');
  end;
  if (h1 = 0) or (h2 = 0) then
  begin
    if Monitor1 then
      ShowMessage('One or both Secure Socket Layer (SSL) DLLs cannot be found.'^M^J+
      'See download information which follows.');
    SSL := Nil;
  end
  else
  begin
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    SSL.SSLOptions.Method := sslvSSLv23;
    SSL.SSLOptions.Mode := sslmClient;
  end;
  FreeLibrary(h1);
  FreeLibrary(h2);
  {$else}
  //SSL := Nil;
  {$endif}

  if (ParamCount >= 1) then
  begin            {Parameter is file to load}
    S := CmdLine;
    I := Pos('" ', S);
    if I > 0 then
      Delete(S, 1, I+1)  {delete EXE name in quotes}
    else
      Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
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

  ShellAPI.ShellExecute(Handle,nil,PChar(ParamStr(0)),PChar(S),nil,sw_show);
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
  if URLComboBox.Text <> '' then begin
    FMetaInfo.Clear;
    GetButtonClick(Self);
  end;
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
      else
        FoundObjectName := FoundObject.Source;
      SaveImageAs.Enabled := True;
    end
    else
      SaveImageAs.Enabled := False;

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
      if S = '' then
        S := Viewer.CurrentFile;
      if IsFullUrl(S) then
        NewWindowFile := S + Dest
      else
        NewWindowFile := CombineURL(FrameBrowser.GetViewerUrlBase(Viewer), S) + Dest;
      OpenInNewWindow.Enabled := True;
    end
    else
      OpenInNewWindow.Enabled := False;

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
  else
    PopupMenu.Popup(Pt.X, Pt.Y);
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
begin
  ShellAPI.ShellExecute(Handle,nil,PChar(ParamStr(0)),PChar('"'+NewWindowFile+'"'),nil,sw_show);
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

procedure THTTPForm.HTTPDocData1(Sender: TObject);
begin
  StatusBarMain.Panels[0].Text := 'Text: ' + IntToStr(Connection.RcvdCount) + ' bytes';
  Progress(Connection.RcvdCount, Connection.ContentLength);
end;

procedure THTTPForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Connection) then
    Connection.Abort;
  ClearProcessing;
    {$ifdef LogIt}
  FreeAndNil(logwin.LogForm);
  {$endif}
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
  Gauge.Max := 100;
  StatusBarMain.Panels.Items[1].Style := psOwnerDraw;
end;

procedure THTTPForm.StatusBarMainDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel = StatusBar.Panels[1] then
  with Gauge do begin
    Top := Rect.Top;
    Left := Rect.Left;
    Width := Rect.Right - Rect.Left - 15;
    Height := Rect.Bottom - Rect.Top;
  end;
end;

Procedure THTTPForm.CheckException(Sender: TObject; E: Exception);
begin
  if E is ESpecialException then
  begin
    ShowMessage(E.Message);
    AnAbort := False;
  end
  else
  begin
    {$IFDEF LogIt}
    if E is EIdHTTPProtocolException then begin
       LogLine( IntToStr(EIdHTTPProtocolException(E).ErrorCode)+ ' - '+ EIdHTTPProtocolException(E).ErrorMessage );
    end;

    {$ENDIF}
    Application.ShowException(E);
  end;
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

procedure THTTPForm.HTTPHeaderData(Sender: TObject);
{Selected Header data comes here}
var
  S: string;
  I: integer;
begin
  S := Connection.LastResponse;

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
        ARealm := S;
      end;
    end;
    Exit;
  end;

  {see what's allowed for error 405}
  I := Pos('allow:', LowerCase(S));
  if (I > 0) then
  begin
    Allow := Lowercase(S);
    Exit;
  end;

end;

procedure THTTPForm.HTTPHeaders1Click(Sender: TObject);
begin
  with TInfoForm.Create(Application) do
  try
    Caption := 'HTTP Headers';
    mmoInfo.Clear;
    mmoInfo.Lines.Add('Request');
    mmoInfo.Lines.AddStrings( Self.FHeaderRequestData );

    mmoInfo.Lines.Add('Response');
    mmoInfo.Lines.AddStrings( Self.FHeaderResponseData );
    ShowModal;
  finally
    Free;
  end;
end;

procedure THTTPForm.BackButtonClick(Sender: TObject);
begin
  FrameBrowser.GoBack;
  CheckEnableControls;
end;

procedure THTTPForm.FwdButtonClick(Sender: TObject);
begin
  FrameBrowser.GoFwd;
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
    {$ifdef LogIt}
    LogLine ('Page Completed' + #13#10);
    {$endif}
  end;
end;

procedure THTTPForm.PrintPreviewClick(Sender: TObject);
var
{$ifdef UseOldPreviewForm}
  pf: TPreviewForm;
{$else UseOldPreviewForm}
  pf: TBegaHtmlPrintPreviewForm;
{$endif UseOldPreviewForm}
  Viewer: ThtmlViewer;
  Abort: boolean;
begin
  Viewer := FrameBrowser.ActiveViewer;
  if Assigned(Viewer) then
  begin
{$ifdef UseOldPreviewForm}
    pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
{$else UseOldPreviewForm}
    pf := TBegaHtmlPrintPreviewForm.Create(Self);
    pf.FrameViewer := FrameBrowser;
    Abort := False;
{$endif UseOldPreviewForm}
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

procedure THTTPForm.PageInfo1Click(Sender: TObject);
begin
  with TInfoForm.Create(Application) do
  try
    Caption := 'Page Info';
    mmoInfo.Clear;
    if FrameBrowser.UseQuirksMode then  begin
      mmoInfo.Lines.Add( FrameBrowser.DocumentTitle );
      mmoInfo.Lines.Add( 'Render Mode: Quirks Mode');
    end else begin
      mmoInfo.Lines.Add( FrameBrowser.DocumentTitle );
      mmoInfo.Lines.Add( 'Render Mode: Standards Mode');
    end;
    if Self.FMetaInfo.Count >0 then begin
      mmoInfo.Lines.Add ( 'Meta Information');
      mmoInfo.Lines.Add ( '---');
      mmoInfo.Lines.AddStrings( Self.FMetaInfo );
      mmoInfo.Lines.Add ( '---');
    end;
    ShowModal;
  finally
    Free;
  end;
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

procedure THTTPForm.About1Click(Sender: TObject);
var
  AboutBox: TAboutBox;
begin
  AboutBox := TAboutBox.CreateIt(Self, 'FrameBrowser Indy Demo', 'TFrameBrowser');
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

procedure THTTPForm.FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
begin
  {$ifdef LogIt}
LogLine ('FrameBrowserMeta, HttpEq=' + HttpEq + ', MetaName=' + Name + ', MetaContent=' + Content);
  {$endif}
  FMetaInfo.Add('HttpEq=' + HttpEq + ', MetaName=' + Name + ', MetaContent=' + Content);
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
    else
      if TitleStr <> OldTitle then
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
    else
      if (TimerCount >= StartCount) and not HintVisible then
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

procedure THTTPForm.HTTPRedirect(Sender: TObject; var Dest: String;
  var NumRedirect: Integer; var Handled: boolean; var Method: TIdHTTPMethod);
var
  Proto: string;
  FullUrl: boolean;
begin
  FullURL := True;
  Proto := GetProtocol(UrlBase);
  if IsFullUrl(Dest) then  {it's a full URL}
  begin
    Dest := Normalize(Dest);
    NewLocation := Dest;
  end
  else
  begin
    NewLocation := CombineURL(URLBase, Dest);
    FullURL := False;
  end;
  URLBase := GetUrlBase(NewLocation);

{The following is apparently no longer necessary}
  if (Proto = 'https') or not FullURL or (GetProtocol(UrlBase) = 'https') then
  begin
    Handled := False;   {we will handle it}
    Redirect := True;
  end
  else
   Handled := True;  {IdHTTP will handle it}
end;

procedure THTTPForm.SaveCookies(ASender: TObject; ACookieCollection: TIdCookies);
var
  M : TMemIniFile;
{$ifdef HasCookieCollectionPersistent}
  i : Integer;
  LU : TIdURI;
{$endif}
begin
  if Monitor1 then  {write only if first instance}
  begin
      M := TMemIniFile.Create(Cache+CookieFile );
      try
        m.Clear;
{$ifdef HasCookieCollectionPersistent}
          LU := TIdURI.Create;
          try
            m.Clear;
            for i := 0 to ACookieCollection.Count -1 do
            begin
              if ACookieCollection[i].Persistent then begin
                LU.URI := '';
                LU.Path := ACookieCollection[i].Path;
                LU.Host := ACookieCollection[i].Domain;
                if ACookieCollection[i].Secure then  begin
                  LU.Protocol := 'https'
                end else begin
                  LU.Protocol := 'http';
                end;
                m.WriteString(LU.URI,IntToStr(i),
                  LocalDateTimeToGMT( ACookieCollection[i].CreatedAt ) + '|' +
                  LocalDateTimeToGMT( ACookieCollection[i].LastAccessed ) + '|' +
                  ACookieCollection[i].ServerCookie );
              end;
            end;
          finally
            LU.Free;
          end;
{$endif}
        m.UpdateFile;
      finally
        m.Free;
      end;
  end;
end;

{NOTE about Cookies in Indy 10.

The cookie entine was completely rewritten in Indy 10.

The code does not load and save every cookie.  I do not consider this a bug at all
because:

1) Cookies expire
2) Some cookies are session cookies that are meant to only last for your session.
Those cookies do not have expiration timestamps.


}
procedure ReadCookieValues(m : TMemIniFile;
  AURI : TIdURI;
  AValues : TStrings;
  ACookieCollection: TIdCookieManager);
var i : Integer;
  s, LC, LA : String;
  LCookie : TIdCookie;
begin
  for i := 0 to AValues.Count - 1 do begin
    s := AValues[i];
    Fetch(s,'=');
    LC := Fetch(s,'|');
    LA := Fetch(s,'|');
    LCookie := ACookieCollection.CookieCollection.AddServerCookie(s,AURI);
    if Assigned(LCookie) then begin
      LCookie.CreatedAt := GMTToLocalDateTime(LC);
      LCookie.LastAccessed := GMTToLocalDateTime(LA);
    end;
  end;
end;

procedure THTTPForm.LibraryInformation1Click(Sender: TObject);
var LId : TIdC_ULONG;
begin
  //
  with TInfoForm.Create(Application) do
  try
    Caption := 'Library Information';
    mmoInfo.Lines.Clear;
    //
    //       1 -          1 - bit  0
    //       2 -          2 - bit  1
    //       4 -          4 - bit  2
    //       8 -          8 - bit  3
    //      10 -         16 - bit  4
    //      20 -         32 - bit  5
    //      40 -         64 - bit  6
    //      80 -        128 - bit  7

    //     100 -        256 - bit  8
    //     200 -        512 - bit  9
    //     400 -       1024 - bit 10
    //     800 -       2048 - bit 11
    //    1000 -       4096 - bit 12
    //    2000 -       8192 - bit 13
    //    4000 -      16384 - bit 14
    //    8000 -      32768 - bit 15

    //   10000 -      65536 - bit 16
    //   20000 -     131072 - bit 17
    //   40000 -     262144 - bit 18
    //   80000 -     524288 - bit 19
    //  100000 -    1048576 - bit 20
    //  200000 -    2097152 - bit 21
    //  400000 -    4194304 - bit 22
    //  800000 -    8388608 - bit 23
    // 1000000 -   16777216 - bit 24
    // 2000000 -   33554432 - bit 25
    // 4000000 -   67108864 - bit 26
    // 8000000 -  134217728 - bit 27
    //10000000 -  268435456 - bit 28
    //20000000 -  536870912 - bit 29
    //40000000 - 1073741824 - bit 30
    //80000000 - 2147483648 - bit 31
    {$ifdef UseZLib}
    mmoInfo.Lines.Add('ZLib Version: '+ zlibVersion);
    LId := zlibCompileFlags;
    mmoInfo.Lines.Add('Type sizes, two bits each, 00 = 16 bits, 01 = 32, 10 = 64, 11 = other: ');
    case LId and $03 of
      0 : mmoInfo.Lines.Add( 'Size of uInt : 16-bit');
      1 : mmoInfo.Lines.Add( 'Size of uInt : 32-Bit');
      2 : mmoInfo.Lines.Add( 'Size of uInt : 64-Bit');
      3 : mmoInfo.Lines.Add( 'Size of uInt : Other');
    end;
    case ((LId And $C) div $4) of
      0 : mmoInfo.Lines.Add( 'Size of uLong : 16-bit');
      1 : mmoInfo.Lines.Add( 'Size of uLong : 32-Bit');
      2 : mmoInfo.Lines.Add( 'Size of uLong : 64-Bit');
      3 : mmoInfo.Lines.Add( 'Size of uLong : Other');
    end;
    case ((LId And $30) div $10) of
      0 : mmoInfo.Lines.Add( 'Size of voidpf : 16-bit');
      1 : mmoInfo.Lines.Add( 'Size of voidpf : 32-Bit');
      2 : mmoInfo.Lines.Add( 'Size of voidpf : 64-Bit');
      3 : mmoInfo.Lines.Add( 'Size of voidpf : Other');
    end;
    case ((LId And $C0) div $40) of
      0 : mmoInfo.Lines.Add( 'Size of z_off_t : 16-bit');
      1 : mmoInfo.Lines.Add( 'Size of z_off_t : 32-Bit');
      2 : mmoInfo.Lines.Add( 'Size of z_off_t : 64-Bit');
      3 : mmoInfo.Lines.Add( 'Size of z_off_t : Other');
    end;
    // debug 8
    mmoInfo.Lines.Add( 'Compiler, assembler, and debug options: ');
    if LId and $100 > 0 then  begin
      mmoInfo.Lines.Add( 'DEBUG : True');
    end else begin
//      mmoInfo.Lines.Add( 'DEBUG : False');
    end;
    // asm 9
    if LId and $200 > 0 then begin
      mmoInfo.Lines.Add( 'ASMV or ASMINF - use ASM code : True');
    end else begin
//      mmoInfo.Lines.Add( 'ASMV or ASMINF - use ASM code  : False');
    end;
    //winapi 10
    if LId and $400 > 0 then begin
       mmoInfo.Lines.Add( 'ZLIB_WINAPI - exported functions use the WINAPI calling convention : True');
    end else begin
//       mmoInfo.Lines.Add( 'ZLIB_WINAPI - exported functions use the WINAPI calling convention : False');
    end;
    //11
    //12
    mmoInfo.Lines.Add( 'One-time table building (smaller code, but not thread-safe if true): ');
    if LId and $1000 > 0 then begin
      mmoInfo.Lines.Add( 'BUILDFIXED - build static block decoding tables when needed   : True');
    end else begin
//      mmoInfo.Lines.Add( 'BUILDFIXED - build static block decoding tables when needed  : False');
    end;
    //13
    if LId and $2000 > 0 then begin
      mmoInfo.Lines.Add( 'DYNAMIC_CRC_TABLE - build CRC calculation tables when needed : True');
    end else begin
//      mmoInfo.Lines.Add( 'DYNAMIC_CRC_TABLE - build CRC calculation tables when needed : False');
    end;
    //14
    //15
    //16
    if (LId and ($10000 +$20000)> 0) then begin

    mmoInfo.Lines.Add('Library content (indicates missing functionality): ');
    if LId and $10000 > 0 then begin
      mmoInfo.Lines.Add( 'NO_GZCOMPRESS - gz* functions cannot compress (to avoid linking deflate code when not needed)  : True');
    end else begin
 //     mmoInfo.Lines.Add( 'NO_GZCOMPRESS - gz* functions cannot compress (to avoid linking deflate code when not needed)  : False');
    end;
    //17
    if LId and $20000 > 0 then begin
      mmoInfo.Lines.Add( 'NO_GZIP - deflate can''t write gzip streams, and inflate can''t detect and decode gzip streams (to avoid linking crc code)  : True');
    end else begin
 //     mmoInfo.Lines.Add( 'NO_GZIP - deflate can''t write gzip streams, and inflate can''t detect and decode gzip streams (to avoid linking crc code)  : False');
    end;
    end;
    //20
    if (LId and ($100000 + $200000) > 0) then begin

      mmoInfo.Lines.Add('Operation variations (changes in library functionality): ');

      if LId and  $100000 > 0 then begin
        mmoInfo.Lines.Add( 'PKZIP_BUG_WORKAROUND - slightly more permissive inflate : True');
      end else begin
//      mmoInfo.Lines.Add( 'PKZIP_BUG_WORKAROUND - slightly more permissive inflate : False');
      end;
    //21
      if LId and $200000 > 0 then begin
        mmoInfo.Lines.Add( 'FASTEST - deflate algorithm with only one, lowest compression level : True');
      end else begin
 //     mmoInfo.Lines.Add( 'FASTEST - deflate algorithm with only one, lowest compression level  : False');
      end;
    end;
    //24
    mmoInfo.Lines.Add('The sprintf variant used by gzprintf (zero is best): ');
    if LId and $1000000 > 0 then begin
      mmoInfo.Lines.Add( '0 = vs*, 1 = s* - 1 means limited to 20 arguments after the format : True');
    end else begin
      mmoInfo.Lines.Add( '0 = vs*, 1 = s* - 1 means limited to 20 arguments after the format : False');
    end;
    //25
    if LId and $2000000 > 0 then begin
      mmoInfo.Lines.Add( '0 = *nprintf, 1 = *printf - 1 means gzprintf() not secure!  : True');
    end else begin
      mmoInfo.Lines.Add( '0 = *nprintf, 1 = *printf - 1 means gzprintf() not secure!  : False');
    end;
    //26
    if LId and $2000000 > 0 then begin
      mmoInfo.Lines.Add( '0 = returns value, 1 = void - 1 means inferred string length returned  : True');
    end else begin
      mmoInfo.Lines.Add( '0 = returns value, 1 = void - 1 means inferred string length returned  : False');
    end;
    {$endif}
    ShowModal;
  finally
    Free;
  end;
end;

procedure THTTPForm.LoadCookies(ACookieCollection: TIdCookieManager);
var
  M : TMemIniFile;
  sects : TStringList;
  CookieValues : TStringList;
  i : Integer;
  LU : TIdURI;
begin
  if FileExists(Cache+CookieFile) then
  begin
      M := TMemIniFile.Create(Cache+CookieFile );
      try
        sects := TStringList.Create;
        try
          CookieValues := TStringList.Create;
          LU := TIdURI.Create;
          try
            m.ReadSections(sects);
            for i := 0 to sects.Count - 1 do begin
              m.ReadSectionValues(sects[i],CookieValues);
              LU.URI := sects[i];
              ReadCookieValues(m, LU, CookieValues, ACookieCollection);
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

      {$ifdef LogIt}
procedure THTTPForm.LogTStrings(AStrings: TStrings);
var i : Integer;
begin
  for i := 0 to AStrings.Count -1 do begin
        LogLine (AStrings[i]);
  end;
end;
{$endif}

procedure THTTPForm.AuthorizationEvent(Sender: TObject;
  Authentication: TIdAuthentication; var Handled: Boolean);
begin
  if Authentication is TIdBasicAuthentication then
    with TIdBasicAuthentication(Authentication) do
      ARealm := Realm;
  Handled := False;
end;

{ TModIdLogFile }

procedure TModIdLogFile.LogWriteString(const AText: string);
begin
  if Active then
    inherited;
end;

procedure THTTPForm.FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: ThtString);
begin
  {$ifdef LogIt}
   LogLine ('FrameBrowserScript, Name=' + Name + ', Src=' + Src + ', Script=' + Script);
   {$endif}
end;

end.

