{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2023 by HtmlViewer Team

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
unit FBrDemUnit;

// Let the project file define the appropriate condition:
{.$define UseIndy10}
{.$define UseIcs}

{$include htmlcons.inc}

{$ifdef UseIndy10}
{$include OptionsId10.inc}
{$R 'fbHelpIndy10.res' 'Resources\Indy10\fbHelpIndy10.rc'}
{$endif}

{$ifdef UseIcs}
{$R 'fbHelpIcs7.res' 'Resources\Ics7\fbHelpIcs7.rc'}
{$endif}

{A program to demonstrate the TFrameBrowser component}

interface

uses
{$ifdef LCL}
  LCLIntf, LCLType, LMessages, PrintersDlgs, dynlibs, HtmlMisc,
{$else}
  ShellAPI, WinTypes, WinProcs, MPlayer,
  {$if CompilerVersion >= 15}
    {$ifndef UseVCLStyles}
    XpMan,
    {$endif}
  {$ifend}
  {$IF CompilerVersion >= 30}
//      System.ImageList,
  {$IFEND}
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, ImageList,
  Menus, StdCtrls, Buttons, ExtCtrls, IniFiles, ImgList, ComCtrls, ToolWin,
{$ifndef MultiMediaMissing}
  mmSystem,
{$endif}
{$ifdef UseVCLStyles}
   Vcl.Styles,
   Vcl.Themes,
   Vcl.ActnPopup,
{$endif}
{$ifdef UseOldPreviewForm}
  PreviewForm,
{$else UseOldPreviewForm}
  BegaZoom,
  BegaHtmlPrintPreviewForm,
{$endif UseOldPreviewForm}
  HtmlDemoUtils,
  HtmlGlobals,
  HTMLUn2,
  HTMLSubs,
  URLSubs,
  UrlConn,
  HtmlView,
  FramView,
  FramBrwz,
{$ifdef UseIndy10}
  UrlConId10,
{$endif}
{$ifdef UseIcs}
  UrlConIcs,
{$endif}
  CacheUnit;
const
  MaxHistories = 15;  {size of History list}
  wm_LoadURL = wm_User+124;
  wm_DownLoad = wm_User+125;

type
{$ifdef UseVCLStyles}
  TPopupMenu=class(Vcl.ActnPopup.TPopupActionBar);
{$endif}

  { THTTPForm }

  THTTPForm = class(TForm)
    Connectors: ThtConnectionManager;
    FileConnector: ThtFileConnector;
    Gauge: TProgressBar;
    StatusBarMain: TStatusBar;
    ProgressTimer: TTimer;
    LogDiag: TMenuItem;
    ShowLog: TMenuItem;
    ResourceConnector: ThtResourceConnector;
    MainMenu: TMainMenu;
    HistoryMenuItem: TMenuItem;
    Help1: TMenuItem;
    File1: TMenuItem;
    Openfile1: TMenuItem;
    OpenDialog: TOpenDialog;
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
    About: TMenuItem;
    TitleTimer: TTimer;
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
    ImageList: TImageList;
    Panel3: TPanel;
{$ifndef LCL}
    Animate1: TAnimate;
{$endif}
    PopupMenu1: TPopupMenu;
    ViewImage: TMenuItem;
    CopyImagetoclipboard: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    View1: TMenuItem;
    PageInfo1: TMenuItem;
    N4: TMenuItem;
    LibraryInformation1: TMenuItem;
    Source1: TMenuItem;
    Panel1: TPanel;
    LogHttp: TMenuItem;
    N5: TMenuItem;
    LogScript: TMenuItem;
    GetImagesAsyncly: TMenuItem;
    procedure AboutClick(Sender: TObject);
    procedure AsyncLoadersProgress(Sender: TObject; Done, Total: Integer);
    procedure BackButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure DeleteCacheClick(Sender: TObject);
    procedure DemoInformation1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrameBrowserHistoryChange(Sender: TObject);
    procedure FrameBrowserMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameBrowserProcessing(Sender: TObject; ProcessingOn: Boolean);
    procedure FrameBrowserRightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure FwdButtonClick(Sender: TObject);
    procedure HTTPDocData1(Sender: TObject);
    procedure Openfile1Click(Sender: TObject);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrintFooter(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean);
    procedure PrintHeader(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean);
    procedure PrintPreviewClick(Sender: TObject);
    procedure Proxy1Click(Sender: TObject);
    procedure ReloadClick(Sender: TObject);
    procedure SaveImageAsClick(Sender: TObject);
    procedure SaveURLClick(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure ShowImagesClick(Sender: TObject);
    procedure TitleTimerTimer(Sender: TObject);
    procedure URLComboBoxClick(Sender: TObject);
    procedure URLComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ViewerClear(Sender: TObject);
{$ifdef UNICODE}
    procedure BlankWindowRequest(Sender: TObject; const Target, URL: String);
    procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, RefererX: String; Reload: Boolean;
      var NewURL: String; var DocType: ThtDocType;
      var Stream: TStream);
    procedure GetImageRequest(Sender: TObject; const URL: String; var Stream: TStream);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: String; var Handled: Boolean);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: String);
    procedure FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: string);
    procedure FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: string);
{$else}
    procedure BlankWindowRequest(Sender: TObject; const Target, URL: WideString);
    procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, RefererX: WideString; Reload: Boolean;
      var NewURL: WideString; var DocType: ThtDocType;
      var Stream: TStream);
    procedure GetImageRequest(Sender: TObject; const URL: WideString; var Stream: TStream);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: WideString; var Handled: Boolean);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: WideString);
    procedure FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: WideString);
    procedure FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: WideString);
{$endif}
    procedure HTTPHeaders1Click(Sender: TObject);
    procedure PageInfo1Click(Sender: TObject);
    procedure LibraryInformation1Click(Sender: TObject);
    procedure Source1Click(Sender: TObject);
    function ConnectorsGetAuthorization(Connection: ThtConnection; TryRealm: Boolean): Boolean;
    procedure LogDiagClick(Sender: TObject);
    procedure ShowLogClick(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure LogHttpClick(Sender: TObject);
    procedure LogScriptClick(Sender: TObject);
    procedure GetImagesAsynclyClick(Sender: TObject);
  private
    { Private declarations }
    URLBase: string;
    Histories: array[0..MaxHistories-1] of TMenuItem;
    DiskCache: ThtDiskCache;
    CurrentLocalFile,
    DownLoadUrl: string;
    Reloading: Boolean;
    FoundObject: TImageObj;
    FoundObjectName: string;
    NewWindowFile: string;
    AnAbort: Boolean;

    AStream: TMemoryStream;
    AsyncLoaders: ThtUrlDocLoaderThreadList;
    AsyncLoading: Boolean;
{$ifdef UseIndy10}
    HttpConnector: ThtIndyHttpConnector;
{$endif}
{$ifdef UseIcs}
    HttpConnector: ThtIcsHttpConnector;
{$endif}
    Connection: ThtConnection;

    TimerCount: Integer;
    OldTitle: ThtString;
    HintWindow: ThtHintWindow;
    HintVisible: Boolean;
    TitleViewer: THtmlViewer;
    FMetaInfo : TStrings;
{$ifdef UseVCLStyles}
    sepVCLStyles,
    VCLStyles1 : TMenuItem;

    procedure VCLStyleClick(Sender: TObject);
{$endif}
    procedure EnableControls;
    procedure DisableControls;
    procedure HistoryClick(Sender: TObject);
    procedure WMLoadURL(var Message: TMessage); message WM_LoadURL;
    procedure WMDownLoad(var Message: TMessage); message WM_DownLoad;
    procedure CheckEnableControls;
    procedure ClearProcessing;
    procedure Progress(Num, Den: integer);
{$ifdef LCL}
{$else}
    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
{$endif}
    procedure CheckException(Sender: TObject; E: Exception);
    procedure CloseHints;
    procedure FrameBrowserFileBrowse(Sender, Obj: TObject; var S: ThtString);
    procedure UpdateCaption;
    procedure LoadedAsync(Sender: ThtUrlDoc; Receiver: TObject);
    procedure DownLoad(const Url: String);
    procedure LoadIniFile;
    procedure SaveIniFile;
    function DocumentTitle(Viewer: THtmlViewer): ThtString; overload;
    function DocumentTitle(Viewer: TFrameBrowser): ThtString; overload;
  protected
    procedure UpdateActions; override;
  public
    { Public declarations }
    FIniFilename: string;

    procedure LoadURL; overload;
    procedure LoadURL(const URL: String); overload;
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
  Htmlabt,
  AuthorizeFormUnit,
  DownloadFormUnit,
  InfoFormUnit,
  LogFormUnit,
  ProxyFormUnit;

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$if CompilerVersion < 15}
    {$R manifest.res}
  {$ifend}
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

{$ifdef LCL}
{$else}
//-- BG ---------------------------------------------------------- 16.08.2015 --
procedure THTTPForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  WinCtrl: TWinControl;
begin
  if Msg.message = WM_MOUSEWHEEL then
  begin
    WinCtrl := FindVCLWindow(Point(Word(Msg.lParam), HiWord(Msg.lParam)));
    if (WinCtrl is TPaintPanel) {$ifndef UseOldPreviewForm} or (WinCtrl is TBegaZoomBox) {$endif UseOldPreviewForm} then
    begin
      // perform mouse wheel scrolling for the control under the mouse:
      WinCtrl.Perform(CM_MOUSEWHEEL, Msg.WParam, Msg.LParam);
      Handled := True;
    end;
  end;
end;
{$endif}

{----------------THTTPForm.FormCreate}
procedure THTTPForm.FormCreate(Sender: TObject);
var
  DX, DY: Double;
  I: Integer;
  Styles: TStringList;
{$ifdef UseVCLStyles}
  m : TMenuItem;
{$endif}
begin
  BoundsRect := htGetNiceFormSize( Monitor, PixelsPerInch );

  OpenDialog.Filter := htStringToString(GetFileMask);
  FMetaInfo := TStringList.Create;
{$ifdef has_StyleElements}
  TStyleManager.AnimationOnControls := True;
{$Endif}
{$ifdef HasGestures}
  FrameBrowser.Touch.InteractiveGestureOptions := [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia];
  FrameBrowser.Touch.InteractiveGestures := [igPan];
{$endif}

  Cache := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)+'Cache');
  DiskCache := ThtDiskCache.Create(Cache);
  SaveDialog.InitialDir := Cache;
  StatusBarMain.Panels[0].Text := '';

{Monitor1 will be set if this is the first instance opened}
  Monitor1 := True;
  try
    AssignFile(Mon, Cache+'Monitor.txt');  {a monitor file for optional use}
    Rewrite(Mon);
  except
    Monitor1 := False;   {probably open in another instance}
  end;
{$ifdef UseVCLStyles}
  sepVCLStyles := TMenuItem.Create(Options1);
  sepVCLStyles.Caption := '-';
  Options1.Add(sepVCLStyles);
  VCLStyles1 := TMenuItem.Create(Options1);
  VCLStyles1.Caption := '&VCL Styles';
  Options1.Add(VCLStyles1);
  if TStyleManager.Enabled then
  begin
    Styles := TStringList.Create(dupIgnore, True, False);
    try
      for i := Low(TStyleManager.StyleNames) to High(TStyleManager.StyleNames) do
        Styles.AddObject( TStyleManager.StyleNames[i], TObject(i) );

      for i := 0 to Styles.Count - 1 do
      begin
        m := TMenuItem.Create(VCLStyles1);
        m.Caption := Styles.Strings[i];
        m.Tag := Integer(Styles.Objects[i]);
        m.OnClick := VCLStyleClick;
        m.RadioItem := True;
        VCLStyles1.Add(m);
      end;
    finally
      Styles.Free;
    end;
  end;
{$endif}

  AStream := TMemoryStream.Create;

{$ifdef LCL}
  // OnClick also handles clicks on dropdown button.
  UrlComboBox.OnEditingDone := UrlComboBox.OnClick;
  UrlComboBox.OnSelect := UrlComboBox.OnClick;
  UrlComboBox.OnClick := nil;
{$else}
  {Load animation from resource}
  Animate1.ResName := 'StarCross';
{$ENDIF}

{$ifdef ver140}   {delphi 6}
  URLComboBox.AutoComplete := False;
{$endif}

  AsyncLoaders := ThtUrlDocLoaderThreadList.Create(Self);
  ASyncLoaders.OnProgress := ASyncLoadersProgress;

{$ifdef UseIndy10}
  HttpConnector := ThtIndyHttpConnector.Create(Self);
{$endif}
{$ifdef UseIcs}
  HttpConnector := ThtIcsHttpConnector.Create(Self);
{$endif}
  HttpConnector.ConnectionManager := Connectors;
  HttpConnector.OnGetAuthorization := ConnectorsGetAuthorization;
  HttpConnector.ProxyPort := '80';
  HttpConnector.CookieFile := Cache + CookieFile;

  // needs HttpConnector <> nil:
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

  FIniFilename := ChangeFileExt(Application.Exename, '.ini');
  LoadIniFile;

{$ifdef LCL}
{$else}
  DragAcceptFiles(Handle, True);
  Application.OnMessage := AppMessage;
{$endif}
  Application.OnException := CheckException;

{$ifdef M4Viewer}
  ProtocolHandler := M4ProtocolHandler;
{$endif}
  HintWindow := ThtHintWindow.Create(Self);
//  HintWindow.Color := $C0FFFF;
  FrameBrowser.OnFileBrowse := FrameBrowserFileBrowse;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPForm.LoadIniFile;
var
  IniFile: TIniFile;
  SL: TStringList;
  I, J, LTop, LLeft, LWidth, LHeight: Integer;
  Rect: TRect;
begin
  IniFile := TIniFile.Create(FIniFileName);
  try
    Rect.Top    := IniFile.ReadInteger('HTTPForm', 'Top'           , Top     );
    Rect.Left   := IniFile.ReadInteger('HTTPForm', 'Left'          , Left    );
    Rect.Width  := IniFile.ReadInteger('HTTPForm', 'Width'         , Width   );
    Rect.Height := IniFile.ReadInteger('HTTPForm', 'Height'        , Height  );
    BoundsRect  := Rect;

    GetImagesAsyncly.Checked := IniFile.ReadBool('Options', 'GetImagesAsyncly', GetImagesAsyncly.Checked);

    LWidth  := Width  div 2;
    LHeight := Height div 2;
    LTop    := Top   + LWidth  div 2;
    LLeft   := Left  + LHeight div 2;

    LTop    := IniFile.ReadInteger('LogForm', 'Top'   , LTop   );
    LLeft   := IniFile.ReadInteger('LogForm', 'Left'  , LLeft  );
    LWidth  := IniFile.ReadInteger('LogForm', 'Width' , LWidth );
    LHeight := IniFile.ReadInteger('LogForm', 'Height', LHeight);

    LogForm := TLogForm.Create(Self, LTop, LLeft, LWidth, LHeight);

    ShowLog.Checked   := IniFile.ReadBool('LogForm', 'ShowLogWindow' , ShowLog.Checked)  ;
    LogDiag.Checked   := IniFile.ReadBool('LogForm', 'LogDiagnostics', LogDiag.Checked  );
    LogHttp.Checked   := IniFile.ReadBool('LogForm', 'LogHttpHeader' , LogHttp.Checked  );
    LogScript.Checked := IniFile.ReadBool('LogForm', 'LogHttpScript' , LogScript.Checked);

    LogForm.Visible := ShowLog.Checked;
    LogForm.LogActive[laDiag] := LogDiag.Checked;
    LogForm.LogActive[laHttpHeader] := LogHttp.Checked;
    LogForm.LogActive[laHttpScript] := LogScript.Checked;

    HttpConnector.ProxyServer   := IniFile.ReadString('Proxy'   , 'ProxyHost'    , HttpConnector.ProxyServer   );
    HttpConnector.ProxyPort     := IniFile.ReadString('Proxy'   , 'ProxyPort'    , HttpConnector.ProxyPort     );
    HttpConnector.ProxyUsername := IniFile.ReadString('Proxy'   , 'ProxyUsername', HttpConnector.ProxyUsername );
    HttpConnector.ProxyPassword := IniFile.ReadString('Proxy'   , 'ProxyPassword', HttpConnector.ProxyPassword );
    HttpConnector.UserAgent     := IniFile.ReadString('Settings', 'UserAgent'    , HttpConnector.UserAgent     );

    SL := TStringList.Create;
    try
      IniFile.ReadSectionValues('Favorites', SL);
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
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPForm.SaveIniFile;
var
  IniFile: TIniFile;
  I: Integer;
begin       {save only if this is the first instance}
  IniFile := TIniFile.Create(FIniFileName);
  try
    IniFile.WriteInteger('HTTPForm', 'Top'          , Top           );
    IniFile.WriteInteger('HTTPForm', 'Left'         , Left          );
    IniFile.WriteInteger('HTTPForm', 'Width'        , Width         );
    IniFile.WriteInteger('HTTPForm', 'Height'       , Height        );

    IniFile.WriteBool('Options', 'GetImagesAsyncly', GetImagesAsyncly.Checked);

    IniFile.WriteInteger('LogForm', 'Top'   , LogForm.Top   );
    IniFile.WriteInteger('LogForm', 'Left'  , LogForm.Left  );
    IniFile.WriteInteger('LogForm', 'Width' , LogForm.Width );
    IniFile.WriteInteger('LogForm', 'Height', LogForm.Height);

    IniFile.WriteBool('LogForm', 'ShowLogWindow' , ShowLog.Checked  );
    IniFile.WriteBool('LogForm', 'LogDiagnostics', LogDiag.Checked  );
    IniFile.WriteBool('LogForm', 'LogHttpHeader' , LogHttp.Checked  );
    IniFile.WriteBool('LogForm', 'LogHttpScript' , LogScript.Checked);

    IniFile.WriteString('Proxy'   , 'ProxyHost'    , HttpConnector.ProxyServer   );
    IniFile.WriteString('Proxy'   , 'ProxyPort'    , HttpConnector.ProxyPort     );
    IniFile.WriteString('Proxy'   , 'ProxyUsername', HttpConnector.ProxyUsername );
    IniFile.WriteString('Proxy'   , 'ProxyPassword', HttpConnector.ProxyPassword );
    IniFile.WriteString('Settings', 'UserAgent'    , HttpConnector.UserAgent     );

    IniFile.EraseSection('Favorites');
    for I := 0 to UrlCombobox.Items.Count - 1 do
      IniFile.WriteString('Favorites', 'Url'+IntToStr(I), UrlCombobox.Items[I]);
  finally
    IniFile.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure THTTPForm.UpdateActions;
begin
  inherited;
  ShowLog.Checked := LogForm.Visible;
end;

//-- BG ---------------------------------------------------------- 31.05.2016 --
function THTTPForm.DocumentTitle(Viewer: THtmlViewer): ThtString;
begin
  if Viewer.DocumentTitle <> '' then
    Result := Viewer.DocumentTitle
  else if Viewer.URL <> '' then
    Result := Viewer.URL
  else if (Viewer.CurrentFile <> '') and (Copy(Viewer.CurrentFile, 1, 9) <> 'source://') then
    Result := Viewer.CurrentFile
  else
    Result := '';
end;

//-- BG ---------------------------------------------------------- 31.05.2016 --
function THTTPForm.DocumentTitle(Viewer: TFrameBrowser): ThtString;
begin
  if Viewer.DocumentTitle <> '' then
    Result := Viewer.DocumentTitle
  else if Viewer.URL <> '' then
    Result := Viewer.URL
  else if (Viewer.CurrentFile <> '') and (Copy(Viewer.CurrentFile, 1, 9) <> 'source://') then
    Result := Viewer.CurrentFile
  else
    Result := '';
end;

procedure THTTPForm.UpdateCaption;
var
  Title, Version, Cap: ThtString;
begin
  Title := DocumentTitle(FrameBrowser);
  if HttpConnector <> nil then
    Version := HttpConnector.Version;
  Cap := 'FrameBrowser/' + Version + ' Demo';
  if Title <> '' then
    Cap := Cap + ' - ' + Title;
{$ifdef LCL}
  Caption := UTF8Encode(Cap);
{$else}
  Caption := Cap;
{$endif}
end;

{----------------THTTPForm.FormDestroy}
procedure THTTPForm.FormDestroy(Sender: TObject);
begin
  AStream.Free;

  if Monitor1 then
  begin
    CloseFile(Mon);
    DiskCache.Flush;
    SaveIniFile;
    HttpConnector.SaveCookies;
  end;

  DiskCache.Free;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPForm.LoadURL;
{initiate loading of a main document}
var
  URL: String;
begin
  URL := UrlCombobox.Text;
  URL := Normalize(URL);
  URLBase := GetUrlBase(URL);
  DisableControls;
  StatusBarMain.Panels[0].Text := '';
  StatusBarMain.Panels[1].Style := psText;
  StatusBarMain.Panels[1].Text := '';
  try
  {the following initiates one or more GetPostRequest's}
    FrameBrowser.LoadURL(URL);
    Reloading := False;
  finally
    CheckEnableControls;
    FrameBrowser.SetFocus;
  end;
end;

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPForm.LoadURL(const URL: String);
begin
  UrlCombobox.Text := URL;
  LoadURL;
end;

{----------------THTTPForm.FrameBrowserFileBrowse}
//This fires when the user hits a file-browse button on a form.

procedure THTTPForm.FrameBrowserFileBrowse(Sender, Obj: TObject; var S: ThtString);
var
  LFile : String;
begin
  if PromptForFileName(LFile, 'Any File |*.*', '', 'Select File', '', False) then
    S := LFile;
end;

//-- BG ---------------------------------------------------------- 18.05.2016 --
function THTTPForm.ConnectorsGetAuthorization(Connection: ThtConnection; TryRealm: Boolean): Boolean;
var
  Username, Password: string;
begin
  begin
    Username := Connection.Username;
    Password := Connection.Password;
    Result := TAuthorizeForm.Execute(TryRealm and (Connection.Realm <> ''), Connection.Realm, Username, Password);
    if Result then
    begin
      Connection.Username := Username;
      Connection.Password := Password;
      Connection.BasicAuthentication := True;
    end;
  end;
end;

{----------------THTTPForm.FrameBrowserGetPostRequestEx}

procedure THTTPForm.FrameBrowserGetPostRequestEx(Sender: TObject;
  IsGet: Boolean; const URL, Query, EncType, RefererX: ThtString;
  Reload: Boolean; var NewURL: ThtString; var DocType: ThtDocType;
  var Stream: TStream);
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
  Method: array [Boolean] of String = ( 'POST', 'GET' );
var
  S, URL1, CacheFileName, Query1, LastUrl, Protocol, NewUrlDummy: string;
  Error, IsHttpConnection: Boolean;
  RedirectCount: integer;
  HttpConnection: THTTPConnection;
  DownLoad: ThtUrlDoc;
begin
  CloseHints;   {may be a hint window open}
  StatusBarMain.Panels[0].Text := '';
  StatusBarMain.Panels[1].Text := '';
  Error := False;
  AnAbort := False;
  Progress(0, 0);
  Query1 := Query;
  URL1 := DecodeURL(Normalize(URL));
  URLBase := GetUrlBase(URL1);
  DisableControls;
  AStream.Clear;
  RedirectCount := 0;

  SetLength(CacheFileName, 0);
  SetLength(NewUrlDummy, 0);     {will change if document referenced by URL has been relocated}
  if Reloading or Reload then
    {don't want a cache file}
    DiskCache.RemoveFromCache(URL1)
  else
    {if in cache already, get the cache filename}
    DiskCache.GetCacheFilename(URL1, CacheFileName, DocType, NewUrlDummy);

  if (Length(CacheFileName) > 0) and FileExists(CacheFileName) then
  begin
    AStream.LoadFromFile(CacheFileName);
    NewUrl := NewUrlDummy;
    if LogForm.LogActive[laDiag] then
      LogForm.Log('FrameBrowserGetPostRequestEx: from cache ' + CacheFileName);
  end
  else
  begin       {it's not in cache}
    Protocol := GetProtocol(URL1);
    if Connectors.TryCreateConnection(Protocol, Connection) then
      try
        if LogForm.LogActive[laDiag] then
          LogForm.Log('FrameBrowserGetPostRequestEx: ' + Method[IsGet] + ' ' + Url1);
        IsHttpConnection := Connection is THTTPConnection;
        HttpConnection := nil;
        if IsHttpConnection then
          HttpConnection := Connection as THTTPConnection;
        Connection.OnDocData := HTTPDocData1;
        LastURL := Url1;
        DownLoad := Connection.CreateUrlDoc(not IsGet, URL1, Query1, EncType, RefererX);
        try
          try
            Connection.LoadDoc(DownLoad);
            DocType := DownLoad.DocType;
            AStream.LoadFromStream(DownLoad.Stream);
          except
{$ifndef UseSSL}
            On ESpecialException do
            begin
              if LogForm.LogActive[laDiag] then
                LogForm.Log('FrameBrowserGetPostRequestEx: Special Exception');
              Connection.Free;   {needs to be reset}
              Connection := Nil;
              Raise;
            end;
{$endif}
            On E: Exception do
            begin
              if LogForm.LogActive[laDiag] then
                LogForm.Log('FrameBrowserGetPostRequestEx: Exception - ' + E.Message);
              if AnAbort then
                raise ESpecialException.Create('Abort on user request');

              Error := True;
              if Connection is THTTPConnection then
              begin
                if DownLoad.Stream.Size > 0 then   {sometimes error messages come in RcvdStream}
                  AStream.LoadFromStream(DownLoad.Stream)
                else
                begin
                  if RedirectCount >= MaxRedirect then
                      S := 'Excessive Redirects'
                  else
                     S := HttpConnection.ReasonPhrase+'<p>Statuscode: '+IntToStr(HttpConnection.StatusCode);
                  AStream.Write(S[1], Length(S) * SizeOf(S[1]));
                end;
                DocType := HTMLType;
              end
              else  {other connection types}
              begin
                S :=
                  '<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + LastURL +
                  '<p>Cause: ' + E.Message; {load an error message}

                AStream.Write(S[1], Length(S) * SizeOf(S[1]));
              end;
            end;
          end;
          StatusBarMain.Panels.Items[0].Text := 'Received ' + IntToStr(AStream.Size) + ' bytes';

          CacheFileName := DiskCache.AddNameToCache(LastUrl, Download.NewUrl, DocType, Error);
          if Length(CacheFileName) > 0 then
            try
              AStream.SaveToFile(CacheFileName);   {it's now in cache}
            except
            end;
        finally
          NewURL := Download.NewUrl;   {in case location has been changed}
          FreeAndNil(Download);
        end;
      finally
        FreeAndNil(Connection);
      end
    else
    begin
      S := Format('Unsupported protocol ''%s''. Supported protocols are %s.', [Protocol, Connectors.AllProtocols]);
      if LogForm.LogActive[laDiag] then
        LogForm.Log('FrameBrowserGetPostRequestEx: Don''t know how to ' +  Method[IsGet] + ' ''' + Url1 + '''. Cause: ' + S );

//      if Sender is TFrameBrowser then   {main document display}
//        raise ESpecialException.Create(S);
//      {else other errors will get displayed as HTML file}

      S := '<p><img src="res:///qw%&.bmp" alt="Error"> Don''t know how to ' +  Method[IsGet] + ' ''' + Url1 + '''<p>Cause: ' + S; {load an error message}
      AStream.Write(S[1], Length(S) * SizeOf(S[1]));
      DocType := HTMLType;
    end;
  end;
  AStream.Position := 0;
  Stream := AStream;
end;

{----------------THTTPForm.GetImageRequest}
procedure THTTPForm.GetImageRequest(Sender: TObject; const URL: ThtString; var Stream: TStream);
{the OnImageRequest handler}
var
  Protocol, S: string;
  Connection: ThtConnection;
  DownLoad: ThtUrlDoc;
  DocType: ThtmlFileType;
  Dummy: string;
  Viewer: THtmlViewer;
begin
  try
    SetLength(S, 0);
    if Reloading then
      DiskCache.RemoveFromCache(URL)
    else
      DiskCache.GetCacheFilename(URL, S, DocType, Dummy);  {see if image is in cache file already}

    if (Length(S) > 0) and FileExists(S) and (DocType = ImgType) then
    begin                               {yes, it is}
      AStream.LoadFromFile(S);
      Stream := AStream;      {return image immediately}
      if LogForm.LogActive[laDiag] then
        LogForm.Log('GetImageRequest, from Cache: ' + S);
    end
    else
    begin          {get the image asynchronously }
      Protocol := GetProtocol(URL);
      if Connectors.TryCreateConnection(Protocol, Connection) then
      begin
        DownLoad := Connection.CreateUrlDoc(False, URL, '', '', '');
        if (Connection is THTTPConnection) and GetImagesAsyncly.Checked then
        begin
          if Sender is THtmlViewer then
            Viewer := THtmlViewer(Sender)
          else
            Viewer := FrameBrowser.ActiveViewer;
          if LogForm.LogActive[laDiag] then
            LogForm.Log('GetImageRequest, get asynchronously: ' + URL);
          AsyncLoading := True;
          AsyncLoaders.AddLoader(ThtUrlDocLoaderThread.Create(Connection, DownLoad, LoadedAsync, Viewer));
          Stream := WaitStream;   {wait indicator}
        end
        else
          try
            Connection.LoadDoc(DownLoad);
            DocType := DownLoad.DocType;
            AStream.LoadFromStream(DownLoad.Stream);
            Stream := AStream;
          finally
            DownLoad.Free;
            Connection.Free;
          end;
      end
      else
      begin
        S := Format('Unsupported protocol ''%s''. Supported protocols are %s.', [Protocol, Connectors.AllProtocols]);
        if LogForm.LogActive[laDiag] then
          LogForm.Log('GetImageRequest: Don''t know how to get ''' + Url + '''. Cause: ' + S );

  //      if Sender is TFrameBrowser then   {main document display}
  //        raise ESpecialException.Create(S);
  //      {else other errors will get displayed as HTML file}
      end;
    end;
  except
    Stream := ErrorStream;
  end;
end;

//-- BG ---------------------------------------------------------- 28.08.2007 --
procedure THTTPForm.LoadedAsync(Sender: ThtUrlDoc; Receiver: TObject);
var
  Viewer: THtmlViewer absolute Receiver;
  CacheFileName: String;
  Error: Boolean;
begin
  Error := Sender.Status <> ucsLoaded;
  if Receiver is THtmlViewer then
    if Error then
      Viewer.InsertImage(Sender.Url, TStream(nil))
    else
      Viewer.InsertImage(Sender.Url, Sender.Stream);

  CacheFileName := DiskCache.AddNameToCache(Sender.Url, Sender.NewUrl, Sender.DocType, Error);
  if Length(CacheFileName) > 0 then
    try
      Sender.SaveToFile(CacheFileName)
    except
    end;
  Sender.Free;
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
procedure THTTPForm.FrameBrowserHistoryChange(Sender: TObject);
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
      if Histories[I] <> nil then
        with Histories[I] do
          if I < History.Count then
          begin
            Cap := History.Strings[I];  {keep local file name}
            if TitleHistory[I] <> '' then
              Cap := Cap + '--' + TitleHistory[I];
            Caption := copy(Cap, 1, 80);
            Visible := True;
            Checked := I = HistoryIndex;
          end
          else
            Histories[I].Visible := False;
    UpdateCaption;
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
  LoadURL;
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
    LoadURL('file:///'+DosToHTMLNoSharp(OpenDialog.Filename));
    UpdateCaption;
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
begin
  if LogForm.LogActive[laDiag] then
    LogForm.Log ('HotSpotTargetClick: ' + URL);
  Protocol := GetProtocol(URL);
  if Protocol = 'mailto' then
  begin
  {call mail program}
    Handled := OpenDocument(URL);
    Exit;
  end;
{Note: it would be nice to handle ftp protocol here also as some downloads use
 this protocol}

  Ext := Lowercase(GetURLExtension(URL));
  if Pos('http', Protocol) = 1 then
  begin
    if (Ext = 'zip') or (Ext = '7z') or (Ext = 'gz') or (Ext = 'tar') or
       (Ext = 'exe') or (Ext = 'msi') or (Ext = 'pdf') or (Ext = 'iso') or
       (Ext = 'doc') or (Ext = 'xls') or (Ext = 'odt') or (Ext = 'ods')
    then
    begin
    {download can't be done here.  Post a message to do it later at WMDownload}
      DownLoadURL := URL;
      PostMessage(handle, wm_DownLoad, 0, 0);
      Handled := True;
      Exit;
    end;
  end;

  if Protocol = 'file' then
  begin
    S := URL;
    K := Pos(' ', S);     {look for parameters}
    if K = 0 then
      K := Pos('?', S);  {could be '?x,y' , etc}
    if K > 0 then
    begin
      Params := Copy(S, K+1, 255); {save any parameters}
      SetLength(S, K-1);            {truncate S}
    end
    else
      Params := '';
    S := HTMLToDos(S);
    if Ext = 'wav' then
    begin
      Handled := True;
{$ifndef MultiMediaMissing}
      sndPlaySound(StrPCopy(PC, S), snd_ASync);
{$endif}
    end
    else
    if Ext = 'exe' then
      Handled := StartProcess(S, Params)
    else if (Ext = 'mid') or (Ext = 'avi') then
      Handled := OpenDocument(Params);
    {else ignore other extensions}
//    UrlComboBox.Text := URL;
    Exit;
  end;

  UrlComboBox.Text := URL;   {other protocall}
end;

procedure THTTPForm.CheckEnableControls;
begin
  if not FrameBrowser.Processing and not Connection.Processing {(not Assigned(Connection) or (Connection.State in [httpReady, httpNotConnected])) and (HTTPList.Count = 0)} then
  begin
    EnableControls;
    StatusBarMain.Panels[1].Text   := 'DONE';
  end;
end;

procedure THTTPForm.ClearProcessing;
//var
//  I: integer;
begin
//  for I := 0 to HTTPList.Count-1 do   {free any Image HTTPs existing}
//    with TImageHTTP(HTTPList.Items[I]) do
//    begin
//      Free;
//    end;
//  HTTPList.Clear;
//  for I := 0 to Pending.Count-1 do
//  begin
//    ImageRec(Pending.Items[I]).Stream.Free;
//    ImageRec(Pending.Items[I]).Free;
//  end;
//  Pending.Clear;
end;

procedure THTTPForm.ViewerClear(Sender: TObject);
{A ThtmlViewer is about to be cleared. Cancel any image processing destined for it}
//var
//  I: integer;
//  Vw: ThtmlViewer;
begin
//  Vw := Sender as ThtmlViewer;
//  for I := HTTPList.Count-1 downto 0 do
//    with TImageHTTP(HTTPList.Items[I]) do
//      if ImRec.Viewer = Vw then
//      begin
//        Free;
//        HTTPList.Delete(I);
//      end;
//  for I := Pending.Count-1 downto 0 do
//    with ImageRec(Pending.Items[I]) do
//      if Viewer = Vw then
//      begin
//        Stream.Free;
//        Free;
//        Pending.Delete(I);
//      end;
end;

procedure THTTPForm.DeleteCacheClick(Sender: TObject);
begin
  DiskCache.EraseCache;
end;

{$ifdef LCL}
{$else}
procedure THTTPForm.wmDropFiles(var Message: TMessage);
{handles dragging of file into browser window}
var
  S: string;
begin
  SetLength(S, 1024);
  SetLength(S, DragQueryFile(Message.WParam, 0, @S[1], 1024));
  DragFinish(Message.WParam);
  LoadURL('file:///'+DosToHTMLNoSharp(S));
  CurrentLocalFile := FrameBrowser.CurrentFile;
  Message.Result := 0;
end;
{$endif}

procedure THTTPForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure THTTPForm.ShowImagesClick(Sender: TObject);
begin
  FrameBrowser.ViewImages := not FrameBrowser.ViewImages;
  ShowImages.Checked := FrameBrowser.ViewImages;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure THTTPForm.ShowLogClick(Sender: TObject);
begin
  ShowLog.Checked := not ShowLog.Checked;
  LogForm.Visible := ShowLog.Checked;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure THTTPForm.LogDiagClick(Sender: TObject);
begin
  LogDiag.Checked := not LogDiag.Checked;
  LogForm.LogActive[laDiag] := LogDiag.Checked
end;

//-- BG ---------------------------------------------------------- 25.05.2016 --
procedure THTTPForm.LogHttpClick(Sender: TObject);
begin
  LogHttp.Checked := not LogHttp.Checked;
  LogForm.LogActive[laHttpHeader] := LogHttp.Checked
end;

//-- BG ---------------------------------------------------------- 25.05.2016 --
procedure THTTPForm.LogScriptClick(Sender: TObject);
begin
  LogScript.Checked := not LogScript.Checked;
  LogForm.LogActive[laHttpScript] := LogScript.Checked
end;

procedure THTTPForm.Source1Click(Sender: TObject);
begin
  if FrameBrowser.ActiveViewer <> nil then
  begin
    with TInfoForm.Create(Application) do
    try
      Caption := 'Source of ''' + DocumentTitle(FrameBrowser.ActiveViewer) + '''';

      mmoInfo.Lines.Text := FrameBrowser.ActiveViewer.Text;
      ShowModal;
    finally
      Free;
    end;
//    FrameBrowser.ActiveViewer.LoadFromString(FrameBrowser.ActiveViewer.Text, DocumentTitle(FrameBrowser.ActiveViewer), TextType);
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

//-- BG ---------------------------------------------------------- 19.05.2016 --
procedure THTTPForm.DownLoad(const Url: String);
{Handle download of file}
begin
  SaveDialog.Filename := GetURLFilenameAndExt(URL);
  if SaveDialog.Execute then
  begin
    with TDownloadForm.Create(Self) do
      try
        Filename    := SaveDialog.Filename;
        Connections := Connectors;
        DownLoadURL := URL;
        ShowModal;
      finally
        Free;
      end;
  end;
end;

procedure THTTPForm.WMDownLoad(var Message: TMessage);
{Handle download of file}
begin
  DownLoad(DownLoadURL);
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
  if Pos(' ', S) > 0 then
    S := '"' + S + '"';

  StartProcess(ParamStr(0), S);
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
  begin
    Key := #0;
    LoadURL;
  end;
end;

procedure THTTPForm.URLComboBoxClick(Sender: TObject);
begin
  if URLComboBox.Text <> '' then
  begin
    FMetaInfo.Clear;
    LoadURL;
  end;
end;

{----------------THTTPForm.RightClick}
procedure THTTPForm.FrameBrowserRightClick(Sender: TObject; Parameters: TRightClickParameters);
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
    if (FoundObject <> Nil) and (FoundObject.Graphic <> Nil) then
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
begin
  DownLoad(FoundObjectName);
end;

procedure THTTPForm.OpenInNewWindowClick(Sender: TObject);
begin
  BlankWindowRequest(Sender, '_blank', NewWindowFile);
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
  StatusBarMain.Panels[0].Text := 'Text: ' + IntToStr(Connection.ReceivedSize) + ' bytes';
  Progress(Connection.ReceivedSize, Connection.ExpectedSize);
end;

procedure THTTPForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Connection) then
    Connection.Abort;
  ClearProcessing;
end;

procedure THTTPForm.Progress(Num, Den: Integer);
var
  Percent: integer;
begin
  if Den = 0 then
    Percent := 100
  else
    Percent := (100*Num) div Den;
  Gauge.Position := Percent;
  if Gauge.Position = 100 then
    ProgressTimer.Enabled := True
  else
    Gauge.Visible := True;
end;

//-- BG ---------------------------------------------------------- 24.05.2016 --
procedure THTTPForm.ProgressTimerTimer(Sender: TObject);
begin
  ProgressTimer.Enabled := False;
  if Gauge.Position = 100 then
    Gauge.Visible := False;
end;

procedure THTTPForm.CheckException(Sender: TObject; E: Exception);
begin
  if E is ESpecialException then
  begin
    ShowMessage(E.Message);
    AnAbort := False;
  end
  else
  begin
//{$IFDEF LogIt}
//    if E is EIdHTTPProtocolException then
//       LogForm.Log( IntToStr(EIdHTTPProtocolException(E).ErrorCode)+ ' - '+ EIdHTTPProtocolException(E).ErrorMessage );
//{$ENDIF}
    Application.ShowException(E);
  end;
end;

procedure THTTPForm.DisableControls;
begin
//  if not Gauge.Visible then
  begin
    URLCombobox.Enabled := False;
    CancelButton.Enabled := True;
    ReloadButton.Enabled := False;
{$ifdef LCL}
{$else}
    Animate1.Visible := True;
    Animate1.Play(1, Animate1.FrameCount, 0);
{$endif}
    Gauge.Visible := True;
  end;
end;

procedure THTTPForm.EnableControls;
begin
//  if Gauge.Visible then
  begin
    URLCombobox.Enabled := True;
    CancelButton.Enabled := False;
    ReloadButton.Enabled := FrameBrowser.CurrentFile <> '';
    Reloading := False;
    if not AsyncLoading then
    begin
{$ifdef LCL}
{$else}
      Animate1.Active := False;
      Animate1.Visible := False;
{$endif}
//      Gauge.Visible := False;
    end;
  end;
end;

procedure THTTPForm.HTTPHeaders1Click(Sender: TObject);
begin
//  with TInfoForm.Create(Application) do
//  try
//    Caption := 'HTTP Headers';
//    mmoInfo.Clear;
//    mmoInfo.Lines.Add('Request');
//    mmoInfo.Lines.AddStrings( Self.FHeaderRequestData );
//
//    mmoInfo.Lines.Add('Response');
//    mmoInfo.Lines.AddStrings( Self.FHeaderResponseData );
//    ShowModal;
//  finally
//    Free;
//  end;
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

{----------------THTTPForm.FrameBrowserProcessing}
procedure THTTPForm.FrameBrowserProcessing(Sender: TObject; ProcessingOn: Boolean);
const
  What: array [Boolean, Boolean] of String = (('Frame', 'Page'), (#13#10'Frame', 'Page'));
var
  Viewer: THtmlViewer;
  Url: String;
begin
  if csLoading in ComponentState then
    Exit;

  if Sender is THtmlViewer then
  begin
    Exit;
//    Viewer := Sender as THtmlViewer;
//    Url := Viewer.CurrentFile;
  end
  else if Sender is TFrameBrowser then
  begin
    Viewer := nil;
    Url := (Sender as TFrameBrowser).CurrentFile;

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
    end
  end
  else
    Exit;

  if (LogForm <> nil) and (LogForm.LogActive[laDiag]) then
    if ProcessingOn then
      LogForm.Log(What[ProcessingOn, Viewer <> nil] + ' Processing: ' + Url)
    else
      LogForm.Log(What[ProcessingOn, Viewer <> nil] + ' Completed: ' + Url);
end;

procedure THTTPForm.PrintPreviewClick(Sender: TObject);
{$ifndef NoMetaFile}
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
{$else !NoMetaFile}
begin
{$endif !NoMetaFile}
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
    mmoInfo.Lines.Text := DocumentTitle(FrameBrowser);
    if FrameBrowser.UseQuirksMode then
      mmoInfo.Lines.Add( 'Render Mode: Quirks Mode')
    else
      mmoInfo.Lines.Add( 'Render Mode: Standards Mode');

    if Self.FMetaInfo.Count >0 then
    begin
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
{$ifndef NoMetaFile}
  with PrintDialog do
    if Execute then
      if PrintRange = prAllPages then
        FrameBrowser.Print(1, 9999)
      else
        FrameBrowser.Print(FromPage, ToPage);
{$endif !NoMetaFile}
end;

procedure THTTPForm.PrintHeader(Sender: TObject; Canvas: TCanvas; NumPage,
  W, H: Integer; var StopPrinting: Boolean);
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  AFont.Name := FontSans;
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
  AFont.Name := FontSans;
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

procedure THTTPForm.AboutClick(Sender: TObject);
var
  AboutBox: TAboutBox;
  I: Integer;
  C: ThtConnector;
  Remarks: String;
begin
  for I := 0 to Connectors.Count - 1 do
  begin
    C := Connectors[I];
    Remarks := Remarks + '<tr><td class="p">' + C.Protocols + '<td class="m"><td class="v">' + C.Version;
  end;

  AboutBox := TAboutBox.CreateIt(Self, 'FrameBrowser Demo', 'TFrameBrowser',
    '<table><thead><tr><td class="p">protocol(s)<td class="m" width="10"><td class="v">powered by<tbody>' +
    Remarks +
    '</table>',

    'thead td {font-weight: bold}' +
    'td.p {text-align: right}'
    );
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;

procedure THTTPForm.Proxy1Click(Sender: TObject);
begin
  with TProxyForm.Create(Self) do
    try
      ProxyEdit.Text     := HttpConnector.ProxyServer;
      PortEdit.Text      := HttpConnector.ProxyPort;
      ProxyUsername.Text := HttpConnector.ProxyUsername;
      ProxyPassword.Text := HttpConnector.ProxyPassword;
      UserAgent.Text     := HttpConnector.UserAgent;
      if ShowModal = mrOK then
      begin
        HttpConnector.ProxyServer   := ProxyEdit.Text;
        HttpConnector.ProxyPort     := PortEdit.Text;
        HttpConnector.ProxyUsername := ProxyUsername.Text;
        HttpConnector.ProxyPassword := ProxyPassword.Text;
        HttpConnector.UserAgent     := UserAgent.Text;
      end;
    finally
      Free;
    end;
end;

procedure THTTPForm.DemoInformation1Click(Sender: TObject);
begin
  LoadURL('res:///page0.htm');
end;

procedure THTTPForm.FrameBrowserMeta(Sender: TObject; const HttpEq, Name, Content: ThtString);
var
  S: String;
begin
  S := 'HttpEq=' + HttpEq + ', MetaName=' + Name + ', MetaContent=' + Content;
  FMetaInfo.Add(S);
  if LogForm.LogActive[laDiag] then
    LogForm.Log('FrameBrowserMeta: ' + S);
end;

procedure THTTPForm.FrameBrowserMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: string;
begin
  if not TitleTimer.Enabled and (Sender is THtmlViewer) and Assigned(ActiveControl)
         and ActiveControl.Focused then
  begin
    TitleViewer := THtmlViewer(Sender);
    TitleStr := TitleViewer.TitleAttr;
    if TitleStr = '' then
      OldTitle := ''
    else
      if TitleStr <> OldTitle then
      begin
        TimerCount := 0;
        TitleTimer.Enabled := True;
        OldTitle := TitleStr;
     end;
  end;
end;

procedure THTTPForm.CloseHints;
begin
  TitleTimer.Enabled := False;
  HintWindow.ReleaseHandle;
  HintVisible := False;
  TitleViewer := Nil;
end;

procedure THTTPForm.TitleTimerTimer(Sender: TObject);
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

procedure THTTPForm.LibraryInformation1Click(Sender: TObject);
{$ifdef UseZLib}
var LId : TIdC_ULONG;
{$endif}
begin
  //
  with TInfoForm.Create(Self) do
  try
    Caption := 'ZLib Library Information';
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
    try
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
    except
      on E: Exception do
      begin
        mmoInfo.Lines.Add('Retrieving ZLib info failed');
        mmoInfo.Lines.Add('Cause: ' + E.Message);
      end;
    end;
{$else}
    mmoInfo.Lines.Add('ZLib not included.');
{$endif}
    ShowModal;
  finally
    Free;
  end;
end;

procedure THTTPForm.FrameBrowserScript(Sender: TObject; const Name, ContentType, Src, Script: ThtString);
begin
  if LogForm.LogActive[laHttpScript] then
    LogForm.Log('FrameBrowserScript: Name=' + Name + ', Src=' + Src + ', Script=' + Script);
end;

//-- BG ---------------------------------------------------------- 24.05.2016 --
procedure THTTPForm.GetImagesAsynclyClick(Sender: TObject);
begin
  GetImagesAsyncly.Checked := not GetImagesAsyncly.Checked;
end;

procedure THTTPForm.AsyncLoadersProgress(Sender: TObject; Done, Total: Integer);
begin
  AsyncLoading := Total > 0;
  Progress(Done, Total);
  CheckEnableControls;
end;

end.

